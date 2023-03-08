module Main (main) where
import Myth.Internal
import Myth.Client
import Control.Monad
import Foreign hiding (void)
import Foreign.C.String
import qualified Foreign.Concurrent as FC

#include "hsmyth.h"

backgroundConfigure _ _ _ window_ptr w h = do
    bg_ptr <- castPtr <$> c_window_get_user_data window_ptr
    Background {backgroundSurface = base, backgroundWidget = widget} <- peek bg_ptr
    c_widget_schedule_resize widget w h
    let Surface configure_funp = base in freeHaskellFunPtr configure_funp

desktopShellConfigure d_ptr ds_ptr edges wl_surface_ptr w h = do
    window_ptr <- castPtr <$> c_wl_surface_get_user_data wl_surface_ptr
    surface_ptr <- castPtr <$> c_window_get_user_data window_ptr
    Surface configure_funp <- peek surface_ptr
    mkSurfaceConfigure configure_funp d_ptr ds_ptr edges window_ptr w h

desktopShellPrepareLockSurface _ = c_weston_desktop_shell_unlock

desktopShellGrabCursor d_ptr _ _ = pokeByteOff d_ptr #{offset struct desktop, grab_cursor} (unCursorType cursorLeftPtr)

backgroundCreate desktop_ptr = do
    mallocForeignPtr >>= \bg_fp -> withForeignPtr bg_fp $ \bg_ptr -> do
        Desktop {desktopDisplay = display} <- peek desktop_ptr
        base <- Surface <$> mkSurfaceConfigureForeign backgroundConfigure
        window_ptr <- c_window_create_custom display
        widget_ptr <- c_window_add_widget window_ptr (castPtr bg_ptr)
        peek bg_ptr >>= \bg -> poke bg_ptr bg { backgroundSurface = base
                                              , backgroundWindow = window_ptr
                                              , backgroundWidget = widget_ptr
                                              }
        c_window_set_user_data window_ptr (castPtr bg_ptr)
        c_widget_set_transparent widget_ptr 0
        return bg_fp

grabSurfaceEnterHandler _ _ _ _ d_ptr = desktopCursorType <$> peek (castPtr d_ptr)

grabSurfaceCreate desktop_fp = withForeignPtr desktop_fp $ \desktop_ptr -> do
    Desktop {desktopDisplay = display, desktopShell = ds} <- peek desktop_ptr
    window_ptr <- c_window_create_custom display
    pokeByteOff desktop_ptr #{offset struct desktop, grab_window} window_ptr
    c_window_set_user_data window_ptr (castPtr desktop_ptr)
    s <- c_window_get_wl_surface window_ptr
    c_weston_desktop_shell_set_grab_surface ds s
    widget_ptr <- c_window_add_widget window_ptr (castPtr desktop_ptr)
    c_widget_set_allocation widget_ptr 0 0 1 1
    enter_handler_funp <- mkGrabSurfaceEnterHandlerForeign grabSurfaceEnterHandler
    c_widget_set_enter_handler widget_ptr enter_handler_funp
    pokeByteOff desktop_ptr #{offset struct desktop, grab_widget} widget_ptr
    FC.addForeignPtrFinalizer desktop_fp $ freeHaskellFunPtr enter_handler_funp

outputInit o_ptr desktop_ptr = do
    Desktop {desktopShell = ds} <- peek desktop_ptr
    Output {outputWlOutput = wlo} <- peek o_ptr
    backgroundCreate desktop_ptr >>= (`withForeignPtr` \bg_ptr -> do
        pokeByteOff o_ptr #{offset struct output, background} bg_ptr
        Background {backgroundWindow = window} <- peek bg_ptr
        s <- c_window_get_wl_surface window
        c_weston_desktop_shell_set_background ds wlo s)

createOutput desktop_ptr id = do
    Desktop {desktopDisplay = display, desktopShell = ds} <- peek desktop_ptr
    wlo_ptr <- c_display_bind display id c_wl_output_interface 2
    mallocForeignPtr >>= (`withForeignPtr` \o_ptr -> do
        pokeByteOff o_ptr #{offset struct output, output} wlo_ptr
        pokeByteOff desktop_ptr #{offset struct desktop, output} o_ptr
        when (ds /= nullPtr) $ outputInit o_ptr desktop_ptr)

globalHandler l_ptr _ id interface_cs _ d_ptr = do
    let desktop_ptr = castPtr d_ptr
    interface <- peekCString interface_cs
    case interface of
        "weston_desktop_shell" -> do
            Desktop {desktopDisplay = display} <- peek desktop_ptr
            ds_ptr <- castPtr <$> c_display_bind display id c_weston_desktop_shell_interface 1
            pokeByteOff desktop_ptr #{offset struct desktop, shell} ds_ptr
            c_weston_desktop_shell_add_listener ds_ptr l_ptr desktop_ptr
            c_weston_desktop_shell_desktop_ready ds_ptr
        "wl_output" -> createOutput desktop_ptr id
        otherwise -> return ()

globalHandlerRemove _ _ interface_cs _ d_ptr = do
    interface <- peekCString interface_cs
    case interface of
        "wl_output" -> outputDestroy =<< desktopOutput <$> peek (castPtr d_ptr)
        otherwise -> return ()

displayCreate global_handler_fp global_handler_remove_fp = do
    display_ptr <- alloca $ \argv -> c_display_create 0 argv
    display_fp <- newForeignPtr_ display_ptr
    FC.addForeignPtrFinalizer display_fp (withForeignPtr display_fp $ \display_ptr -> do
        freeHaskellFunPtr global_handler_fp
        freeHaskellFunPtr global_handler_remove_fp
        c_display_destroy display_ptr)
    return display_fp

windowDestroy widget_ptr window_ptr = do
    c_widget_destroy widget_ptr
    c_window_destroy window_ptr

outputDestroy o_ptr = do
    Output {outputWlOutput = wlo, outputBackground = bg} <- peek o_ptr
    peek bg >>= \Background {backgroundWindow = window, backgroundWidget = widget} -> windowDestroy widget window
    c_wl_output_destroy wlo

desktopCreate l_ptr = do
    mallocForeignPtr >>= \desktop_fp -> do
        FC.addForeignPtrFinalizer desktop_fp (withForeignPtr desktop_fp $ \desktop_ptr ->
            peek desktop_ptr >>= \Desktop { desktopShell = ds
                                          , desktopOutput = o
                                          , desktopWindow = window
                                          , desktopWidget = widget
                                          } -> do
                windowDestroy widget window
                outputDestroy o
                peek l_ptr >>= \l ->
                    let funps = [castFunPtr . listenerConfigure, castFunPtr . listenerPrepareLockSurface, castFunPtr . listenerGrabCursor] <*> pure l
                    in mapM_ freeHaskellFunPtr funps
                free l_ptr
                c_weston_desktop_shell_destroy ds)
        return desktop_fp

main = do
    listener <- new =<< Listener <$> mkDesktopShellConfigureForeign desktopShellConfigure
                                 <*> mkDesktopShellPrepareLockSurfaceForeign desktopShellPrepareLockSurface
                                 <*> mkDesktopShellGrabCursorForeign desktopShellGrabCursor
    global_handler_fp <- mkGlobalHandlerForeign $ globalHandler listener
    global_handler_remove_fp <- mkGlobalHandlerRemoveForeign globalHandlerRemove
    displayCreate global_handler_fp global_handler_remove_fp >>= (`withForeignPtr` \display_ptr -> do
        desktopCreate listener >>= \desktop_fp -> (withForeignPtr desktop_fp $ \desktop_ptr -> do
            pokeByteOff desktop_ptr #{offset struct desktop, display} display_ptr
            c_display_set_user_data display_ptr (castPtr desktop_ptr)
            c_display_set_global_handler display_ptr global_handler_fp
            c_display_set_global_handler_remove display_ptr global_handler_remove_fp
            o_ptr <- desktopOutput <$> peek desktop_ptr
            join $ when . (== nullPtr) . outputBackground <$> peek o_ptr <*> pure (outputInit o_ptr desktop_ptr)
            grabSurfaceCreate desktop_fp
            statusCreate display_ptr 800 480 >>= (`withForeignPtr` (const <$> c_display_run) display_ptr)))

