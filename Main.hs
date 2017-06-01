{-# LINE 1 "Main.hsc" #-}
module Main where
{-# LINE 2 "Main.hsc" #-}
import Myth.Internal
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

backgroundConfigure d_ptr ds_ptr edges window_ptr w h = do
    bg_ptr <- c_window_get_user_data window_ptr >>= return . castPtr :: IO (Ptr Background)
    widget_ptr <- peek bg_ptr >>= return . backgroundWidget
    c_widget_schedule_resize widget_ptr w h

desktopShellConfigure d_ptr ds_ptr edges wl_surface_ptr w h = do
    window_ptr <- c_wl_surface_get_user_data wl_surface_ptr >>= return . castPtr :: IO (Ptr Window)
    surface_ptr <- c_window_get_user_data window_ptr >>= return . castPtr :: IO (Ptr Surface)
    Surface configure_funp <- peek surface_ptr
    mkSurfaceConfigure configure_funp d_ptr ds_ptr edges window_ptr w h

desktopShellPrepareLockSurface d_ptr ds_ptr = return ()

desktopShellGrabCursor d_ptr ds_ptr c = do
    let desktop_ptr = castPtr d_ptr :: Ptr Desktop
    peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopCursorType = c }

backgroundCreate desktop_ptr = do
    bg_fp <- mallocForeignPtr :: IO (ForeignPtr Background)
    withForeignPtr bg_fp $ \bg_ptr -> do
        display_ptr <- peek desktop_ptr >>= return . desktopDisplay
        base <- mkSurfaceConfigureForeign backgroundConfigure >>= return . Surface -- free this funp, via finalizer?
        window_ptr <- c_window_create_custom display_ptr
        widget_ptr <- c_window_add_widget window_ptr (castPtr bg_ptr :: Ptr ())
        poke bg_ptr (Background base window_ptr widget_ptr)
        c_window_set_user_data window_ptr (castPtr bg_ptr :: Ptr ())
        c_widget_set_transparent widget_ptr 0
    return bg_fp

grabSurfaceEnterHandler widget_ptr input_ptr x y d_ptr = do
    let desktop_ptr = castPtr d_ptr :: Ptr Desktop
    peek desktop_ptr >>= return . desktopCursorType

grabSurfaceCreate desktop_ptr = do
    Desktop display_ptr ds_ptr _ _ _ gc <- peek desktop_ptr
    window_ptr <- c_window_create_custom display_ptr
    c_window_set_user_data window_ptr (castPtr desktop_ptr :: Ptr ())
    s <- c_window_get_wl_surface window_ptr
    c_weston_desktop_shell_set_grab_surface ds_ptr s
    widget_ptr <- c_window_add_widget window_ptr (castPtr desktop_ptr :: Ptr ())
    c_widget_set_allocation widget_ptr 0 0 1 1
    gseh_funp <- mkGrabSurfaceEnterHandlerForeign grabSurfaceEnterHandler
    c_widget_set_enter_handler widget_ptr gseh_funp
    peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopWindow = window_ptr, desktopWidget = widget_ptr }
    return gseh_funp

outputInit o_ptr desktop_ptr = do
    ds_ptr <- peek desktop_ptr >>= return . desktopShell
    wlo_ptr <- peek o_ptr >>= return . outputWlOutput
    bg_fp <- backgroundCreate desktop_ptr
    withForeignPtr bg_fp $ \bg_ptr -> do
        window_ptr <- peek bg_ptr >>= return . backgroundWindow
        s <- c_window_get_wl_surface window_ptr
        c_weston_desktop_shell_set_background ds_ptr wlo_ptr s

createOutput desktop_ptr id = do
    Desktop display_ptr ds_ptr _ _ _ gc <- peek desktop_ptr
    wlo_ptr <- c_display_bind display_ptr id c_wl_output_interface 2
    o_fp <- mallocForeignPtr :: IO (ForeignPtr Output)
    withForeignPtr o_fp $ \o_ptr -> do
        peek o_ptr >>= \o -> poke o_ptr o { outputWlOutput = wlo_ptr }
        peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopOutput = o_ptr }
        if ds_ptr /= nullPtr
            then outputInit o_ptr desktop_ptr
            else return ()

globalHandler _ id interface_cs version d_ptr = do
    c_funp   <- mkDesktopShellConfigureForeign desktopShellConfigure -- free this funp, via finalizer?
    pls_funp <- mkDesktopShellPrepareLockSurfaceForeign desktopShellPrepareLockSurface -- free this funp, via finalizer?
    gc_funp  <- mkDesktopShellGrabCursorForeign desktopShellGrabCursor -- free this funp, via finalizer?
    with (Listener c_funp pls_funp gc_funp) $ \l_ptr -> do
        let desktop_ptr = castPtr d_ptr :: Ptr Desktop
        display_ptr <- peek desktop_ptr >>= return . desktopDisplay
        interface <- peekCString interface_cs
        if interface == "weston_desktop_shell"
            then do ds_ptr <- c_display_bind display_ptr id c_weston_desktop_shell_interface 1 >>= return . castPtr :: IO (Ptr WestonDesktopShell)
                    peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopShell = ds_ptr }
                    c_weston_desktop_shell_add_listener ds_ptr l_ptr desktop_ptr
                    c_weston_desktop_shell_desktop_ready ds_ptr
            else if interface == "wl_output"
            then createOutput desktop_ptr id
            else return ()

displayCreate = alloca $ \argv -> c_display_create 0 argv >>= newForeignPtr finalizerFree

main = do
    desktop_fp <- mallocForeignPtr :: IO (ForeignPtr Desktop)
    display_fp <- displayCreate
    withForeignPtr display_fp $ \display_ptr -> do
    withForeignPtr desktop_fp $ \desktop_ptr -> do
        peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopDisplay = display_ptr }
        gh_funp <- mkGlobalHandlerForeign globalHandler
        c_display_set_user_data display_ptr (castPtr desktop_ptr :: Ptr ())
        c_display_set_global_handler display_ptr gh_funp
        o_ptr <- peek desktop_ptr >>= return . desktopOutput
        bg_ptr <- peek o_ptr >>= return . outputBackground
        if bg_ptr == nullPtr
            then outputInit o_ptr desktop_ptr
            else return ()
        gseh_funp <- grabSurfaceCreate desktop_ptr
        c_display_run display_ptr
        -- Clean up
        freeHaskellFunPtr gseh_funp
        freeHaskellFunPtr gh_funp
    return 0

