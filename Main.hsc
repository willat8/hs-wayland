module Main where
import Myth.Internal
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import qualified Graphics.Rendering.Cairo as XP
import qualified Graphics.Rendering.Cairo.Types as XP
import System.Posix.Types
import System.Posix.IO
import System.Random
import Control.Monad.Trans (liftIO)

#include "C/hsmyth.h"

drawSquare w x y = do
    let h = w
        aspect = 1
        corner_radius = h / 10
        radius = corner_radius / aspect
        degrees = pi / 180
    c1 <- liftIO $ getStdRandom (randomR (0, 256))
    c2 <- liftIO $ getStdRandom (randomR (0, 256))
    c3 <- liftIO $ getStdRandom (randomR (0, 256))
    XP.newPath
    XP.arc (x + w - radius) (y + radius) radius (-90 * degrees) (0 * degrees)
    XP.arc (x + w - radius) (y + h - radius) radius (0 * degrees) (90 * degrees)
    XP.arc (x + radius) (y + h - radius) radius (90 * degrees) (180 * degrees)
    XP.arc (x + radius) (y + radius) radius (180 * degrees) (270 * degrees)
    XP.closePath
    XP.setSourceRGB (148 / 256) (194 / 256) (105 / 256)
    XP.fillPreserve
    XP.setSourceRGBA (c1 / 256) (c2 / 256) (c3 / 256) 0.5
    XP.setLineWidth 10
    XP.stroke

drawStatus xpsurface w h = XP.renderWith xpsurface $ do
    XP.setOperator XP.OperatorSource
    XP.setSourceRGBA 0 0 0 0
    XP.paint
    XP.setOperator XP.OperatorOver
    let sq_dim = 100
    let init_x = 160
    let y = (h - sq_dim) / 2
    drawSquare sq_dim init_x y
    drawSquare sq_dim ((w - sq_dim) / 2) y
    drawSquare sq_dim (w - init_x - sq_dim) y

statusCheck t_ptr _ = do
    let status_ptr = t_ptr `plusPtr` negate #{offset struct status, check_task}
    Status _ _ widget_ptr _ _ check_fd _ <- peek status_ptr
    fdRead check_fd #{size uint64_t}
    c_widget_schedule_redraw widget_ptr

resizeHandler _ _ _ d_ptr = do
    Status _ _ widget_ptr w h _ _ <- peek $ castPtr d_ptr
    c_widget_set_size widget_ptr w h

redrawHandler _ d_ptr = do
    Status _ window_ptr _ w h _ _ <- peek $ castPtr d_ptr
    xpsurface <- XP.mkSurface =<< c_window_get_surface window_ptr
    XP.manageSurface xpsurface
    drawStatus xpsurface (fromIntegral w) (fromIntegral h)

buttonHandler _ input_ptr _ _ state d_ptr = do
    Status display_ptr window_ptr _ _ _ _ _ <- peek $ castPtr d_ptr
    if state == wlPointerButtonStatePressed
        then c_window_move window_ptr input_ptr =<< c_display_get_serial display_ptr
        else return ()

touchDownHandler _ input_ptr _ _ _ _ _ d_ptr = do
    Status display_ptr window_ptr _ _ _ _ _ <- peek $ castPtr d_ptr
    c_window_move window_ptr input_ptr =<< c_display_get_serial display_ptr

statusConfigure status_ptr = do
    Status display_ptr window_ptr widget_ptr w h check_fd _ <- peek status_ptr
    c_display_watch_fd display_ptr check_fd epollin (#{ptr struct status, check_task} status_ptr)
    with (ITimerSpec (TimeSpec 5 0) (TimeSpec 5 0)) $ \its_ptr -> c_timerfd_settime check_fd 0 its_ptr nullPtr
    c_widget_set_resize_handler widget_ptr =<< mkResizeHandlerForeign resizeHandler
    c_widget_set_redraw_handler widget_ptr =<< mkRedrawHandlerForeign redrawHandler
    c_widget_set_button_handler widget_ptr =<< mkButtonHandlerForeign buttonHandler
    c_widget_set_touch_down_handler widget_ptr =<< mkTouchDownHandlerForeign touchDownHandler
    c_window_schedule_resize window_ptr w h

statusCreate display_ptr w h = do
    mallocForeignPtr >>= \status_fp -> withForeignPtr status_fp $ \status_ptr -> do
        window_ptr <- c_window_create display_ptr
        widget_ptr <- c_window_add_widget window_ptr $ castPtr status_ptr
        check_fd <- c_timerfd_create clockMonotonic tfdCloexec
        check_task <- Task <$> mkStatusCheckForeign statusCheck
        poke status_ptr (Status display_ptr window_ptr widget_ptr w h check_fd check_task)
        return status_fp

backgroundConfigure _ _ _ window_ptr w h = do
    bg_ptr <- castPtr <$> c_window_get_user_data window_ptr
    Background (Surface configure_funp) _ widget_ptr <- peek bg_ptr
    c_widget_schedule_resize widget_ptr w h
    freeHaskellFunPtr configure_funp

desktopShellConfigure d_ptr ds_ptr edges wl_surface_ptr w h = do
    window_ptr <- castPtr <$> c_wl_surface_get_user_data wl_surface_ptr
    surface_ptr <- castPtr <$> c_window_get_user_data window_ptr
    Surface configure_funp <- peek surface_ptr
    mkSurfaceConfigure configure_funp d_ptr ds_ptr edges window_ptr w h

desktopShellPrepareLockSurface _ = c_weston_desktop_shell_unlock

desktopShellGrabCursor d_ptr _ _ = do
    let desktop_ptr = castPtr d_ptr
    peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopCursorType = cursorLeftPtr }

backgroundCreate desktop_ptr = do
    mallocForeignPtr >>= \bg_fp -> withForeignPtr bg_fp $ \bg_ptr -> do
        display_ptr <- desktopDisplay <$> peek desktop_ptr
        base <- Surface <$> mkSurfaceConfigureForeign backgroundConfigure
        window_ptr <- c_window_create_custom display_ptr
        widget_ptr <- c_window_add_widget window_ptr $ castPtr bg_ptr
        poke bg_ptr (Background base window_ptr widget_ptr)
        c_window_set_user_data window_ptr $ castPtr bg_ptr
        c_widget_set_transparent widget_ptr 0
        return bg_fp

grabSurfaceEnterHandler _ _ _ _ d_ptr = return . desktopCursorType =<< peek (castPtr d_ptr)

grabSurfaceCreate desktop_ptr = do
    Desktop display_ptr ds_ptr _ _ _ _ <- peek desktop_ptr
    window_ptr <- c_window_create_custom display_ptr
    c_window_set_user_data window_ptr $ castPtr desktop_ptr
    s <- c_window_get_wl_surface window_ptr
    c_weston_desktop_shell_set_grab_surface ds_ptr s
    widget_ptr <- c_window_add_widget window_ptr $ castPtr desktop_ptr
    c_widget_set_allocation widget_ptr 0 0 1 1
    c_widget_set_enter_handler widget_ptr =<< mkGrabSurfaceEnterHandlerForeign grabSurfaceEnterHandler
    peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopWindow = window_ptr, desktopWidget = widget_ptr }

outputInit o_ptr desktop_ptr = do
    ds_ptr <- desktopShell <$> peek desktop_ptr
    wlo_ptr <- outputWlOutput <$> peek o_ptr
    backgroundCreate desktop_ptr >>= (`withForeignPtr` \bg_ptr -> do
        window_ptr <- backgroundWindow <$> peek bg_ptr
        s <- c_window_get_wl_surface window_ptr
        c_weston_desktop_shell_set_background ds_ptr wlo_ptr s
        )

createOutput desktop_ptr id = do
    Desktop display_ptr ds_ptr _ _ _ gc <- peek desktop_ptr
    wlo_ptr <- c_display_bind display_ptr id c_wl_output_interface 2
    mallocForeignPtr >>= (`withForeignPtr` \o_ptr -> do
        peek o_ptr >>= \o -> poke o_ptr o { outputWlOutput = wlo_ptr }
        peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopOutput = o_ptr }
        if ds_ptr /= nullPtr
            then outputInit o_ptr desktop_ptr
            else return ()
        )

globalHandler _ id interface_cs _ d_ptr = do
    c_funp   <- mkDesktopShellConfigureForeign desktopShellConfigure
    pls_funp <- mkDesktopShellPrepareLockSurfaceForeign desktopShellPrepareLockSurface
    gc_funp  <- mkDesktopShellGrabCursorForeign desktopShellGrabCursor
    with (Listener c_funp pls_funp gc_funp) $ \l_ptr -> do
        let desktop_ptr = castPtr d_ptr
        display_ptr <- desktopDisplay <$> peek desktop_ptr
        interface <- peekCString interface_cs
        if interface == "weston_desktop_shell"
            then do ds_ptr <- castPtr <$> c_display_bind display_ptr id c_weston_desktop_shell_interface 1
                    peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopShell = ds_ptr }
                    c_weston_desktop_shell_add_listener ds_ptr l_ptr desktop_ptr
                    c_weston_desktop_shell_desktop_ready ds_ptr
            else if interface == "wl_output"
            then createOutput desktop_ptr id
            else return ()

displayCreate = alloca $ \argv -> c_display_create 0 argv >>= newForeignPtr c_display_destroy

windowDestroy widget_ptr window_ptr = do
    c_widget_destroy widget_ptr
    c_window_destroy window_ptr

main = do
    mallocForeignPtr >>= (`withForeignPtr` \desktop_ptr -> do -- use Finalizers for all of these?
    displayCreate >>= (`withForeignPtr` \display_ptr -> do
    statusCreate display_ptr 800 480 >>= (`withForeignPtr` \status_ptr -> do
        peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopDisplay = display_ptr }
        c_display_set_user_data display_ptr $ castPtr desktop_ptr
        c_display_set_global_handler display_ptr =<< mkGlobalHandlerForeign globalHandler
        o_ptr <- desktopOutput <$> peek desktop_ptr
        bg_ptr <- outputBackground <$> peek o_ptr
        if bg_ptr == nullPtr
            then outputInit o_ptr desktop_ptr
            else return ()
        grabSurfaceCreate desktop_ptr
        statusConfigure status_ptr
        c_display_run display_ptr
        -- Clean up
        peek status_ptr >>= \(Status _ window_ptr widget_ptr _ _ check_fd _) -> do
            c_display_unwatch_fd display_ptr check_fd
            closeFd check_fd
            windowDestroy widget_ptr window_ptr
        peek desktop_ptr >>= \(Desktop _ _ _ window_ptr widget_ptr _) -> windowDestroy widget_ptr window_ptr
        peek bg_ptr >>= \(Background _ window_ptr widget_ptr) -> windowDestroy widget_ptr window_ptr
        c_wl_output_destroy =<< outputWlOutput <$> peek o_ptr
        c_weston_desktop_shell_destroy =<< desktopShell <$> peek desktop_ptr
        )))
    return 0

