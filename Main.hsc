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

drawStatus xpsurface w h = do
    XP.renderWith xpsurface $ do
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

statusCheck t_ptr events = do
    let status_ptr = t_ptr `plusPtr` negate #{offset struct status, check_task}
    Status _ _ widget_ptr _ _ check_fd _ <- peek status_ptr
    fdRead check_fd #{size uint64_t}
    c_widget_schedule_redraw widget_ptr

redrawHandler widget_ptr d_ptr = do
    Status _ window_ptr _ w h _ _ <- peek $ castPtr d_ptr
    xpsurface <- c_window_get_surface window_ptr >>= XP.mkSurface
    XP.manageSurface xpsurface
    drawStatus xpsurface (fromIntegral w) (fromIntegral h)

buttonHandler widget_ptr input_ptr time button state d_ptr = do
    Status display_ptr window_ptr _ _ _ _ _ <- peek $ castPtr d_ptr
    if state == #{const WL_POINTER_BUTTON_STATE_PRESSED}
        then c_window_move window_ptr input_ptr =<< c_display_get_serial display_ptr
        else return ()

statusConfigure status_ptr = do
    Status display_ptr window_ptr widget_ptr w h check_fd _ <- peek status_ptr
    c_display_watch_fd display_ptr check_fd epollin (#{ptr struct status, check_task} status_ptr)
    with (ITimerSpec (TimeSpec 5 0) (TimeSpec 5 0)) $ \its_ptr -> c_timerfd_settime check_fd 0 its_ptr nullPtr
    rh_funp <- mkRedrawHandlerForeign redrawHandler
    bh_funp <- mkButtonHandlerForeign buttonHandler
    c_widget_set_redraw_handler widget_ptr rh_funp
    c_widget_set_button_handler widget_ptr bh_funp
    c_window_schedule_resize window_ptr w h
    return (rh_funp, bh_funp)

statusCreate display_ptr w h = do
    mallocForeignPtr >>= \status_fp -> withForeignPtr status_fp $ \status_ptr -> do
        window_ptr <- c_window_create display_ptr
        widget_ptr <- c_window_add_widget window_ptr $ castPtr status_ptr
        check_fd <- c_timerfd_create clockMonotonic tfdCloexec
        check_task <- mkStatusCheckForeign statusCheck >>= return . Task
        poke status_ptr (Status display_ptr window_ptr widget_ptr w h check_fd check_task)
        return status_fp

backgroundConfigure d_ptr ds_ptr edges window_ptr w h = do
    bg_ptr <- c_window_get_user_data window_ptr >>= return . castPtr
    Background (Surface configure_funp) _ widget_ptr <- peek bg_ptr
    c_widget_schedule_resize widget_ptr w h
    freeHaskellFunPtr configure_funp

desktopShellConfigure d_ptr ds_ptr edges wl_surface_ptr w h = do
    window_ptr <- c_wl_surface_get_user_data wl_surface_ptr >>= return . castPtr
    surface_ptr <- c_window_get_user_data window_ptr >>= return . castPtr
    Surface configure_funp <- peek surface_ptr
    mkSurfaceConfigure configure_funp d_ptr ds_ptr edges window_ptr w h

desktopShellPrepareLockSurface _ = c_weston_desktop_shell_unlock

desktopShellGrabCursor d_ptr ds_ptr c = do
    let desktop_ptr = castPtr d_ptr
    peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopCursorType = cursorLeftPtr }

backgroundCreate desktop_ptr = do
    mallocForeignPtr >>= \bg_fp -> withForeignPtr bg_fp $ \bg_ptr -> do
        display_ptr <- peek desktop_ptr >>= return . desktopDisplay
        base <- mkSurfaceConfigureForeign backgroundConfigure >>= return . Surface
        window_ptr <- c_window_create_custom display_ptr
        widget_ptr <- c_window_add_widget window_ptr $ castPtr bg_ptr
        poke bg_ptr (Background base window_ptr widget_ptr)
        c_window_set_user_data window_ptr $ castPtr bg_ptr
        c_widget_set_transparent widget_ptr 0
        return bg_fp

grabSurfaceEnterHandler widget_ptr input_ptr x y d_ptr = do
    let desktop_ptr = castPtr d_ptr
    peek desktop_ptr >>= return . desktopCursorType

grabSurfaceCreate desktop_ptr = do
    Desktop display_ptr ds_ptr _ _ _ _ <- peek desktop_ptr
    window_ptr <- c_window_create_custom display_ptr
    c_window_set_user_data window_ptr $ castPtr desktop_ptr
    s <- c_window_get_wl_surface window_ptr
    c_weston_desktop_shell_set_grab_surface ds_ptr s
    widget_ptr <- c_window_add_widget window_ptr $ castPtr desktop_ptr
    c_widget_set_allocation widget_ptr 0 0 1 1
    gseh_funp <- mkGrabSurfaceEnterHandlerForeign grabSurfaceEnterHandler
    c_widget_set_enter_handler widget_ptr gseh_funp
    peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopWindow = window_ptr, desktopWidget = widget_ptr }
    return gseh_funp

outputInit o_ptr desktop_ptr = do
    ds_ptr <- peek desktop_ptr >>= return . desktopShell
    wlo_ptr <- peek o_ptr >>= return . outputWlOutput
    backgroundCreate desktop_ptr >>= \bg_fp -> withForeignPtr bg_fp $ \bg_ptr -> do
        window_ptr <- peek bg_ptr >>= return . backgroundWindow
        s <- c_window_get_wl_surface window_ptr
        c_weston_desktop_shell_set_background ds_ptr wlo_ptr s

createOutput desktop_ptr id = do
    Desktop display_ptr ds_ptr _ _ _ gc <- peek desktop_ptr
    wlo_ptr <- c_display_bind display_ptr id c_wl_output_interface 2
    mallocForeignPtr >>= \o_fp -> withForeignPtr o_fp $ \o_ptr -> do
        peek o_ptr >>= \o -> poke o_ptr o { outputWlOutput = wlo_ptr }
        peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopOutput = o_ptr }
        if ds_ptr /= nullPtr
            then outputInit o_ptr desktop_ptr
            else return ()

globalHandler _ id interface_cs version d_ptr = do
    c_funp   <- mkDesktopShellConfigureForeign desktopShellConfigure
    pls_funp <- mkDesktopShellPrepareLockSurfaceForeign desktopShellPrepareLockSurface
    gc_funp  <- mkDesktopShellGrabCursorForeign desktopShellGrabCursor
    with (Listener c_funp pls_funp gc_funp) $ \l_ptr -> do
        let desktop_ptr = castPtr d_ptr
        display_ptr <- peek desktop_ptr >>= return . desktopDisplay
        interface <- peekCString interface_cs
        if interface == "weston_desktop_shell"
            then do ds_ptr <- c_display_bind display_ptr id c_weston_desktop_shell_interface 1 >>= return . castPtr
                    peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopShell = ds_ptr }
                    c_weston_desktop_shell_add_listener ds_ptr l_ptr desktop_ptr
                    c_weston_desktop_shell_desktop_ready ds_ptr
            else if interface == "wl_output"
            then createOutput desktop_ptr id
            else return ()

displayCreate = alloca $ \argv -> c_display_create 0 argv >>= newForeignPtr c_display_destroy

main = do
    mallocForeignPtr >>= \desktop_fp -> withForeignPtr desktop_fp $ \desktop_ptr -> do
    displayCreate >>= \display_fp -> withForeignPtr display_fp $ \display_ptr -> do
    statusCreate display_ptr 800 480 >>= \status_fp -> withForeignPtr status_fp $ \status_ptr -> do
        peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopDisplay = display_ptr }
        gh_funp <- mkGlobalHandlerForeign globalHandler
        c_display_set_user_data display_ptr $ castPtr desktop_ptr
        c_display_set_global_handler display_ptr gh_funp
        o_ptr <- peek desktop_ptr >>= return . desktopOutput
        bg_ptr <- peek o_ptr >>= return . outputBackground
        if bg_ptr == nullPtr
            then outputInit o_ptr desktop_ptr
            else return ()
        gseh_funp <- grabSurfaceCreate desktop_ptr
        (rh_funp, bh_funp) <- statusConfigure status_ptr
        c_display_run display_ptr
        -- Clean up
        freeHaskellFunPtr rh_funp
        freeHaskellFunPtr bh_funp
        freeHaskellFunPtr gseh_funp
        freeHaskellFunPtr gh_funp
        peek status_ptr >>= (\(Task status_check_funp) -> freeHaskellFunPtr status_check_funp) . statusCheckTask
        peek status_ptr >>= c_widget_destroy . statusWidget
        peek status_ptr >>= c_window_destroy . statusWindow
        peek desktop_ptr >>= c_widget_destroy . desktopWidget
        peek desktop_ptr >>= c_window_destroy . desktopWindow
        peek bg_ptr >>= c_widget_destroy . backgroundWidget
        peek bg_ptr >>= c_window_destroy . backgroundWindow
        peek o_ptr >>= c_wl_output_destroy . outputWlOutput
        peek desktop_ptr >>= c_weston_desktop_shell_destroy . desktopShell
    return 0

