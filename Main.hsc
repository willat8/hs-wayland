module Main where
import Myth.Internal
import Myth.Status
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import qualified Foreign.Concurrent as FC
import qualified Graphics.Rendering.Cairo as XP
import qualified Graphics.Rendering.Cairo.Types as XP
import System.Posix.Types
import System.Posix.IO
import Data.Bits.Bitwise
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Control.Monad.Trans (liftIO)
import Control.Applicative

#include "C/hsmyth.h"

drawText win_w win_h s = do
    XP.setSourceRGBA 1 1 1 0.5
    XP.selectFontFace "Sans" XP.FontSlantNormal XP.FontWeightNormal
    XP.setFontSize 150
    XP.TextExtents xb yb w h _ _ <- XP.textExtents s
    XP.moveTo ((win_w - w) / 2 - xb) ((win_h - h) / 2 - yb)
    XP.showText s

drawSquare w x y isGreen isPurple = do
    let h = w
        aspect = 1
        corner_radius = h / 10
        radius = corner_radius / aspect
        degrees = pi / 180
        red = (239, 41, 41)
        green = (148, 194, 105)
        purple = (152, 107, 194)
    let (fc1, fc2, fc3) = if isPurple then purple else if isGreen then green else red  -- Fill colour
        (bc1, bc2, bc3) = if isPurple then green else (fc1, fc2, fc3)                  -- Border colour
    XP.newPath
    XP.arc (x + w - radius) (y + radius) radius (-90 * degrees) (0 * degrees)
    XP.arc (x + w - radius) (y + h - radius) radius (0 * degrees) (90 * degrees)
    XP.arc (x + radius) (y + h - radius) radius (90 * degrees) (180 * degrees)
    XP.arc (x + radius) (y + radius) radius (180 * degrees) (270 * degrees)
    XP.closePath
    XP.setSourceRGB (fc1 / 256) (fc2 / 256) (fc3 / 256)
    XP.fillPreserve
    XP.setSourceRGBA (bc1 / 256) (bc2 / 256) (bc3 / 256) 0.5
    XP.setLineWidth 10
    XP.stroke

drawStatus :: MonadIO m => XP.Surface -> Double -> Double -> Int -> m ()
drawStatus xpsurface w h code = XP.renderWith xpsurface $ do
    XP.setOperator XP.OperatorSource
    XP.setSourceRGBA 0 0 0 0
    XP.paint
    XP.setOperator XP.OperatorOver
    let sq_dim = 100
    let init_x = 160
    let y1 = (h - 2 * sq_dim) / 3
    let y2 = h - y1 - sq_dim
    let (c11:a11:c12:a12:
         c21:a21:c22:a22:
         c31:a31:c32:a32:_) = toListLE code
    drawSquare sq_dim init_x y1 c11 a11
    drawSquare sq_dim init_x y2 c12 a12
    drawSquare sq_dim ((w - sq_dim) / 2) y1 c21 a21
    drawSquare sq_dim ((w - sq_dim) / 2) y2 c22 a22
    drawSquare sq_dim (w - init_x - sq_dim) y1 c31 a31
    drawSquare sq_dim (w - init_x - sq_dim) y2 c32 a32

drawClock :: MonadIO m => XP.Surface -> Double -> Double -> Int -> m ()
drawClock xpsurface w h code = XP.renderWith xpsurface $ do
    XP.setOperator XP.OperatorSource
    XP.setSourceRGBA 0 0 0 0
    XP.paint
    XP.setOperator XP.OperatorOver
    t <- liftIO $ getCurrentTime
    tz <- liftIO $ getCurrentTimeZone
    let s = formatTime defaultTimeLocale "%l %M" <$> localTimeOfDay $ utcToLocalTime tz t
    drawText w h s

statusCheck t_ptr _ = do
    let status_ptr = t_ptr `plusPtr` negate #{offset struct status, check_task}
    Status _ _ widget_ptr _ _ check_fd _ _ _ <- peek status_ptr
    fdRead check_fd #{size uint64_t}
    s <- getStatus
    let code = (fromListLE s :: Int)
    -- Show the clock when the code is unchanged from the last status check
    peek status_ptr >>= \status -> poke status_ptr status { statusCode = fromIntegral code, statusShowClock = (fromIntegral code == fromIntegral (statusCode status)) }
    c_widget_schedule_redraw widget_ptr

resizeHandler _ _ _ d_ptr = do
    Status _ _ widget_ptr w h _ _ _ _ <- peek (castPtr d_ptr)
    c_widget_set_size widget_ptr w h

redrawHandler _ d_ptr = do
    let status_ptr = castPtr d_ptr
    Status _ window_ptr _ w h _ _ code show_clock <- peek status_ptr
    xpsurface <- XP.mkSurface =<< c_window_get_surface window_ptr
    XP.manageSurface xpsurface
    case show_clock of False -> do drawStatus xpsurface (fromIntegral w) (fromIntegral h) (fromIntegral code)
                                   peek status_ptr >>= \status -> poke status_ptr status { statusShowClock = True }
                       True -> drawClock xpsurface (fromIntegral w) (fromIntegral h) (fromIntegral code)

buttonHandler _ input_ptr _ _ state d_ptr = do
    Status display_ptr window_ptr _ _ _ _ _ _ _ <- peek (castPtr d_ptr)
    if state == wlPointerButtonStatePressed
        then c_window_move window_ptr input_ptr =<< c_display_get_serial display_ptr
        else return ()

touchDownHandler _ input_ptr _ _ _ _ _ d_ptr = do
    Status display_ptr window_ptr _ _ _ _ _ _ _ <- peek (castPtr d_ptr)
    c_window_move window_ptr input_ptr =<< c_display_get_serial display_ptr

keyHandler _ _ _ _ _ state d_ptr = do
    let status_ptr = castPtr d_ptr
    Status _ _ widget_ptr _ _ check_fd _ _ _ <- peek status_ptr
    if state == wlKeyboardKeyStatePressed
        then do with (ITimerSpec (TimeSpec 30 0) (TimeSpec 30 0)) $ \its_ptr -> c_timerfd_settime check_fd 0 its_ptr nullPtr
                peek status_ptr >>= \status -> poke status_ptr status { statusShowClock = False }
                c_widget_schedule_redraw widget_ptr
        else return ()

statusConfigure status_ptr = do
    Status display_ptr window_ptr widget_ptr w h check_fd _ _ _ <- peek status_ptr
    c_display_watch_fd display_ptr check_fd epollin (#{ptr struct status, check_task} status_ptr)
    with (ITimerSpec (TimeSpec 30 0) (TimeSpec 1 0)) $ \its_ptr -> c_timerfd_settime check_fd 0 its_ptr nullPtr
    c_widget_set_resize_handler widget_ptr =<< mkResizeHandlerForeign resizeHandler
    c_widget_set_redraw_handler widget_ptr =<< mkRedrawHandlerForeign redrawHandler
    c_widget_set_button_handler widget_ptr =<< mkButtonHandlerForeign buttonHandler
    c_widget_set_touch_down_handler widget_ptr =<< mkTouchDownHandlerForeign touchDownHandler
    c_window_set_key_handler window_ptr =<< mkKeyHandlerForeign keyHandler
    c_window_schedule_resize window_ptr w h

statusCreate display_ptr w h = do
    mallocForeignPtr >>= \status_fp -> withForeignPtr status_fp $ \status_ptr -> do
        FC.addForeignPtrFinalizer status_fp $ peek status_ptr >>= \(Status _ window_ptr widget_ptr _ _ check_fd _ _ _) -> do
            c_display_unwatch_fd display_ptr check_fd
            closeFd check_fd
            windowDestroy widget_ptr window_ptr
        window_ptr <- c_window_create display_ptr
        widget_ptr <- c_window_add_widget window_ptr $ castPtr status_ptr
        check_fd <- c_timerfd_create clockMonotonic tfdCloexec
        check_task <- Task <$> mkStatusCheckForeign statusCheck
        poke status_ptr (Status display_ptr window_ptr widget_ptr w h check_fd check_task 0 True)
        c_window_set_user_data window_ptr $ castPtr status_ptr
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
        peek bg_ptr >>= \bg -> poke bg_ptr bg { backgroundSurface = base, backgroundWindow = window_ptr }
        widget_ptr <- c_window_add_widget window_ptr $ castPtr bg_ptr
        peek bg_ptr >>= \bg -> poke bg_ptr bg { backgroundWidget = widget_ptr }
        c_window_set_user_data window_ptr $ castPtr bg_ptr
        c_widget_set_transparent widget_ptr 0
        return bg_fp

grabSurfaceEnterHandler _ _ _ _ d_ptr = desktopCursorType <$> peek (castPtr d_ptr)

grabSurfaceCreate desktop_ptr = do
    Desktop display_ptr ds_ptr _ _ _ _ <- peek desktop_ptr
    window_ptr <- c_window_create_custom display_ptr
    peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopWindow = window_ptr }
    c_window_set_user_data window_ptr $ castPtr desktop_ptr
    s <- c_window_get_wl_surface window_ptr
    c_weston_desktop_shell_set_grab_surface ds_ptr s
    widget_ptr <- c_window_add_widget window_ptr $ castPtr desktop_ptr
    c_widget_set_allocation widget_ptr 0 0 1 1
    c_widget_set_enter_handler widget_ptr =<< mkGrabSurfaceEnterHandlerForeign grabSurfaceEnterHandler
    peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopWidget = widget_ptr }

outputInit o_ptr desktop_ptr = do
    ds_ptr <- desktopShell <$> peek desktop_ptr
    wlo_ptr <- outputWlOutput <$> peek o_ptr
    backgroundCreate desktop_ptr >>= (`withForeignPtr` \bg_ptr -> do
        peek o_ptr >>= \o -> poke o_ptr o { outputBackground = bg_ptr }
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
    let desktop_ptr = castPtr d_ptr
    interface <- peekCString interface_cs
    if interface == "weston_desktop_shell"
        then do display_ptr <- desktopDisplay <$> peek desktop_ptr
                ds_ptr <- castPtr <$> c_display_bind display_ptr id c_weston_desktop_shell_interface 1
                peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopShell = ds_ptr }
                l_ptr <- new =<< Listener <$> mkDesktopShellConfigureForeign desktopShellConfigure
                                          <*> mkDesktopShellPrepareLockSurfaceForeign desktopShellPrepareLockSurface
                                          <*> mkDesktopShellGrabCursorForeign desktopShellGrabCursor
                c_weston_desktop_shell_add_listener ds_ptr l_ptr desktop_ptr
                c_weston_desktop_shell_desktop_ready ds_ptr
        else if interface == "wl_output"
        then createOutput desktop_ptr id
        else return ()

globalHandlerRemove _ _ interface_cs _ d_ptr = do
    interface <- peekCString interface_cs
    if interface == "wl_output"
        then outputDestroy =<< desktopOutput <$> peek (castPtr d_ptr)
        else return ()

displayCreate = alloca $ \argv -> c_display_create 0 argv >>= newForeignPtr c_display_destroy

windowDestroy widget_ptr window_ptr = do
    c_widget_destroy widget_ptr
    c_window_destroy window_ptr

outputDestroy o_ptr = do
    Output wlo_ptr bg_ptr <- peek o_ptr
    peek bg_ptr >>= \(Background _ window_ptr widget_ptr) -> windowDestroy widget_ptr window_ptr
    c_wl_output_destroy wlo_ptr

desktopCreate = do
    mallocForeignPtr >>= \desktop_fp -> withForeignPtr desktop_fp $ \desktop_ptr -> do
        FC.addForeignPtrFinalizer desktop_fp $ peek desktop_ptr >>= \(Desktop _ ds_ptr o_ptr window_ptr widget_ptr _) -> do
            windowDestroy widget_ptr window_ptr
            outputDestroy o_ptr
            c_weston_desktop_shell_destroy ds_ptr
        return desktop_fp

main = do
    displayCreate >>= (`withForeignPtr` \display_ptr -> do
    desktopCreate >>= \desktop_fp -> withForeignPtr desktop_fp $ \desktop_ptr -> do
    statusCreate display_ptr 800 480 >>= \status_fp -> withForeignPtr status_fp $ \status_ptr -> do
        peek desktop_ptr >>= \desktop -> poke desktop_ptr desktop { desktopDisplay = display_ptr }
        c_display_set_user_data display_ptr $ castPtr desktop_ptr
        c_display_set_global_handler display_ptr =<< mkGlobalHandlerForeign globalHandler
        c_display_set_global_handler_remove display_ptr =<< mkGlobalHandlerRemoveForeign globalHandlerRemove
        o_ptr <- desktopOutput <$> peek desktop_ptr
        bg_ptr <- outputBackground <$> peek o_ptr
        if bg_ptr == nullPtr
            then outputInit o_ptr desktop_ptr
            else return ()
        grabSurfaceCreate desktop_ptr
        statusConfigure status_ptr
        c_display_run display_ptr
        finalizeForeignPtr status_fp
        finalizeForeignPtr desktop_fp
        )

