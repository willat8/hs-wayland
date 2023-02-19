module Main (main) where
import Myth.Internal
import Myth.Status
import Myth.Alert
import Myth.Render
import Control.Concurrent
import Control.Monad
import Foreign hiding (void)
import Foreign.C.String
import qualified Foreign.Concurrent as FC
import qualified Graphics.Rendering.Cairo.Types as XP
import System.Posix.IO

#include "hsmyth.h"

statusCheck t_ptr _ = do
    void . forkIO $ do
    let status_ptr = t_ptr `plusPtr` negate #{offset struct status, check_task}
    Status _ _ widget_ptr _ _ check_fd _ _ _ <- peek status_ptr
    fdRead check_fd #{size uint64_t}
    encoders <- getEncodersStatus
    showClock <- peek status_ptr >>= \status -> return (encoders == statusEncoders status)
    -- Free any existing recording_title CStrings and channel_icon StablePtrs, then the array
    old_encoders <- take <$> #{peek struct status, num_encoders} status_ptr <*> (iterate (flip advancePtr 1) <$> #{peek struct status, encoders} status_ptr :: IO [Ptr Encoder])
    mapM_ free =<< mapM #{peek struct encoder, recording_title} old_encoders
    mapM_ freeStablePtr =<< mapM #{peek struct encoder, channel_icon} old_encoders
    free =<< #{peek struct status, encoders} status_ptr
    -- Show the clock when the encoders are unchanged from the last status check
    peek status_ptr >>= \status -> poke status_ptr status { statusEncoders = encoders, statusShowClock = showClock }
    c_widget_schedule_redraw widget_ptr

resizeHandler _ _ _ d_ptr = do
    Status _ _ widget_ptr w h _ _ _ _ <- peek (castPtr d_ptr)
    c_widget_set_size widget_ptr w h

redrawHandler _ d_ptr = do
    let status_ptr = castPtr d_ptr
    Status _ window_ptr _ w h _ _ show_clock encoders <- peek status_ptr
    xpsurface <- XP.mkSurface =<< c_window_get_surface window_ptr
    XP.manageSurface xpsurface
    case (encoders, show_clock) of (_:_, False) -> do drawStatus xpsurface (fromIntegral w) (fromIntegral h) encoders
                                                      pokeByteOff status_ptr #{offset struct status, show_clock} True
                                   otherwise -> drawClock xpsurface (fromIntegral w) (fromIntegral h) encoders

buttonHandler _ input_ptr _ _ state d_ptr = do
    return ()

touchDownHandler _ input_ptr _ _ _ _ _ d_ptr = do
    return ()

keyHandler _ _ _ _ _ state d_ptr = do
    return ()

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
        window_ptr <- c_window_create display_ptr
        widget_ptr <- c_window_add_widget window_ptr $ castPtr status_ptr
        check_fd <- c_timerfd_create clockMonotonic tfdCloexec
        check_task <- Task <$> mkTimerTaskForeign statusCheck
        poke status_ptr (Status display_ptr window_ptr widget_ptr w h check_fd check_task True [])
        c_window_set_user_data window_ptr $ castPtr status_ptr
        alertCreate display_ptr window_ptr >>= \alert_fp -> withForeignPtr alert_fp $ \alert_ptr -> do
            FC.addForeignPtrFinalizer status_fp $ do
                finalizeForeignPtr alert_fp
                c_display_unwatch_fd display_ptr check_fd
                closeFd check_fd
                windowDestroy widget_ptr window_ptr
        return status_fp

alertCheck t_ptr _ = do
    void . forkIO $ do
    let alert_ptr = t_ptr `plusPtr` negate #{offset struct alert, check_task}
    Alert widget_ptr check_fd _ _ _ _ _ _ _ _ _ _ <- peek alert_ptr
    fdRead check_fd #{size uint64_t}
    babyMonitorHealth <- getBabyMonitorStatus
    isHDHomeRunHealthy <- getHDHomeRunStatus
    isMythTVHealthy <- getMythTVStatus
    isPiholeHealthy <- getPiholeStatus
    hueHealth <- getHueStatus
    peek alert_ptr >>= \alert -> poke alert_ptr alert { alertBabyMonitorHealth = babyMonitorHealth, alertHDHomeRunHealth = isHDHomeRunHealthy, alertMythTVHealth = isMythTVHealthy, alertPiholeHealth = isPiholeHealthy, alertHueHealth = hueHealth }
    unless (babyMonitorHealth == healthy && isHDHomeRunHealthy && isMythTVHealthy && isPiholeHealthy && hueHealth == healthy) $ c_widget_schedule_redraw widget_ptr

alertHide node_fps t_ptr _ = do
    let alert_ptr = t_ptr `plusPtr` negate #{offset struct alert, hide_task}
    Alert widget_ptr _ _ hide_fd _ _ _ _ _ _ _ _ <- peek alert_ptr
    fdRead hide_fd #{size uint64_t}
    mapM_ finalizeForeignPtr node_fps
    free =<< #{peek struct alert, node_buttons} alert_ptr
    peek alert_ptr >>= \alert -> poke alert_ptr alert { alertShowDashboard = False, alertNodeButtons = [] }
    c_widget_schedule_redraw widget_ptr

alertResizeHandler _ _ _ d_ptr = do
    Alert widget_ptr _ _ _ _ _ _ _ _ _ _ _ <- peek (castPtr d_ptr)
    c_widget_set_allocation widget_ptr 0 400 800 80

alertRedrawHandler _ d_ptr = do
    Alert widget_ptr _ _ _ _ babyMonitorHealth isHDHomeRunHealthy isMythTVHealthy isPiholeHealthy hueHealth showDashboard _ <- peek (castPtr d_ptr)
    xp <- c_widget_cairo_create widget_ptr
    xpsurface <- XP.mkSurface =<< c_cairo_get_target xp
    c_cairo_destroy xp
    drawAlert xpsurface showDashboard babyMonitorHealth isHDHomeRunHealthy isMythTVHealthy isPiholeHealthy hueHealth =<< c_widget_get_last_time widget_ptr
    unless (babyMonitorHealth == healthy && isHDHomeRunHealthy && isMythTVHealthy && isPiholeHealthy && hueHealth == healthy) $ c_widget_schedule_redraw widget_ptr

alertTouchDownHandler _ input_ptr _ _ _ x _ d_ptr = do
    let alert_ptr = castPtr d_ptr
    Alert widget_ptr _ _ hide_fd (Task hide_fp) _ _ _ _ _ showDashboard nodeButton <- peek alert_ptr
    -- If touch hits k8s region
    when (nodeButton == [] && showDashboard && x < 111) $ do
        node_fps <- zipWithM (nodeButtonCreate widget_ptr) ["control-plane-1", "control-plane-2", "control-plane-3"] [200, 400, 600]
        freeHaskellFunPtr hide_fp
        hide_task <- Task <$> mkTimerTaskForeign (alertHide node_fps)
        withForeignPtrs node_fps $ \node_ptrs -> peek alert_ptr >>= \alert -> poke alert_ptr alert { alertNodeButtons = node_ptrs, alertHideTask = hide_task }
    pokeByteOff alert_ptr #{offset struct alert, show_dashboard} True
    with (ITimerSpec (TimeSpec 0 0) (TimeSpec 30 0)) $ \its_ptr -> c_timerfd_settime hide_fd 0 its_ptr nullPtr
    c_widget_schedule_redraw widget_ptr

alertCreate display_ptr window_ptr = do
    mallocForeignPtr >>= \alert_fp -> withForeignPtr alert_fp $ \alert_ptr -> do
        widget_ptr <- c_window_add_subsurface window_ptr (castPtr alert_ptr) subsurfaceDesynchronized
        check_fd <- c_timerfd_create clockMonotonic tfdCloexec
        check_task <- Task <$> mkTimerTaskForeign alertCheck
        hide_fd <- c_timerfd_create clockMonotonic tfdCloexec
        null_fp <- newForeignPtr_ nullPtr
        hide_task <- Task <$> mkTimerTaskForeign (alertHide [])
        poke alert_ptr (Alert widget_ptr check_fd check_task hide_fd hide_task healthy True True True healthy False [])
        redraw_funp <- mkRedrawHandlerForeign alertRedrawHandler
        resize_funp <- mkResizeHandlerForeign alertResizeHandler
        touch_funp <- mkTouchDownHandlerForeign alertTouchDownHandler
        c_widget_set_redraw_handler widget_ptr redraw_funp
        c_widget_set_resize_handler widget_ptr resize_funp
        c_widget_set_touch_down_handler widget_ptr touch_funp
        s_ptr <- c_widget_get_wl_surface widget_ptr
        region_ptr <- c_wl_compositor_create_region =<< c_display_get_compositor display_ptr
        c_wl_surface_set_input_region s_ptr region_ptr
        c_wl_region_destroy region_ptr
        c_display_watch_fd display_ptr check_fd epollin (#{ptr struct alert, check_task} alert_ptr)
        c_display_watch_fd display_ptr hide_fd epollin (#{ptr struct alert, hide_task} alert_ptr)
        with (ITimerSpec (TimeSpec 150 0) (TimeSpec 15 0)) $ \its_ptr -> c_timerfd_settime check_fd 0 its_ptr nullPtr
        with (ITimerSpec (TimeSpec 0 0) (TimeSpec 0 0)) $ \its_ptr -> c_timerfd_settime hide_fd 0 its_ptr nullPtr
        FC.addForeignPtrFinalizer alert_fp $ do
            c_display_unwatch_fd display_ptr check_fd
            c_display_unwatch_fd display_ptr hide_fd
            closeFd check_fd
            closeFd hide_fd
            withForeignPtr alert_fp $ free <=< #{peek struct alert, node_buttons}
            freeHaskellFunPtr redraw_funp
            freeHaskellFunPtr resize_funp
            freeHaskellFunPtr touch_funp
            c_widget_destroy widget_ptr
        return alert_fp

nodeButtonRedrawHandler _ d_ptr = do
    NodeButton widget_ptr hostname <- peek (castPtr d_ptr)
    allocation <- alloca $ \allocation_ptr -> do
        c_widget_get_allocation widget_ptr allocation_ptr
        allocation <- peek allocation_ptr
        return allocation
    xp <- c_widget_cairo_create widget_ptr
    xpsurface <- XP.mkSurface =<< c_cairo_get_target xp
    c_cairo_destroy xp
    -- Hacky draw
    when (hostname == "control-plane-1") $ drawBlank xpsurface
    drawNodeButton xpsurface allocation

nodeButtonTouchDownHandler _ input_ptr _ _ _ x y d_ptr = do
    let node_ptr = castPtr d_ptr
    NodeButton widget_ptr hostname <- peek node_ptr
    c_widget_schedule_redraw widget_ptr

nodeButtonCreate alert_ptr hostname x = do
    mallocForeignPtr >>= \node_fp -> withForeignPtr node_fp $ \node_ptr -> do
        widget_ptr <- c_widget_add_widget (castPtr alert_ptr) (castPtr node_ptr)
        poke node_ptr (NodeButton widget_ptr hostname)
        redraw_funp <- mkRedrawHandlerForeign nodeButtonRedrawHandler
        touch_funp <- mkTouchDownHandlerForeign nodeButtonTouchDownHandler
        c_widget_set_redraw_handler widget_ptr redraw_funp
        c_widget_set_touch_down_handler widget_ptr touch_funp
        c_widget_set_allocation widget_ptr (x - 60) 400 120 80
        FC.addForeignPtrFinalizer node_fp $ do
            withForeignPtr node_fp $ free <=< #{peek struct node_button, hostname}
            freeHaskellFunPtr redraw_funp
            freeHaskellFunPtr touch_funp
            c_widget_destroy widget_ptr
        return node_fp

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

desktopShellGrabCursor d_ptr _ _ = pokeByteOff d_ptr #{offset struct desktop, grab_cursor} (unCursorType cursorLeftPtr)

backgroundCreate desktop_ptr = do
    mallocForeignPtr >>= \bg_fp -> withForeignPtr bg_fp $ \bg_ptr -> do
        display_ptr <- desktopDisplay <$> peek desktop_ptr
        base <- Surface <$> mkSurfaceConfigureForeign backgroundConfigure
        window_ptr <- c_window_create_custom display_ptr
        peek bg_ptr >>= \bg -> poke bg_ptr bg { backgroundSurface = base, backgroundWindow = window_ptr }
        widget_ptr <- c_window_add_widget window_ptr (castPtr bg_ptr)
        pokeByteOff bg_ptr #{offset struct background, widget} widget_ptr
        c_window_set_user_data window_ptr (castPtr bg_ptr)
        c_widget_set_transparent widget_ptr 0
        return bg_fp

grabSurfaceEnterHandler _ _ _ _ d_ptr = desktopCursorType <$> peek (castPtr d_ptr)

grabSurfaceCreate desktop_ptr = do
    Desktop display_ptr ds_ptr _ _ _ _ <- peek desktop_ptr
    window_ptr <- c_window_create_custom display_ptr
    pokeByteOff desktop_ptr #{offset struct desktop, grab_window} window_ptr
    c_window_set_user_data window_ptr (castPtr desktop_ptr)
    s <- c_window_get_wl_surface window_ptr
    c_weston_desktop_shell_set_grab_surface ds_ptr s
    widget_ptr <- c_window_add_widget window_ptr (castPtr desktop_ptr)
    c_widget_set_allocation widget_ptr 0 0 1 1
    c_widget_set_enter_handler widget_ptr =<< mkGrabSurfaceEnterHandlerForeign grabSurfaceEnterHandler
    pokeByteOff desktop_ptr #{offset struct desktop, grab_widget} widget_ptr

outputInit o_ptr desktop_ptr = do
    ds_ptr <- desktopShell <$> peek desktop_ptr
    wlo_ptr <- outputWlOutput <$> peek o_ptr
    backgroundCreate desktop_ptr >>= (`withForeignPtr` \bg_ptr -> do
        pokeByteOff o_ptr #{offset struct output, background} bg_ptr
        window_ptr <- backgroundWindow <$> peek bg_ptr
        s <- c_window_get_wl_surface window_ptr
        c_weston_desktop_shell_set_background ds_ptr wlo_ptr s)

createOutput desktop_ptr id = do
    Desktop display_ptr ds_ptr _ _ _ gc <- peek desktop_ptr
    wlo_ptr <- c_display_bind display_ptr id c_wl_output_interface 2
    mallocForeignPtr >>= (`withForeignPtr` \o_ptr -> do
        pokeByteOff o_ptr #{offset struct output, output} wlo_ptr
        pokeByteOff desktop_ptr #{offset struct desktop, output} o_ptr
        when (ds_ptr /= nullPtr) $ outputInit o_ptr desktop_ptr)

globalHandler _ id interface_cs _ d_ptr = do
    let desktop_ptr = castPtr d_ptr
    interface <- peekCString interface_cs
    case interface of
        "weston_desktop_shell" -> do display_ptr <- desktopDisplay <$> peek desktop_ptr
                                     ds_ptr <- castPtr <$> c_display_bind display_ptr id c_weston_desktop_shell_interface 1
                                     pokeByteOff desktop_ptr #{offset struct desktop, shell} ds_ptr
                                     l_ptr <- new =<< Listener <$> mkDesktopShellConfigureForeign desktopShellConfigure
                                                               <*> mkDesktopShellPrepareLockSurfaceForeign desktopShellPrepareLockSurface
                                                               <*> mkDesktopShellGrabCursorForeign desktopShellGrabCursor
                                     c_weston_desktop_shell_add_listener ds_ptr l_ptr desktop_ptr
                                     c_weston_desktop_shell_desktop_ready ds_ptr
        "wl_output" -> createOutput desktop_ptr id
        _ -> return ()

globalHandlerRemove _ _ interface_cs _ d_ptr = do
    interface <- peekCString interface_cs
    case interface of
        "wl_output" -> outputDestroy =<< desktopOutput <$> peek (castPtr d_ptr)
        _ -> return ()

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
    Output wlo_ptr bg_ptr <- peek o_ptr
    peek bg_ptr >>= \(Background _ window_ptr widget_ptr) -> windowDestroy widget_ptr window_ptr
    c_wl_output_destroy wlo_ptr

desktopCreate = do
    mallocForeignPtr >>= \desktop_fp -> do
        FC.addForeignPtrFinalizer desktop_fp (withForeignPtr desktop_fp $ \desktop_ptr -> peek desktop_ptr >>= \(Desktop _ ds_ptr o_ptr window_ptr widget_ptr _) -> do
            windowDestroy widget_ptr window_ptr
            outputDestroy o_ptr
            c_weston_desktop_shell_destroy ds_ptr)
        return desktop_fp

main = do
    global_handler_fp <- mkGlobalHandlerForeign globalHandler
    global_handler_remove_fp <- mkGlobalHandlerRemoveForeign globalHandlerRemove
    displayCreate global_handler_fp global_handler_remove_fp >>= (`withForeignPtr` \display_ptr -> do
        desktopCreate >>= (`withForeignPtr` \desktop_ptr -> do
            statusCreate display_ptr 800 480 >>= (`withForeignPtr` \status_ptr -> do
                pokeByteOff desktop_ptr #{offset struct desktop, display} display_ptr
                c_display_set_user_data display_ptr (castPtr desktop_ptr)
                c_display_set_global_handler display_ptr global_handler_fp
                c_display_set_global_handler_remove display_ptr global_handler_remove_fp
                o_ptr <- desktopOutput <$> peek desktop_ptr
                bg_ptr <- outputBackground <$> peek o_ptr
                -- Is this safe? Is bg_ptr uninitialised?
                when (bg_ptr == nullPtr) $ outputInit o_ptr desktop_ptr
                grabSurfaceCreate desktop_ptr
                statusConfigure status_ptr
                c_display_run display_ptr)))

-- f([fps]) -> f([ptrs])
withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO ()
withForeignPtrs [] _ = return ()
withForeignPtrs (fp:fps) f = withForeignPtr fp $ \ptr -> withForeignPtrs' [ptr] fps f

withForeignPtrs' [] [] f = return ()
withForeignPtrs' ptrs [] f = f ptrs >> return ()
withForeignPtrs' ptrs (fp:fps) f = withForeignPtr fp $ \ptr -> withForeignPtrs' (ptrs ++ [ptr]) fps f

