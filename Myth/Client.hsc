module Myth.Client (statusCreate) where
import Myth.Internal
import Myth.Status
import Myth.Alert
import Myth.Render
import Control.Concurrent
import Control.Monad
import Foreign hiding (void)
import qualified Foreign.Concurrent as FC
import qualified Graphics.Rendering.Cairo.Types as XP
import System.Posix.IO

#include "hsmyth.h"

statusCheck t_ptr _ = do
    void . forkIO $ do
    let status_ptr = t_ptr `plusPtr` negate #{offset struct status, check_task}
    Status {statusWidget = widget, statusCheckFd = checkFd} <- peek status_ptr
    fdRead checkFd #{size uint64_t}
    encoders <- getEncodersStatus
    peek status_ptr >>= \status -> do
        -- Free any existing recording_title CStrings and channel_icon StablePtrs, then the array
        old_encoders <- take <$> #{peek struct status, num_encoders} status_ptr <*> (iterate (flip advancePtr 1) <$> #{peek struct status, encoders} status_ptr :: IO [Ptr Encoder])
        mapM_ free =<< mapM #{peek struct encoder, recording_title} old_encoders
        mapM_ freeStablePtr =<< mapM #{peek struct encoder, channel_icon} old_encoders
        free =<< #{peek struct status, encoders} status_ptr
        poke status_ptr status { statusEncoders = encoders
                                 -- Show the clock when the encoders are unchanged from the last status check
                               , statusShowClock = (encoders == statusEncoders status)
                               }
    c_widget_schedule_redraw widget

statusResizeHandler _ _ _ d_ptr = do
    Status {statusWidget = widget, statusWidth = w, statusHeight = h} <- peek (castPtr d_ptr)
    c_widget_set_size widget w h

statusRedrawHandler _ d_ptr = do
    let status_ptr = castPtr d_ptr
    Status { statusWindow = window
           , statusWidth = w
           , statusHeight = h
           , statusShowClock = showClock
           , statusEncoders = encoders
           } <- peek status_ptr
    xpsurface <- XP.mkSurface =<< c_window_get_surface window
    XP.manageSurface xpsurface
    case (encoders, showClock) of
        (_:_, False) -> do
            drawStatus xpsurface (fromIntegral w) (fromIntegral h) encoders
            pokeByteOff status_ptr #{offset struct status, show_clock} True
        otherwise -> drawClock xpsurface (fromIntegral w) (fromIntegral h) encoders

statusButtonHandler _ input_ptr _ _ state d_ptr = do
    return ()

statusTouchDownHandler _ input_ptr _ _ _ _ _ d_ptr = do
    return ()

statusKeyHandler _ _ _ _ _ state d_ptr = do
    return ()

statusCreate display_ptr w h = do
    mallocForeignPtr >>= \status_fp -> withForeignPtr status_fp $ \status_ptr -> do
        window_ptr <- c_window_create display_ptr
        widget_ptr <- c_window_add_widget window_ptr (castPtr status_ptr)
        check_fd <- c_timerfd_create clockMonotonic tfdCloexec
        check_task <- Task <$> mkTimerTaskForeign statusCheck
        poke status_ptr Status { statusDisplay = display_ptr
                               , statusWindow = window_ptr
                               , statusWidget = widget_ptr
                               , statusWidth = w
                               , statusHeight = h
                               , statusCheckFd = check_fd
                               , statusCheckTask = check_task
                               , statusShowClock = True
                               , statusEncoders = []
                               }
        c_window_set_user_data window_ptr (castPtr status_ptr)
        c_display_watch_fd display_ptr check_fd epollin (#{ptr struct status, check_task} status_ptr)
        with (ITimerSpec (TimeSpec 30 0) (TimeSpec 1 0)) $ \its_ptr -> c_timerfd_settime check_fd 0 its_ptr nullPtr
        resize_funp <- mkResizeHandlerForeign statusResizeHandler
        redraw_funp <- mkRedrawHandlerForeign statusRedrawHandler
        button_funp <- mkButtonHandlerForeign statusButtonHandler
        touch_funp <- mkTouchDownHandlerForeign statusTouchDownHandler
        key_funp <- mkKeyHandlerForeign statusKeyHandler
        c_widget_set_resize_handler widget_ptr resize_funp
        c_widget_set_redraw_handler widget_ptr redraw_funp
        c_widget_set_button_handler widget_ptr button_funp
        c_widget_set_touch_down_handler widget_ptr touch_funp
        c_window_set_key_handler window_ptr key_funp
        c_window_schedule_resize window_ptr w h
        alertCreate display_ptr window_ptr
        FC.addForeignPtrFinalizer status_fp $ do
            let Task check_funp = check_task in mapM_ freeHaskellFunPtr [ castFunPtr check_funp
                                                                        , castFunPtr resize_funp
                                                                        , castFunPtr redraw_funp
                                                                        , castFunPtr button_funp
                                                                        , castFunPtr touch_funp
                                                                        , castFunPtr key_funp
                                                                        ]
            c_display_unwatch_fd display_ptr check_fd
            closeFd check_fd
            c_widget_destroy widget_ptr
            c_window_destroy window_ptr
        return status_fp

alertCheck t_ptr _ = do
    void . forkIO $ do
    let alert_ptr = t_ptr `plusPtr` negate #{offset struct alert, check_task}
    Alert {alertWidget = widget, alertCheckFd = checkFd} <- peek alert_ptr
    fdRead checkFd #{size uint64_t}
    babyMonitorHealth <- getBabyMonitorStatus
    isHDHomeRunHealthy <- getHDHomeRunStatus
    isMythTVHealthy <- getMythTVStatus
    isPiholeHealthy <- getPiholeStatus
    hueHealth <- getHueStatus
    peek alert_ptr >>= \alert -> poke alert_ptr alert { alertBabyMonitorHealth = babyMonitorHealth
                                                      , alertHDHomeRunHealth = isHDHomeRunHealthy
                                                      , alertMythTVHealth = isMythTVHealthy
                                                      , alertPiholeHealth = isPiholeHealthy
                                                      , alertHueHealth = hueHealth
                                                      }
    unless (all id [ babyMonitorHealth == healthy
                   , isHDHomeRunHealthy
                   , isMythTVHealthy
                   , isPiholeHealthy
                   , hueHealth == healthy
                   ]) $ c_widget_schedule_redraw widget

alertHide node_fps t_ptr _ = do
    let alert_ptr = t_ptr `plusPtr` negate #{offset struct alert, hide_task}
    Alert {alertWidget = widget, alertHideFd = hideFd} <- peek alert_ptr
    fdRead hideFd #{size uint64_t}
    mapM_ finalizeForeignPtr node_fps
    free =<< #{peek struct alert, node_buttons} alert_ptr
    peek alert_ptr >>= \alert -> poke alert_ptr alert { alertShowDashboard = False, alertNodeButtons = [] }
    c_widget_schedule_redraw widget

alertResizeHandler _ _ _ d_ptr = do
    Alert {alertWidget = widget} <- peek (castPtr d_ptr)
    c_widget_set_allocation widget 0 400 800 80

alertRedrawHandler _ d_ptr = do
    Alert { alertWidget = widget
          , alertBabyMonitorHealth = babyMonitorHealth
          , alertHDHomeRunHealth = isHDHomeRunHealthy
          , alertMythTVHealth = isMythTVHealthy
          , alertPiholeHealth = isPiholeHealthy
          , alertHueHealth = hueHealth
          , alertShowDashboard = showDashboard
          , alertNodeButtons = nodeButtons
          } <- peek (castPtr d_ptr)
    xp <- c_widget_cairo_create widget
    xpsurface <- XP.mkSurface =<< c_cairo_get_target xp
    c_cairo_destroy xp
    let hideAlert = length nodeButtons /= 0
    drawAlert xpsurface showDashboard hideAlert babyMonitorHealth isHDHomeRunHealthy isMythTVHealthy isPiholeHealthy hueHealth =<< c_widget_get_last_time widget
    unless (hideAlert || all id [ babyMonitorHealth == healthy
                                , isHDHomeRunHealthy
                                , isMythTVHealthy
                                , isPiholeHealthy
                                , hueHealth == healthy
                                ]) $ c_widget_schedule_redraw widget

alertTouchDownHandler _ input_ptr _ _ _ x _ d_ptr = do
    let alert_ptr = castPtr d_ptr
    Alert { alertWidget = widget
          , alertHideFd = hideFd
          , alertHideTask = hideTask
          , alertShowDashboard = showDashboard
          , alertNodeButtons = nodeButtons
          } <- peek alert_ptr
    let isK8sMenuOpen = nodeButtons /= []
    let shouldOpenK8sMenu = nodeButtons == [] && showDashboard && x < 111
    when shouldOpenK8sMenu $ do
        node_fps <- zipWithM (nodeButtonCreate widget) ["control-plane-1", "control-plane-2", "control-plane-3"] [200, 400, 600]
        let Task hideFp = hideTask in freeHaskellFunPtr hideFp
        hide_task <- Task <$> mkTimerTaskForeign (alertHide node_fps)
        withForeignPtrs node_fps $ \node_ptrs ->
            peek alert_ptr >>= \alert -> poke alert_ptr alert { alertNodeButtons = node_ptrs, alertHideTask = hide_task }
    pokeByteOff alert_ptr #{offset struct alert, show_dashboard} (not isK8sMenuOpen && not shouldOpenK8sMenu)
    with (ITimerSpec (TimeSpec 0 0) (TimeSpec 30 0)) $ \its_ptr -> c_timerfd_settime hideFd 0 its_ptr nullPtr
    c_widget_schedule_redraw widget

alertCreate display_ptr window_ptr = do
    mallocForeignPtr >>= \alert_fp -> withForeignPtr alert_fp $ \alert_ptr -> do
        widget_ptr <- c_window_add_subsurface window_ptr (castPtr alert_ptr) subsurfaceDesynchronized
        check_fd <- c_timerfd_create clockMonotonic tfdCloexec
        check_task <- Task <$> mkTimerTaskForeign alertCheck
        hide_fd <- c_timerfd_create clockMonotonic tfdCloexec
        hide_task <- Task <$> mkTimerTaskForeign (alertHide [])
        poke alert_ptr Alert { alertWidget = widget_ptr
                             , alertCheckFd = check_fd
                             , alertCheckTask = check_task
                             , alertHideFd = hide_fd
                             , alertHideTask = hide_task
                             , alertBabyMonitorHealth = healthy
                             , alertHDHomeRunHealth = True
                             , alertMythTVHealth = True
                             , alertPiholeHealth = True
                             , alertHueHealth = healthy
                             , alertShowDashboard = False
                             , alertNodeButtons = []
                             }
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
            let Task checkFp = check_task in freeHaskellFunPtr checkFp
            withForeignPtr alert_fp $ \alert_ptr -> do
                free =<< #{peek struct alert, node_buttons} alert_ptr
                Task hideFp <- alertHideTask <$> peek alert_ptr
                freeHaskellFunPtr hideFp
            mapM_ freeHaskellFunPtr [ castFunPtr resize_funp
                                    , castFunPtr redraw_funp
                                    , castFunPtr touch_funp
                                    ]
            c_widget_destroy widget_ptr
        return alert_fp

nodeButtonRedrawHandler _ d_ptr = do
    NodeButton {nodeButtonWidget = widget, nodeButtonHostname = hostname} <- peek (castPtr d_ptr)
    allocation <- alloca $ \allocation_ptr -> do
        c_widget_get_allocation widget allocation_ptr
        peek allocation_ptr
    xp <- c_widget_cairo_create widget
    xpsurface <- XP.mkSurface =<< c_cairo_get_target xp
    c_cairo_destroy xp
    drawNodeButton xpsurface allocation ("server" ++ show (last hostname))

nodeButtonTouchDownHandler _ input_ptr _ _ _ x y d_ptr = do
    NodeButton {nodeButtonWidget = widget} <- peek (castPtr d_ptr)
    c_widget_schedule_redraw widget

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
            mapM_ freeHaskellFunPtr [castFunPtr redraw_funp, castFunPtr touch_funp]
            c_widget_destroy widget_ptr
        return node_fp

-- f([fps]) -> f([ptrs])
withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO ()
withForeignPtrs [] _ = return ()
withForeignPtrs (fp:fps) f = withForeignPtr fp $ \ptr -> wfps [ptr] fps f
                             where wfps [] [] f = return ()
                                   wfps ptrs [] f = f ptrs >> return ()
                                   wfps ptrs (fp:fps) f = withForeignPtr fp $ \ptr -> wfps (ptrs ++ [ptr]) fps f

