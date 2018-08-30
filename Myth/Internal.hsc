{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Myth.Internal where
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Graphics.Rendering.Cairo.Types as XP
import System.Posix.Types

#include <sys/epoll.h>
#include <sys/timerfd.h>
#include <wayland-client.h>
#include "../C/window.h"
#include "../C/weston-desktop-shell-client-protocol.h"
#include "../C/hsmyth.h"

newtype CursorType = CursorType { unCursorType :: CInt }
    deriving (Eq,Show)
#enum CursorType, CursorType, CURSOR_LEFT_PTR

newtype WlPointerButtonState = WlPointerButtonState { unWlPointerButtonState :: CInt }
    deriving (Eq,Show)
#enum WlPointerButtonState, WlPointerButtonState, WL_POINTER_BUTTON_STATE_PRESSED

newtype WlKeyboardKeyState = WlKeyboardKeyState { unWlKeyboardKeyState :: CInt }
    deriving (Eq,Show)
#enum WlKeyboardKeyState, WlKeyboardKeyState, WL_KEYBOARD_KEY_STATE_PRESSED

newtype TimerFdOption = TimerFdOption { unTimerFdOption :: CInt }
    deriving (Eq,Show)
#enum TimerFdOption, TimerFdOption, CLOCK_MONOTONIC, TFD_CLOEXEC

newtype EpollOperation = EpollOperation { unEpollOperation :: CInt }
    deriving (Eq,Show)
#enum EpollOperation, EpollOperation, EPOLLIN

data WlOutputInterface
foreign import ccall unsafe "&wl_output_interface"
    c_wl_output_interface :: Ptr WlOutputInterface
foreign import ccall unsafe "&weston_desktop_shell_interface"
    c_weston_desktop_shell_interface :: Ptr WlOutputInterface

data Display
data WestonDesktopShell
data Window
data Widget
data WlOutput
data WlSurface
data Input

data Desktop = Desktop { desktopDisplay    :: Ptr Display
                       , desktopShell      :: Ptr WestonDesktopShell
                       , desktopOutput     :: Ptr Output
                       , desktopWindow     :: Ptr Window
                       , desktopWidget     :: Ptr Widget
                       , desktopCursorType :: CursorType
                       }
instance Storable Desktop where
    sizeOf _    = #{size struct desktop}
    alignment _ = #{alignment struct desktop}
    peek ptr = do
        d_ptr      <- #{peek struct desktop, display} ptr
        s_ptr      <- #{peek struct desktop, shell} ptr
        o_ptr      <- #{peek struct desktop, output} ptr
        window_ptr <- #{peek struct desktop, grab_window} ptr
        widget_ptr <- #{peek struct desktop, grab_widget} ptr
        c          <- #{peek struct desktop, grab_cursor} ptr
        return (Desktop d_ptr s_ptr o_ptr window_ptr widget_ptr (CursorType c))
    poke ptr (Desktop d_ptr s_ptr o_ptr window_ptr widget_ptr c) = do
        #{poke struct desktop, display} ptr d_ptr
        #{poke struct desktop, shell} ptr s_ptr
        #{poke struct desktop, output} ptr o_ptr
        #{poke struct desktop, grab_window} ptr window_ptr
        #{poke struct desktop, grab_widget} ptr widget_ptr
        #{poke struct desktop, grab_cursor} ptr (unCursorType c)

data Surface = Surface (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()))
instance Storable Surface where
    sizeOf _    = #{size struct surface}
    alignment _ = #{alignment struct surface}
    peek ptr = do
        c_funp <- #{peek struct surface, configure} ptr
        return (Surface c_funp)
    poke ptr (Surface c_funp) = do
        #{poke struct surface, configure} ptr c_funp

data Listener = Listener (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr WlSurface -> Int32 -> Int32 -> IO ()))
                         (FunPtr (Ptr () -> Ptr WestonDesktopShell -> IO ()))
                         (FunPtr (Ptr () -> Ptr WestonDesktopShell -> CursorType -> IO ()))
instance Storable Listener where
    sizeOf _    = #{size struct weston_desktop_shell_listener}
    alignment _ = #{alignment struct weston_desktop_shell_listener}
    peek ptr = do
        c_funp   <- #{peek struct weston_desktop_shell_listener, configure} ptr
        pls_funp <- #{peek struct weston_desktop_shell_listener, prepare_lock_surface} ptr
        gc_funp  <- #{peek struct weston_desktop_shell_listener, grab_cursor} ptr
        return (Listener c_funp pls_funp gc_funp)
    poke ptr (Listener c_funp pls_funp gc_funp) = do
        #{poke struct weston_desktop_shell_listener, configure} ptr c_funp
        #{poke struct weston_desktop_shell_listener, prepare_lock_surface} ptr pls_funp
        #{poke struct weston_desktop_shell_listener, grab_cursor} ptr gc_funp

data Background = Background { backgroundSurface :: Surface
                             , backgroundWindow  :: Ptr Window
                             , backgroundWidget  :: Ptr Widget
                             }
instance Storable Background where
    sizeOf _    = #{size struct background}
    alignment _ = #{alignment struct background}
    peek ptr = do
        base       <- #{peek struct background, base} ptr
        window_ptr <- #{peek struct background, window} ptr
        widget_ptr <- #{peek struct background, widget} ptr
        return (Background base window_ptr widget_ptr)
    poke ptr (Background base window_ptr widget_ptr) = do
        #{poke struct background, base} ptr base
        #{poke struct background, window} ptr window_ptr
        #{poke struct background, widget} ptr widget_ptr

data Output = Output { outputWlOutput   :: Ptr WlOutput
                     , outputBackground :: Ptr Background
                     }
instance Storable Output where
    sizeOf _    = #{size struct output}
    alignment _ = #{alignment struct output}
    peek ptr = do
        o_ptr  <- #{peek struct output, output} ptr
        bg_ptr <- #{peek struct output, background} ptr
        return (Output o_ptr bg_ptr)
    poke ptr (Output o_ptr bg_ptr) = do
        #{poke struct output, output} ptr o_ptr
        #{poke struct output, background} ptr bg_ptr

data TimeSpec = TimeSpec { timeSpecTvSec  :: CLong
                         , timeSpecTvNsec :: CLong
                         }
instance Storable TimeSpec where
    sizeOf _    = #{size struct timespec}
    alignment _ = #{alignment struct timespec}
    peek ptr = do
        tv_sec <- #{peek struct timespec, tv_sec} ptr
        tv_nsec <- #{peek struct timespec, tv_nsec} ptr
        return (TimeSpec tv_sec tv_nsec)
    poke ptr (TimeSpec tv_sec tv_nsec) = do
        #{poke struct timespec, tv_sec} ptr tv_sec
        #{poke struct timespec, tv_nsec} ptr tv_nsec

data ITimerSpec = ITimerSpec { iTimerInterval :: TimeSpec
                             , iTimerValue    :: TimeSpec
                             }
instance Storable ITimerSpec where
    sizeOf _    = #{size struct itimerspec}
    alignment _ = #{alignment struct itimerspec}
    peek ptr = do
        it_interval <- #{peek struct itimerspec, it_interval} ptr
        it_value <- #{peek struct itimerspec, it_value} ptr
        return (ITimerSpec it_interval it_value)
    poke ptr (ITimerSpec it_interval it_value) = do
        #{poke struct itimerspec, it_interval} ptr it_interval
        #{poke struct itimerspec, it_value} ptr it_value

data Task = Task (FunPtr (Ptr Task -> Word32 -> IO ()))
instance Storable Task where
    sizeOf _    = #{size struct task}
    alignment _ = #{alignment struct task}
    peek ptr = do
        run_funp <- #{peek struct task, run} ptr
        return (Task run_funp)
    poke ptr (Task run_funp) = do
        #{poke struct task, run} ptr run_funp

data Encoder = Encoder { encoderIsConnected    :: Bool
                       , encoderIsActive       :: Bool
                       , encoderRecordingTitle :: String
                       }
    deriving (Eq)
instance Storable Encoder where
    sizeOf _    = #{size struct encoder}
    alignment _ = #{alignment struct status}
    peek ptr = do
        is_connected <- #{peek struct encoder, is_connected} ptr
        is_active <- #{peek struct encoder, is_active} ptr
        recording_title <- peekCString =<< #{peek struct encoder, recording_title} ptr
        return (Encoder is_connected is_active recording_title)
    poke ptr (Encoder is_connected is_active recording_title) = do
        #{poke struct encoder, is_connected} ptr is_connected
        #{poke struct encoder, is_active} ptr is_active
        #{poke struct encoder, recording_title} ptr =<< newCString recording_title

data Status = Status { statusDisplay     :: Ptr Display
                     , statusWindow      :: Ptr Window
                     , statusWidget      :: Ptr Widget
                     , statusWidth       :: Int32
                     , statusHeight      :: Int32
                     , statusCheckFd     :: Fd
                     , statusCheckTask   :: Task
                     , statusShowClock   :: Bool
                     , statusNumEncoders :: CSize
                     , statusEncoders    :: [Encoder]
                     }
instance Storable Status where
    sizeOf _    = #{size struct status}
    alignment _ = #{alignment struct status}
    peek ptr = do
        display_ptr <- #{peek struct status, display} ptr
        window_ptr <- #{peek struct status, window} ptr
        widget_ptr <- #{peek struct status, widget} ptr
        width <- #{peek struct status, width} ptr
        height <- #{peek struct status, height} ptr
        check_fd <- #{peek struct status, check_fd} ptr
        check_task <- #{peek struct status, check_task} ptr
        show_clock <- #{peek struct status, show_clock} ptr
        num_encoders <- #{peek struct status, num_encoders} ptr
        encoders <- peekArray (fromIntegral num_encoders) =<< #{peek struct status, encoders} ptr
        return (Status display_ptr window_ptr widget_ptr width height check_fd check_task show_clock num_encoders encoders)
    poke ptr (Status display_ptr window_ptr widget_ptr width height check_fd check_task show_clock num_encoders encoders) = do
        #{poke struct status, display} ptr display_ptr
        #{poke struct status, window} ptr window_ptr
        #{poke struct status, widget} ptr widget_ptr
        #{poke struct status, width} ptr width
        #{poke struct status, height} ptr height
        #{poke struct status, check_fd} ptr check_fd
        #{poke struct status, check_task} ptr check_task
        #{poke struct status, show_clock} ptr show_clock
        mapM_ free =<< mapM #{peek struct encoder, recording_title} =<< take <$> #{peek struct status, num_encoders} ptr <*> (iterate (flip advancePtr 1) <$> #{peek struct status, encoders} ptr :: IO [Ptr Encoder])
        free =<< #{peek struct status, encoders} ptr
        #{poke struct status, num_encoders} ptr num_encoders
        #{poke struct status, encoders} ptr =<< newArray encoders

foreign import ccall unsafe "display_bind"
    c_display_bind :: Ptr Display -> Word32 -> Ptr WlOutputInterface -> CInt -> IO (Ptr WlOutput)

foreign import ccall unsafe "display_create"
    c_display_create :: CInt -> Ptr CString -> IO (Ptr Display)

foreign import ccall unsafe "&display_destroy"
    c_display_destroy :: FinalizerPtr Display

foreign import ccall unsafe "display_get_serial"
    c_display_get_serial :: Ptr Display -> IO (Word32)

foreign import ccall safe "display_run"
    c_display_run :: Ptr Display -> IO ()

foreign import ccall safe "display_set_global_handler"
    c_display_set_global_handler :: Ptr Display -> FunPtr (Ptr Display -> Word32 -> CString -> Word32 -> Ptr () -> IO ()) -> IO ()

foreign import ccall safe "display_set_global_handler_remove"
    c_display_set_global_handler_remove :: Ptr Display -> FunPtr (Ptr Display -> Word32 -> CString -> Word32 -> Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "display_set_user_data"
    c_display_set_user_data :: Ptr Display -> Ptr () -> IO ()

foreign import ccall unsafe "display_unwatch_fd"
    c_display_unwatch_fd :: Ptr Display -> Fd -> IO ()

foreign import ccall unsafe "display_watch_fd"
    c_display_watch_fd :: Ptr Display -> Fd -> EpollOperation -> Ptr Task -> IO ()

foreign import ccall unsafe "timerfd_create"
    c_timerfd_create :: TimerFdOption -> TimerFdOption -> IO (Fd)

foreign import ccall unsafe "timerfd_settime"
    c_timerfd_settime :: Fd -> CInt -> Ptr ITimerSpec -> Ptr ITimerSpec -> IO ()

foreign import ccall unsafe "window_add_widget"
    c_window_add_widget :: Ptr Window -> Ptr () -> IO (Ptr Widget)

foreign import ccall unsafe "window_create"
    c_window_create :: Ptr Display -> IO (Ptr Window)

foreign import ccall unsafe "window_create_custom"
    c_window_create_custom :: Ptr Display -> IO (Ptr Window)

foreign import ccall unsafe "window_destroy"
    c_window_destroy :: Ptr Window -> IO ()

foreign import ccall unsafe "window_get_surface"
    c_window_get_surface :: Ptr Window -> IO (Ptr XP.Surface)

foreign import ccall unsafe "window_get_user_data"
    c_window_get_user_data :: Ptr Window -> IO (Ptr ())

foreign import ccall unsafe "window_get_wl_surface"
    c_window_get_wl_surface :: Ptr Window -> IO (Ptr WlSurface)

foreign import ccall unsafe "window_move"
    c_window_move :: Ptr Window -> Ptr Input -> Word32 -> IO ()

foreign import ccall unsafe "window_schedule_resize"
    c_window_schedule_resize :: Ptr Window -> Int32 -> Int32 -> IO ()

foreign import ccall unsafe "window_set_key_handler"
    c_window_set_key_handler :: Ptr Window -> FunPtr (Ptr Window -> Ptr Input -> Word32 -> Word32 -> Word32 -> WlKeyboardKeyState -> Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "window_set_user_data"
    c_window_set_user_data :: Ptr Window -> Ptr () -> IO ()

foreign import ccall unsafe "widget_destroy"
    c_widget_destroy :: Ptr Widget -> IO ()

foreign import ccall unsafe "widget_schedule_redraw"
    c_widget_schedule_redraw :: Ptr Widget -> IO ()

foreign import ccall unsafe "widget_schedule_resize"
    c_widget_schedule_resize :: Ptr Widget -> Int32 -> Int32 -> IO ()

foreign import ccall unsafe "widget_set_allocation"
    c_widget_set_allocation :: Ptr Widget -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall unsafe "widget_set_enter_handler"
    c_widget_set_enter_handler :: Ptr Widget -> FunPtr (Ptr Widget -> Ptr Input -> Float -> Float -> Ptr () -> IO (CursorType)) -> IO ()

foreign import ccall unsafe "widget_set_button_handler"
    c_widget_set_button_handler :: Ptr Widget -> FunPtr (Ptr Widget -> Ptr Input -> Word32 -> Word32 -> WlPointerButtonState -> Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "widget_set_redraw_handler"
    c_widget_set_redraw_handler :: Ptr Widget -> FunPtr (Ptr Widget -> Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "widget_set_resize_handler"
    c_widget_set_resize_handler :: Ptr Widget -> FunPtr (Ptr Widget -> Int32 -> Int32 -> Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "widget_set_size"
    c_widget_set_size :: Ptr Widget -> Int32 -> Int32 -> IO ()

foreign import ccall unsafe "widget_set_touch_down_handler"
    c_widget_set_touch_down_handler :: Ptr Widget -> FunPtr (Ptr Widget -> Ptr Input -> Word32 -> Word32 -> Int32 -> Float -> Float -> Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "widget_set_transparent"
    c_widget_set_transparent :: Ptr Widget -> CInt -> IO ()

foreign import ccall unsafe "wl_proxy_destroy"
    c_wl_output_destroy :: Ptr WlOutput -> IO ()

foreign import ccall unsafe "wl_proxy_add_listener"
    c_weston_desktop_shell_add_listener :: Ptr WestonDesktopShell -> Ptr Listener -> Ptr Desktop -> IO ()

foreign import ccall unsafe "wl_proxy_destroy"
    c_weston_desktop_shell_destroy :: Ptr WestonDesktopShell -> IO ()

foreign import ccall unsafe "wl_proxy_get_user_data"
    c_wl_surface_get_user_data :: Ptr WlSurface -> IO (Ptr ())

foreign import ccall unsafe "wl_proxy_marshal"
    c_wl_proxy_marshal :: Ptr WestonDesktopShell -> CInt -> Ptr () -> Ptr () -> IO ()

c_weston_desktop_shell_desktop_ready :: Ptr WestonDesktopShell -> IO ()
c_weston_desktop_shell_desktop_ready ds_ptr =
    c_wl_proxy_marshal ds_ptr #{const WESTON_DESKTOP_SHELL_DESKTOP_READY} nullPtr nullPtr

c_weston_desktop_shell_unlock :: Ptr WestonDesktopShell -> IO ()
c_weston_desktop_shell_unlock ds_ptr =
    c_wl_proxy_marshal ds_ptr #{const WESTON_DESKTOP_SHELL_UNLOCK} nullPtr nullPtr

c_weston_desktop_shell_set_background :: Ptr WestonDesktopShell -> Ptr WlOutput -> Ptr WlSurface -> IO ()
c_weston_desktop_shell_set_background ds_ptr wlo_ptr s_ptr =
    c_wl_proxy_marshal ds_ptr #{const WESTON_DESKTOP_SHELL_SET_BACKGROUND} (castPtr wlo_ptr) (castPtr s_ptr)

c_weston_desktop_shell_set_grab_surface :: Ptr WestonDesktopShell -> Ptr WlSurface -> IO ()
c_weston_desktop_shell_set_grab_surface ds_ptr s_ptr =
    c_wl_proxy_marshal ds_ptr #{const WESTON_DESKTOP_SHELL_SET_GRAB_SURFACE} (castPtr s_ptr) nullPtr

foreign import ccall unsafe "wrapper"
    mkStatusCheckForeign ::            (Ptr Task -> Word32 -> IO ()) ->
                            IO (FunPtr (Ptr Task -> Word32 -> IO ()))

foreign import ccall unsafe "wrapper"
    mkResizeHandlerForeign ::            (Ptr Widget -> Int32 -> Int32 -> Ptr () -> IO ()) ->
                              IO (FunPtr (Ptr Widget -> Int32 -> Int32 -> Ptr () -> IO ()))

foreign import ccall unsafe "wrapper"
    mkRedrawHandlerForeign ::            (Ptr Widget -> Ptr () -> IO ()) ->
                              IO (FunPtr (Ptr Widget -> Ptr () -> IO ()))

foreign import ccall unsafe "wrapper"
    mkButtonHandlerForeign ::            (Ptr Widget -> Ptr Input -> Word32 -> Word32 -> WlPointerButtonState -> Ptr () -> IO ()) ->
                              IO (FunPtr (Ptr Widget -> Ptr Input -> Word32 -> Word32 -> WlPointerButtonState -> Ptr () -> IO ()))

foreign import ccall unsafe "wrapper"
    mkTouchDownHandlerForeign ::            (Ptr Widget -> Ptr Input -> Word32 -> Word32 -> Int32 -> Float -> Float -> Ptr () -> IO ()) ->
                                 IO (FunPtr (Ptr Widget -> Ptr Input -> Word32 -> Word32 -> Int32 -> Float -> Float -> Ptr () -> IO ()))

foreign import ccall unsafe "wrapper"
    mkKeyHandlerForeign ::            (Ptr Window -> Ptr Input -> Word32 -> Word32 -> Word32 -> WlKeyboardKeyState -> Ptr () -> IO ()) ->
                           IO (FunPtr (Ptr Window -> Ptr Input -> Word32 -> Word32 -> Word32 -> WlKeyboardKeyState -> Ptr () -> IO ()))

foreign import ccall unsafe "wrapper"
    mkSurfaceConfigureForeign ::            (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()) ->
                                 IO (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()))

foreign import ccall unsafe "wrapper"
    mkDesktopShellConfigureForeign ::            (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr WlSurface -> Int32 -> Int32 -> IO ()) ->
                                      IO (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr WlSurface -> Int32 -> Int32 -> IO ()))

foreign import ccall unsafe "wrapper"
    mkDesktopShellPrepareLockSurfaceForeign ::            (Ptr () -> Ptr WestonDesktopShell -> IO ()) ->
                                               IO (FunPtr (Ptr () -> Ptr WestonDesktopShell -> IO ()))

foreign import ccall unsafe "wrapper"
    mkDesktopShellGrabCursorForeign ::            (Ptr () -> Ptr WestonDesktopShell -> CursorType -> IO ()) ->
                                       IO (FunPtr (Ptr () -> Ptr WestonDesktopShell -> CursorType -> IO ()))

foreign import ccall unsafe "wrapper"
    mkGrabSurfaceEnterHandlerForeign ::            (Ptr Widget -> Ptr Input -> Float -> Float -> Ptr () -> IO (CursorType)) ->
                                        IO (FunPtr (Ptr Widget -> Ptr Input -> Float -> Float -> Ptr () -> IO (CursorType)))

foreign import ccall unsafe "wrapper"
    mkGlobalHandlerForeign ::            (Ptr Display -> Word32 -> CString -> Word32 -> Ptr () -> IO ()) ->
                              IO (FunPtr (Ptr Display -> Word32 -> CString -> Word32 -> Ptr () -> IO ()))

foreign import ccall unsafe "wrapper"
    mkGlobalHandlerRemoveForeign ::            (Ptr Display -> Word32 -> CString -> Word32 -> Ptr () -> IO ()) ->
                                    IO (FunPtr (Ptr Display -> Word32 -> CString -> Word32 -> Ptr () -> IO ()))

foreign import ccall safe "dynamic"
    mkSurfaceConfigure :: FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()) ->
                                 (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ())

