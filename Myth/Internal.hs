{-# LINE 1 "Myth/Internal.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LINE 2 "Myth/Internal.hsc" #-}

module Myth.Internal where
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import qualified Graphics.Rendering.Cairo.Types as XP
import System.Posix.Types


{-# LINE 12 "Myth/Internal.hsc" #-}

{-# LINE 13 "Myth/Internal.hsc" #-}

{-# LINE 14 "Myth/Internal.hsc" #-}

{-# LINE 15 "Myth/Internal.hsc" #-}


{-# LINE 25 "Myth/Internal.hsc" #-}


{-# LINE 35 "Myth/Internal.hsc" #-}


{-# LINE 42 "Myth/Internal.hsc" #-}


{-# LINE 48 "Myth/Internal.hsc" #-}


{-# LINE 58 "Myth/Internal.hsc" #-}

newtype CursorType = CursorType { unCursorType :: CInt }
    deriving (Eq,Show)
cursorLeftPtr :: CursorType
cursorLeftPtr = CursorType 4

{-# LINE 62 "Myth/Internal.hsc" #-}

newtype TimerFdOption = TimerFdOption { unTimerFdOption :: CInt }
    deriving (Eq,Show)
clockMonotonic :: TimerFdOption
clockMonotonic = TimerFdOption 1
tfdCloexec :: TimerFdOption
tfdCloexec = TimerFdOption 524288

{-# LINE 66 "Myth/Internal.hsc" #-}

newtype EpollOperation = EpollOperation { unEpollOperation :: CInt }
    deriving (Eq,Show)
epollin :: EpollOperation
epollin = EpollOperation 1

{-# LINE 70 "Myth/Internal.hsc" #-}

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
    sizeOf _    = (48)
{-# LINE 94 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 95 "Myth/Internal.hsc" #-}
    peek ptr = do
        d_ptr      <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 97 "Myth/Internal.hsc" #-}
        s_ptr      <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 98 "Myth/Internal.hsc" #-}
        o_ptr      <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 99 "Myth/Internal.hsc" #-}
        window_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 100 "Myth/Internal.hsc" #-}
        widget_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 101 "Myth/Internal.hsc" #-}
        c          <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 102 "Myth/Internal.hsc" #-}
        return (Desktop d_ptr s_ptr o_ptr window_ptr widget_ptr (CursorType c))
    poke ptr (Desktop d_ptr s_ptr o_ptr window_ptr widget_ptr c) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr d_ptr
{-# LINE 105 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr s_ptr
{-# LINE 106 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr o_ptr
{-# LINE 107 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr window_ptr
{-# LINE 108 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr widget_ptr
{-# LINE 109 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr (unCursorType c)
{-# LINE 110 "Myth/Internal.hsc" #-}

data Surface = Surface (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()))
instance Storable Surface where
    sizeOf _    = (8)
{-# LINE 114 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 115 "Myth/Internal.hsc" #-}
    peek ptr = do
        c_funp <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 117 "Myth/Internal.hsc" #-}
        return (Surface c_funp)
    poke ptr (Surface c_funp) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr c_funp
{-# LINE 120 "Myth/Internal.hsc" #-}

data Listener = Listener (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr WlSurface -> Int32 -> Int32 -> IO ()))
                         (FunPtr (Ptr () -> Ptr WestonDesktopShell -> IO ()))
                         (FunPtr (Ptr () -> Ptr WestonDesktopShell -> CursorType -> IO ()))
instance Storable Listener where
    sizeOf _    = (24)
{-# LINE 126 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 127 "Myth/Internal.hsc" #-}
    peek ptr = do
        c_funp   <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 129 "Myth/Internal.hsc" #-}
        pls_funp <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 130 "Myth/Internal.hsc" #-}
        gc_funp  <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 131 "Myth/Internal.hsc" #-}
        return (Listener c_funp pls_funp gc_funp)
    poke ptr (Listener c_funp pls_funp gc_funp) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr c_funp
{-# LINE 134 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr pls_funp
{-# LINE 135 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr gc_funp
{-# LINE 136 "Myth/Internal.hsc" #-}

data Background = Background { backgroundSurface :: Surface
                             , backgroundWindow  :: Ptr Window
                             , backgroundWidget  :: Ptr Widget
                             }
instance Storable Background where
    sizeOf _    = (24)
{-# LINE 143 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 144 "Myth/Internal.hsc" #-}
    peek ptr = do
        base       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 146 "Myth/Internal.hsc" #-}
        window_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 147 "Myth/Internal.hsc" #-}
        widget_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 148 "Myth/Internal.hsc" #-}
        return (Background base window_ptr widget_ptr)
    poke ptr (Background base window_ptr widget_ptr) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr base
{-# LINE 151 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr window_ptr
{-# LINE 152 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr widget_ptr
{-# LINE 153 "Myth/Internal.hsc" #-}

data Output = Output { outputWlOutput   :: Ptr WlOutput
                     , outputBackground :: Ptr Background
                     }
instance Storable Output where
    sizeOf _    = (16)
{-# LINE 159 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 160 "Myth/Internal.hsc" #-}
    peek ptr = do
        o_ptr  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 162 "Myth/Internal.hsc" #-}
        bg_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 163 "Myth/Internal.hsc" #-}
        return (Output o_ptr bg_ptr)
    poke ptr (Output o_ptr bg_ptr) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr o_ptr
{-# LINE 166 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr bg_ptr
{-# LINE 167 "Myth/Internal.hsc" #-}

data TimeSpec = TimeSpec { timeSpecTvSec  :: CLong
                         , timeSpecTvNsec :: CLong
                         }
instance Storable TimeSpec where
    sizeOf _    = (16)
{-# LINE 173 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 174 "Myth/Internal.hsc" #-}
    peek ptr = do
        tv_sec <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 176 "Myth/Internal.hsc" #-}
        tv_nsec <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 177 "Myth/Internal.hsc" #-}
        return (TimeSpec tv_sec tv_nsec)
    poke ptr (TimeSpec tv_sec tv_nsec) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr tv_sec
{-# LINE 180 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr tv_nsec
{-# LINE 181 "Myth/Internal.hsc" #-}

data ITimerSpec = ITimerSpec { iTimerInterval :: TimeSpec
                             , iTimerValue    :: TimeSpec
                             }
instance Storable ITimerSpec where
    sizeOf _    = (32)
{-# LINE 187 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 188 "Myth/Internal.hsc" #-}
    peek ptr = do
        it_interval <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 190 "Myth/Internal.hsc" #-}
        it_value <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 191 "Myth/Internal.hsc" #-}
        return (ITimerSpec it_interval it_value)
    poke ptr (ITimerSpec it_interval it_value) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr it_interval
{-# LINE 194 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr it_value
{-# LINE 195 "Myth/Internal.hsc" #-}

data Task = Task (FunPtr (Ptr Task -> Word32 -> IO ()))
instance Storable Task where
    sizeOf _    = (24)
{-# LINE 199 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 200 "Myth/Internal.hsc" #-}
    peek ptr = do
        run_funp <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 202 "Myth/Internal.hsc" #-}
        return (Task run_funp)
    poke ptr (Task run_funp) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr run_funp
{-# LINE 205 "Myth/Internal.hsc" #-}

data Status = Status { statusDisplay   :: Ptr Display
                     , statusWindow    :: Ptr Window
                     , statusWidget    :: Ptr Widget
                     , statusWidth     :: Int32
                     , statusHeight    :: Int32
                     , statusCheckFd   :: Fd
                     , statusCheckTask :: Task
                     }
instance Storable Status where
    sizeOf _    = (64)
{-# LINE 216 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 217 "Myth/Internal.hsc" #-}
    peek ptr = do
        display_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 219 "Myth/Internal.hsc" #-}
        window_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 220 "Myth/Internal.hsc" #-}
        widget_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 221 "Myth/Internal.hsc" #-}
        width <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 222 "Myth/Internal.hsc" #-}
        height <- (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 223 "Myth/Internal.hsc" #-}
        check_fd <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 224 "Myth/Internal.hsc" #-}
        check_task <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 225 "Myth/Internal.hsc" #-}
        return (Status display_ptr window_ptr widget_ptr width height check_fd check_task)
    poke ptr (Status display_ptr window_ptr widget_ptr width height check_fd check_task) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr display_ptr
{-# LINE 228 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr window_ptr
{-# LINE 229 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr widget_ptr
{-# LINE 230 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr width
{-# LINE 231 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 28) ptr height
{-# LINE 232 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr check_fd
{-# LINE 233 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr check_task
{-# LINE 234 "Myth/Internal.hsc" #-}

foreign import ccall unsafe "display_bind"
    c_display_bind :: Ptr Display -> Word32 -> Ptr WlOutputInterface -> CInt -> IO (Ptr WlOutput)

foreign import ccall unsafe "display_create"
    c_display_create :: CInt -> Ptr CString -> IO (Ptr Display)

foreign import ccall unsafe "&display_destroy"
    c_display_destroy :: FinalizerPtr Display

foreign import ccall safe "display_run"
    c_display_run :: Ptr Display -> IO ()

foreign import ccall safe "display_set_global_handler"
    c_display_set_global_handler :: Ptr Display -> FunPtr (Ptr Display -> Word32 -> CString -> Word32 -> Ptr () -> IO ()) -> IO ()

foreign import ccall unsafe "display_set_user_data"
    c_display_set_user_data :: Ptr Display -> Ptr () -> IO ()

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

foreign import ccall unsafe "window_schedule_resize"
    c_window_schedule_resize :: Ptr Window -> Int32 -> Int32 -> IO ()

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

foreign import ccall unsafe "widget_set_redraw_handler"
    c_widget_set_redraw_handler :: Ptr Widget -> FunPtr (Ptr Widget -> Ptr () -> IO ()) -> IO ()

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
    c_wl_proxy_marshal ds_ptr 5 nullPtr nullPtr
{-# LINE 328 "Myth/Internal.hsc" #-}

c_weston_desktop_shell_set_background :: Ptr WestonDesktopShell -> Ptr WlOutput -> Ptr WlSurface -> IO ()
c_weston_desktop_shell_set_background ds_ptr wlo_ptr s_ptr =
    c_wl_proxy_marshal ds_ptr 0 (castPtr wlo_ptr) (castPtr s_ptr)
{-# LINE 332 "Myth/Internal.hsc" #-}

c_weston_desktop_shell_set_grab_surface :: Ptr WestonDesktopShell -> Ptr WlSurface -> IO ()
c_weston_desktop_shell_set_grab_surface ds_ptr s_ptr =
    c_wl_proxy_marshal ds_ptr 4 (castPtr s_ptr) nullPtr
{-# LINE 336 "Myth/Internal.hsc" #-}

foreign import ccall unsafe "wrapper"
    mkStatusCheckForeign ::            (Ptr Task -> Word32 -> IO ()) ->
                            IO (FunPtr (Ptr Task -> Word32 -> IO ()))

foreign import ccall unsafe "wrapper"
    mkRedrawHandlerForeign ::            (Ptr Widget -> Ptr () -> IO ()) ->
                              IO (FunPtr (Ptr Widget -> Ptr () -> IO ()))

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

foreign import ccall safe "dynamic"
    mkSurfaceConfigure :: FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()) ->
                                 (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ())

