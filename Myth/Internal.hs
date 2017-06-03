{-# LINE 1 "Myth/Internal.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LINE 2 "Myth/Internal.hsc" #-}

module Myth.Internal where
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import qualified Graphics.Rendering.Cairo.Types as XP


{-# LINE 11 "Myth/Internal.hsc" #-}

{-# LINE 12 "Myth/Internal.hsc" #-}


{-# LINE 22 "Myth/Internal.hsc" #-}


{-# LINE 32 "Myth/Internal.hsc" #-}


{-# LINE 39 "Myth/Internal.hsc" #-}


{-# LINE 45 "Myth/Internal.hsc" #-}


{-# LINE 53 "Myth/Internal.hsc" #-}

newtype CursorType = CursorType { unCursorType :: CInt }
    deriving (Eq, Show)
cursorLeftPtr :: CursorType
cursorLeftPtr = CursorType 4

{-# LINE 57 "Myth/Internal.hsc" #-}

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
{-# LINE 81 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 82 "Myth/Internal.hsc" #-}
    peek ptr = do
        d_ptr      <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 84 "Myth/Internal.hsc" #-}
        s_ptr      <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 85 "Myth/Internal.hsc" #-}
        o_ptr      <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 86 "Myth/Internal.hsc" #-}
        window_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 87 "Myth/Internal.hsc" #-}
        widget_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 88 "Myth/Internal.hsc" #-}
        c          <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 89 "Myth/Internal.hsc" #-}
        return (Desktop d_ptr s_ptr o_ptr window_ptr widget_ptr (CursorType c))
    poke ptr (Desktop d_ptr s_ptr o_ptr window_ptr widget_ptr c) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr d_ptr
{-# LINE 92 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr s_ptr
{-# LINE 93 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr o_ptr
{-# LINE 94 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr window_ptr
{-# LINE 95 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr widget_ptr
{-# LINE 96 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr (unCursorType c)
{-# LINE 97 "Myth/Internal.hsc" #-}

data Surface = Surface (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()))
instance Storable Surface where
    sizeOf _    = (8)
{-# LINE 101 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 102 "Myth/Internal.hsc" #-}
    peek ptr = do
        c_funp <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 104 "Myth/Internal.hsc" #-}
        return (Surface c_funp)
    poke ptr (Surface c_funp) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr c_funp
{-# LINE 107 "Myth/Internal.hsc" #-}

data Listener = Listener (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr WlSurface -> Int32 -> Int32 -> IO ()))
                         (FunPtr (Ptr () -> Ptr WestonDesktopShell -> IO ()))
                         (FunPtr (Ptr () -> Ptr WestonDesktopShell -> CursorType -> IO ()))
instance Storable Listener where
    sizeOf _    = (24)
{-# LINE 113 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 114 "Myth/Internal.hsc" #-}
    peek ptr = do
        c_funp   <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 116 "Myth/Internal.hsc" #-}
        pls_funp <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 117 "Myth/Internal.hsc" #-}
        gc_funp  <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 118 "Myth/Internal.hsc" #-}
        return (Listener c_funp pls_funp gc_funp)
    poke ptr (Listener c_funp pls_funp gc_funp) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr c_funp
{-# LINE 121 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr pls_funp
{-# LINE 122 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr gc_funp
{-# LINE 123 "Myth/Internal.hsc" #-}

data Background = Background { backgroundSurface :: Surface
                             , backgroundWindow  :: Ptr Window
                             , backgroundWidget  :: Ptr Widget
                             }
instance Storable Background where
    sizeOf _    = (24)
{-# LINE 130 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 131 "Myth/Internal.hsc" #-}
    peek ptr = do
        base       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 133 "Myth/Internal.hsc" #-}
        window_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 134 "Myth/Internal.hsc" #-}
        widget_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 135 "Myth/Internal.hsc" #-}
        return (Background base window_ptr widget_ptr)
    poke ptr (Background base window_ptr widget_ptr) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr base
{-# LINE 138 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr window_ptr
{-# LINE 139 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr widget_ptr
{-# LINE 140 "Myth/Internal.hsc" #-}

data Output = Output { outputWlOutput   :: Ptr WlOutput
                     , outputBackground :: Ptr Background
                     }
instance Storable Output where
    sizeOf _    = (16)
{-# LINE 146 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 147 "Myth/Internal.hsc" #-}
    peek ptr = do
        o_ptr  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 149 "Myth/Internal.hsc" #-}
        bg_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 150 "Myth/Internal.hsc" #-}
        return (Output o_ptr bg_ptr)
    poke ptr (Output o_ptr bg_ptr) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr o_ptr
{-# LINE 153 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr bg_ptr
{-# LINE 154 "Myth/Internal.hsc" #-}

data Status = Status { statusDisplay :: Ptr Display
                     , statusWindow  :: Ptr Window
                     , statusWidget  :: Ptr Widget
                     , statusWidth   :: Int32
                     , statusHeight  :: Int32
                     }
instance Storable Status where
    sizeOf _    = (32)
{-# LINE 163 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 164 "Myth/Internal.hsc" #-}
    peek ptr = do
        display_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 166 "Myth/Internal.hsc" #-}
        window_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 167 "Myth/Internal.hsc" #-}
        widget_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 168 "Myth/Internal.hsc" #-}
        width <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 169 "Myth/Internal.hsc" #-}
        height <- (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 170 "Myth/Internal.hsc" #-}
        return (Status display_ptr window_ptr widget_ptr width height)
    poke ptr (Status display_ptr window_ptr widget_ptr width height) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr display_ptr
{-# LINE 173 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr window_ptr
{-# LINE 174 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr widget_ptr
{-# LINE 175 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr width
{-# LINE 176 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 28) ptr height
{-# LINE 177 "Myth/Internal.hsc" #-}

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
{-# LINE 259 "Myth/Internal.hsc" #-}

c_weston_desktop_shell_set_background :: Ptr WestonDesktopShell -> Ptr WlOutput -> Ptr WlSurface -> IO ()
c_weston_desktop_shell_set_background ds_ptr wlo_ptr s_ptr =
    c_wl_proxy_marshal ds_ptr 0 (castPtr wlo_ptr :: Ptr ()) (castPtr s_ptr :: Ptr ())
{-# LINE 263 "Myth/Internal.hsc" #-}

c_weston_desktop_shell_set_grab_surface :: Ptr WestonDesktopShell -> Ptr WlSurface -> IO ()
c_weston_desktop_shell_set_grab_surface ds_ptr s_ptr =
    c_wl_proxy_marshal ds_ptr 4 (castPtr s_ptr :: Ptr ()) nullPtr
{-# LINE 267 "Myth/Internal.hsc" #-}

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

