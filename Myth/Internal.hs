{-# LINE 1 "Myth/Internal.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LINE 2 "Myth/Internal.hsc" #-}

module Myth.Internal where
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types


{-# LINE 10 "Myth/Internal.hsc" #-}

{-# LINE 11 "Myth/Internal.hsc" #-}


{-# LINE 21 "Myth/Internal.hsc" #-}


{-# LINE 27 "Myth/Internal.hsc" #-}


{-# LINE 37 "Myth/Internal.hsc" #-}


{-# LINE 44 "Myth/Internal.hsc" #-}

newtype CursorType = CursorType { unCursorType :: CInt }
    deriving (Eq, Show)
cursorLeftPtr :: CursorType
cursorLeftPtr = CursorType 4

{-# LINE 48 "Myth/Internal.hsc" #-}

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
{-# LINE 72 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 73 "Myth/Internal.hsc" #-}
    peek ptr = do
        d_ptr      <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 75 "Myth/Internal.hsc" #-}
        s_ptr      <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 76 "Myth/Internal.hsc" #-}
        o_ptr      <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 77 "Myth/Internal.hsc" #-}
        window_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 78 "Myth/Internal.hsc" #-}
        widget_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 79 "Myth/Internal.hsc" #-}
        c          <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 80 "Myth/Internal.hsc" #-}
        return (Desktop d_ptr s_ptr o_ptr window_ptr widget_ptr (CursorType c))
    poke ptr (Desktop d_ptr s_ptr o_ptr window_ptr widget_ptr c) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr d_ptr
{-# LINE 83 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr s_ptr
{-# LINE 84 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr o_ptr
{-# LINE 85 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr window_ptr
{-# LINE 86 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr widget_ptr
{-# LINE 87 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr (unCursorType c)
{-# LINE 88 "Myth/Internal.hsc" #-}

data Output = Output { outputWlOutput   :: Ptr WlOutput
                     , outputBackground :: Ptr Background
                     }
instance Storable Output where
    sizeOf _    = (16)
{-# LINE 94 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 95 "Myth/Internal.hsc" #-}
    peek ptr = do
        o_ptr  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 97 "Myth/Internal.hsc" #-}
        bg_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 98 "Myth/Internal.hsc" #-}
        return (Output o_ptr bg_ptr)
    poke ptr (Output o_ptr bg_ptr) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr o_ptr
{-# LINE 101 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr bg_ptr
{-# LINE 102 "Myth/Internal.hsc" #-}

data Surface = Surface (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()))
instance Storable Surface where
    sizeOf _    = (8)
{-# LINE 106 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 107 "Myth/Internal.hsc" #-}
    peek ptr = do
        c_funp <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 109 "Myth/Internal.hsc" #-}
        return (Surface c_funp)
    poke ptr (Surface c_funp) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr c_funp
{-# LINE 112 "Myth/Internal.hsc" #-}

data Listener = Listener (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr WlSurface -> Int32 -> Int32 -> IO ()))
                         (FunPtr (Ptr () -> Ptr WestonDesktopShell -> IO ()))
                         (FunPtr (Ptr () -> Ptr WestonDesktopShell -> CursorType -> IO ()))
instance Storable Listener where
    sizeOf _    = (24)
{-# LINE 118 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 119 "Myth/Internal.hsc" #-}
    peek ptr = do
        c_funp   <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 121 "Myth/Internal.hsc" #-}
        pls_funp <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 122 "Myth/Internal.hsc" #-}
        gc_funp  <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 123 "Myth/Internal.hsc" #-}
        return (Listener c_funp pls_funp gc_funp)
    poke ptr (Listener c_funp pls_funp gc_funp) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr c_funp
{-# LINE 126 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr pls_funp
{-# LINE 127 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr gc_funp
{-# LINE 128 "Myth/Internal.hsc" #-}

data Background = Background { backgroundSurface :: Surface
                             , backgroundWindow  :: Ptr Window
                             , backgroundWidget  :: Ptr Widget
                             }
instance Storable Background where
    sizeOf _    = (24)
{-# LINE 135 "Myth/Internal.hsc" #-}
    alignment _ = (8)
{-# LINE 136 "Myth/Internal.hsc" #-}
    peek ptr = do
        base       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 138 "Myth/Internal.hsc" #-}
        window_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 139 "Myth/Internal.hsc" #-}
        widget_ptr <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 140 "Myth/Internal.hsc" #-}
        return (Background base window_ptr widget_ptr)
    poke ptr (Background base window_ptr widget_ptr) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr base
{-# LINE 143 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr window_ptr
{-# LINE 144 "Myth/Internal.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr widget_ptr
{-# LINE 145 "Myth/Internal.hsc" #-}

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

foreign import ccall unsafe "window_create_custom"
    c_window_create_custom :: Ptr Display -> IO (Ptr Window)

foreign import ccall unsafe "window_destroy"
    c_window_destroy :: Ptr Window -> IO ()

foreign import ccall unsafe "window_get_user_data"
    c_window_get_user_data :: Ptr Window -> IO (Ptr ())

foreign import ccall unsafe "window_get_wl_surface"
    c_window_get_wl_surface :: Ptr Window -> IO (Ptr WlSurface)

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
{-# LINE 215 "Myth/Internal.hsc" #-}

c_weston_desktop_shell_set_background :: Ptr WestonDesktopShell -> Ptr WlOutput -> Ptr WlSurface -> IO ()
c_weston_desktop_shell_set_background ds_ptr wlo_ptr s_ptr =
    c_wl_proxy_marshal ds_ptr 0 (castPtr wlo_ptr :: Ptr ()) (castPtr s_ptr :: Ptr ())
{-# LINE 219 "Myth/Internal.hsc" #-}

c_weston_desktop_shell_set_grab_surface :: Ptr WestonDesktopShell -> Ptr WlSurface -> IO ()
c_weston_desktop_shell_set_grab_surface ds_ptr s_ptr =
    c_wl_proxy_marshal ds_ptr 4 (castPtr s_ptr :: Ptr ()) nullPtr
{-# LINE 223 "Myth/Internal.hsc" #-}

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

