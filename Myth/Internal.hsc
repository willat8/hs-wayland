{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Myth.Internal where
import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#include "window.h"
#include "weston-desktop-shell-client-protocol.h"

#{def struct surface {
    void (*configure)(void *data,
                      struct weston_desktop_shell *desktop_shell,
                      uint32_t edges,
                      struct window *window,
                      int32_t width,
                      int32_t height);
  };
}

#{def struct background {
    struct surface base;
    struct window *window;
    struct widget *widget;
  };
}

#{def struct desktop {
    struct display *display;
    struct weston_desktop_shell *shell;
    struct output *output;
    struct window *grab_window;
    struct widget *grab_widget;
    enum cursor_type grab_cursor;
  };
}

#{def struct output {
    struct wl_output *output;
    struct background *background;
  };
}

newtype CursorType = CursorType { unCursorType :: CInt }
    deriving (Eq, Show)
#enum CursorType, CursorType, CURSOR_LEFT_PTR

data WlOutputInterface
foreign import ccall "&wl_output_interface"
    c_wl_output_interface :: Ptr WlOutputInterface
foreign import ccall "&weston_desktop_shell_interface"
    c_weston_desktop_shell_interface :: Ptr WlOutputInterface

data Display
data WestonDesktopShell
data Window
data Widget
data WlOutput
data WlSurface
data Input

data Surface = Surface (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()))
instance Storable Surface where
    sizeOf _    = #{size struct surface}
    alignment _ = #{alignment struct surface}
    peek ptr = do
        c_funp <- #{peek struct surface, configure} ptr
        return (Surface c_funp)
    poke ptr (Surface c_funp) = do
        #{poke struct surface, configure} ptr c_funp

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

foreign import ccall safe "display_bind"
    c_display_bind :: Ptr Display -> Word32 -> Ptr WlOutputInterface -> CInt -> IO (Ptr WlOutput)

foreign import ccall safe "display_create"
    c_display_create :: CInt -> Ptr CString -> IO (Ptr Display)

foreign import ccall safe "display_destroy"
    c_display_destroy :: Ptr Display -> IO ()

foreign import ccall safe "display_run"
    c_display_run :: Ptr Display -> IO ()

foreign import ccall safe "display_set_global_handler"
    c_display_set_global_handler :: Ptr Display -> FunPtr (Ptr Display -> Word32 -> CString -> Word32 -> Ptr () -> IO ()) -> IO ()

foreign import ccall safe "display_set_user_data"
    c_display_set_user_data :: Ptr Display -> Ptr () -> IO ()

foreign import ccall safe "window_add_widget"
    c_window_add_widget :: Ptr Window -> Ptr () -> IO (Ptr Widget)

foreign import ccall safe "window_create_custom"
    c_window_create_custom :: Ptr Display -> IO (Ptr Window)

foreign import ccall safe "window_destroy"
    c_window_destroy :: Ptr Window -> IO ()

foreign import ccall safe "window_get_user_data"
    c_window_get_user_data :: Ptr Window -> IO (Ptr ())

foreign import ccall safe "window_get_wl_surface"
    c_window_get_wl_surface :: Ptr Window -> IO (Ptr WlSurface)

foreign import ccall safe "window_set_user_data"
    c_window_set_user_data :: Ptr Window -> Ptr () -> IO ()

foreign import ccall safe "widget_destroy"
    c_widget_destroy :: Ptr Widget -> IO ()

foreign import ccall safe "widget_schedule_resize"
    c_widget_schedule_resize :: Ptr Widget -> Int32 -> Int32 -> IO ()

foreign import ccall safe "widget_set_allocation"
    c_widget_set_allocation :: Ptr Widget -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "widget_set_enter_handler"
    c_widget_set_enter_handler :: Ptr Widget -> FunPtr (Ptr Widget -> Ptr Input -> Float -> Float -> Ptr () -> IO (CursorType)) -> IO ()

foreign import ccall safe "widget_set_transparent"
    c_widget_set_transparent :: Ptr Widget -> CInt -> IO ()

foreign import ccall safe "wl_proxy_destroy"
    c_wl_output_destroy :: Ptr WlOutput -> IO ()

foreign import ccall safe "wl_proxy_add_listener"
    c_weston_desktop_shell_add_listener :: Ptr WestonDesktopShell -> Ptr Listener -> Ptr Desktop -> IO ()

foreign import ccall safe "wl_proxy_destroy"
    c_weston_desktop_shell_destroy :: Ptr WestonDesktopShell -> IO ()

foreign import ccall safe "wl_proxy_get_user_data"
    c_wl_surface_get_user_data :: Ptr WlSurface -> IO (Ptr ())

foreign import ccall safe "wl_proxy_marshal"
    c_wl_proxy_marshal :: Ptr WestonDesktopShell -> CInt -> Ptr () -> Ptr () -> IO ()

c_weston_desktop_shell_desktop_ready :: Ptr WestonDesktopShell -> IO ()
c_weston_desktop_shell_desktop_ready ds_ptr =
    c_wl_proxy_marshal ds_ptr #{const WESTON_DESKTOP_SHELL_DESKTOP_READY} nullPtr nullPtr

c_weston_desktop_shell_set_background :: Ptr WestonDesktopShell -> Ptr WlOutput -> Ptr WlSurface -> IO ()
c_weston_desktop_shell_set_background ds_ptr wlo_ptr s_ptr =
    c_wl_proxy_marshal ds_ptr #{const WESTON_DESKTOP_SHELL_SET_BACKGROUND} (castPtr wlo_ptr :: Ptr ()) (castPtr s_ptr :: Ptr ())

c_weston_desktop_shell_set_grab_surface :: Ptr WestonDesktopShell -> Ptr WlSurface -> IO ()
c_weston_desktop_shell_set_grab_surface ds_ptr s_ptr =
    c_wl_proxy_marshal ds_ptr #{const WESTON_DESKTOP_SHELL_SET_GRAB_SURFACE} (castPtr s_ptr :: Ptr ()) nullPtr

foreign import ccall "wrapper"
    mkSurfaceConfigureForeign ::            (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()) ->
                                 IO (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()))

foreign import ccall "wrapper"
    mkDesktopShellConfigureForeign ::            (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr WlSurface -> Int32 -> Int32 -> IO ()) ->
                                      IO (FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr WlSurface -> Int32 -> Int32 -> IO ()))

foreign import ccall "wrapper"
    mkDesktopShellPrepareLockSurfaceForeign ::            (Ptr () -> Ptr WestonDesktopShell -> IO ()) ->
                                               IO (FunPtr (Ptr () -> Ptr WestonDesktopShell -> IO ()))

foreign import ccall "wrapper"
    mkDesktopShellGrabCursorForeign ::            (Ptr () -> Ptr WestonDesktopShell -> CursorType -> IO ()) ->
                                       IO (FunPtr (Ptr () -> Ptr WestonDesktopShell -> CursorType -> IO ()))

foreign import ccall "wrapper"
    mkGrabSurfaceEnterHandlerForeign ::            (Ptr Widget -> Ptr Input -> Float -> Float -> Ptr () -> IO (CursorType)) ->
                                        IO (FunPtr (Ptr Widget -> Ptr Input -> Float -> Float -> Ptr () -> IO (CursorType)))

foreign import ccall "wrapper"
    mkGlobalHandlerForeign ::            (Ptr Display -> Word32 -> CString -> Word32 -> Ptr () -> IO ()) ->
                              IO (FunPtr (Ptr Display -> Word32 -> CString -> Word32 -> Ptr () -> IO ()))

foreign import ccall "dynamic"
    mkSurfaceConfigure :: FunPtr (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ()) ->
                                 (Ptr () -> Ptr WestonDesktopShell -> Word32 -> Ptr Window -> Int32 -> Int32 -> IO ())

