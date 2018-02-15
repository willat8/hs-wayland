#include "window.h"
#include "weston-desktop-shell-client-protocol.h"

struct desktop {
    struct display *display;
    struct weston_desktop_shell *shell;
    struct output *output;
    struct window *grab_window;
    struct widget *grab_widget;
    enum cursor_type grab_cursor;
};

struct surface {
    void (*configure)(void *data,
                      struct weston_desktop_shell *desktop_shell,
                      uint32_t edges,
                      struct window *window,
                      int32_t width,
                      int32_t height);
};

struct background {
    struct surface base;
    struct window *window;
    struct widget *widget;
};

struct output {
    struct wl_output *output;
    struct background *background;
};

struct status {
    struct display *display;
    struct window *window;
    struct widget *widget;
    int width, height;
    int check_fd;
    struct task check_task;
    int code;
    int show_clock;
};

