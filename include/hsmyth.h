#include <HsFFI.h>
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

struct encoder {
    int is_connected;
    int is_active;
    char *recording_title;
    HsStablePtr *channel_icon;
};

struct node_button {
    struct widget *widget;
    char *hostname;
};

struct alert {
    struct widget *widget;
    int check_fd;
    struct task check_task;
    int hide_fd;
    struct task hide_task;
    int baby_monitor_health;
    int hdhomerun_health;
    int mythtv_health;
    int pihole_health;
    int hue_health;
    int show_dashboard;
    size_t num_node_buttons;
    struct node_button **node_buttons;
};

struct status {
    struct display *display;
    struct window *window;
    struct widget *widget;
    int width, height;
    int check_fd;
    struct task check_task;
    int show_clock;
    size_t num_encoders;
    struct encoder *encoders;
};

