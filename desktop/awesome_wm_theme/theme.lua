-- Multicolor Awesome WM theme 2.0 github.com/lcpz
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local dpi   = require("beautiful.xresources").apply_dpi
local beautify = require("beautiful")

local os = os
local my_table = awful.util.table or gears.table
local theme                                     = {}
theme.confdir = os.getenv("HOME") .. "/.config/haxconf/desktop/awesome_wm_theme"
theme.font                                      = "Iosevka 12"
theme.menu_bg_normal                            = "#000000"
theme.menu_bg_focus                             = "#000000"
theme.bg_normal                                 = "#000000"
theme.bg_focus                                  = "#000000"
theme.bg_urgent                                 = "#000000"
-- Urgent and focused modes have the same background color to avoid
-- spurious color flashes all over the place. I will see the change when I
-- open the software, there is no need to bombard me with visual
-- distractions all the time.
theme.fg_normal                                 = "#aaaaaa"
theme.fg_focus                                  = "#af1d18"
theme.fg_urgent                                 = "#aaaaaa"
theme.fg_minimize                               = "#ffffff"
theme.border_width                              = 2
theme.border_normal                             = "#1c2022"
theme.border_focus                              = "#606060"
theme.border_marked                             = "#3ca4d8"
theme.menu_border_width                         = 0
theme.menu_width                                = 130
theme.menu_submenu_icon                         = theme.confdir .. "/icons/submenu.png"
theme.menu_fg_normal                            = "#aaaaaa"
theme.menu_fg_focus                             = "#ff8c00"
theme.menu_bg_normal                            = "#050505dd"
theme.menu_bg_focus                             = "#050505dd"
local icon = theme.confdir .. "/icons"

theme.widget_temp                               = icon .. "/temp.png"
theme.widget_uptime                             = icon .. "/ac.png"
theme.widget_cpu                                = icon .. "/cpu.png"
theme.widget_weather                            = icon .. "/dish.png"
theme.widget_fs                                 = icon .. "/fs.png"
theme.widget_mem                                = icon .. "/mem.png"
theme.widget_note                               = icon .. "/note.png"
theme.widget_note_on                            = icon .. "/note_on.png"
theme.widget_netdown                            = icon .. "/net_down.png"
theme.widget_netup                              = icon .. "/net_up.png"
theme.widget_mail                               = icon .. "/mail.png"
theme.widget_batt                               = icon .. "/bat.png"
theme.widget_clock                              = icon .. "/clock.png"
theme.widget_vol                                = icon .. "/spkr.png"
theme.taglist_squares_sel                       = icon .. "/square_a.png"
theme.taglist_squares_unsel                     = icon .. "/square_b.png"
theme.tasklist_plain_task_name                  = true
theme.tasklist_disable_icon                     = true
theme.useless_gap                               = 2
theme.gap_single_client = true
theme.layout_tile                               = icon .. "/tile.png"
theme.layout_tilegaps                           = icon .. "/tilegaps.png"
theme.layout_tileleft                           = icon .. "/tileleft.png"
theme.layout_tilebottom                         = icon .. "/tilebottom.png"
theme.layout_tiletop                            = icon .. "/tiletop.png"
theme.layout_fairv                              = icon .. "/fairv.png"
theme.layout_fairh                              = icon .. "/fairh.png"
theme.layout_spiral                             = icon .. "/spiral.png"
theme.layout_dwindle                            = icon .. "/dwindle.png"
theme.layout_max                                = icon .. "/max.png"
theme.layout_fullscreen                         = icon .. "/fullscreen.png"
theme.layout_magnifier                          = icon .. "/magnifier.png"
theme.layout_floating                           = icon .. "/floating.png"
local title = icon .. "/titlebar"
theme.titlebar_close_button_normal              = title .. "/close_normal.png"
theme.titlebar_close_button_focus               = title .. "/close_focus.png"
theme.titlebar_minimize_button_normal           = title .. "/minimize_normal.png"
theme.titlebar_minimize_button_focus            = title .. "/minimize_focus.png"
theme.titlebar_ontop_button_normal_inactive     = title .. "/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive      = title .. "/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = title .. "/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active        = title .. "/ontop_focus_active.png"
theme.titlebar_sticky_button_normal_inactive    = title .. "/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive     = title .. "/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = title .. "/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active       = title .. "/sticky_focus_active.png"
theme.titlebar_floating_button_normal_inactive  = title .. "/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive   = title .. "/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = title .. "/floating_normal_active.png"
theme.titlebar_floating_button_focus_active     = title .. "/floating_focus_active.png"
theme.titlebar_maximized_button_normal_inactive = title .. "/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = title .. "/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = title .. "/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active    = title .. "/maximized_focus_active.png"

return theme
