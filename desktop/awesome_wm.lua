-- Standard awesome library
local gears = require("gears")
local fs = gears.filesystem
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")

globalkeys = gears.table.join(
    awful.key(
      { modkey, "Control" },
      "r",
      awesome.restart,
      {description = "reload awesome", group = "awesome"})
)


require("awesome-wm-widgets.volume-widget.volume")

local beautiful = require("beautiful") -- Theme handling library
local naughty = require("naughty") -- Notification library
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget

require("awful.hotkeys_popup.keys")


naughty.config.defaults['icon_size'] = 64

function error_notify(text)
  naughty.notify({
      title = "Error:",
      preset = naughty.config.presets.critical,
      text = text,
      timeout = 40,
  })
end

local warn_preset = naughty.config.presets.normal
warn_preset.bg = "#aa1111"

function warn_notify(text)
  naughty.notify({
      title = "Warning",
      preset = warn_preset,
      text = text,
      timeout = 8,
  })
end

function debug_notify(text)
  naughty.notify({
      -- title = "Debug notifty",
      preset = naughty.config.presets.low,
      text = text,
      timeout = 10,
  })
end

if awesome.startup_errors then
  error_notify(awesome.startup_errors)
end


function haxconf(file)
  return os.getenv("HOME") .. "/.config/haxconf/desktop/" .. file
end

function load_hax_config(file)
  dofile(haxconf(file))
end

load_hax_config("helper_functions.lua")

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal("debug::error", function (err)
      -- Make sure we don't go into an endless error loop
      if in_error then return end
      in_error = true

      error_notify(tostring(err))
      write_log("[error]" .. "awesome wm: " .. tostring(err))

      in_error = false
  end)
end
-- }}}


-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
local theme_location = haxconf("awesome_wm_theme/theme.lua") -- string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), "hax")
write_log("Loading theme from " .. theme_location)
beautiful.init(theme_location)

debug_notify("test")

-- This is used later as the default terminal and editor to run.
terminal = "kitty"
editor = "nvim"
editor_cmd = terminal .. " " .. editor

modkey = "Mod4"

awful.layout.layouts = { awful.layout.suit.tile, awful.layout.suit.floating }

local function client_menu_toggle_fn()
  local instance = nil

  return function ()
    if instance and instance.wibox.visible then
      instance:hide()
      instance = nil
    else
      instance = awful.menu.clients({ theme = { width = 250 } })
    end
  end
end
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
  { "hotkeys", function() return false, hotkeys_popup.show_help end},
  { "manual", terminal .. " -e man awesome" },
  { "edit config", editor_cmd .. " " .. awesome.conffile },
  { "restart", awesome.restart },
  { "quit", function() awesome.quit() end}
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
      { "open terminal", terminal }
    }
})

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
    menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock(" %Y-%m-%d %H:%M (%b %a) ")

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
  awful.button({ }, 1, function(t) t:view_only() end),
  awful.button({ modkey }, 1, function(t)
      if client.focus then
        client.focus:move_to_tag(t)
      end
  end),
  awful.button({ }, 3, awful.tag.viewtoggle),
  awful.button({ modkey }, 3, function(t)
      if client.focus then
        client.focus:toggle_tag(t)
      end
  end),
  awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
  awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
  awful.button({ }, 1, function (c)
      if c == client.focus then
        c.minimized = true
      else
        -- Without this, the following
        -- :isvisible() makes no sense
        c.minimized = false
        if not c:isvisible() and c.first_tag then
          c.first_tag:view_only()
        end
        -- This will also un-minimize
        -- the client, if needed
        client.focus = c
        c:raise()
      end
  end),
  awful.button({ }, 3, client_menu_toggle_fn()),
  awful.button({ }, 4, function ()
      awful.client.focus.byidx(1)
  end),
  awful.button({ }, 5, function ()
      awful.client.focus.byidx(-1)
end))


root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))

load_hax_config("awesome_wm_screens.lua")
load_hax_config("awesome_wm_globalkeys.lua")
load_hax_config("awesome_wm_clientkeys.lua")
load_hax_config("awesome_wm_client.lua")

local tag_count = 6
for i = 1, tag_count do
  globalkeys = gears.table.join(globalkeys,
    -- View tag only.
    awful.key({ modkey }, "#" .. i + 9,
      function ()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        if tag then
          tag:view_only()
        end
      end,
      {description = "view tag #"..i, group = "tag"}),

    awful.key({ modkey, "Shift" }, "#" .. i + 9,
      function ()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:move_to_tag(tag)
          end
        end
      end,
      {description = "move focused client to tag #"..i, group = "tag"})
  )
end

clientbuttons = gears.table.join(
  awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
  awful.button({ modkey }, 1, awful.mouse.client.move),
  awful.button({ modkey }, 3, awful.mouse.client.resize))

root.keys(globalkeys)
awful.rules.rules = {
  {
    rule = { },
    properties = { floating = true, titlebars_enabled = true }
  }
}

for _, rule in ipairs(awful.rules.rules) do
  rule.properties = gears.table.join(rule.properties, {
    focus = awful.client.focus.filter,
    raise = true,
    keys = clientkeys,
    buttons = clientbuttons,
    -- placement = awful.placement.under_mouse,
  })
end

log("Done loading")

