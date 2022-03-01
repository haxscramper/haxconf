local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local help_group = "client"

-- Closing shortcut ignores this applications
local noclose_class = {-- "Emacs",
                       "firefox",
                       "fish",
                       "nr",
                       "kitty"
                       }


-- Close client if it is not in ignored list
function close_client(c)
  if has_value(noclose_class, c.class) then
    warn_notify("client " .. c.class .. " (" .. c.name .. ")\nshould be closed manually")
  else
    log("closing [" .. c.class .. "] " .. c.name)
    c:kill()
  end
end

clientkeys = gears.table.join(
  awful.key(
    { modkey },
    "f",
    function (c)
      c.fullscreen = not c.fullscreen
      c:raise()
    end,
    {description = "toggle fullscreen", group = help_group}),

  awful.key(
    { modkey, "Shift"   },
    "q",
    function (c) close_client(c) end,
    {description = "quit client", group = help_group}),

  awful.key(
    { modkey },
    "o",
    function (c)
      -- awful.placement.under_mouse(c)
      c:move_to_screen()
    end,
    {description = "move to screen", group = help_group}),

  awful.key(
    { modkey }, "l",
    function (c)
      log("Toggle client floating")
      awful.client.floating.toggle(c)
    end,
    {description = "toggle max layout", group = "client"})
)
