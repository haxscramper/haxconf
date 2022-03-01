local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local lain = require("lain")
local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup").widget

local function set_wallpaper(s)
  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)
  end
end

--- Update names of all tags in all screens.
function update_all_tag_names()
  for _,tag in pairs(root.tags()) do
    client_names = {}
    for _,client in ipairs(tag:clients()) do
       name = client.instance
       table.insert(client_names, name)
    end

    if #client_names > 0 then
      tag.name = "[" .. tag.index .. ": " .. table.concat(client_names, " ") .. "]"
    else
      tag.name = "[" .. tag.index .. "]"
    end
  end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

local volumearc_widget = require("awesome-wm-widgets.volumearc-widget.volumearc")

awful.screen.connect_for_each_screen(function(s)
    set_wallpaper(s)

    awful.tag(
      { "(1)", "(2)", "(3)", "(4)", "(5)", "(6)" },
      s, awful.layout.layouts[1])

    for k, t in pairs(s.tags) do
      t:connect_signal("tagged", function(t) update_all_tag_names() end)
      t:connect_signal("untagged", function(t) update_all_tag_names() end)
    end

    s.mytaglist = awful.widget.taglist {
      screen  = s,
      filter  = awful.widget.taglist.filter.all,
    }

    s.mywibox = awful.wibar({ position = "top", screen = s })

    s.mywibox:setup {
      layout = wibox.layout.align.horizontal,
      { -- Left widgets
        layout = wibox.layout.fixed.horizontal,
        mylauncher,
        s.mytaglist,
      },
      nil,
      { -- Right widgets
        layout = wibox.layout.fixed.horizontal,
        -- create_battery_widget(),
        mykeyboardlayout,
        wibox.widget.systray(),
        mytextclock,
        volumearc_widget,
      },
    }
end)

update_all_tag_names()
