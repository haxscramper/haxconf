local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local os = require("os")
local io = require("io")

local drop = dofile(haxconf("awesome_scratchdrop/init.lua"))

local note_config = {
  log_min_delay = 8,
  new_day_after = 5,
}

function term_drop (prog, arg)
  drop.toggle(
    "alacritty -e " .. prog,
    arg.vert or "top",
    arg.horiz or "center",
    arg.width or 0.75,
    arg.height or 0.4,
    arg.sticky or false,
    arg.screen
  )
end

string.rpad = function(str, len, char)
  if char == nil then char = ' ' end
  return string.rep(char, len - #str) .. str
end

function get_hour_now () return tonumber(os.date("%H")) end
function get_minute_now () return tonumber(os.date("%M")) end
function get_month_day () return tonumber(os.date("%d")) end

function get_hour_now_padded ()
  return string.rpad(os.date("%H"), 2, '0')
end


function get_minute_now_padded ()
  return string.rpad(os.date("%M"), 2, '0')
end

function get_time_stamp_now()
  return "@time:" ..
    get_hour_now_padded() .. ":" ..
    get_minute_now_padded() .. ";"
end

function get_current_note_path ()
  local month_day = get_month_day()

  if get_hour_now() < note_config.new_day_after then
    month_day = month_day - 1
  end

  return os.getenv("HOME")
  .. "/.config/hax-local/dirs/personal/notes/daily/"
    .. os.date("%Y-%m-")
    .. string.rpad(tostring(month_day), 2, '0')
    .. ".org"

end

function file_is_empty (file_path)
  return count_lines(file_path) == 0
end

function file_exists(file_path)
  local file = io.open(file_path, "r")
  if file == nil then
    return false
  else
    io.close(file)
    return true
  end
end

local hotkeys_popup = require("awful.hotkeys_popup").widget

globalkeys = gears.table.join(
    awful.key(
      { modkey,           },
      "h",
      hotkeys_popup.show_help,
     {description="show help", group="awesome"}),

    awful.key(
      { modkey,           },
      "n",
      function ()
        log("asdfasdf")
        print("Pressed mod N")
      end,
     {description="show help", group="awesome"}),

    awful.key(
      { modkey,           },
      "j",
      function ()
        awful.client.focus.byidx( 1)
      end,
      {description = "focus next by index", group = "client"}),

    awful.key(
      { modkey,           },
      "k",
      function ()
        awful.client.focus.byidx(-1)
      end,
      {description = "focus previous by index", group = "client"}),

    -- Layout manipulation
    awful.key(
      { modkey, "Shift"   },
      "j",
      function ()
        awful.client.swap.byidx(1)
      end,
      {description = "swap with next client by index", group = "client"}),

    awful.key(
      { modkey, "Shift" },
      "k",
      function ()
        awful.client.swap.byidx(-1)
      end,
      {description = "swap with previous client by index", group = "client"}),

    awful.key(
      { modkey, "Control" },
      "j",
      function ()
        awful.screen.focus_relative(1)
      end,
      {description = "focus the next screen", group = "screen"}),

    awful.key(
      { modkey, "Control" },
      "k",
      function ()
        awful.screen.focus_relative(-1)
      end,
      {description = "focus the previous screen", group = "screen"}),

    awful.key(
      { modkey,           },
      "Return",
      function ()
        awful.spawn(terminal)
      end,
      {description = "open a terminal", group = "launcher"}),

    awful.key(
      { modkey, "Control" },
      "r",
      awesome.restart,
      {description = "reload awesome", group = "awesome"}),

    awful.key(
      { modkey, "Control" },
      "m",
      function()
        term_drop("ncmpcpp", { height = 0.6 })
      end,
      { description = "open/close ncmpcpp", group = "custom"}),

    awful.key(
      { modkey },
      "i",
      function()
        -- TODO pass current window name to emacs
        local c = mouse.object_under_pointer()
        local name = ""
        if c ~= nil then
          name = c.name
          debug_notify(name)
        end

        local cmd = "emacsclient --create-frame " ..
            "--eval '(setq hax/fullscreen-capture t)' " ..
            "--eval '(setq hax/fullscreen-client-name \"" .. string.format("%q", name) .. "\")'" ..
            "--eval '(org-capture nil \"d\")' " ..
            "--eval '(delete-other-windows)' "

        -- debug_notify(cmd)
        drop.toggle(cmd, "top", "center", 0.75, 0.4, false)

      end,
      { description = "open emacs dropdown", group = "custom"}),

    awful.key(
      { modkey, "Control" },
      "e",
      function()
        term_drop("fish", { height = 0.6 })
      end,
      { description = "open/close emacsclient", group = "custom"})
)

function add_shortcut (shortcut)
  globalkeys = gears.table.join(globalkeys, shortcut)
end

