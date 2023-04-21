#!/usr/bin/env lua
local os = require("os")
local io = require("io")


function get_hour_now () return tonumber(os.date("%H")) end
function get_minute_now () return tonumber(os.date("%M")) end
function get_month_day () return tonumber(os.date("%d")) end


local note_config = {
  log_min_delay = 8,
  new_day_after = 5,
}

function get_current_note_path ()
  local month_day = get_month_day()

  if get_hour_now() < note_config.new_day_after then
    month_day = month_day - 1
  end

  return os.getenv("HOME")
  .. "/.config/hax-local/dirs/personal/notes/daily/"
    .. os.date("%Y-%m-")
    .. month_day
    .. ".org"

end


print(get_current_note_path())
