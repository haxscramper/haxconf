local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local hotkeys_popup = require("awful.hotkeys_popup").widget
local help_group = "client"

local start = os.time()

function client_under_cursor ()
  local cord = mouse.coords()
  for c in awful.client.iterate(function (c) return true end) do
    local geo = c:geometry()
    if geo.x <= cord.x and cord.x <= geo.x + geo.width and
      geo.y <= cord.y and cord.y <= geo.y + geo.height then
      return c
    end
  end
end

client.connect_signal("manage", function (c)
    get_num_widget(c)
    update_client_numeration()
    local under = client_under_cursor()
    if under ~= nil then
      client.focus = under
      debug_notify(string.format("Focsing on %q", under.class))
    end

    local desc =
      "class:" .. (c.class or "<none>") ..
      " type:" .. (c.type or "<none>") ..
      " inst:" .. (c.instance or "<none>")

    if (os.time() - start) < 5 then
      -- debug_notify(string.format("Initial layout debounce - %.2f on %q", os.time() - start, desc))
      return
    end

    local geo = c:geometry()
    local coords = mouse.coords()

    debug_notify("manage:: " .. desc)

    if has_value({"sun-awt-X11-XDialogPeer", "de-jave-jave-Jave"}, c.class) then
      c.floating = true
      return
    end

    geo.x = coords.x
    geo.y = coords.y
    c.floating = true

    if has_value({"TelegramDesktop", "InputOutput"}, c.class) then
      awful.titlebar.hide(c)
      return

    elseif has_value({"Emacs", "firefox", "kitty"}, c.class) and
           (c.type ~= nil and c.type == "normal") then
      c.floating = false
      c:geometry(geo)
      loc = c:geometry()
      mouse.coords({x = loc.x, y = loc.y})
      client.focus = c

      return
    end

    c.maximized = false
    c.maximized_horizontal = false
    c.maximized_vertical = false

    local screen = mouse.screen.geometry

    if (screen.width * 0.6 < geo.width) or
       (screen.height * 0.8 < geo.height) then

      geo.width = screen.width * 0.6
      geo.height = screen.height * 0.8
    end

    geo.x = geo.x - (geo.width / 4)
    geo.y = geo.y - (geo.height / 4)

    other = {
      x = (geo.x - screen.x) + geo.width,
      y = (geo.y - screen.y) + geo.height
    }

    if geo.x < 0 then
      -- Falling out of the left border
      geo.x = screen.x
      -- debug_notify("Left border")

    elseif screen.width < other.x then
      -- Falling out of the right border
      geo.x = screen.x + screen.width - geo.width
      -- debug_notify("Right border")

    end

    if geo.y < 0 then
      -- Falling out of the top border
      geo.y = screen.y
      -- debug_notify("Top border")

    elseif screen.height < other.y then
      -- Falling out of the bottom border
      geo.y = screen.y + screen.height - geo.height
      -- debug_notify("Bottom border")

    end

    c:geometry(geo)
end)


client.connect_signal("unmanage",
  function (c)
    update_client_numeration()
  end
)

-- function delete_client_numeration(c)
--   -- TODO
-- end

---------------------------  enumerate clients  ---------------------------
-- - IDEA :: Provide order of screen iteration (instead of following order
--   of tags enforced by `tag:clients()`). Prioritise tags positioned on
--   bottom screens (iterate screen grid from right corner in rows and go
--   upwards)

-- Keep track of all client numbering widgets
number_widgets = {} -- Numbering widgets -> clients
client_numbers = {} -- Clients -> client numbers

--- Get numbering widget for client. If no widget were registered
--- create new one and add it to the table.
function get_num_widget(cl)
  if number_widgets[cl] == nil then
    wgt = wibox.widget.textbox()
    wgt:set_text("[test]")

    number_widgets[cl] = wgt
  end

  return number_widgets[cl]
end

--- Update all client numbers
function update_client_numeration()
  num = 1
  -- TODO ignore tags that are not currently visible
  for _, tag in pairs(root.tags()) do
    for _, cl in ipairs(tag:clients()) do
      -- print("setting number " .. num .. " to cl " .. tostring(cl))
      local color = "green"
      if cl == client.focus then color = "red" end

      local text = "<span foreground=\"" ..
        color .. "\">" .. "[- " .. num .. " -]</span>"

      if number_widgets[cl] then
        number_widgets[cl]:set_markup_silently(text)
        client_numbers[cl] = num
        num = num + 1
      end
    end
  end
  -- print("---")
end

function switch_to_client(client_class)
  for _, tag in pairs(root.tags()) do
    for _, cl in ipairs(tag:clients()) do

    end
  end
end

-------------------------  switch to nth client  --------------------------

function move_cursor (x, y)
  local command = "xdotool mousemove " ..
    tostring(x) ..
    " " ..
    tostring(y)

  -- print("running command: ", command)

  awful.spawn(command)
end

function mouse_on_client(c)
  -- log("moving mouse to client")
  if c == nil then print("client is nil") end
  local geom = c:geometry()
  -- aldump("geometry: ", geom)

  move_cursor(
    geom.x + geom.width / 3 + 10,
    geom.y + geom.height / 3 + 10
  )
end

function focus_nth_client(n)
  for cl, num in pairs(client_numbers) do
    if num == n then
      local status, err = pcall(mouse_on_client, cl)

      if status then
        client.focus = cl
        cl:raise()
        -- log("focused client")
      else
        print(err)
      end
    end
  end
end


client.connect_signal("mouse::enter", function(c) client.focus = c end)
-- client.connect_signal("focus", function(cl) update_client_numeration() end)
-- client.connect_signal("unfocus", function(cl) update_client_numeration() end)


--- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
      awful.button({ }, 1, function()
          client.focus = c
          c:raise()
          awful.mouse.client.move(c)
      end),
      awful.button({ }, 3, function()
          client.focus = c
          c:raise()
          awful.mouse.client.resize(c)
      end)
    )

    awful.titlebar(c) : setup {
      { -- Left
        awful.titlebar.widget.iconwidget(c),
        get_num_widget(c), -- get numbering widget for client
        buttons = buttons,
        layout  = wibox.layout.fixed.horizontal
      },
      { -- Middle
        { -- Title
          align  = "center",
          widget = awful.titlebar.widget.titlewidget(c)
        },
        buttons = buttons,
        layout  = wibox.layout.flex.horizontal
      },
      { -- Right
        awful.titlebar.widget.ontopbutton    (c),
        awful.titlebar.widget.closebutton    (c),
        layout = wibox.layout.fixed.horizontal()
      },
      layout = wibox.layout.align.horizontal
    }

    update_client_numeration()
end)


for i = 1, 12 do
  globalkeys = gears.table.join(
    globalkeys,
    awful.key({ modkey }, "F" .. i,
      function () focus_nth_client(i) end,
      {description = "focus client #" .. i, group = "client"})
  )
end
