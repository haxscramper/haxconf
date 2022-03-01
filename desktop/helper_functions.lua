#!/usr/bin/env lua

local math = require("math")

function istable(tbl) return type(tbl) == 'table' end

function has_value(tbl, val)
  for k, v in pairs(tbl) do
    if v == val then
      return true
    end
  end

  return false
end

function subtable_depth(tbl)
  depth_now = 1
  if not istable(tbl) then return 0 end
  for k, v in pairs(tbl) do
    if istable(k) then
      local sub_depth = subtable_depth(k)
      depth_now = math.max(sub_depth, depth_now)
    end

    if istable(v) then
      local sub_depth = subtable_depth(k)
      depth_now = math.max(sub_depth, depth_now)
    end
  end

  return depth_now
end

function total_size(obj)
  if istable(obj) then
    local sum = 0
    for key, val in pairs(obj) do
      if not istable(key) and not istable(val) then
        sum = sum + 1
      else
        sum = sum +total_size(key) + total_size(val) - 1
      end
    end

    return sum
  else
    return 1
  end
end

function is_small_table(tbl)
  if
    (subtable_depth(tbl) > 1) or
    (total_size(tbl) > 8)
  then
    return false
  end

  return true
end

print(total_size({as = "12", as2 = {12, 12}}))

-- Convert table to string (beacuse /dynamic programming language lua/
-- has no built-in support for converting things to strings yeah)
function dump(o, pretty, level, depthlimit)
  if level == nil then level = 0 end
  if depthlimit == nil then depthlimit = 5 end

  if level > depthlimit then
    return "reached nested level " .. depthlimit .. ", cutting off"
  end

  if pretty == nil then pretty = true end

  local indent = string.rep("  ", level)
  if not pretty then indent = "" end

  local key_indent = "  "
  if not pretty then key_indent = "" end

  local newline = '\n'
  if not pretty then newline = ' ' end

  if type(o) == 'table' then
    local s = '{' .. newline
    local values = {}
    for k,v in pairs(o) do
      key = k
      if type(k) == 'table' then
        key = dump(k, pretty, level + 1)
      elseif type(k) == 'function' then
        key = "function " .. tostring(key)
      else
        -- print("key type is " .. type(k))
      end

      table.insert(
        values,
        indent .. key_indent .. key .. " = " .. dump(v, pretty, level + 1, depthlimit))
    end

    s = s .. table.concat(values, "," .. newline)

    return s .. newline .. indent .. '}'
  elseif type(o) == 'string' then
    return '"' .. o .. '"'
  else
    return tostring(o)
  end

  print(indent .. "finished dump " .. level)
end

-- Write message into today's log file
function write_log(log_message)
  local path = os.getenv("HOME") ..
    "/.config/hax-local/log/" .. os.date("%Y-%m-%d")
  local file = io.open(path, "a")

  io.output(file)
  io.write(os.date("[%H:%M:%S] " .. log_message .. "\n"))
  io.close(file)
end

-- TODO enable for main instance and turn off for all nested (using
-- awmtt)
print_logging = true

function log(text)
  if print_logging then
    print(text)
  else
    write_log(text)
  end
end

