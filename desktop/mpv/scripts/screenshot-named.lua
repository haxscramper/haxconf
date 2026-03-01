local utils = require("mp.utils")

mp.add_key_binding("s", "screenshot-named", function()
    local base_dir = os.getenv("HOME") .. "/defaultdirs/images/video_screenshot"
    
    local filename = mp.get_property("filename/no-ext")
    local time = mp.get_property_number("time-pos", 0)
    
    local hours = math.floor(time / 3600)
    local mins = math.floor((time % 3600) / 60)
    local secs = math.floor(time % 60)
    local ms = math.floor((time % 1) * 1000)
    
    local timestamp = string.format("%02d-%02d-%02d-%03d", hours, mins, secs, ms)
    
    -- Sanitize filename: keep only alphanumeric, dash, underscore
    local sanitized = filename:gsub("[^%w%-_]", "_"):gsub("_+", "_"):gsub("^_", ""):gsub("_$", "")
    
    -- Truncate to 24 characters
    local truncated = sanitized:sub(1, 24):gsub("_$", "")
    
    local dir = base_dir .. "/" .. truncated
    os.execute("mkdir -p '" .. dir .. "'")
    
    local path = string.format("%s/%s.jpg", dir, timestamp)
    
    mp.commandv("screenshot-to-file", path, "video")
    mp.osd_message("Saved: " .. path)
end)
