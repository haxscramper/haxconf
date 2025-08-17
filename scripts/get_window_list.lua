local awful = require('awful')

local clients = {}
for _, c in ipairs(client.get()) do
    local client_tags = {}
    for _, tag in ipairs(c:tags()) do
        table.insert(client_tags, tag.name)
    end
    
    local visible_on_screen = false
    for _, tag in ipairs(c:tags()) do
        if tag.selected then
            visible_on_screen = true
            break
        end
    end
    
    table.insert(clients, string.format('"%s","%d","%d","%d","%d","%d","%s","%s","%s"',
        c.window, 
        c.screen.index, 
        c.x, 
        c.y, 
        c.width, 
        c.height, 
        c.class or "",
        table.concat(client_tags, ";"),
        visible_on_screen and "true" or "false"))
end

return table.concat(clients, '\n')
