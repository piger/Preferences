--- cheatsheet
-- inspired by: https://github.com/dharmapoudel/hammerspoon-config/blob/master/cheatsheets.lua

local utils = require("utils")
local view = nil

local function createCheatSheet()
   local html = utils.read_file(os.getenv("HOME") .. "/.hammerspoon/cheatsheet.html")
   local width, height = 1080, 600

   local frame = hs.screen.mainScreen():frame()
   local x = (frame.w / 2) - (width / 2)
   local y = (frame.h / 2) - (height / 2)

   return hs.webview.new({x=x, y=y, w=width, h=height}, {developerExtrasEnabled=true})
      :allowGestures(true)
      :closeOnEscape(true)
      :windowStyle("utility")
      :html(html)
      :bringToFront(true)
      :alpha(0.9)
      :show()
end

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "C", function()
      if not view then
         view = createCheatSheet()
      else
         view:delete()
         view = nil
      end
end)
