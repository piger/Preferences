-- hammerspoon init.lua

local mash = {"cmd", "alt", "ctrl"}
local mashift = {"cmd", "alt", "shift"}
local wifiWatcher = nil
local workSSID = "Zendesk"
local lastSSID = hs.wifi.currentNetwork()

-- set grid size
hs.grid.GRIDWIDTH = 12
hs.grid.GRIDHEIGHT = 12
hs.grid.MARGINX = 0
hs.grid.MARGINY = 0

-- no animation pls
hs.window.animationDuration = 0


hs.hotkey.bind(mash, "R", hs.reload)

hs.hotkey.bind(mash, "M", function()
                  local win = hs.window.focusedWindow()
                  win:maximize()
                  -- hs.grid.maximizeWindow()
end)

hs.hotkey.bind(mash, "space", hs.spotify.displayCurrentTrack)

hs.hotkey.bind(mash, "h", hs.grid.resizeWindowThinner)
hs.hotkey.bind(mash, "l", hs.grid.resizeWindowWider)
hs.hotkey.bind(mash, "j", hs.grid.resizeWindowTaller)
hs.hotkey.bind(mash, "k", hs.grid.resizeWindowShorter)

hs.hotkey.bind(mash, "up", hs.grid.pushWindowUp)
hs.hotkey.bind(mash, "down", hs.grid.pushWindowDown)
hs.hotkey.bind(mash, "left", hs.grid.pushWindowLeft)
hs.hotkey.bind(mash, "right", hs.grid.pushWindowRight)

hs.hotkey.bind(mash, ";", function() hs.grid.snap(hs.window.focusedWindow()) end)
hs.hotkey.bind(mash, "'", function() hs.fnutils.map(hs.window.visibleWindows(), hs.grid.snap) end)

function ssidChangedCallback()
   newSSID = hs.wifi.currentNetwork()

   if newSSID == workSSID and lastSSID ~= workSSID then
      hs.audiodevice.defaultOutputDevice():setVolume(10)
      hs.notify.new({title="Hammerspoon", informativeText="Lowering volume as we are at Work!"}):send():release()
   end

   lastSSID = newSSID
end

wifiWatcher = hs.wifi.watcher.new(ssidChangedCallback)
wifiWatcher:start()

hs.alert.show("Hammerspoon loaded!")
