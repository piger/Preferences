-- hammerspoon init.lua

local mash = {"cmd", "alt", "ctrl"}
local mashift = {"cmd", "alt", "shift"}
local workSSID = "zendesk"
local lastSSID = hs.wifi.currentNetwork()

-- set grid size
hs.grid.GRIDWIDTH = 40
hs.grid.GRIDHEIGHT = 40
hs.grid.MARGINX = 0
hs.grid.MARGINY = 0

-- no animation pls
hs.window.animationDuration = 0


hs.hotkey.bind(mash, "R", hs.reload)

hs.hotkey.bind(mash, "M", function() hs.window.focusedWindow():maximize() end)

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
      hs.audiodevice.defaultOutputDevice():setVolume(1)
      hs.notify.new({title="Hammerspoon", informativeText="Lowering volume as we are at Work!"}):send():release()
   end

   lastSSID = newSSID
end

local wifiWatcher = hs.wifi.watcher.new(ssidChangedCallback)
wifiWatcher:start()

-- caffeine mode
local caffeine = hs.menubar.new()
local sleepType = "displayIdle"

function setCaffeineDisplay(state)
   if state then
      caffeine:setTitle("🐴")
   else
      caffeine:setTitle("🎋")
   end
end

function caffeineClicked()
   setCaffeineDisplay(hs.caffeinate.toggle(sleepType))
   if hs.caffeinate.get(sleepType) then
      hs.alert.show("Niiihhhh!")
   else
      hs.alert.show("Zzz...")
   end
end

if caffeine then
   caffeine:setClickCallback(caffeineClicked)
   setCaffeineDisplay(hs.caffeinate.get(sleepType))
end

-- disable caffeine mode after sleep
function sleepWatcherAction(eventType)
   if (eventType == hs.caffeinate.watcher.systemDidWake) then
      setCaffeineDisplay(hs.caffeinate.set(sleepType, false, true))
   end
end

local sleepWatcher = hs.caffeinate.watcher.new(sleepWatcherAction)
sleepWatcher:start()

hs.alert.show("Hammerspoon loaded!")

