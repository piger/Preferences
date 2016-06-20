-- hammerspoon init.lua

-- set grid size
hs.grid.GRIDWIDTH = 40
hs.grid.GRIDHEIGHT = 40
hs.grid.MARGINX = 0
hs.grid.MARGINY = 0

-- no animation pls
hs.window.animationDuration = 0

local mash = {"cmd", "alt", "ctrl"}
local mashift = {"cmd", "alt", "shift"}

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

-- functions and callbacks

local wifi_work_ssid = "zendesk"
local wifi_last_ssid = hs.wifi.currentNetwork()
local wifi_watcher = hs.wifi.watcher.new(function ()
   local wifi_new_ssid = hs.wifi.currentNetwork()
   wifi_new_ssid = hs.wifi.currentNetwork()

   if wifi_new_ssid == wifi_work_ssid and wifi_last_ssid ~= wifi_work_ssid then
      hs.audiodevice.defaultOutputDevice():setVolume(1)
      hs.notify.new({title="Hammerspoon", informativeText="Lowering volume as we are at Work!"}):send():release()
   end

   wifi_last_ssid = wifi_new_ssid
end)
wifi_watcher:start()

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
local sleepWatcher = hs.caffeinate.watcher.new(function (eventType)
   if (eventType == hs.caffeinate.watcher.systemDidWake) then
      setCaffeineDisplay(hs.caffeinate.set(sleepType, false, true))
   end
end)
sleepWatcher:start()

--- try to detect the external monitor
local monitorWatcher = hs.screen.watcher.new(function ()
   hasExternal = false
   for id, screen in pairs(hs.screen.allScreens()) do
      if screen:name() == "Thunderbolt Display" then
         hasExternal = true
         break
      end
   end

   if hasExternal == true then
      hs.execute("ln -sf $HOME/Preferences/iTerm2/iTerm2_Dynamic_12.json \"$HOME/Library/Application Support/iTerm2/DynamicProfiles/iTerm2_Dynamic.json\"")
   else
      hs.execute("ln -sf $HOME/Preferences/iTerm2/iTerm2_Dynamic_11.json \"$HOME/Library/Application Support/iTerm2/DynamicProfiles/iTerm2_Dynamic.json\"")
   end
end)
monitorWatcher:start()

--- ole'
hs.alert.show("Hammerspoon 💩 loaded!")

