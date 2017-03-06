-- hammerspoon init.lua
-- NOTE: When you reload your configuration you keep all the old "watchers" running!
local utils = require("utils")


local HOME = os.getenv("HOME")
local sleepScript = HOME .. "/.sleep"
local wakeupScript = HOME .. "/.wakeup"

-- Audio Configuration
local AUDIO_EXTERNAL_NAME = "FiiO USB DAC-E10"
local AUDIO_INTERNAL_NAME = "Built-in Output"

-- Screens Configuration
local BUILTIN_MONITOR = "Color LCD"
local EXTERNAL_MONITOR = "Thunderbolt Display"


local function hasExternalMonitor()
   for _, screen in pairs(hs.screen.allScreens()) do
      if screen:name() == EXTERNAL_MONITOR then
         return true
      end
   end
   return false
end

-- Grid configuration
hs.grid.setGrid("3x3", BUILTIN_MONITOR)

-- no animation pls
hs.window.animationDuration = 0

local mash = {"cmd", "alt", "ctrl"}
-- local mod1 = {"alt", "shift"}
-- local mod2 = {"ctrl", "option", "shift"}
-- local mod3 = {"cmd", "ctrl"}
local mod4 = {"cmd", "shift"} -- usa lo shift di destra, e' piu' comodo!

hs.hotkey.bind(mash, "M", function() hs.window.focusedWindow():maximize() end)
hs.hotkey.bind(mash, "space", hs.spotify.displayCurrentTrack)
hs.hotkey.bind(mash, "delete", function() hs.caffeinate.startScreensaver() end)
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

-- Window layouts
local layouts = {
   work = {
      { "Spotify", nil, BUILTIN_MONITOR, hs.layout.maximized, nil, nil },
      { "Emacs", nil, EXTERNAL_MONITOR, hs.layout.maximized, nil, nil },
      { "Google Chrome", nil, EXTERNAL_MONITOR, hs.layout.maximized, nil, nil },
      { "iTerm2", nil, EXTERNAL_MONITOR, hs.layout.maximized, nil, nil },
   },
   home = {
      { "Spotify", nil, BUILTIN_MONITOR, hs.layout.maximized, nil, nil },
      { "Emacs", nil, BUILTIN_MONITOR, hs.layout.maximized, nil, nil },
      { "Google Chrome", nil, BUILTIN_MONITOR, hs.layout.maximized, nil, nil },
      { "iTerm2", nil, BUILTIN_MONITOR, hs.layout.maximized, nil, nil },
   }
}

hs.hotkey.bind(mod4, "1", function() if hasExternalMonitor() then hs.layout.apply(layouts["work"]) end end)
hs.hotkey.bind(mod4, "2", function() hs.layout.apply(layouts["home"]) end)

-- WiFi Watcher --
------------------
local wifi_work_ssid = "zendesk"
local wifi_last_ssid = hs.wifi.currentNetwork()

local wifi_watcher = hs.wifi.watcher.new(function ()
   local wifi_new_ssid = hs.wifi.currentNetwork()

   if wifi_new_ssid == wifi_work_ssid and wifi_last_ssid ~= wifi_work_ssid then
      hs.audiodevice.defaultOutputDevice():setVolume(1)
      hs.notify.new({title="Hammerspoon", informativeText="Lowering volume as we are at Work!"}):send()
   end

   wifi_last_ssid = wifi_new_ssid
end)
wifi_watcher:start()

--- caffeine mode
require("caffeine")

--- Sleep watcher ---
--------------------
-- Can execute ~/.sleep and ~/.wakeup after sleep or wakeup events; note that those files
-- must be shell scripts with the executable bit set
local sleepWatcher = hs.caffeinate.watcher.new(function (eventType)
      if (eventType == hs.caffeinate.watcher.systemDidWake) then
         if utils.ken_lee(wakeupScript) then
            hs.notify.new({title="Hammerspoon", informativeText="Running wakeup script"}):send()
            hs.execute(wakeupScript)
         end
      elseif (eventType == hs.caffeinate.watcher.systemWillSleep) then
         if utils.ken_lee(sleepScript) then
            hs.notify.new({title="Hammerspoon", informativeText="Running sleep script"}):send()
            hs.execute(sleepScript)
         end
      end
end)
sleepWatcher:start()


-- Monitor watcher, used to toggle the dynamic profile in iTerm2 --
-------------------------------------------------------------------

-- Switch dynamic profile in iTerm2
-- Valid values: iTerm2_Dynamic_12.json, iTerm2_Dynamic_11.json
local function setIterm2Profile(filename)
   hs.execute("ln -sf $HOME/Preferences/iTerm2/" .. filename .. " \"$HOME/Library/Application Support/iTerm2/DynamicProfiles/iTerm2_Dynamic.json\"")
end

-- Switch font in emacs
local function tellEmacsToChangeFont()
   hs.execute("/usr/local/bin/emacsclient -e '(set-the-right-font)'")
end

local function displayWatcherCallback()
   tellEmacsToChangeFont()

   if hasExternalMonitor() then
      setIterm2Profile("iTerm2_Dynamic_12.json")
   else
      setIterm2Profile("iTerm2_Dynamic_11.json")
   end
end

local monitorWatcher = hs.screen.watcher.new(displayWatcherCallback)
monitorWatcher:start()


-- Audio Device toggle --
-------------------------
local audioToggleMenu = hs.menubar.new()
audioToggleMenu:setTooltip("Toggle the default audio output device")

-- Set the menubar icon corresponding to the default output device
local function setToggleAudioIcon()
   local device = hs.audiodevice.defaultOutputDevice()
   if device:name() == AUDIO_INTERNAL_NAME then
      audioToggleMenu:setTitle("🔈")
   else
      audioToggleMenu:setTitle("🎧")
   end
end

-- Toggle the default sound output bewteen built-in and DAC
local function toggleAudioDevice(modifier)
   local currOutput = hs.audiodevice.defaultOutputDevice()
   local builtinOut = hs.audiodevice.findOutputByName(AUDIO_INTERNAL_NAME)
   local fioOutput = hs.audiodevice.findOutputByName(AUDIO_EXTERNAL_NAME)

   if not builtinOut or not fioOutput then
      hs.alert.show("Cannot find output devices, or FIO is not plugged in")
      setToggleAudioIcon()
      return
   end

   if currOutput:name() == fioOutput:name() then
      builtinOut:setDefaultOutputDevice()
   else
      fioOutput:setDefaultOutputDevice()
   end

   hs.notify.new({title="Hammerspoon", informativeText="New default audio device: " .. hs.audiodevice.defaultOutputDevice():name()}):send()

   setToggleAudioIcon()
end
audioToggleMenu:setClickCallback(toggleAudioDevice)

-- Set up a watcher to update the menubar icon when the FIO gets unplugged
hs.audiodevice.watcher.setCallback(function(event)
      setToggleAudioIcon(audioToggleMenu)
end)
hs.audiodevice.watcher.start()

-- Set the initial menubar icon
setToggleAudioIcon()

--- external modules
require("weather")
-- require("music_menu")


-- reload hammerspoon (and stop all the running watchers) --
------------------------------------------------------------
local function reloadHammerspoon()
   wifi_watcher:stop()
   sleepWatcher:stop()
   monitorWatcher:stop()
   hs.audiodevice.watcher.stop()
   hs.reload()
end
hs.hotkey.bind(mash, "R", reloadHammerspoon)


--- ole'
hs.alert.show("Hammerspoon 💩 loaded!")
