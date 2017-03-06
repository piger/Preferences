-- caffeine mode --
-------------------

local caffeineMenu = hs.menubar.new()
caffeineMenu:setTooltip("Caffeinate mode (i.e. disable screen locking)")

local caffeinateSleepType = "displayIdle"

local function setCaffeineDisplay(state)
   if state then
      caffeineMenu:setTitle("🐎")
   else
      caffeineMenu:setTitle("😴")
   end
end

local function caffeineClicked()
   setCaffeineDisplay(hs.caffeinate.toggle(caffeinateSleepType))
   if hs.caffeinate.get(caffeinateSleepType) then
      hs.alert.show("Niiihhhh!")
   else
      hs.alert.show("Zzz...")
   end
end

caffeineMenu:setClickCallback(caffeineClicked)
setCaffeineDisplay(hs.caffeinate.get(caffeinateSleepType))

-- disable caffeine mode after the system wakes up from sleep
local caffeineWatcher = hs.caffeinate.watcher.new(function (eventType)
      if (eventType == hs.caffeinate.watcher.systemDidWake) then
         setCaffeineDisplay(hs.caffeinate.set(caffeinateSleepType, false, true))
      end
end)
caffeineWatcher:start()
