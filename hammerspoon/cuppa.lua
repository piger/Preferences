-- Cuppa --
-----------
local HOME = os.getenv("HOME")

local cuppaMenu = hs.menubar.new()
cuppaMenu:setTitle("🍵")
cuppaMenu:setTooltip("Cuppa tea timer. ⌘+click to cancel the timer")

local cuppaTimer = nil
local cuppaTimerTimer = nil

-- stolen from Cuppa.app
local pourSound = hs.sound.getByFile(HOME .. "/Preferences/hammerspoon/pour.aiff")
local spoonSound = hs.sound.getByFile(HOME .. "/Preferences/hammerspoon/spoon.aiff")

-- for notification center
local cuppaImage = hs.image.imageFromPath(HOME .. "/Preferences/hammerspoon/cuppa.jpg")


-- set the title of the menubar
local function cuppaSetTitle(timeLeft)
   if timeLeft ~= nil then
      cuppaMenu:setTitle("🍵 " .. timeLeft)
   else
      cuppaMenu:setTitle("🍵")
   end
end

-- callback called when the cuppa timer reaches the end
local function cuppaTimerEnd()
   hs.alert.show("🍵 is ready!")
   hs.notify.new({title="Cuppa", informativeText="Your tea cup 🍵 is ready!"}):setIdImage(cuppaImage):send()
   cuppaSetTitle()
   spoonSound:play()
end

-- callback called to update the countdown timer on the menubar
local function cuppaTimerUpdate()
   if cuppaTimer ~= nil then
      local secondsLeft = cuppaTimer:nextTrigger()
      if secondsLeft < 0 then
         secondsLeft = 0
      end
      local timeLeft = os.date("!%X", math.floor(secondsLeft))
      if secondsLeft < (60 * 60) then
         timeLeft = string.sub(timeLeft, 4)
      end
      cuppaSetTitle(timeLeft)
   end
end

-- callback called by the supervisor timer to check on the cuppa timer
local function cuppaTimerPing()
   if cuppaTimer == nil or not cuppaTimer:running() then
      return true
   end
   return false
end

-- cancel the current cuppa timer
local function cuppaTimerCancel()
   if cuppaTimer ~= nil then
      cuppaTimer:stop()
   end
   if cuppaTimerTimer ~= nil then
      cuppaTimerTimer:stop()
   end
   hs.alert.show("Cuppa timer canceled")
   cuppaSetTitle()
end

-- prompt the user for a cuppa timer
local function cuppaAskForTimer(mod)
   local script = [[
   display dialog "How many minutes?" default answer "3"
   set answer to text returned of result
   return answer
   ]]

   if mod["cmd"] then cuppaTimerCancel(); return; end

   local success, out, _ = hs.osascript.applescript(script)
   if success then
      local minutes = tonumber(out)
      if minutes ~= nil and minutes > 0 then
         cuppaTimer = hs.timer.doAfter(minutes * 60, cuppaTimerEnd)
         cuppaTimerTimer = hs.timer.doUntil(cuppaTimerPing, cuppaTimerUpdate, 1)
         cuppaTimerUpdate()
         pourSound:play()
      end
   end
end

cuppaMenu:setClickCallback(cuppaAskForTimer)
