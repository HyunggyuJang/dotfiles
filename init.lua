-- input sorce changer
do  
    local korean =  "com.apple.inputmethod.Korean.2SetKorean"
    local changeInput = function()
        hs.keycodes.currentSourceID(korean)
    end
    hs.hotkey.bind({'cmd'}, 'space', changeInput)
end

local delMode = hs.hotkey.modal.new()
local currentApp = nil
local appsWithNativeDelBinding = {
    'emacs',
    'iterm2',
    'xquartz',
    'isabelle2020',
    'code'
}

function assignKeys()
    local function delChar() hs.eventtap.keyStroke({},'delete') end
    local delWord = function() hs.eventtap.keyStroke({'alt'},'delete') end
    local delLine = function() hs.eventtap.keyStroke({'cmd'},'delete') end
    delMode:bind({'ctrl'}, 'h', delChar, nil, delChar)
    delMode:bind({'ctrl'}, 'w', delWord, nil, delWord)
    delMode:bind({'ctrl'}, 'u', delLine, nil, delLine)
end

function hasValue (tab, val)
  for index, value in ipairs(tab) do
    if value == val then
      return true
    end
  end

  return false
end

function chooseKeyMap()
  if hasValue(appsWithNativeDelBinding, currentApp:lower()) then
    print('Turnning OFF keybindings for: ' .. currentApp)
    delMode:exit()      

  else
    print('Turning ON keybindings for: ' .. currentApp)
    delMode:enter()      
  end
end

function appOnStartup()
  app = hs.application.frontmostApplication()

  if app ~= nil then
    return app:title()
  end
end

function appWatcherFunction(appName, eventType, appObject)
  if (eventType == hs.application.watcher.activated) then
    currentApp = appName
    
    chooseKeyMap()    
  end
end

-- Application start
print('---------------------------------')
print('Starting Delmode hammerspoon Script')

assignKeys()

currentApp = appOnStartup()

chooseKeyMap()

local appWatcher = hs.application.watcher.new(appWatcherFunction)
appWatcher:start()
