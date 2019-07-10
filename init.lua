-- input sorce changer
do  
    local korean =  "com.apple.inputmethod.Korean.2SetKorean"
    local changeInput = function()
        hs.keycodes.currentSourceID(korean)
    end
    hs.hotkey.bind({'cmd','ctrl'}, 'space', changeInput)
    -- hs.hotkey.bind({},'escape',function() hs.eventtap.keyStroke({}, 'CAPS_LOCK') end)
end
DelMode = {mode = nil, enabled = true}

function DelMode:new()
    self.mode = hs.hotkey.modal.new()
    self.enabled = false
    self.watchers = {}
    return self
end
function DelMode:enable()
    if self.enabled then return end
    self.mode:enter()
    self.enabled = true
    -- print "deactivated works"
end
function DelMode:exit()
    if not self.enabled then return end
    self.mode:exit()
    self.enabled = false
    -- print "it works"
end
function DelMode:bindModeKeys()
    local function delChar() hs.eventtap.keyStroke({},'delete') end
    local delWord = function() hs.eventtap.keyStroke({'alt'},'delete') end
    local delLine = function() hs.eventtap.keyStroke({'cmd'},'delete') end
    self.mode:bind({'ctrl'}, 'h', delChar, nil, delChar)
    self.mode:bind({'ctrl'}, 'w', delWord, nil, delWord)
    self.mode:bind({'ctrl'}, 'u', delLine, nil, delLine)
end
function DelMode:disableForApp(disabledApp)
  local myDel = self

  local watcher =
    hs.application.watcher.new(function(applicationName, eventType)
      if disabledApp ~= applicationName then return end
      if eventType == hs.application.watcher.activated then
        myDel:exit()
      elseif eventType == hs.application.watcher.deactivated then
        myDel:enable()
      end
    end)

  watcher:start()

  self.watchers[disabledApp] = watcher
end

myDel = DelMode:new()
myDel:bindModeKeys()
myDel:enable()
myDel:disableForApp('iTerm2')
