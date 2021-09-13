local Device = {}

Device.__index = function(self, k)
  if type(k) == "number" then
    local value = self.portdata[k] or 0
    local port = self.ports[k]
    if port then
      if port.onread then
        value = port.onread(self) 
      end
      
      if port.byte == "high" then
        value = bit.rshift(value, 8)
      end
    end

    return bit.band(value, 0xff)
  elseif Device[k] then
    return Device[k]
  else
    return rawget(self, k)
  end
end

function Device:readShort(port)
  return bit.lshift(self[port],8) + self[port+1]
end

Device.__newindex = function(self, k, v)
  if type(k) == "number" then
    -- Cache the result
    self.portdata[k] = v
    local port = self.ports[k]
    if port then
      -- Duplicating writing the low byte for convenience
      local b = port.byte
      if b == "high" then
        self[k+1] = bit.band(v, 0xff)
      end
      -- Ignore write triggers for the low byte of a short
      if port.onwrite and b ~= "low" then
        return port.onwrite(self, v)
      end
    end
  else 
    return rawset(self, k, v)
  end
end

function Device:new(devicenum)
  local device = setmetatable({
    ports = {},
    portdata = {},
  }, self)

  -- Add vector port by default
  device:addPort(0, true)

  return device
end

function Device:addPort(num, short, read, write)
  if short then
    self.ports[num] = {byte="high", onread=read, onwrite=write}
    self.ports[num+1] = {byte="low", onread=read, onwrite=write}
  else
    self.ports[num] = {byte="single", onread=read, onwrite=write}
  end
end

function Device:trigger()
  self.cpu:trig_device(self.device_num)
end

return Device
