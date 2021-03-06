local Device = {}

Device.DEBUG_NUM_CALLS = { read = {}, write = {}}

Device.__index = function(self, k)
  if type(k) == "number" then
    Device.DEBUG_NUM_CALLS.read[self.device_num or self] = (Device.DEBUG_NUM_CALLS.read[self.device_num or self] or 0) + 1

    local value = self.portdata[k] or 0
    local port = self.ports[k]
    if port then
      if port.onread then
        value = port.onread(self) 
        if port.byte == "high" then
          value = bit.rshift(value, 8)
        end
      end
    end
    return bit.band(value, 0xff)
  elseif Device[k] then
    return Device[k]
  end
end

function Device:readShort(port)
  return bit.lshift(self[port], 8) + self[port+1]
end

function Device:writeShort(port, short)
  -- Write the low byte first since onwrite only triggers on the high byte
  self[port+1] = bit.band(short, 0xff)
  self[port] = bit.band(bit.rshift(short, 8), 0xff)
end

Device.__newindex = function(self, k, v)
  if type(k) == "number" then
    --Device.DEBUG_NUM_CALLS.write[self.device_num or self] = (Device.DEBUG_NUM_CALLS.write[self.device_num or self] or 0) + 1
    self.portdata[k] = v
    local port = self.ports[k]
    if port then
      -- Ignore write triggers for the low byte of a short
      if port.onwrite and port.byte ~= "low" then
        return port.onwrite(self, v)
      end
    end
    -- If there's no handlers attached to this port, just store the value
    if not (port and (port.onwrite or port.onread)) then
      -- Cache the result
      rawset(self, k, v)
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
  device:addPort(0, true, false, function(self, value)
    self.cpu.vectors[self.device_num] = self:readShort(0)
  end)

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
  self.cpu:triggerDevice(self.device_num)
end

return Device
