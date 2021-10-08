local bit = require "bit"
local math = require "math"

local band, bor, bxor, bnot = bit.band, bit.bor, bit.bxor, bit.bnot
local arshift, rshift, lshift = bit.arshift, bit.rshift, bit.lshift

local function uint8_to_int8(byte)
  byte = band(byte, 0xff)
  if band(byte, 0x80) ~= 0 then
    byte = byte - 0x100
  end

  return byte
end

local Stack = {}

Stack.__index = Stack

function Stack:new(limit)
  local limit = limit or 256
  return setmetatable({
    limit = limit,
    head = 0,
  }, self)
end

function Stack:push(byte)
  local head = self.head + 1
  self[head] = byte
  self.head = head
end

function Stack:pop()
  local head = self.head
  local byte = self[head]
  self.head = head - 1
  return byte
end

function Stack:check(n)
  local new = self.head + n
  return new >= 0 and new <= self.limit
end

-- 0-indexed non-destructive get
function Stack:getnth(n)
  return self[self.head - n]
end

function Stack:len()
  return self.head
end

function Stack:debug()
  local t = {}
  for i = 1, self.head do
    t[#t + 1] = bit.tohex(self[i], 2)
  end
  return table.concat(t, " ")
end

local Memory = {
  __index = function(self, k)
    if type(k) == "number" then
      -- Uninitialized memory should be randomized for robust testing
      if self.ERROR_ON_UNINITIALIZED_READ then
        error("READ UNINITIALIZED MEMORY @ "..bit.tohex(k))
      end
      return love.math.random(255)
    end
  end
}

local Uxn = {}

Uxn.__index = Uxn

function Uxn:new(mem)
  return setmetatable({
    ip = 1,
    program_stack = Stack:new(),
    return_stack = Stack:new(),
    memory = setmetatable(mem or {}, Memory),
    devices = {},
    vectors = {},
    -- DEBUG TABLES
    debug_profile = {},
    device_triggers = {}, -- Debug
    device_reads = {}, -- DEBUG
    device_writes = {}, -- DEBUG
  }, self)
end

function Uxn:profile(...)
  local name = table.concat({...}, "_")
  self.debug_profile[name] = (self.debug_profile[name] or 0) + 1
end

function Uxn:print_profile()
  local output = "name\t\tcount\n"
  for k,v in pairs(self.debug_profile) do
    output = output .. k.."\t\t"..v.."\n"
  end
  return output
end

function bytes_to_shorts(bytes)
  local shorts = {}
  for i = 1, #bytes, 2 do
    local high_byte = bytes[i]
    local low_byte = bytes[i+1]
    shorts[#shorts + 1] = band(lshift(high_byte, 8) + low_byte, 0xffff)
  end
  return shorts
end

function bytes_to_short(high, low)
  if not low then
    low = high[2]
    high = high[1]
  end
  return band(lshift(high, 8) + low, 0xffff)
end

function shorts_to_bytes(shorts)
  local bytes = {}

  for i = 1, #shorts do
    local short = shorts[i]
    local high_byte = rshift(short, 8)
    -- Mask out the low byte
    local low_byte = band(short, 0xff)
    bytes[#bytes + 1] = high_byte
    bytes[#bytes + 1] = low_byte
  end

  return bytes
end

function Uxn:get_n(n, keep_bit, return_bit, short_bit)
  -- Choose which stack to operate on
  local stack = return_bit and self.return_stack or self.program_stack

  -- Fetch 2 bytes for each number needed
  if short_bit then n = n * 2 end

  -- Make sure the stack has enough space to fetch n bytes
  if not stack:check(-n) then
    error("Stack not big enough to get "..tostring(n).." bytes")
  end

  local output = {}

  for i = 1, n do
    if keep_bit then
      output[(n - i) + 1] = stack:getnth(i-1)
    else
      output[(n - i) + 1] = stack:pop()
    end
  end

  if short_bit then
    output = bytes_to_shorts(output)
  end

  return output
end

function Uxn:push(value, k, r, s)
  local stack = r and self.return_stack or self.program_stack
  --assert(stack:check(1 + (s and 1 or 0)), "Can't push", value, "k", k, "r", r, "s", s)

  if s then
    stack:push(band(rshift(value, 8), 0xff))
  end
  stack:push(band(value, 0xff))
end

function Uxn:pop(k, r, s)
  local stack = r and self.return_stack or self.program_stack
  local value
  if k then
    local offset = self.peek_offset
    if s then
      value = bytes_to_short(stack:getnth(offset+1), stack:getnth(offset))
      offset = offset + 2
    else
      value = stack:getnth(offset)
      offset = offset + 1
    end
    self.peek_offset = offset
  else
    if s then
      local low = stack:pop()
      return bytes_to_short(stack:pop(), low)
    else
      return stack:pop()
    end
  end
  return value
end

local function extractOpcode(byte)
  local keep_bit = band(byte, 0x80) ~= 0  -- 0b1000 0000
  local return_bit = band(byte, 0x40) ~= 0 -- 0b0100 0000
  local short_bit = band(byte, 0x20) ~= 0  -- 0b0010 0000

  local opcode = band(byte, 0x1f) -- 0b0001 1111

  return opcode, keep_bit, return_bit, short_bit
end

function Uxn:device_read(addr, k, r, s)
  local device_num = rshift(addr, 4)
  local device = self.devices[device_num]

  if device then
    self.device_reads[device_num] = (self.device_reads[device_num] or 0) + 1
    local port = band(addr, 0x0f)

    local value
    if s then
      value = device:readShort(port)
    else
      value = band(device[port], 0xff)
    end

    return value
  else
    print("device", device_num, "doesn't exist")
    return 0
  end
end

function Uxn:device_write(addr, value, k, r, s)
  local device_num = rshift(addr, 4)
  local device = self.devices[device_num]

  local port_num = band(addr, 0x0f)

  if device then
    --self:profile("DEO", device_num, port_num)
    self.device_writes[device_num] = (self.device_writes[device_num] or 0) + 1
    if self.PRINT then print("wrote", bit.tohex(value), "to", bit.tohex(addr)) end

    if s then
      device:writeShort(port_num, value)
    else
      device[port_num] = value
    end
  end
end

function Uxn:addDevice(device_num, device)
  if self.devices[device_num] then
    error("Device already exists at ", bit.tohex(device_num))
  end

  print("adding", device_num)

  device.cpu = self
  device.device_num = device_num

  self.devices[device_num] = device

  return device
end

function Uxn:triggerDevice(device_num)
  local vector = self.vectors[device_num]
  --local vector = self.devices[device_num]:readShort(0)
  if vector then
    self.device_triggers[device_num] = (self.device_triggers[device_num] or 0) + 1
    self.ip = vector
    return self:runUntilBreak()
  end
end

local opNames = {
  [0] = "LIT",
  "INC",
  "POP",
  "DUP",
  "NIP",
  "SWAP",
  "OVER",
  "ROT",

  "EQU",
  "NEQ",
  "GTH",
  "LTH",
  "JMP",
  "JCN",
  "JSR",
  "STASH",

  "LDZ",
  "STZ",
  "LDR",
  "STR",
  "LDA",
  "STA",
  "DEI",
  "DEO",

  "ADD",
  "SUB",
  "MUL",
  "DIV",
  "AND",
  "OR",
  "XOR",
  "SHIFT",
}

local opTable = {
  -- STACK
  --
  -- 0x00 BRK/LIT
  function(self, k, r, s)
    local value
    if s then
      value = bytes_to_short(self.memory[self.ip], self.memory[self.ip+1])
    else
      value = self.memory[self.ip]
    end
    if self.PRINT then print("Push "..(s and "short" or "byte").." value = ",bit.tohex(value)) end
    self:push(value, k, r, s)
    self.ip = self.ip + (s and 2 or 1)
  end,

  -- 0x01 INC
  function(self, k, r, s)
    local a = self:pop(k, r, s)
    self:push(a + 1, k, r, s)
  end,

  -- 0x02 POP
  function(self, k, r, s)
    self:pop(k, r, s)
  end,

  -- 0x03 DUP
  function(self, k, r, s)
    local a = self:pop(k, r, s)
    self:push(a, k, r, s)
    self:push(a, k, r, s)
  end,

  -- 0x04 NIP
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    self:pop(k, r, s)
    self:push(b, k, r, s)
  end,

  -- 0x05 SWAP
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(b, k, r, s)
    self:push(a, k, r, s)
  end,

  -- 0x06 OVER
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(a, k, r, s)
    self:push(b, k, r, s)
    self:push(a, k, r, s)
  end,

  -- 0x07 ROT
  function(self, k, r, s)
    local c = self:pop(k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(b, k, r, s)
    self:push(c, k, r, s)
    self:push(a, k, r, s)
  end,


  -- LOGIC
  --
  -- 0x08 EQU
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    -- Push the flag as a single byte, so short mode is false in Uxn:push
    self:push(a == b and 1 or 0, k, r, false)
  end,

  -- 0x09 NEQ
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(a ~= b and 1 or 0, k, r, false)
  end,

  -- 0x0a GTH
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(a > b and 1 or 0, k, r, false)
  end,

  -- 0x0b LTH
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(a < b and 1 or 0, k, r, false)
  end,

  -- 0x0c JMP
  function(self, k, r, s)
    local addr = self:pop(k, r, s)
    if not s then
      -- relative jump
      addr = uint8_to_int8(addr) + self.ip
    end

    self.ip = addr
  end,

  -- 0x0d JCN
  function(self, k, r, s)
    local addr = self:pop(k, r, s)
    local flag = self:pop(k, r, false)

    if flag ~= 0 then
      if not s then
        addr = self.ip + uint8_to_int8(addr)
      end

      self.ip = addr
    end
  end,

  -- 0x0e JSR
  function(self, k, r, s)
    local addr = self:pop(k, r, s)
    if not s then
      addr = uint8_to_int8(addr) + self.ip
    end

    -- Stash
    self:push(self.ip, k, true, true)

    self.ip = addr
  end,

  -- 0x0f STH
  function(self, k, r, s)
    local a = self:pop(k, r, s)
    self:push(a, k, not r, s)
  end,

  -- MEMORY
  --
  -- 0x10 LDZ
  function(self, k, r, s)
    local offset = self:pop(k, r, false)

    local value = self.memory[offset]

    if s then
      value = lshift(value, 8) + self.memory[offset + 1]
    end

    self:push(value, k, r, s)
  end,

  -- 0x11 STZ
  function(self, k, r, s)
    local data = self:get_n(s and 3 or 2, k, r, false)

    local offset = table.remove(data)

    self.memory[offset] = data[1]

    if s then
      self.memory[offset+1] = data[2]
    end
  end,

  -- 0x12 LDR
  function(self, k, r, s)
    local offset = self:pop(k, r, false)

    local addr = uint8_to_int8(offset) + self.ip

    local value = self.memory[addr]

    if s then
      value = lshift(value, 8) + self.memory[addr+1]
    end

    self:push(value, k, r, s)
  end,

  -- 0x13 STR
  function(self, k, r, s)
    local data = self:get_n(s and 3 or 2, k, r, false)

    local address = uint8_to_int8(table.remove(data)) + self.ip

    self.memory[address] = data[1]

    if s then
      self.memory[address+1] = data[2]
    end
  end,

  -- 0x14 LDA
  function(self, k, r, s)
    local addr = self:pop(k, r, true)

    local value = self.memory[addr]

    if s then
      value = lshift(value, 8) + self.memory[addr+1]
    end

    self:push(value, k, r, s)
  end,

  -- 0x15 STA
  function(self, k, r, s)
    local data = self:get_n(s and 4 or 3, k, r, false)

    local address = bytes_to_short(data[#data-1], data[#data])

    self.memory[address] = data[1]

    if s then
      self.memory[address+1] = data[2]
    end
  end,


  -- 0x16 DEI
  function(self, k, r, s)
    local offset = self:pop(k, r, false)
    self:push(self:device_read(offset, k, r, s), k, r, s)
  end,


  -- 0x17 DEO
  function(self, k, r, s)
    local address = self:pop(k, r, false)
    local value = self:pop(k, r, s)

    self:device_write(address, value, k, r, s)
  end,


  -- ARITHMETIC
  --
  -- 0x18 ADD
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(a + b, k, r, s)
  end,

  -- 0x19 SUB
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(a - b, k, r, s)
  end,

  -- 0x1a MUL
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(a * b, k, r, s)
  end,

  -- 0x1b DIV
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    assert(b ~= 0, "Can't divide by zero!")
    self:push(math.floor(a / b), k, r, s)
  end,

  -- 0x1c AND
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(band(a, b), k, r, s)
  end,

  -- 0x1d OR
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(bor(a, b), k, r, s)
  end,

  -- 0x1e EOR
  function(self, k, r, s)
    local b = self:pop(k, r, s)
    local a = self:pop(k, r, s)
    self:push(bxor(a, b), k, r, s)
  end,

  -- 0x1f SFT
  -- "Shift in short mode expects a single byte"
  function(self, k, r, s)
    local amount = self:pop(k, r, false)
    local value = self:pop(k, r, s)

    value = arshift(value, band(amount, 0x0f))
    value = lshift(value, rshift(band(amount, 0xf0), 4))

    self:push(value, k, r, s)
  end,
}

function Uxn:runUntilBreak()
  local count = 0
  local extractOpcode = extractOpcode
  local memory = self.memory
  while true do
    local opByte = memory[self.ip]
    if opByte == 0 then break end
    if self.PRINT then
      print("ip", bit.tohex(self.ip))
    end
    self.ip = self.ip + 1

    local opcode, k, r, s = extractOpcode(opByte)
    --self:profile(opNames[opcode])
    if self.PRINT then
      print("OP", opNames[opcode], "keep", k, "return", r, "short", s)
      print("PS", self.program_stack:debug())
      print("RS", self.return_stack:debug())
    end
    -- Reset the peek offset so consective calls to :pop maintain state
    if k then self.peek_offset = 0 end
    opTable[opcode+1](self, k, r, s)

    count = count + 1
  end
  return count
end

return {Uxn = Uxn, uint8_to_int8 = uint8_to_int8}
