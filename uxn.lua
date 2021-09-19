local bit = require "bit"
local math = require "math"

local band, bor, bxor, bnot = bit.band, bit.bor, bit.bxor, bit.bnot
local arshift, rshift, lshift = bit.arshift, bit.rshift, bit.lshift


local function uint8_to_int8(byte)
  byte = band(byte, 0xff)
  if band(byte, 0x80) ~= 0 then
    -- Two's complement
    byte = bnot(byte) + 1

    -- Crop out the non 8-bit data
    byte = band(byte, 0xff)

    -- It's now the absolute value
    -- Return a negative lua number
    byte = -byte
  end

  return byte
end

local Stack = {}

Stack.__index = Stack

function Stack:new(limit)
  local limit = limit or 256
  return setmetatable({
    limit = limit,
  }, self)
end

function Stack:push(byte)
  self[#self + 1] = byte
end

function Stack:pop()
  return table.remove(self)
end

function Stack:check(n)
  local new = #self + n
  return new >= 0 and new <= self.limit
end

function Stack:getnth(n)
  return self[#self - n]
end

function Stack:debug()
  print(unpack(self))
end

local Memory = {
  __index = function(self, k)
    if type(k) == "number" then
      --[[local rom = t.hex_rom
      if rom then
        -- Don't try to map the rom onto the zero page
        k = k - 256

        -- The rom string has two indices per byte and is 1-indexed
        k = (k * 2)
        if k >= 0 and #rom >= k then
          local rom_byte = tonumber(string.sub(rom, k + 1, k + 2), 16)
          return rom_byte
        end
      end
      ]]--
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

function Uxn:put(data, keep_bit, return_bit, short_bit)
  local n = #data

  local stack = return_bit and self.return_stack or self.program_stack

  if short_bit then n = n * 2 end

  -- Make sure the stack has enough space to push n bytes
  if not stack:check(n) then return nil end

  if short_bit then
    data = shorts_to_bytes(data)
  end

  for i = 1, #data do
    local byte = data[i]
    if type(byte) == "boolean" then
      byte = byte and 1 or 0 
    end
    
    -- Ensure that the data fits within byte limits
    byte = band(byte, 0xff)
    stack:push(byte)
  end
end

function Uxn:push(value, k, r, s)
  local stack = r and self.return_stack or self.program_stack
  assert(stack:check(1 + (s and 1 or 0)), "Can't push", value, "k", k, "r", r, "s", s)

  if s then
    stack:push(band(rshift(value, 8), 0xff))
  end
  stack:push(band(value, 0xff))
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
      value = device[port]
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
    self:profile("DEO", device_num, port_num)
    self.device_writes[device_num] = (self.device_writes[device_num] or 0) + 1
    if self.PRINT then print("wrote", bit.tohex(value), "to", bit.tohex(addr)) end

    if s then
      device:writeShort(port_num, value)
    else
      device[port_num] = value
    end
  end
end

function Uxn:add_device(device_num, device)
  if self.devices[device_num] then
    error("Device already exists at ", bit.tohex(device_num))
  end

  print("adding", device_num)
  
  device.cpu = self
  device.device_num = device_num

  self.devices[device_num] = device

  return device
end

function Uxn:trig_device(device_num)
  local vector = self.vectors[device_num]
  --local vector = self.devices[device_num]:readShort(0)
  if vector then
    self.device_triggers[device_num] = (self.device_triggers[device_num] or 0) + 1
    self.ip = vector
    return self:runUntilBreak()
  end
end

local function makeOp(n_args, f)
  local args = {}
  -- Preallocate args table
  -- 1 for self, 3 for k r s
  local len = n_args + 1 + 3
  for i = 1, len do
    args[i] = false
  end
  local k_ind = #args - 2
  local r_ind = #args - 1
  local s_ind = #args
  return function(self, k, r, s)
    local stack_data = self:get_n(n_args, k, r, s)
    if #stack_data ~= n_args then
      print("About to crash because OP got wrong # of args")
      print("k", k, "r", r, "s", s)
      print("PS", table.concat(self.program_stack, " "))
      print("stack_data", table.concat(stack_data, " "))
      error("Expected "..tostring(n_args).." args, got "..tostring(#stack_data))
    end

    args[1] = self

    for i = 1, n_args do
      args[i+1] = stack_data[i]
    end

    args[k_ind] = k
    args[r_ind] = r
    args[s_ind] = s
    
    return f(unpack(args))
  end
end

local function simpleOp(n_args, f)
  f = makeOp(n_args, f)
  return function(self, k, r, s)
    local ret = {
      f(self, k, r, s)
    }
    for i = 1, #ret do
      self:push(ret[i], k, r, s)
    end
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
  makeOp(1, function(self, a, k, r, s)
    self:push(a + 1, k, r, s)
  end),

  -- 0x02 POP
  makeOp(1, function(self, a, k, r, s)
    -- Do nothing
  end),

  -- 0x03 DUP
  makeOp(1, function(self, a, k, r, s)
    self:push(a, k, r, s)
    self:push(a, k, r, s)
  end),

  -- 0x04 NIP
  makeOp(2, function(self, a, b, k, r, s)
    self:push(b, k, r, s)
  end),

  -- 0x05 SWAP
  makeOp(2, function(self, a, b, k, r, s)
    self:push(b, k, r, s)
    self:push(a, k, r, s)
  end),

  -- 0x06 OVER
  makeOp(2, function(self, a, b, k, r, s)
    self:push(a, k, r, s)
    self:push(b, k, r, s)
    self:push(a, k, r, s)
  end),

  -- 0x07 ROT 
  makeOp(3, function(self, a, b, c, k, r, s)
    self:push(b, k, r, s)
    self:push(c, k, r, s)
    self:push(a, k, r, s)
  end),


  -- LOGIC
  --
  -- 0x08 EQU
  makeOp(2, function(self, a, b, k, r, s)
    -- Push the flag as a single byte, so short mode is false in Uxn:put
    self:push(a == b and 1 or 0, k, r, false)
  end),

  -- 0x09 NEQ
  makeOp(2, function(self, a, b, k, r, s)
    self:push(a ~= b and 1 or 0, k, r, false)
  end),

  -- 0x0a GTH
  makeOp(2, function(self, a, b, k, r, s)
    self:push(a > b and 1 or 0, k, r, false)
  end),

  -- 0x0b LTH
  makeOp(2, function(self, a, b, k, r, s)
    self:push(a < b and 1 or 0, k, r, false)
  end),

  -- 0x0c JMP
  makeOp(1, function(self, addr, k, r, s)
    if not s then
      -- relative jump
      addr = uint8_to_int8(addr) + self.ip
    end

    self.ip = addr
  end),
  
  -- 0x0d JCN
  function(self, k, r, s)
    -- Fetch the top 2 or 3 bytes
    local data = self:get_n(2 + (s and 1 or 0), k, r, false)

    -- TODO: Make this more efficient
    local flag = table.remove(data, 1)
    if flag ~= 0 then
      local addr
      if s then
        addr = bytes_to_short(data)
      else
        addr = self.ip + uint8_to_int8(data[1])
      end

      self.ip = addr
    end
  end,

  -- 0x0e JSR
  makeOp(1, function(self, addr, k, r, s)
    if not s then
      addr = uint8_to_int8(addr) + self.ip
    end
    
    -- Stash 
    self:push(self.ip, k, true, true)

    self.ip = addr
  end),

  -- 0x0f STH
  makeOp(1, function(self, a, k, r, s)
    self:push(a, k, not r, s)
  end),

  -- MEMORY
  --
  -- 0x10 LDZ
  function(self, k, r, s)
    local offset = (self:get_n(1, k, r, false))[1]
    
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
    local offset = (self:get_n(1, k, r, false))[1]
    
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
    local addr = (self:get_n(1, k, r, true))[1]

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
    local offset = (self:get_n(1, k, r, false))[1]
    self:push(self:device_read(offset, k, r, s), k, r, s)
  end,


  -- 0x17 DEO
  function(self, k, r, s)
    local data = self:get_n(s and 3 or 2, k, r, false)

    local address = table.remove(data)
    local value
    if s then
      value = bytes_to_short(data)
    else
      value = data[1]
    end
    
    self:device_write(address, value, k, r, s)
  end,


  -- ARITHMETIC
  --
  -- 0x18 ADD
  makeOp(2, function(self, a, b, k, r, s)
    self:push(a + b, k, r, s)
  end),

  -- 0x19 SUB
  makeOp(2, function(self, a, b, k, r, s)
    self:push(a - b, k, r, s)
  end),

  -- 0x1a MUL
  makeOp(2, function(self, a, b, k, r, s)
    self:push(a * b, k, r, s)
  end),

  -- 0x1b DIV
  makeOp(2, function(self, a, b, k, r, s)
    assert(b ~= 0, "Can't divide by zero!")
    self:push(math.floor(a / b), k, r, s)
  end),

  -- 0x1c AND
  makeOp(2, function(self, a, b, k, r, s)
    self:push(band(a, b), k, r, s)
  end),

  -- 0x1d OR
  makeOp(2, function(self, a, b, k, r, s)
    self:push(bor(a, b), k, r, s)
  end),

  -- 0x1e EOR
  makeOp(2, function(self, a, b, k, r, s)
    self:push(bxor(a, b), k, r, s)
  end),

  -- 0x1f SFT
  -- "Shift in short mode expects a single byte"
  function(self, k, r, s)
    local data = self:get_n(s and 3 or 2, k, r, false)

    local amount = data[#data]
    local value
    if s then
      value = bytes_to_short(data)
    else
      value = data[1]
    end

    value = arshift(value, band(amount, 0x0f))
    value = lshift(value, rshift(band(amount, 0xf0), 4))

    self:push(value, k, r, s)
  end,
}

function map(t, f)
  local new = {}
  for k,v in pairs(t) do
    new[k] = f(v)
  end
  return new
end

function Uxn:runUntilBreak()
  local i = 0
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
      print("PS", table.concat(map(self.program_stack, function(b) return bit.tohex(b, 2) end), " "))
      print("RS", table.concat(map(self.return_stack, function(b) return bit.tohex(b, 2) end), " "))
    end
    opTable[opcode+1](self, k, r, s)

    i = i + 1
  end
  return i
end

return Uxn
