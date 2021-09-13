local bit = require "bit"
local math = require "math"

function uint8_to_int8(byte)
  byte = bit.band(byte, 0xff)
  if bit.band(byte, 0x80) ~= 0 then
    -- Two's complement
    byte = bit.bnot(byte) + 1

    -- Crop out the non 8-bit data
    byte = bit.band(byte, 0xff)

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
  __index = function(t, k)
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
      error("READ UNINITIALIZED MEMORY @ "..bit.tohex(k))
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
    devices = {}
  }, self)
end

function bytes_to_shorts(bytes)
  local shorts = {}
  for i = 1, #bytes, 2 do
    local high_byte = bytes[i]
    local low_byte = bytes[i+1]
    shorts[#shorts + 1] = bit.band(bit.lshift(high_byte, 8) + low_byte, 0xffff)
  end
  return shorts
end

function bytes_to_short(high, low)
  if not low then
    low = high[2]
    high = high[1]
  end
  return bit.band(bit.lshift(high, 8) + low, 0xffff)
end

function shorts_to_bytes(shorts)
  local bytes = {}
  
  for i = 1, #shorts do
    local short = shorts[i]
    local high_byte = bit.rshift(short, 8)
    -- Mask out the low byte
    local low_byte = bit.band(short, 0xff)
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
    byte = bit.band(byte, 0xff)
    stack:push(byte)
  end
end

function Uxn:push(value, k, r, s)
  local stack = r and self.return_stack or self.program_stack
  assert(stack:check(1 + (s and 1 or 0)), "Can't push", value, "k", k, "r", r, "s", s)

  if s then
    stack:push(bit.band(bit.rshift(value, 8), 0xff))
  end
  stack:push(bit.band(value, 0xff))
end

local function extractOpcode(byte)
  local keep_bit = bit.band(byte, 0x80) ~= 0  -- 0b1000 0000
  local return_bit = bit.band(byte, 0x40) ~= 0 -- 0b0100 0000
  local short_bit = bit.band(byte, 0x20) ~= 0  -- 0b0010 0000

  local opcode = bit.band(byte, 0x1f) -- 0b0001 1111

  return opcode, keep_bit, return_bit, short_bit
end

function Uxn:device_read(addr, k, r, s)
  local device_num = bit.rshift(addr, 4)
  local device = self.devices[device_num]
  
  if device then
    local port = bit.band(addr, 0x0f)

    local value = device[port]
    
    if s then
      value = bit.lshift(value, 8) + device[port+1] 
    end
    
    return value
  else
    print("device", device_num, "doesn't exist")
    return 0
  end
end

function Uxn:device_write(addr, value, k, r, s)
  local device_num = bit.rshift(addr, 4)
  local device = self.devices[device_num]

  local port_num = bit.band(addr, 0x0f) 

  if device then
    if self.PRINT then print("wrote", bit.tohex(value), "to", bit.tohex(addr)) end
    if s then
      -- Write the low byte first
      -- Device only triggers onwrite when the high byte is written
      device[port_num+1] = bit.band(value, 0xff)
    end
    device[port_num] = value
  end
end

function Uxn:add_device(device_num, device)
  if self.devices[device_num] then
    error("Device already exists at ", bit.tohex(device_num))
  end
  
  device.cpu = self
  device.device_num = device_num

  self.devices[device_num] = device

  return device
end

function Uxn:trig_device(device_num)
  local device_addr = bit.lshift(device_num, 4)
  local vector = self:device_read(device_addr, nil, nil, true)
  if vector > 0 then
    self.ip = vector
    return self:runUntilBreak()
  end
  return nil
end

local function makeOp(n_args, f)
  return function(self, k, r, s)
    local stack_data = self:get_n(n_args, k, r, s)
    if #stack_data ~= n_args then
      print("About to crash because OP got wrong # of args")
      print("k", k, "r", r, "s", s)
      print("PS", table.concat(self.program_stack, " "))
      print("stack_data", table.concat(stack_data, " "))
      error("Expected "..tostring(n_args).." args, got "..tostring(#stack_data))
    end
    local args = {self, unpack(stack_data) }
    args[#args + 1] = k
    args[#args + 1] = r
    args[#args + 1] = s
    
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
    self:put({value}, k, r, s)
    self.ip = self.ip + (s and 2 or 1)
  end,

  -- 0x01 INC
  simpleOp(1, function(self, a)
    return a + 1
  end),

  -- 0x02 POP
  simpleOp(1, function(self, a)
    return nil
  end),

  -- 0x03 DUP
  simpleOp(1, function(self, a)
    return a, a
  end),

  -- 0x04 NIP
  simpleOp(2, function(self, a, b)
    return b
  end),

  -- 0x05 SWAP
  simpleOp(2, function(self, a, b)
    return b, a
  end),

  -- 0x06 OVER
  simpleOp(2, function(self, a, b)
    return a, b, a
  end),

  -- 0x07 ROT 
  simpleOp(3, function(self, a, b, c)
    return b, c, a
  end),


  -- LOGIC
  --
  -- 0x08 EQU
  makeOp(2, function(self, a, b, k, r, s)
    -- Push the flag as a single byte, so short mode is false in Uxn:put
    self:put({a == b and 1 or 0}, k, r, false)
  end),

  -- 0x09 NEQ
  makeOp(2, function(self, a, b, k, r, s)
    self:put({a ~= b and 1 or 0}, k, r, false)
  end),

  -- 0x0a GTH
  makeOp(2, function(self, a, b, k, r, s)
    self:put({a > b and 1 or 0}, k, r, false)
  end),

  -- 0x0b LTH
  makeOp(2, function(self, a, b, k, r, s)
    self:put({a < b and 1 or 0}, k, r, false)
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
    self:put({self.ip}, k, true, true)

    self.ip = addr
  end),

  -- 0x0f STH
  makeOp(1, function(self, a, k, r, s)
    self:put({a}, k, not r, s)
  end),

  -- MEMORY
  --
  -- 0x10 LDZ
  function(self, k, r, s)
    local offset = (self:get_n(1, k, r, false))[1]
    
    local value = self.memory[offset]

    if s then
      value = bit.lshift(value, 8) + self.memory[offset + 1]
    end

    self:put({value}, k, r, s)
  end,

  -- 0x11 STZ
  function(self, k, r, s)
    local data = self:get_n(s and 3 or 2, k, r, false)

    local offset = table.remove(data)
     
    self.memory[offset] = data[1]
    
    if s then
      self.memory[offset+1] = data[2]
    end
    if self.PRINT then
      print("[STZ]")
      for i = 0, 15 do
        s = ""
        for j = 0, 15 do
          s = s .. bit.tohex(self.memory[i*16+j], 2) .. " "
        end
        print(s)
      end
    end
  end,

  -- 0x12 LDR
  function(self, k, r, s)
    local offset = (self:get_n(1, k, r, false))[1]
    
    local addr = uint8_to_int8(offset) + self.ip

    local value = self.memory[addr]

    if s then
      value = bit.lshift(value, 8) + self.memory[addr+1]
    end

    self:put({value}, k, r, s)
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
      value = bit.lshift(value, 8) + self.memory[addr+1]
    end

    self:put({value}, k, r, s)
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
    self:put({self:device_read(offset, k, r, s)}, k, r, s)
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
  simpleOp(2, function(self, a, b)
    return a + b
  end),

  -- 0x19 SUB
  simpleOp(2, function(self, a, b)
    return a - b
  end),

  -- 0x1a MUL
  simpleOp(2, function(self, a, b)
    return a * b
  end),

  -- 0x1b DIV
  simpleOp(2, function(self, a, b)
    assert(b ~= 0, "Can't divide by zero!")
    return math.floor(a / b)
  end),

  -- 0x1c AND
  simpleOp(2, function(self, a, b)
    return bit.band(a, b)
  end),

  -- 0x1d OR
  simpleOp(2, function(self, a, b)
    return bit.bor(a, b)
  end),

  -- 0x1e EOR
  simpleOp(2, function(self, a, b)
    return bit.bxor(a, b)
  end),

  -- 0x1f SFT
  -- "Shift in short mode expects a single byte"
  function(self, k, r, s)
    local data = self:get_n(s and 3 or 2, k, r, false)

    local amount = table.remove(data)
    local value
    if s then
      value = bytes_to_short(data)
    else
      value = data[1]
    end

    value = bit.arshift(value, bit.band(amount, 0x0f))
    value = bit.lshift(value, bit.rshift(bit.band(amount, 0xf0), 4))

    self:put({value}, k, r, s)
  end,
}

function map(t, f)
  local new = {}
  for k,v in pairs(t) do
    new[k] = f(v)
  end
  return new
end

function Uxn:executeOnce()
  local opByte = self.memory[self.ip]
  if opByte == 0 then return false end
  if self.PRINT then
    print("ip", bit.tohex(self.ip)) 
  end
  self.ip = self.ip + 1

  local opcode, k, r, s = extractOpcode(opByte)
  if self.PRINT then
    print("OP", opNames[opcode], "keep", k, "return", r, "short", s)
    print("PS", table.concat(map(self.program_stack, function(b) return bit.tohex(b, 2) end), " "))
    print("RS", table.concat(map(self.return_stack, function(b) return bit.tohex(b, 2) end), " "))
  end
  opTable[opcode+1](self, k, r, s)
  return true
end

function Uxn:runUntilBreak()
  local i = 0
  while self:executeOnce() do
    i = i + 1
  end
  return i
end

return Uxn
