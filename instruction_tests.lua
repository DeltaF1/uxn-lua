local Device = require "device"
local uxn = require "uxn"

local Uxn = uxn.Uxn

function init_zero_page(memory)
  for i = 0, 255 do
    memory[i] = 0x00
  end
end

function load_program(memory, program)
  for i = 1, #program do
    memory[i+255] = program[i]
  end
  -- Don't forget a trailing BRK
  memory[#program+256] = 0x00
end

describe("the uxn instruction", function()
  local cpu
  local memory

  local function PS(offset)
    offset = offset or 0

    return cpu.program_stack[cpu.program_stack:len() + offset]
  end

  local function RS(offset)
    offset = offset or 0

    return cpu.return_stack[cpu.return_stack:len() + offset]
  end

  local function run_program(program)
    load_program(memory, program)

    return cpu:runUntilBreak()
  end

  before_each(function()
    memory = {}
    for i = 0, 255 do
      memory[i] = 0x00
    end
    cpu = Uxn:new(memory)
    cpu.ip = 0x100
  end)

  describe("LIT", function()
    it("requires a value after the opcode in memory", function()
      -- Run a program that has no literal after the LIT instruction
      assert.has_errors(function() run_program {
        0x80, -- LIT 
      } end)
    end)

    it("must have the keep, return, or short bit set", function()
      run_program {
        0x00, -- BRK
        0x12,
        0x34,
      }

      assert(cpu.program_stack:len() == 0)
      assert(cpu.return_stack:len() == 0)
    end)

    it("pushes a short in the correct byte order", function()
      local high = 0x12
      local low  = 0x34
      run_program {
        0x20, -- LIT2
        high,
        low,
      }
      
      assert(PS() == low)
      assert(PS(-1) == high)
    end)
    describe("pushes bytes to", function()
      it("the program stack by default", function()
        local value = 0x12
        run_program {
          0x80, -- LIT
          value
        }

        assert(cpu.program_stack[cpu.program_stack:len()] == value)
        assert(cpu.return_stack:len() == 0)
      end)
      it("the return stack when requested", function()
        local value = 0x12
        run_program {
          0x40, -- LITr
          value,
        }

        assert(cpu.return_stack[cpu.return_stack:len()] == value)
        assert(cpu.program_stack:len() == 0)
      end)
    end)
  end)

  describe("INC", function()
    it("increments bytes", function()
      run_program {
        0x80, 0x90, -- LIT 0x90
        0x01, -- INC
      }

      assert(PS() == 0x91)
    end)

    it("increments shorts", function()
      run_program {
        0x20, 0x90, 0xab, -- LIT 0x90ab
        0x21, -- INC2
      }
      value = 0x90ab
      value = value + 1

      assert(PS() == bit.band(value, 0xff))
      assert(PS(-1) == bit.band(bit.rshift(value, 8), 0xff))
    end)

    it("wraps bytes", function()
      run_program {
        0x80, 0xff, -- LIT 0xff
        0x01 -- INC
      }

      assert(PS() == 0x00)
    end)

    describe("wraps shorts", function()
      it("with one byte", function()
        run_program {
          0x20, 0xab, 0xff, -- LIT 0xffff
          0x21 -- INC2
        }

        assert(PS() == 0x00)
        assert(PS(-1) == 0xac)
      end)

      it("with two bytes", function()
        run_program {
          0x20, 0xff, 0xff, -- LIT 0xffff
          0x21 -- INC2
        }
        
        assert(PS() == 0x00)
        assert(PS(-1) == 0x00)
      end)
    end)
  end)

  describe("POP", function()
    it("removes bytes", function()
      run_program {
        0x80, 0x00, -- LIT  0x00
        0x40, 0x00, -- LITr 0x00
        0x02 -- POP
      }

      assert(cpu.program_stack:len() == 0)
      assert(cpu.return_stack:len() == 1)
    end)

    it("removes bytes from return stack", function()
      run_program {
        0x80, 0x00, -- LIT  0x00
        0x40, 0x00, -- LITr 0x00
        0x42 -- POPr
      }

      assert(cpu.program_stack:len() == 1)
      assert(cpu.return_stack:len() == 0)
    end)

    it("removes no bytes with keep", function()
      run_program {
        0x80, 0xab, --LIT 0xab
        0x82 -- POPk
      }

      assert(PS() == 0xab)
    end)

    it("pops shorts", function()
      run_program {
        0x80, 0xab, -- LIT 0xab
        0x20, 0x12, 0x34, -- LIT2 0x1234
        0x22 -- POP2
      }

      assert(PS() == 0xab)
      assert(cpu.program_stack:len() == 1)
    end)

    it("can't pop shorts with only one byte on the stack", function()
      assert.has_errors(function()
        run_program {
          0x80, 0xab, -- LIT 0xab
          0x22 -- POP2
        }
      end)
    end)

    it("can't pop a byte when the stack is empty", function()
      assert.has_errors(function()
        run_program {
          0x02, -- POP
        }
      end)
    end)

    it("can't pop a short when the stack is empty", function()
      assert.has_errors(function()
        run_program {
          0x22, -- POP2
        }
      end)
    end)
  end)

  describe("DUP", function()
    it("leaves 6 bytes on the stack with K and S", function()
      run_program {
        0x20, 0x78, 0x9a,
        0xa3, -- DUP2k 1010 0011
      }

      assert(cpu.program_stack:len() == 6)
    end)

    it("leaves 3 bytes on the stack with Keep", function()
      run_program {
        0x80, 0x12,
        0x83, -- DUPk
      }

      assert(cpu.program_stack:len() == 3)
    end)

    it("duplicates shorts correctly", function()
      run_program {
        0x20, 0x21, 0x43, -- LIT 0x2143
        0x23, -- DUP2
      }

      assert(cpu.program_stack:len() == 4)
      assert(PS() == 0x43)
      assert(PS(-1) == 0x21)
      assert(PS() == PS(-2))
      assert(PS(-1) == PS(-3))
    end)

    it("duplicates bytes correctly", function()
      run_program {
        0x80, 0x83,
        0x03,
      }

      assert(cpu.program_stack:len() == 2)
      assert(PS() == 0x83)
      assert(PS() == PS(-1))
    end)
  end)

  describe("NIP", function()
    pending("one byte fail", true)
    pending("one short fail", true)
    pending("one byte short fail", true)

    pending("byte", true)
    pending("short", true)

    pending("return stack", true)

    pending("keep", true)
  end)

  pending("SWAP", function()

  end)

  pending("OVER", function()

  end)

  describe("EQU", function()
    describe("returns 1", function()
      it("with equal bytes", function()
        run_program {
          0x80, 0x34,
          0x80, 0x34,
          0x08
        }

        assert(PS() == 0x01)
        assert(cpu.program_stack:len() == 1)
      end)
      
      it("with equal shorts", function()
        run_program {
          0x20, 0x12, 0x34, -- LIT 0x1234
          0x20, 0x12, 0x34, -- LIT 0x1234
          0x28 -- EQU2
        }

        assert(PS() == 0x01)
        assert(cpu.program_stack:len() == 1)
      end)

      pending("keep", true)
    end)

    describe("returns 0", function()
      it("not equal bytes", function()
        run_program {
          0x80, 0xab, -- LIT 0xab
          0x80, 0x34, -- LIT 0x34
          0x08
        }

        assert(PS() == 0x00)
        assert(cpu.program_stack:len() == 1)
      end)
      
      it("fully not equal shorts", function()
        run_program {
          0x20, 0x12, 0x34, -- LIT 0x1234
          0x20, 0x21, 0x43, -- LIT 0x2143
          0x28, -- EQU2
        }

        assert(PS() == 0x00)
        assert(cpu.program_stack:len() == 1)
      end)
      
      it("with shorts with same low bytes", function()
        run_program {
          0x20, 0x12, 0x34, -- LIT 0x1234
          0x20, 0xab, 0x34, -- LIT 0x1234
          0x28 -- EQU2
        }

        assert(PS() == 0x00)
        assert(cpu.program_stack:len() == 1)
      end)
      
      it("with shorts with same high bytes", function()
        run_program {
          0x20, 0x12, 0x34, -- LIT 0x1234
          0x20, 0x12, 0xde, -- LIT 0x1234
          0x28 -- EQU2
        }

        assert(PS() == 0x00)
        assert(cpu.program_stack:len() == 1)
      end)
    end)
  end)
  
  describe("NEQ", function()
    describe("returns 0", function()
      it("with equal bytes", function()
        run_program {
          0x80, 0x34,
          0x80, 0x34,
          0x09
        }

        assert(PS() == 0x00)
        assert(cpu.program_stack:len() == 1)
      end)
      
      it("with equal shorts", function()
        run_program {
          0x20, 0x12, 0x34, -- LIT 0x1234
          0x20, 0x12, 0x34, -- LIT 0x1234
          0x29 -- NEQ2
        }

        assert(PS() == 0x00)
        assert(cpu.program_stack:len() == 1)
      end)

      pending("keep", true)
    end)

    describe("returns 1", function()
      it("not equal bytes", function()
        run_program {
          0x80, 0x34, -- LIT 0x34
          0x80, 0xc3, -- LIT 0xc3
          0x09
        }

        assert(PS() == 0x01)
        assert(cpu.program_stack:len() == 1)
      end)

      it("fully not equal shorts", function()
        run_program {
          0x20, 0x12, 0x34, -- LIT 0x1234
          0x20, 0x21, 0x43, -- LIT 0x2143
          0x29, -- NEQ2
        }

        assert(PS() == 0x01)
        assert(cpu.program_stack:len() == 1) 
      end)
      
      it("with shorts with same low bytes", function()
        run_program {
          0x20, 0x12, 0x34, -- LIT 0x1234
          0x20, 0xab, 0x34, -- LIT 0x1234
          0x29 -- NEQ2
        }

        assert(PS() == 0x01)
        assert(cpu.program_stack:len() == 1)
      end)
      
      it("with shorts with same high bytes", function()
        run_program {
          0x20, 0x12, 0x34, -- LIT 0x1234
          0x20, 0x12, 0xde, -- LIT 0x1234
          0x29 -- NEQ2
        }

        assert(PS() == 0x01)
        assert(cpu.program_stack:len() == 1)
      end)
    end)
  end)

  pending("GTH", function()
    
  end)

  pending("LTH", function()
    
  end)

  describe("JMP", function()
    describe("jumps to a relative", function()
      it("negative offset", function()
        offset = -10
        init_zero_page(memory)
        load_program(memory, {
          0x80, bit.band(offset, 0xff), -- LIT value
          0x0c, -- relative byte jump
        })
        
        cpu:executeOnce()
        assert(cpu.ip == 0x102)

        cpu:executeOnce()

        assert(cpu.ip == 0x102 + 1 + offset, "ip is incremented by 1, so must take that into account") 
      end)

      it("positive offset", function()
        offset = 10
        load_program(memory, {
          0x80, bit.band(offset, 0xff), -- LIT value
          0x0c, -- relative byte jump
          0x00, -- padding
          0x00, 0x00, 0x00, 0x00, 0x00,
          0x00, 0x00, 0x00, 0x00, 0x00,
        })
        
        cpu:executeOnce()
        assert(cpu.ip == 0x102)

        cpu:executeOnce()

        assert(cpu.ip == 0x102 + 1 + offset, "ip is incremented by 1, so must take that into account") 

      end)

      it("zero offset", function()
        run_program {
          0x80, 0x00, -- LIT 0x00
          0x0c,
          0x80, 0xab,
        }

        assert(PS() == 0xab)
        assert(cpu.program_stack:len() == 1)
      end)

      it("wrapping offset", function()
        load_program(memory, {
          0x80, 0x7f, -- LIT 0x7f (127)
          0x80, 0x7b, -- LIT 0x7b (123)
          0x0c, -- Relative byte jump
        })

        for i = 0x107, 0x201 do
          memory[i] = 0x00
        end
        
        for i = 1, 3 do
          cpu:executeOnce()
        end
        
        assert(cpu.ip == 0x180)
        
        memory[cpu.ip] = 0x0c -- relative byte jump

        cpu:executeOnce()
        assert(cpu.ip == 0x200)
      end)
    end)

    describe("jumps to an absolute", function() 
      pending("short", function() end)
      pending("zero-page address", function() end)
    end)
  end)
  
  pending("JCN", function()
    
  end)
  
  pending("JSR", function()
    
  end)
  
  describe("STASH", function()
    describe("moves one", function()
      it("byte from program to return", function()
        run_program {
          0x80, 0x12, -- LIT  0x12
          0x40, 0x34, -- LITr 0x34
          0x0f, -- STH
        }

        assert(cpu.program_stack:len() == 0)
        assert(cpu.return_stack:len() == 2)
        
        assert(RS() == 0x12)
        assert(RS(-1) == 0x34)
      end)

      it("byte from return to program", function()
        run_program {
          0x80, 0x12, -- LIT  0x12
          0x40, 0x34, -- LITr 0x34
          0x4f, -- STHr
        }

        assert(cpu.program_stack:len() == 2)
        assert(cpu.return_stack:len() == 0)
        
        assert(PS() == 0x34)
        assert(PS(-1) == 0x12)
      end)

      it("short from program to return", function()
        run_program {
          0x20, 0x12, 0x34, -- LIT  0x1234
          0x60, 0xab, 0xcd, -- LITr 0xabcd
          0x2f -- STH
        }
      
        assert(cpu.program_stack:len() == 0)
        assert(cpu.return_stack:len() == 4)

        assert(RS() == 0x34)
        assert(RS(-1) == 0x12)
        assert(RS(-2) == 0xcd)
        assert(RS(-3) == 0xab)
      end)
      
      it("short from return to program", function()
        run_program {
          0x20, 0x12, 0x34, -- LIT  0x1234
          0x60, 0xab, 0xcd, -- LITr 0xabcd
          0x6f -- STH
        }
      
        assert(cpu.program_stack:len() == 4)
        assert(cpu.return_stack:len() == 0)

        assert(PS() == 0xcd)
        assert(PS(-1) == 0xab)
        assert(PS(-2) == 0x34)
        assert(PS(-3) == 0x12)
      end)
    end)

    it("keeps number of bytes properly", function()
      run_program {
        0x20, 0x21, 0x43, -- LIT 0x2143
        0xaf, -- STH2k
      }

      assert(cpu.program_stack:len() == 2)
      assert(cpu.return_stack:len() == 2)
    end)
  end)

  describe("ADD", function()
    it("adds two bytes without overflow", function()
      value1 = 0x45
      value2 = 0x2c

      run_program {
        0x80, value1,
        0x80, value2,
        0x18, -- ADD
      }

      assert(PS() == value1 + value2)
    end)
    
    it("adds two bytes with overflow", function()
      value1 = 0xdf
      value2 = 0x30

      run_program {
        0x80, value1,
        0x80, value2,
        0x18, -- ADD
      }

      assert(PS() == 0x0f)
    end)

    describe("adds two shorts", function()
      it("with no overflow", function()
        run_program {
          0x20, 0x12, 0xa0, -- LIT 0x12fe
          0x20, 0x34, 0x02, -- LIT 0x3402
          0x38, -- ADD2
        }

        assert(PS() == 0xa0 + 0x02)
        assert(PS(-1) == 0x12 + 0x34)
      end)

     it("with low overflow", function()
        run_program {
          0x20, 0x12, 0xfe, -- LIT 0x12fe
          0x20, 0x34, 0x02, -- LIT 0x3402
          0x38, -- ADD2
        }

        assert(PS(-1) == 0x12 + 0x34 + 0x01, "top byte should be + 1")
        assert(PS() == 0x00)
      end)

      it("with high overflow", function()
        run_program {
          0x20, 0xdf, 0x12, -- LIT 0x12fe
          0x20, 0x21, 0x34, -- LIT 0x3402
          0x38, -- ADD2
        }

        assert(PS(-1) == 0x00)
        assert(PS() == 0x12 + 0x34)
      end)
    end)
  end)

  describe("DIV", function()
    it("doesn't divide by zero", function()
      assert.has_error(function()
        run_program {
          0x80, 0x40,
          0x80, 0x00,
          0x1b,
        }
      end)
    end)

    it("divides cleanly by 2", function()
      run_program {
        -- DIV 0x34/0x02
        0x80, 0x34,
        0x80, 0x02,
        0x1b,

        -- DIV 0xfe/0x02
        0x80, 0xfe,
        0x80, 0x02,
        0x1b,

        -- DIV 0xceaa/0x0002
        0x20, 0xce, 0xaa,
        0x20, 0x00, 0x02,
        0x3b,
      }

      assert(cpu.program_stack:len() == 4)
      assert(PS() == 0x55)
      assert(PS(-1) == 0x67)

      assert(PS(-2) == 0x7f)

      assert(PS(-3) == 0x1a)
    end)
  end)
  
  describe("LDZ", function()
    before_each(function()
      -- Initialize the zero page to avoid uninitialized errors
      for i = 0x00, 0xff do
        memory[i] = 0
      end

      -- Boundary conditions
      memory[0x00] = 0x12
      memory[0x01] = 0x34

      memory[0xfe] = 0x78
      memory[0xff] = 0x9a
      
      -- Byte test
      memory[0x09] = 0x1d
      memory[0x0a] = 0x3c
      memory[0x0b] = 0x5a

      -- Short test
      memory[0x9f] = 0xab
      memory[0xa0] = 0xcd
      memory[0xa1] = 0xef
    end)

    it("fetches bytes", function()
      run_program {
        -- Fetch 0x00
        0x80, 0x00, 0x10,

        -- Fetch a byte in the middle of the page
        0x80, 0x0a, 0x10,

        -- Fetch 0xff
        0x80, 0xff, 0x10,
      }

      assert(cpu.program_stack:len() == 3)
      assert(PS() == 0x9a)
      assert(PS(-1) == 0x3c)
      assert(PS(-2) == 0x12)
    end)

    it("fetches shorts", function()
      run_program {
        -- Fetch 0x00
        0x80, 0x00, 0x30,

        -- Fetch a short in the middle of the page
        0x80, 0xa0, 0x30,

        -- Fetch 0xff
        0x80, 0xfe, 0x30,
      }

      assert(cpu.program_stack:len() == 6)
      assert(PS() == 0x9a)
      assert(PS(-1) == 0x78)
      assert(PS(-2) == 0xef)
      assert(PS(-3) == 0xcd)
      assert(PS(-4) == 0x34)
      assert(PS(-5) == 0x12)
    end)
  end)

  describe("STZ", function()
    it("only takes one byte of address", function()
      run_program {
        0x20, 0xff, 0xff, -- Push values that should not be touched
        0x80, 0xab,
        0x80, 0x0a,

        0x11, -- STZ

        0x20, 0x21, 0x43,
        0x80, 0xa0, -- Store a short at 0xa0
        0x31, -- STZ2

      }
      
      assert(cpu.program_stack:len() == 2)
    end)

    it("overwrites manually set values ", function()
      memory[0x9c] = 0xab

      run_program {
        0x80, 0x34, -- LIT 0x34
        0x80, 0x9c, -- Address 0x9c
        0x11, -- STZ
      }

      assert(memory[0x9c] == 0x34)
    end)
  end)

  pending("LDA", function()

  end)

  pending("STA", function()

  end)

  describe("LDR", function()
    it("loads positive offsets", function()
      run_program {
        0x80, 0x02, -- Offset 0x02
        0x12, -- LDR
        0x00,
        0x00,
        0x12,
      }

      assert(PS() == 0x12)
    end)

    it("loads negative offsets", function()
      run_program {
        0x80, 0x08, -- Jump offset
        0x0c, -- Relative jump
        0x00,
        0x00,
        0x00,
        0x00,
        0xab, -- The desired data
        0x00,
        0x00,
        0x00,
        0x80, 0xf9, -- Load offset (-6)
        0x12,
      }

      assert(PS() == 0xab)
    end)

    it("loads shorts", function()
      run_program {
        0x80, 0x02, -- Load offset
        0x32, -- LDR2
        0x00,
        0x00,
        0x21,
        0x43,
        0x65,
        0x00,
      }

      assert(cpu.program_stack:len() == 2)
      assert(PS() == 0x43)
      assert(PS(-1) == 0x21)
    end)
  end)

  pending("STR", function()

  end)

  describe("DEI", function()
    before_each(function()
      local mem_device = Device:new()
      mem_device:addPort(2, true)

      mem_device:addPort(4, true)

      mem_device:addPort(6, false)
      mem_device:addPort(7, false)

      mem_device:writeShort(0x02, 0x2143)
      mem_device[0x04] = 0x7c -- Too short value in short port

      mem_device[0x06] = 0xab
      mem_device:writeShort(0x07, 0x1337) -- Too long value in byte port

      cpu:addDevice(1, mem_device)

      local func_device = Device:new()
      
      func_device:addPort(2, true, function() return 0x87a9 end, nil)
      func_device:addPort(4, true, function() return 0x34 end, nil)
      
      func_device:addPort(6, false, function() return 0xab end, nil)
      func_device:addPort(7, false, function() return 0x1337 end, nil)

      cpu:addDevice(2, func_device)
    end)

    it("fetches stored bytes", function()
      run_program {
        0x80, 0x17, -- Address of too long byte
        0x16, -- DEI

        0x80, 0x12, -- Address of short port
        0x16, -- DEI

        0x80, 0x16, -- Address of proper byte port
        0x16, -- DEI
      }

      assert(cpu.program_stack:len() == 3, "Only bytes fetched")
      assert(PS() == 0xab)
      assert(PS(-1) == 0x21)
      assert(PS(-2) == 0x13)
    end)

    it("fetches stored shorts", function()
      run_program {
        0x80, 0x12, -- Address of short port 
        0x36, -- DEI2

        0x80, 0x14, -- Address of too short short port
        0x36, -- DEI2

        0x80, 0x16, -- Address of byte port
        0x36, -- DEI2
      }
      
      assert(cpu.program_stack:len() == 6, "Only fetch shorts")
      print("PS")
      cpu.program_stack:debug()

      -- Gets the byte from the next mem slot
      assert(PS() == 0x13)
      assert(PS(-1) == 0xab)
      
      -- Fetching from a port with too short of a value
      assert(PS(-2) == 0x00) 
      assert(PS(-3) == 0x7c)
     
      -- Fetches normal short properly
      assert(PS(-4) == 0x43)
      assert(PS(-5) == 0x21) 
    end)

    pending("fetches function bytes", function()
    
    end)
  end)
end)

describe("helper function", function()
  test("uint8_to_int8", function()
    local u = uxn.uint8_to_int8
    assert(u(0xff) == -1)
    assert(u(128) == -128)
    assert(u(127) == 127)
    assert(u(0) == 0)
    assert(u(-10) == -10)
    assert(u(55) == 55)
    assert(u(0xfffe) == -2)
    assert(u(0x000e) == 0x0e)
  end)

  describe("bytes_to_short", function()
    pending("larger than 16 bits", true) 
  end)
end)
