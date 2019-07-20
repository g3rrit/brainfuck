
-- ------------------ TAPE_CLASS ------------------- --

Tape = {}

function Tape:create(v, n, p)
  local t = {}
  setmetatable(t, self)
  self.__index = self
  t.n = n
  t.p = p
  t.from = {}
  t.to = {}
  t.v = v
  return t
end

function Tape:next()
  local nv = 0
  table.insert(self.to, self.v)
  if #self.from >= 1 then
    nv = self.from[#self.from]
    table.remove(self.from, #self.from)
  else
    nv = self.n()
  end
  self.v = nv
  return nv
end

function Tape:prev()
  local nv = 0
  table.insert(self.from, self.v)
  if #self.to >= 1 then
    nv = self.to[#self.to]
    table.remove(self.to, #self.to)
  else
    nv = self.n()
  end
  self.v = nv
  return nv
end

function Tape:get()
  return self.v
end

function Tape:set(v)
  self.v = v
end

-- ------------------ TAPE_MEMORY ------------------- --

intape = Tape:create(0,
function() return io.read(1) end,
function() print("Unable to rewind input that far") os.exit() end)

mem = Tape:create(0,
function() return 0 end,
function() return 0 end)

-- ------------------ SEEK_FUNCTIONS ------------------- --

function seek_right(d)
  local c = intape:next()
  if c == ']' then
    if d <= 0 then
      return
    end
    seek_right(d - 1)
  elseif c == '[' then
    seek_right(d + 1)
  else
    seek_right(d)
  end
end

function seek_left(d)
  local c = intape:prev()
  if c == '[' then
    if d <= 0 then
      return
    end
    seek_left(d - 1)
  elseif c == ']' then
    seek_left(d + 1)
  else
    seek_left(d)
  end
end


-- ------------------ MAIN_LOOP ------------------- --

while true do
  local c = intape:next()

  if     c == '>' then mem:next()
  elseif c == '<' then mem:prev()
  elseif c == '+' then mem:set(mem:get() + 1)
  elseif c == '-' then mem:set(mem:get() - 1)
  elseif c == '.' then io.write(string.char(mem:get())) io.flush()
  elseif c == ',' then mem:set(string.byte(io.read(1)))
  elseif c == '[' then if mem:get() == 0 then seek_right(0) end
  elseif c == ']' then if mem:get() ~= 0 then seek_left(0) end
  end

end
