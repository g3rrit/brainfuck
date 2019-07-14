import System.IO
import Data.Char

data State = State [Int] Int [Int]

data Action = Rewind | Skip | Set | Print | None

--------------------------------------------------------
-- EVALUATION_EXECUTION
--------------------------------------------------------
exec :: State           -- current state
     -> Char            -- input char
     -> (State, Action) -- resulting action and state

exec s@(State mi@(i:mi') v mt@(t:mt')) c =
  case c of
    '>' -> ((State (v:mi) t mt'), None)
    '<' -> ((State mi' i (v:mt)), None)
    '+' -> ((State mi (v + 1) mt), None)
    '-' -> ((State mi (v - 1) mt), None)
    '.' -> (s, Print)
    ',' -> (s, Set)
    '[' -> (s, if v == 0 then Skip else None)
    ']' -> (s, if v /= 0 then Rewind else None)
    _ -> (s, None)

next :: State  -- current state
     -> [Char] -- already read chars
     -> [Char] -- buffered of yet to read chars
     -> IO ()  -- IO action

next s to [] = do
  c <- hGetChar stdin
  next s to [c]
next s to (c:from) = do
  (ns, nt, nf) <- do_action (exec s c) to (c:from)
  next ns nt nf

--------------------------------------------------------
-- ACTIONS
--------------------------------------------------------
do_action :: (State, Action)            -- current state and action
          -> [Char]                     -- already read chars
          -> [Char]                     -- buffered of yet to read chars
          -> IO (State, [Char], [Char]) -- IO action with next state and tape
do_action (s, a) to (f:from) =
  case a of
    Rewind -> let (nt, nf) = do_rewind to (f:from) in return (s, nt, nf)
    Skip -> do
      (nt, nf) <- do_skip (f:to) from
      return (s, nt, nf)
    Set -> do
      ns <- do_set s
      return (ns, (f:to), from)
    Print -> do
      do_print s
      return (s, (f:to), from)
    None -> return (s, (f:to), from)

-- ------------------ REWIND_ACTION ------------------- --
do_rewind :: [Char]           -- to buffer
          -> [Char]           -- from buffer
          -> ([Char], [Char]) -- resulting to and from buffer

do_rewind (x:xs) l =
  case x of
    '[' -> (xs, (x:l))
    ']' -> let (nxs, nl) = do_rewind xs (x:l) in do_rewind nxs nl
    _ -> do_rewind xs (x:l)

-- ------------------ SKIP_ACTION ------------------- --
do_skip :: [Char]              -- to buffer
        -> [Char]              -- from buffer
        -> IO ([Char], [Char]) -- resulting to and from buffer as action

do_skip xs [] = do
  c <- hGetChar stdin
  do_skip xs [c]
do_skip xs (x:l) =
  case x of
    ']' -> return ((x:xs), l)
    '[' -> do
      (nxs, nl) <- do_skip (x:xs) l
      do_skip nxs nl
    _ -> do_skip (x:xs) l


-- ------------------ SET_ACTION ------------------- --
do_set :: State    -- current state
       -> IO State -- next state as action

do_set (State mi v mt) = do
  c <- hGetChar stdin
  return (State mi (ord c) mt)

-- ------------------ PRINT_ACTION ------------------- --
do_print :: State -- current state
         -> IO () -- resulting action

do_print (State mi v mt) = print $ (chr v)

-- ------------------ ENTRY_POINT ------------------- --
main = do
  putStrLn "Brainfuck hs"
  next (State mem 0 mem) [] []
    where mem = 0 : mem
