import System.IO
import Data.Char

data State = State
  { mem_in :: [Int]
  , mem_to :: [Int] }

val :: State -> Int
val (State _ (v:m)) = v

data Action = WRewind | WSkip | Set | Print | None

exec :: State -> Char -> (State, Action)
exec s c = let State (i:mi) (t:mt) = s in
             case c of
               '>' -> ((State (t:(i:mi)) mt), None)
               '<' -> ((State mi (i:(t:mt))), None)
               '+' -> ((State (i:mi) ((t + 1):mt)), None)
               '-' -> ((State (i:mi) ((t - 1):mt)), None)
               '.' -> (s, Print)
               ',' -> (s, Set)
               '[' -> (s, if (val s) == 0 then WSkip else None)
               ']' -> (s, if (val s) /= 0 then WRewind else None)
               _ -> (s, None)

next :: State -> [Char] -> [Char] -> IO ()
next s to [] = do
  c <- hGetChar stdin
  next s to [c]
next s to (c:from) = do
  (ns, nt, nf) <- do_action (exec s c) to (c:from)
  next ns nt nf

do_action :: (State, Action) -> [Char] -> [Char] -> IO (State, [Char], [Char])
do_action (s, a) to (f:from) =
  case a of
    WRewind -> let (nt, nf) = do_rewind to (f:from) in return (s, nt, nf)
    WSkip -> do
      (nt, nf) <- do_skip (f:to) from
      return (s, nt, nf)
    Set -> do
      ns <- do_set s
      return (ns, (f:to), from)
    Print -> do
      do_print s
      return (s, (f:to), from)
    None -> return (s, (f:to), from)

do_rewind :: [Char] -> [Char] -> ([Char], [Char])
do_rewind (x:xs) l =
  case x of
    '[' -> (xs, (x:l))
    ']' -> let (nxs, nl) = do_rewind xs (x:l) in do_rewind nxs nl
    _ -> do_rewind xs (x:l)

do_skip :: [Char] -> [Char] -> IO ([Char], [Char])
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


do_set :: State -> IO State
do_set (State mi (t:mt)) = do
  c <- hGetChar stdin
  return (State mi ((ord c):mt))

do_print :: State -> IO ()
do_print (State mi (t:mt)) = print $ show t


main = do
  putStrLn "Brainfuck hs"
  next (State mem mem) [] []
    where mem = 0 : mem
