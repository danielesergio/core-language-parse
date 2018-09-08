module Read(
        readF
    ) where

import System.IO
import Model(Program, Name)

-- main :: IO (Program Name)
-- main = do inp <- readF
--           return (comp (parse parseProg inp)) --here you call parseProg

comp :: [(Program Name, Name)] -> Program Name
comp []       = error "no parse"
comp [(e,[])] = e
comp [(_,a)]  = error ("doesn't use all input"++ a)


readF::FilePath -> IO String
readF path = do inh<- (openFile path) ReadMode
                prog<- readloop inh
                hClose inh
                return prog

readloop inh = do ineof <- hIsEOF inh
                  if ineof
                       then return []
                       else do
                              x  <- hGetLine inh
                              xs <- readloop inh
                              return (x++xs)