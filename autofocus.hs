
module Main where

import System.IO
import Data.List

import Control.Monad
import Control.Monad.State

import qualified Data.Map as M


data Zipper a = Empty | Zip [a] a [a]
  deriving (Read, Show)

instance Functor (Zipper a) where
  fmap _ Empty       = Empty
  fmap f (Zip l x r) = Zip (map f l) (f x) (map f r)

type Record = String

-- Open gets only one pass. Closed records the pass number, and the number of items actioned this pass
-- Work stores the active record and the mode we were in previously.
data AFModes = Open | Closed Int Int | Review | Work Record AFModes

data AFState = AFS {
     mode      :: AFModes
    ,closed    :: Zipper Record
    ,open      :: Zipper Record
    ,review    :: Zipper Record
    ,dismissed :: [Record]
}


newtype AF a = AF (StateT AFState IO a)
 deriving (Functor, Monad, MonadIO, MonadState AFState)

type Command = AF ()



io = liftIO
snoc xs x = reverse . (x:) . reverse $ xs

zipHead Empty       = "<none>"
zipHead (Zip _ x _) = x

flipZip Empty = Empty
flipZip (Zip l x r) = let (x':xs') = reverse l ++ [x] ++ r
                      in  Zip [] x' xs'

commands :: M.Map String Command
commands = M.fromList [
             ("add", add)
            ,("next", next)
            ,("work", work)
            ,("done", done)
            ,("stop", stop)
            ,("dismiss", dismiss)
            ,("readd", readd)
            ,("status", status)
            ]

add :: Command
add = do
  putStr "Enter a description of the task: "
  desc <- getLine
  s <- get
  case open s of
    Empty     -> set $ s { open = Zip [] desc [] }
    Zip l x r -> set $ s { open = Zip l x (r `snoc` desc) }


next :: Command
next = do
  s <- get
  case mode s of
    Open -> case open s of
              Empty      -> do set $ s { mode = Closed 0 0 } -- not sure if this one can happen, but hey
                               putStrLn "Open list empty, moving to closed list."
              Zip l x [] -> do let (x':r') = reverse (x:l)
                               set $ s { mode = Closed 0 0, open = Zip [] x' r' }
                               putStrLn "Moving to closed list."
                               putStrLn $ "Active entry now: '" ++ zipHead (closed s) ++ "'"
              Zip l x (r:rs) -> do set $ s { open = Zip (x:l) r rs }
                                   putStrLn $ "Active entry now: '" ++ r ++ "'"
    Closed pass acted -> case (closed s, review s) of
                           (Empty, Zip _ _ _) -> do set $ s { mode = Review }
                                                    putStrLn "Reached end of closed list. Moving to review."
                           (c@(Zip _ _ []), Zip _ _ _) -> do set $ s { mode = Review, closed = flipZip c }
                                                             putStrLn "Reached end of closed list. Moving to review."
                           (c@(Zip _ _ []), Empty) | acted > 0 -> do set $ s { mode = Closed (pass+1) 0, closed = flipZip c }
                                                                     putStrLn $ "Reached end of closed list. Actioned " ++ show acted ++ " items last pass." ++
                                                                       ++ " Returning to start of closed list."
                                                   | pass  > 0 -> do set $ s { mode = Open, closed = flipZip c }
                                                                     putStrLn "Reached end of closed list. Did not action anything, but not first pass. Moving to open list."
                                                   | otherwise -> do set $ s { mode = Closed 0 0, review = closed s, closed = open s, open = Empty }
                                                                     putStrLn $ "Reached end of closed list. First pass, did not action anything. " 
                                                                       ++ "Closed list up for review, open list now closed. Moving to the newly closed list."



-- state machine. if at the end of:
-- open -- go to closed
-- closed
   -- if review nonempty, go to review
   -- else, did I action anything last pass?
      -- if yes, return to closed
      -- if no, was that the first pass?
         -- if yes, rename closed to review and open to closed. move to the new closed.
         -- if no,  move to open
-- review -- go to closed
   -- note that next is illegal in review mode -- review forces the user to deal with everything now


