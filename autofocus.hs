
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
data AFModes = Open | Closed Int Int | Review

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
    Closed pass acted -> case closed s of
                           Empty -> do set $ s { mode = Open }






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


