
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
             ("add", addCmd)
            ,("next", nextCmd)
            ,("work", workCmd)
            ,("done", doneCmd)
            ,("stop", stopCmd)
            ,("dismiss", dismissCmd)
            ,("readd", readdCmd)
            ,("status", statusCmd)
            ]

addCmd :: Command
addCmd = do
  putStr "Enter a description of the task: "
  desc <- getLine
  s <- get
  case open s of
    Empty     -> set $ s { open = Zip [] desc [] }
    Zip l x r -> set $ s { open = Zip l x (r `snoc` desc) }


workCmd :: Command
workCmd = do
  s <- get
  case mode s of
    Work r m -> do io $ putStrLn $ "Currently working on '" ++ r ++ "'."
                   io $ putStrLn $ "If you are done with it for now, either 'stop' working or mark it 'done'."

    Closed pass acted -> 
                 case closed s of
                   Empty -> do io $ putStrLn $ "No task currently selected."
                               next -- move to the next task
                               io $ putStrLn $ "You are NOT currently working on this new task. Run 'work' again to work on it, or 'next' to continue searching."
                   Zip _ x _ -> startWork x (Closed pass (acted+1)) -- start work, and bump the acted-on count for this pass.

    Open -> case open s of
              Empty -> do io $ putStrLn $ "No task currently selected."
                          next -- move to the next task
                          io $ putStrLn $ "You are NOT currently working on this new task. Run 'work' again to work on it, or 'next' to continue searching."
              Zip _ x _ -> startWork x Open

    Review -> io $ putStrLn $ "Currently in review mode. Complete the review using 'status', 'readd' and 'dismiss'."
    

-- the user has completely finished working on a task
doneCmd :: Command
doneCmd = do
  s <- get
  case mode s of
    Work r m -> do io $ putStrLn $ "Marked task '" ++ r ++ "' as done.\nThe task has been removed from the list."
                   next
    Review -> io $ putStrLn $ "Currently in review mode. Complete the review using 'status', 'readd' and 'dismiss'."
    Closed pass acted -> handleDone closed (\s x -> s { closed = x })
    Open -> handleDone open (\s x -> s { open = x })
 where handleDone access update = do 
         s <- get
         io $ putStrLn $ "You are not currently working on a task."
         case access s of
           Empty      -> next
           c@(Zip l x r) -> do io $ putStrLn $ "You were considering: '" ++ x ++ "'"
                               resp <- prompt "Do you want to mark it as done? (y/N) "
                               if resp
                                   then case r of
                                          [] -> case l of
                                                  [] -> do update s Empty
                                                           next
                                                  (y:l') -> do update s (Zip l' y r)
                                                               next
                                          (y:r') -> do update s (Zip l y r')
                                                       io $ putStrLn $ "Active entry now '" ++ y ++ "'"
                                    else return ()


-- the user has finished working on a task for now
stopCmd :: Command
stopCmd = do
  s <- get
  case mode s of
    Work r m -> do io $ putStrLn $ "Stopped working on '" ++ r ++ "'.\nThe task has been added to the end of the open list."
                   next
    Review -> io $ putStrLn $ "Currently in review mode. Complete the review using 'status', 'readd' and 'dismiss'."
    Closed pass acted -> handleDone closed (\s x -> s { closed = x })
    Open -> handleDone open (\s x -> s { open = x })
 where handleDone access update = do 
         s <- get
         io $ putStrLn $ "You are not currently working on a task."
         case access s of
           Empty      -> next
           c@(Zip l x r) -> do io $ putStrLn $ "You were considering: '" ++ x ++ "'"
                               resp <- prompt "Do you want to stop working on it? (y/N) "
                               if resp
                                   then case r of
                                          [] -> case l of
                                                  [] -> do update s Empty
                                                           next
                                                  (y:l') -> do update s (Zip l' y r)
                                                               next
                                          (y:r') -> do update s (Zip l y r')
                                                       io $ putStrLn $ "Active entry now '" ++ y ++ "'"
                                    else return ()



nextCmd :: Command
nextCmd = next' True -- from the user


-- internal next
next :: Command
next = next' False -- internal, not from the user

-- the actual implementation of next
next':: Bool -> Command
next' fromUser = do
  s <- get
  case mode s of
    Open -> case open s of
              Empty      -> do set $ s { mode = Closed 0 0 } -- not sure if this one can happen, but hey
                               io $ putStrLn "Open list empty, moving to closed list."
              Zip l x [] -> do let (x':r') = reverse (x:l)
                               set $ s { mode = Closed 0 0, open = Zip [] x' r' }
                               io $ putStrLn "Moving to closed list."
                               io $ putStrLn $ "Active entry now: '" ++ zipHead (closed s) ++ "'"
              Zip l x (r:rs) -> do set $ s { open = Zip (x:l) r rs }
                                   io $ putStrLn $ "Active entry now: '" ++ r ++ "'"
    Closed pass acted -> case (closed s, review s) of
                           (Empty, Zip _ _ _) -> do set $ s { mode = Review }
                                                    io $ putStrLn "Reached end of closed list. Moving to review."
                           (Empty, Empty)     -> do set $ s { mode = Open}
                                                    io $ putStrLn "Reached end of closed list. Moving to open list."
                           (c@(Zip _ _ []), Zip _ _ _) -> do set $ s { mode = Review, closed = flipZip c }
                                                             io $ putStrLn "Reached end of closed list. Moving to review."
                           (c@(Zip _ _ []), Empty) | acted > 0 -> do set $ s { mode = Closed (pass+1) 0, closed = flipZip c }
                                                                     io $ putStrLn $ "Reached end of closed list. Actioned " ++ show acted ++ " items last pass." ++
                                                                       ++ " Returning to start of closed list."
                                                   | pass  > 0 -> do set $ s { mode = Open, closed = flipZip c }
                                                                     io $ putStrLn "Reached end of closed list. Did not action anything, but not first pass. Moving to open list."
                                                                     io $ putStrLn $ "Active entry now: '" ++ zipHead (open s) ++ "'"
                                                   | otherwise -> do set $ s { mode = Closed 0 0, review = closed s, closed = open s, open = Empty }
                                                                     io $ putStrLn $ "Reached end of closed list. First pass, did not action anything. " 
                                                                       ++ "Closed list up for review, open list now closed. Moving to the newly closed list."
                                                                     io $ putStrLn $ "Active entry now: '" ++ zipHead (open s) -- actually closed now
                           (Zip l x (y:r), _) -> do set $ s { closed = Zip (x:l) y r }
                                                    io $ putStrLn $ "Active entry now: '" ++ y ++ "'"
    Review | fromUser  -> io $ putStrLn "'next' not allowed in Review mode. Use 'status', 'readd', 'dismiss' to complete the review first."
           | otherwise -> case review s of
                            Empty         -> endReview
                            Zip _ _ []    -> endReview
                            Zip l x (y:r) -> do set $ s { review = Zip (x:l) y r }
                                                io $ putStrLn $ "Active entry now: '" ++ y ++ "'"
                          where endReview = do set $ s { mode = Closed 0 0 }
                            io $ putStrLn $ "Reached end of review. Moving to closed list."
                            io $ putStrLn $ "Active entry now: '" ++ zipHead (closed s) ++ "'"

    Work r m -> do io $ putStrLn $ "Currently working on '" ++ r ++ "'."
                   io $ putStrLn $ "You must either 'stop' working or mark this task 'done'."

    

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


