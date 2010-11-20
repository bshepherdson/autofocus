{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Handler.Handlers where

import Autofocus

import Data.Maybe
import Data.Time

import Control.Monad

import Yesod.Form

-- helper functions

-- given an item id and a user id, get the item and make sure it belongs to the user.
-- return notFound if it doesn't exist, and permissionDenied if it doesn't belong to this user
getItem :: Maybe ItemId -> UserId -> Handler (ItemId, Item)
getItem miid uid = do
  when (isNothing miid) $ do
    setMessage $ string "You do not have any tasks. Create at least one."
    redirect RedirectTemporary NewR
  let iid = fromJust miid
  mitem <- runDB $ get iid
  when (isNothing mitem) $ do
    findNext uid timeZero False 0
    redirect RedirectTemporary ViewR
  let item = fromJust mitem
  when (itemOwner item /= uid) $ permissionDenied "You are not the owner of this item."
  return (iid,item)


-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Autofocus.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        setTitle "Autofocus homepage"
        addWidget $(hamletFile "homepage")


getViewR :: Handler RepHtml
getViewR = do
  (uid,u) <- requireAuth
  (iid,item) <- getItem (userItem u) uid
  displayItem uid u iid item


displayItem :: UserId -> User -> ItemId -> Item -> Handler RepHtml
displayItem uid u iid item = do
  let region = if itemBacklog item then "Backlog: " else "Active: "
      iidstr = show . fromPersistKey $ iid
  defaultLayout $ do
    setTitle $ string $ region ++ (itemTitle item)
    addJulius $(juliusFile "item")
    addWidget $(hamletFile "item")



postNextR :: Handler ()
postNextR = do
  (uid,u) <- requireAuth
  (iid,item) <- getItem (userItem u) uid
  findNext uid (itemTimestamp item) (itemBacklog item) 0
  redirect RedirectTemporary ViewR


timeZero :: UTCTime
timeZero = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)


-- lots and lots of cases here
-- * if not at the end of a list, move on to the next one
-- * if at the end of active, move to backlog
--   * if backlog is empty, active becomes backlog and we start at the top of it
-- * if at the end of backlog
--   * move to beginning of backlog if we made at least one change (and reset the change var)
--   * move to beginning of active if we didn't make any changes
--     * if we didn't make any changes AND this was our first pass through backlog, bubble everything in backlog, make active into backlog, then go to the first non-bubbled item
-- Bool parameter holds whether we are currently on the backlog
findNext :: UserId -> UTCTime -> Bool -> Int -> Handler ()
findNext uid timestamp backlog swaps | swaps >= 2 = do
  setMessage (string "You have no tasks. Create a task to continue using Autofocus.")
  redirect RedirectTemporary NewR

findNext uid timestamp True swaps = do
    next <- runDB $ selectList [ItemOwnerEq uid, ItemTimestampGt timestamp, ItemBacklogEq True] [ItemTimestampAsc] 1 0
    case next of
      []           -> do
        mu <- runDB $ get uid
        case (swaps > 0, fmap userChangeBacklog mu, fmap userFreshBacklog mu) of
          (True, _, _)               -> do 
            liftIO $ putStrLn $ "Case 0"
            runDB $ updateWhere [ItemBacklogEq False] [ItemBacklog True]            -- move all active to backlog
            findNext uid timeZero True 0                                            -- and start over at the top of the backlog
          (_,Nothing, Nothing)       -> liftIO (putStrLn "Case 1") >> redirect RedirectTemporary RootR            -- something went wrong, user doesn't exist.
          (_,Just True, _)           -> liftIO (putStrLn "Case 2") >> findNext uid timeZero True 0                -- changed, start the backlog again
          (_,Just False, Just False) -> liftIO (putStrLn "Case 3") >> findNext uid timeZero False (swaps+1)       -- no change, but not fresh. move over to active
          (_,Just False, Just True)  -> do -- tricky case. need to bubble all backlog items, make active into backlog
            liftIO $ putStrLn "Case 4"
            runDB $ do
              updateWhere [ItemBacklogEq True] [ItemBubble True]   -- bubble all backlog entries
              updateWhere [ItemBacklogEq False] [ItemBacklog True] -- active becomes backlog, active is empty
              update uid [UserFreshBacklog False]
            findNext uid timestamp True 0

      [(iid, item)] -> do
        liftIO $ putStrLn "Case 5"
        runDB $ if swaps > 0 
                  then update uid [UserOnBacklog True, UserItem (Just iid), UserFreshBacklog True, UserChangeBacklog False] -- coming from Active
                  else update uid [UserOnBacklog True, UserItem (Just iid)]                                                 -- inside Backlog

findNext uid timestamp False swaps = do
    next <- runDB $ selectList [ItemOwnerEq uid, ItemTimestampGt timestamp, ItemBacklogEq False] [ItemTimestampAsc] 1 0
    case next of
      []           -> liftIO (putStrLn "Case 6") >> findNext uid timeZero True (swaps+1)
      [(iid,item)] -> do
        liftIO (putStrLn "Case 7")
        runDB $ update uid [UserOnBacklog False, UserItem (Just iid), UserFreshBacklog True, UserChangeBacklog False]



postDeleteR :: Handler ()
postDeleteR = do
  (uid,u) <- requireAuth
  (iid,item) <- getItem (userItem u) uid
  findNext uid (itemTimestamp item) (userOnBacklog u) 0
  runDB $ do
    delete iid
    when (itemBacklog item) $ update uid [UserChangeBacklog True]
  redirect RedirectTemporary ViewR


postReEnterR :: Handler ()
postReEnterR = do
  (uid,u) <- requireAuth
  (iid,item) <- getItem (userItem u) uid
  mtitle <- lookupPostParam "title"
  let title = maybe (itemTitle item) id mtitle
  now <- liftIO $ getCurrentTime
  iid' <- runDB $ do 
    iid' <- insert $ Item title uid now False False
    delete iid -- delete the original
    when (userOnBacklog u) $ update uid [UserChangeBacklog True]
    return iid'
  findNext uid (itemTimestamp item) (userOnBacklog u) 0
  redirect RedirectTemporary ViewR



getNewR :: Handler RepHtml
getNewR = do
  (uid,u) <- requireAuth
  defaultLayout $ do
    setTitle "New Task"
    addWidget [$hamlet|
%h1 New Task
%form!method=POST!action=@NewR@
    %input!type=text!size=30!name=title
    %input!type=submit!value=Add
|]


postNewR :: Handler RepHtml
postNewR = do
  (uid,u) <- requireAuth
  title <- runFormPost' $ stringInput "title"
  when (null title) $ redirect RedirectTemporary NewR
  now <- liftIO getCurrentTime
  runDB $ do
    iid <- insert $ Item title uid now False False
    when (isNothing $ userItem u) $ update uid [UserItem (Just iid)]
  redirect RedirectTemporary ViewR


