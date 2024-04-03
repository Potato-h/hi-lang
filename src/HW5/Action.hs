{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HW5.Action
    (HiPermission (..)
    , HIO (..)
    , PermissionException (..)
    )
    where

import           Control.Arrow          ((>>>))
import           Control.Exception      (Exception, IOException, catch, throwIO)
import           Control.Monad          (ap, when)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString        as BS (empty, readFile, writeFile)
import           Data.Functor           (($>), (<&>))
import           Data.Set               (Set, notMember)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8')
import qualified Data.Time              as TM (getCurrentTime)
import           HW5.Base               (HiAction (..), HiMonad (..),
                                         HiValue (..), hiInt, hiList)
import           System.Directory       (createDirectory, doesDirectoryExist,
                                         doesFileExist, getCurrentDirectory,
                                         listDirectory, setCurrentDirectory)
import           System.Random.Stateful (randomRIO)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Ord, Show)

data PermissionException = PermissionRequired HiPermission
    deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving Functor

instance Applicative HIO where
  pure x = HIO $ const $ pure x
  (<*>) = ap

instance Monad HIO where
  (HIO run) >>= f = HIO $ \perms -> do
    a <- run perms
    let (HIO run') = f a
    run' perms

instance MonadIO HIO where
  liftIO x = HIO $ const x

ask :: HIO (Set HiPermission)
ask = HIO return

checkPermission :: HiPermission -> HIO ()
checkPermission perm = do
  perms <- ask
  when (perm `notMember` perms) $ liftIO . throwIO $ PermissionRequired perm

execIO :: HiPermission -> IO HiValue -> HIO HiValue
execIO perm val = do
  checkPermission perm
  liftIO val

instance HiMonad HIO where
  runAction (HiActionRead s)     = do
    checkPermission AllowRead
    isFile <- liftIO $ doesFileExist s
    if isFile
      then do
        content <- liftIO $ catch (BS.readFile s)
                              (\(_ :: IOException) -> return BS.empty)
        return $ case decodeUtf8' content of
          Left _    -> HiValueBytes content
          Right txt -> HiValueString txt
      else do
        isDir <- liftIO $ doesDirectoryExist s
        if isDir
          then do
            entries <- liftIO $ listDirectory s
            return $ hiList $ map (T.pack >>> HiValueString) entries
          else return $ HiValueBytes BS.empty
  runAction (HiActionWrite path bs) = execIO AllowWrite (BS.writeFile path bs $> HiValueNull)
  runAction (HiActionMkDir s)       = execIO AllowWrite (createDirectory s $> HiValueNull)
  runAction (HiActionChDir s)       = execIO AllowRead (setCurrentDirectory s $> HiValueNull)
  runAction HiActionCwd             = execIO AllowRead (getCurrentDirectory <&> (T.pack >>> HiValueString))
  runAction HiActionNow             = execIO AllowTime (TM.getCurrentTime <&> HiValueTime)
  runAction (HiActionRand i j)      = randomRIO (i, j) <&> hiInt
  runAction (HiActionEcho txt)      = execIO AllowWrite (putStrLn (T.unpack txt) $> HiValueNull)
