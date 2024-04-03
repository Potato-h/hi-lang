module Main (main) where

import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Set                  (Set, fromList)
import           HW5.Action                (HIO (..), HiPermission (..))
import           HW5.Evaluator             (eval)
import           HW5.Parser                (errorBundlePretty, parse)
import           HW5.Pretty                (prettyValue)
import           System.Console.Haskeline  (InputT, defaultSettings,
                                            getInputLine, outputStrLn,
                                            runInputT)

env :: Set HiPermission
env = fromList [AllowRead, AllowTime]

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          case parse input of
            Left e     -> outputStrLn $ errorBundlePretty e
            Right expr -> do
              evaled <- lift $ runHIO (eval expr) env
              case evaled of
                Left e    -> outputStrLn $ show e
                Right val -> outputStrLn $ show $ prettyValue val
          loop
