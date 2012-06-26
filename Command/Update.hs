{-# LANGUAGE OverloadedStrings #-}
module Command.Update where
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Reader
import           System.Concert
import           System.Concert.Filesystem
import qualified Data.Text as T

git = command1 "git"

isDirty = git "diff" ["--no-ext-diff", "--quiet", "--exit-code"] exitError

bracketWhen :: Monad m => m Bool -> m a -> m b -> m c -> m c
bracketWhen mb mbefore mafter maction = do
  b <- mb
  when b $ mbefore >> return ()
  a <- maction
  when b $ mafter >> return ()
  return a

smartStash = bracketWhen isDirty (git "stash" [] exec) (git "stash" ["pop"] exec)

getRef = asSingleLine <$> git "rev-parse" ["HEAD"] out

getBranchTip = git "symbolic-ref" ["HEAD", "--short"] $ \p -> do
  b <- exitOK p
  if b then out p >>= (return . Just . asSingleLine) else return Nothing

getTrackingBranches :: Concert [(T.Text, T.Text)]
getTrackingBranches = do
  raw <- asLines <$> git "for-each-ref" ["--format=(\"%(refname:short)\", \"%(upstream:short)\")", "refs/heads"] out
  return $! filter (not . T.null . snd) $ map (read . T.unpack) $ raw

-- puts you on the new head of the most recent branch you were on.
-- if you were not on a branch, returns to the commit you were on.
smartCheckout action = do
  msym <- getBranchTip
  location <- case msym of
    Nothing -> getRef
    Just loc -> return loc
  result <- action
  git "checkout" [T.unpack location] exec
  return result

rebase local remote = do
  liftIO $ putStrLn $ "rebasing " ++ T.unpack remote ++ " " ++ T.unpack local
  git "rebase" [T.unpack remote, T.unpack local] exec

update repo = chdir repo $ do
  liftIO $ putStrLn "fetching"
  git "fetch" [] exec
  updatable <- getTrackingBranches
  smartCheckout $ smartStash $ mapM_ (uncurry rebase) updatable
