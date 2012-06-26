{-# LANGUAGE OverloadedStrings #-}
module Command.Update where
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Reader
import           System.Concert
import           System.Concert.Filesystem
import qualified Data.Text as T

git = command1 "git"

isDirty = didExitFail $ git "diff" ["--no-ext-diff", "--quiet", "--exit-code"]

bracketWhen :: Monad m => m Bool -> m a -> m b -> m c -> m c
bracketWhen mb mbefore mafter maction = do
  b <- mb
  when b $ mbefore >> return ()
  a <- maction
  when b $ mafter >> return ()
  return a

smartStash = bracketWhen isDirty (awaitExit $ git "stash" []) (awaitExit $ git "stash" ["pop"])

-- puts you on the new head of the most recent branch you were on.
-- if you were not on a branch, returns to the commit you were on.
smartCheckout action = do
  symP <- getBranchTip
  hasSymbolicName <- wasExitSuccess symP
  reference <- if hasSymbolicName
               then (head . asLines) <$> out symP
               else (head . asLines) <$> out getBranchTip  
  result <- action
  awaitExit $ git "checkout" [T.unpack reference]
  return result

getRef = git "rev-parse" ["HEAD"]
getBranchTip = git "symbolic-ref" ["HEAD", "--short"]

getTrackingBranches :: Concert [(T.Text, T.Text)]
getTrackingBranches = do
  raw <- asLines <$> (out $ git "for-each-ref" ["--format=(\"%(refname:short)\", \"%(upstream:short)\")", "refs/heads"])
  return $! filter (not . T.null . snd) $ map (read . T.unpack) $ raw

rebase local remote = git "rebase" [T.unpack remote, T.unpack local]

update repo = {- notify_error $ -} chdir repo $ do
  -- notify $ LT.append "Updating " $ toTextIgnore repo
  git "fetch" []
  updatable <- getTrackingBranches
  smartCheckout $ smartStash $ mapM_ (uncurry rebase) updatable
