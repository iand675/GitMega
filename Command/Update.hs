module Command.Update where
import           Control.Concurrent
import           Control.Monad.Trans
import           Control.Monad.Reader
import           System.Concert
import qualified Data.Text as T

isDirty = didExitFail $ git "diff" ["--no-ext-diff", "--quiet", "--exit-code"]

bracketWhen :: Monad m => m Bool -> m a -> m b -> m c -> m c
bracketWhen mb mbefore mafter maction = do
  b <- mb
  when b $ mbefore >> return ()
  a <- ma
  when b $ mafter >> return ()
  return a

smartStash = bracketWhen isDirty (awaitExit $ git "stash" []) (awaitExit $ git "stash" ["pop"])

-- puts you on the new head of the most recent branch you were on.
-- if you were not on a branch, returns to the commit you were on.
smartCheckout a = do
  symP <- getBranchTip
  hasSymbolicName <- wasExitSuccess symP
  reference <- if hasSymbolicName
               then (head . asLines) <$> out symP
               else (head . asLines) <$> out getBranchTip  
  result <- action
  awaitExit $ git "checkout" [location]
  return a

getRef = git "rev-parse" ["HEAD"]
getBranchTip = git "symbolic-ref" ["HEAD", "--short"]

getTrackingBranches :: ShIO [(Ref, Tracking)]
getTrackingBranches = do
  raw <- git "for-each-ref" ["--format=(\"%(refname:short)\", \"%(upstream:short)\")", "refs/heads"]
  return $! filter (not . LT.null . snd) $ map read $ lines $ LT.unpack raw

rebase local remote = git "rebase" [remote, local]

update repo = {- notify_error $ -} chdir repo $ do
  -- notify $ LT.append "Updating " $ toTextIgnore repo
  git "fetch" []
  updatable <- getTrackingBranches
  smartCheckout $ smartStash $ mapM_ (uncurry rebase) updatable
