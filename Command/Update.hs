{-# LANGUAGE KindSignatures #-}
module Command.Update where
import           Control.Concurrent
import           Control.Monad.Trans
import           Control.Monad.Reader
import qualified Data.Text.Lazy as LT
import           Shelly
default (LT.Text)

type Ref = LT.Text
type Tracking = LT.Text

data ConsoleMessage = Good
                    | Bad
                    | Ugly


command1' cmd cmdargs sub subargs = LT.init <$> command1 cmd cmdargs sub subargs
git = command1' "git" []

needsStash = catchany_sh
  (git "diff" ["--no-ext-diff", "--quiet", "--exit-code"] >> return False)
  (const $ return True)

smartStash a = do
  shouldStash <- needsStash
  when shouldStash $ git "stash" [] >> return ()
  result <- a
  when shouldStash $ git "stash" ["pop"] >> return ()
  return a
  
-- puts you on the new head of the most recent branch you were on.
-- if you were not on a branch, returns to the commit you were on.
smartCheckout a = do
  location <- catchany_sh getBranchTip (const getRef)
  result <- a
  git "checkout" [location]
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
