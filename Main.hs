module Main where
import Control.Concurrent
import Control.Monad
import qualified Data.Text.Lazy as LT
import GHC.Conc
import Shelly
import Shelly.Background
default (LT.Text)

git = command1 "git" []

branchInfo branch field = git "config" $ [LT.concat ["branch.", branch, ".", field]]

upstream branch = do
  status <- git "status" []
  let uplines = map LT.pack . grep ("# Your branch" :: String) $ lines $ LT.unpack status
  case uplines of
    [] -> return Nothing
    (x:_) -> (Just . LT.init) <$> (return (head uplines) -|- cmd "cut" "-d" "'" "-f" "2")

isTracking branch = (not . any LT.null) <$> mapM (branchInfo branch) ["remote", "merge"]

withStash a = do
  stash <- git "stash" []
  let didStash = null $ grep ("No local changes" :: String) $ lines $ LT.unpack stash
  result <- a
  when didStash $ git "stash" ["pop", "-q"] >> return ()
  return a
  
gitRepos :: [Shelly.FilePath] -> ShIO [Shelly.FilePath]
gitRepos = filterM (test_e . (</> ".git"))

tellFinished chan repo = liftIO $ writeChan chan $ LT.append "Finished updating " $ toTextIgnore repo

update chan gitRepo = chdir gitRepo $ do
  liftIO $ writeChan chan $ LT.append "Updating " $ toTextIgnore gitRepo
  git "fetch" []
  branch <- LT.init <$> git "describe" ["--contains", "--all", "HEAD"]
  notTracking <- isTracking branch
  unlessM (isTracking branch) $ do
    errorExit $ LT.concat ["\"", branch, "\" is not a tracking branch"]
  remote <- upstream branch
  case remote of
    Nothing -> tellFinished chan gitRepo
    Just r -> withStash (git "rebase" ["-p", r]) >> tellFinished chan gitRepo

main = do
  processorCount <- getNumProcessors
  setNumCapabilities processorCount
  outputChan <- newChan
  shelly $ print_stdout False $ print_commands False $ do
  dirs <- ls "."
  repos <- gitRepos dirs
  jobs processorCount $ \manager -> do
    promises <- mapM (background manager . update outputChan) repos
    verbosely $ replicateM_ (2 * length repos) $ (liftIO $ readChan outputChan) >>= echo
    
    