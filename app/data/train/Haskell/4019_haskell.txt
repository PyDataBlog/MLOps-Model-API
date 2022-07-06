import Cookbook.Essential.IO
import Cookbook.Project.Preprocess.Preprocess
import Cookbook.Recipes.Sanitize
import Cookbook.Ingredients.Tupples.Look
import Cookbook.Essential.Continuous
import Cookbook.Ingredients.Lists.Modify
import Cookbook.Ingredients.Lists.Replace

import System.IO
import System.Environment
import System.Directory

getShortName :: String -> String
getShortName x = (rev (before (rev x) '/'))

main = do
  (a:b:c:_) <- getArgs
  ppFolder <- getHomePath "/.preprocess/"
  files_ <- getDirectoryContents $ ppFolder
  let files = blacklist files_ [".",".."]
  allContents <- mapM filelines (map (ppFolder ++) files)

  input <- filelines b

  let appropriateParams = gPL (head $ lookList (zip (map getShortName files) allContents) a)

  let translated = map ((flip replace) appropriateParams) input
  writeFile c (unlines translated)
  
