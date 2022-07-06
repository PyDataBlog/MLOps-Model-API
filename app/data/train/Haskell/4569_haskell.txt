{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy as BS
import System.FilePath.Glob (namesMatching)
import Text.Read
import Data.Either (partitionEithers)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Monoid ((<>))
import Data.Foldable (for_)
import Options.Generic

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

-- Generic html stuff

-- | Returns a link to a licence file indexed by an Int
linkI :: H.Html -> Int -> H.Html
linkI caption i = link caption (H.toValue i <> ".html")

-- | Returns a link
link :: H.Html -> H.AttributeValue -> H.Html
link caption ref = H.a H.! A.href ref $ caption

-- | html preambule, with title and body
html :: H.Html -> H.Html -> H.Html
html title body = H.docTypeHtml $ do
  H.head $ do
    H.title title
  H.body body

-- Utils

-- | partition either argument is in the form `XXX.ext` or not, with XXX an int.
partitionInvalidNames :: FilePath -> Either FilePath (FilePath, Int)
partitionInvalidNames s = case readMaybe (takeBaseName s) of
  Nothing -> Left s
  Just i -> Right (s, i)

-- The real work
generatePage :: FilePath -> Int -> ((FilePath, Int), Maybe Int, Maybe Int) -> IO ()
generatePage outDir lastI ((path, currentI), prevI, nextI) = do
  content <- BS.readFile path

  let output = generatePage' lastI (prevI, currentI, nextI) content

  BS.writeFile (outDir </> show currentI <.> "html") $ renderHtml $ output

generatePage' :: Int -> (Maybe Int, Int, Maybe Int) -> BS.ByteString -> H.Html
generatePage' lastI (prevI, currentI, nextI) content = do
  html ("GPL Version " <> H.toHtml currentI) $ do
    link "Home" "index.html"
    maybe mempty (linkI "Previous") prevI
    maybe mempty (linkI "Next") nextI
    linkI "Last" lastI

    H.pre $ H.unsafeLazyByteString content

-- | generate an index linking all pages i.html
generateIndex :: [Int] -> H.Html
generateIndex iS =
  html "Index" $ do
    H.ul $ do
      for_ iS $ \i -> do
        H.li $ do
          link (H.toHtml i) (H.toValue i <> ".html")

data Options = Options
  {
    inputDirectory :: FilePath,
    outputDirectory :: FilePath
  } deriving (Generic, Show, ParseRecord)

main :: IO ()
main = do
  Options{..} <- getRecord "Generate HTML from text files"

  names <- namesMatching (inputDirectory </> "*" <.> "txt")

  -- only keep name looking like 'XXXX.txt', where XXXX is an int
  let (invalid, valid) = partitionEithers (map partitionInvalidNames names)

  putStr "Ignored invalid files: "
  print invalid

  let
    files = sortBy (comparing snd) valid
    idsJust = map (Just . snd) files

    -- previous and next button are generated only if the file exists
    prev_indices = Nothing : idsJust
    next_indices = drop 1 idsJust ++ [Nothing]

    lastI = snd (last files)
    filesWithPreviousAndNext = zip3 files prev_indices next_indices

  createDirectoryIfMissing False outputDirectory

  mapM_ (generatePage outputDirectory lastI) filesWithPreviousAndNext

  -- index
  let index = generateIndex (map snd files)
  BS.writeFile (outputDirectory </> "index.html") (renderHtml index)
