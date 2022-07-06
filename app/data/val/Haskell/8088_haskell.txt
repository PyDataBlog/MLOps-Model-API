module Main (main) where
 
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import System.FilePath
import System.Environment(getArgs)
 
main = do
    [url, path] <- getArgs
    webSnap url path

webSnap :: String -> FilePath -> IO ()
webSnap url output = do
  initGUI
  w  <- offscreenWindowNew
  wv <- webViewNew
  set w
    [ containerChild       := wv
    , windowDefaultWidth   := 1024
    , windowDefaultHeight  := 768
    ]
 
  webViewLoadUri wv url
  widgetShowAll w
  wv `on` loadFinished $ \_ -> savePage w output
  mainGUI
 
savePage :: OffscreenWindow -> FilePath -> IO ()
savePage w f = do
  p <- offscreenWindowGetPixbuf w
  case p of
    Nothing -> mainQuit
    Just pixbuf -> do
      pixbufSave pixbuf f "png" []
      mainQuit
