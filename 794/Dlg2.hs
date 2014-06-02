import Graphics.UI.Gtk
import Control.Monad
import Control.Concurrent (forkIO)

startMainLoop :: IO ()
startMainLoop = forkIO_ $ do
  unsafeInitGUIForThreadedRTS
  mainGUI

forkIO_ :: IO () -> IO ()
forkIO_ f = forkIO f >> return ()

ask :: IO Bool
ask = do
  dlg <- messageDialogNew Nothing [] MessageWarning ButtonsYesNo "yes or no"
  rspns <- dialogRun dlg
  widgetDestroy dlg
  case rspns of
    ResponseYes -> do
      putStrLn "yes"
      return True
    _ -> do
      putStrLn "no"
      return False

echo :: IO ()
echo = do
  putStrLn "echo"
  b <- postGUISync ask
  when b echo

main = do
  startMainLoop
  echo
  postGUISync mainQuit
