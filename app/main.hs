import Network.Wai.Handler.CGI (run)
import Prelude     (IO)
import Application (appMain)

main :: IO ()
main = do
  app <- appMain
  run app
