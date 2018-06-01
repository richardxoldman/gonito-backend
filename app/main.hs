import Prelude     (IO)
import Network.Wai.Handler.FastCGI          (run)
import Application (appMain)

main :: IO ()
main = do
  app <- appMain
  run app
