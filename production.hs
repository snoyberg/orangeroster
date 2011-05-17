import Controller (withOR)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withOR $ run 3000
