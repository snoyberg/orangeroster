import Controller
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    putStrLn "Loaded"
    withOR $ run 3000
