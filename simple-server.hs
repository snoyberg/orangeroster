import Controller
import Network.Wai.Handler.SimpleServer (run)

main :: IO ()
main = do
    putStrLn "Loaded"
    withOR $ run 3000
