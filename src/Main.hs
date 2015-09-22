
import qualified Bachelor.SeqParse as Parse
import System.Environment ( getArgs )

main :: IO()
main = do
    args <- getArgs
    if (length args /= 1)
        then putStrLn "please provide a directory"
        else
            Parse.run $ head args

