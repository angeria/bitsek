main :: String -> IO String
main a = do
    str <- getLine
    putStrLn (str ++ a)