us = [("Fabian", 1 :: Int), ("Holger", 2 :: Int), ("Benjamin", 3 :: Int)]

getUser :: String -> [(String, Int)] -> (String, Int)
getUser x [] = ("Not Found", 0)
getUser x (y:ys) 
    | x == fst y = y
    | otherwise = getUser x ys

userBalance :: [(String, Int)] -> IO ()
userBalance users = do 
    putStrLn "which user do you want to check"
    user <- getLine
    let foundUser = (getUser user users)
    putStrLn ((fst foundUser) ++ " " ++ (show (snd foundUser)))


