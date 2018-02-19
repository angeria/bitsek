main (pb, bc) = do 
    menu
    action <- getLine
    case action of 
        "sendBitsek" -> sendBitsek (pb, bc)

menu = do 
    putStrLn "Menu"
    putStrLn "Send bitsek, enter: sendBitsek " 
    putStrLn "Show balance, enter: showBalance"
    putStrLn "What do you want to do?"

balance (pb, bc) = do
    putStrLn "What user balance do you want to snoop?"
    adress <- getLine

checkBalance :: Blockchain -> String -> Int
checkBalance bc adress
    |adress (head (aggUsers bc)) = adress

sendBitsek (pb, bc) = do 
    putStrLn "To who from who and how much"

    putStrLn "Enter sender adress" 
    sender <- getLine

    putStrLn "Enter sender password" 
    password <- getLine

    putStrLn "Enter receiver adress" 
    receiver <- getLine 

    putStrLn "Enter amount"
    am <- getLine
    let amount = (read am :: Int)

    if (validTransaction bc (Transaction sender receiver amount) password)
        then do
            let pb' = (newBlock bc (Transaction sender receiver amount))
            putStrLn "Hooray! Your transactions was cleared."
            return (main (pb', bc))
    else do
        putStrLn "Bam, worng password bro."
        return (pb, bc)

