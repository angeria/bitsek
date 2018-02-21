import Bitsek

--------------
---- APP -----
--------------

main :: IO b
main = do 
    let initPendingBlock = (Block 0 [] 0 "")
    let initBlockchain = (Blockchain [genesisBlock])
    program (initPendingBlock, initBlockchain)

program :: (Block, Blockchain) -> IO b
program (pb, bc) = do 
    menu
    action <- getLine
    case action of 
        "sendBitsek" -> sendBitsek (pb, bc)
        "showBalance" -> showBalance (pb, bc)
        "mineBitsek" -> mineBitsek (pb, bc)
        "showVerifiedTransactions" -> printTransactions (pb, bc)
        "showUsers" -> printUsers (pb, bc)

menu :: IO ()
menu = do 
    putStrLn "--------------------------------" 
    putStrLn "Menu"
    putStrLn "--------------------------------"
    putStrLn "1. sendBitsek" 
    putStrLn "2. showBalance"
    putStrLn "3. mineBitsek"
    putStrLn "4. showVerifiedTransactions"
    putStrLn "5. showUsers"
    putStrLn "--------------------------------"    
    putStrLn "What do you want to do?"
    putStrLn "--------------------------------" 

sendBitsek :: (Block, Blockchain) -> IO b
sendBitsek (pb, bc) = do 
    let pbIx = index pb
    let pbTs = transactions pb
    let pbPf = proof pb
    let pbPh = previousHash pb

    putStrLn "Type in sender adress: "
    s <- getLine
    let sender = (getUser s bc)
    -- What if user not found? how to handle?

    putStrLn "Type in sender password: "
    pw <- getLine

    putStrLn "Type in receiver adress"
    r <- getLine
    let receiver = (getUser r bc)
    -- What if user not found? how to handle?

    putStrLn "Type in amount to send"
    a <- getLine
    let amount = (read a :: Int)
    -- TO DO: implement try & catch exception handler with Either monad

    let t = (Transaction sender receiver amount)

    if validTransaction bc t pw
        then do
            let pb' = (Block pbIx (pbTs ++ [t]) pbPf pbPh)
            program (pb', bc)
        else do
            putStrLn "Transaction denied. Wrong password or insufficient funds."
            putStrLn "Redirecting back to menu."
            program (pb, bc)

showBalance :: (Block, Blockchain) -> IO b
showBalance (pb, bc) = do

    putStrLn "Input adress:"
    username <- getLine
    putStrLn ("Account balance of user " ++ username ++ " is:")
    let user = getUser username bc
    putStrLn ((show $ balance user) ++ " Bitsek")

    putStrLn "Press enter to go back to main menu."
    getChar

    program (pb, bc)

mineBitsek :: (Block, Blockchain) -> IO b
mineBitsek (pb, bc) = do

    let bc' = addToBlockchain bc (newBlockIO bc pb)
    let pb' = (Block 0 [] 0 "")

    putStrLn "--------------------------------" 
    putStrLn "Result:" 
    print (addToBlockchain bc (newBlockIO bc pb))
    putStrLn "--------------------------------" 
    putStrLn "You have successfully mined!"
    putStrLn "The block with your transactions were inserted into the blockchain."
    putStrLn "--------------------------------"
    putStrLn "Press enter to go back to main menu."
    getChar

    program (pb', bc')

printTransactions :: (Block, Blockchain) -> IO b
printTransactions (pb, bc) = do
    putStrLn "--------------------------------" 
    printTransactionsAux (allTransactions bc)

    putStrLn "Press enter to go back to main menu."
    getChar
    program (pb, bc)

printTransactionsAux :: [Transaction] -> IO ()
printTransactionsAux [] = putStrLn ""
printTransactionsAux (t:ts) = do
    let s = adress (sender t)
    let r = adress (receiver t)
    let a = show (amount t)
    putStrLn ("From: " ++ s ++ "  |  To: " ++ r ++ "  |  Amount: " ++ a)
    printTransactionsAux ts

printUsers :: (Block, Blockchain) -> IO b
printUsers (pb, bc) = do 
    putStrLn "--------------------------------" 
    printUsersAux (allUsers bc)
    
    putStrLn "Press enter to go back to main menu."
    getChar
    program (pb, bc)

printUsersAux :: [User] -> IO ()
printUsersAux [] = putStrLn ""
printUsersAux (u:us) = do 
    let ad = adress u
    let bal = show (balance u)
    putStrLn ("Public adress: " ++ ad ++ " | Balance: " ++ bal)
    printUsersAux us