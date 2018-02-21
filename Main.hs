import Bitsek

--------------
---- APP -----
--------------

main :: IO b
main = do 
    let initPendingBlock = (Block 0 [] 0 "")
    let initBlockchain = (Blockchain [genesisBlock])
    let initUsers = [fabbe, benne, hogge, dave]
    program (initPendingBlock, initBlockchain, initUsers)

{-

 PRE:
 POST: 
 RETURNS:
 EXAMPLE:
-}
program :: (Block, Blockchain, [User]) -> IO b
program (pb, bc, us) = do 
    menu
    action <- getLine
    case action of 
        "send" -> sendBitsek (pb, bc, us)
        "balance" -> showBalance (pb, bc, us)
        "mine" -> mineBitsek (pb, bc, us)
        "show" -> printTransactions (pb, bc, us)
        "new" -> createUser (pb, bc, us)
        "list" -> printUsers (pb, bc, us)

{-
    Trivial menu output
-}
menu :: IO ()
menu = do 
    putStrLn "--------------------------------" 
    putStrLn "Menu"
    putStrLn "--------------------------------"
    putStrLn "Type what's inside the square brackets."
    putStrLn ""
    putStrLn "1. Send Bitsek [send]" 
    putStrLn "2. Check balance [balance]"
    putStrLn "3. Mine [mine]"
    putStrLn "4. Show verified transactions [show]"
    putStrLn "5. Create a new user [new]"
    putStrLn "6. List all users [list]"
    putStrLn "--------------------------------"    
    putStrLn "What do you want to do?"
    putStrLn "--------------------------------" 

{-

 PRE:
 POST: 
 RETURNS:
 EXAMPLE:
-}
sendBitsek :: (Block, Blockchain, [User]) -> IO b
sendBitsek (pb, bc, us) = do 
    let pbIx = index pb
    let pbTs = transactions pb
    let pbPf = proof pb
    let pbPh = previousHash pb

    putStrLn "Type in sender adress: "
    s <- getLine
    let sender = (getUser s us bc)
    -- What if user not found? how to handle?

    putStrLn "Type in sender password: "
    pw <- getLine

    putStrLn "Type in receiver adress"
    r <- getLine
    let receiver = (getUser r us bc)
    -- What if user not found? how to handle?

    putStrLn "Type in amount to send"
    a <- getLine
    let amount = (read a :: Int)
    -- TO DO: implement try & catch exception handler with Either monad

    let t = (Transaction sender receiver amount)

    if validTransaction bc t pw
        then do
            let pb' = (Block pbIx (pbTs ++ [t]) pbPf pbPh)
            program (pb', bc, us)
        else do
            putStrLn "Transaction denied. Wrong password or insufficient funds."
            putStrLn "Redirecting back to menu."
            program (pb, bc, us)

{-

 PRE:
 POST: 
 RETURNS:
 EXAMPLE:
-}
showBalance :: (Block, Blockchain, [User]) -> IO b
showBalance (pb, bc, us) = do

    putStrLn "Input adress:"
    username <- getLine
    putStrLn ("Account balance of user " ++ username ++ " is:")
    let user = getUser username us bc
    putStrLn ((show $ balance user) ++ " Bitsek")

    putStrLn "Press enter to go back to main menu."
    getChar

    program (pb, bc, us)

{-

 PRE:
 POST: 
 RETURNS:
 EXAMPLE:
-}
mineBitsek :: (Block, Blockchain, [User]) -> IO b
mineBitsek (pb, bc, us) = do

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

    program (pb', bc', us)

{-

 PRE:
 POST: 
 RETURNS:
 EXAMPLE:
-}
printTransactions :: (Block, Blockchain, [User]) -> IO b
printTransactions (pb, bc, us) = do
    putStrLn "--------------------------------" 
    printTransactionsAux (allTransactions bc)

    putStrLn "Press enter to go back to main menu."
    getChar
    program (pb, bc, us)

printTransactionsAux :: [Transaction] -> IO ()
printTransactionsAux [] = putStrLn ""
printTransactionsAux (t:ts) = do
    let s = adress (sender t)
    let r = adress (receiver t)
    let a = show (amount t)
    putStrLn ("From: " ++ s ++ "  |  To: " ++ r ++ "  |  Amount: " ++ a)
    printTransactionsAux ts

{-

 PRE:
 POST: 
 RETURNS:
 EXAMPLE:
-}
printUsers :: (Block, Blockchain, [User]) -> IO b
printUsers (pb, bc, us) = do 
    putStrLn "--------------------------------" 
    printUsersAux (aggUsers us bc)
    
    putStrLn "Press enter to go back to main menu."
    getChar
    program (pb, bc, us)

printUsersAux :: [User] -> IO ()
printUsersAux [] = putStrLn ""
printUsersAux (u:us) = do 
    let ad = adress u
    let bal = show (balance u)
    putStrLn ("Public adress: " ++ ad ++ " | Balance: " ++ bal)
    printUsersAux us

{-

 PRE:
 POST: 
 RETURNS:
 EXAMPLE:
-}
createUser :: (Block, Blockchain, [User]) -> IO b    
createUser (pb, bc, us) = do
    putStrLn "--------------------------------" 
    putStrLn "Choose an adress"
    ad <- getLine

    putStrLn "Choose a password"
    pw <- getLine
    let pk = encryptPassword pw

    let u = (User ad pk 1000)
    putStrLn ""
    putStrLn ("Welcome to the world of Bitsek, " ++ ad ++ "!")
    putStrLn "Your starting balance is 1000 Bitsek"
    putStrLn ""

    let us' = u:us

    putStrLn "Press enter to go back to main menu."
    getChar
    program (pb, bc, us')