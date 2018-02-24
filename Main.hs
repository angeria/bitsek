import Bitsek
import Tests

import System.Exit

--------------
---- APP -----
--------------


main :: IO b
main = do 
    let initpb = (Block 0 [] 0 "")
    let initbc = (Blockchain [genesisBlock])
    let initus = [fabbe, benne, hogge, dave]
    program (initpb, initbc, initus)


{-  program block blockchain users 

   Runs the show. Prints a menu, takes an action and delegates the action to the adequate subfunction.

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
        "q" -> exitWith ExitSuccess
        _ -> do 
            putStrLn "Invalid command. Press enter to try again."
            getChar
            program (pb, bc, us)

{- menu 

    Trivial menu output (exists to keep program more readable)

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
    putStrLn "7. Quit [q]"
    putStrLn "--------------------------------"    
    putStrLn "What do you want to do?"
    putStrLn "--------------------------------" 

{- sendBitsek block blockchain users

    Let's the user request a transaction. Retrieves, validates and conditionally stores 
    transactional information in state.

-}
sendBitsek :: (Block, Blockchain, [User]) -> IO b
sendBitsek (pb, bc, us) = do 
    let pbIx = index pb
    let pbTs = transactions pb
    let pbPf = proof pb
    let pbPh = previousHash pb

    s <- userCommunicate "Type in sender adress: "
    let sender = (getUser s us bc)
    -- What if user not found? how to handle?

    pw <- userCommunicate "Type in sender password: "

    r <- userCommunicate "Type in receiver adress"
    let receiver = (getUser r us bc)
    -- What if user not found? how to handle?
    
    a <- userCommunicate "Type in amount to send: "
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

{- showBalance block blockchain users

    Asks the user for an user adress and shows the corresponding user balance.

-}
showBalance :: (Block, Blockchain, [User]) -> IO b
showBalance (pb, bc, us) = do

    --putStrLn "Input adress:"
    --username <- getLine
    username <- userCommunicate "Input adress: "

    putStrLn ("Account balance of user " ++ username ++ " is:")
    let user = getUser username us bc
    putStrLn ((show $ balance user) ++ " Bitsek")

    putStrLn "Press enter to go back to main menu."
    getChar

    program (pb, bc, us)

{- mineBitsek block blockchain users
    Mines the pending block and adds it to the blockchain.
    SIDE EFFECTS: Modifies state variables pending block and blockchain
    RETURNS: 
-}
mineBitsek :: (Block, Blockchain, [User]) -> IO b
mineBitsek (pb, bc, us) = do

    let bc' = addToBlockchain bc (newBlock bc pb)
    let pb' = (Block 0 [] 0 "")

    putStrLn "--------------------------------" 
    putStrLn "Result:" 
    print (addToBlockchain bc (newBlock bc pb))
    putStrLn "--------------------------------" 
    putStrLn "You have successfully mined!"
    putStrLn "The block with your transactions were inserted into the blockchain."
    putStrLn "--------------------------------"
    putStrLn "Press enter to go back to main menu."
    getChar

    program (pb', bc', us)

{- printTransactions block blockchain users

    Prepares transactional data for printTransactionsAux whom in return print the transactions
    SIDE EFFECTS: Prints stuff to the console

-}
printTransactions :: (Block, Blockchain, [User]) -> IO b
printTransactions (pb, bc, us) = do
    putStrLn "--------------------------------" 
    printTransactionsAux (allTransactions bc)

    putStrLn "Press enter to go back to main menu."
    getChar
    program (pb, bc, us)

{- printTransactionsAux transactions

    Handles the actual printing of transactions from a list given by printTransactions.
    SIDE EFFECTS: Prints stuff to the console

-}
printTransactionsAux :: [Transaction] -> IO ()
printTransactionsAux [] = putStrLn ""
printTransactionsAux (t:ts) = do
    let s = adress (sender t)
    let r = adress (receiver t)
    let a = show (amount t)
    putStrLn ("From: " ++ s ++ "  |  To: " ++ r ++ "  |  Amount: " ++ a)
    printTransactionsAux ts

{- 
-}
printUsers :: (Block, Blockchain, [User]) -> IO b
printUsers (pb, bc, us) = do 
    putStrLn "--------------------------------" 
    printUsersAux (aggUsers us bc)
    
    putStrLn "Press enter to go back to main menu."
    getChar
    program (pb, bc, us)

{-
-}
printUsersAux :: [User] -> IO ()
printUsersAux [] = putStrLn ""
printUsersAux (u:us) = do 
    let ad = adress u
    let bal = show (balance u)
    putStrLn ("Public adress: " ++ ad ++ " | Balance: " ++ bal)
    printUsersAux us

{- createUser
-}
createUser :: (Block, Blockchain, [User]) -> IO b    
createUser (pb, bc, us) = do
    putStrLn "--------------------------------" 
    ad <- userCommunicate "Choose an adress"
    
    if (adressTaken ad us)
        then do 
            putStrLn "Adress has been taken."
            createUser (pb, bc, us)
        else do
            pw <- userCommunicate "Choose a password"
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
            
-- Experimenting
userCommunicate :: String -> IO String
userCommunicate msg = do
    putStrLn msg
    input <- getLine 
    putStrLn ""
    putStrLn ("Input: " ++ input ++ " - Are you happy with your input? y/n")
    putStrLn ""
    answer <- getLine
    putStrLn ""
    case answer of 
        "y" -> return input
        "n" -> userCommunicate "Ok, try again."
    


