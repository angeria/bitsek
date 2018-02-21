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

menu :: IO ()
menu = do 
    putStrLn "--------------------------------" 
    putStrLn "Menu"
    putStrLn "--------------------------------"
    putStrLn "1. sendBitsek" 
    putStrLn "2. showBalance"
    putStrLn "3. mineBitsek"
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

    putStrLn "Type in sender password: "
    pw <- getLine

    putStrLn "Type in receiver adress"
    r <- getLine
    let receiver = (getUser r bc)

    putStrLn "Type in amount to send"
    a <- getLine
    let amount = (read a :: Int)
    -- TO DO: implement try & catch exception handler with Either monad

    let t = (Transaction sender receiver amount)

    -- TO DO: implement validTransaction with pk
    let pb' = Block pbIx (pbTs ++ [t]) pbPf pbPh

    putStrLn "Press enter to go back to main menu."
    getChar

    program (pb', bc)

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

    let newBlockchain = addToBlockchain bc (newBlockIO bc pb)
    let initPendingBlock = (Block 0 [] 0 "")

    putStrLn "--------------------------------" 
    putStrLn "Result:" 
    print (addToBlockchain bc (newBlockIO bc pb))
    putStrLn "--------------------------------" 
    putStrLn "You have successfully mined!"
    putStrLn "The block with your transactions were inserted into the blockchain."
    putStrLn "--------------------------------"
    putStrLn "Press enter to go back to main menu."
    getChar

    program (initPendingBlock, newBlockchain)
