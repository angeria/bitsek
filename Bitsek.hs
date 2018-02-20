---------------------------------------------------------------------
--                 Haskell Project: Bitsek                         --
-- Authors: Benjamin Angeria, Fabian Haglund and Holger Swartling. --
---------------------------------------------------------------------

-------------
-- IMPORTS --
-------------

-- Cryptographic hash functions.
import Crypto.Hash

-- Functions to convert to and from bytestrings which can be hashed.
import Data.ByteString.Conversion

----------------
-- DATA TYPES --
----------------

{-  User Adress PrivateKey Balance
    - Adress: A public adress that money can be sent to.
    - PrivateKey: The hash of a secret password needed to send a transaction from the users wallet.
    - Balance: The user's total funds.
-}
data User = User { adress :: String
                 , privateKey :: String 
                 , balance :: Int 
                 } deriving (Show)

data Transaction = Transaction { sender :: User
                               , receiver :: User
                               , amount :: Int
                               } deriving (Show)

{-  Block
    A block with the following information:
        Index: The index of the block, where the first block in the blockchain (the "genesis block") has index 0.
        Transactions: A list of all the transactions in that block.
        Proof: The nonce (an arbitrary Int), that when hashed with the previous block gives a string that matches a predefined condition.
        PreviousHash: The hash of the block before the current block in the blockchain.
-}
data Block = Block { index :: Int 
                   , transactions :: [Transaction]
                   , proof :: Int
                   , previousHash :: String
                   } deriving (Show)

{-  Blockchain
    Represents a list of blocks.
    INVARIANT: The latest block has to be the head of the list of blocks.

-}
data Blockchain = Blockchain [Block] deriving (Show)

-----------------------
-- TESTING VARIABLES --
-----------------------

-- password: singularity
fabbe = User "fabbe" "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da" 1000
-- password: entropy
benne = User "benne" "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992" 1000
-- password: anka 
hogge = User "hogge" "01d9db6e08b2426c3c56122aca300c143a60157de6899d6bc84614c40d86bb66" 1000
-- password: monadsforbreakfast
dave = User "dave" "4c7be2f6d37d20fe95050f329b58bcb3b552c3544260b14319964314b38ad416" 1000

initialTransaction1 = Transaction fabbe benne 0
initialTransaction2 = Transaction hogge dave 0

initialTransactions = [initialTransaction1, initialTransaction2]

genesisBlock = Block {index = 0, transactions = initialTransactions, proof = 0, previousHash = (show $ hashWith SHA256 $ toByteString' "plants are institutions")}
genesisBlockchain = Blockchain [genesisBlock]

----------------
-- BLOCKCHAIN --
----------------

addToBlockchain :: Blockchain -> Block -> Blockchain
addToBlockchain (Blockchain blocks) newBlock = Blockchain (newBlock:blocks)

newBlock :: Blockchain -> Transaction -> Block
newBlock blockchain newTransaction = Block newIndex [newTransaction] proof previousHash
  where
    newIndex = 1 + (index $ lastBlock blockchain)
    proof = snd $ mineBlock (lastBlock blockchain)
    previousHash = fst $ mineBlock (lastBlock blockchain)

appendBlock :: Block -> Blockchain -> Blockchain
appendBlock x (Blockchain xs) = (Blockchain (x:xs))

{-  encryptPassword password
    Takes a password and encrypts it.
    RETURNS: A hashed string of password.
    EXAMPLE: encryptPassword "test" = "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"
-}
encryptPassword :: String -> String
encryptPassword password = show $ hashWith SHA256 $ toByteString' password

validPassword :: User -> String -> Bool
validPassword (User _ privateKey _) password
    | (encryptPassword password) == privateKey = True
    | otherwise = False 

{-  lastBlock blockchain 
    Takes a blockchain and returns the last block in it.
    PRE: blockchain must be non-empty.
-}
lastBlock :: Blockchain -> Block
lastBlock blockchain = case blockchain of Blockchain (x:xs) -> x

validBlockchain :: Blockchain -> Bool
validBlockchain (Blockchain blocks) = validBlockchainAux reversedBlockchain 
    where 
        reversedBlockchain = reverse blocks

validBlockchainAux :: [Block] -> Bool
validBlockchainAux [] = True
validBlockchainAux [x] = True
validBlockchainAux (x:xs)
   | hashBlock x (proof (head xs)) == (previousHash (head xs)) = validBlockchainAux xs
   | otherwise = False

----------------------------------
-- BLOCKCHAIN UTILITY FUNCTIONS --
----------------------------------

aggUsers :: Blockchain -> [User]
aggUsers b = aggUsersAux (allUsers b) (allTransactions b)

aggUsersAux :: [User] -> [Transaction] -> [User]
aggUsersAux [] _ = []
aggUsersAux (u:us) ts = aggUser u ts : aggUsersAux us ts

userBalance :: User -> Blockchain -> Int
userBalance user blockchain = balance (aggUser user (allTransactions blockchain))

checkBalance :: String -> Blockchain -> Int
checkBalance adress bc = userBalance (getUser adress bc) bc

aggUser :: User -> [Transaction] -> User
aggUser u [] = u
aggUser u (t:ts) = aggUser (updateUser u t) ts

updateUser :: User -> Transaction -> User 
updateUser (User ad pkey balance) (Transaction sender receiver amount)
    | ad == (adress sender) = (User ad pkey (balance-amount))
    | ad == (adress receiver) = (User ad pkey (balance+amount))
    | otherwise = (User ad pkey balance)

allTransactions :: Blockchain -> [Transaction]
allTransactions (Blockchain blocks) = allTransactionsAux blocks

allTransactionsAux :: [Block] -> [Transaction]
allTransactionsAux [] = []
allTransactionsAux (block:blocks) = transactions block ++ allTransactionsAux blocks

allUsers :: Blockchain -> [User]
allUsers b = uniqueUsers ((allSenders (allTransactions b)) ++ (allReceivers (allTransactions b))) []

-- PRE: shall be invoked with ys == []
uniqueUsers :: [User] -> [User] -> [User]
uniqueUsers [] ys = ys
uniqueUsers (x:xs) ys
    | userExist x ys = uniqueUsers xs ys
    | otherwise = uniqueUsers xs (x:ys)

userExist :: User -> [User] -> Bool
userExist u [] = False
userExist u (x:xs)
    | adress u == adress x = True
    | otherwise = userExist u xs

adressExist :: String -> [User] -> Bool
adressExist a [] = False
adressExist a (x:xs)
        | a == adress x = True
        | otherwise = adressExist a xs

allSenders :: [Transaction] -> [User]
allSenders [] = []
allSenders (t:ts) = sender t : allSenders ts

allReceivers :: [Transaction] -> [User]
allReceivers [] = []
allReceivers (t:ts) = receiver t : allReceivers ts

getUser :: String -> Blockchain -> User 
getUser ad b = getUserAux ad (aggUsers b)

getUserAux :: String -> [User] -> User
getUserAux ad [] = (User "User" "Not Found" 0)
getUserAux ad (u:us) 
    | ad == adress u = u
    | otherwise = getUserAux ad us

----------------------------
-- Proof of Work / Mining --
----------------------------

{-  mineBlock block
    Runs a proof of work mechanism on block.
    RETURNS: Hash beginning with three 0's and a proof.
-}
mineBlock :: Block -> (String, Int)
mineBlock block = mineBlockAux block 0

mineBlockAux :: Block -> Int -> (String, Int)
mineBlockAux block nonce
    | head hashResult == '0' 
        && hashResult !! 1 == '0'
        && hashResult !! 2 == '0'
        = (hashResult, nonce)
    | otherwise = mineBlockAux block (nonce + 1)
        where
            hashResult = hashBlock block nonce

{-  hashBlock block nonce
    Takes a block and a nonce and returns the hash of it.
-}
hashBlock :: Block -> Int -> String
hashBlock block nonce = show $ hashWith SHA256 $ toByteString' $ (show nonce ++ previousHash block ++ transactionsToString block)

{-  transactionsToString block
    Takes a block and returns a concatenation of the whole data structure into a string.
    RETURNS: String of all transaction data in block.
-}
transactionsToString :: Block -> String
transactionsToString block = transactionsToStringAux (transactions block)
  where
    transactionsToStringAux :: [Transaction] -> String
    transactionsToStringAux []     = []
    transactionsToStringAux (x:xs) = transactionToString x ++ transactionsToStringAux xs
      where
        transactionToString :: Transaction -> String
        transactionToString (Transaction sender receiver amount) = userToString sender ++ userToString receiver ++ show amount 
          where
            userToString :: User -> String
            userToString (User adress _ balance) = adress ++ show balance


validTransaction :: Blockchain -> Transaction -> String -> Bool
validTransaction blockchain (Transaction sender receiver amount) password= 
    if validPassword sender password
        then if (userBalance sender blockchain) >= amount
            then True
            else False
        else False


--------------
---- APP -----
--------------

main = do 
    let initPendingBlock = (Block 0 [] 0 "")
    let initBlockchain = (Blockchain [genesisBlock])
    program (initPendingBlock, initBlockchain)

program (pb, bc) = do 
    menu
    action <- getLine
    case action of 
        "sendBitsek" -> sendBitsek (pb, bc)
        "showBalance" -> showBalance (pb, bc)
        "mineBlock" -> blockMiner (pb, bc)

menu = do 
    putStrLn "--------------------------------" 
    putStrLn "Menu"
    putStrLn "--------------------------------"
    putStrLn "1. sendBitsek" 
    -- putStrLn "2. showBalance"
    -- putStrLn "3. mineBlock"
    putStrLn "--------------------------------"    
    putStrLn "What do you want to do?"
    putStrLn "--------------------------------" 

blockMiner (pb, bc) = do
    let bc' = (appendBlock (mineBlock pb) bc)
    let pb' = (Block ((index pb)+1) [] 0 "") 
    program (pb', bc')

showBalance (pb, bc) = do
    putStrLn "What adress you wanna snoop?"
    adress <- getLine
    let bal = show $ checkBalance adress bc
    putStrLn ("Your balance is "++bal)
    program (pb, bc)

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
    --program ((Block pbIx, (pbTs ++ t), pbPf, pbPh), bc)
    let pb' = Block pbIx (pbTs ++ [t]) pbPf pbPh

    program (pb', bc)