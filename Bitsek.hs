---------------------------------------------------------------------
--                 Haskell Project: Bitsek                         --
-- Authors: Benjamin Angeria, Fabian Haglund and Holger Swartling. --
---------------------------------------------------------------------

module Bitsek where

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

genesisBlock = Block {index = 0, transactions = [], proof = 0, previousHash = (show $ hashWith SHA256 $ toByteString' "plants are institutions")}
genesisBlockchain = Blockchain [genesisBlock]

testBlock1 = Block {index = 1, transactions = [Transaction {sender = User {adress = "Benne", privateKey = "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992", balance = 100}, receiver = User {adress = "Fabbe", privateKey = "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da", balance = 100}, amount = 100}], proof = 911, previousHash = "000854f0985938bb5d557eadef1bbc8f1d0ab9bf46d58cecfdb774c87f2094c2"}
testBlock2 = Block {index = 2, transactions = [Transaction {sender = User {adress = "Fabbe", privateKey = "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da", balance = 100}, receiver = User {adress = "Benne", privateKey = "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992", balance = 100}, amount = 20}], proof = 2719, previousHash = "00035fee66451dbc750d037bec5c5cb6e7f5e17c6a721e34db2de8be92d9dd1a"}
testBlockchain = Blockchain [testBlock2, testBlock1, genesisBlock]

--------------------------
-- BLOCKCHAIN FUNCTIONS --
--------------------------

{- 	addToBlockchain blockchain block
	Adds a new block to an existing blockchain.
	RETURNS: blockchain with block inserted.
	EXAMPLES: addToBlockchain (Blockchain [genesisBlock]) testBlock1 = Blockchain [testBlock1, genesisBlock]
-}
addToBlockchain :: Blockchain -> Block -> Blockchain
addToBlockchain (Blockchain blocks) newBlock = Blockchain (newBlock:blocks)

{-	validBlockchain blockchain
	Checks that a blockchain is valid by verifying that every block hash meets the proof of work precondition.
	RETURNS: Bool saying if blockchain is valid or not.
-}
validBlockchain :: Blockchain -> Bool
validBlockchain (Blockchain blocks) = validBlockchainAux reversedBlockchain 
    where 
        reversedBlockchain = reverse blocks

{- 	validBlockchainAux blocks
	Checks that every block in a list of blocks meets the proof of work precondition.
	RETURNS: Bool saying if every block blocks is valid, or if at least one is incorrect.
-}
validBlockchainAux :: [Block] -> Bool
-- VARIANT: Length of the list blocks.
validBlockchainAux [] = True
validBlockchainAux [x] = True
validBlockchainAux (x:xs)
   | hashBlock x (proof (head xs)) == (previousHash (head xs)) = validBlockchainAux xs
   | otherwise = False

{-  lastBlock blockchain 
    Takes a blockchain and returns the last block in it.
    PRE: blockchain must be non-empty.
    RETURNS: last block in blockchain.
-}
lastBlock :: Blockchain -> Block
lastBlock blockchain = case blockchain of Blockchain (x:xs) -> x

--------------------
-- USER FUNCTIONS --
--------------------

{-  encryptPassword password
    Takes a password and encrypts it with SHA256.
    RETURNS: A hashed string of password.
    EXAMPLE: encryptPassword "test" = "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"
-}
encryptPassword :: String -> String
encryptPassword password = show $ hashWith SHA256 $ toByteString' password

{-	validPassword user password
	Checks if the password is valid for the given user.
	RETURNS: Bool saying if password is valid for user.
	EXAMPLE: validPassword fabbe "singularity" = True
-}
validPassword :: User -> String -> Bool
validPassword (User _ privateKey _) password
    | (encryptPassword password) == privateKey = True
    | otherwise = False 

{-	userBalance user blockchain
	
	RETURNS: 
	EXAMPLE:
-}
userBalance :: User -> Blockchain -> Int
userBalance user blockchain = balance (aggUser user (allTransactions blockchain))    

{-	userBalance user blockchain
	
	RETURNS: 
	EXAMPLE:
-}
aggUsers :: [User] -> Blockchain -> [User]
aggUsers us b = aggUsersAux us (allTransactions b)

aggUsersAux :: [User] -> [Transaction] -> [User]
aggUsersAux [] _ = []
aggUsersAux (u:us) ts = aggUser u ts : aggUsersAux us ts

{-	userBalance user blockchain
	
	RETURNS: 
	EXAMPLE:
-}
aggUser :: User -> [Transaction] -> User
aggUser u [] = u
aggUser u (t:ts) = aggUser (updateUser u t) ts

updateUser :: User -> Transaction -> User 
updateUser (User ad pkey balance) (Transaction sender receiver amount)
    | ad == (adress sender) = (User ad pkey (balance-amount))
    | ad == (adress receiver) = (User ad pkey (balance+amount))
    | otherwise = (User ad pkey balance)

{- getUser adress blockchain
Returns the user with the given adress, with correct current balance
PRE: 
POST: User has correct balance
-}
getUser :: String -> [User] -> Blockchain -> User 
getUser ad us b = getUserAux ad (aggUsers us b)

getUserAux :: String -> [User] -> User
getUserAux ad [] = (User "User" "Not Found" 0)
getUserAux ad (u:us) 
    | ad == adress u = u
    | otherwise = getUserAux ad us

{- adressNotTaken adress blockchain
Returns the user with the given adress, with correct current balance
PRE: 
POST: User has correct balance
-}
adressTaken :: Adress -> [User] -> Bool 
adressTaken ad [] = False
adressTaken ad (u:us)
        | ad == adress u = True
        | otherwise = adressTaken us

---------------------------
-- TRANSACTION FUNCTIONS --
---------------------------

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

{- 	validTransaction blockchain transaction password
	Checks if the sender has enough funds and that the given password is valid.
	RETURNS: Bool stating if transaction is valid.
-}
validTransaction :: Blockchain -> Transaction -> String -> Bool
validTransaction blockchain (Transaction sender receiver amount) password = 
    if validPassword sender password
        then if (userBalance sender blockchain) >= amount
            then True
            else False
        else False

allTransactions :: Blockchain -> [Transaction]
allTransactions (Blockchain blocks) = allTransactionsAux blocks

allTransactionsAux :: [Block] -> [Transaction]
allTransactionsAux [] = []
allTransactionsAux (block:blocks) = transactions block ++ allTransactionsAux blocks

----------------------------
-- Mining / Proof of work --
----------------------------

-- CAN MAYBE BE REMOVED? 
{- 	newBlock blockchain transaction
	Creates a new block with a transaction.
	RETURNS: A new block that contains transaction, which can be added to blockchain.
-}
newBlock :: Blockchain -> Transaction -> Block
newBlock blockchain newTransaction = Block newIndex [newTransaction] proof previousHash
  where
    newIndex = 1 + (index $ lastBlock blockchain)
    proof = snd $ mineBlock (lastBlock blockchain)
    previousHash = fst $ mineBlock (lastBlock blockchain)

-- Works with the client side.
newBlockIO :: Blockchain -> Block -> Block
newBlockIO blockchain block = Block newIndex newTransactions proof previousHash
  where
    newIndex = 1 + (index $ lastBlock blockchain)
    newTransactions = transactions block
    proof = snd $ mineBlock (lastBlock blockchain)
    previousHash = fst $ mineBlock (lastBlock blockchain)

{-  mineBlock block
    Runs a proof of work mechanism on block.
    RETURNS: Hash of block beginning with three 0's and a proof.
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
    Takes a block and a nonce and hashes it with SHA256.
    RETURNS: Hash of nonce (arbitrary number) and the information in block.
-}
hashBlock :: Block -> Int -> String
hashBlock block nonce = show $ hashWith SHA256 $ toByteString' $ (show nonce ++ previousHash block ++ transactionsToString block)
