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

{-  User
      - Adress: Public adress that money can be sent to.
      - PrivateKey: The hash of a secret password needed to send a transaction from a user's wallet.
      - Balance: User's total funds.
-}
data User = User { adress :: String
                 , privateKey :: String 
                 , balance :: Int 
                 } deriving (Show, Read)

{-  Transaction
    Transaction with a sender, receiver and amount.
-}
data Transaction = Transaction { sender :: User
                               , receiver :: User
                               , amount :: Int
                               } deriving (Show, Read)

{-  Block
    A block with the following information:
        Index: The index of the block, where the first block in the blockchain (the "genesis block") has index 0.
        Transactions: List of all the transactions in that block.
        Proof: The nonce (an arbitrary Int), that when hashed with the previous block gives a string that matches a predefined condition.
        PreviousHash: The hash of the block before the current block in the blockchain.
-}
data Block = Block { index :: Int 
                   , transactions :: [Transaction]
                   , proof :: Int
                   , previousHash :: String
                   } deriving (Show, Read)

{-  Blockchain
    Represents a list of blocks.
    INVARIANT: The latest mined block has to be the head of the list of blocks.

-}
data Blockchain = Blockchain [Block] deriving (Show, Read)

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
	Checks that a blockchain is valid by verifying that every block's hash meets the proof of work precondition.
	RETURNS: Bool saying if blockchain is valid or not.
-}
validBlockchain :: Blockchain -> Bool
validBlockchain (Blockchain blocks) = validBlockchainAux (reverse blocks)

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

----------------------------
-- Mining / Proof of work --
----------------------------

{-  newBlock blockchain block
    Generates a new block which can be added to a blockchain.
    PRE: blockchain must be non-empty.
    RETURNS: A new block based on information in input blockchain and block.
-}
newBlock :: Blockchain -> Block -> Block
newBlock blockchain block = Block newIndex newTransactions proof previousHash
  where
    newIndex = 1 + (index $ lastBlock blockchain)
    newTransactions = transactions block
    proof = snd $ mineBlock (lastBlock blockchain)
    previousHash = fst $ mineBlock (lastBlock blockchain)

{-  mineBlock block
    Runs a proof of work mechanism on block.
    RETURNS: Hash of block beginning with three 0's and a proof.
    EXAMPLE: mineBlock testBlock1 = ("00035fee66451dbc750d037bec5c5cb6e7f5e17c6a721e34db2de8be92d9dd1a",2719)
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


hashBlock :: Block -> Int -> String
hashBlock block nonce = show $ hashWith SHA256 $ toByteString' $ (show nonce ++ previousHash block ++ transactionsToString block)

--------------------
-- USER FUNCTIONS --
--------------------

{-  encryptPassword password
    Takes a password and encrypts it with SHA256.
    RETURNS: Hashed string of password.
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
    RETURNS: Balance of user in blockchain.
    EXAMPLE: userBalance benne testBlockchain = 1000
-}
userBalance :: User -> Blockchain -> Int
userBalance user blockchain = balance (aggUser user (allTransactions blockchain))    

{-  aggUsers users blockchain
    Updates the balance of all users in a list based on a given blockchain, see aggUsersAux
    RETURNS: A list of users with updated balance values
    EXAMPLE: aggUsers [(User "u1" _ 100), (User "u2" _ 100)] (Blockchain [(Block _ [(Transaction u1 u2 50)])] _ _)
                     = [(User "u1" _ 50), (User "u2" _ 150)]
-}
aggUsers :: [User] -> Blockchain -> [User]
aggUsers us b = aggUsersAux us (allTransactions b)

{-  aggUsersAux users transactions
    Computes the account balance for a list of users by recursively going through a list of transactions.
    RETURNS: A list of users with updated balance values
    EXAMPLE: aggUser [(User "u1" _ 100), (User "u2" _ 100)] [(Transaction u1 u2 50)]
                     = [(User "u1" _ 50), (User "u2" _ 150)]
-}
aggUsersAux :: [User] -> [Transaction] -> [User]
-- VARIANT: length users
aggUsersAux [] _ = []
aggUsersAux (u:us) ts = aggUser u ts : aggUsersAux us ts

{-  aggUser user transactions
    Computes the account balance for a user by recursively going through a list of transactions.
    RETURNS: u with updated balance with respect to ts
    EXAMPLE: aggUser (User "u1" _ 100) [(Transaction u1 u2 50)]
             = [(User "u1" _ 50), (User "u2" _ 150)]
-}
aggUser :: User -> [Transaction] -> User
-- VARIANT: length transactions
aggUser u [] = u
aggUser u (t:ts) = aggUser (updateUser u t) ts

{-  updateUser user transaction
    Updates user balance for a given transaction.
    RETURNS: u with updated balance with respect to t
    EXAMPLE: aggUser (User "u1" _ 100) [(Transaction u1 u2 50)])]
             = [(User "u1" _ 50), (User "u2" _ 150)]
-}
updateUser :: User -> Transaction -> User 
updateUser (User ad pkey balance) (Transaction sender receiver amount)
    | ad == (adress sender) = (User ad pkey (balance-amount))
    | ad == (adress receiver) = (User ad pkey (balance+amount))
    | otherwise = (User ad pkey balance)

{-  getUser adress blockchain
    Fetches a user from a blockchain for a given adress, see getUserAux
    RETURNS: user with the given adress, with correct current balance.
-}
getUser :: String -> [User] -> Blockchain -> User 
getUser ad us b = getUserAux ad (aggUsers us b)

{-  getUserAux adress users
    Fetches a user from a list of users by recursively checking each user adress.
    RETURNS: user with the given adress, with correct current balance.
-}
getUserAux :: String -> [User] -> User
-- VARIANT: length users
getUserAux ad [] = (User "User" "Not Found" 0)
getUserAux ad (u:us) 
    | ad == adress u = u
    | otherwise = getUserAux ad us

{-  adressTaken adress users
    Checks if an adress already has been taken by some user in users.
    RETURNS: Bool stating if adress exists in user list.
-}
adressTaken :: String -> [User] -> Bool
-- VARIANT: length users
adressTaken ad [] = False
adressTaken ad (u:us)
        | ad == adress u = True
        | otherwise = adressTaken ad us

---------------------------
-- TRANSACTION FUNCTIONS --
---------------------------

{-  transactionsToString block
    Takes a block and returns a concatenation of the whole data structure into a string.
    RETURNS: String of all transaction data in block.
    EXAMPLE: transactionsToString testBlock2 = "Fabbe100Benne10020"
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

{-  validTransaction blockchain transaction password
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

{-  allTransactions blockchain
    Takes a blockchain and returns a list of all transactions in that blockchain, see allTransactionsAux
    RETURNS: a list of transactions from blockchain
-}
allTransactions :: Blockchain -> [Transaction]
allTransactions (Blockchain blocks) = allTransactionsAux blocks

{-  allTransactionsAux blocks
    Retrieves all transactions from each block by recursively going through blocks
    RETURNS: a list of transactions from list of blocks
-}
allTransactionsAux :: [Block] -> [Transaction]
-- VARIANT: length blocks
allTransactionsAux [] = []
allTransactionsAux (block:blocks) = transactions block ++ allTransactionsAux blocks
