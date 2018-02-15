---------------------------------------------------------------------
--                 Haskell Project: Bitsek                         --
-- Authors: Benjamin Angeria, Fabian Haglund and Holger Swartling. --
---------------------------------------------------------------------

import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Binary
import Data.ByteString.Conversion

{-  User Adress PrivateKey Balance
    - Adress: A public adress that money can be sent to.
    - PrivateKey: A secret password needed to complete a transaction from the users wallet.
    - Balance: The user's total funds.
-}
data User = User { adress :: String
                 , privateKey :: String 
                 , balance :: Int 
                 } deriving (Show)

-- Transaction Sender Receiver Amount
data Transaction = Transaction Sender Receiver Amount deriving (Show)
type Sender = User
type Receiver = User
type Amount = Int

data Block = Block { index :: Int 
                   , transactions :: [Transaction]
                   , proof :: Int
                   , previousHash :: String
                   } deriving (Show)

-- Latest block should be head of list.
data Blockchain = Blockchain [Block] deriving (Show)

-----------------------
-- TESTING VARIABLES --
-----------------------
-- password: singularity
fabbe = User "Fabbe" "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da" 100
-- password: entropy
benne = User "Benne" "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992" 100
people = [fabbe, benne]
testTransaction = Transaction benne fabbe 100
-- testBlockchain = Blockchain [testBlock2, testBlock1, genesisBlock]
genesisBlock = Block {index = 0, transactions = [], proof = 0, previousHash = (show $ hashWith SHA256 $ B.pack "plants are institutions")}
genesisBlockchain = Blockchain [genesisBlock]
testBlockchain1 = Blockchain [Block {index = 1, transactions = [Transaction (User {adress = "Benne", privateKey = "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992", balance = 100}) (User {adress = "Fabbe", privateKey = "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da", balance = 100}) 100], proof = 911, previousHash = "000854f0985938bb5d557eadef1bbc8f1d0ab9bf46d58cecfdb774c87f2094c2"}
                             ,Block {index = 0, transactions = [], proof = 0, previousHash = "a2f2e5f03072b1b8d0b5ad55a1d3da642f1c327ce8e5de89385651176743fb39"}]
testBlockchain2 = Blockchain [Block {index = 2, transactions = [Transaction (User {adress = "Benne", privateKey = "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992", balance = 100}) (User {adress = "Fabbe", privateKey = "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da", balance = 100}) 100], proof = 2719, previousHash = "00035fee66451dbc750d037bec5c5cb6e7f5e17c6a721e34db2de8be92d9dd1a"}
                             ,Block {index = 1, transactions = [Transaction (User {adress = "Benne", privateKey = "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992", balance = 100}) (User {adress = "Fabbe", privateKey = "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da", balance = 100}) 100], proof = 911, previousHash = "000854f0985938bb5d557eadef1bbc8f1d0ab9bf46d58cecfdb774c87f2094c2"}
                             ,Block {index = 0, transactions = [], proof = 0, previousHash = "a2f2e5f03072b1b8d0b5ad55a1d3da642f1c327ce8e5de89385651176743fb39"}]

hoggerBlock1 = Block {index = 1, transactions = [Transaction (User "Benne" "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992" 100) (User "Fabbe" "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da" 100) 52], proof = 911, previousHash = "000854f0985938bb5d557eadef1bbc8f1d0ab9bf46d58cecfdb774c87f2094c2"}
hoggerBlock2 = Block {index = 2, transactions = [Transaction (User "Benne" "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992" 100) (User "Fabbe" "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da" 100) 22,Transaction (User "Benne" "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992" 100) (User "Fabbe" "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da" 100) 27], proof = 2719, previousHash = "00035fee66451dbc750d037bec5c5cb6e7f5e17c6a721e34db2de8be92d9dd1a"}
hoggerBlock3 = Block {index = 3, transactions = [Transaction (User "Benne" "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992" 100) (User "Fabbe" "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da" 100) 31,Transaction (User "Benne" "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992" 100) (User "Fabbe" "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da" 100) 82,Transaction (User "Benne" "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992" 100) (User "Fabbe" "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da" 100) 12], proof = 1462, previousHash = "000970c8c3edafbf06fd059fd7bd30436eb8c6afd451004d8d5339f5bf0067da"}
hoggerChain = Blockchain [hoggerBlock3, hoggerBlock2, hoggerBlock1, genesisBlock]

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
userBalance u b = balance (aggUser u (allTransactions b))

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

allSenders :: [Transaction] -> [User]
allSenders [] = []
allSenders (t:ts) = sender t : allSenders ts

allReceivers :: [Transaction] -> [User]
allReceivers [] = []
allReceivers (t:ts) = receiver t : allReceivers ts

sender :: Transaction -> User
sender (Transaction s _ _) = s

receiver :: Transaction -> Receiver
receiver (Transaction _ r _) = r

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