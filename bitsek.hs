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
testTransaction = Transaction benne fabbe 100
-- testBlockchain = Blockchain [testBlock2, testBlock1, genesisBlock]
genesisBlock = Block {index = 0, transactions = [], proof = 0, previousHash = (show $ hashWith SHA256 $ B.pack "plants are institutions")}
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