---------------------------------------------------------------------
--                 Haskell Project: Bitsek                         --
-- Authors: Benjamin Angeria, Fabian Haglund and Holger Swartling. --
---------------------------------------------------------------------

import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Binary
import Data.ByteString.Conversion

data User = User Adress Balance deriving (Show)
type Adress = String
type Balance = Int

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
data Blockchain = EmptyBlockchain | Blockchain [Block] deriving (Show)

----------------
-- BLOCKCHAIN --
----------------

-- Testing function for blockchain.
exampleHashWith :: String -> String
exampleHashWith msg = show $ hashWith SHA256 $ B.pack msg


-- Testing variables.
fabbe = User "Fabbe" 100
benne = User "Benne" 100
testTransaction = Transaction benne fabbe 100
testBlock1 = Block {index = 1, transactions = [testTransaction], proof = 0, previousHash = (show $ hashWith SHA256 $ B.pack "test1")}
testBlock2 = Block {index = 2, transactions = [testTransaction], proof = 1, previousHash = (show $ hashWith SHA256 $ B.pack "test2")}
testBlockchain = Blockchain [testBlock2, testBlock1, genesisBlock]

genesisBlock = Block {index = 0, transactions = [], proof = 0, previousHash = (show $ hashWith SHA256 $ B.pack "plants are institutions")}

addToBlockchain :: Blockchain -> Block -> Blockchain
addToBlockchain (Blockchain blocks) newBlock = Blockchain (newBlock:blocks)

newBlock :: Blockchain -> Transaction -> Block
newBlock blockchain newTransaction = Block newIndex newTransactions proof previousHash
  where
    newIndex = 1 + (index $ lastBlock blockchain)
    newTransactions = newTransaction:(transactions $ lastBlock blockchain)
    proof = snd $ mineBlock (lastBlock blockchain)
    previousHash = fst $ mineBlock (lastBlock blockchain)

addTransaction :: Block -> Transaction -> Block
addTransaction (Block index transactions proof previousHash) newTransaction = Block index (newTransaction:transactions) proof previousHash


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
            userToString (User adress balance) = adress ++ show balance

-- TODO
{-
proofOfWork :: Blockchain -> Proof
proofOfWork blockchain = lastblock blockchain
-}

{-  lastBlock blockchain 
    Takes a blockchain and returns the last block in it.
    PRE: blockchain must be non-empty.
-}
lastBlock :: Blockchain -> Block
lastBlock blockchain = case blockchain of Blockchain (x:xs) -> x