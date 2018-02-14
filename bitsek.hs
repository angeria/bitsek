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
data Blockchain = Blockchain [Block] deriving (Show)

-----------------------
-- TESTING VARIABLES --
-----------------------

fabbe = User "Fabbe" 100
benne = User "Benne" 100
testTransaction = Transaction benne fabbe 100
testBlock1 = Block {index = 1, transactions = [testTransaction], proof = 0, previousHash = (show $ hashWith SHA256 $ B.pack "test1")}
testBlock2 = Block {index = 2, transactions = [testTransaction], proof = 1, previousHash = (show $ hashWith SHA256 $ B.pack "test2")}
testBlockchain = Blockchain [testBlock2, testBlock1, genesisBlock]
genesisBlock = Block {index = 0, transactions = [], proof = 0, previousHash = (show $ hashWith SHA256 $ B.pack "plants are institutions")}
genesisBlockchain = Blockchain [genesisBlock]

hoggerBlock1 = Block {index = 1, transactions = [Transaction (User "Benne" 100) (User "Fabbe" 100) 100], proof = 911, previousHash = "000854f0985938bb5d557eadef1bbc8f1d0ab9bf46d58cecfdb774c87f2094c2"}
hoggerBlock2 = Block {index = 2, transactions = [Transaction (User "Benne" 100) (User "Fabbe" 100) 100,Transaction (User "Benne" 100) (User "Fabbe" 100) 100], proof = 2719, previousHash = "00035fee66451dbc750d037bec5c5cb6e7f5e17c6a721e34db2de8be92d9dd1a"}
hoggerBlock3 = Block {index = 3, transactions = [Transaction (User "Benne" 100) (User "Fabbe" 100) 100,Transaction (User "Benne" 100) (User "Fabbe" 100) 100,Transaction (User "Benne" 100) (User "Fabbe" 100) 100], proof = 1462, previousHash = "000970c8c3edafbf06fd059fd7bd30436eb8c6afd451004d8d5339f5bf0067da"}
hoggerChain = Blockchain [hoggerBlock3, hoggerBlock2, hoggerBlock1, genesisBlock]

----------------
-- BLOCKCHAIN --
----------------

addToBlockchain :: Blockchain -> Block -> Blockchain
addToBlockchain (Blockchain blocks) newBlock = Blockchain (newBlock:blocks)

newBlock :: Blockchain -> Transaction -> Block
newBlock blockchain newTransaction = Block newIndex newTransactions proof previousHash
  where
    newIndex = 1 + (index $ lastBlock blockchain)
    newTransactions = newTransaction:(transactions $ lastBlock blockchain)
    proof = snd $ mineBlock (lastBlock blockchain)
    previousHash = fst $ mineBlock (lastBlock blockchain)

{-  lastBlock blockchain 
    Takes a blockchain and returns the last block in it.
    PRE: blockchain must be non-empty.
-}
lastBlock :: Blockchain -> Block
lastBlock blockchain = case blockchain of Blockchain (x:xs) -> x

validBlockchain :: Blockchain -> Bool
validBlockchain (Blockchain blocks) = validBlockchainAux1 reversedBlockchain where
   reversedBlockchain = reverse blocks

validBlockchainAux1 :: [Block] -> Bool
validBlockchainAux1 [] = True
validBlockchainAux1 [x] = True
validBlockchainAux1 (x:xs)
   | hashBlock x (proof (head xs)) == (previousHash (head xs)) = validBlockchainAux1 xs
   | otherwise = False

----------------------------------
-- BLOCKCHAIN UTILITY FUNCTIONS --
----------------------------------

allTransactions :: Blockchain -> [Transaction]
allTransactions (Blockchain blocks) = allTransactionsAux blocks

allTransactionsAux :: [Block] -> [Transaction]
allTransactionsAux [] = []
allTransactionsAux (block:blocks) = transactions block ++ allTransactionsAux blocks

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