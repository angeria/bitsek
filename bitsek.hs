---------------------------------------------------------------------
--                 Haskell Project: Bitsek                         --
-- Authors: Benjamin Angeria, Fabian Haglund and Holger Swartling. --
---------------------------------------------------------------------

import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Binary

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
                   , previousHash :: Digest SHA256
                   } deriving (Show)

-- Latest block should be head of list.
data Blockchain = EmptyBlockchain | Blockchain [Block] deriving (Show)

----------------
-- BLOCKCHAIN --
----------------



-- Testing function
exampleHashWith :: String -> Digest SHA256
exampleHashWith msg = hashWith SHA256 $ B.pack msg

-- Testing variables.
fabbe = User "Fabbe" 100
benne = User "Benne" 100
testTransaction = Transaction benne fabbe 100
testBlock1 = Block {index = 1, transactions = [testTransaction], proof = 0, previousHash = (hashWith SHA256 $ B.pack "test1")}
testBlock2 = Block {index = 2, transactions = [testTransaction], proof = 1, previousHash = (hashWith SHA256 $ B.pack "test2")}
testBlockchain = Blockchain [testBlock1, testBlock2]

genesisBlock = Block {index = 0, transactions = [], proof = 0, previousHash = (hashWith SHA256 $ B.pack "genesis")}

addToBlockchain :: Blockchain -> [Transaction] -> Blockchain
addToBlockchain blockchain pendingTransactions = undefined

-- TODO
{-
newBlock ::
newBlock = undefined
-}

-- TODO
{-
proofOfWork :: Blockchain -> Proof
proofOfWork blockchain = lastblock blockchain
-}

-- TODO
{-  hashBlock block
    Takes a block and returns the hash of it.
-}
{-
hashBlock :: Block -> Hash
hashBlock = undefined
-}

{-  lastBlock blockchain 
    Takes a blockchain and returns the last block in it.
    PRE: blockchain must be non-empty.
-}
lastBlock :: Blockchain -> Block
lastBlock blockchain = case blockchain of Blockchain (x:xs) -> x