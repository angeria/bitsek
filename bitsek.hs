-- Haskell Project: Bitsek 

-- Authors: Benjamin Angeria, Fabian Haglund and Holger Swartling.

data User = User Adress Balance deriving (Show)
type Adress = String
type Balance = Int

data Transaction = Transaction Sender Recipient Amount deriving (Show)
type Sender = String
type Recipient = String
type Amount = Int

data Block = Block { index :: Int
                   , transactions :: [Transaction]
                -- , proof :: -- Type?
                -- , previousHash :: Hash
                   } deriving (Show)

-- Latest block should be head of list.
data Blockchain = EmptyBlockchain | Blockchain [Block] deriving (Show)

----------------
-- BLOCKCHAIN --
----------------

genesisBlock = undefined

-- TODO
newTransaction :: 
newTransaction = undefined

-- TODO
newBlock :: 
newBlock = undefined


-- TODO
{-  hashBlock block
    Takes a block and returns the hash of it.
-}
hashBlock :: Block -> Hash
hashBlock = undefined


{-  lastBlock blockchain 
    Takes a blockchain and returns the last block in it.
    PRE: blockchain must be non-empty.
-}
lastBlock :: Blockchain -> Block
lastBlock blockchain = case blockchain of Blockchain (x:xs) -> x

