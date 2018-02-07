-- Haskell Project: Bitsek 

-- Authors: Benjamin Angeria, Fabian Haglund and Holger Swartling.

data User = User Adress Balance
type Adress = String
type Balance = Int

data Transaction = Transaction Sender Recipient Amount
type Sender = String
type Recipient = String
type Amount = Int

data Block = Block { index :: Int
				   , transactions :: [Transaction]
				   , proof ::
				   , previousHash ::
				   } deriving (Show)

data Blockchain = Blockchain [Block]


----------------
-- BLOCKCHAIN --
----------------

newTransaction ::  
newTransaction = undefined

newBlock :: 
newBlock = undefined