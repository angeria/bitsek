{-
        users.hs

        here you will find the following: 
        * blockchain -> user data aggregates
        * blockchain -> all transactions 
        * blockchain -> all senders 
        * blockchain -> all recipients 
        * blockchain -> balances

-}

module Users where

data User = User Adress Balance deriving (Show)
type Adress = String
type Balance = Int

data Transaction = Transaction User User Amount deriving (Show)
type Amount = Int

data Block = Block { index :: Int
                   , transactions :: [Transaction]
                -- , proof :: -- Type?
                -- , previousHash :: Hash
                   } deriving (Show)

-- Latest block should be head of list.
data Blockchain = EmptyBlockchain | Blockchain [Block] deriving (Show)

-- UTILITY FUNCTIONS

user :: Adress -> Blockchain -> User
user ad bc = findUser ad (allUsers bc)

findUser :: Adress -> [User] -> User
findUser ad [] = User "not found" 0
findUser ad ((User uad b):us) 
    | ad == uad = (User uad b)
    | otherwise = findUser ad us


-- AGGREGATE DATA 

{-
Transaction "fabbe" "benne" 10
Transaction "benne" "hogge" 5
Transaction "fabbe" "hogge" 20
Transaction "hogge" "benne" 10

User "fabbe" 100 
User "benne" 100 
User "hogge" 100 

senders = "fabbe", "benne", "hogge"
recipients = "benne", "hogge", "benne"
-}


allUsers :: Blockchain -> [User]
allUsers = undefined

-- for now use:
initUsers = [(User "fabbe" 100), (User "benne" 100), (User "hogge" 100)]

allTransactions :: Blockchain -> [Transaction]
allTransactions (Blockchain []) = []
allTransactions (Blockchain (block:blocks)) = transactions block ++ aggTransactions blocks

{- aggUsers users transactions

    PRE: input users list is with starting default values,
         transaction list contains all transactions

-}
aggUsers :: [User] -> [Transaction] -> [User]
aggUsers [] _ = []
aggUsers (u:us) ts = (aggUser u ts) : (aggUsers us ts)

{- aggUser user transactions

    PRE: input user is with starting default values
         transaction list contains all transactions

-}
aggUser :: User -> [Transaction] -> User
aggUser u [] = u
aggUser u (t:ts) = aggUser (updateUser u t) ts

updateUser :: User -> Transaction -> User 
updateUser (User ad b) (Transaction s r am)
    | ad == s = (User ad (b-am))
    | ad == r = (User ad (b+am))
    | otherwise = (User ad b)


-- UPDATE SPECIFIC USERS 

-- not sure if useful, experimenting
updateSender :: Transaction -> [User] -> User
updateSender _ [] = (User "not found" 0)
updateSender (Transaction s _ am) (u:us)
    | s == ad = (User ad (b - am))
    | otherwise = updateSender (Transaction s _ am) us

updateRecipient :: Transaction -> [User] -> User
updateRecipient _ [] = (User "not found" 0)
updateRecipient (Transaction _ r am) (u:us)
    | u == ad = (User ad (b + am))
    | otherwise = updateRecipient (Transaction _ r am) us