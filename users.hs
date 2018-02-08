{-
        users.hs

        here you will find the following: 
        * blockchain -> user data aggregates
        * blockchain -> all transactions 
        * blockchain -> all senders 
        * blockchain -> all recipients 
        * blockchain -> balances

-}

-- UTILITY FUNCTIONS

user :: Adress -> Blockchain -> User
user ad bc = findUser ad (allUsers bc)

findUser :: Adress -> [User] -> User
findUser ad [] = User "not found" 0
findUser ad ((User uad b):us) 
    | ad == uad = (User uad b)
    | otherwise = findUser ad us


-- AGGREGATE DATA 

aggTransactions :: Blockchain -> [Transaction]
aggTransactions (Blockchain []) = []
aggTransactions (Blockchain (block:blocks)) = transactions block ++ aggTransactions blocks

-- UPDATE SPECIFIC USERS 

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