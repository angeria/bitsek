# Bitsek
> A simple cryptocurrency in Haskell.

## Table of Contents
- [Bitsek](#bitsek)
  * [About](#about)
  * [Contributors](#contributors)
  * [Introduction](#introduction)
  * [Features](#features)
  * [Getting started](#getting-started)
    + [Requirements](#requirements)
    + [Usage](#usage)
    + [Testing](#testing)
  * [Program Documentation](#program-documentation)
    + [Data Structures](#data-structures)
    + [Functions & Algorithms](#functions--algorithms)
      - [Mining blocks](#mining-blocks)
      - [Encrypting passwords](#encrypting-passwords)
      - [Validating the blockchain](#validating-the-blockchain)
      - [Main, Program and State Architecture](#main-program-and-state-architecture)
      - [Program Subfunctions](#program-subfunctions)
        * [Sending Bitsek](#sending-bitsek)
        * [Mining](#mining)
        * [Printing transactions](#printing-transactions)
      - [Control Flow](#control-flow)
  * [Future Improvements](#future-improvements)
    + [User Interface](#user-interface)
    + [Write to file](#write-to-file)
    + [Network and Consensus](#network-and-consensus)
    + [Mining Reward](#mining-reward)


## About
This is a Haskell project in the course "Program Design and Data Structures" during spring term 2018 at Uppsala University.

## Contributors
Benjamin Angeria, Holger Swartling and Fabian Haglund.

## Introduction
Bitsek is a locally run cryptocurrency with a non-distributed ledger. The ledger is stored in a blockchain with a proof of work-mechanism based on hashing with SHA256. In its current state, no network is involved and no information is saved after the session is closed.

The program has side-effects and IO in _Main.hs_ to interact with the user. However, the core in _Bitsek.hs_ which does the heavy lifting is functionally pure. 

## Features

- Set up a blockchain with a local ledger.
- Send and receive cryptocurrency locally to/from other users.
- Mine blocks.
- Show verified transactions.
- Create new users.
- List all users.

## Getting started
### Requirements
Make sure you have the [Haskell Platform](https://www.haskell.org/downloads#platform) installed.

Additionally, Bitsek uses the following packages.
* [cryptonite](https://hackage.haskell.org/package/cryptonite)
* [bytestring-conversion](https://hackage.haskell.org/package/bytestring-conversion)

Run these commands in the terminal to install them:

```
$ cabal install cryptonite
$ cabal install bytestring-conversion
```

### Usage

It's time to build the source with GHC by running:

```
$ ghc Main
```

A bunch of new files should now have popped up in the repository folder, including a executable file called _Main_.

Run it by typing in:

```
& ./Main
```

You should be greeted with the main menu. Let's take Bitsek for a test drive.
![main-menu](https://i.imgur.com/NC94yKZ.png)

A natural first step as a new user would be to create a new user. Do this by writing _new_ and press enter. Here we are creating an account for Mr. Satoshi Nakamoto. He is immediately gifted with 1000 Bitsek.
![new-user](https://i.imgur.com/K6GsgLa.png)

Satoshi wants to send all his newly aquired Bitsek to someone. He first has to take a look at the user list by typing in _list_. He recognizes his friend Dave and sends all his Bitsek to him by typing in _send_. He inputs his own adress, password, dave's adress, and amount. Confirm the input by typing _y_, or _n_ to correct it.

**CAUTION**: Be precise with your input. It would be very tragic if you'd mistakenly send your Bitsek to the wrong person. There are no refunds in the blockchain world - code is law.
![users](https://i.imgur.com/iAxNTT0.png)

Everything went smooth. Satoshi takes a look at the user list again to see if Dave has received his Bitsek.
![after-send](https://i.imgur.com/lQGXecJ.png)

What it this? It seems like nothing has happened. Does Satoshi have to wait 2-3 banking days before the transaction has cleared? The answer is no - he simply has to do some mining. For the transaction to be confirmed and part of the the blockchain, the current block with all pending transactions has to be mined.

Satoshi decides to mine the block by running _mine_ from the main menu. His computer does the embedded proof-of-work-mechanism by solving a cryptographic puzzle to prove he has invested time and hash power.

![mine](https://i.imgur.com/yfwg6RP.png)

And just like that, the transaction has been put into the ledger.

![after-mine](https://i.imgur.com/IXYIqs5.png)

Take a look at the video below to see all the functionality in action.

[![asciicast](https://asciinema.org/a/sYSh7OOmaof5QOq3Hy62CUE9I.png)](https://asciinema.org/a/sYSh7OOmaof5QOq3Hy62CUE9I)

___

### Testing

If you want to run unit tests, load _Tests.hs_ in your terminal by typing in the following:

```
$ ghci Tests
$ runTests
```

![run-tests](https://i.imgur.com/qkH1uT4.png)

## Program Documentation

### Data Structures
Bitsek is built around four data types: **User**, **Transaction**, **Block** and **Blockchain**. 

First of all, let's look at the **User** data type.

```haskell
data User = User { adress :: String
                 , privateKey :: String 
                 , balance :: Int 
                 }
```

A user in Bitsek is declared with an _adress_, a _private key_ and a _balance_. The _adress_ is a publicly visible name to which people can send Bitsek. The _private key_ is the encrypted hash of a secret password that the user inputs during creation of a new user. The secret password is needed when sending Bitsek. _Balance_ is the user's current account balance.

Secondly, we have the **Transaction** data type. 

```haskell
data Transaction = Transaction { sender :: User
                               , receiver :: User
                               , amount :: Int
                               }
```
A transaction consists of _sender_, _receiver_ and _amount_ - all of which are rather self explanatory.

Thirdly, there is the **Block** data type.

```haskell
data Block = Block { index :: Int 
                   , transactions :: [Transaction]
                   , proof :: Int
                   , previousHash :: String
                   }
```

Compared to **User** and **Transaction**, the **Block** data type is a bit more complex - but nothing to be scared about. It consists of _index_, _transactions_, _proof_, and _previous hash_. Every block has an index, where the first block in the blockchain (the _Genesis Block_) has index 0. The blocks after it has 1, 2, 3 and so on. This is to keep track of the order they were created. Each block also has a list of transactions. The list can be empty or it can contain an arbitrary number of transactions. It contains all the transaction that were made since the block before it in the blockchain was mined. Talking about mining brings us to the next component of a block - the _proof_. The proof is the nonce (an arbitrary int), that when hashed together with the previous block gives a string that matches a predefined condition. Lastly, the _previous hash_ is simply the hash of the block before it in the blockchain.

The fourth and last data type used, is **Blockchain**. It represents a list of blocks where the head of the list is the latest mined block. 

```haskell
data Blockchain = Blockchain [Block]
```

### Functions & Algorithms

#### Mining blocks

The mining of a block is essential to the inner workings of a blockchain. In order to secure and verify transactions we use a proof-of-work mechanism that makes any given node apply computational power to solve a cryptographic problem. Here, finding a nonce that in combination with the input data generates a hash that satisfies a given precondition. In this case the precondition is that the resulting hash begins with three 0's.

```haskell
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
```

#### Encrypting passwords
This function makes use of SHA256 to encrypt passwords and protect users. 

In detail, we convert the password of type string into a byte string. We then proceed to hash the byte string using the aforementioned SHA256 algorithm. The result is data of type _Digest SHA256_. Therefore, we finally call show on the result to convert it to a regular string.

```haskell
encryptPassword :: String -> String
encryptPassword password = show $ hashWith SHA256 $ toByteString' password

```

#### Validating the blockchain

The _validBlockchain_ function recurringly checks the blockchain for tampering. The proof-of-work concept makes this very easy to do. New blocks are hashed with a random nonce to create a hash that conforms to our specifications. The nonce for a particular block is saved in the next block as the _proof_ as well as the _hash_ for that block with that nonce. When looking back, instead of hashing through all ints till the proof is reached we can now have it hash the block with the nonce already provided. This is a lot faster than the process of actually finding the proofs. If the new hash is the same as the _previousHash_, this particular link of the chain is valid. If any link would be invalid, the chain past that point is invalid as well.

```haskell
validBlockchain :: Blockchain -> Bool
validBlockchain (Blockchain blocks) = validBlockchainAux (reverse blocks) 

validBlockchainAux :: [Block] -> Bool
validBlockchainAux [] = True
validBlockchainAux [x] = True
validBlockchainAux (x:xs)
   | hashBlock x (proof (head xs)) == (previousHash (head xs)) = validBlockchainAux xs
   | otherwise = False
```

#### Main, Program and State Architecture

State is a question of necessity. In order for user interactions to be useful over time we need to store the state of pending transactions (those transactions that are waiting to be mined and verified), the blockchain itself and a finally a list of users.

We decided to go with the straight forward approach of passing around state in our main program loop and mutate it as we go. We start off by setting up initial values for an empty pending block (_initpb_), a blockchain only containing the genesis block (_initbc_) and a list containing root users (_initus_). _main_ ends its work by calling _program_ with these values as arguments.

```haskell
main :: IO b
main = do 
    let initpb = (Block 0 [] 0 "")
    let initbc = (Blockchain [genesisBlock])
    let initus = [fabbe, benne, hogge, dave]
    program (initpb, initbc, initus)
```

_program_ handles the interactive I/O loop so to speak. Firstly, _menu_ prints a menu of options and asks the user for some _action_. _program_ then delegates the _action_ to the correct subfunction and the program continues from there.

```haskell
program :: (Block, Blockchain, [User]) -> IO b
program (pb, bc, us) = do 
    menu
    action <- getLine
    case action of 
        "send" -> sendBitsek (pb, bc, us)
        "balance" -> showBalance (pb, bc, us)
        "mine" -> mineBitsek (pb, bc, us)
        "show" -> printTransactions (pb, bc, us)
        "new" -> createUser (pb, bc, us)
        "list" -> printUsers (pb, bc, us)
	"q" -> exitWith ExitSuccess
```

An informal convention is that these subfunctions always end by again calling _program_, possibly (usually) with modified state values as new arguments. For any state x that is modified, the modified state value is then denoted as x'.

#### Program Subfunctions

##### Sending Bitsek

_sendBitsek_ handles the sending of bitsek between users by adding a transaction request to the pending block. It asks the user for some information, namely; _sender_, _sender password_, _receiver_ and _amount_ to send. It then controls whether the entered password is correct and if _sender_ has sufficient funds. If so, then a transaction is created and added to the pending block, containing a list of transactions requests. Finally it calls _program_ with a modified pending block state, _pb'_.   

```haskell
sendBitsek :: (Block, Blockchain, [User]) -> IO b
sendBitsek (pb, bc, us) = do 

	-- a bunch of statements
	
	program (pb', bc, us)
	
```

##### Mining

_mineBitsek_ utilises the pure function _newBlock_ to secure and verify requested transactions and add them to the blockchain. Read more about this in the "Mining blocks" section above.

**Reminder**: If you want your transaction verified and added to the blockchain, you have to mine the pending block. This job is done by _mineBitsek_.

```haskell
mineBitsek :: (Block, Blockchain, [User]) -> IO b
mineBitsek (pb, bc, us) = do

    -- another bunch of statements

    program (pb', bc', us)
```

##### Printing transactions

_printTransactions_ fetches all transactions from the current blockchain (that is, all thus far mined transactions).

```haskell
printTransactions :: (Block, Blockchain, [User]) -> IO b
printTransactions (pb, bc, us) = do
    putStrLn "--------------------------------" 
    printTransactionsAux (allTransactions bc)

    putStrLn "Press enter to go back to main menu."
    getChar
    program (pb, bc, us)

printTransactionsAux :: [Transaction] -> IO ()
printTransactionsAux [] = putStrLn ""
printTransactionsAux (t:ts) = do
    let s = adress (sender t)
    let r = adress (receiver t)
    let a = show (amount t)
    putStrLn ("From: " ++ s ++ "  |  To: " ++ r ++ "  |  Amount: " ++ a)
    printTransactionsAux ts
```

#### Control Flow

Diagram displaying the control flow of the IO top layer.

![main-control-flow](https://i.imgur.com/xI7zkDW.png)

## Future Improvements
Bitsek is in an early development phase and there is long way left before it's a viable cryptocurrency. In its current state it can only be started locally in a single terminal session. After closing the terminal the blockchain is gone. No network is involved and the ledger is hence non-distributed. The following features are crucial to further developing Bitsek.

### User Interface
The user interface of Bitsek is still young and sensitive to peculiar input. It's important to type with clinical accuracy or else the program might crash. Future work will be done to improve the graphical interface as well as implement input and exception handling.

### Network and Consensus
Another crucial feature to implement is network and blockchain consensus. This would mean that several nodes in a  network could make transactions and mine simultaneously. The consensus mechanism works simply by choosing the longest blockchain with a valid proof-of-work. The blockchain technology hence rely on that the major part of the miners have good intentions to outwork potenial malevolent miners with hash power.

### Mining Reward
To amass constructive hash power from nodes, there has to be an incentive for mining. A solution is to give some of the currency as a reward to the node that successfully mines a block. At the same time this reward mechanism acts as the influx of currency into circulation.
