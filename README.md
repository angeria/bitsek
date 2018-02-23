# Bitsek
> A simple cryptocurrency in Haskell.

## Table of Contents
- [Bitsek](#bitsek)
  * [Introduction](#introduction)
  * [Authors](#authors)
  * [Getting started](#getting-started)
    + [Requirements](#requirements)
    + [Setup](#setup)
    + [Heads up](#heads-up)
  * [Table of Contents](#table-of-contents)
  * [Introduction](#introduction)
  * [Inner workings](#inner-workings)

## About
This is a Haskell project in the course "Program Design and Data Structures" during spring term 2018 at Uppsala University.

## Developers 
Benjamin Angeria, Holger Swartling and Fabian Haglund.

## Introduction
Bitsek is a locally run cryptocurrency with a non-distributed ledger. It runs on a blockchain with a proof of work-mechanism based on hashing with SHA256.

The program has side-effects and IO in _Main.hs_ to interact with the user. However, the core in _Bitsek.hs_ is computationally pure.

## Features

- Set up a blockchain.
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

Run these commands in the terminal to install them.

```
$ cabal install cryptonite
$ cabal install bytestring-conversion
```

### Usage

Start off by running the _Main_ executable in the _app_ folder. You should be greeted with the main menu in the terminal.
![main-menu](https://i.imgur.com/pnNQWa6.png)

A natural first step as a new user would be to create a new user. Do this by writing _new_ and press enter. Here we are creating an account for Mr. Satoshi Nakamoto. He is immediately gifted with 1000 Bitsek.
![new-user](https://i.imgur.com/taYfsBF.png)

You could now for example send some Bitsek to someone by choosing the _send_ option. Input your adress, password, the reciever's address and the amount. After you have completed the transaction, you have to choose the _mine_ option for it to be verified and added to the blockchain.
![send-and-mine](https://i.imgur.com/qjdaE0H.png)

The video below shows a typical use case.

[![asciicast](https://asciinema.org/a/sYSh7OOmaof5QOq3Hy62CUE9I.png)](https://asciinema.org/a/sYSh7OOmaof5QOq3Hy62CUE9I)

___

### Testing

If you want to run unit tests, load _Tests.hs_ in your terminal by typing in the following.

```
$ ghci Tests
$ runTests
```

![run-tests](https://i.imgur.com/qkH1uT4.png)

## Program Documentation

### Data Structures
Bitsek is built around four data types: User, Transaction, Block and Blockchain. 

First of all, let's look at the User data type.
```haskell
data User = User { adress :: String
                 , privateKey :: String 
                 , balance :: Int 
                 }
```
A user in Bitsek is declared with an _adress_, a _private key_ and a _balance_. The _adress_ is a publicly visible name to which people can send Bitsek. The _private key_ is the encrypted hash of a secret password that the user inputs during creation of a new user. The secret password is needed when sending Bitsek. _Balance_ is the user's current account balance.

Secondly, we have the Transaction data type. 

```haskell
data Transaction = Transaction { sender :: User
                               , receiver :: User
                               , amount :: Int
                               }
```
A transaction consists of _sender_, _receiver_ and an _amount_ - all of which are rather self explanatory.

Thirdly, there is the Block data type.

```haskell
data Block = Block { index :: Int 
                   , transactions :: [Transaction]
                   , proof :: Int
                   , previousHash :: String
                   }
```

Compared to User and Transaction, the Block data type is a bit more complex - but nothing to be scared about. It consists of _Index_, _Transactions_, _Proof_, and _PreviousHash_. Every block has an index, and the first block in the blockchain (a.k.a. the _Genesis Block_) has index 0. The blocks after it has 1, 2, 3 and so on. This is to keep track of the order they were created. Each block also has a list of transactions. The list can be empty or it can contain an arbitrary number of transactions. It contains all the transaction that were made since the block before it in the blockchain was mined. Talking about mining brings us to the next component of a block - the _proof_. The proof is the nonce (an arbitrary int), that when hashed together with the previous block gives a string that matches a predefined condition. Lastly, the _PreviousHash_ is simply the hash of the block before it in the blockchain.

The fourth and last data type used, is Blockchain. It represents a list of blocks where the head of the list is the latest mined block. 

```haskell
data Blockchain = Blockchain [Block]
```

### Functions & Algorithms

#### Mining blocks

The mining of a block is essential to the inner workings of a blockchain. In order to secure and verify transactions we use a proof of work mechanism that basically makes any given node apply computational power to solve some mathematical problem. In this case finding a nonce that in combination with the input data generates a hash that satisfies a given precondition. In this case the precondition is that the hash begins with three zeroes.

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
This function makes use of the widely popular SHA256 algorithm to encrypt passwords and protect users. 

In detail, we convert the password of type string into a byte string, we then proceed to hash this string using the aforementioned SHA256 algorithm, finally we call show with the result in order to return the result as a regular string. 

```haskell
encryptPassword :: String -> String
encryptPassword password = show $ hashWith SHA256 $ toByteString' password

```

#### Validating the blockchain

The _validBlockchain_ function recurringly checks the blockchain for tampering. The proof of work concept makes this very easy to do. New blocks are hashed with a random nonce to create a hash that conforms to our specifications. The nonce for a particular block is saved in the next block as the _proof_ as well as the _hash_ for that block with that nonce. When looking back, instead of hashing through all ints till the proof is reached we can now have it hash the block with the nonce already provided. If the new hash is the same as the _previousHash_ this particular link of the chain is valid. If any link would be invalid, the chain past that point is invalid as well.

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

#### Main, Program and State

State is a question of necessity. In order for user interactions to be useful over time we need to store the state of pending transactions (those transactions who are waiting to be mined and verified), the blockchain itself and a list users.

We decided to go with the straight forward approach of passing around state in our main program loop and mutate it as we go. 

First we set up initial values for an empty pending block, a blockchain with only the genesis block and a list containing only root users. The main function then preceeds to call program with these three values as arguments.

```haskell
main :: IO b
main = do 
    let initPendingBlock = (Block 0 [] 0 "")
    let initBlockchain = (Blockchain [genesisBlock])
    let initUsers = [fabbe, benne, hogge, dave]
    program (initPendingBlock, initBlockchain, initUsers)
```

Program then handles the interactive I/O loop so to speak. It simply prints out a menu of options and asks the user for some action. It then delegates the action to the correct subfunction and the program continues from there. Without getting into details of all subfunctions, it is useful to know that these subfunctions always end by again calling program, possibly (usually) with modified state values as new arguments.

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
```

#### Control Flow

![main-control-flow](https://i.imgur.com/xI7zkDW.png)

## Future Improvements
Bitsek is in an early development phase and there is long way left before it's a viable cryptocurrency. In its current state it can only be started locally in a single terminal session. After closing the terminal the blockchain is gone. No network is involved and the ledger is hence non-distributed. The following features are crucial to further developing Bitsek.

### User Interface
The user interface of Bitsek is still young and sensitive to peculiar input. It's important to type with clinical accuracy or else the program might crash. Future work will be done to improve the graphical interface as well as implement input and exception handling.

### Write to file
As mentioned, a shortcoming of the program is that the blockchain and the transactions it holds doesn't get saved when the terminal is closed. This could be solved by writing everything to a file instead of just keeping it in the memory. This shouldn't be too hard to implement.

### Network and Consensus
Another more challenging feature is to implement network and blockchain consensus. This would mean that several nodes in a  network could make transactions and mine simultaneously. The consensus mechanism works simply by choosing the longest blockchain with a valid proof-of-work. The blockchain technology hence rely on that the major part of the miners have good intentions to outwork potenial malevolent miners with hash power.

### Mining Reward
To amass constructive hash power from nodes, there has to be an incentive for mining. A solution is to give some of the currency as a reward to the node that successfully mines a block. At the same time this reward mechanism acts as the influx of currency into circulation.
