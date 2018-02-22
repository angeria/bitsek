# Bitsek

A simple cryptocurrency in Haskell.

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

## Authors 
Benjamin Angeria, Holger Swartling and Fabian Haglund.

## Introduction
Bitsek is a local cryptocurrency with a non-distributed ledger. It runs on a blockchain and a proof-of-work mechanism based on hashing with SHA256.

The program has side-effect and IO in _Main.hs_ to interact with the user. However, the core in _Bitsek.hs_ is computationally pure for more demanding calculations.

## Features

- Send and receive cryptocurrency to/from other users.
- Mine blocks.
- Show verified transactions.
- Create new users.
- List all users.

# Getting started
### Requirements
Make sure you have the [Haskell Platform](https://www.haskell.org/downloads#platform) installed.

Additionally, Bitsek uses the following packages.
* [cryptonite](https://hackage.haskell.org/package/cryptonite)
* [bytestring-conversion](https://hackage.haskell.org/package/bytestring-conversion)

To install them, run these commands in your terminal.

```
$ cabal install cryptonite
$ cabal install bytestring-conversion
```

### Usage

Start off by running the _Main_ executable. You should be greeted with the main menu in the terminal.
![main-menu](https://i.imgur.com/pnNQWa6.png)

A natural first step as a new user would be to create a new user. Do this by writing _new_ and press enter. Here we are creating an account for Mr. Satoshi Nakamoto. He is immediately gifted with 1000 Bitsek.
![new-user](https://i.imgur.com/taYfsBF.png)

You could now for example send some Bitsek to someone by choosing the _send_ option. Input your adress, password, the reciever's address and the amount. After you have completed the transaction, you have to choose the _mine_ option for it to be verified and added to the blockchain.
![send-and-mine](https://i.imgur.com/qjdaE0H.png)

The video below shows a typical use case.

[![asciicast](https://asciinema.org/a/sYSh7OOmaof5QOq3Hy62CUE9I.png)](https://asciinema.org/a/sYSh7OOmaof5QOq3Hy62CUE9I)

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

### Algorithms
Lorem Ipsum

### Functions
Lorem Ipsum

### Control Flow
Lorem Ipsum

## Future Improvements
Lorem Ipsum


### Heads Up
Bitsek is still young and sensitive to peculiar input.  
Make sure to type with clinical accuracy.
