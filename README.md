# Bitsek

A simple cryptocurrency in Haskell.

- [Bitsek](#bitsek)
  * [About](#about)
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

Bitsek is a local cryptocurrency with a non-distributed ledger. It runs on a blockchain and a proof-of-work mechanism based on hashing with SHA256.

## Authors 
Benjamin Angeria, Holger Swartling and Fabian Haglund.

## Features

- Send and receive cryptocurrency to/from other users.
- Mine blocks.
- Show verified transactions.
- Create new users.
- List all users.

# Getting started
### Requirements
Make sure you have the Haskell Platform installed.

Additionally, Bitsek uses the following packages.
* cryptonite
* bytestring-conversion

To install them, run the these commands in your terminal.

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



### Heads Up
Bitsek is still young and sensitive to peculiar input.  
Make sure to type with clinical accuracy.

## Inner workings

The program runs client-side commands in Main.hs but uses the computationally pure Bitsek.hs for more demanding calculations.
