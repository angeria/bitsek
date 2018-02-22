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
* GHC
* cryptonite
* bytestring-conversion

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



### Heads up

Bitsek is still young and sensitive to peculiar input.  
Make sure to type with clinical accuracy.

## Table of Contents

## Introduction

## Inner workings

The program runs client-side commands in Main.hs but uses the computationally pure Bitsek.hs for more demanding calculations.

# This document should include:
- A table of contents, an introduction, and summary of what the program does.
- Use cases: a guide for how to actually use your program, including key examples.
- Program documentation: a description of how your program really works, including at least:
- Description of data structures. For abstract data types, you should also describe the interface.
- Description of the algorithms your program uses.
- Description of the various functions of the program. Describe algorithms and provide functional specifications for the main elements. Describe the control flow (i.e., how functions call each other).
- Description of known shortcomings of the program. There may be things that work but not as well as you would like, or things that despite valiant attempts you have not succeeded in implementing properly.
