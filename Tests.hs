import Test.HUnit
import Bitsek
import Data.ByteString.Conversion
import Crypto.Hash

-----------------------
-- TESTING VARIABLES --
-----------------------

-- password: singularity
fabbe = User "fabbe" "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da" 1000
-- password: entropy
benne = User "benne" "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992" 1000
-- password: anka 
hogge = User "hogge" "01d9db6e08b2426c3c56122aca300c143a60157de6899d6bc84614c40d86bb66" 1000
-- password: monadsforbreakfast
dave = User "dave" "4c7be2f6d37d20fe95050f329b58bcb3b552c3544260b14319964314b38ad416" 1000

genesisBlock = Block {index = 0, transactions = [], proof = 0, previousHash = (show $ hashWith SHA256 $ toByteString' "plants are institutions")}
genesisBlockchain = Blockchain [genesisBlock]

emptyBlock = Block {index = 0, transactions = [], proof = 0, previousHash = ""}
testBlock1 = Block {index = 1, transactions = [Transaction {sender = User {adress = "Benne", privateKey = "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992", balance = 100}, receiver = User {adress = "Fabbe", privateKey = "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da", balance = 100}, amount = 100}], proof = 911, previousHash = "000854f0985938bb5d557eadef1bbc8f1d0ab9bf46d58cecfdb774c87f2094c2"}
testBlock2 = Block {index = 2, transactions = [Transaction {sender = User {adress = "Fabbe", privateKey = "61933d3774170c68e3ae3ab49f20ca22db83a6a202410ffa6475b25ab44bb4da", balance = 100}, receiver = User {adress = "Benne", privateKey = "67671a2f53dd910a8b35840edb6a0a1e751ae5532178ca7f025b823eee317992", balance = 100}, amount = 20}], proof = 2719, previousHash = "00035fee66451dbc750d037bec5c5cb6e7f5e17c6a721e34db2de8be92d9dd1a"}
testBlockchain = Blockchain [testBlock2, testBlock1, genesisBlock]
invalidBlockchain = Blockchain [testBlock1, testBlock2, genesisBlock]
emptyBlockchain = Blockchain []

----------------
-- UNIT TESTS --
----------------

-- hashBlock
hashBlock1 = TestCase (assertEqual "for hashBlock genesisBlock 0" "921607be76a1d3afb408c5b68e728059348c78c3fa09e43ff50388f6f5d50132" (hashBlock genesisBlock 0))
hashBlock2 = TestCase (assertEqual "for hashBlock genesisBlock (-1)" "bf83e4dcf59d0d36c4be023010925327db90e1ce7df6484b51ec498fa0e05a16" (hashBlock genesisBlock (-1)))
hashBlock3 = TestCase (assertEqual "for hashBlock testBlock1 (9128391812323123322)" "ed6349412e46a496d6eca1965f965fce0397185c43d087ac6d106edfb28056cf" (hashBlock testBlock1 (9128391812323123322)))
-- TODO: Tests for numbers outside Int range.

validBlockchain1 = TestCase (assertEqual "for validBlockchain testBlockchain" True (validBlockchain testBlockchain))
validBlockchain2 = TestCase (assertEqual "for validBlockchain genesisBlockchain" True (validBlockchain genesisBlockchain))
validBlockchain3 = TestCase (assertEqual "for validBlockchain invalidBlockchain" False (validBlockchain invalidBlockchain))
validBlockchain4 = TestCase (assertEqual "for validBlockchain emptyBlockchain" True (validBlockchain emptyBlockchain))

mineBlock1 = TestCase (assertEqual "for mineBlock testBlock1" ("00035fee66451dbc750d037bec5c5cb6e7f5e17c6a721e34db2de8be92d9dd1a",2719) (mineBlock testBlock1))
mineBlock2 = TestCase (assertEqual "for mineBlock emptyBlock" ("000f21ac06aceb9cdd0575e82d0d85fc39bed0a7a1d71970ba1641666a44f530",886) (mineBlock emptyBlock))

-- newBlock1 = TestCase (assertEqual "for newBlock ")

-- adressTaken
adressTaken1 = TestCase (assertEqual "for adressTaken (adress dave) [dave]," True (adressTaken (adress dave) [dave]))
adressTaken2 = TestCase (assertEqual "for adressTaken (adress dave) [fabbe,benne]," False (adressTaken (adress dave) [fabbe, benne]))
adressTaken3 = TestCase (assertEqual "for adressTaken (adress dave) [fabbe,benne]," False (adressTaken (adress dave) []))

-- encryptPassword
encryptPassword1 = TestCase (assertEqual "for encryptPassword pw," (privateKey dave) (encryptPassword "monadsforbreakfast"))

-- validPassword
validPassword1 = TestCase (assertEqual "for validPassword u pw" True (validPassword dave "monadsforbreakfast"))

-- aggUser
testTransactions = [(Transaction dave fabbe 42), (Transaction dave hogge 42)]
aggUser1 = TestCase (assertEqual "for aggUser u ts" (balance (User "dave" "4c7be2f6d37d20fe95050f329b58bcb3b552c3544260b14319964314b38ad416" 916)) 
                                        (balance (aggUser dave testTransactions)))

runTests = runTestTT $ TestList [TestLabel "hashBlock1" hashBlock1,
                                 TestLabel "hashBlock2" hashBlock2,
                                 TestLabel "hashBlock3" hashBlock3,
                                 TestLabel "validBlockchain1" validBlockchain1,
                                 TestLabel "validBlockchain2" validBlockchain2,
                                 TestLabel "validBlockchain3" validBlockchain3,
                                 TestLabel "validBlockchain4" validBlockchain4,
                                 TestLabel "mineBlock1" mineBlock1,
                                 TestLabel "mineBlock2" mineBlock2,
                                 TestLabel "adressTaken1" adressTaken1, 
                                 TestLabel "adressTaken2" adressTaken2, 
                                 TestLabel "adressTaken3" adressTaken3,
                                 TestLabel "encryptPassword1" encryptPassword1,
                                 TestLabel "validPassword1" validPassword1,
                                 TestLabel "aggUser1" aggUser1]
                                 