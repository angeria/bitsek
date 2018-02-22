----------------
-- UNIT TESTS --
----------------

test1 = TestCase (assertEqual "for adressTaken (adress dave) [dave]," True (adressTaken (adress dave) [dave]))
test2 = TestCase (assertEqual "for adressTaken (adress dave) [fabbe,benne]," False (adressTaken (adress dave) [fabbe, benne]))
test3 = TestCase (assertEqual "for adressTaken (adress dave) [fabbe,benne]," False (adressTaken (adress dave) []))

test4 = TestCase (assertEqual "for encryptPassword pw," (privateKey dave) (encryptPassword "monadsforbreakfast"))

test5 = TestCase (assertEqual "for validPassword u pw" True (validPassword dave "monadsforbreakfast"))


testTransactions = [(Transaction dave fabbe 42), (Transaction dave hogge 42)]
test6 = TestCase (assertEqual "for aggUser u ts" (balance (User "dave" "4c7be2f6d37d20fe95050f329b58bcb3b552c3544260b14319964314b38ad416" 916)) 
                                        (balance (aggUser dave testTransactions)))

tests = TestList [TestLabel "adressTaken1" test1, 
                  TestLabel "adressTaken2" test2, 
                  TestLabel "adressTaken3" test3,
                  TestLabel "encryptPassword1" test4,
                  TestLabel "validPassword1" test5,
                  TestLabel "aggUser1" test6] 