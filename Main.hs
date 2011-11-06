{---------------------------------------------------------------
 --
 -- Main.hs : the main program for the PolyGP system.
 -- T.Yu@cs.ucl.ac.uk	September 25, 1997
 --
 --------------------------------------------------------------}

module Main where
import Header(Expression(..),TypeExp(..))
import Auxil (getParas, create, evolve, displayPop)
import System.Environment (getArgs)
import System.Random (getStdGen, randoms)

randomInts :: Int -> Int -> [Int] -> [Int]
randomInts start end numbers = getRandomNumbers start end numbers
                        
randomDoubles :: Int -> Int -> [Double] -> [Double]
randomDoubles start end numbers = getRandomNumbers start end numbers

getRandomNumbers :: Int -> Int -> [a] -> [a]
getRandomNumbers 0 0 [] = []
getRandomNumbers start end (number:numbers) = if start > 0
                                    then
                                        getRandomNumbers (start-1) (end-1) numbers
                                    else
                                        number:getRandomNumbers 0 (end-1) numbers


main = do
       fileName <- getArgs
       inputs <- readFile "Para" -- (head fileName)
       gen <- getStdGen
       let (treeDepth, popSize, randomInt, maxEval, parScale, xOverRate) = getParas inputs 0 0 0 0 0.0 0
	in  if (treeDepth==0 || popSize==0 || randomInt==0 || maxEval==0 || parScale==0.0 || xOverRate==0) then
		print "Parameter reading fails."
            else
	       let (population, rList) = create popSize [] (randomInts randomInt (randomInt + 10) (randoms gen)) treeDepth
	   	   (population', dList, rList') = evolve population maxEval parScale popSize treeDepth xOverRate
				   (randomDoubles randomInt (randomInt+10) (randoms gen)) rList
		in 
	    	   displayPop 1 population'



