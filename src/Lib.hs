{-# LANGUAGE RankNTypes #-}
{-# options_ghc -Wno-unused-imports #-}
module Lib (pwdGen) where

import Data.List (intersperse)

import Control.Monad (replicateM)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.ST (ST,runST)
import System.Random.MWC (withSystemRandomST, withSystemRandom)
import System.Random.MWC.Probability (Prob, Gen, sample, samples, UniformRange(..), bernoulli, dirichlet, discrete, discreteUniform)

import System.IO (readFile)


rand0 :: Int -> IO [Char]
rand0 n = withSystemRandomST (\g -> samples n (uniformRM ('a', 'z') g) g)



-- pwd = do
--   pp <- wordsG
--   withSystemRandom $ \gen -> do
--     k1 <- uniformRM (0,1) gen
--     k2 <- uniformRM (0,1) gen
--     mixp <- dirichlet [k1, k2]

pwdGen :: Double -> Int -> IO String
pwdGen k n = do
  ws <- pwdTokensGen k n
  pure $ mconcat $ intersperse "-" ws


pwdTokensGen :: Double -> Int -> IO [String]
pwdTokensGen k n = do
  wsg <- wordsG
  withSystemRandom $ \gen -> do
    samples n (choose (numbersSG gen) wsg k) gen
    -- p <- bernoulli k
    


sampleWords :: Int -> IO [String]
sampleWords n = do
  pp <- wordsG
  withSystemRandom $ \gen -> do
    samples n pp gen

-- tokens :: Gen s -> Double -> IO (Prob (ST s) String)
-- tokens gen k = do
--   ws <- wordsG
--   pure $ choose (numbersSG gen) ws k

choose :: PrimMonad m => Prob m b -> Prob m b -> Double -> Prob m b
choose gen1 gen2 prop = do
  p <- bernoulli prop
  if p
    then gen1
    else gen2


numbersSG :: PrimMonad m => Gen (PrimState m) -> Prob m String
numbersSG gen = show <$> numbersG gen

numbersG :: (PrimMonad m) => Gen (PrimState m) -> Prob m Int
numbersG = uniformRM (1 :: Int , 9999)


wordsG :: IO (Prob (ST s) String)
wordsG = do
  ws <- wordsEn
  pure $ discreteUniform ws

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
    where go [] acc = [acc]
          go (y : ys) acc = if x == y
                            then reverse acc : go ys []
                            else go ys (y : acc)

wordsEn :: IO [String]
wordsEn = splitOn '\n' <$> readFile "src/words-en.txt"
