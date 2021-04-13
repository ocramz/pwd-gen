module Main where

import Control.Monad (replicateM, void)
import Lib (pwdGen)

import Options.Applicative (Parser, execParser, info, (<**>), helper, fullDesc, progDesc, header, option, auto, long, short, showDefault, value)

main :: IO ()
main = do
  let opts = info (params <**> helper) (fullDesc <> progDesc "generate a few password")
  (Params n k m) <- execParser opts
  ws <- replicateM m (pwdGen k n)
  void $ traverse putStrLn ws


data Params =
  Params {
  pLength :: Int
  , pBernParam :: Double
  , pNumSamples :: Int
         }

params :: Parser Params
params = Params <$>
  lengthP <*>
  bernParamP <*>
  samplesP

bernParamP :: Parser Double
bernParamP = option auto (
  long "bernoulli-param" <>
  short 'p' <>
  value 0.5 <>
  showDefault
                         )

samplesP :: Parser Int
samplesP =
  option auto (
    long "length" <>
    short 'm' <>
    value 10 <>
    showDefault
              )

lengthP :: Parser Int
lengthP =
  option auto (
    long "length" <>
    short 'n' <>
    value 5 <>
    showDefault
              )
