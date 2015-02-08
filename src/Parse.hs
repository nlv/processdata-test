{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import Data
import Test.QuickCheck()

import System.IO()

import Control.Applicative
import Control.Monad.Trans.Either
import PipesCerealPlus 

import qualified Data.ByteString as B

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Extras as PE
import Data.Text
import Data.Int
import qualified Data.HashMap.Strict as Hash

import qualified Control.Foldl as Fold

desPacket :: Pipe B.ByteString Packet (EitherT Text IO) ()
desPacket = deserializingPipe 

f :: Fold.Fold Packet (Hash.HashMap Packet Int)
f = Fold.Fold (\h a -> Hash.insertWith (+) a 1 h) Hash.empty id

f2 :: Fold.Fold Packet ((Hash.HashMap Packet Int), Int)
f2 = (,) <$> f <*> Fold.length




main :: IO ()
main = do
  {-
  res' <- runEitherT $ PE.fold f2 $ (PB.stdin >-> desPacket)
  either print printRes res'
  -}
  res' <- runEitherT $ PE.fold f $ (PB.stdin >-> desPacket)
  either print (printRes' . Hash.toList) res'

printRes :: ((Hash.HashMap Packet Int), Int) -> IO ()
printRes (h, c) = do
  putStrLn $ "Count = " ++ (show c)
  let l = Hash.toList h
  let cntrl = sum $ Prelude.map snd l
  putStrLn $ "Control count: " ++ (show cntrl)
  printRes' l

printRes' [] = return ()
printRes' (r:rs) = print r >> printRes' rs
