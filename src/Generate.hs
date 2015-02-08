{-# LANGUAGE OverloadedStrings #-}
import Data

import Test.QuickCheck


import qualified Data.Serialize as Ser

import Control.Monad (forever)

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB

genPacket :: IO Packet
genPacket = generate arbitrary

producePackets :: (Functor m, MonadIO m) => Producer Packet m ()
producePackets = forever (liftIO genPacket >>= yield) 

limitKilo :: Int
limitKilo = 100
limit :: Int
limit = limitKilo * 1000 * 1000
--limit = limitKilo

limitedCount :: (Functor m, MonadIO m) => Int -> m ()
limitedCount l = runEffect $ producePackets >-> P.take l >-> P.map Ser.encode >-> PB.stdout

main :: IO ()
main = limitedCount limit
