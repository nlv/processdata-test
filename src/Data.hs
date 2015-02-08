{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric #-}
module Data ( 
  Protocol (..),
  Address  (..),
  Port,
  Packet   (..)
  ) where

import Data.Word

import Data.Serialize 
import CerealPlus.Serializable 
import CerealPlus.Deserialize
import qualified CerealPlus.Serialize as Ser

import Test.QuickCheck
import Test.QuickCheck.Instances()

import Control.Applicative

import GHC.Generics (Generic)
import Data.Hashable

data Protocol = IP | TCP | UDP deriving (Eq, Generic, Show)

instance Hashable Protocol

data Address = Address Word8 Word8 Word8 Word8 deriving (Eq, Generic, Show)

instance Hashable Address

type Port = Word16

data Packet = Packet {
  packProtocol :: Protocol,
  srcAddress   :: Address,
  srcPort      :: Port,
  dstAddress   :: Address,
  dstPort      :: Port
} deriving (Eq, Generic, Show)

instance Hashable Packet

instance Serialize Protocol where
  put IP  = put (0::Word8)
  put TCP = put (1::Word8)
  put UDP = put (2::Word8)

  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> return IP
      1 -> return TCP
      2 -> return UDP
      _ -> error $ "cannot parse Protocol: illegal tag: " ++ (show tag)

instance Serialize Address where
  put (Address a1 a2 a3 a4) = put a1 >> put a2 >> put a3 >> put a4
  get = Address <$> get <*> get <*> get <*> get 

instance Serialize Packet where
  put (Packet p sa sp da dp ) = put p >> put sa >> put sp >> put da >> put dp
  get = Packet <$> get <*> get <*> get <*> get <*> get

instance Arbitrary Protocol where
  arbitrary = frequency [(50, return IP), (30, return UDP), (40, return TCP)]

instance Arbitrary Address where
--  arbitrary = Address <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  arbitrary = Address <$> elements [193, 192] <*> return 168 <*> return 0 <*> elements [0, 1]

instance Arbitrary Packet where
  -- arbitrary = Packet <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  arbitrary = Packet <$> arbitrary <*> arbitrary <*> elements [80, 22] <*> arbitrary <*> elements [108]
                     
instance Serializable m Packet where
  serialize a = Ser.liftPut $ put a
  deserialize = liftGet get

