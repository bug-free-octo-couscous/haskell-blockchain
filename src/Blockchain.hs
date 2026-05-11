module Blockchain where

import Data.Time.Clock (UTCTime, getCurrentTime)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16


type Hash = String

data Block = Block
  { blockIndex     :: Int
  , blockTimestamp :: UTCTime
  , blockData      :: String
  , blockPrevHash  :: Hash
  , blockHash      :: Hash
  } deriving (Show, Eq)

type Blockchain = [Block]

calculateHash :: Int -> String -> String -> String -> Hash
calculateHash index timestamp dat prevHash = 
    let content = show index ++ timestamp ++ dat ++ prevHash
        bytes = BC.pack content
        hashed = hash bytes
    in BC.unpack (B16.encode hashed)
    
createGenesisBlock :: IO Block
createGenesisBlock = do
    now <- getCurrentTime
    let timestamp = show now
        prevHash = "0"
        h = calculateHash 0 timestamp "Genesis Block" prevHash
    return $ Block
      { blockIndex = 0
      , blockTimestamp = now
      , blockData = "Genesis Block"
      , blockPrevHash = prevHash
      , blockHash = h
      }

addBlock :: Blockchain -> String -> IO Blockchain
addBlock chain dat = do
    now <- getCurrentTime
    let prevBlock = last chain
        newIndex = blockIndex prevBlock + 1
        prevHash = blockHash prevBlock
        timestamp = show now
        h = calculateHash newIndex timestamp dat prevHash
        newBlock = Block
          { blockIndex = newIndex
          , blockTimestamp = now
          , blockData = dat
          , blockPrevHash = prevHash
          ,blockHash = h
          }
    return (chain ++ [newBlock])

isValidChain :: Blockchain -> Bool
isValidChain [] = True
isValidChain [_] = True
isValidChain (b1:b2:rest) = blockHash b1 == blockPrevHash b2 && isValidChain (b2:rest)
