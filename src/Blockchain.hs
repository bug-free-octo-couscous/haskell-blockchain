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
  , blockNonce     :: Int
  , blockHash      :: Hash
  } deriving (Show, Eq)

type Blockchain = [Block]

difficulty :: Int
difficulty = 1 -- should be 4 or higher

calculateHash :: Int -> String -> String -> String -> Int -> Hash
calculateHash index timestamp dat prevHash nonce =
  let content = show index ++ timestamp ++ dat ++ prevHash ++ show nonce
      bytes   = BC.pack content
      hashed  = hash bytes
  in BC.unpack (B16.encode hashed)

-- No more shadowing, no more head
mineBlock :: Int -> Int -> String -> String -> String -> (Hash, Int)
mineBlock diff index timestamp dat prevHash =
  go 0
  where
    go nonce
      | take diff h == replicate diff '0' = (h, nonce)
      | otherwise                          = go (nonce + 1)
      where
        h = calculateHash index timestamp dat prevHash nonce

createGenesisBlock :: IO Block
createGenesisBlock = do
  now <- getCurrentTime
  let timestamp  = show now
      prevHash   = "0"
      dat        = "Genesis Block"
      (h, nonce) = mineBlock difficulty 0 timestamp dat prevHash
  return $ Block
    { blockIndex     = 0
    , blockTimestamp = now
    , blockData      = dat
    , blockPrevHash  = prevHash
    , blockNonce     = nonce
    , blockHash      = h
    }

addBlock :: Blockchain -> String -> IO Blockchain
addBlock chain dat = do
  now <- getCurrentTime
  let prevBlock  = last chain
      newIndex   = blockIndex prevBlock + 1
      prevHash   = blockHash prevBlock
      timestamp  = show now
      (h, nonce) = mineBlock difficulty newIndex timestamp dat prevHash
      newBlock   = Block
        { blockIndex     = newIndex
        , blockTimestamp = now
        , blockData      = dat
        , blockPrevHash  = prevHash
        , blockNonce     = nonce
        , blockHash      = h
        }
  return (chain ++ [newBlock])

isValidHash :: Hash -> Bool
isValidHash h = take difficulty h == replicate difficulty '0'

isValidChain :: Blockchain -> Bool
isValidChain []         = True
isValidChain [b]        = isValidHash (blockHash b)
isValidChain (b1:b2:rest) =
     blockHash b1 == blockPrevHash b2
  && isValidHash (blockHash b1)
  && isValidChain (b2:rest)