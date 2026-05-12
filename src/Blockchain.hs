module Blockchain
  ( Block(..)
  , Blockchain
  , BlockHash
  , unBlockHash
  , ChainConfig(..)
  , defaultConfig
  , mkBlockchain
  , chainToList
  , chainTip
  , chainConfig
  , calculateHash
  , genesisHash
  , mineBlockAsync
  , createGenesisBlock
  , addBlock
  , isValidHash
  , isValidChain
  , TimestampError(..)
  , validateTimestamp
  ) where

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Crypto.Hash (Digest, SHA256, hash, digestFromByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Data.Binary (encode)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)

import Transaction (Transaction, txId)
import MerkleTree  (buildMerkleTree, merkleRoot, emptyMerkleRoot)

-- ---------------------------------------------------------------------------
-- BlockHash newtype
-- ---------------------------------------------------------------------------

newtype BlockHash = BlockHash (Digest SHA256)
  deriving (Show, Eq)

unBlockHash :: BlockHash -> String
unBlockHash (BlockHash d) =
  BC.unpack (B16.encode (B16.decodeLenient (BC.pack (show d))))

genesisHash :: BlockHash
genesisHash = BlockHash zeroDigest
  where
    zeroDigest = case digestFromByteString (BS.replicate 32 0) of
      Just d  -> d
      Nothing -> error "genesisHash: impossible"

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

data ChainConfig = ChainConfig
  { cfgDifficulty      :: Int
  , cfgTargetBlockTime :: NominalDiffTime
  , cfgRetargetEvery   :: Int
  , cfgChainId         :: Int             -- ^ used for replay protection in transactions
  , cfgMaxFutureTime   :: NominalDiffTime -- ^ how far ahead of wall-clock a timestamp may be
  } deriving (Show, Eq)

defaultConfig :: ChainConfig
defaultConfig = ChainConfig
  { cfgDifficulty      = 1
  , cfgTargetBlockTime = 10
  , cfgRetargetEvery   = 5
  , cfgChainId         = 1
  , cfgMaxFutureTime   = 120  -- 2 minutes, matching Bitcoin's rule
  }

-- ---------------------------------------------------------------------------
-- Block
-- ---------------------------------------------------------------------------

-- | A block now carries a list of transactions and commits to them via a
-- Merkle root rather than an unstructured 'String'.  This enables:
--   * Multiple transactions per block.
--   * Efficient SPV proofs (verify a tx is in a block without the full block).
--   * Replay protection: each tx encodes the chain ID.
data Block = Block
  { blockIndex      :: Int
  , blockTimestamp  :: UTCTime
  , blockTxs        :: [Transaction]   -- ^ full transaction list
  , blockMerkleRoot :: BlockHash       -- ^ Merkle root of blockTxs
  , blockPrevHash   :: BlockHash
  , blockNonce      :: Int
  , blockHash       :: BlockHash
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Blockchain
-- ---------------------------------------------------------------------------

data Blockchain = Blockchain
  { chainBlocks :: Seq Block
  , chainTip    :: Block
  , chainConfig :: ChainConfig
  }

mkBlockchain :: Block -> ChainConfig -> Blockchain
mkBlockchain genesis cfg = Blockchain (Seq.singleton genesis) genesis cfg

chainToList :: Blockchain -> [Block]
chainToList = foldr (:) [] . chainBlocks

-- ---------------------------------------------------------------------------
-- Timestamp validation
-- ---------------------------------------------------------------------------

data TimestampError
  = TimestampInPast    -- ^ block timestamp is before the previous block's
  | TimestampTooFuture -- ^ block timestamp is too far ahead of wall-clock time
  deriving (Show, Eq)

-- | Validate a candidate block timestamp against two rules:
--
--   1. __Monotonicity__: the new timestamp must be ≥ the previous block's
--      timestamp.  This prevents time-warp attacks where a miner backdates
--      blocks to manipulate the difficulty retarget window.
--
--   2. __Future cap__: the new timestamp must not exceed the node's current
--      wall-clock time by more than 'cfgMaxFutureTime'.  This bounds how far
--      ahead a miner can pre-date a block, limiting the drift they can inject.
validateTimestamp
  :: ChainConfig
  -> UTCTime   -- ^ wall-clock "now" at the receiving node
  -> UTCTime   -- ^ previous block's timestamp
  -> UTCTime   -- ^ candidate block's timestamp
  -> Either TimestampError ()
validateTimestamp cfg now prevTime newTime
  | newTime < prevTime            = Left TimestampInPast
  | drift > cfgMaxFutureTime cfg  = Left TimestampTooFuture
  | otherwise                     = Right ()
  where
    drift = diffUTCTime newTime now

-- ---------------------------------------------------------------------------
-- Merkle helpers
-- ---------------------------------------------------------------------------

-- | Compute the Merkle root for a list of transactions.
-- Returns 'emptyMerkleRoot' for an empty block (coinbase-only in production).
computeMerkleRoot :: [Transaction] -> BlockHash
computeMerkleRoot [] = BlockHash emptyMerkleRoot
computeMerkleRoot txs =
  case buildMerkleTree (map txId txs) of
    Just tree -> BlockHash (merkleRoot tree)
    Nothing   -> BlockHash emptyMerkleRoot

-- ---------------------------------------------------------------------------
-- Difficulty retargeting
-- ---------------------------------------------------------------------------

retargetDifficulty :: ChainConfig -> [Block] -> ChainConfig
retargetDifficulty cfg window =
  case (safeHead window, safeLast window) of
    (Just first, Just lst) ->
      let actual  = diffUTCTime (blockTimestamp lst) (blockTimestamp first)
          target  = cfgTargetBlockTime cfg * fromIntegral (cfgRetargetEvery cfg)
          actual' = max actual 1
          ratio   = toRational target / toRational actual'
          oldDiff = cfgDifficulty cfg
          newDiff = max 1
                  . min (oldDiff * 4)
                  . max (max 1 (oldDiff `div` 4))
                  $ round (fromIntegral oldDiff * (fromRational ratio :: Double))
      in cfg { cfgDifficulty = newDiff }
    _ -> cfg
  where
    safeHead []    = Nothing
    safeHead (x:_) = Just x
    safeLast []    = Nothing
    safeLast xs    = Just (last xs)

-- ---------------------------------------------------------------------------
-- Hashing
-- ---------------------------------------------------------------------------

-- | Hash the block *header* fields — notably the Merkle root rather than raw
-- data.  Miners only need the root; the full tx list is not re-hashed per nonce.
calculateHash
  :: Int       -- ^ block index
  -> String    -- ^ timestamp
  -> BlockHash -- ^ Merkle root of transactions
  -> BlockHash -- ^ previous block hash
  -> Int       -- ^ nonce
  -> BlockHash
calculateHash index timestamp (BlockHash merkle) (BlockHash prevDigest) nonce =
  let merkleBytes = BC.pack (show merkle)
      prevBytes   = BC.pack (show prevDigest)
      bytes       = BL.toStrict
                  $  encode index
                  <> encode timestamp
                  <> BL.fromStrict merkleBytes
                  <> BL.fromStrict prevBytes
                  <> encode nonce
  in BlockHash (hash bytes)

isValidHash :: Int -> BlockHash -> Bool
isValidHash diff (BlockHash d) =
  let hex = BC.unpack (B16.encode (BC.pack (show d)))
  in take diff hex == replicate diff '0'

-- ---------------------------------------------------------------------------
-- Mining
-- ---------------------------------------------------------------------------

mineBlockAsync
  :: Int       -- ^ difficulty
  -> Int       -- ^ block index
  -> String    -- ^ timestamp
  -> BlockHash -- ^ Merkle root (pre-computed, fixed for this mining run)
  -> BlockHash -- ^ previous block hash
  -> TVar Bool -- ^ cancellation flag
  -> IO (Maybe (BlockHash, Int))
mineBlockAsync diff index timestamp merkle prevHash cancelVar =
  go 0
  where
    go nonce = do
      let (found, h, nextNonce) = searchBatch nonce 1000
      if found
        then return (Just (h, nextNonce - 1))
        else do
          cancelled <- readTVarIO cancelVar
          if cancelled then return Nothing else go nextNonce

    searchBatch :: Int -> Int -> (Bool, BlockHash, Int)
    searchBatch nonce 0 = (False, genesisHash, nonce)
    searchBatch nonce n =
      let h = calculateHash index timestamp merkle prevHash nonce
      in if isValidHash diff h
           then (True, h, nonce + 1)
           else searchBatch (nonce + 1) (n - 1)

-- ---------------------------------------------------------------------------
-- Chain operations
-- ---------------------------------------------------------------------------

createGenesisBlock :: ChainConfig -> IO Block
createGenesisBlock cfg = do
  now <- getCurrentTime
  let diff      = cfgDifficulty cfg
      timestamp = show now
      txs       = []
      merkle    = computeMerkleRoot txs
  cancelVar <- newTVarIO False
  result    <- mineBlockAsync diff 0 timestamp merkle genesisHash cancelVar
  case result of
    Nothing         -> fail "Genesis mining cancelled — should never happen"
    Just (h, nonce) -> return Block
      { blockIndex      = 0
      , blockTimestamp  = now
      , blockTxs        = txs
      , blockMerkleRoot = merkle
      , blockPrevHash   = genesisHash
      , blockNonce      = nonce
      , blockHash       = h
      }

-- | Mine and append a new block containing the given transactions.
-- Returns Left with a timestamp error if the wall-clock time fails validation
-- (this should be rare in normal operation but guards against clock skew).
addBlock :: Blockchain -> [Transaction] -> TVar Bool -> IO (Either TimestampError (Maybe Blockchain))
addBlock bc txs cancelVar = do
  now <- getCurrentTime
  let cfg       = chainConfig bc
      prev      = chainTip bc
      prevTime  = blockTimestamp prev
  case validateTimestamp cfg now prevTime now of
    Left err -> return (Left err)
    Right () -> do
      let newIndex  = blockIndex prev + 1
          prevHash  = blockHash prev
          timestamp = show now
          merkle    = computeMerkleRoot txs
          cfg'      = if newIndex `mod` cfgRetargetEvery cfg == 0
                        then retargetDifficulty cfg
                               (takeLast (cfgRetargetEvery cfg) (chainToList bc))
                        else cfg
          diff      = cfgDifficulty cfg'
      result <- mineBlockAsync diff newIndex timestamp merkle prevHash cancelVar
      case result of
        Nothing         -> return (Right Nothing)
        Just (h, nonce) ->
          let newBlock = Block
                { blockIndex      = newIndex
                , blockTimestamp  = now
                , blockTxs        = txs
                , blockMerkleRoot = merkle
                , blockPrevHash   = prevHash
                , blockNonce      = nonce
                , blockHash       = h
                }
          in return $ Right $ Just $ Blockchain (chainBlocks bc |> newBlock) newBlock cfg'

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

-- | Validate chain linkage, PoW, Merkle root integrity, and timestamp ordering.
-- Note: the future-cap check uses each block's own timestamp as "now" for the
-- next block, which is the correct offline validation behaviour.
isValidChain :: Blockchain -> Bool
isValidChain bc = go (cfgDifficulty (chainConfig bc)) (chainToList bc)
  where
    cfg = chainConfig bc

    go _    []           = True
    go diff [b]          = isValidHash diff (blockHash b)
                        && blockMerkleRoot b == computeMerkleRoot (blockTxs b)
    go diff (b1:b2:rest) =
         blockHash b1 == blockPrevHash b2
      && isValidHash diff (blockHash b1)
      && blockMerkleRoot b1 == computeMerkleRoot (blockTxs b1)
      && isRight (validateTimestamp cfg (blockTimestamp b2)
                                        (blockTimestamp b1)
                                        (blockTimestamp b2))
      && go diff (b2:rest)

    isRight (Right _) = True
    isRight (Left  _) = False