module Transaction
  ( TxId
  , Transaction(..)
  , txId
  ) where

import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Binary (encode)

-- | A transaction ID is a SHA-256 digest of the transaction's canonical encoding.
type TxId = Digest SHA256

-- | A minimal transaction type.
-- 'txChainId' provides replay protection: a transaction signed for chain A
-- is rejected on chain B because its serialisation differs.
data Transaction = Transaction
  { txChainId  :: Int     -- ^ replay-protection tag; differs per chain/network
  , txSender   :: String  -- ^ sender identifier (pubkey in a real system)
  , txReceiver :: String  -- ^ receiver identifier
  , txAmount   :: Int     -- ^ amount transferred
  , txNonce    :: Int     -- ^ per-sender sequence number, prevents tx replay
  } deriving (Show, Eq)

-- | Compute the canonical ID of a transaction.
-- Uses length-prefixed binary encoding (Data.Binary) so no two distinct
-- transactions can produce the same byte sequence.
txId :: Transaction -> TxId
txId tx =
  let bytes = BL.toStrict
            $  encode (txChainId  tx)
            <> encode (txSender   tx)
            <> encode (txReceiver tx)
            <> encode (txAmount   tx)
            <> encode (txNonce    tx)
  in hash bytes