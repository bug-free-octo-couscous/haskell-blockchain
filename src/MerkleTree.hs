module MerkleTree
  ( MerkleTree
  , buildMerkleTree
  , merkleRoot
  , emptyMerkleRoot
  ) where

import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A binary Merkle tree of SHA-256 digests.
data MerkleTree
  = MerkleLeaf (Digest SHA256)
  | MerkleNode (Digest SHA256) MerkleTree MerkleTree
  deriving (Show, Eq)

-- | Extract the digest stored at any node.
nodeDigest :: MerkleTree -> Digest SHA256
nodeDigest (MerkleLeaf d)     = d
nodeDigest (MerkleNode d _ _) = d

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Build a Merkle tree from a non-empty list of leaf digests.
-- Odd-length levels duplicate the last node (Bitcoin convention).
buildMerkleTree :: [Digest SHA256] -> Maybe MerkleTree
buildMerkleTree [] = Nothing
buildMerkleTree leaves = Just (buildLevel (map MerkleLeaf leaves))
  where
    buildLevel :: [MerkleTree] -> MerkleTree
    buildLevel [t] = t
    buildLevel ts  = buildLevel (pairUp ts)

    pairUp :: [MerkleTree] -> [MerkleTree]
    pairUp []       = []
    pairUp [t]      = [combineNodes t t]   -- duplicate odd node
    pairUp (a:b:ts) = combineNodes a b : pairUp ts

    combineNodes :: MerkleTree -> MerkleTree -> MerkleTree
    combineNodes l r =
      let lBytes = BC.pack (show (nodeDigest l))
          rBytes = BC.pack (show (nodeDigest r))
          d      = hash (lBytes <> rBytes) :: Digest SHA256
      in MerkleNode d l r

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

-- | Extract the root digest from a tree.
merkleRoot :: MerkleTree -> Digest SHA256
merkleRoot = nodeDigest

-- | Conventional empty-block Merkle root: hash of 32 zero bytes.
emptyMerkleRoot :: Digest SHA256
emptyMerkleRoot = hash (BS.replicate 32 0 :: BS.ByteString)