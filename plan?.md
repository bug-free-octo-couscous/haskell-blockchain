Good read on both files. Here's a comprehensive breakdown of what to improve to bring this to production level, organized by priority:

---

## 🔴 Critical Issues

### 1. `last` / `++` on lists — use `Data.Sequence` or a proper chain structure
`last chain` is O(n) and `chain ++ [newBlock]` is O(n). A production chain should use a **finger tree** (`Data.Sequence`) or store the tip separately.

```haskell
data Blockchain = Blockchain
  { chainBlocks :: Seq Block
  , chainTip    :: Block   -- O(1) access to latest
  }
```

### 2. Mining is single-threaded and blocks the main thread
`mineBlock` is a tight pure loop with no concurrency. Use `async` + `STM` so mining can be cancelled, parallelised, or interrupted by a new block from the network.

```haskell
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
```

### 3. `difficulty` is a global constant — should be dynamic
Real chains do **difficulty retargeting** based on average block time. At minimum, pass it as a parameter through your monad stack rather than a top-level constant.

---

## 🟠 Security / Correctness

### 4. Hash input is unstructured string concatenation
`show index ++ timestamp ++ dat ++ prevHash ++ show nonce` can collide (e.g. index `12`, data `"3foo"` vs index `1`, data `"23foo"`). Use a **canonical serialisation** like `binary`, `cereal`, or CBOR:

```haskell
import Data.Binary (encode)
-- or use a length-prefixed format
```

### 5. `Hash = String` — use `newtype` over `ByteString`
`String` is a list of `Char` (very slow). Store hashes as raw `ByteString` or `Digest SHA256`:

```haskell
import Crypto.Hash (Digest, SHA256, hash)
newtype BlockHash = BlockHash (Digest SHA256) deriving (Show, Eq)
```

### 6. No replay protection / no Merkle tree
`blockData :: String` means only one transaction per block. Production chains use a **Merkle tree** of transactions for efficient verification and SPV proofs.

### 7. Timestamp is not validated
A block's timestamp should be checked to be ≥ previous block's timestamp and not too far in the future (to prevent time-warp attacks).

---

## 🟡 Architecture

### 8. Missing error handling — no `Either`/`ExceptT`
`addBlock` silently calls `last []` on an empty chain (runtime exception). Use:

```haskell
addBlock :: Blockchain -> String -> ExceptT BlockchainError IO Blockchain
data BlockchainError = EmptyChain | InvalidParent | HashMismatch
```

### 9. No persistence
Blocks live only in memory. Add serialisation (`aeson` or `binary`) and write to disk or a database (`sqlite-simple`, `postgresql-simple`).

### 10. No networking layer
A real node needs a P2P layer. Consider `network-simple` or `conduit` for TCP, and a simple gossip protocol for block/tx propagation.

### 11. `isValidChain` doesn't re-derive hashes
The validator only checks `prevHash` linkage and the leading-zero property, but never **recomputes** the hash from block contents. A tampered block with a faked hash string would pass validation.

```haskell
-- Add this check:
blockHash b == calculateHash (blockIndex b) ... (blockNonce b)
```

---

## 🟢 Code Quality

### 12. Logging — replace `putStrLn` with `katip` or `fast-logger`

```haskell
import Katip
logMsg ns InfoS "Mining genesis block"
```

### 13. Configuration — replace hardcoded values with a config file

Use `optparse-applicative` for CLI args and `dhall` or `aeson` for a config file (difficulty, port, data directory).

### 14. Testing — add `QuickCheck` properties

```haskell
prop_validChainAfterAdd :: [String] -> Property
prop_validChainAfterAdd txs = monadicIO $ do
  chain <- run $ buildChain txs
  assert (isValidChain chain)
```

### 15. Minor bugs in `Main.hs`
- Block 2 is added to `chain1` not `chain2` (typo creates a fork)
- `isValidChain chain2` validates the wrong (shorter) chain — should be `chain3`

---

## Suggested Stack for Production

| Concern | Library |
|---|---|
| Hashing | `cryptonite` |
| Serialisation | `binary` / `aeson` |
| Concurrency | `async` + `stm` |
| Persistence | `sqlite-simple` |
| Logging | `katip` |
| CLI config | `optparse-applicative` |
| Testing | `hspec` + `QuickCheck` |
| Networking | `conduit-network` |

---

The two most impactful immediate fixes are **#11** (re-derive hashes in validation — it's a correctness bug) and **#4** (canonical serialisation — it's a security bug). Everything else layers on top of a correct foundation.