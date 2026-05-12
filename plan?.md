## 🟡 Architecture

### 8. Missing error handling — no `Either`/`ExceptT`
`addBlock` silently calls `last []` on an empty chain (runtime exception). Use:

```haskell
addBlock :: Blockchain -> String -> ExceptT BlockchainError IO Blockchain
data BlockchainError = EmptyChain | InvalidParent | HashMismatch
```

### 9. No persistence - do it later
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
