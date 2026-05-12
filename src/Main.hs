-- app/Main.hs
module Main where

import Blockchain
import Transaction
import Control.Concurrent.STM (newTVarIO)

main :: IO ()
main = do
  putStrLn "Mining genesis block..."
  let cfg    = defaultConfig
  genesis   <- createGenesisBlock cfg
  let chain0 = mkBlockchain genesis cfg
      cid    = cfgChainId cfg

  chain1 <- mineNext chain0
    [ Transaction cid "Alice" "Bob"   10 0
    , Transaction cid "Bob"   "Carol"  3 0
    ]

  chain2 <- mineNext chain1
    [ Transaction cid "Carol" "Jihoo"  1 0
    , Transaction cid "Alice" "Jihoo"  5 1   -- Alice's second tx: nonce=1
    ]

  chain3 <- mineNext chain2
    [ Transaction cid "Jihoo" "Alice"  2 0 ]

  mapM_ printBlock (chainToList chain3)
  putStrLn $ "\nChain valid?      " ++ show (isValidChain chain3)
  putStrLn $ "Final difficulty: " ++ show (cfgDifficulty (chainConfig chain3))

mineNext :: Blockchain -> [Transaction] -> IO Blockchain
mineNext bc txs = do
  let idx = blockIndex (chainTip bc) + 1
  putStrLn $ "Mining block " ++ show idx
          ++ " (difficulty " ++ show (cfgDifficulty (chainConfig bc))
          ++ ", " ++ show (length txs) ++ " tx)..."
  cancelVar <- newTVarIO False
  result    <- addBlock bc txs cancelVar
  case result of
    Left  err        -> fail $ "Timestamp error: " ++ show err
    Right Nothing    -> fail "Mining was cancelled"
    Right (Just chain) -> return chain

printBlock :: Block -> IO ()
printBlock b = do
  putStrLn $ replicate 50 '-'
  putStrLn $ "Index:       " ++ show (blockIndex b)
  putStrLn $ "Merkle root: " ++ unBlockHash (blockMerkleRoot b)
  putStrLn $ "Prev Hash:   " ++ unBlockHash (blockPrevHash b)
  putStrLn $ "Hash:        " ++ unBlockHash (blockHash b)
  putStrLn $ "Nonce:       " ++ show (blockNonce b)
  putStrLn   "Transactions:"
  mapM_ printTx (blockTxs b)

printTx :: Transaction -> IO ()
printTx tx =
  putStrLn $ "  [chain=" ++ show (txChainId tx)
          ++ " nonce=" ++ show (txNonce tx)
          ++ "] " ++ txSender tx
          ++ " -> " ++ txReceiver tx
          ++ " : " ++ show (txAmount tx)