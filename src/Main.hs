-- app/Main.hs
module Main where

import Blockchain

main :: IO ()
main = do
  putStrLn "Mining genesis block..."
  genesis <- createGenesisBlock

  putStrLn "Mining block 1..."
  chain1 <- addBlock [genesis] "Alice sends 10 coins to Bob"

  putStrLn "Mining block 2..."
  chain2 <- addBlock chain1 "Bob sends 5 coins to Carol"

  chain3 <- addBlock chain2 "jihoo taken 1 coins from Carol"
  
  mapM_ printBlock chain3
  putStrLn $ "\nChain valid? " ++ show (isValidChain chain2)

printBlock :: Block -> IO ()
printBlock b = do
  putStrLn $ replicate 50 '-'
  putStrLn $ "Index:     " ++ show (blockIndex b)
  putStrLn $ "Data:      " ++ blockData b
  putStrLn $ "Nonce:     " ++ show (blockNonce b)   -- NEW
  putStrLn $ "Prev Hash: " ++ blockPrevHash b
  putStrLn $ "Hash:      " ++ blockHash b