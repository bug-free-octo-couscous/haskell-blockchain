module Main where

import Blockchain

main :: IO ()
main = do
  -- Start with genesis block
  genesis <- createGenesisBlock
  let chain0 = [genesis]

  -- Add some blocks
  chain1 <- addBlock chain0 "Alice sends 10 coins to Bob"
  chain2 <- addBlock chain1 "Bob sends 5 coins to Carol"
  chain3 <- addBlock chain2 "Carol sends 2 coins to Dave"

  -- Print all blocks
  mapM_ printBlock chain3

  -- Validate
  putStrLn $ "\nChain valid? " ++ show (isValidChain chain3)

printBlock :: Block -> IO ()
printBlock b = do
  putStrLn $ replicate 50 '-'
  putStrLn $ "Index:     " ++ show (blockIndex b)
  putStrLn $ "Data:      " ++ blockData b
  putStrLn $ "Prev Hash: " ++ blockPrevHash b
  putStrLn $ "Hash:      " ++ blockHash b