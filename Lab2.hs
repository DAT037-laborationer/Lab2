import Control.Applicative
import System.Environment
import System.IO
import PriorityQueue

-- | Bids.

data Bid = Bid String Integer

history :: [String] -> String -> String -> Integer -> [String]
history historyBook buyer seller price
  = historyBook ++ (buyer ++ " buys from " ++ seller ++ " for "
                    ++ show price ++ "kr.") 

-- | Parses a bid. Incorrectly formatted bids are returned verbatim
-- (tagged with 'Left').

parseBid :: String -> Either String Bid
parseBid s = case words s of
  name : kind : prices ->
    case (kind, mapM readInteger prices) of
      ("K",  Just [price])              -> Right $ addBid price sellers
      ("S",  Just [price])              -> Right $ addAsk price sellers  
      ("NK", Just [oldPrice, newPrice]) -> Right $ update oldPrice
                                                          newPrice
                                                          sellers
                                                          isBid
      ("NS", Just [oldPrice, newPrice]) -> Right $ update oldPrice
                                                          newPrice
                                                          buyers
                                                          isBid
      _ -> Left s
  _ -> Left s
  where
  readInteger :: String -> Maybe Integer
  readInteger s = case filter (null . snd) $ reads s of
    [(x, _)] -> Just x
    _        -> Nothing

-- | Parses a sequence of bids. Correctly formatted bids are returned
-- (in the order encountered), and an error message is printed for
-- each incorrectly formatted bid.

parseBids :: String -> IO [Bid]
parseBids s = concat <$> mapM (check . parseBid) (lines s)
  where
  check (Left bid) = do
    hPutStrLn stderr $ "Malformed bid: " ++ bid
    return []
  check (Right bid) = return [bid]

-- | ...

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> process stdin
    [f] -> process =<< openFile f ReadMode
    _   -> hPutStr stderr $ unlines
      [ "Usage: ./Lab2 [<file>]"
      , "If no file is given, then input is read from standard input."
      ]
  where
  process h = trade =<< parseBids =<< hGetContents h

-- | ...

trade :: [Bid] -> IO ()
trade = undefined
