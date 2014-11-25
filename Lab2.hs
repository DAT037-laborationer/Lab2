import Control.Applicative
import System.Environment
import System.IO
import PriorityQueue

-- | Bids.

data Trader = Trader String TradeType Integer Integer
data TradeType = Ask | Bid
  deriving (Eq)

--history :: [String] -> String -> String -> Integer -> [String]
--history historyBook buyer seller price
--  = historyBook ++ (buyer ++ " buys from " ++ seller ++ " for "
--                    ++ show price ++ "kr.") 

-- | Parses a bid. Incorrectly formatted bids are returned verbatim
-- (tagged with 'Left').

parseBid :: String -> Either String Trader
parseBid s = case words s of
  name : kind : prices ->
    case (kind, mapM readInteger prices) of
      ("K",  Just [price])              -> Right $ Trader name Bid price price
      ("S",  Just [price])              -> Right $ Trader name Ask price price
      ("NK", Just [oldPrice, newPrice]) -> Right $ Trader name Bid oldPrice newPrice
      ("NS", Just [oldPrice, newPrice]) -> Right $ Trader name Ask oldPrice newPrice
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

parseBids :: String -> IO [Trader]
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

trade :: [Trader] -> IO ()
trade bids = do
  let (askT,bidT) = transactions bids
  putStr $ deals askT bidT
  
deals :: BinHeap Trade -> BinHeap Trade -> String
deals b1 b2 = "Orderbok:"

transactions :: [Trader] -> (BinHeap Trade, BinHeap Trade)
transactions bids = t' (reverse bids)
  where
t' [] = (emptyQueue,emptyQueue)
t' ((Trader name tt old new):[])
  | old == new = case tt of
      Ask -> (addAsk (new,name) emptyQueue, emptyQueue)
      Bid -> (emptyQueue, addBid (new,name) emptyQueue)
  | otherwise  = t' []
t' (bid@(Trader name tt old new):bids) 
  | old == new = case tt of
      Ask -> (merge (<) ((fst . t') [bid]) a, b)
      Bid -> (a, merge (>) ((snd . t') [bid]) b)
  | otherwise  = case tt of
      Ask -> (update (<) (new,name) a, b)
      Bid -> (a, update (>) (new,name) b)
  where (a,b) = t' bids
 
{-  
update :: [Trade] -> PriQue a -> PriQue a -> PriQue a
update ((Trade n tt op np):bs) p1 p2 =
  case tt of
    Ask -> case op == np  of
      True  -> addAsk op p1 
      False -> decreaseKey 
    Bid -> case op == np of
      True  -> addBid op p2
      False -> increaseKey 
-}
