import Control.Applicative
import System.Environment
import System.IO
import PriorityQueue

-- | Datatype for a trader.
data Trader = Trader String TradeType Integer Integer
data TradeType = Ask | Bid
  deriving (Eq)

-- | Parses a bid. Incorrectly formatted bids are returned verbatim
-- | (tagged with 'Left').
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
-- | (in the order encountered), and an error message is printed for
-- | each incorrectly formatted bid.
parseBids :: String -> IO [Trader]
parseBids s = concat <$> mapM (check . parseBid) (lines s)
  where
  check (Left bid) = do
    hPutStrLn stderr $ "Malformed bid: " ++ bid
    return []
  check (Right bid) = return [bid]

-- | Main
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
  where process h = trade =<< parseBids =<< hGetContents h

-- | trade make the purchases by calling transactions and deals, and prints out
-- | the result.
trade :: [Trader] -> IO ()
trade bids = let (askT,bidT) = transactions bids
             in putStr $  deals askT bidT

-- | Takes an BinHeap Trade for the asks and another BinHeap Trade for the bids,
-- | and returns the result of the closed deals and order book.
deals :: BinHeap Trade -> BinHeap Trade -> String
deals b1 b2 
  | (b1 == Heap [] || b1 == Empty) &&
    (b2 == Heap [] || b2 == Empty) = unlines ["Orderbok:","Säljare: "
                                             ,"Köpare: "]
  | (b1 == Heap [] || b1 == Empty) = unlines ["Orderbok:","Säljare: "
                                             ,"Köpare:" 
                                              ++ f' (listElements (>) b2)]
  | (b2 == Heap [] || b2 == Empty) = unlines ["Orderbok:"
                                             ,"Säljare: "
                                              ++ f' (listElements (<) b1)
                                             ,"Köpare: "]
  | (fst . getPrio (<)) b1 > (fst . getPrio (>)) b2 = "\nOrderbok:\n" ++
                "Säljare: " ++ f' (listElements (<) b1) ++
                "Köpare: " ++ f' (listElements (>) b2)
  | otherwise = (snd . getPrio (>)) b2 ++ " köper från "
                  ++ (snd . getPrio (<)) b1 ++ " för "
                  ++ (show . fst . getPrio (<)) b1 ++ " kr.\n"
                  ++ deals (deletePrio (<) b1) (deletePrio (>) b2)
  where f' []         = ""
        f' ((k,n):[]) = n ++ " " ++ show k ++ ""
        f' ((k,n):es) = n ++ " " ++ show k ++ ", " ++ f' es
        
-- | Takes a list of traders and puts them in two BinHeap Trade trees (asks and
-- | bids)
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
