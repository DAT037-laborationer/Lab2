module PriorityQueue (
    PriQue,   -- type of priority queues
    emptyQue, -- PriQue a
    isEmpty,  -- PriQue a -> Bool
    addBid,      -- Ord a => a -> PriQue a -> PriQue a
    addAsk,      -- Ord a => a -> PriQue a -> PriQue a
    getMin,   -- Ord a => PriQue a -> a
    removeMin -- Ord a => PriQue a -> PriQue a
) where

--datatyp för prioriteringskö
data PriQue a = Empty |
                Node a (PriQue a) (PriQue a)
                deriving ( Eq, Show, Read )

emptyQue :: PriQue a
emptyQue = Empty

isEmpty :: PriQue a -> Bool 
isEmpty Empty = True
isEmpty _     = False

addBid :: Ord a => (a,String) -> PriQue (a,String) -> PriQue (a,String)
addBid a Empty        = Node a Empty Empty
addBid a (Node b l r) 
  | fst a > fst b = Node a r (addBid b l)
  | otherwise     = Node b r (addBid a l)

addAsk :: Ord a => (a,String) -> PriQue (a,String) -> PriQue (a,String)
addAsk a Empty        = Node a Empty Empty
addAsk a (Node b l r) 
  | fst a < fst b = Node a r (addAsk b l)
  | otherwise     = Node b r (addAsk a l)

decreaseKey = undefined

increaseKey = undefined

getMin :: Ord a => PriQue a -> a
getMin Empty         = error "NoSuchElementException"
getMin (Node a _ _ ) = a

getMostRight :: Ord a => PriQue a -> (a, PriQue a)
getMostRight (Node a Empty Empty) = (a,Empty)
getMostRight (Node a l r) = ( b, Node a bt l )
                            where (b,bt) =  getMostRight r

restore :: Ord a => a -> PriQue a -> PriQue a -> PriQue a
restore a Empty Empty   = Node a Empty Empty
restore a Empty bt@(Node b Empty Empty) 
      | a < b     = Node a Empty bt
      | otherwise = Node b Empty (Node a Empty Empty)
restore a bt@(Node b bl br) ct@(Node c cl cr)
      | a < b && a < c = Node a bt ct
      | b < c          = Node b (restore a bl br) ct
      | otherwise      = Node c bt (restore a cl cr)

removeMin :: Ord a => PriQue a -> PriQue a
removeMin Empty = error "IllegalStateException"
removeMin (Node _ l r )   = restore b bt l 
                            where (b,bt) = getMostRight r


{-
data Node a b = Node a [b] 
  deriving (Eq,Ord,Show)

data BinHeap a = Empty |
                 Heap [Node a (BinHeap a)]
  deriving (Eq,Ord,Show)

-- | Returns an empty queue.
emptyQueue :: BinHeap a
emptyQueue = Empty

-- | Returns True if queue is empty, False otherwise.
isEmpty :: BinHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Returns the value of the order of a queue.
order :: BinHeap a -> Int
order (Heap [Node _ []]) = 0 
order (Heap [Node _ ns]) = length ns
order _                  = undefined

-- | Merges two queues into one.
merge :: Ord a => (a -> a -> Bool) -> BinHeap (a,String) -> BinHeap (a,String)
           -> BinHeap (a,String)
merge _ t1 Empty = t1
merge _ Empty t2 = t2
merge f t1@(Heap [Node a tt1]) t2@(Heap [Node b tt2])
  | order t1 == order t2 = if f (fst a) (fst b)
                           then Heap [Node a ([t2] ++ tt1)]
                           else Heap [Node b ([t1] ++ tt2)]
  | order t1 < order t2  = Heap [Node b ((merge f t1 (head tt2)):(tail tt2))]
  | otherwise            = Heap [Node a ((merge f t2 (head tt1)):(tail tt1))]

-- | Adds an bid to a queue.
addBid :: Ord a => (a,String) -> BinHeap (a,String) -> BinHeap (a,String)
addBid a Empty = Heap [Node a []] 
addBid a t = merge (>) (Heap [Node a []]) t

-- | Adds an ask to a queue.
addAsk :: Ord a => (a,String) -> BinHeap (a,String) -> BinHeap (a,String)
addAsk a Empty = Heap [Node a []] 
addAsk a t = merge (<) (Heap [Node a []]) t
-}