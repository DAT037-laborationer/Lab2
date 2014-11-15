{-
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
-}

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

-- | Returns the value of the order of a tree.
order :: Node a [b] -> Int
order (Node _ []) = 0
order (Node _ hs) = length hs

-- | Merges two heaps into one.
merge :: Ord a => (a -> a -> Bool) -> BinHeap (a,String) -> BinHeap (a,String)
           -> BinHeap (a,String)
merge _ t1 Empty = t1
merge _ Empty t2 = t2
merge f h1@(Heap (n1@(Node a tt1):n1s)) h2@(Heap (n2@(Node b tt2):n2s))
  | or [ order xs == order ys
       | (Node _ xs)<-h1, (Node _ ys)<-h2 ]
      = if f (fst a) (fst b)
                           then merge f (Heap ((Node a (n2:tt1)):n1s)) (Heap n2s)
                           else merge f (Heap ((Node b (n1:tt2)):n1s)) (Heap n2s)
  | otherwise            = undefined

-- | Adds an bid to a queue.
addBid :: Ord a => (a,String) -> BinHeap (a,String) -> BinHeap (a,String)
addBid a Empty = Heap [Node a []] 
addBid a t = merge (>) (Heap [Node a []]) t

-- | Adds an ask to a queue.
addAsk :: Ord a => (a,String) -> BinHeap (a,String) -> BinHeap (a,String)
addAsk a Empty = Heap [Node a []] 
addAsk a t = merge (<) (Heap [Node a []]) t
