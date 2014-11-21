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

-- | Data structure for a binomial tree, which has a root value and
-- | a list of subtrees.
data Node a = Null | Node a [BinHeap a] 
  deriving (Show)

-- | Data structure for a binomial heap, which is a collection
-- | of binomial trees. 
data BinHeap a = Empty | Heap [Node a]
  deriving (Eq,Show)
  
-- | Instance for the equality of two nodes, which is determined
-- | by their orders.
instance Eq a => Eq (Node a) where
  n1 == n2 = order n1 == order n2

-- | Instance for the ordering of two nodes, which is determined
-- | by their orders.
instance Eq a => Ord (Node a) where
  compare n1 n2 = compare (order n1) (order n2)
  
-- | Instance for adding two heaps together.
instance (Ord a, Eq a) => Num (BinHeap a) where
  (+) Empty Empty           = Empty
  (+) h Empty               = h
  (+) Empty h               = h 
  (+) (Heap n1s) (Heap n2s) = Heap (n1s ++ n2s)
  (*) _ _                   = undefined
  abs _                     = undefined
  signum _                  = undefined
  fromInteger _             = undefined 
  
type Trade = (Int,String)

-------------------------------------------------------------------------------

-- | Returns an empty queue.
emptyQueue :: BinHeap a
emptyQueue = Empty

-- | Returns True if queue is empty, False otherwise.
isEmpty :: BinHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Returns the value of the order of a tree.
order :: Node a -> Int
order (Node _ [Empty]) = 0
order (Node _ hs) = length hs

-- | Sorts a heap by increasing order.
sortHeap :: (Ord a, Eq a) => BinHeap a -> BinHeap a
sortHeap Empty         = Empty
sortHeap (Heap [])     = Empty
sortHeap (Heap (n:[])) = Heap [n]
sortHeap (Heap (n:ns)) 
  | n < head ns = Heap ([n] ++ extract (sortHeap (Heap ns)))
  | otherwise   = Heap ([head ns] ++ extract (sortHeap (Heap (n:(tail ns)))))

-- | Merges two heaps into one.
merge :: (Int -> Int -> Bool) -> BinHeap Trade -> BinHeap Trade
           -> BinHeap Trade
merge f Empty Empty = merge f (Heap []) (Heap [])
merge f Empty h     = merge f h (Heap [])
merge f h Empty     = merge f h (Heap [])
merge _ (Heap []) (Heap [])
                    = Empty
merge f (Heap []) h = merge f h (Heap [])
merge f h (Heap []) 
  | duplicates [ order x | x <- extract h ]
      = merge f (Heap [headN h]) (Heap (tailN h))
  | otherwise
      = h
merge f h1 h2
  | headN h1 == headN h2
      = merge f (Heap [combine f (headN h1) (headN h2)])
          (Heap (tailN h1) + Heap (tailN h2))
  | headN h1 < headN h2
      = Heap [headN h1] + merge f (Heap (tailN h1)) h2
  | otherwise
      = Heap [headN h2] + merge f (Heap (tailN h2)) h1

-- | Returns the head and tail, respectively, of a Heap's nodes.
headN :: BinHeap Trade -> Node Trade
tailN :: BinHeap Trade -> [Node Trade]
headN b 
  | b == Empty || b == Heap [] = Null
  | otherwise                  = (head . extract) b
tailN   = tail . extract

-- | Merges two nodes into one.
combine :: (Int -> Int -> Bool) -> Node Trade
             -> Node Trade
             -> Node Trade
combine f n1 n2
  | f (key n1) (key n2) || key n1 == key n2
      = Node (element n1) ([Heap [n2]] ++ (children n1))
  | otherwise 
      = combine f n2 n1

-- | Checks if there are any duplicates in a list of ordered Ints.
duplicates :: [Int] -> Bool
duplicates []     = False
duplicates (i:[]) = False
duplicates (i:is) = i == head is || duplicates (is)

-- | Extracts the nodes of a tree.
extract :: Ord a => BinHeap a -> [Node a]
extract (Heap ns) = ns

-------------------------------------------------------------------------------
{- Node functions -}

-- | Returns the Trade element of a node.
element :: Node Trade -> Trade
element (Node a _) = a

-- | Returns the key (priority) of the Trade element of a node.
key :: Node Trade -> Int
key = fst . element 

-- | Returns the string from the Trade element of a node.
name :: Node Trade -> String
name = snd . element

-- | Returns the children of a node.
children :: Node Trade -> [BinHeap Trade] 
children Null        = []
children (Node _ []) = []
children (Node _ h)  = h

-------------------------------------------------------------------------------

-- | Adds an bid to a queue.
addBid :: Trade -> BinHeap Trade -> BinHeap Trade
addBid a Empty = Heap [Node a []] 
addBid a t = merge (>) (addBid a Empty) t

-- | Adds an ask to a queue.
addAsk :: Trade -> BinHeap Trade -> BinHeap Trade
addAsk a Empty = Heap [Node a []] 
addAsk a t = merge (<) (addAsk a Empty) t

-- | Deletes the most prioritized element in a heap.
deletePrio :: (Int -> Int -> Bool) -> BinHeap Trade -> BinHeap Trade
deletePrio f h = merge f (addHeaps $ children $ headN $ fst $ findPrio f h) (snd $ findPrio f h)

-- | Adds a list of heaps into a single heap.
addHeaps :: [BinHeap Trade] -> BinHeap Trade
addHeaps []     = Empty
addHeaps (h:hs) = h + addHeaps hs 

-- | Finds the most prioritized element in a heap.
findPrio :: (Int -> Int -> Bool) -> BinHeap Trade
              -> (BinHeap Trade, BinHeap Trade)
findPrio f Empty           = (Empty, Empty)
findPrio f (Heap [])       = (Empty, Empty)
findPrio f (Heap (n : [])) = (Heap [n], Empty)
findPrio f (Heap (n1 : (n2 : [])))
  | f (key n1) (key n2)    = (Heap [n1], Heap [n2])
  | otherwise              = (Heap [n2], Heap [n1]) 
findPrio f (Heap (n1 : (n2 : ns)))
  | f (key n1) (key n2)    = (fst (findPrio f (Heap([n1] ++ ns))),
                              Heap [n2] + snd (findPrio f (Heap([n1] ++ ns))))
  | otherwise              = (fst (findPrio f (Heap([n2] ++ ns))),
                              Heap [n1] + snd (findPrio f (Heap([n2] ++ ns))))

-- | Updates an existing element in a heap with a new key value.
update :: (Int -> Int -> Bool) -> BinHeap Trade -> Trade
            -> BinHeap Trade
update f b t
  | b == Empty || b == Heap [] = emptyQueue
  | not (isInTree t b)         = Heap [headN b] + update f ht t
  | otherwise                  = case not (f (fst t) ((key . headN) b)) of
      {- The new element would not violate the properties of the heap, if
         it later would be in a subtree of the top node.                   -}
      True  -> case snd t == (name . headN) b of
        {- The top node contains the trader, so we swap the old key value
           with the new.                                                   -}
        True  -> bubbleDown f (Heap [ Node t c ]) + ht
        
        {- The top node does not contain the trader, so we continue the
           search through its children.                                    -}
        False -> Heap [ Node e [ update f x t | x <- c ] ] + ht
      {- The new element violates the properties of the heap if it is not
         replaced here, so we find the old element while bubbling down.    -}
      False -> findAndBubbleDown f b t
  where c  = (children . headN) b
        e  = (element . headN) b
        ht = (Heap . tailN) b
  
-- | Checks whether a Trade is in the BinHeap tree or not. 
isInTree :: Trade -> BinHeap Trade -> Bool
isInTree t b
  | b == Empty || b == Heap [] = False
  | snd t == name (headN b)    = True
  | otherwise                  = or [ isInTree t x
                                    | x <- (children . headN) b ] 
            
{-
update _ Empty _ _        = Empty
update _ (Heap []) _ _    = Empty
update f h old new
  | f (fst new) (fst old)       = undefined
  | otherwise             = case snd old == name (headN h) of
      True -> bubbleDown f (Heap [change (headN h)]) + Heap (tailN h)
      _    -> Heap [headN h] + update f (Heap (tailN h)) old new              
  where change (Node _ hs) = Node new hs            
-}

-- | Bubbles down the top element in a binomial tree.
bubbleDown :: (Int -> Int -> Bool) -> BinHeap Trade -> BinHeap Trade
bubbleDown _ (Heap []) = Heap []
bubbleDown _ h@(Heap [Node _ []])
                       = h
bubbleDown f h@(Heap [n@(Node a hs)])
  | f (key n) (key n') = h
  | otherwise          = bubbleDown f (Heap [Node a gs])
  where n'@(Node _ gs) = nodePrio f hs
  
-- | Finds an element in a tree and replaces it, while bubbling down.
findAndBubbleDown :: (Int -> Int -> Bool) -> BinHeap Trade -> Trade
                       -> BinHeap Trade  
findAndBubbleDown f b t = undefined

-- | Finds the most prioritized top node in a list of heaps.
nodePrio :: (Int -> Int -> Bool) -> [BinHeap Trade] -> Node Trade
nodePrio f hs = case f 1 2 of
  True -> minimum [ headN h | h <- hs] -- FEL!!!
  _    -> maximum [ headN h | h <- hs] -- FEL!!!