module PriorityQueue where

-- | Data structure for a binomial tree, which has a root value and
-- | a list of subtrees.
data Node a = Node a [BinHeap a] 
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
  
type Trade = (Integer,String)

-------------------------------------------------------------------------------

-- | Returns an empty queue.
emptyQueue :: BinHeap a
emptyQueue = Empty

-- | Returns True if queue is empty, False otherwise.
isEmpty :: BinHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Returns the value of the order of a tree.
order :: Node a -> Integer
order (Node _ [Empty]) = 0
order (Node _ hs) = (toInteger . length) hs

-- | Sorts a heap by increasing order.
sortHeap :: (Ord a, Eq a) => BinHeap a -> BinHeap a
sortHeap Empty         = Empty
sortHeap (Heap [])     = Empty
sortHeap (Heap (n:[])) = Heap [n]
sortHeap (Heap (n:ns)) 
  | n < head ns = Heap ([n] ++ extract (sortHeap (Heap ns)))
  | otherwise   = Heap ([head ns] ++ extract (sortHeap (Heap (n:(tail ns)))))

-- | Merges two heaps into one.
merge :: (Integer -> Integer -> Bool) -> BinHeap Trade -> BinHeap Trade
           -> BinHeap Trade
merge f Empty Empty = merge f (Heap []) (Heap [])
merge f Empty b     = merge f b (Heap [])
merge f b Empty     = merge f b (Heap [])
merge _ (Heap []) (Heap [])
                    = Empty
merge f (Heap []) b = merge f b (Heap [])
merge f b (Heap []) 
  | duplicates [ order x | x <- extract b ]
      = merge f (Heap [headN b]) (Heap (tailN b))
  | otherwise
      = b
merge f b1 b2
  | headN b1 == headN b2
      = merge f (Heap [combine f (headN b1) (headN b2)])
          (Heap (tailN b1) + Heap (tailN b2))
  | headN b1 < headN b2
      = Heap [headN b1] + merge f (Heap (tailN b1)) b2
  | otherwise
      = Heap [headN b2] + merge f (Heap (tailN b2)) b1

-- | Returns the head and tail, respectively, of a Heap's nodes.
headN :: BinHeap Trade -> Node Trade
tailN :: BinHeap Trade -> [Node Trade]
headN b 
  | b == Empty || b == Heap [] = error "Can't use headN on empty list."
  | otherwise                  = (head . extract) b
tailN   = tail . extract

-- | Merges two nodes into one.
combine :: (Integer -> Integer -> Bool) -> Node Trade
             -> Node Trade
             -> Node Trade
combine f n1 n2
  | f (key n1) (key n2) || key n1 == key n2
      = Node (element n1) ([Heap [n2]] ++ (children n1))
  | otherwise 
      = combine f n2 n1

-- | Checks if there are any duplicates in a list of ordered Integers.
duplicates :: [Integer] -> Bool
duplicates []     = False
duplicates (i:[]) = False
duplicates (i:is) = i == head is || duplicates (is)

-- | Extracts the nodes of a tree.
extract :: Ord a => BinHeap a -> [Node a]
extract (Heap ns) = ns

-- | Returns the Trade element of a node.
element :: Node Trade -> Trade
element (Node a _) = a

-- | Returns the key (priority) of the Trade element of a node.
key :: Node Trade -> Integer
key = fst . element 

-- | Returns the string from the Trade element of a node.
name :: Node Trade -> String
name = snd . element

-- | Returns the children of a node.
children :: Node Trade -> [BinHeap Trade] 
children (Node _ []) = []
children (Node _ h)  = h

-- | Adds an bid to a queue.
addBid :: Trade -> BinHeap Trade -> BinHeap Trade
addBid a Empty = Heap [Node a []] 
addBid a t = merge (>) (addBid a Empty) t

-- | Adds an ask to a queue.
addAsk :: Trade -> BinHeap Trade -> BinHeap Trade
addAsk a Empty = Heap [Node a []] 
addAsk a b = merge (<) (addAsk a Empty) b

-- | Deletes the most prioritized element in a heap.
deletePrio :: (Integer -> Integer -> Bool) -> BinHeap Trade -> BinHeap Trade
deletePrio f b
  | b == Heap [] || b == Empty = emptyQueue
  | otherwise
      = merge f (addHeaps $ children $ headN $ fst $ findPrio f b) (snd $ findPrio f b)

-- | Adds a list of heaps into a single heap.
addHeaps :: [BinHeap Trade] -> BinHeap Trade
addHeaps []     = Empty
addHeaps (h:hs) = addHeaps hs + h 

-- | Finds the most prioritized element in a heap and returns a tuple with 
-- | the tree that contains the found element and the rest of the trees.
findPrio :: (Integer -> Integer -> Bool) -> BinHeap Trade
              -> (BinHeap Trade, BinHeap Trade)
findPrio f Empty           = (Empty, Empty)
findPrio f (Heap [])       = (Empty, Empty)
findPrio _ (Heap (n : [])) = (Heap [n], Empty)
findPrio f (Heap (n1 : n2 : []))
  | f (key n1) (key n2)    = (Heap [n1], Heap [n2])
  | otherwise              = (Heap [n2], Heap [n1]) 
findPrio f (Heap (n1 : n2 : ns))
  | f (key n1) (key n2)    = (fst (findPrio f (Heap([n1] ++ ns))),
                              Heap [n2] + snd (findPrio f (Heap([n1] ++ ns))))
  | otherwise              = (fst (findPrio f (Heap([n2] ++ ns))),
                              Heap [n1] + snd (findPrio f (Heap([n2] ++ ns))))

-- | Returns the top node element
getPrio :: (Integer -> Integer -> Bool) -> BinHeap Trade -> Trade
getPrio f b
  | b == Heap [] || b == Empty = error "Can't use getPrio on empty heap."
  | otherwise = (element . headN . fst . findPrio f) b

-- | Returns a list of all the elements in a heap arranged from highest
-- | priority to lowest.
listElements :: (Integer -> Integer -> Bool) -> BinHeap Trade -> [Trade]
listElements f b
  | b == Heap [] || b == Empty = []
  | otherwise
      = [getPrio f b] ++ (listElements f . deletePrio f) b

-- | Updates an existing element in a heap with a new key value.
update :: (Integer -> Integer -> Bool) -> Trade -> BinHeap Trade
            -> BinHeap Trade
update f t b
  | b == Empty || b == Heap [] = emptyQueue
  | not (isInTree t bh) = bh + update f t bt
  | otherwise                  = case not (f (fst t) (key n)) of
      {- The new element would not violate the properties of the heap, if
         it later would be in a subtree of the top node.                   -}
      True  -> case snd t == name n of
        {- The top node contains the trader, so we swap the old key value
           with the new.                                                   -}
        True  -> bubbleDown f (Heap [ Node t c ]) + bt
        
        {- The top node does not contain the trader, so we continue the
           search through its children.                                    -}
        False -> Heap [ Node e [ update f t x | x <- c ] ] + bt
      {- The new element violates the properties of the heap if it is not
         replaced here, so we find the old element while bubbling down.    -}
      False -> findAndBubbleDown f b t
  where n  = headN b
        c  = children n
        e  = element n
        bh = Heap [n]         
        bt = (Heap . tailN) b
  
-- | Checks whether a Trade is in the BinHeap tree or not. 
isInTree :: Trade -> BinHeap Trade -> Bool
isInTree t b
  | b == Empty || b == Heap [] = False
  | snd t == name n            = True
  | otherwise                  = or [ isInTree t x
                                    | x <- children n ]
  where n = headN b 

-- | Bubbles down the top element in a binomial tree.
bubbleDown :: (Integer -> Integer -> Bool) -> BinHeap Trade -> BinHeap Trade
bubbleDown f b
  | b == Heap [] || b == Empty = emptyQueue
  | order n == 0               = b
  | f k k'                     = b
  | otherwise
      = Heap [ Node e' (bf ++ [ bubbleDown f (Heap [Node e c']) ] ++ bb) ] 
  where n          = headN b
        (bf,b',bb) = nodePrio f c
        c          = children n
        c'         = (children . headN) b'
        e          = element n
        e'         = (element . headN) b'
        k          = key n
        k'         = (key . headN) b'
        
-- | Finds an element in a tree and replaces it, while bubbling down.
findAndBubbleDown :: (Integer -> Integer -> Bool) -> BinHeap Trade -> Trade
                       -> BinHeap Trade  
findAndBubbleDown f b t = f' f b t t
  where f' f b t r 
          | b == Empty || b == Heap [] = emptyQueue
          | snd t == snd e             = bubbleDown f (Heap [Node r c] + bt)
          | not (isInTree t bh)        = bh + f' f bt t r
          | otherwise                  = case f (fst r) (key n) of
              True  -> Heap [ Node r [ f' f x t e | x <- c ] ]
              False -> Heap [ Node e [ f' f x t r | x <- c ] ]
          where n  = headN b
                hc = if c /= [] then head c else Empty
                c  = children n
                e  = element n
                bh = Heap [headN b]
                bt = (Heap . tailN) b

-- | Finds the most prioritized top node in a list of heaps, and returns a
-- | tuple with a list of the nodes in front of the prioritized node, the
-- | most prioritized node and a list of the nodes behind the most
-- | prioritized node.
nodePrio :: (Integer -> Integer -> Bool) -> [BinHeap Trade]
              -> ([BinHeap Trade], BinHeap Trade, [BinHeap Trade])
nodePrio _ []     = ([], Empty, [])
nodePrio _ (b:[]) = ([], b, [])
nodePrio f x@(b:bs) 
  | b == Heap [] || b == Empty = nodePrio f bs
  | key n == np                = ([], b, bs)
  | otherwise                  = ([b] ++ bf, b', bb)
  where n          = headN b
        np         = foldl f' k1 [ (key . headN) x | x <- bs]
        f' a b     = if f a b then a else b 
        k1         = (key . headN) b
        k2         = (key . headN . head) bs
        (bf,b',bb) = nodePrio f bs
