{-# OPTIONS_GHC -Wall #-}
module Marchenko03 where

data BinTree a = EmptyB 
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a   
               | Node2 (Tree23 a) a (Tree23 a) 
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- порожнє 2-3-дерево!!!
                   deriving (Eq, Show)

-- Задача 1 -----------------------------------------
-- Checks if all elements of tree match the predicate
checkAll :: (Ord a) => BinTree a -> a -> (a->a->Bool) -> Bool
checkAll EmptyB _ _ = True
checkAll (Node h t1 t2) element comp = (comp h element) && (checkAll t1 element comp) && (checkAll t2 element comp)

isSearch :: (Ord a) => BinTree a -> Bool
isSearch EmptyB = True
isSearch (Node h b1 b2) = (checkAll b1 h (<)) && (checkAll b2 h (>)) && isSearch b1 && isSearch b2 

-- Задача 2-----------------------------------------
elementSearch :: (Ord a) => BinTree a -> a -> Bool
elementSearch EmptyB _ = False
elementSearch (Node h t1 t2) v = if (v==h) then True else if isSearch (Node h t1 t2) then if (v<h) then elementSearch t1 v else elementSearch t2 v
                                                          else elementSearch t1 v || elementSearch t2 v -- error "THIS TREE IS NOT BINARY SEARCH"

-- Задача 3 -----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a 
insSearch EmptyB v = (Node v EmptyB EmptyB)
insSearch (Node h t1 t2) v = if (h==v) then (Node h t1 t2) else if (h > v) then (Node h (insSearch t1 v) t2) else (Node h t1 (insSearch t2 v))

-- Задача 4 -----------------------------------------
minElem :: (Ord a) => BinTree a -> a
minElem EmptyB = error "EMPTY TREE"
minElem (Node h EmptyB _) = h
minElem (Node _ t1 _) = minElem t1

delSearch :: (Ord a) => BinTree a -> a -> BinTree a 
delSearch EmptyB _ = EmptyB
delSearch (Node h t1 EmptyB) v = if (v==h) then t1 else if (v<h) then (Node h (delSearch t1 v) EmptyB) else (Node h t1 EmptyB)
delSearch (Node h EmptyB t2) v = if (v==h) then t2 else if (v<h) then (Node h EmptyB t2) else (Node h EmptyB (delSearch t2 v))
delSearch (Node h t1 t2) v = if(v < h) then (Node h (delSearch t1 v) t2) else if (v > h) then (Node h t1 (delSearch t2 v)) else (Node m t1 (delSearch t2 m)) where m = minElem t2

-- Задача 5 -----------------------------------------
treeToList :: (Ord a) => BinTree a -> [a]
treeToList EmptyB = []
treeToList (Node h t1 t2) = treeToList t1 ++ [h] ++ treeToList t2

sortList :: (Ord a) => [a] -> [a]
sortList xs = treeToList (foldl insSearch EmptyB xs)

-- Задача 6-----------------------------------------

height :: (Ord a) => Tree23 a -> Int
height Empty23 = 0
height (Leaf _) = 1
height (Node2 t1 _ t2) = if (height t1 == height t2) then 1 + height t1 else max (height t1) (height t2) -- error "Heights of subtrees are not equal" 
height (Node3 t1 _ t2 _ t3) = if height t1 == height t2 && height t2 == height t3 then 1 + height t3 else max (max (height t1) (height t2)) (height t3) -- error "Heights of subtrees are not equal"

check23 :: (Ord a) => Tree23 a -> a -> (a -> a -> Bool) -> Bool
check23 Empty23 _ _ = True
check23 (Leaf e) key comp = comp e key
check23 (Node2 t1 _ t2) key comp = check23 t1 key comp && check23 t2 key comp
check23 (Node3 t1 _ t2 _ t3) key comp = check23 t1 key comp && check23 t2 key comp && check23 t3 key comp
-- check23 (Node2 t1 h t2) key comp = (comp h key) && check23 t1 key comp && check23 t2 key comp
-- check23 (Node3 t1 h1 t2 h2 t3) key comp = (comp h1 key) && (comp h2 key) && check23 t1 key comp && check23 t2 key comp && check23 t3 key comp

isTree23  :: (Ord a) => Tree23 a -> Bool 
isTree23 Empty23 = True
isTree23 (Leaf _) = True
isTree23 (Node2 t1 h t2) = check23 t1 h (<=) && check23 t2 h (>=) && height t1 == height t2
isTree23 (Node3 t1 h1 t2 h2 t3) = check23 t1 h1 (<=) && check23 t2 h1 (>=) && check23 t2 h2 (<=) && check23 t3 h2 (>=) && height t1 == height t2 && height t1 == height t3
-- isTree23 (Node2 t1 h t2) = check23 t1 h (<=) && check23 t2 h (>) && isTree23 t1 && isTree23 t2
-- isTree23 (Node3 t1 h1 t2 h2 t3) = check23 t1 h1 (<=) && check23 t2 h1 (>) && check23 t2 h2 (<=) && check23 t3 h2 (>) && isTree23 t1 && isTree23 t2 && isTree23 t3

-- Задача 7-----------------------------------------
elementTree23 :: (Ord a) => Tree23 a -> a -> Bool
elementTree23 Empty23 _ = False
elementTree23 (Leaf e) key = key == e
elementTree23 (Node2 t1 h t2) key = if (key==h) then (elementTree23 t1 key) || (elementTree23 t2 key) else if (key < h) then elementTree23 t1 key else elementTree23 t2 key
elementTree23 (Node3 t1 h1 t2 h2 t3) key = if (key < h1) then elementTree23 t1 key else if (key==h1) then (elementTree23 t1 key) || (elementTree23 t2 key) else if (h2==key) then (elementTree23 t2 key) || (elementTree23 t3 key) else if (h2 < key) then elementTree23 t3 key else elementTree23 t2 key


-- Задача 8-----------------------------------------

tree23ToList :: (Ord a) => Tree23 a -> [a]
tree23ToList Empty23 = []
tree23ToList (Leaf e) = [e]
tree23ToList (Node2 t1 _ t2) = tree23ToList t1 ++ tree23ToList t2
tree23ToList (Node3 t1 _ t2 _ t3) = tree23ToList t1 ++ tree23ToList t2 ++ tree23ToList t3

eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23  t1 t2 = tree23ToList t1 == tree23ToList t2

-- Задача 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 tr v =    let res = insert v tr
                    in case (snd res) of
                        Nothing -> fst res
                        Just pair ->
                                let f = fst pair in
                                let s = snd pair in    
                                (Node2 (fst res) f s)

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True 
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Результат вставки вузла в 2-3-дерево, 
--   корінь якого - вузол вида Node2 або Node3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a 
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довілье дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

-- insTerm v tr - додається значення v в дерево tr з корнем - термінальний вузол 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm v (Node2 (Leaf l) x (Leaf r)) = (
                                        if v <= l then (Node3 (Leaf v) l (Leaf l) x (Leaf r)) 
                                        else if v > l && v < x then (Node3 (Leaf l) v (Leaf v) x (Leaf r))  
                                        else if v >= x && v < r then (Node3 (Leaf l) x (Leaf v) r (Leaf r))
                                        else (Node3 (Leaf l) r (Leaf r) v (Leaf v)),
                                        Nothing)
insTerm v (Node3 (Leaf l) x (Leaf m) y (Leaf r)) = if v <= l then ((Node2 (Leaf v) l (Leaf l)), Just (x, (Node2 (Leaf m) y (Leaf r))))
                                                   else if v > l && v < x then ((Node2 (Leaf l) v (Leaf v)), Just (x, (Node2 (Leaf m) y (Leaf r))))
                                                   else if v>=x && v < y then
                                                                              if v < m then ((Node2 (Leaf l) x (Leaf v)), Just (v, (Node2 (Leaf m) y (Leaf r))))
                                                                              else ((Node2 (Leaf l) x (Leaf m)), Just (m, (Node2 (Leaf v) y (Leaf r))))
                                                   else if v < r then ((Node2 (Leaf l) x (Leaf m)), Just (y, (Node2 (Leaf v) r (Leaf r))))
                                                   else ((Node2 (Leaf l) x (Leaf m)), Just (y, (Node2 (Leaf r) v (Leaf v))))
insTerm _ _ = undefined -- to disable compiler warnings
                                                    

-- insNode v tr - додає значення v в дерево tr з корнем - нетермінальний вузол 
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode v Empty23 = ((Leaf v), Nothing)
insNode v (Leaf x) = ((Node2 (Leaf (min v x)) (max v x) (Leaf (max v x))), Nothing) 
insNode v (Node2 l x r) =   if v < x then
                                let res = insert v l
                                in case (snd res) of
                                    Nothing -> ((Node2 (fst res) x r), Nothing)
                                    Just pair -> let f = fst pair in
                                        let s = snd pair in
                                        ((Node3 (fst res) f s x r), Nothing)
                            else 
                                let res = insert v r
                                in case (snd res) of
                                    Nothing -> ((Node2 l x (fst res)), Nothing)
                                    Just pair -> let f = fst pair in
                                        let s = snd pair in
                                        ((Node3 l x (fst res) f s), Nothing)
insNode v (Node3 l x m y r) =   if v < x then
                                    let res = insert v l
                                    in case (snd res) of
                                        Nothing -> ((Node3 (fst res) x m y r), Nothing)
                                        Just pair -> let f = fst pair in
                                            let s = snd pair in
                                            ((Node2 (fst res) f s), Just (x, (Node2 m y r)))
                                else if v>=x && v < y then
                                    let res = insert v m
                                    in case (snd res) of
                                        Nothing -> ((Node3 l x (fst res) y r), Nothing)
                                        Just pair ->
                                            let f = fst pair in
                                            let s = snd pair in
                                            ((Node2 l x (fst res)), Just (f, (Node2 s y r)))
                                else 
                                    let res = insert v r 
                                    in case (snd res) of
                                        Nothing -> ((Node3 l x m y (fst res)), Nothing)
                                        Just pair -> let f = fst pair in
                                            let s = snd pair in
                                            ((Node2 l x m), Just (y, (Node2 s f (fst res))))

---  Бінарні дерева 
bt1, bt2, bt3 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB 
                     (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                      EmptyB)
bt2 = Node 9 (Node 4 EmptyB 
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                       EmptyB)

bt3 = Node 9 (Node 4 EmptyB 
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 8 EmptyB EmptyB) 
                       EmptyB) 

---- 2-3-дерева
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )