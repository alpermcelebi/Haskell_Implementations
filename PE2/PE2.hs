module PE2 where

data Tree k v = EmptyTree | Node k v [Tree k v] deriving (Show, Eq)

-- Question 1
selectiveMap _ _ [] = []

selectiveMap g h (x:xs) | g x = (h x) : (selectiveMap g h xs)
                        | otherwise = x : (selectiveMap g h xs)


-- Question 2

tSelectiveMap :: (k -> Bool) -> (v -> v) -> Tree k v -> Tree k v
tSelectiveMap _ _ EmptyTree = EmptyTree
tSelectiveMap g h (Node k v children) = Node k v' children'
                        where
                             v' | g k = h v
                                | otherwise = v
                             children' = map (tSelectiveMap g h) children
-- Question 3
preOrderTraversal :: Tree k v -> [(k, v)]
preOrderTraversal EmptyTree = []
preOrderTraversal (Node k value children) = (k, value) : concatMap preOrderTraversal children

helperfst [] _  = []
helperfst ((x,y):xs) predicate | predicate x = y : helperfst xs predicate
                               | otherwise = helperfst xs predicate

tSelectiveMappingFold :: (k -> Bool) -> (k -> v -> r) -> (r -> r -> r) -> r -> Tree k v -> r
tSelectiveMappingFold _ _ _ idt EmptyTree = idt
tSelectiveMappingFold predicate mapper combiner idt (Node k v children) =
    combiner mappedValue childResult
    where
        mappedValue = if predicate k then mapper k v else idt
        childResult = foldl (\acc child -> combiner acc (tSelectiveMappingFold predicate mapper combiner idt child)) idt children


-- Question 4
-- This question commented out so that your file compiles even before you complete it
-- It shouldn't effect the grades of other questions when commented out
-- When ready, just remove the lines with {- and -} to uncomment the function

searchTree :: (Eq v, Eq k) => v -> Tree k v -> (k -> v)
searchTree def = tSelectiveMappingFold a b c d
    where a = const True
          b = const(\v _ -> v)
          c = const
          d = const def
