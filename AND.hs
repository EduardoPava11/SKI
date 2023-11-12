import qualified Math.Combinat.Permutations as Perm

-- Define the combinators
data Combinator = S | K deriving (Show, Eq)

-- Implement the evaluation rules for S and K
evaluate :: [Combinator] -> [Combinator]
evaluate [] = []
evaluate (K:x:y:rest) = x : evaluate rest  -- Rule for K
evaluate (S:x:y:z:rest) = evaluate (x:z:(y:z:rest))  -- Rule for S
evaluate (x:xs) = x : evaluate xs  -- Continue evaluation

-- Generate combinations of S and K up to a certain length
generateCombinations :: Int -> [[Combinator]]
generateCombinations n = concatMap (combinations n [S, K]) [1..n]
  where
    combinations 0 _ _ = [[]]
    combinations _ _ 0 = []
    combinations n items m = [x:ys | x <- items, ys <- combinations (n-1) items (m-1)]

-- Check if a combination behaves like AND
behavesLikeAnd :: [Combinator] -> Bool
behavesLikeAnd combo = 
    let true = [K, K, K]  -- K(KK)
        false = [K, K, S, K]  -- K(K(SK))
    in evaluate (combo ++ true ++ true) == true
    && evaluate (combo ++ true ++ false) == false
    && evaluate (combo ++ false ++ true) == false
    && evaluate (combo ++ false ++ false) == false

-- Find a combination that behaves like AND
findAndCombinator :: Int -> Maybe [Combinator]
findAndCombinator maxLength = findAndCombinatorHelper (generateCombinations maxLength)

findAndCombinatorHelper :: [[Combinator]] -> Maybe [Combinator]
findAndCombinatorHelper [] = Nothing
findAndCombinatorHelper (combo:rest) =
    if behavesLikeAnd combo
    then Just combo
    else findAndCombinatorHelper rest

main :: IO ()
main = print $ findAndCombinator 10  -- Adjust the length as needed


