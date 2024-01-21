import Control.Monad (replicateM)

data Combinator = S | K | Combinator :@: Combinator deriving (Show, Eq)

apply :: Combinator -> [Combinator] -> IO (Maybe [Combinator])
apply K (x:_:xs) = return $ Just (x : xs) -- K x y = x
apply S (x:y:z:xs) = do  -- S x y z = x z (y z)
    -- Evaluate the result of applying S combinator
    let appliedXZ = x :@: z
    let appliedYZ = y :@: z
    return $ Just (appliedXZ :@: appliedYZ : xs)  -- No need to reduce here; reduceOnce will handle it
apply (_ :@: _) xs = do
    maybeReduced <- reduceOnce xs
    case maybeReduced of
        Just reduced -> apply (head reduced) xs
        Nothing -> return Nothing
apply _ _ = return Nothing -- In case there aren't enough elements to apply the combinator

reduce :: [Combinator] -> Int -> IO Int
reduce xs counter 
    | counter >= 50 = return counter
    | otherwise = do
        -- Comment out or remove the following line to suppress step-by-step printing
        putStrLn $ "Step " ++ show counter ++ ": " ++ showCombinators xs
        result <- reduceOnce xs
        case result of
            Just xs' -> if xs' == xs 
                        then return counter
                        else reduce xs' (counter + 1)
            Nothing -> return counter

reduceOnce :: [Combinator] -> IO (Maybe [Combinator])
reduceOnce [] = return $ Just []
reduceOnce [x] = return $ Just [x]
reduceOnce (x:y:xs) = do
    appliedResult <- apply x (y:xs)
    case appliedResult of
        Just xs' -> return $ Just xs'  -- Successfully applied, return the result
        Nothing -> case x of
            x1 :@: x2 -> do
                -- Recursively reduce the nested structure
                reducedX <- reduceOnce [x1]
                case reducedX of
                    Just reducedX' -> return $ Just (reducedX' ++ (y : xs))
                    Nothing -> return Nothing  -- Unable to reduce further
            _ -> return $ Just (x : y : xs)  -- No further reduction possible

parseCombinators :: String -> [Combinator]
parseCombinators = map parseChar
  where
    parseChar 'S' = S
    parseChar 'K' = K
    parseChar c = error $ "Invalid character: " ++ [c]

showCombinators :: [Combinator] -> String
showCombinators [] = ""
showCombinators (S:xs) = "S" ++ showCombinators xs
showCombinators (K:xs) = "K" ++ showCombinators xs
showCombinators ((x :@: y):xs) = "(" ++ showCombinators [x] ++ " :@: " ++ showCombinators [y] ++ ")" ++ showCombinators xs

generateCombinations :: Int -> [[Combinator]]
generateCombinations n = replicateM n [S, K]

main :: IO ()
main = do
    let length = 7  -- or any length you choose
    let combinations = generateCombinations length
    results <- mapM evaluateCombination combinations
    let nonTerminating = filter (\(_, steps) -> steps >= 50) results
    putStrLn "Combinations that didn't terminate within 150 steps:"
    mapM_ print nonTerminating
    -- Further analysis can be done here

evaluateCombination :: [Combinator] -> IO ([Combinator], Int)
evaluateCombination combinators = do
    steps <- reduce combinators 0
    return (combinators, steps)
