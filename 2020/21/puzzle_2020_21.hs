module Puzzle_2020_21 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- data types
type Ingredient = String
type Allergen = String
type Label = (Set Ingredient, Set Allergen)

-- parsing
labelsP :: Parser [Label]
labelsP = sepBy labelP newline


-- let's assume that we always have allergens
labelP :: Parser Label
labelP = do
    is <- ingredientsP 
    as <- allergensP
    return (S.fromList is, S.fromList as) 


ingredientsP :: Parser [Ingredient]
ingredientsP = some (some lowerChar <* optional space)


allergensP :: Parser [Allergen]
allergensP = between (char '(') (char ')') $ do
    string "contains "
    some allergenP

allergenP :: Parser Allergen
allergenP = some lowerChar <* optional (string ", ")

-- functions
-- build a map Allergen -> Set Ingredient
-- where a -> the possible ingredients for a (intersection of all ingredient sets having a)
-- things safe from a is complement of that
-- things safe from everything are complement of union of those
safeFoods :: [Label] -> Set Ingredient
safeFoods labels = S.difference allI unsafeFoods
  where allI = allIngredients labels
        unsafeFoods = S.unions (possibilities labels)


possibilities :: [Label] -> Map Allergen (Set Ingredient)
possibilities labels = M.fromList $ map (\a -> (a, narrowAllergen labels a)) (S.toList $ allAllergens labels)


allIngredients :: [Label] -> Set Ingredient
allIngredients labels = foldl (\acc l -> S.union acc (fst l)) S.empty labels


allAllergens :: [Label] -> Set Allergen
allAllergens labels = foldl (\acc l -> S.union acc (snd l)) S.empty labels


narrowAllergen :: [Label] -> Allergen -> Set Ingredient
narrowAllergen labels allergen = foldl f (allIngredients labels) labels
  where f :: Set Ingredient -> Label -> Set Ingredient
        f acc (is, as)
            | S.member allergen as = S.intersection acc is
            | otherwise            = acc


countSafe :: [Label] -> Int
countSafe labels = sum $ map (\l -> S.size $ S.intersection sf (fst l)) labels
  where sf = safeFoods labels

-- mains

mainA :: IO ()
mainA = do
    (Just labels) <- parseInput labelsP "21/input.txt"
    let answer = countSafe labels
    print answer
    -- result <- submitAnswer 2020 21 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just labels) <- parseInput labelsP "21/input.txt"
    mapM_ print $ M.toList $ possibilities labels
    -- result <- submitAnswer 2020 21 2 answer
    -- print result
    return ()
