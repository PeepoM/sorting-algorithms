module SortingAlgos where

import Data.List (sort)
import Test.QuickCheck (quickCheck)


-- Selection sort
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = minimum xs : selectionSort (remove (minimum xs) xs)
  where
    remove :: Ord a => a -> [a] -> [a]
    remove _ [] = []
    remove y (x : xs)
      | y == x = xs
      | otherwise = x : remove y xs


--Bubble sort
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort list@(x : xs) = bSort list $ length list
  where
    bSort :: Ord a => [a] -> Int -> [a]
    bSort [] _ = []
    bSort list@(x : xs) n
      | n > 0 = bSort (helpSwap $ x : xs) $ n - 1
      | otherwise = x : xs

helpSwap :: Ord a => [a] -> [a]
helpSwap [] = []
helpSwap (x : []) = [x]
helpSwap (x : y : ys)
  | x <= y = x : helpSwap (y : ys)
  | otherwise = y : helpSwap (x : ys)


-- Insertion sort
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insert x $ insertionSort xs

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x > y = y : insert x ys
  | otherwise = x : y : ys


-- Merge sort 
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list@(x : xs) = merge (mergeSort fstHalf) (mergeSort sndHalf)
  where
    m = length list `div` 2
    fstHalf = take m list
    sndHalf = drop m list

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys


-- Quick sort
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x : xs) = (quickSort smaller) ++ [x] ++ (quickSort bigger)
  where
    smaller = [e | e <- xs, e < x]
    bigger = [e | e <- xs, e >= x]


-- Function for QuickCheck to check a sorting algorithm against a Data.sort function
prop_sort :: Ord a => ([a] -> [a]) -> [a] -> Bool
prop_sort sortingAlgo xs = sortingAlgo xs == sort xs

-- GHCi command to run a QuickCheck test => quickCheck (prop_sort "sorting algorithm of choice")


-- Sample list
test :: [Int]
test = [4, 1, 7, 3, 42, 53, 2, 1, 8, 0]