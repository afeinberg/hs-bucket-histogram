-- Port of histogram code from Voldemort
-- Original: https://github.com/afeinberg/voldemort/blob/32744e33affff6d18b1d5124a090dea7bf5befa4/src/java/voldemort/store/stats/Histogram.java

module Data.BucketHistogram (
  BucketHistogram
  , bucketHistogram
  , bucketHistogramWithBounds
  , reset
  , quantile
  , insert
  ) where

import Data.Array

data BucketHistogram = BucketHistogram {
  numBuckets :: Int
  , buckets :: Array Int Int
  , lowerBounds :: Array Int Int
  , upperBounds :: Array Int Int
  , countElems :: Int
  } deriving Show
             
bucketHistogram :: Int -> Int -> BucketHistogram
bucketHistogram n s = BucketHistogram {
  numBuckets = n
  , buckets = mkBuckets n
  , lowerBounds = lowerBoundsArr
  , upperBounds = upperBoundsArr
  , countElems = 0
  }
  where
    lowerBoundsArr = listArray (0, n - 1) [i * s | i <- [0..(n - 1)]]
    upperBoundsArr = listArray (0, n - 1) [i * s | i <- [1..n]]
    
bucketHistogramWithBounds :: Int -> [Int] -> [Int] -> BucketHistogram
bucketHistogramWithBounds n lb ub = BucketHistogram {
  numBuckets = n
  , buckets = mkBuckets n
  , lowerBounds = listArray (0, n - 1) lb
  , upperBounds = listArray (0, n - 1) ub
  , countElems = 0
  }

mkBuckets :: Int -> Array Int Int
mkBuckets n = listArray (0, n - 1) (replicate n 0)

reset :: BucketHistogram -> BucketHistogram

reset h@BucketHistogram { numBuckets = n } = h {
  buckets = mkBuckets n
  , countElems = 0
  }
                                             
quantile :: (Fractional a, Ord a) => BucketHistogram -> a -> Int
quantile h@BucketHistogram { numBuckets = n
                           , buckets = bucketsArr
                           , countElems = cnt 
                           , lowerBounds = lowerBoundsArr 
                           } targetQuantile = loop 0 0
  where 
    loop total i | i == n = 0
                 | currQuantile total >= targetQuantile = lowerBoundsArr ! i
                 | otherwise = loop (total + bucketsArr ! i) (i + 1)
    currQuantile total = fromIntegral total / fromIntegral cnt
    
insert :: BucketHistogram -> Int -> BucketHistogram
insert h@BucketHistogram { buckets = bucketsArr, countElems = cnt } datum = h {
  buckets = bucketsArr // [(idx, curr + 1)]
  , countElems = cnt + 1
  }
  where
    curr = bucketsArr ! idx
    idx = findBucket h datum
    
findBucket :: BucketHistogram -> Int -> Int
findBucket h@BucketHistogram { upperBounds = upperBoundsArr, numBuckets = nb } datum 
  | datum > upperBoundsArr ! (nb - 1) = nb - 1
  | otherwise = loop 0 (nb - 1)
    where
      loop low high 
        | low > high = -1
        | otherwise = case cmp of
          EQ -> mid
          GT -> loop low (mid - 1)
          LT -> loop (mid + 1) high
          where mid = (low + high) `div` 2
                cmp = compareBucket h mid datum
                
compareBucket :: BucketHistogram -> Int -> Int -> Ordering                
compareBucket h bucket datum 
  | low <= datum && high > datum = EQ
  | low > datum = GT
  | otherwise = LT
    where
      low = lowerBounds h ! bucket
      high = upperBounds h ! bucket
      
                                   
    

    
