-- 3 kyu
-- Rail Fence Cipher: Encoding and Decoding
-- https://www.codewars.com/kata/58c5577d61aefcf3ff000081
--
-- Create two functions to encode and then decode a string using the Rail Fence Cipher. This cipher is used to encode a string by placing each character successively in a diagonal along a set of "rails". First start off moving diagonally and down. When you reach the bottom, reverse direction and move diagonally and up until you reach the top rail. Continue until you reach the end of the string. Each "rail" is then read left to right to derive the encoded string.
--
-- For example, the string "WEAREDISCOVEREDFLEEATONCE" could be represented in a three rail system as follows:
--
-- W       E       C       R       L       T       E
--   E   R   D   S   O   E   E   F   E   A   O   C
--     A       I       V       D       E       N
-- The encoded string would be:
--
-- WECRLTEERDSOEEFEAOCAIVDEN
-- Write a function/method that takes 2 arguments, a string and the number of rails, and returns the ENCODED string.
--
-- Write a second function/method that takes 2 arguments, an encoded string and the number of rails, and returns the DECODED string.
--
-- For both encoding and decoding, assume number of rails >= 2 and that passing an empty string will return an empty string.
--
-- Note that the example above excludes the punctuation and spaces just for simplicity. There are, however, tests that include punctuation. Don't filter out punctuation as they are a part of the string.

module CodeWars.Kata.RailFenceCipherEncodingAndDecoding
  ( testRailFenceCipherEncodingAndDecoding,
  )
where

import Control.Monad (forM_)
import Data.Foldable (fold)
import qualified Data.Map.Lazy as Map
import Lang.Utils
import Test.Hspec

encode :: [a] -> Int -> [a]
encode items size =
  let op i v -- check if the size is valid
        | size > 1 = (row, [v])
        | otherwise = (1, [v])
        where
          unit = 2 * size - 2 -- the size of group data
          rem = i `mod` unit -- get the index offset in group
          row -- compute the row index in "v"
            | rem == 0 = 2
            | rem < size = rem
            | otherwise = 2 * size - rem
   in fold $ Map.elems $ Map.fromListWith (flip (++)) $ zipWith op [1 ..] items

decode :: [a] -> Int -> [a]
decode items 1 = items
decode items rowSize = results
  where
    unit = 2 * rowSize - 2 -- the size of group data
    itemSize = length items
    groupCount = itemSize `div` unit -- get the count of group
    rem = itemSize `mod` unit -- get the index offset in group
    range = map op [1 .. rowSize]
      where
        op row = (row, f row)
        f row
          | row == 1 || row == rowSize =
            groupCount + if rem >= row then 1 else 0 -- first and last row
          | row >= 2 * rowSize - rem = groupCount * 2 + 2 -- some row which have two elements
          | rem >= row = groupCount * 2 + 1
          | otherwise = groupCount * 2
    rowToSize = foldl op [] range
      where
        op lastItems row@(rowIndex, size)
          | null lastItems = [row]
          | let (_, lastSize) = last lastItems =
            lastItems ++ [(rowIndex, lastSize + size)]
    rowToChars = build (zip [1 ..] items) rowToSize
      where
        build ((i, item) : otherItems) allRanges@((row, size) : otherRanges) =
          build otherItems ranges ++ [(row, item)]
          where
            ranges
              | i < size = allRanges
              | otherwise = otherRanges
        build _ _ = []
    rowMapChars = foldl op Map.empty rowToChars where op maps (k, v) = Map.insertWith (++) k [v] maps
    results = deal rowMapChars 1 (+ 1)
      where
        deal maps index indexOp = case maps Map.!? index of
          Just (n : others) -> n : deal (mapOp others) nextIndex nextOp
          Nothing -> []
          where
            nextIndex = nextOp index
            nextOp
              | index == 1 = (+ 1)
              | index == rowSize = flip (-) 1
              | otherwise = indexOp
            mapOp others = Map.update (const item) index maps
              where
                item
                  | null others = Nothing
                  | otherwise = Just others

decodeDebug :: Show a => [a] -> Int -> [a]
decodeDebug items 1 = items
decodeDebug items rowSize = results
  where
    unit = 2 * rowSize - 2 -- the size of group data
    itemSize = length items
    groupCount = itemSize `div` unit -- get the count of group
    rem = itemSize `mod` unit -- get the index offset in group
    range =
      traceSelf
        ( "unit: "
            ++ show unit
            ++ ", itemSize: "
            ++ show itemSize
            ++ ", groupCount: "
            ++ show groupCount
            ++ ", rem: "
            ++ show rem
            ++ ", rowSize: "
            ++ show rowSize
            ++ ", range: "
        )
        $ map op [1 .. rowSize]
      where
        op row = (row, f row)
        f row
          | row == 1 || row == rowSize =
            groupCount + if rem >= row then 1 else 0 -- first row and last row
          | row >= 2 * rowSize - rem = groupCount * 2 + 2
          | rem >= row = groupCount * 2 + 1
          | otherwise = groupCount * 2
    rowToSize = foldl op [] range
      where
        op lastItems row@(rowIndex, size)
          | null lastItems = [row]
          | let (_, lastSize) = last lastItems =
            lastItems ++ [(rowIndex, lastSize + size)]
    rowToChars =
      traceSelf "rowToChars: " $
        build
          (traceSelf "indexToChars: " $ zip [1 ..] items)
          (traceSelf "rowToSize: " rowToSize)
          []
      where
        build ((i, item) : otherItems) ranges@((row, size) : otherRanges) result
          | i < size = build otherItems ranges result ++ [(row, item)]
          | otherwise = build otherItems otherRanges result ++ [(row, item)]
        build _ _ result = result
    rowMapChars = traceSelf "rowMapChars: " $ foldl op Map.empty rowToChars
      where
        op maps (k, v) = Map.insertWith (++) k [v] maps
    results = deal rowMapChars 1 (+ 1)
      where
        deal :: Show a => Map.Map Int [a] -> Int -> (Int -> Int) -> [a]
        deal maps index indexOp =
          case traceSelf ("Map K: " ++ show index ++ " Value: ") $ maps Map.!? index of
            Just (n : others) -> n : deal (mapOp others) nextIndex nextOp
            Nothing -> []
          where
            nextIndex = nextOp index
            nextOp
              | index == 1 = (+ 1)
              | index == rowSize = flip (-) 1
              | otherwise = indexOp
            mapOp others = Map.update (const item) index maps
              where
                item
                  | null others = Nothing
                  | otherwise = Just others

testRailFenceCipherEncodingAndDecoding :: Spec
testRailFenceCipherEncodingAndDecoding =
  it "RailFenceCipherEncodingAndDecoding" $ do
    encode "WEAREDISCOVEREDFLEEATONCE" 3 `shouldBe` "WECRLTEERDSOEEFEAOCAIVDEN"
    encode "123456789" 6 `shouldBe` "123948576"
    encode "123456789" 4 `shouldBe` "172683594"
    encode "123456789" 2 `shouldBe` "135792468"
    encode "123456789" 1 `shouldBe` "123456789"
    decode "172683594" 4 `shouldBe` "123456789"
    decode "WECRLTEERDSOEEFEAOCAIVDEN" 3 `shouldBe` "WEAREDISCOVEREDFLEEATONCE"
    encode "Hello, World!" 3 `shouldBe` "Hoo!el,Wrdl l"
    "Hello, World!" `encode` 3 `decode` 3 `shouldBe` "Hello, World!"
    forM_
      [1 .. 100]
      ( \i ->
          trace ("test with row: " ++ show i) $
            "123456789"
              `encode` i
              -- `decodeDebug` i
              `decode` i
              `shouldBe` "123456789"
      )
