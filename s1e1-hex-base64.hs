module Main where

import qualified Data.ByteString as B
import Data.Word (Word8, Word32)
import Data.Bits

hexToB64 :: B.ByteString -> B.ByteString
hexToB64 bs = binToB64 (mungeFourToSixBitBin . hexToBin $ bs)

hexToBin :: B.ByteString -> B.ByteString
hexToBin bs = B.map hexCharToBinWord bs

binToB64 :: B.ByteString -> B.ByteString
binToB64 bs = B.map binWordToB64Char bs

mungeFourToSixBitBin :: B.ByteString -> B.ByteString
mungeFourToSixBitBin bs = (B.pack . reverse . flatten) (
    map fourBitTripleToSixBitPair (toTriples . B.reverse $ bs))

fourBitTripleToSixBitPair :: (Word8, Word8, Word8) -> (Word8, Word8)
fourBitTripleToSixBitPair (x, y, z) = (x + 16 * (getTwoBits y 0),
                                       (getTwoBits y 2) + z * 4)

flatten :: [(a, a)] -> [a]
flatten []          = []
flatten ((x1, x2):xs) = x1:x2:flatten xs

toTriples :: B.ByteString -> [(Word8, Word8, Word8)]
toTriples bs
    | B.null bs        = []
    | B.length bs == 1 = [(B.head bs, fromIntegral 0, fromIntegral 0)]
    | B.length bs == 2 = [(B.index bs 0, B.index bs 1, fromIntegral 0)]
    | otherwise
        = (B.index bs 0, B.index bs 1, B.index bs 2):toTriples (B.drop 3 bs)

getTwoBits :: (Integral a, Bits a) => a -> Int -> a
getTwoBits w index = (w `shiftR` index) .&. 3

hexCharToBinWord :: Word8 -> Word8
hexCharToBinWord w
    | isNumeralHexDigit w   = w - 48
    | isLowerCaseHexDigit w = w - 87
    | isUpperCaseHexDigit w = w - 55
    | otherwise             = error "Invalid hex character"

isNumeralHexDigit :: Word8 -> Bool
isNumeralHexDigit w = (w >= 48) && (w <= 57)

isLowerCaseHexDigit :: Word8 -> Bool
isLowerCaseHexDigit w = (w >= 97) && (w <= 102)

isUpperCaseHexDigit :: Word8 -> Bool
isUpperCaseHexDigit w = (w >= 65) && (w <= 70)

binWordToB64Char :: Word8 -> Word8
binWordToB64Char w
    | mapsToUpperCaseB64Digit w = w + 65
    | mapsToLowerCaseB64Digit w = w + 71
    | mapsToNumeralB64Digit w   = w - 4
    | w == 62                   = fromIntegral 43
    | w == 63                   = fromIntegral 47
    | otherwise                 = error "Invalid Base64 digit value"

mapsToUpperCaseB64Digit :: Word8 -> Bool
mapsToUpperCaseB64Digit w = (w >= 0) && (w <= 25)

mapsToLowerCaseB64Digit :: Word8 -> Bool
mapsToLowerCaseB64Digit w = (w >= 26) && (w <= 51)

mapsToNumeralB64Digit :: Word8 -> Bool
mapsToNumeralB64Digit w = (w >= 52) && (w <= 61)


main :: IO ()
main = do
  contents <- B.getContents
  B.putStr (hexToB64 contents)
