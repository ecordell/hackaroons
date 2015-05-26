module Macaroon.Macaroon (Macaroon(..)) where

import Macaroon.Caveat
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Printf
import qualified Data.ByteString.Base64.URL.Lazy as Base64
import qualified Data.ByteString.Base16.Lazy as Hex
import Data.Binary.Get
import Data.Word
import Data.Char
import Data.Bits

type Identifier = String
type Location = String
type Signature = C.ByteString
type Key = String

data Macaroon = Macaroon { identifier :: Identifier
                         , location :: Location
                         , caveats :: [Caveat]
                         , signature :: Signature
                         } deriving (Show)

baseMacaroon :: Key -> Identifier -> Location -> Macaroon
baseMacaroon key id loc = Macaroon id loc [] $ initialSignature key id

bytestringSignature :: Macaroon -> C.ByteString
bytestringSignature = signature

showSignature :: Macaroon -> String
showSignature = C.unpack . Hex.encode . signature

initialSignature :: Key -> Identifier -> Signature
initialSignature key id = bytestringDigest $ hmacSha256 derivedKey packedId
  where derivedKey = generateDerivedKey key
        packedId = C.pack id

generateDerivedKey :: Key -> Signature
generateDerivedKey key = bytestringDigest $ hmacSha256 seed packedKey
  where seed = C.pack "macaroons-key-generator"
        packedKey = C.pack key

inspect :: Macaroon -> String
inspect m = "identifier " ++ identifier m ++ "\n" ++
            "location " ++ location m ++ "\n" ++
            caveatsString ++
            "signature " ++ showSignature m
  where caveatsString = foldl (\acc c -> acc ++ inspectCaveat c ++ "\n") "" $ caveats m

addFirstPartyCaveat :: Macaroon -> String -> Macaroon
addFirstPartyCaveat macaroon predicate =
  macaroon { caveats = caveats macaroon ++ [caveat],
             signature = signFirstPartyCaveat macaroon caveat }
  where caveat = FirstPartyCaveat predicate

signFirstPartyCaveat :: Macaroon -> Caveat -> Signature
signFirstPartyCaveat m caveat = bytestringDigest $ hmacSha256 currentSig packedCaveat
  where currentSig = signature m
        packedCaveat = C.pack $ caveatId caveat

serialize :: Macaroon -> String
serialize = trimBase64 . C.unpack . Base64.encode . C.pack . formatBinary

deserialize :: String -> Macaroon
deserialize = parseBinary . C.unpack . Base64.decodeLenient . C.pack . padBase64

formatBinary :: Macaroon -> String
formatBinary m = packetize ("location " ++ location m) ++
                 packetize ("identifier " ++ identifier m) ++
                 caveatPackets ++
                 packetize ("signature " ++ C.unpack (signature m))
  where caveatPackets = foldl (\acc c -> acc ++ packetizeCaveat c) "" $ caveats m

parseBinary :: String -> Macaroon
parseBinary s = parseBinaryHelper blankMacaroon (runGet listOfPackets (C.pack s))
  where blankMacaroon = Macaroon "" "" [] $ initialSignature "" ""

parseBinaryHelper :: Macaroon -> [C.ByteString] -> Macaroon
parseBinaryHelper m (current:rest)
    | label == C.pack "identifier" = parseBinaryHelper (m {identifier = C.unpack value}) rest
    | label == C.pack "location" = parseBinaryHelper (m {location = C.unpack value}) rest
    | label == C.pack "cid" = parseBinaryHelper (m {caveats = (caveats m) ++ [caveat]}) rest
    | label == C.pack "signature" = parseBinaryHelper (m {signature = value}) rest
    | otherwise = parseBinaryHelper m rest
    where [label, value] = C.split ' ' current
          caveat = parseCaveat value rest

parseBinaryHelper m [] = m

parseCaveat :: C.ByteString -> [C.ByteString] -> Caveat
parseCaveat cid rest
  | vidLabel == C.pack "vid" = ThirdPartyCaveat (C.unpack cid) (C.unpack vidValue) (C.unpack clValue)
  | otherwise = FirstPartyCaveat (C.unpack cid)
  where [vidLabel, vidValue] = C.split ' ' (head rest)
        [clLabel, clValue] = C.split ' ' (head (tail rest))

listOfPackets :: Get [C.ByteString]
listOfPackets = do
  empty <- isEmpty
  if empty
     then return []
     else do len <- getLazyByteString 4
             packet <- getLazyByteString (fromIntegral (hexToInt len) - 5)
             skip 1 -- newline
             rest <- listOfPackets
             return (packet : rest)

hexToInt :: C.ByteString -> Int
hexToInt hex = C.foldl' f 0 hex
  where f n c = 16*n + digitToInt c

packetize :: String  -> String
packetize s = (printf "%04x" (prefix_length + length s + newline_length)) ++ s ++ "\n"
  where prefix_length = 4
        newline_length = 1

packetizeCaveat :: Caveat  -> String
packetizeCaveat (FirstPartyCaveat p) = packetize $ "cid " ++ p
packetizeCaveat (ThirdPartyCaveat p loc id) = (packetize $ "cid " ++ p) ++
                                              (packetize $ "vid " ++ id) ++
                                              (packetize $ "cl " ++ loc)

trimBase64 :: String -> String
trimBase64 = reverse . dropWhile isEqualSign . reverse

padBase64 :: String -> String
padBase64 s
  | len `mod` 4 == 0 = s
  | otherwise = s ++ padding
  where len = length s
        padding = replicate ((-len) `mod` 4) '='

isEqualSign :: Char -> Bool
isEqualSign c
    | c == '=' = True
    | otherwise = False
