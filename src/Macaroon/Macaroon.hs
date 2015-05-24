module Macaroon.Macaroon (Macaroon(..)) where

import Macaroon.Caveat
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Printf
import qualified Data.ByteString.Base64.URL.Lazy as Base64

type Identifier = String
type Location = String
type Signature = Digest SHA256State
type Key = String

data Macaroon = Macaroon { identifier :: Identifier
                         , location :: Location
                         , caveats :: [Caveat]
                         , signature :: Signature
                         } deriving (Show)


baseMacaroon :: Key -> Identifier -> Location -> Macaroon
baseMacaroon key id loc = Macaroon id loc [] $ initialSignature key id

bytestringSignature :: Macaroon -> C.ByteString
bytestringSignature = bytestringDigest . signature

showSignature :: Macaroon -> String
showSignature = showDigest . signature

initialSignature :: Key -> Identifier -> Signature
initialSignature key id = hmacSha256 derivedKey packedId
  where derivedKey = bytestringDigest $ generateDerivedKey key
        packedId = C.pack id

generateDerivedKey :: Key -> Signature
generateDerivedKey key = hmacSha256 seed packedKey
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
signFirstPartyCaveat currentSig caveat = hmacSha256 packedCurrentSig packedCaveat
  where packedCurrentSig = bytestringSignature currentSig
        packedCaveat = C.pack $ caveatId caveat

serialize :: Macaroon -> String
serialize = trimBase64. C.unpack . Base64.encode . C.pack . formatBinary

formatBinary :: Macaroon -> String
formatBinary m = packetize ("identifier " ++ identifier m) ++
              packetize ("location " ++ location m) ++
              caveatPackets ++
              packetize ("signature " ++ showDigest (signature m))
  where caveatPackets = foldl (\acc c -> acc ++ packetizeCaveat c) "" $ caveats m

packetize :: String  -> String
packetize s = (printf "%04x" (length s)) ++ s ++ "\n"

packetizeCaveat :: Caveat  -> String
packetizeCaveat (FirstPartyCaveat p) = packetize "cid " ++ p
packetizeCaveat (ThirdPartyCaveat p loc id) = (packetize "cid " ++ p) ++
                                              (packetize "vid " ++ id) ++
                                              (packetize "cl " ++ loc)

trimBase64 :: String -> String
trimBase64 = reverse . dropWhile isEqualSign . reverse

isEqualSign :: Char -> Bool
isEqualSign c
    | c == '=' = True
    | otherwise = False
