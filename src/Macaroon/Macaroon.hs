module Macaroon.Macaroon (Macaroon(..)) where

import Macaroon.Caveat
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as C

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

