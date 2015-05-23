module Macaroon.Macaroon (Macaroon(..)) where

import Macaroon.Caveat
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as C

type Identifier = String
type Location = String
type Signature = String
type Key = String

data Macaroon = Macaroon { identifier :: Identifier
                         , location :: Location
                         , caveats :: [Caveat]
                         , signature :: Signature
                         } deriving (Show)


baseMacaroon :: Key -> Identifier -> Location -> Macaroon
baseMacaroon key id loc = Macaroon id loc [] $ initialSignature key id


initialSignature :: Key -> Identifier -> Signature
initialSignature key id = showDigest $ hmacSha256 derivedKey packedId
  where derivedKey = generateDerivedKey key
        packedId = C.pack id

generateDerivedKey :: Key -> C.ByteString
generateDerivedKey key = bytestringDigest $ hmacSha256 seed packedKey
  where seed = C.pack "macaroons-key-generator"
        packedKey = C.pack key

inspect :: Macaroon -> String
inspect m = "identifier " ++ identifier m ++ "\n" ++
            "location " ++ location m ++ "\n" ++
            caveatsString ++
            "signature " ++ signature m
  where caveatsString = foldl (\acc c -> acc ++ inspectCaveat c ++ "\n") "" $ caveats m

