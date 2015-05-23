module Macaroon.Caveat (Caveat(..), inspectCaveat) where

data Caveat = FirstPartyCaveat { caveatId :: String }
              | ThirdPartyCaveat { caveatId :: String
                                 , caveatLocation :: String
                                 , verificationKeyId :: String
                                 } deriving (Show)

inspectCaveat :: Caveat -> String
inspectCaveat (FirstPartyCaveat cid) = "cid " ++ cid
inspectCaveat (ThirdPartyCaveat cid cl vid) = "cid " ++ cid ++ "\n" ++
                                              "cl " ++ cl ++ "\n" ++
                                              "vid " ++ vid
