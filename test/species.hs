{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)
import System.Exit (exitFailure)

import Data.Sv

file :: FilePath
file = "test/species.csv"

config :: SvConfig
config = defaultConfig & (parsingLib .~ Attoparsec)

species :: IO (DecodeValidation S [Species])
species = decodeFromFile speciesDecoder (Just config) file

type S = ByteString
s :: FieldDecode' ByteString ByteString
s = byteString

data Species =
  Species {
    taxonId :: Int
  , kingdom :: S
  , clazz :: S
  , family :: S
  , scientificName :: S
  , taxonAuthor :: S
  , commonName :: S
  , ncaCode:: Maybe NcaCode
  , epbcCode :: Maybe EpbcCode
  , significant :: Maybe Significance
  , endemicityCode :: Maybe Endemicity
  }
  deriving Show

-- Nature Conservation Act 1992
data NcaCode
  = NExtinct -- PE
  | NCriticallyEndangered -- CE
  | NEndangered -- E
  | NVulnerable -- V
  | NNearThreatened -- NT
  | NSpecialLeastConcern -- SL
  | NLeastConcern -- C
  | NInternational -- I
  | NProhibited -- P
  deriving Show

nca :: FieldDecode' S NcaCode
nca =
  categorical [
    (NExtinct, ["PE"])
  , (NCriticallyEndangered, ["CE"])
  , (NEndangered, ["E"])
  , (NVulnerable, ["V"])
  , (NNearThreatened, ["NT"])
  , (NSpecialLeastConcern, ["SL"])
  , (NLeastConcern, ["C"])
  , (NInternational, ["I"])
  , (NProhibited, ["P"])
  ]

-- Environment Protection and Biodiversity Conservation Act 1999
data EpbcCode
  = EExtinct -- EX
  | EWildExtinct -- WX
  | ECriticallyEndangered -- CE
  | EEndangered -- E
  | EVulnerable -- V
  | EConservationDependent -- CD
  deriving Show

epbc :: FieldDecode' S EpbcCode
epbc =
  categorical [
    (EExtinct, ["EX"])
  , (EWildExtinct, ["WX"])
  , (ECriticallyEndangered, ["CE"])
  , (EEndangered, ["E"])
  , (EVulnerable, ["V"])
  , (EConservationDependent, ["CD"])
  ]

data Endemicity
  = QueenslandEndemic -- Q
  | AustralianEndemic -- QA
  | QldAndInternational -- QI
  | AustraliaAndInternational -- QAI
  | QldNativeUndefinedEndemicity -- U
  | NaturalisedFromOverseas -- II
  | NaturalisedFromElsewhereInAus -- IA
  | NaturalisedFromUndefinedOrigin -- IU
  | VagrantAustralian -- VA
  | VagrantInternational -- VI
  | VagrantUndefined -- VU
  deriving Show

endemicity :: FieldDecode' S Endemicity
endemicity =
  categorical [
    (QueenslandEndemic, ["Q"])
  , (AustralianEndemic, ["QA"])
  , (QldAndInternational, ["QI"])
  , (AustraliaAndInternational, ["QAI"])
  , (QldNativeUndefinedEndemicity, ["U"])
  , (NaturalisedFromOverseas, ["II"])
  , (NaturalisedFromElsewhereInAus, ["IA"])
  , (NaturalisedFromUndefinedOrigin, ["IU"])
  , (VagrantAustralian, ["VA"])
  , (VagrantInternational, ["VI"])
  , (VagrantUndefined, ["VU"])
  ]

data Significance
  = Y
  | N
  deriving Show

significance :: FieldDecode' S Significance
significance =
  categorical [
    (Y, ["Y", "y", "yes"])
  , (N, ["N", "n", "no"])
  ]

speciesDecoder :: FieldDecode' S Species
speciesDecoder =
  Species <$> int <*> s <*> s <*> s <*> s <*> s <*> s <*>
    option nca <*> option epbc <*> option significance <*> option endemicity

main :: IO ()
main = do
  dv <- species
  case dv of
    AccSuccess _ -> pure ()
    AccFailure e -> do
      putStrLn "Failed to parse and decode species.csv:"
      print e
      exitFailure