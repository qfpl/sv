\begin{code}
{-# LANGUAGE OverloadedStrings #-}
\end{code}

\begin{code}

-- | This is an example of using the Sv library to parse and decode a real CSV
-- document into a Haskell datatype. We consider this a good demonstration of
-- usage of the library.
--
-- This is a literate Haskell file. If you are reading the haddock,
-- we recommend that instead you view the source to follow along.
module Main where

import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)
import System.Exit (exitFailure)

import Data.Sv

\end{code}

Here is our test file, in comma-separated-values format. Its subject is
fauna of Queensland, Australia.

The file contains a header row featuring the column names, and then
thousands of rows of data. We will load the file from disk, parse it, and
then decode each row into a regular Haskell datatype, which will be
collected in a list.

attributions.mdvc describes how this file was sourced.

It is important to mention that this file is encoded using Windows-1252,
which is a non-ISO extension to latin1 8-bit ASCII. In particular, it is
incompatible with UTF-8.

\begin{code}
file :: FilePath
file = "test/species.csv"
\end{code}

The parser needs some configuration to parse our file. For example, it
needs to know which separator character we're using - in our case: comma.
We set up our config by modifying the default config using lens, but you
could just as easily modify it with record syntax, or build a config from
scratch using the types and values defined in Data.Sv.Config

The only setting we're changing is the choice of parsing library. The
default parsing library used is Trifecta because of its very helpful
clang-style error messages. However Trifecta requires UTF-8 compatible
input and, as mentioned above, our input file is Windows-1252 encoded.
Hence we're using Attoparsec, which has less helpful error messages but
can handle this encoding.

\begin{code}
config :: SvConfig
config = defaultConfig & (parsingLib .~ Attoparsec)
\end{code}

This is the type we've made for our rows. Many of the fields are simply
textual data, so we've chosen ByteString. We recommend using Text if
your data is ASCII, UTF-8, or UTF-16.

Other fields include the ID which is an int, and several optional fields.
These optional fields are categorical data, meaning that each entry is
from a fixed set of categories or classes.

\begin{code}
data Species =
  Species {
    taxonId :: ID
  , kingdom :: ByteString
  , clazz :: ByteString
  , family :: ByteString
  , scientificName :: ByteString
  , taxonAuthor :: ByteString
  , commonName :: ByteString
  , ncaCode:: Maybe NcaCode
  , epbcCode :: Maybe EpbcCode
  , significant :: Maybe Significance
  , endemicityCode :: Maybe Endemicity
  }
  deriving Show
\end{code}

We newtype the ID so that we don't get it mixed up with any other ints.

\begin{code}
newtype ID = ID Int deriving Show
\end{code}

Here we have our first decoder. We decode an ID by decoding an int and
then mapping the ID constructor over it. `int' is a primitive decoder
defined in Data.Sv.Decode. Typically more complex decoders are made by
combining these primitive decoders. Decoders are Applicative and
Alternative, giving us access to many derived combinators. There are also
other useful combinators defined in Data.Sv.Decode.

TODO describe type parameters

\begin{code}
idDecode :: FieldDecode ByteString ByteString ID
idDecode = ID <$> int
\end{code}

This is a sum type for the different categories defined by the
Nature Conservation Act 1992. It is a simple sum type with only nullary
constructors. These are our categories...

\begin{code}
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
\end{code}

... and here is our decoder. 

\begin{code}
nca :: FieldDecode' ByteString NcaCode
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
\end{code}

Environment Protection and Biodiversity Conservation Act 1999

\begin{code}
data EpbcCode
  = EExtinct -- EX
  | EWildExtinct -- WX
  | ECriticallyEndangered -- CE
  | EEndangered -- E
  | EVulnerable -- V
  | EConservationDependent -- CD
  deriving Show

epbc :: FieldDecode' ByteString EpbcCode
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

endemicity :: FieldDecode' ByteString Endemicity
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
\end{code}

\begin{code}
data Significance
  = Y
  | N
  deriving Show

significance :: FieldDecode' ByteString Significance
significance =
  categorical [
    (Y, ["Y", "y", "yes"])
  , (N, ["N", "n", "no"])
  ]
\end{code}


\begin{code}
speciesDecoder :: FieldDecode' ByteString Species
speciesDecoder = let s = byteString in
  Species <$> idDecode <*> s <*> s <*> s <*> s <*> s <*> s <*>
    option nca <*> option epbc <*> option significance <*> option endemicity
\end{code}

\begin{code}
species :: IO (DecodeValidation ByteString [Species])
species = decodeFromFile speciesDecoder (Just config) file
\end{code}

\begin{code}
main :: IO ()
main = do
  dv <- species
  case dv of
    AccSuccess _ -> pure ()
    AccFailure e -> do
      putStrLn "Failed to parse and decode species.csv:"
      print e
      exitFailure
\end{code}
