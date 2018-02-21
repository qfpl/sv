{-|
Module      : Data.Sv
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv (
  -- * Core data types
  Sv (..)
  , mkSv
  , emptySv
  , Header (..)
  , noHeader
  , mkHeader
  , Headedness (..)
  , getHeadedness
  , Separator
  , comma
  , pipe
  , tab

  , Field (..)
  , SpacedField
  , Spaced (Spaced)
  , unescapedField
  , foldField
  , fieldContents

  , Record (..)
  , recordSpacedFieldsIso
  , emptyRecord
  , singleField
  , Records (..)
  , _EmptyRecords
  , _NonEmptyRecords
  , mkRecords
  , singleRecord
  , recordList

  -- * Decoding
  , decode
  , parseDecode
  , parseDecodeFromFile
  , decodeMay
  , decodeEither
  , decodeEither'

  , FieldDecode (..)
  , FieldDecode'
  , DecodeState (..)
  , AccValidation (..)
  , DecodeValidation
  , DecodeError (..)
  , DecodeErrors (..)

  , runFieldDecode
  , (>>==)
  , (==<<)
  , fieldDecode
  , fieldDecodeWithQuotes
  , fieldDecodeWithSpaces
  , validateMay
  , validateMay'
  , promote

  -- * Parsing
  , separatedValues
  , separatedValuesEof
  , ParseOptions (..)
  , defaultParseOptions
  , orDefault
  , ParsingLib (..)
  , defaultParsingLib

  -- * Printing
  , printSv
  , printSvLazy
  , printSvText
  , printSvTextLazy
  , printSv'
  , printSvLazy'
  , writeSvToFile
  , writeSvToHandle
  , writeSvToFile'
  , writeSvToHandle'
  , PrintOptions
  , HasPrintOptions (..)
  , defaultPrintOptions

  -- * Encoding
  , encode
  , encodeToFile
  , encodeToHandle
  , encodeBuilder
  , encodeRow
  , encodeSv
  , Encode (..)
  , EncodeOptions (..)
  , HasEncodeOptions (..)
  , defaultEncodeOptions

  -- * Lenses
  , HasSv (..)
  , HasHeader (..)
  , HasHeadedness (..)
  , HasSeparator (..)
  , HasFields (..)
  , AsField (..)
  , HasRecord (..)
  , HasRecords (..)
  , HasParseOptions (..)
  , HasParsingLib (..)

  -- * Operators
  , Alt ((<!>))
  , Applicative (pure, (<*>))
) where

import Data.Sv.Decode
import Data.Sv.Encode
import Data.Sv.Parse
import Data.Sv.Print
import Data.Sv.Syntax
