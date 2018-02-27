{-|
Module      : Data.Sv
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv (
  -- * Decoding
    decode
  , parseDecode
  , parseDecode'
  , parseDecodeFromFile
  , parseDecodeFromFile'
  , decodeMay
  , decodeEither
  , decodeEither'

  , FieldDecode (..)
  , FieldDecode'
  , DecodeState (..)
  , Validation (..)
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
  , parseSv
  , parseSv'
  , parseSvFromFile
  , parseSvFromFile'
  , separatedValues
  , ParseOptions (..)
  , Headedness (..)
  , defaultParseOptions
  , orDefault
  , defaultHeadedness
  , defaultSeparator
  , SvParser (..)
  , trifecta
  , attoparsecByteString
  , attoparsecText

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
  , defaultEncodeOptions

  -- * Core data types
  , Sv (..)
  , mkSv
  , emptySv
  , Header (..)
  , noHeader
  , mkHeader
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
  , HasPrintOptions (..)
  , HasEncodeOptions (..)

  -- * Operators
  , Alt ((<!>))
  , Applicative (pure, (<*>))
) where

import Data.Sv.Decode
import Data.Sv.Encode
import Data.Sv.Parse
import Data.Sv.Print
import Data.Sv.Syntax
