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
  , mkSv'
  , emptySv
  , Header (Header, _headerRecord)
  , noHeader
  , mkHeader
  , Headedness (Unheaded, Headed)
  , getHeadedness
  , Separator
  , comma
  , pipe
  , tab

  , Field (Unquoted, Quoted)
  , SpacedField
  , Spaced (Spaced)
  , unescapedField
  , foldField
  , fieldContents

  , Record (Record, _fields)
  , recordSpacedFieldsIso
  , emptyRecord
  , singleField
  , Records (Records, _theRecords)
  , emptyRecords
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
  , AccValidation (AccSuccess, AccFailure)
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
  , writeSvToFile

  -- * Encoding
  , Encode (..)
  , EncodeOptions (..)
  , HasEncodeOptions (..)
  , defaultEncodeOptions
  , encode
  , encodeBuilder
  , encodeRow
  , encodeSv

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
) where

import Data.Sv.Decode
import Data.Sv.Encode
import Data.Sv.Parse
import Data.Sv.Print
import Data.Sv.Syntax
