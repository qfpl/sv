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
  , HasSv (..)
  , mkSv
  , mkSv'
  , empty
  , Header (Header, _headerRecord)
  , HasHeader (header, headerRecord, headerNewline)
  , noHeader
  , mkHeader
  , Headedness (Unheaded, Headed)
  , HasHeadedness (headedness)
  , getHeadedness
  , Separator
  , HasSeparator (separator)
  , comma
  , pipe
  , tab

  , Field (Unquoted, Quoted)
  , SpacedField
  , Spaced (Spaced)
  , HasFields (fields)
  , AsField (_Field, _Unquoted, _Quoted)
  , unescapedField
  , foldField
  , fieldContents

  , Record (Record, _fields)
  , HasRecord (record, spacedFields)
  , recordSpacedFieldsIso
  , emptyRecord
  , singleField
  , Records (Records, _theRecords)
  , HasRecords (records, theRecords, traverseRecords)
  , emptyRecords
  , singleRecord
  , recordList

  -- * Decoding
  , decode
  , parseDecode
  , decodeFromFile
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
  , HasParseOptions (..)
  , defaultParseOptions
  , orDefault
  , ParsingLib (..)
  , HasParsingLib (..)
  , defaultParsingLib

  -- * Printing
  , printSv
  , printSvLazy
  , writeSvToFile

  , module Data.Sv.Encode.Type
  , module Data.Sv.Encode.Options
) where

import Data.Sv.Decode
import Data.Sv.Encode.Options
import Data.Sv.Encode.Type
import Data.Sv.Parse
import Data.Sv.Print
import Data.Sv.Syntax
