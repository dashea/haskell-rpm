-- |
-- Module: Codec.RPM.Parse
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: stable
-- Portability: portable
--
-- A module for serializing 'RPM' data from record types

{-# LANGUAGE RecordWildCards #-}

module Codec.RPM.Serialize(serializeRPM)
 where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C
import           Data.Monoid((<>))
import           Data.Word(Word32)

import Codec.RPM.Types(Header(..), Lead(..), RPM(..), SectionHeader(..))
import Codec.RPM.Tags(TagSerializer(..), serializeTag)

serializeRPM :: RPM -> BSL.ByteString
serializeRPM RPM{..} =
    let lead = serializeLead rpmLead
        -- XXX the signatures are going to be wrong for the newly serialized data
        sig  = mconcat $ map serializePaddedSection rpmSignatures
        hdr  = mconcat $ map (fst . serializeSection) rpmHeaders
        archive = BB.byteString rpmArchive
    in BB.toLazyByteString $ mconcat [lead, sig, hdr, archive]
 where
    serializePaddedSection :: Header -> BB.Builder
    serializePaddedSection hdr = let
        (section, size) = serializeSection hdr
        remainder = size `mod` 8
        pad       = if remainder > 0 then fromIntegral $ 8 - remainder else 0
     in
        section <> BB.lazyByteString (BSL.take pad $ BSL.repeat 0)

serializeLead :: Lead -> BB.Builder
serializeLead Lead{..} =
    let magic   = BB.word32BE 0xedabeedb
        major   = BB.word8 rpmMajor
        minor   = BB.word8 rpmMinor
        typ     = BB.word16BE rpmType
        archNum = BB.word16BE rpmArchNum

        -- the name is exactly 66 bytes long, 0-padded
        nameBS  = BSL.fromStrict $ C.pack rpmName
        name    = BB.lazyByteString $ BSL.take 66 $ BSL.append nameBS $ BSL.repeat 0

        osNum   = BB.word16BE rpmOSNum
        sigType = BB.word16BE rpmSigType
        reserve = BB.lazyByteString $ BSL.take 16 $ BSL.repeat 0
    in mconcat [magic, major, minor, typ, archNum, name, osNum, sigType, reserve]

serializeSection :: Header -> (BB.Builder, Word32)
serializeSection Header{..} =
    let -- Serialize all of the tags
        TagSerializer{..} = foldl serializeTag (TagSerializer mempty mempty 0) headerTags
        -- recalculate the sectionCount and sectionSize from the tag list and serialized store
        count = fromIntegral $ length headerTags
        header = serializeSectionHeader headerSectionHeader{sectionCount=count, sectionSize=tagStoreSize}
    in (mconcat [header, tagArray, tagStore], tagStoreSize)

serializeSectionHeader :: SectionHeader -> BB.Builder
serializeSectionHeader SectionHeader{..} =
    let magic   = BB.word8 0x8e <> BB.word8 0xad <> BB.word8 0xe8
        version = BB.word8 sectionVersion
        reserve = BB.lazyByteString $ BSL.take 4 $ BSL.repeat 0
        count   = BB.word32BE sectionCount
        size    = BB.word32BE sectionSize
    in mconcat [magic, version, reserve, count, size]
