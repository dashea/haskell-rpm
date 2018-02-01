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

import Codec.RPM.Types(Header(..), Lead(..), RPM(..))

serializeRPM :: RPM -> BSL.ByteString
serializeRPM RPM{..} =
    let lead = serializeLead rpmLead
        -- XXX signature padding?
        sig  = mconcat $ map serializeSection rpmSignatures
        hdr  = mconcat $ map serializeSection rpmHeaders
        archive = BB.byteString rpmArchive
    in BB.toLazyByteString $ mconcat [lead, sig, hdr, archive]

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

serializeSection :: Header -> BB.Builder
serializeSection = undefined
