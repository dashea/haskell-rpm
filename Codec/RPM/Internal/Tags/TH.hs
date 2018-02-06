{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- |
-- Module: Codec.RPM.Tags
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--  
-- Maintainer: https://github.com/weldr
-- Stability: stable 
-- Portability: portable

module Codec.RPM.Internal.Tags.TH(createMkTagTmpl,
                                  createSerializeTagTmpl)
 where

import qualified Data.ByteString as BS
import           Data.Data(gmapQi, showConstr, toConstr)
import           Data.Typeable(typeOf)
import           Data.Word(Word16, Word32, Word64)
import           Language.Haskell.TH

import Codec.RPM.Internal.Tags

-- Enumerate all the types of things that can go on the right hand side of a Tag constructor
data TagType = NullType
             | StringType      -- This is both lists of individual chars, e.g. FileStates,
                               -- and string types, e.g. Name.
             | Word16Type
             | Word32Type
             | Word64Type
             | ByteStringType  -- This is both "binary" tags, e.g. SigPGP, and I18N string
                               -- tags, e.g. Description
             | StringArrayType -- [String]
             | Word16ArrayType
             | Word32ArrayType
             | Word64ArrayType
             | DependsDictType -- special case, [(Word32, Word32)]
             | TagType         -- recursive types, e.g. DEPRECATED
 deriving(Eq)

-- get the type of a tag
tagType :: Tag -> Q TagType
tagType t = do
    let ty = gmapQi 0 typeOf t
    if | ty == typeOf Null             -> return NullType
       | ty == typeOf ("" :: String)   -> return StringType
       | ty == typeOf (0  :: Word16)   -> return Word16Type
       | ty == typeOf (0  :: Word32)   -> return Word32Type
       | ty == typeOf (0  :: Word64)   -> return Word64Type
       | ty == typeOf BS.empty         -> return ByteStringType
       | ty == typeOf ([] :: [String]) -> return StringArrayType
       | ty == typeOf ([] :: [Word16]) -> return Word16ArrayType
       | ty == typeOf ([] :: [Word32]) -> return Word32ArrayType
       | ty == typeOf ([] :: [Word64]) -> return Word64ArrayType
       | ty == typeOf ([] :: [(Word32, Word32)]) -> return DependsDictType
       | ty == typeOf t                -> return TagType
       | otherwise -> fail $ "Unknown tag constructor type: " ++ show ty

-- Does the tag only contain one of its type?
-- For example, a rpmTagType of 4 could be a single Word32 or an array of Word32
-- For nested tags, recurse and return the result of the innermost tag
isSingular :: Tag -> Q Bool
isSingular t = tagType t >>= \case
    Word16Type -> return True
    Word32Type -> return True
    Word64Type -> return True
    TagType    -> unNest t >>= isSingular
    _          -> return False

unNest :: Tag -> Q Tag
unNest t = maybe (fail "Not a recursive tag") return $ tagValue t

-- get the constructor name out of a Tag
tagName :: Tag -> String
tagName t = showConstr $ toConstr t

-- Return the function body for mkTag
-- All of the symbols looked up are looked up in the scope of the calling module; i.e., Codec.RPM.Tags
{-# ANN createMkTagTmpl "HLint: ignore Use <$>" #-}
createMkTagTmpl :: Q Exp
createMkTagTmpl = do
    -- create some names for the function arguments
    let storeName  = mkName "store"
        tName      = mkName "tag"
        tyName     = mkName "ty"
        offsetName = mkName "offset"
        countName  = mkName "count"

    -- The argument list as a list of patterns
    let argList = map VarP [storeName, tName, tyName, offsetName, countName]
    
    -- The body is of the form `case tag of \n <tag number>  -> <mkType> store ty offset count >>= <constructor>`
    -- start by converting tagLibrary into a list of matches
    matchList <- mapM (mkMatch storeName tyName offsetName countName) tagLibrary

    -- Add the _ case to the end
    Just nothingName <- lookupValueName "Nothing"
    let matchList' = matchList ++ [Match WildP (NormalB $ ConE nothingName) []]

    -- wrap that in the case expression
    let caseExp = CaseE (VarE tName) matchList'

    -- wrap the case in a lambda with the argument list
    return $ LamE argList caseExp
 where
    mkMatch :: Name -> Name -> Name -> Name -> TagDescr -> Q Match
    mkMatch storeName tyName offsetName countName (tagNum, tagTy, tagTmpl) = do
        -- get names for the "." and ">>=" operators
        Just monadCompose <- lookupValueName ">>="
        Just functionCompose <- lookupValueName "."

        -- get the "Just" constructor
        Just justCons <- lookupValueName "Just"

        -- The tag number as the pattern being matched
        let pat = LitP $ IntegerL $ fromIntegral tagNum

        -- the mkThing part depends on the tag type
        Just makerFunction <- lookupValueName =<< case tagTy of
            0 -> return "mkNull"
            1 -> return "mkChar"
            -- 2 is int8, unused
            3 -> return "mkWord16"
            4 -> return "mkWord32"
            5 -> return "mkWord64"
            6 -> return "mkString"
            7 -> return "mkBinary"
            8 -> return "mkStringArray"
            9 -> return "mkI18NString"
            _ -> fail "Unknown tag type"
        let makerCall = VarE makerFunction `AppE` VarE storeName `AppE` VarE tyName `AppE` VarE offsetName `AppE` VarE countName

        -- Check if this kind of tag only takes the first of a list of arguments returned by mkType
        -- If so, add a listToMaybe
        Just listToMaybeName <- lookupValueName "listToMaybe"
        makerCallPost <- isSingular tagTmpl >>= \case
            False -> return makerCall
            True  -> return $ InfixE (Just makerCall) (VarE monadCompose) (Just $ VarE listToMaybeName)

        -- To construct a Maybe Tag value from the result of mkThing we want Just . constructor
        constructorCall <- mkConstructor tagTmpl
        let justConstructor = InfixE (Just $ ConE justCons) (VarE functionCompose) (Just constructorCall)

        -- We now want `mkThing [>>= listToMaybe] >>= Just . <constructor>`
        let body = InfixE (Just makerCallPost) (VarE monadCompose) (Just justConstructor)

        return $ Match pat (NormalB body) []

    -- rebuild the constructor from the template tag
    mkConstructor :: Tag -> Q Exp
    mkConstructor t = do
        Just functionCompose <- lookupValueName "."
        Just constructorName <- lookupValueName $ tagName t

        -- DependsDict is a special case, everything else just takes the value as-is
        -- XXX clean this up into one conditional
        ty <- tagType t
        constructor <- if ty == DependsDictType then do
            ddE <- dependsDictMap
            return $ InfixE (Just $ ConE constructorName) (VarE functionCompose) (Just ddE)
        else return $ ConE constructorName

        if ty == TagType then do
            recur <- unNest t >>= mkConstructor 
            return $ InfixE (Just constructor) (VarE functionCompose) (Just recur)
        else
            return constructor

    dependsDictMap :: Q Exp
    dependsDictMap = do
        -- The result is `map (\x -> ((x `shiftR` 24) .&. 0xff, x .&. 0x00ffffff))`
        Just mapName <- lookupValueName "map"
        Just shiftRName <- lookupValueName "shiftR"
        Just andName <- lookupValueName ".&."
        let xName = mkName "x"

        let shiftRE = InfixE (Just $ VarE xName) (VarE shiftRName) (Just $ LitE $ IntegerL 24)
            tupleLeft  = InfixE (Just shiftRE) (VarE andName) (Just $ LitE $ IntegerL 0xff)
            tupleRight = InfixE (Just $ VarE xName) (VarE andName) (Just $ LitE $ IntegerL 0x00ffffff)

        return $ VarE mapName `AppE` LamE [VarP xName] (TupE [tupleLeft, tupleRight])

createSerializeTagTmpl :: Q [Dec]
createSerializeTagTmpl = undefined
