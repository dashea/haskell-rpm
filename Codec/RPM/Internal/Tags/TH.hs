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

module Codec.RPM.Internal.Tags.TH(createMkTag,
                                  createSerializeTag)
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
{-# ANN createMkTag "HLint: ignore Use <$>" #-}
createMkTag :: Q Exp
createMkTag = do
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

        let constructorE = ConE constructorName

        ty <- tagType t
        case ty of
            -- DependsDict is a special case that needs to modify the argument coming in
            DependsDictType -> do
                ddE <- dependsDictMap
                return $ InfixE (Just constructorE) (VarE functionCompose) (Just ddE)

            -- For nested tag types, recurse on the inner tag and add that to the outer constructor
            TagType -> do
                recur <- unNest t >>= mkConstructor
                return $ InfixE (Just constructorE) (VarE functionCompose) (Just recur)

            _ -> return constructorE

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

createSerializeTag :: Q Exp
createSerializeTag = do
    -- create some names for the function arguments
    let serialName = mkName "serializer"
        tName      = mkName "tag"

    -- the argument list as a list of patterns
    let argList = map VarP [serialName, tName]

    -- The body is of the form `case tag of \n <constructor> <argument> -> <emit bytes>`
    -- The nested types (INTERNAL, DEPRECATED, ...) are not represented in the byte stream, so start
    -- with some matches that just unwrap those types.
    -- There might be a way to find these constructors with Data or Typeable but it seems like a lot of trouble
    nestedMatches <- mapM (mkNested serialName) ["DEPRECATED", "INTERNAL", "OBSOLETE", "UNIMPLEMENTED", "UNUSED"]

    -- convert tagLibrary into the rest of the matches
    matchList <- mapM (mkMatch serialName) tagLibrary

    -- If tagLibrary is complete, that will cover every possible Tag case. If not, we'll get a warning
    -- at compile time when the template is run.

    -- wrap the matches in a case expression
    let caseExp = CaseE (VarE tName) (nestedMatches ++ matchList)

    -- wrap the case in a lambda with the argument list
    return $ LamE argList caseExp
 where
    mkNested :: Name -> String -> Q Match
    mkNested serialName constructor = do
        -- get the function name to recurse
        Just serializeTagName <- lookupValueName "serializeTag"

        -- The pattern is `<constructor> t` where t is the Tag parameter we're going to recurse on
        let conName = mkName constructor
            tName   = mkName "t"
            pat     = ConP conName [VarP tName]

        -- The body is the recursive function application applied to 't'
        let body = NormalB $ VarE serializeTagName `AppE` VarE serialName `AppE` VarE tName
        return $ Match pat body []

    mkMatch :: Name -> TagDescr -> Q Match
    mkMatch serialName (tagNum, tagTy, tagTmpl) = do
        -- If this is a nested type, unnest it until it isn't
        typeEnum <- tagType tagTmpl
        if typeEnum == TagType then do
            inner <- unNest tagTmpl
            mkMatch serialName (tagNum, tagTy, inner)
        else do
            -- Create the pattern match for this tag constructor
            Just constructorName <- lookupValueName $ tagName tagTmpl
            let argName = mkName "t"
            let pat = ConP constructorName [VarP argName]

            -- Figure out which serializer function to use
            Just serializerFunction <- lookupValueName =<< case tagTy of
                0 -> return "serializeNull"
                1 -> return "serializeChar"
                -- 2 is int8, unused
                3 -> return "serializeWord16"
                4 -> return "serializeWord32"
                5 -> return "serializeWord64"
                6 -> return "serializeString"
                7 -> return "serializeBinary"
                8 -> return "serializeStringArray"
                9 -> return "serializeI18NString"
                _ -> fail "Unknown tag type"

            -- The first three arguments to the serializer function are easy: TagSerializer, tagNum, tagTy
            let serializerPartial = VarE serializerFunction `AppE`
                                    VarE serialName `AppE`
                                    LitE (IntegerL $ fromIntegral tagNum) `AppE`
                                    LitE (IntegerL $ fromIntegral tagTy)

            -- For the last argument:
            -- if the tag is a singular type, wrap it in a list
            -- if the tag is DependsDict, pack the data back into a [Word32]
            singular <- isSingular tagTmpl

            lastArg <- if | singular                    -> return $ ListE [VarE argName]
                          | typeEnum == DependsDictType -> dependsDictMap argName
                          | otherwise                   -> return $ VarE argName

            return $ Match pat (NormalB $ serializerPartial `AppE` lastArg) []

    dependsDictMap :: Name -> Q Exp
    dependsDictMap argName = do
        -- the result is `map (\(x, y) -> (x `shiftL` 24) .|. y) t`
        Just mapName <- lookupValueName "map"
        Just shiftLName <- lookupValueName "shiftL"
        Just orName <- lookupValueName ".|."

        let xName   = mkName "x"
            yName   = mkName "y"
            shiftLE = InfixE (Just $ VarE xName) (VarE shiftLName) (Just $ LitE $ IntegerL 24)
            orE     = InfixE (Just shiftLE) (VarE orName) (Just $ VarE yName)
            lambda  = LamE [TupP [VarP xName, VarP yName]] orE

        return $ VarE mapName `AppE` lambda `AppE` VarE argName
