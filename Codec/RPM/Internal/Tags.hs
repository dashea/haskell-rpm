{-# LANGUAGE DeriveDataTypeable #-}

module Codec.RPM.Internal.Tags(Tag(..),
                               Null(..),
                               TagDescr,
                               tagLibrary,
                               tagValue)
 where

import qualified Data.ByteString as BS
import           Data.Data(Data, gmapQi)
import           Data.Word
import           Data.Typeable(Typeable, cast)
import           Text.PrettyPrint.HughesPJClass(Pretty(..))
import           Text.PrettyPrint(text)

-- The character lists are actually lists of characters, ignore the suggestions
-- to use String instead
{-# ANN module "HLint: ignore Use String" #-}

-- | A very large data type that holds all the possibilities for the various tags that can
-- be contained in an 'Codec.RPM.Types.RPM' 'Codec.RPM.Types.Header'.  Each tag describes
-- one piece of metadata.  Most tags include some typed value, such as a 'String' or
-- 'Word32'.  Many tags contain lists of these values, for instance any tag involving files
-- or changelog entries.  Some tags contain no useful value at all.
--
-- Because there are so many possibilities for tags and each 'Codec.RPM.Types.RPM' likely
-- contains dozens of tags, it is unwieldy to write functions that pattern match on tags and
-- take some action.  This module therefore provides a variety of find*Tag functions for
-- searching the list of tags by name and returning a 'Maybe' value.  The name provided to
-- each should be the constructor you are looking for in this data type.
--
-- To find the list of all files in the RPM, you would therefore do:
--
-- > findStringTag "FileNames" tags
data Tag = DEPRECATED                   Tag
         | INTERNAL                     Tag
         | OBSOLETE                     Tag
         | UNIMPLEMENTED                Tag
         | UNUSED                       Tag

         | HeaderImage                  Null
         | HeaderSignatures             Null
         | HeaderImmutable              Null
         | HeaderRegions                Null
         | HeaderI18NTable              [String]

         | SigBase                      Null
         | SigSize                      Word32
         | SigLEMD5_1                   Null
         | SigPGP                       BS.ByteString
         | SigLEMD5_2                   Null
         | SigMD5                       BS.ByteString
         | SigGPG                       BS.ByteString
         | SigPGP5                      Null
         | SigBadSHA1_1                 Null
         | SigBadSHA1_2                 Null
         | PubKeys                      [String]
         | DSAHeader                    BS.ByteString
         | RSAHeader                    BS.ByteString
         | SHA1Header                   String
         | LongSigSize                  Word64
         | LongArchiveSize              Word64

         | Name                         String
         | Version                      String
         | Release                      String
         | Epoch                        Word32
         | Summary                      BS.ByteString
         | Description                  BS.ByteString
         | BuildTime                    Word32
         | BuildHost                    String
         | InstallTime                  Word32
         | Size                         Word32
         | Distribution                 String
         | Vendor                       String
         | GIF                          BS.ByteString
         | XPM                          BS.ByteString
         | License                      String
         | Packager                     String
         | Group                        BS.ByteString
         | ChangeLog                    [String]
         | Source                       [String]
         | Patch                        [String]
         | URL                          String
         | OS                           String
         | Arch                         String
         | PreIn                        String
         | PostIn                       String
         | PreUn                        String
         | PostUn                       String
         | OldFileNames                 [String]
         | FileSizes                    [Word32]
         | FileStates                   [Char]
         | FileModes                    [Word16]
         | FileUIDs                     [Word32]
         | FileGIDs                     [Word32]
         | FileRDevs                    [Word16]
         | FileMTimes                   [Word32]
         | FileMD5s                     [String]
         | FileLinkTos                  [String]
         | FileFlags                    [Word32]
         | Root                         Null
         | FileUserName                 [String]
         | FileGroupName                [String]
         | Exclude                      Null
         | Exclusive                    Null
         | Icon                         BS.ByteString
         | SourceRPM                    String
         | FileVerifyFlags              [Word32]
         | ArchiveSize                  Word32
         | ProvideName                  [String]
         | RequireFlags                 [Word32]
         | RequireName                  [String]
         | RequireVersion               [String]
         | NoSource                     [Word32]
         | NoPatch                      [Word32]
         | ConflictFlags                [Word32]
         | ConflictName                 [String]
         | ConflictVersion              [String]
         | DefaultPrefix                String
         | BuildRoot                    String
         | InstallPrefix                String
         | ExcludeArch                  [String]
         | ExcludeOS                    [String]
         | ExclusiveArch                [String]
         | ExclusiveOS                  [String]
         | AutoReqProv                  String
         | RPMVersion                   String
         | TriggerScripts               [String]
         | TriggerName                  [String]
         | TriggerVersion               [String]
         | TriggerFlags                 [Word32]
         | TriggerIndex                 [Word32]
         | VerifyScript                 String
         | ChangeLogTime                [Word32]
         | ChangeLogName                [String]
         | ChangeLogText                [String]
         | BrokenMD5                    Null
         | PreReq                       Null
         | PreInProg                    [String]
         | PostInProg                   [String]
         | PreUnProg                    [String]
         | PostUnProg                   [String]
         | BuildArchs                   [String]
         | ObsoleteName                 [String]
         | VerifyScriptProg             [String]
         | TriggerScriptProg            [String]
         | DocDir                       Null
         | Cookie                       String
         | FileDevices                  [Word32]
         | FileINodes                   [Word32]
         | FileLangs                    [String]
         | Prefixes                     [String]
         | InstPrefixes                 [String]
         | TriggerIn                    Null
         | TriggerUn                    Null
         | TriggerPostUn                Null
         | AutoReq                      Null
         | AutoProv                     Null
         | Capability                   Word32
         | SourcePackage                Word32
         | OldOrigFileNames             Null
         | BuildPreReq                  Null
         | BuildRequires                Null
         | BuildConflicts               Null
         | BuildMacros                  Null
         | ProvideFlags                 [Word32]
         | ProvideVersion               [String]
         | ObsoleteFlags                [Word32]
         | ObsoleteVersion              [String]
         | DirIndexes                   [Word32]
         | BaseNames                    [String]
         | DirNames                     [String]
         | OrigDirIndexes               [Word32]
         | OrigBaseNames                [String]
         | OrigDirNames                 [String]
         | OptFlags                     String
         | DistURL                      String
         | PayloadFormat                String
         | PayloadCompressor            String
         | PayloadFlags                 String
         | InstallColor                 Word32
         | InstallTID                   Word32
         | RemoveTID                    Word32
         | SHA1RHN                      Null
         | RHNPlatform                  String
         | Platform                     String
         | PatchesName                  [String]
         | PatchesFlags                 [Word32]
         | PatchesVersion               [String]
         | CacheCTime                   Word32
         | CachePkgPath                 String
         | CachePkgSize                 Word32
         | CachePkgMTime                Word32
         | FileColors                   [Word32]
         | FileClass                    [Word32]
         | ClassDict                    [String]
         | FileDependsX                 [Word32]
         | FileDependsN                 [Word32]
         | DependsDict                  [(Word32, Word32)]
         | SourcePkgID                  BS.ByteString
         | FileContexts                 [String]
         | FSContexts                   [String]
         | ReContexts                   [String]
         | Policies                     [String]
         | PreTrans                     String
         | PostTrans                    String
         | PreTransProg                 [String]
         | PostTransProg                [String]
         | DistTag                      String
         | OldSuggestsName              [String]
         | OldSuggestsVersion           [String]
         | OldSuggestsFlags             [Word32]
         | OldEnhancesName              [String]
         | OldEnhancesVersion           [String]
         | OldEnhancesFlags             [Word32]
         | Priority                     [Word32]
         | CVSID                        String
         | BLinkPkgID                   [String]
         | BLinkHdrID                   [String]
         | BLinkNEVRA                   [String]
         | FLinkPkgID                   [String]
         | FLinkHdrID                   [String]
         | FLinkNEVRA                   [String]
         | PackageOrigin                String
         | TriggerPreIn                 Null
         | BuildSuggests                Null
         | BuildEnhances                Null
         | ScriptStates                 [Word32]
         | ScriptMetrics                [Word32]
         | BuildCPUClock                Word32
         | FileDigestAlgos              [Word32]
         | Variants                     [String]
         | XMajor                       Word32
         | XMinor                       Word32
         | RepoTag                      String
         | Keywords                     [String]
         | BuildPlatforms               [String]
         | PackageColor                 Word32
         | PackagePrefColor             Word32
         | XattrsDict                   [String]
         | FileXattrsx                  [Word32]
         | DepAttrsDict                 [String]
         | ConflictAttrsx               [Word32]
         | ObsoleteAttrsx               [Word32]
         | ProvideAttrsx                [Word32]
         | RequireAttrsx                [Word32]
         | BuildProvides                Null
         | BuildObsoletes               Null
         | DBInstance                   Word32
         | NVRA                         String

         | FileNames                    [String]
         | FileProvide                  [String]
         | FileRequire                  [String]
         | FSNames                      [String]
         | FSSizes                      [Word64]
         | TriggerConds                 [String]
         | TriggerType                  [String]
         | OrigFileNames                [String]
         | LongFileSizes                [Word64]
         | LongSize                     Word64
         | FileCaps                     [String]
         | FileDigestAlgo               Word32
         | BugURL                       String
         | EVR                          String
         | NVR                          String
         | NEVR                         String
         | NEVRA                        String
         | HeaderColor                  Word32
         | Verbose                      Word32
         | EpochNum                     Word32
         | PreInFlags                   Word32
         | PostInFlags                  Word32
         | PreUnFlags                   Word32
         | PostUnFlags                  Word32
         | PreTransFlags                Word32
         | PostTransFlags               Word32
         | VerifyScriptFlags            Word32
         | TriggerScriptFlags           [Word32]
         | Collections                  [String]
         | PolicyNames                  [String]
         | PolicyTypes                  [String]
         | PolicyTypesIndexes           [Word32]
         | PolicyFlags                  [Word32]
         | PolicyVCS                    String
         | OrderName                    [String]
         | OrderVersion                 [String]
         | OrderFlags                   [Word32]
         | MSSFManifest                 [String]
         | MSSFDomain                   [String]
         | InstFileNames                [String]
         | RequireNEVRs                 [String]
         | ProvideNEVRs                 [String]
         | ObsoleteNEVRs                [String]
         | ConflictNEVRs                [String]
         | FileNLinks                   [Word32]
         | RecommendName                [String]
         | RecommendVersion             [String]
         | RecommendFlags               [Word32]
         | SuggestName                  [String]
         | SuggestVersion               [String]
         | SuggestFlags                 [Word32]
         | SupplementName               [String]
         | SupplementVersion            [String]
         | SupplementFlags              [Word32]
         | EnhanceName                  [String]
         | EnhanceVersion               [String]
         | EnhanceFlags                 [Word32]
         | RecommendNEVRs               [String]
         | SuggestNEVRs                 [String]
         | SupplementNEVRs              [String]
         | EnhanceNEVRs                 [String]
         | Encoding                     String
         | FileTriggerIn                Null
         | FileTriggerUn                Null
         | FileTriggerPostUn            Null
         | FileTriggerScripts           [String]
         | FileTriggerScriptProg        [String]
         | FileTriggerScriptFlags       [Word32]
         | FileTriggerName              [String]
         | FileTriggerIndex             [Word32]
         | FileTriggerVersion           [String]
         | FileTriggerFlags             [Word32]
         | TransFileTriggerIn           Null
         | TransFileTriggerUn           Null
         | TransFileTriggerPostUn       Null
         | TransFileTriggerScripts      [String]
         | TransFileTriggerScriptProg   [String]
         | TransFileTriggerScriptFlags  [Word32]
         | TransFileTriggerName         [String]
         | TransFileTriggerIndex        [Word32]
         | TransFileTriggerVersion      [String]
         | TransFileTriggerFlags        [Word32]
         | RemovePathPostFixes          String
         | FileTriggerPriorities        [Word32]
         | TransFileTriggerPriorities   [Word32]
         | FileTriggerConds             [String]
         | FileTriggerType              [String]
         | TransFileTriggerConds        [String]
         | TransFileTriggerType         [String]
         | FileSignatures               [String]
         | FileSignatureLength          Word32
  deriving(Eq, Show, Data, Typeable)

instance Pretty Tag where
    -- This is a lot quicker than having to provide a Pretty instance that takes every
    -- single Tag into account.
    pPrint = text . show

-- | Some 'Tag's do not contain any value, likely because support for that tag has been
-- removed.  RPM never removes a tag from its list of known values, however, so we must
-- still recognize them.  These tags have a special value of 'Null', which contains no
-- value.
data Null = Null
 deriving(Eq, Show, Data, Typeable)

-- | Given a 'Tag', return its value.  This is a helper function to be used with 'findTag',
-- essentially as a type-safe way to cast the value into a known type.  It is used internally
-- in all the type-specific find*Tag functions but can also be used on its own.  A function
-- to find the Epoch tag could be written as follows:
--
-- > epoch = findTag "Epoch" tags >>= \t -> tagValue t :: Maybe Word32
tagValue :: Typeable a => Tag -> Maybe a
tagValue = gmapQi 0 cast

-- This is used by Codec.RPM.Tags.TH to generate the parsing and serializing functions
-- Each tuple is the tag number, the tag type, and a template value
type TagDescr = (Int, Word32, Tag)

tagLibrary :: [TagDescr]
tagLibrary = [(61,   0, HeaderImage Null),
              (62,   0, HeaderSignatures Null),
              (63,   0, HeaderImmutable Null),
              (64,   0, HeaderRegions Null),
              (100,  8, HeaderI18NTable []),
              (256,  0, SigBase Null),
              (257,  4, SigSize 0),
              (258,  0, INTERNAL (OBSOLETE (SigLEMD5_1 Null))),
              (259,  7, SigPGP BS.empty),
              (260,  0, INTERNAL (OBSOLETE (SigLEMD5_2 Null))),
              (261,  7, SigMD5 BS.empty),
              (262,  7, SigGPG BS.empty),
              (263,  0, INTERNAL (OBSOLETE (SigPGP5 Null))),
              (264,  0, INTERNAL (OBSOLETE (SigBadSHA1_1 Null))),
              (265,  0, INTERNAL (OBSOLETE (SigBadSHA1_2 Null))),
              (266,  8, PubKeys []),
              (267,  7, DSAHeader BS.empty),
              (268,  7, RSAHeader BS.empty),
              (269,  6, SHA1Header ""),
              (270,  5, LongSigSize 0),
              (271,  5, LongArchiveSize 0),

              (1000, 6, Name ""),
              (1001, 6, Version ""),
              (1002, 6, Release ""),

              (1003, 4, Epoch 0),
              (1004, 9, Summary BS.empty),
              (1005, 9, Description BS.empty),
              (1006, 4, BuildTime 0),
              (1007, 6, BuildHost ""),
              (1008, 4, InstallTime 0),
              (1009, 4, Size 0),
              (1010, 6, Distribution ""),
              (1011, 6, Vendor ""),
              (1012, 7, GIF BS.empty),
              (1013, 7, XPM BS.empty),
              (1014, 6, License ""),
              (1015, 6, Packager ""),
              (1016, 9, Group BS.empty),
              (1017, 8, INTERNAL (ChangeLog [])),
              (1018, 8, Source []),
              (1019, 8, Patch []),
              (1020, 6, URL ""),
              (1021, 6, OS ""),
              (1022, 6, Arch ""),
              (1023, 6, PreIn ""),
              (1024, 6, PostIn ""),
              (1025, 6, PreUn ""),
              (1026, 6, PostUn ""),
              (1027, 8, OBSOLETE (OldFileNames [])),
              (1028, 4, FileSizes []),
              (1029, 1, FileStates []),
              (1030, 3, FileModes []),
              (1031, 4, INTERNAL (OBSOLETE (FileUIDs []))),
              (1032, 4, INTERNAL (OBSOLETE (FileGIDs []))),
              (1033, 3, FileRDevs []),
              (1034, 4, FileMTimes []),
              (1035, 8, FileMD5s []),
              (1036, 8, FileLinkTos []),
              (1037, 4, FileFlags []),
              (1038, 0, INTERNAL (OBSOLETE (Root Null))),
              (1039, 8, FileUserName []),
              (1040, 8, FileGroupName []),
              (1041, 0, INTERNAL (OBSOLETE (Exclude Null))),
              (1042, 0, INTERNAL (OBSOLETE (Exclusive Null))),
              (1043, 7, Icon BS.empty),
              (1044, 6, SourceRPM ""),
              (1045, 4, FileVerifyFlags []),
              (1046, 4, ArchiveSize 0),
              (1047, 8, ProvideName []),
              (1048, 4, RequireFlags []),
              (1049, 8, RequireName []),
              (1050, 8, RequireVersion []),
              (1051, 4, NoSource []),
              (1052, 4, NoPatch []),
              (1053, 4, ConflictFlags []),
              (1054, 8, ConflictName []),
              (1055, 8, ConflictVersion []),
              (1056, 6, INTERNAL (DEPRECATED (DefaultPrefix ""))),
              (1057, 6, INTERNAL (OBSOLETE (BuildRoot ""))),
              (1058, 6, INTERNAL (DEPRECATED (InstallPrefix ""))),
              (1059, 8, ExcludeArch []),
              (1060, 8, ExcludeOS []),
              (1061, 8, ExclusiveArch []),
              (1062, 8, ExclusiveOS []),
              (1063, 6, INTERNAL (AutoReqProv "")),
              (1064, 6, RPMVersion ""),
              (1065, 8, TriggerScripts []),
              (1066, 8, TriggerName []),
              (1067, 8, TriggerVersion []),
              (1068, 4, TriggerFlags []),
              (1069, 4, TriggerIndex []),
              (1079, 6, VerifyScript ""),
              (1080, 4, ChangeLogTime []),
              (1081, 8, ChangeLogName []),
              (1082, 8, ChangeLogText []),
              (1083, 0, INTERNAL (OBSOLETE (BrokenMD5 Null))),
              (1084, 0, INTERNAL (PreReq Null)),
              (1085, 8, PreInProg []),
              (1086, 8, PostInProg []),
              (1087, 8, PreUnProg []),
              (1088, 8, PostUnProg []),
              (1089, 8, BuildArchs []),
              (1090, 8, ObsoleteName []),
              (1091, 8, VerifyScriptProg []),
              (1092, 8, TriggerScriptProg []),
              (1093, 0, INTERNAL (DocDir Null)),
              (1094, 6, Cookie ""),
              (1095, 4, FileDevices []),
              (1096, 4, FileINodes []),
              (1097, 8, FileLangs []),
              (1098, 8, Prefixes []),
              (1099, 8, InstPrefixes []),
              (1100, 0, INTERNAL (TriggerIn Null)),
              (1101, 0, INTERNAL (TriggerUn Null)),
              (1102, 0, INTERNAL (TriggerPostUn Null)),
              (1103, 0, INTERNAL (AutoReq Null)),
              (1104, 0, INTERNAL (AutoProv Null)),
              (1105, 4, INTERNAL (OBSOLETE (Capability 0))),
              (1106, 4, SourcePackage 0),
              (1107, 0, INTERNAL (OBSOLETE (OldOrigFileNames Null))),
              (1108, 0, INTERNAL (BuildPreReq Null)),
              (1109, 0, INTERNAL (BuildRequires Null)),
              (1110, 0, INTERNAL (BuildConflicts Null)),
              (1111, 0, INTERNAL (UNUSED (BuildMacros Null))),
              (1112, 4, ProvideFlags []),
              (1113, 8, ProvideVersion []),
              (1114, 4, ObsoleteFlags []),
              (1115, 8, ObsoleteVersion []),
              (1116, 4, DirIndexes []),
              (1117, 8, BaseNames []),
              (1118, 8, DirNames []),
              (1119, 4, OrigDirIndexes []),
              (1120, 8, OrigBaseNames []),
              (1121, 8, OrigDirNames []),
              (1122, 6, OptFlags ""),
              (1123, 6, DistURL ""),
              (1124, 6, PayloadFormat ""),
              (1125, 6, PayloadCompressor ""),
              (1126, 6, PayloadFlags ""),
              (1127, 4, InstallColor 0),
              (1128, 4, InstallTID 0),
              (1129, 4, RemoveTID 0),
              (1130, 0, INTERNAL (OBSOLETE (SHA1RHN Null))),
              (1131, 6, INTERNAL (OBSOLETE (RHNPlatform ""))),
              (1132, 6, Platform ""),
              (1133, 8, DEPRECATED (PatchesName [])),
              (1134, 4, DEPRECATED (PatchesFlags [])),
              (1135, 8, DEPRECATED (PatchesVersion [])),
              (1136, 4, INTERNAL (OBSOLETE (CacheCTime 0))),
              (1137, 6, INTERNAL (OBSOLETE (CachePkgPath ""))),
              (1138, 4, INTERNAL (OBSOLETE (CachePkgSize 0))),
              (1139, 4, INTERNAL (OBSOLETE (CachePkgMTime 0))),
              (1140, 4, FileColors []),
              (1141, 4, FileClass []),
              (1142, 8, ClassDict []),
              (1143, 4, FileDependsX []),
              (1144, 4, FileDependsN []),
              (1145, 4, DependsDict []),
              (1146, 7, SourcePkgID BS.empty),
              (1147, 8, OBSOLETE (FileContexts [])),
              (1148, 8, FSContexts []),
              (1149, 8, ReContexts []),
              (1150, 8, Policies []),
              (1151, 6, PreTrans ""),
              (1152, 6, PostTrans ""),
              (1153, 8, PreTransProg []),
              (1154, 8, PostTransProg []),
              (1155, 6, DistTag ""),
              (1156, 8, OBSOLETE (OldSuggestsName [])),
              (1157, 8, OBSOLETE (OldSuggestsVersion [])),
              (1158, 4, OBSOLETE (OldSuggestsFlags [])),
              (1159, 8, OBSOLETE (OldEnhancesName [])),
              (1160, 8, OBSOLETE (OldEnhancesVersion [])),
              (1161, 4, OBSOLETE (OldEnhancesFlags [])),
              (1162, 4, UNIMPLEMENTED (Priority [])),
              (1163, 6, UNIMPLEMENTED (CVSID "")),
              (1164, 8, UNIMPLEMENTED (BLinkPkgID [])),
              (1165, 8, UNIMPLEMENTED (BLinkHdrID [])),
              (1166, 8, UNIMPLEMENTED (BLinkNEVRA [])),
              (1167, 8, UNIMPLEMENTED (FLinkPkgID [])),
              (1168, 8, UNIMPLEMENTED (FLinkHdrID [])),
              (1169, 8, UNIMPLEMENTED (FLinkNEVRA [])),
              (1170, 6, UNIMPLEMENTED (PackageOrigin "")),
              (1171, 0, INTERNAL (TriggerPreIn Null)),
              (1172, 0, INTERNAL (UNIMPLEMENTED (BuildSuggests Null))),
              (1173, 0, INTERNAL (UNIMPLEMENTED (BuildEnhances Null))),
              (1174, 4, UNIMPLEMENTED (ScriptStates [])),
              (1175, 4, UNIMPLEMENTED (ScriptMetrics [])),
              (1176, 4, UNIMPLEMENTED (BuildCPUClock 0)),
              (1177, 4, UNIMPLEMENTED (FileDigestAlgos [])),
              (1178, 8, UNIMPLEMENTED (Variants [])),
              (1179, 4, UNIMPLEMENTED (XMajor 0)),
              (1180, 4, UNIMPLEMENTED (XMinor 0)),
              (1181, 6, UNIMPLEMENTED (RepoTag "")),
              (1182, 8, UNIMPLEMENTED (Keywords [])),
              (1183, 8, UNIMPLEMENTED (BuildPlatforms [])),
              (1184, 4, UNIMPLEMENTED (PackageColor 0)),
              (1185, 4, UNIMPLEMENTED (PackagePrefColor 0)),
              (1186, 8, UNIMPLEMENTED (XattrsDict [])),
              (1187, 4, UNIMPLEMENTED (FileXattrsx [])),
              (1188, 8, UNIMPLEMENTED (DepAttrsDict [])),
              (1189, 4, UNIMPLEMENTED (ConflictAttrsx [])),
              (1190, 4, UNIMPLEMENTED (ObsoleteAttrsx [])),
              (1191, 4, UNIMPLEMENTED (ProvideAttrsx [])),
              (1192, 4, UNIMPLEMENTED (RequireAttrsx [])),
              (1193, 0, UNIMPLEMENTED (BuildProvides Null)),
              (1194, 0, UNIMPLEMENTED (BuildObsoletes Null)),
              (1195, 4, DBInstance 0),
              (1196, 6, NVRA ""),
              
              (5000, 8, FileNames []),
              (5001, 8, FileProvide []),
              (5002, 8, FileRequire []),
              (5003, 8, UNIMPLEMENTED (FSNames [])),
              (5004, 5, UNIMPLEMENTED (FSSizes [])),
              (5005, 8, TriggerConds []),
              (5006, 8, TriggerType []),
              (5007, 8, OrigFileNames []),
              (5008, 5, LongFileSizes []),
              (5009, 5, LongSize 0),
              (5010, 8, FileCaps []),
              (5011, 4, FileDigestAlgo 0),
              (5012, 6, BugURL ""),
              (5013, 6, EVR ""),
              (5014, 6, NVR ""),
              (5015, 6, NEVR ""),
              (5016, 6, NEVRA ""),
              (5017, 4, HeaderColor 0),
              (5018, 4, Verbose 0),
              (5019, 4, EpochNum 0),
              (5020, 4, PreInFlags 0),
              (5021, 4, PostInFlags 0),
              (5022, 4, PreUnFlags 0),
              (5023, 4, PostUnFlags 0),
              (5024, 4, PreTransFlags 0),
              (5025, 4, PostTransFlags 0),
              (5026, 4, VerifyScriptFlags 0),
              (5027, 4, TriggerScriptFlags []),
              (5029, 8, UNIMPLEMENTED (Collections [])),
              (5030, 8, PolicyNames []),
              (5031, 8, PolicyTypes []),
              (5032, 4, PolicyTypesIndexes []),
              (5033, 4, PolicyFlags []),
              (5034, 6, PolicyVCS ""),
              (5035, 8, OrderName []),
              (5036, 8, OrderVersion []),
              (5037, 4, OrderFlags []),
              (5038, 8, UNIMPLEMENTED (MSSFManifest [])),
              (5039, 8, UNIMPLEMENTED (MSSFDomain [])),
              (5040, 8, InstFileNames []),
              (5041, 8, RequireNEVRs []),
              (5042, 8, ProvideNEVRs []),
              (5043, 8, ObsoleteNEVRs []),
              (5044, 8, ConflictNEVRs []),
              (5045, 4, FileNLinks []),
              (5046, 8, RecommendName []),
              (5047, 8, RecommendVersion []),
              (5048, 4, RecommendFlags []),
              (5049, 8, SuggestName []),
              (5050, 8, SuggestVersion []),
              (5051, 4, SuggestFlags []),
              (5052, 8, SupplementName []),
              (5053, 8, SupplementVersion []),
              (5054, 4, SupplementFlags []),
              (5055, 8, EnhanceName []),
              (5056, 8, EnhanceVersion[]),
              (5057, 4, EnhanceFlags []),
              (5058, 8, RecommendNEVRs []),
              (5059, 8, SuggestNEVRs []),
              (5060, 8, SupplementNEVRs []),
              (5061, 8, EnhanceNEVRs []),
              (5062, 6, Encoding ""),
              (5063, 0, INTERNAL (FileTriggerIn Null)),
              (5064, 0, INTERNAL (FileTriggerUn Null)),
              (5065, 0, INTERNAL (FileTriggerPostUn Null)),
              (5066, 8, FileTriggerScripts []),
              (5067, 8, FileTriggerScriptProg []),
              (5068, 4, FileTriggerScriptFlags []),
              (5069, 8, FileTriggerName []),
              (5070, 4, FileTriggerIndex []),
              (5071, 8, FileTriggerVersion []),
              (5072, 4, FileTriggerFlags []),
              (5073, 0, INTERNAL (TransFileTriggerIn Null)),
              (5074, 0, INTERNAL (TransFileTriggerUn Null)),
              (5075, 0, INTERNAL (TransFileTriggerPostUn Null)),
              (5076, 8, TransFileTriggerScripts []),
              (5077, 8, TransFileTriggerScriptProg []),
              (5078, 4, TransFileTriggerScriptFlags []),
              (5079, 8, TransFileTriggerName []),
              (5080, 4, TransFileTriggerIndex []),
              (5081, 8, TransFileTriggerVersion []),
              (5082, 4, TransFileTriggerFlags []),
              (5083, 6, INTERNAL (RemovePathPostFixes "")),
              (5084, 4, FileTriggerPriorities []),
              (5085, 4, TransFileTriggerPriorities []),
              (5086, 8, FileTriggerConds []),
              (5087, 8, FileTriggerType []),
              (5088, 8, TransFileTriggerConds []),
              (5089, 8, TransFileTriggerType []),
              (5090, 8, FileSignatures []),
              (5091, 4, FileSignatureLength 0)]

