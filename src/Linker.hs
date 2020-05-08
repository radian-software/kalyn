module Linker
  ( link
  )
where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           Data.List
import qualified Data.Map.Strict               as Map
import           Data.Maybe

import           OS
import           Util

-- https://refspecs.linuxfoundation.org/elf/elf.pdf
-- https://uclibc.org/docs/elf-64-gen.pdf
--
-- Most of the necessary information is in the original ELF spec,
-- which is really quite good. However we are using ELF-64 and so it
-- is important to note that some of the fields have become 64-bit
-- instead of 32-bit in the headers.

data HeaderInfo = HeaderInfo
  { elfHeaderLen :: Int
  , phEntryLen :: Int
  , phNumEntries :: Int
  , shEntryLen :: Int
  , shNumEntries :: Int
  , shstrtabIndices :: Map.Map String Int
  , shstrtabLen :: Int
  , symtabEntryLen :: Int
  , symtabNumEntries :: Int
  , symstrtabLen :: Int
  , codeLen :: Int
  , dataLen :: Int
  } deriving (Show)

phOffset :: HeaderInfo -> Int
phOffset = elfHeaderLen

shOffset :: HeaderInfo -> Int
shOffset info = phOffset info + (phEntryLen info * phNumEntries info)

shstrtabOffset :: HeaderInfo -> Int
shstrtabOffset info = shOffset info + (shEntryLen info * shNumEntries info)

symtabOffset :: HeaderInfo -> Int
symtabOffset info = shstrtabOffset info + shstrtabLen info

symtabLen :: HeaderInfo -> Int
symtabLen info = symtabEntryLen info * symtabNumEntries info

symstrtabOffset :: HeaderInfo -> Int
symstrtabOffset info = symtabOffset info + symtabLen info

headerEndOffset :: HeaderInfo -> Int
headerEndOffset info = symstrtabOffset info + symstrtabLen info

headerPadding :: HeaderInfo -> Int
headerPadding info = leftover pageSize (headerEndOffset info)

codeOffset :: HeaderInfo -> Int
codeOffset info = headerEndOffset info + headerPadding info

dataOffset :: HeaderInfo -> Int
dataOffset info = codeOffset info + roundUp pageSize (codeLen info)

-- see page 20
elfIdent :: B.ByteString
elfIdent =
  let hdr =
          toLazyByteString
            $  word8 0x7f -- magic bytes
            <> stringUtf8 "ELF"
            <> word8 2 -- address size, 64-bit
            <> word8 1 -- endianness, little-endian
            <> word8 1 -- version of ELF specification
  in  hdr <> B.pack (replicate (16 - fromIntegral (B.length hdr)) 0)

-- see page 18; for architecture codes see
-- <https://opensource.apple.com/source/dtrace/dtrace-90/sys/elf.h>
elfHeader :: HeaderInfo -> B.ByteString
elfHeader info =
  toLazyByteString
    $  lazyByteString elfIdent
    <> word16LE 2 -- file type, non-relocatable executable
    <> word16LE 62 -- architecture, x86_64
    <> word32LE 1 -- object file version
    <> word64LE (fromIntegral $ codeOffset info + vaOffset) -- entry point in virtual memory
    <> word64LE (fromIntegral $ phOffset info) -- program header offset
    <> word64LE (fromIntegral $ shOffset info) -- section header offset
    <> word32LE 0 -- processor-specific flags, none needed
    <> word16LE (fromIntegral $ elfHeaderLen info) -- ELF header size
    <> word16LE (fromIntegral $ phEntryLen info) -- program header entry length
    <> word16LE (fromIntegral $ phNumEntries info) -- program header entry count
    <> word16LE (fromIntegral $ shEntryLen info) -- section header entry size
    <> word16LE (fromIntegral $ shNumEntries info) -- section header entry count
    <> word16LE 1 -- index of string table in section header

-- see page 40
programHeader :: HeaderInfo -> [B.ByteString]
programHeader info =
  [ toLazyByteString -- code section
    $  word32LE 1 -- segment type, loadable code/data
    <> word32LE 0x5 -- permissions, read/execute only (see page 73)
    <> word64LE (fromIntegral $ codeOffset info) -- offset from beginning of file
    <> word64LE (fromIntegral $ codeOffset info + vaOffset) -- virtual address at which to map code/data
    <> word64LE 0 -- physical address at which to map, unused
    <> word64LE (fromIntegral $ codeLen info) -- number of bytes listed in file image
    <> word64LE (fromIntegral $ codeLen info) -- number of bytes to reserve in memory
    <> word64LE 0 -- alignment, none required
  , toLazyByteString -- data section
    $  word32LE 1 -- segment type, loadable code/data
    <> word32LE 0x6 -- permissions, read/write only (see page 73)
    <> word64LE (fromIntegral $ dataOffset info) -- offset from beginning of file
    <> word64LE (fromIntegral $ dataOffset info + vaOffset) -- virtual address at which to map code/data
    <> word64LE 0 -- physical address at which to map, unused
    <> word64LE (fromIntegral $ dataLen info) -- number of bytes listed in file image
    <> word64LE (fromIntegral $ dataLen info) -- number of bytes to reserve in memory
    <> word64LE 0 -- alignment, none required
  ]

-- see page 24 and page 28 and page 66
sectionHeader :: HeaderInfo -> Int -> [(B.ByteString, Maybe String)]
sectionHeader info numSymbols =
  let getIdx name =
          fromIntegral $ Map.findWithDefault 0 name (shstrtabIndices info)
  in  [ ( toLazyByteString -- index 0 (see page 27)
          $  word32LE 0
          <> word32LE 0
          <> word64LE 0
          <> word64LE 0
          <> word64LE 0
          <> word64LE 0
          <> word32LE 0
          <> word32LE 0
          <> word64LE 0
          <> word64LE 0
        , Nothing
        )
      , ( toLazyByteString
          $  word32LE (getIdx ".shstrtab") -- section name in shstrtab
          <> word32LE 3 -- section type, string table
          <> word64LE 0 -- section attribute flags, none needed
          <> word64LE 0 -- memory address, none needed
          <> word64LE (fromIntegral $ shstrtabOffset info) -- file address
          <> word64LE (fromIntegral $ shstrtabLen info) -- segment length
          <> word32LE 0 -- section header table index link, unused
          <> word32LE 0 -- additional information, unused
          <> word64LE 0 -- alignment, none required
          <> word64LE 0 -- table entry size, unused
        , Just ".shstrtab"
        )
      , ( toLazyByteString
          $  word32LE (getIdx ".symtab") -- section name in shstrtab
          <> word32LE 2 -- section type, symbol table
          <> word64LE 0 -- section attribute flags, none needed
          <> word64LE 0 -- memory address, none needed
          <> word64LE (fromIntegral $ symtabOffset info) -- file address
          <> word64LE (fromIntegral $ symtabLen info) -- segment length
          <> word32LE 3 -- index of associated string table in section header
          <> word32LE (fromIntegral $ numSymbols + 1) -- index after last local symbol in symbol table
          <> word64LE 0 -- alignment, none required
          <> word64LE (fromIntegral $ symtabEntryLen info) -- table entry size
        , Just ".symtab"
        )
      , ( toLazyByteString
          $  word32LE (getIdx ".strtab") -- section name in shstrtab
          <> word32LE 3 -- section type, string table
          <> word64LE 0 -- section attribute flags, none needed
          <> word64LE 0 -- memory address, none needed
          <> word64LE (fromIntegral $ symstrtabOffset info) -- file address
          <> word64LE (fromIntegral $ symstrtabLen info) -- segment length
          <> word32LE 0 -- section header table index link, unused
          <> word32LE 0 -- additional information, unused
          <> word64LE 0 -- alignment, none required
          <> word64LE 0 -- table entry size, unused
        , Just ".strtab"
        )
      , ( toLazyByteString
          $  word32LE (getIdx ".text") -- section name in shstrtab
          <> word32LE 1 -- section type, program information
          <> word64LE 0x6 -- section attribute flags, executable memory
          <> word64LE (fromIntegral $ codeOffset info + vaOffset) -- memory address
          <> word64LE (fromIntegral $ codeOffset info) -- file address
          <> word64LE (fromIntegral $ codeLen info) -- segment length
          <> word32LE 0 -- section header table index link, unused
          <> word32LE 0 -- additional information, unused
          <> word64LE 0 -- alignment, none required
          <> word64LE 0 -- table entry size, unused
        , Just ".text"
        )
      , ( toLazyByteString
          $  word32LE (getIdx ".data") -- section name in shstrtab
          <> word32LE 1 -- section type, program information
          <> word64LE 0x3 -- section attribute flags, writeable memory
          <> word64LE (fromIntegral $ dataOffset info + vaOffset) -- memory address
          <> word64LE (fromIntegral $ dataOffset info) -- file address
          <> word64LE (fromIntegral $ dataLen info) -- segment length
          <> word32LE 0 -- section header table index link, unused
          <> word32LE 0 -- additional information, unused
          <> word64LE 0 -- alignment, none required
          <> word64LE 0 -- table entry size, unused
        , Just ".data"
        )
      ]

-- see page 32
symtab
  :: HeaderInfo
  -> Map.Map String Int
  -> Map.Map String Int
  -> Map.Map String Int
  -> [B.ByteString]
symtab info codeSymbols dataSymbols indices =
  let getIdx = fromIntegral . (indices Map.!)
  in
    toLazyByteString
      (word32LE 0 <> word8 0 <> word8 0 <> word16LE 0 <> word64LE 0 <> word64LE
        0
      )
    :  map
         (\(name, offset) ->
           let va = fromIntegral $ offset + codeOffset info + vaOffset
           in  toLazyByteString -- main
                 $  word32LE (getIdx name) -- symbol name in symstrtab
                 <> word8 2 -- symbol type and binding, locally bound function
                 <> word8 0 -- unused field
                 <> word16LE 4 -- index of code section from section header
                 <> word64LE va -- symbol value, virtual address
                 <> word64LE 0 -- symbol size, not specified
         )
         (Map.toList codeSymbols)
    ++ map
         (\(name, offset) ->
           let va = fromIntegral $ offset + codeOffset info + vaOffset
           in  toLazyByteString -- main
                 $  word32LE (getIdx name) -- symbol name in symstrtab
                 <> word8 2 -- symbol type and binding, locally bound function
                 <> word8 0 -- unused field
                 <> word16LE 5 -- index of code section from section header
                 <> word64LE va -- symbol value, virtual address
                 <> word64LE 0 -- symbol size, not specified
         )
         (Map.toList dataSymbols)

-- see page 31
strtab :: [String] -> (B.ByteString, Map.Map String Int)
strtab strs =
  let uniqStrs = nub strs
      indices  = scanl (+) 1 $ map ((+ 1) . length) uniqStrs
      table    = toLazyByteString $ word8 0 <> mconcat
        (map (\s -> stringUtf8 s <> word8 0) uniqStrs)
      tableM = foldr (uncurry Map.insert) Map.empty $ zip uniqStrs indices
  in  (table, tableM)

-- see page 15
link
  :: (B.ByteString, B.ByteString, Map.Map String Int, Map.Map String Int)
  -> B.ByteString
link (codeB, dataB, codeSymbols, dataSymbols) =
  let
    (ehdr', phdr', shdr', shstrtabB', symtabB', symstrtabB', _, hpad') =
      fixedPoint (B.empty, [], [], B.empty, [], B.empty, Map.empty, B.empty)
        $ \(ehdr, phdr, shdr, shstrtabB, symtabB, symstrtabB, shstrtabIndicesM, _) ->
            let phelen     = maybe 0 B.length $ listToMaybe phdr
                shelen     = maybe 0 B.length $ listToMaybe shdr
                stelen     = maybe 0 B.length $ listToMaybe symtabB
                allSymbols = Map.keys codeSymbols ++ Map.keys dataSymbols
                info       = HeaderInfo
                  { elfHeaderLen     = fromIntegral $ B.length ehdr
                  , phEntryLen       = fromIntegral phelen
                  , phNumEntries     = length phdr
                  , shEntryLen       = fromIntegral shelen
                  , shNumEntries     = length shdr
                  , shstrtabIndices  = shstrtabIndicesM
                  , shstrtabLen      = fromIntegral $ B.length shstrtabB
                  , symtabEntryLen   = fromIntegral stelen
                  , symtabNumEntries = length symtabB
                  , symstrtabLen     = fromIntegral $ B.length symstrtabB
                  , codeLen          = fromIntegral $ B.length codeB
                  , dataLen          = fromIntegral $ B.length dataB
                  }
                shdrData      = sectionHeader info (length allSymbols)
                shStringList  = collectMaybes (map snd shdrData)
                shstrtabData  = strtab shStringList
                symstrtabData = strtab allSymbols
            in  if all (\phe -> B.length phe == phelen) phdr
                     && all (\she -> B.length she == shelen) shdr
                     && all (\ste -> B.length ste == stelen) symtabB
                  then
                    ( elfHeader info
                    , programHeader info
                    , map fst shdrData
                    , fst shstrtabData
                    , symtab info codeSymbols dataSymbols (snd symstrtabData)
                    , fst symstrtabData
                    , snd shstrtabData
                    , B.pack $ replicate (headerPadding info) 0
                    )
                  else error "ELF header entries are not same length"

  -- assume the assembler already put the appropriate padding between
  -- code and data so they don't share a page
  in
    toLazyByteString
    $  lazyByteString ehdr'
    <> lazyByteString (mconcat phdr')
    <> lazyByteString (mconcat shdr')
    <> lazyByteString shstrtabB'
    <> lazyByteString (mconcat symtabB')
    <> lazyByteString symstrtabB'
    <> lazyByteString hpad'
    <> lazyByteString codeB
    <> mconcat
         (replicate (leftover pageSize (fromIntegral $ B.length codeB))
                    (word8 0)
         )
    <> lazyByteString dataB
