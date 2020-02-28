module Linker
  ( link
  )
where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           Data.Word

import           Util

-- https://refspecs.linuxfoundation.org/elf/elf.pdf

-- see page 20
elfIdent :: B.ByteString
elfIdent =
  toLazyByteString
    $  word8 0x7f -- magic bytes
    <> stringUtf8 "ELF"
    <> word8 2 -- address size, 64-bit
    <> word8 1 -- endianness, little-endian
    <> word8 1 -- version of ELF specification
    <> mconcat (replicate 9 $ word8 0)

-- see page 18; for architecture codes see
-- <https://opensource.apple.com/source/dtrace/dtrace-90/sys/elf.h>
elfHeader :: Word16 -> Word16 -> B.ByteString
elfHeader elfHeaderLength programHeaderLength =
  let totalLength = elfHeaderLength + programHeaderLength
  in  toLazyByteString
        $  lazyByteString elfIdent
        <> word16LE 3 -- file type, relocatable executable (called "shared object file")
        <> word16LE 62 -- architecture, x86_64
        <> word32LE 1 -- object file version
        <> word64LE (fromIntegral totalLength) -- entry point in virtual memory
        <> word64LE (fromIntegral elfHeaderLength) -- program header offset
        <> word64LE 0 -- section header offset, unused
        <> word32LE 0 -- processor-specific flags, none needed
        <> word16LE elfHeaderLength -- ELF header size
        <> word16LE programHeaderLength -- program header entry length
        <> word16LE 1 -- program header entry count
        <> word16LE 0 -- section header entry size, unused
        <> word16LE 0 -- section header entry count, unused
        <> word16LE 0 -- index of string table in section header, unused

-- see page 40
programHeader :: Word16 -> Word16 -> Word64 -> B.ByteString
programHeader elfHeaderLength programHeaderLength imageSize =
  let totalLength = fromIntegral $ elfHeaderLength + programHeaderLength
  in  toLazyByteString
        $  word32LE 1 -- segment type, loadable code/data
        <> word32LE 0x7 -- permissions, allow all (see page 73)
        <> word64LE totalLength -- offset from beginning of file
        <> word64LE totalLength -- virtual address at which to map code/data
        <> word64LE 0 -- physical address at which to map, unused
        <> word64LE imageSize -- number of bytes listed in file image
        <> word64LE imageSize -- number of bytes to reserve in memory
        <> word64LE 0 -- alignment, none required

-- see page 15
link :: B.ByteString -> B.ByteString
link code =
  let (ehdr', phdr') = fixedPoint (B.empty, B.empty) $ \(ehdr, phdr) ->
        let elen      = fromIntegral $ B.length ehdr
            plen      = fromIntegral $ B.length phdr
            imageSize = fromIntegral $ B.length code
        in  (elfHeader elen plen, programHeader elen plen imageSize)
  in  toLazyByteString
        $  lazyByteString ehdr'
        <> lazyByteString phdr'
        <> lazyByteString code
