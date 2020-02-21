module ELF
  ( elfData
  )
where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           Data.Word

-- https://refspecs.linuxfoundation.org/elf/elf.pdf

fixedPoint :: Eq a => a -> (a -> a) -> a
fixedPoint x f = if x == f x then x else fixedPoint (f x) f

-- Virtual addresses near 0 are reserved, so we need to map the code
-- at a higher virtual address than its byte index in the file.
virtualAddressOffset :: Word32
virtualAddressOffset = 0x10000

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

-- see page 18
elfHeader :: Word16 -> Word16 -> B.ByteString
elfHeader elfHeaderLength programHeaderLength =
  let totalLength = elfHeaderLength + programHeaderLength
      entryPoint  = fromIntegral totalLength + virtualAddressOffset
  in  toLazyByteString
        $  lazyByteString elfIdent
        <> word16LE 2 -- file type, executable
        <> word16LE 3 -- architecture, Intel
        <> word32LE 1 -- object file version
        <> word32LE entryPoint -- entry point in virtual memory
        <> word32LE (fromIntegral elfHeaderLength) -- program header offset
        <> word32LE 0 -- section header offset, unused
        <> word32LE 0 -- processor-specific flags, none needed
        <> word16LE elfHeaderLength -- ELF header size
        <> word16LE programHeaderLength -- program header entry length
        <> word16LE 1 -- program header entry count
        <> word16LE 0 -- section header entry size, unused
        <> word16LE 0 -- section header entry count, unused
        <> word16LE 0 -- index of string table in section header, unused

-- see page 40
programHeader :: Word16 -> Word16 -> Word32 -> B.ByteString
programHeader elfHeaderLength programHeaderLength imageSize =
  let totalLength = fromIntegral $ elfHeaderLength + programHeaderLength
      va          = totalLength + virtualAddressOffset
  in  toLazyByteString
        $  word32LE 1 -- segment type, loadable code/data
        <> word32LE totalLength -- offset from beginning of file
        <> word32LE va -- virtual address at which to map code/data
        <> word32LE 0 -- physical address at which to map, unused
        <> word32LE imageSize -- number of bytes listed in file image
        <> word32LE imageSize -- number of bytes to reserve in memory
        <> word32LE 0x7 -- permissions, allow all (see page 73)
        <> word32LE 0 -- alignment, none required

-- see page 15
elfData :: B.ByteString -> B.ByteString
elfData code =
  let (ehdr', phdr') = fixedPoint (B.empty, B.empty) $ \(ehdr, phdr) ->
        let elen      = fromIntegral $ B.length ehdr
            plen      = fromIntegral $ B.length phdr
            imageSize = fromIntegral $ B.length code
        in  (elfHeader elen plen, programHeader elen plen imageSize)
  in  toLazyByteString
        $  lazyByteString ehdr'
        <> lazyByteString phdr'
        <> lazyByteString code
