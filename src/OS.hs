module OS where

pageSize :: Int
pageSize = 0x1000

-- Virtual addresses near 0 are reserved, so we need to map the code
-- at a higher virtual address than its byte index in the file.
vaOffset :: Int
vaOffset = 0x10000
