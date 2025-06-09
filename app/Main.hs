module Main where

import System.Environment (getArgs)
import System.IO (stdin, stdout, stderr, hPutStrLn)
import System.Exit (exitFailure)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Data.Char (isPrint, chr)
import Text.Printf (printf)

-- Convert a Word8 to its printable ASCII char or '.'
toPrintableChar :: Word8 -> Char
toPrintableChar byte =
    let c = chr (fromIntegral byte)
    in if isPrint c then c else '.'

-- Formats a single line of output for xxd
formatLine :: Int -> [Word8] -> String
formatLine offset bytes =
    let 
        offsetStr = (printf "%08x:" offset) :: String
        hexGroups = map (printf "%02x") bytes ++ replicate (16 - length bytes) "  "
        finalHexStr = unwords (take 8 (groupPairs hexGroups))
        paddedAscii = take 16 (map toPrintableChar bytes ++ repeat ' ')
    in (printf "%s %s  %s" offsetStr finalHexStr paddedAscii) :: String

groupPairs :: [String] -> [String]
groupPairs [] = []
groupPairs [x] = [x ++ "  "]  -- Ensure alignment for odd-length cases
groupPairs (x:y:xs) = (x ++ y) : groupPairs xs


-- Processes the ByteString chunk by chunk
processByteString :: Int -> BL.ByteString -> IO ()
processByteString offset bs
    | BL.null bs = return ()
    | otherwise  = do
        let (chunk, rest) = BL.splitAt 16 bs
        let byteList = BL.unpack chunk

        putStrLn $ formatLine offset byteList
        processByteString (offset + length byteList) rest

-- Main entry point
main :: IO ()
main = do
    args <- getArgs
    contents <- case args of
        []         -> BL.getContents
        [filename] -> BL.readFile filename
        _          -> do
            hPutStrLn stderr "Usage: hxxd [filename]"
            exitFailure
    processByteString 0 contents