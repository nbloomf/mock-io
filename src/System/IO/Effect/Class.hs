{-# LANGUAGE NoImplicitPrelude #-}
module System.IO.Effect.Class (
    IO.FilePath
  , IO.stdin
  , IO.stdout
  , IO.stderr
  , IO.BufferMode(..)
  , IO.HandlePosn
  , IO.SeekMode(..)
  , IO.TextEncoding
  , IO.utf8
  , IO.latin1
  , IO.utf8_bom
  , IO.utf16
  , IO.utf16le
  , IO.utf16be
  , IO.utf32
  , IO.utf32le
  , IO.utf32be
  , IO.localeEncoding
  , IO.Newline(..)
  , IO.nativeNewline
  , IO.NewlineMode(..)
  , IO.noNewlineTranslation
  , IO.universalNewlineMode
  , IO.nativeNewlineMode

  , FixIO(..)

  , WithFile(..)
  , OpenFile(..)
  , HClose(..)
  , ReadFile(..)
  , WriteFile(..)
  , AppendFile(..)

  , HFileSize(..)
  , HSetFileSize(..)
  , HIsEOF(..)
  , IsEOF(..)
  , HSetBuffering(..)
  , HGetBuffering(..)
  , HFlush(..)
  , HGetPosn(..)
  , HSetPosn(..)
  , HSeek(..)
  , HTell(..)
  , HIsOpen(..)
  , HIsClosed(..)
  , HIsReadable(..)
  , HIsWritable(..)
  , HIsSeekable(..)

  , HIsTerminalDevice(..)
  , HSetEcho(..)
  , HGetEcho(..)

  , HShow(..)

  , HWaitForInput(..)
  , HReady(..)
  , HGetChar(..)
  , HGetLine(..)
  , HLookAhead(..)
  , HGetContents(..)
  , HPutChar(..)
  , HPutStr(..)
  , HPutStrLn(..)
  , HPrint(..)
  , Interact(..)
  , PutChar(..)
  , PutStr(..)
  , PutStrLn(..)
  , Print(..)
  , GetChar(..)
  , GetLine(..)
  , GetContents(..)
  , ReadIO(..)
  , ReadLn(..)

  , WithBinaryFile(..)
  , OpenBinaryFile(..)
  , HSetBinaryMode(..)
  , HPutBuf(..)
  , HGetBuf(..)
  , HPutBufNonBlocking(..)
  , HGetBufNonBlocking(..)

  , OpenTempFile(..)
  , OpenBinaryTempFile(..)
  , OpenTempFileWithDefaultPermissions(..)
  , OpenBinaryTempFileWithDefaultPermissions(..)

  , HGetEncoding(..)
  , HSetEncoding(..)
  , MkTextEncoding(..)

  , HSetNewlineMode(..)
) where

import Prelude (Monad, String, Bool, Integer, Char, Int, Maybe, Read, Show)
import Foreign.Ptr (Ptr)
import qualified System.IO as IO



class FixIO t where
  fixIO :: (a -> t a) -> t a

instance FixIO IO.IO where
  fixIO = IO.fixIO



class WithFile t where
  withFile :: IO.FilePath -> IO.IOMode -> (IO.Handle -> t r) -> t r

instance WithFile IO.IO where
  withFile = IO.withFile



class OpenFile t where
  openFile :: IO.FilePath -> IO.IOMode -> t IO.Handle

instance OpenFile IO.IO where
  openFile = IO.openFile



class HClose t where
  hClose :: IO.Handle -> t ()

instance HClose IO.IO where
  hClose = IO.hClose



class ReadFile t where
  readFile :: IO.FilePath -> t String

instance ReadFile IO.IO where
  readFile = IO.readFile



class WriteFile t where
  writeFile :: IO.FilePath -> String -> t ()

instance WriteFile IO.IO where
  writeFile = IO.writeFile



class AppendFile t where
  appendFile :: IO.FilePath -> String -> t ()

instance AppendFile IO.IO where
  appendFile = IO.appendFile



class HFileSize t where
  hFileSize :: IO.Handle -> t Integer

instance HFileSize IO.IO where
  hFileSize = IO.hFileSize



class HSetFileSize t where
  hSetFileSize :: IO.Handle -> Integer -> t ()

instance HSetFileSize IO.IO where
  hSetFileSize = IO.hSetFileSize



class HIsEOF t where
  hIsEOF :: IO.Handle -> t Bool

instance HIsEOF IO.IO where
  hIsEOF = IO.hIsEOF



class IsEOF t where
  isEOF :: t Bool

instance IsEOF IO.IO where
  isEOF = IO.isEOF



class HSetBuffering t where
  hSetBuffering :: IO.Handle -> IO.BufferMode -> t ()

instance HSetBuffering IO.IO where
  hSetBuffering = IO.hSetBuffering



class HGetBuffering t where
  hGetBuffering :: IO.Handle -> t IO.BufferMode

instance HGetBuffering IO.IO where
  hGetBuffering = IO.hGetBuffering



class HFlush t where
  hFlush :: IO.Handle -> t ()

instance HFlush IO.IO where
  hFlush = IO.hFlush



class HGetPosn t where
  hGetPosn :: IO.Handle -> t IO.HandlePosn

instance HGetPosn IO.IO where
  hGetPosn = IO.hGetPosn



class HSetPosn t where
  hSetPosn :: IO.HandlePosn -> t ()

instance HSetPosn IO.IO where
  hSetPosn = IO.hSetPosn



class HSeek t where
  hSeek :: IO.Handle -> IO.SeekMode -> Integer -> t ()

instance HSeek IO.IO where
  hSeek = IO.hSeek



class HTell t where
  hTell :: IO.Handle -> t Integer

instance HTell IO.IO where
  hTell = IO.hTell



class HIsOpen t where
  hIsOpen :: IO.Handle -> t Bool

instance HIsOpen IO.IO where
  hIsOpen = IO.hIsOpen



class HIsClosed t where
  hIsClosed :: IO.Handle -> t Bool

instance HIsClosed IO.IO where
  hIsClosed = IO.hIsClosed



class HIsReadable t where
  hIsReadable :: IO.Handle -> t Bool

instance HIsReadable IO.IO where
  hIsReadable = IO.hIsReadable



class HIsWritable t where
  hIsWritable :: IO.Handle -> t Bool

instance HIsWritable IO.IO where
  hIsWritable = IO.hIsWritable



class HIsSeekable t where
  hIsSeekable :: IO.Handle -> t Bool

instance HIsSeekable IO.IO where
  hIsSeekable = IO.hIsSeekable



class HIsTerminalDevice t where
  hIsTerminalDevice :: IO.Handle -> t Bool

instance HIsTerminalDevice IO.IO where
  hIsTerminalDevice = IO.hIsTerminalDevice



class HSetEcho t where
  hSetEcho :: IO.Handle -> Bool -> t ()

instance HSetEcho IO.IO where
  hSetEcho = IO.hSetEcho



class HGetEcho t where
  hGetEcho :: IO.Handle -> t Bool

instance HGetEcho IO.IO where
  hGetEcho = IO.hGetEcho



class HShow t where
  hShow :: IO.Handle -> t String

instance HShow IO.IO where
  hShow = IO.hShow



class HWaitForInput t where
  hWaitForInput :: IO.Handle -> Int -> t Bool

instance HWaitForInput IO.IO where
  hWaitForInput = IO.hWaitForInput



class HReady t where
  hReady :: IO.Handle -> t Bool

instance HReady IO.IO where
  hReady = IO.hReady



class HGetChar t where
  hGetChar :: IO.Handle -> t Char

instance HGetChar IO.IO where
  hGetChar = IO.hGetChar



class HGetLine t where
  hGetLine :: IO.Handle -> t String

instance HGetLine IO.IO where
  hGetLine = IO.hGetLine



class HLookAhead t where
  hLookAhead :: IO.Handle -> t Char

instance HLookAhead IO.IO where
  hLookAhead = IO.hLookAhead



class HGetContents t where
  hGetContents :: IO.Handle -> t String

instance HGetContents IO.IO where
  hGetContents = IO.hGetContents



class HPutChar t where
  hPutChar :: IO.Handle -> Char -> t ()

instance HPutChar IO.IO where
  hPutChar = IO.hPutChar



class HPutStr t where
  hPutStr :: IO.Handle -> String -> t ()

instance HPutStr IO.IO where
  hPutStr = IO.hPutStr



class HPutStrLn t where
  hPutStrLn :: IO.Handle -> String -> t ()

instance HPutStrLn IO.IO where
  hPutStrLn = IO.hPutStrLn



class HPrint t where
  hPrint :: Show a => IO.Handle -> a -> t ()

instance HPrint IO.IO where
  hPrint = IO.hPrint



class Interact t where
  interact :: (String -> String) -> t ()

instance Interact IO.IO where
  interact = IO.interact



class PutChar t where
  putChar :: Char -> t ()

instance PutChar IO.IO where
  putChar = IO.putChar



class PutStr t where
  putStr :: String -> t ()

instance PutStr IO.IO where
  putStr = IO.putStr



class PutStrLn t where
  putStrLn :: String -> t ()

instance PutStrLn IO.IO where
  putStrLn = IO.putStrLn



class Print t where
  print :: Show a => a -> t ()

instance Print IO.IO where
  print = IO.print



class GetChar t where
  getChar :: t Char

instance GetChar IO.IO where
  getChar = IO.getChar



class GetLine t where
  getLine :: t String

instance GetLine IO.IO where
  getLine = IO.getLine



class GetContents t where
  getContents :: t String

instance GetContents IO.IO where
  getContents = IO.getContents



class ReadIO t where
  readIO :: Read a => String -> t a

instance ReadIO IO.IO where
  readIO = IO.readIO



class ReadLn t where
  readLn :: Read a => t a

instance ReadLn IO.IO where
  readLn = IO.readLn



class WithBinaryFile t where
  withBinaryFile :: IO.FilePath -> IO.IOMode -> (IO.Handle -> t r)-> t r

instance WithBinaryFile IO.IO where
  withBinaryFile = IO.withBinaryFile



class OpenBinaryFile t where
  openBinaryFile :: IO.FilePath -> IO.IOMode -> t IO.Handle

instance OpenBinaryFile IO.IO where
  openBinaryFile = IO.openBinaryFile



class HSetBinaryMode t where
  hSetBinaryMode :: IO.Handle -> Bool -> t ()

instance HSetBinaryMode IO.IO where
  hSetBinaryMode = IO.hSetBinaryMode



class HPutBuf t where
  hPutBuf :: IO.Handle -> Ptr a -> Int -> t ()

instance HPutBuf IO.IO where
  hPutBuf = IO.hPutBuf



class HGetBuf t where
  hGetBuf :: IO.Handle -> Ptr a -> Int -> t Int

instance HGetBuf IO.IO where
  hGetBuf = IO.hGetBuf



class HPutBufNonBlocking t where
  hPutBufNonBlocking :: IO.Handle -> Ptr a -> Int -> t Int

instance HPutBufNonBlocking IO.IO where
  hPutBufNonBlocking = IO.hPutBufNonBlocking



class HGetBufNonBlocking t where
  hGetBufNonBlocking :: IO.Handle -> Ptr a -> Int -> t Int

instance HGetBufNonBlocking IO.IO where
  hGetBufNonBlocking = IO.hGetBufNonBlocking



class OpenTempFile t where
  openTempFile
    :: IO.FilePath -> String -> t (IO.FilePath, IO.Handle)

instance OpenTempFile IO.IO where
  openTempFile = IO.openTempFile



class OpenBinaryTempFile t where
  openBinaryTempFile
    :: IO.FilePath -> String -> t (IO.FilePath, IO.Handle)

instance OpenBinaryTempFile IO.IO where
  openBinaryTempFile = IO.openBinaryTempFile



class OpenTempFileWithDefaultPermissions t where
  openTempFileWithDefaultPermissions
    :: IO.FilePath -> String -> t (IO.FilePath, IO.Handle)

instance OpenTempFileWithDefaultPermissions IO.IO where
  openTempFileWithDefaultPermissions =
    IO.openTempFileWithDefaultPermissions



class OpenBinaryTempFileWithDefaultPermissions t where
  openBinaryTempFileWithDefaultPermissions
    :: IO.FilePath -> String -> t (IO.FilePath, IO.Handle)

instance OpenBinaryTempFileWithDefaultPermissions IO.IO where
  openBinaryTempFileWithDefaultPermissions =
    IO.openBinaryTempFileWithDefaultPermissions



class HSetEncoding t where
  hSetEncoding :: IO.Handle -> IO.TextEncoding -> t ()

instance HSetEncoding IO.IO where
  hSetEncoding = IO.hSetEncoding



class HGetEncoding t where
  hGetEncoding :: IO.Handle -> t (Maybe IO.TextEncoding)

instance HGetEncoding IO.IO where
  hGetEncoding = IO.hGetEncoding



class MkTextEncoding t where
  mkTextEncoding :: String -> t IO.TextEncoding

instance MkTextEncoding IO.IO where
  mkTextEncoding = IO.mkTextEncoding



class HSetNewlineMode t where
  hSetNewlineMode :: IO.Handle -> IO.NewlineMode -> t ()

instance HSetNewlineMode IO.IO where
  hSetNewlineMode = IO.hSetNewlineMode
