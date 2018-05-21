{-# LANGUAGE NoImplicitPrelude #-}
module System.IO.Effect (
    IO.FilePath
  , IO.Handle
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

  , IOEffectFix(..)
  , IOEffectFile(..)
  , IOEffectHandle(..)
  , IOEffectTerminal(..)
  , IOEffectShowHandle(..)
  , IOEffectBinary(..)
  , IOEffectTemp(..)
  , IOEffectEncoding(..)
  , IOEffectNewline(..)
) where

import Prelude (String, Bool, Integer, Char, Int, Maybe, Read, Show)
import Foreign.Ptr (Ptr)
import qualified System.IO as IO



class
  ( IOEffectFix t
  , IOEffectFile t
  , IOEffectHandle t
  , IOEffectTerminal t
  , IOEffectShowHandle t
  , IOEffectBinary t
  , IOEffectTemp t
  , IOEffectEncoding t
  , IOEffectNewline t
  ) => IOEffect t



class IOEffectFix t where
  fixIO :: (a -> t a) -> t a

instance IOEffectFix IO.IO where
  fixIO = IO.fixIO



class IOEffectFile t where
  withFile :: IO.FilePath -> IO.IOMode -> (IO.Handle -> t r) -> t r
  openFile :: IO.FilePath -> IO.IOMode -> t IO.Handle
  hClose :: IO.Handle -> t ()
  readFile :: IO.FilePath -> t String
  writeFile :: IO.FilePath -> String -> t ()
  appendFile :: IO.FilePath -> String -> t ()

instance IOEffectFile IO.IO where
  withFile = IO.withFile
  openFile = IO.openFile
  hClose = IO.hClose
  readFile = IO.readFile
  writeFile = IO.writeFile
  appendFile = IO.appendFile



class IOEffectHandle t where
  hFileSize :: IO.Handle -> t Integer
  hSetFileSize :: IO.Handle -> Integer -> t ()
  hIsEOF :: IO.Handle -> t Bool
  isEOF :: t Bool
  hSetBuffering :: IO.Handle -> IO.BufferMode -> t ()
  hGetBuffering :: IO.Handle -> t IO.BufferMode
  hFlush :: IO.Handle -> t ()
  hGetPosn :: IO.Handle -> t IO.HandlePosn
  hSetPosn :: IO.HandlePosn -> t ()
  hSeek :: IO.Handle -> IO.SeekMode -> Integer -> t ()
  hTell :: IO.Handle -> t Integer
  hIsOpen :: IO.Handle -> t Bool
  hIsClosed :: IO.Handle -> t Bool
  hIsReadable :: IO.Handle -> t Bool
  hIsWritable :: IO.Handle -> t Bool
  hIsSeekable :: IO.Handle -> t Bool

instance IOEffectHandle IO.IO where
  hFileSize = IO.hFileSize
  hSetFileSize = IO.hSetFileSize
  hIsEOF = IO.hIsEOF
  isEOF = IO.isEOF
  hSetBuffering = IO.hSetBuffering
  hGetBuffering = IO.hGetBuffering
  hFlush = IO.hFlush
  hGetPosn = IO.hGetPosn
  hSetPosn = IO.hSetPosn
  hSeek = IO.hSeek
  hTell = IO.hTell
  hIsOpen = IO.hIsOpen
  hIsClosed = IO.hIsClosed
  hIsReadable = IO.hIsReadable
  hIsWritable = IO.hIsWritable
  hIsSeekable = IO.hIsSeekable



class IOEffectTerminal t where
  hIsTerminalDevice :: IO.Handle -> t Bool
  hSetEcho :: IO.Handle -> Bool -> t ()
  hGetEcho :: IO.Handle -> t Bool

instance IOEffectTerminal IO.IO where
  hIsTerminalDevice = IO.hIsTerminalDevice
  hSetEcho = IO.hSetEcho
  hGetEcho = IO.hGetEcho



class IOEffectShowHandle t where
  hShow :: IO.Handle -> t String

instance IOEffectShowHandle IO.IO where
  hShow = IO.hShow



class IOEffectText t where
  hWaitForInput :: IO.Handle -> Int -> t Bool
  hReady :: IO.Handle -> t Bool
  hGetChar ::IO.Handle -> t Char
  hGetLine ::IO.Handle -> t String
  hLookAhead ::IO.Handle -> t Char
  hGetContents ::IO.Handle -> t String
  hPutChar ::IO.Handle -> Char -> t ()
  hPutStr ::IO.Handle -> String -> t ()
  hPutStrLn ::IO.Handle -> String -> t ()
  hPrint :: Show a =>IO.Handle -> a -> t ()
  interact :: (String -> String) -> t ()
  putChar :: Char -> t ()
  putStr :: String -> t ()
  putStrLn :: String -> t ()
  print :: Show a => a -> t ()
  getChar :: t Char
  getLine :: t String
  getContents :: t String
  readIO :: Read a => String -> t a
  readLn :: Read a => t a

instance IOEffectText IO.IO where
  hWaitForInput = IO.hWaitForInput
  hReady = IO.hReady
  hGetChar = IO.hGetChar
  hGetLine = IO.hGetLine
  hLookAhead = IO.hLookAhead
  hGetContents = IO.hGetContents
  hPutChar = IO.hPutChar
  hPutStr = IO.hPutStr
  hPutStrLn = IO.hPutStrLn
  hPrint = IO.hPrint
  interact = IO.interact
  putChar = IO.putChar
  putStr = IO.putStr
  putStrLn = IO.putStrLn
  print = IO.print
  getChar = IO.getChar
  getLine = IO.getLine
  getContents = IO.getContents
  readIO = IO.readIO
  readLn = IO.readLn



class IOEffectBinary t where
  withBinaryFile :: IO.FilePath -> IO.IOMode -> (IO.Handle -> t r)-> t r
  openBinaryFile :: IO.FilePath -> IO.IOMode -> t IO.Handle
  hSetBinaryMode :: IO.Handle -> Bool -> t ()
  hPutBuf :: IO.Handle -> Ptr a -> Int -> t ()
  hGetBuf :: IO.Handle -> Ptr a -> Int -> t Int
  hPutBufNonBlocking :: IO.Handle -> Ptr a -> Int -> t Int
  hGetBufNonBlocking :: IO.Handle -> Ptr a -> Int -> t Int

instance IOEffectBinary IO.IO where
  withBinaryFile = IO.withBinaryFile
  openBinaryFile = IO.openBinaryFile
  hSetBinaryMode = IO.hSetBinaryMode
  hPutBuf = IO.hPutBuf
  hGetBuf = IO.hGetBuf
  hPutBufNonBlocking = IO.hPutBufNonBlocking
  hGetBufNonBlocking = IO.hGetBufNonBlocking



class IOEffectTemp t where
  openTempFile :: IO.FilePath -> String -> t (IO.FilePath, IO.Handle)
  openBinaryTempFile :: IO.FilePath -> String -> t (IO.FilePath, IO.Handle)
  openTempFileWithDefaultPermissions :: IO.FilePath -> String -> t (IO.FilePath, IO.Handle)
  openBinaryTempFileWithDefaultPermissions :: IO.FilePath -> String -> t (IO.FilePath, IO.Handle)

instance IOEffectTemp IO.IO where
  openTempFile = IO.openTempFile
  openBinaryTempFile = IO.openBinaryTempFile
  openTempFileWithDefaultPermissions = IO.openTempFileWithDefaultPermissions
  openBinaryTempFileWithDefaultPermissions = IO.openBinaryTempFileWithDefaultPermissions



class IOEffectEncoding t where
  hSetEncoding :: IO.Handle -> IO.TextEncoding -> t ()
  hGetEncoding :: IO.Handle -> t (Maybe IO.TextEncoding)
  mkTextEncoding :: String -> t IO.TextEncoding

instance IOEffectEncoding IO.IO where
  hSetEncoding = IO.hSetEncoding
  hGetEncoding = IO.hGetEncoding
  mkTextEncoding = IO.mkTextEncoding



class IOEffectNewline t where
  hSetNewlineMode :: IO.Handle -> IO.NewlineMode -> t ()

instance IOEffectNewline IO.IO where
  hSetNewlineMode = IO.hSetNewlineMode
