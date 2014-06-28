{-# LANGUAGE CPP #-}

-- |Functions to extract additional informations like permissions or size from a
-- sequence of files. Is relevant only when the sequence is on the disk, not virtual
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
module System.FileSequence.Status(module System.FileSequence.Status.Windows) where
import System.FileSequence.Status.Windows
#else
module System.FileSequence.Status(module System.FileSequence.Status.Posix) where
import System.FileSequence.Status.Posix
#endif

