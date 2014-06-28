{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
module System.FileSequence.Manip(module System.FileSequence.Manip.Windows) where
import System.FileSequence.Manip.Windows
#else
module System.FileSequence.Manip(module System.FileSequence.Manip.Posix) where
import System.FileSequence.Manip.Posix
#endif


