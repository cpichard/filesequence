filesequence
============
Filesequence contains a library and tools to manipulate sequences of files.
The first tool is seqls, a specialized version of ls for displaying file
sequences in a directory.

INSTALLATION
------------
First make sure you have the latest haskell platform installed with a least
GHC 7.4.0. Filesequence library uses the perl pcre regex library to match the frames of a sequence,
so you have to be sure that libprce is installed on your system as well as haskell pcre bindings:
The command :
    cabal install regex-pcre
will install the haskell bindings for libpcre.

Once everything is setup, just run
    cabal configure
    cabal install

The command seqls should now be installed in your haskell binary directory.

