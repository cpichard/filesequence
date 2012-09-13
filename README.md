FILESEQUENCE 
============

Description
------------

Filesequence is a toy project to learn and pratice haskell. It contains a library and a few tools to manipulate sequences of files, for example :

toto.0001.dpx  
toto.0002.dpx  
toto.0003.dpx  
toto.0004.dpx  

will find a sequence named "toto" with four frames (1 to 4) and dpx extension. The first tool is seqls, a specialized version of ls for displaying file
sequences in a directory.

Synopsis
--------

seqls [-rfg] [files ...] [directories ...]

seqrm

seqrename

seqmv

seqcp

Installation
------------
You would surely look at the binaries here :

You can still compile the project by yourself. First make sure you have the latest haskell platform installed with a least
GHC 7.4.0. Filesequence library uses the perl pcre regex library to match the frames of a sequence,
so you have to be sure that libprce is installed on your system as well as haskell pcre bindings:
The command :

`cabal install regex-pcre`

will install the haskell bindings for libpcre.
Once everything is setup, just run the following commands in the filesequence directory:

`cabal configure`

`cabal install`

The command `seqls` should now be installed in your haskell binary directory.

