FILESEQUENCE 
============

Description
------------

Filesequence is a toy project to learn and practice haskell. It contains a library and a few tools to manipulate sequences of files, like seqls to display the list of sequences in a directory. In terms of functionality it's pretty similar to python computer graphic kit http://cgkit.sourceforge.net/doc2/filesequencetools.html or PySeq http://rsgalloway.github.com/pyseq/ which are widely used in the vfx industry.

Synopsis
--------

seqls [-Rg] [-f nuke|rv|printf] [files ...] [directories ...]

Installation
------------
###Binaries

If you don't want to install the whole haskell eco-system, the binaries are available here :

Linux 64:  
MacOSX:  
TODO : download links  

To install, just untar, unzip and copy the files in a executable path of your choice. If you need another platform, just email me or build the project from sources.

###Building from sources
First make sure you have the latest haskell platform installed with a least GHC 7.4.0. (see: http://hackage.haskell.org/platform/). This project also uses the perl pcre regex library which is quite standard on all unix platform and should be already installed but you might have to install the developers library as well:

`apt-get install libpcre-dev`
 
Then install the haskell bindings for libpcre with the following command:

`cabal install regex-pcre`

Once everything is setup, just run the following commands in the filesequence directory:

`cabal configure`

`cabal install`

The command `seqls` should now be installed in your haskell binary directory.

Examples
--------

Diagnostic
----------

Environment
-----------

Bugs
----

See also
--------



