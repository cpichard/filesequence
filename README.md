FILESEQUENCE 
============

Description
------------

Filesequence was a toy project to learn and practice haskell. As of today it contains a library and several tools that make the life easier when you have to work with sequences of files. They are perfectly usable in production. By sequence of files we mean a set of files with a common prefix and suffix and a "counter" number in between. This library is specially tailored for the visual effect and post production industries, where the sequence of images or geometries generally share the same patterns.

Tools
--------

###seqls
List sequences of files in a directory

        seqls [-Rgl] [-f nuke|rv|printf] [files ...] [directories ...]

###seqsum
Compute the hash value of all sequence of file in directories. It processes all the files as a single block with the SHA224 algorithm.

        seqsum  [-Rj] [files ...] [directories ...]

###seqcp
Copy a sequence of file in a directory.

        seqcp [-vc] /path/to/sequence.%05d.exr [first frame] [last frame] [target directory]

Installation
------------
###Binaries

If you don't want to install the whole haskell eco-system, the binaries are available here :

Linux 64: development branch  
MacOSX: development branch

To install, just untar, unzip and copy the files in a executable path of your choice. If you need another platform, just email me or build the project from sources.

###Building from sources
First make sure you have the latest haskell platform installed with a least GHC 7.4.0. (see: http://hackage.haskell.org/platform/). This project also uses the perl pcre regex library which is quite standard on all unix platform.
Then install the haskell bindings for libpcre with the following command:

`cabal install regex-pcre-builtin`

Once everything is setup, just run the following commands in the filesequence directory:

`cabal configure`

`cabal install`

The commands `seqls`,`seqsum`and `seqcp` should now be installed in your haskell binary directory.

Examples
--------
### List sequence in a directory with details

        seqls -lg /path/to/directory

        -rw-r--r--    93.09K   210.29K     7.95M         1       50  /path/to/directory/checker_board_distotest.%04d.png
        -rw-r--r--    38.35K   133.74K     4.72M         1       50  /path/to/directory/checker_board_clean.%04d.png

There are 2 sequences in this folder, the minimun size of a frame is 93.09K, the maximum 210.29K, the total size of the first sequence is 7.95M, the first frame is 1, last frame 50.

See also
--------
* python computer graphic kit http://cgkit.sourceforge.net/doc2/filesequencetools.html
* PySeq http://rsgalloway.github.com/pyseq/ which are widely used in the vfx industry.
* https://github.com/gchatelet/light_sequence_parser 


