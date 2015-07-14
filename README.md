FILESEQUENCE 
============

Description
------------

This project contains a library and several tools to work with sequences of files sharing a common prefix, a common suffix and a number in between which acts as a counter. The tools are available for download and don't require any dependencies. 

To illustrate, here is an example of a sequence of file: 

        ls /tmp/sequences
        background.0001.jpg
        background.0002.jpg
        background.0003.jpg

and the seqls tool:

        seqls /tmp/sequences
        background.%04d.jpg  1  3

This library is especially tailored for visual effect and post production industries, where the sequences of files are very common, and generally share the same patterns. Hopefully the tools provided here should be usable in different contexts, let me know if you find them useful.

Tools
--------

###seqls
List sequences of files in a directory

        seqls [-RglmcsS] [-e exr,dpx,...] [-j 3] [-f nuke|rv|printf] [files ...] [directories ...]

###seqsum
Compute the hash value of all sequences of files in directories. It processes all the files with the SHA224 algorithm as if they were concatenated in a single block 

        seqsum [-Rj] [files ...] [directories ...]

###seqcp
Copy a subset of a sequence of files in a destination directory.

        seqcp [-vc] /path/to/sequence.%05d.exr [first frame] [last frame] [target directory]

Installation
------------
###Binaries

If you don't want to install the whole haskell eco-system, the binaries are available in the [releases page](https://github.com/cpichard/filesequence/releases). To install, just unzip, untar and copy the files in a executable path of your choice. If you need another platform, just email me or build the project from sources.

###Building from sources
First make sure you have the latest haskell platform installed with a least GHC 7.4.0. (see: http://hackage.haskell.org/platform/). Latest buil fo the project used GHC 7.8.3.
Once the platform is read, just retrieve the source code:

        git clone https://github.com/cpichard/filesequence

and run the following commands in the filesequence directory:

        cd filesequence
        cabal sandbox init
        cabal install --only-dependencies
        cabal configure
        cabal build
        cabal install --prefix=/where/you/want/your/binaries

It will build and install all the dependencies in a sandbox, which is a kind of container, and enventually build and install `seqls` `seqsum`and `seqcp` in the path describe in prefix.

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
* PySeq http://rsgalloway.github.com/pyseq/ widely used in the vfx industry.
* Fast parser: https://github.com/gchatelet/light_sequence_parser 
* Sequence parser with python bindings: https://github.com/mikrosimage/sequenceparser
* Filesequence in go with a seqls version: https://github.com/justinfx/gofileseq
