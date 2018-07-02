FILESEQUENCE 
============

Description
------------

This project contains the command line tools 'seqls', 'seqsum', 'seqcp', which are similar to 'ls', 'cp', 'shasum' but for sequences of files. 

Here is an example: 
```bash
>>> ls /tmp/sequences
>>> background.0001.jpg
>>> background.0002.jpg
>>> background.0003.jpg
>>> background.0004.jpg
```
with seqls:
```bash
>>> seqls /tmp/sequences
>>> background.%04d.jpg  1  4
```
Those tools are used a lot in the vfx and post production industries, where the sequences of files are very common. They are available for [download](https://github.com/cpichard/filesequence/releases) and don't require any dependencies.


Tools
--------

### seqls
List sequences of files in directories

        seqls [-RglmcsS] [-e exr,dpx,...] [-j 3] [-f nuke|rv|printf] [files ...] [directories ...]

### seqsum
Compute the hash value of all sequences of files in directories. It processes all the files with the SHA224 algorithm as if they were concatenated in a single block 

        seqsum [-Rj] [files ...] [directories ...]

### seqcp
Copy a subset of a sequence of files in a destination directory.

        seqcp [-vc] /path/to/sequence.%05d.exr [first frame] [last frame] [target directory]

Installation
------------
### Binaries

The binaries are available in the [releases page](https://github.com/cpichard/filesequence/releases). To install, just unzip, untar and copy the files in a executable path of your choice. If you need another platform, just email me or build the project from sources.

### Building from sources
The code is written in Haskell and you will need to install 'stack' if you want to build your own version. (see: https://docs.haskellstack.org/en/stable/README/). 
Once stack is installed, just retrieve the source code:

        git clone https://github.com/cpichard/filesequence

and run the following commands in the filesequence directory:
```bash
cd filesequence
stack build
stack install --local-bin-path BINARY_PATH
```
It will build and install the tools in `BINARY_PATH`.

Examples
--------
### List sequence in a directory with details
```bash
seqls -lg /path/to/directory

-rw-r--r--    93.09K   210.29K     7.95M         1       50  /path/to/directory/checker_board_distotest.%04d.png
-rw-r--r--    38.35K   133.74K     4.72M         1       50  /path/to/directory/checker_board_clean.%04d.png
```
There are 2 sequences in this folder. On the first sequence, the minimum size of a frame is 93.09K, the maximum 210.29K, the total size of the first sequence is 7.95M, the first frame is 1, last frame 50.

See also
--------
There are many other project similar to this one,

* python computer graphic kit http://cgkit.sourceforge.net/doc2/filesequencetools.html
* PySeq http://rsgalloway.github.com/pyseq/ widely used in the vfx industry.
* Fileseq https://github.com/sqlboy/fileseq
* Fast parser: https://github.com/gchatelet/light_sequence_parser 
* Sequence parser with python bindings: https://github.com/mikrosimage/sequenceparser
* Filesequence in go with a seqls version: https://github.com/justinfx/gofileseq
