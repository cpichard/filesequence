Issues

TODO now
========
* merge current developments in develop

Code health
===========
* rename tests with more meaningful names
* rename FileSequenceStatus by FileSequenceAttributes
* remove all unused functions, remove getRecursivedir function
* rename frameList to frameNames
* export library functions only in one file
* seqls to handle correctly exceptions
* change comments, add a space after the bar|
* fix all compilation warnings
* refresh the documentation

Bug fix
=======
* check if the display of rv seq is correct.
* option to display /detect with relative or global path 
   Bug => display global path or relative correctly. 
* handle the case where a file has multiple extension (.tar.gz) or decide if we don't care
* change symlinks to follow reference as in ls

Tests
=====
* build a list of named sequences with their associated filesequence structures to use in the tests ???? in a separate file ???
* finish to write arbitraries for file names
* add test with mixed folder and file having the same name
* add minimal sequence check ???? ()
* add a chrooted test that creates sequences on the disk and tries to read them

Improvements
============
Visual improvements
* display a header in the result list
* add color to the output. Color could be interesting for error reporting
    Display in color ? with red for possible problem ?
* seqls display range of sequence and they are not aligned, which is horrible. replace tabs per spaces ?

* compute date max min mid and add statistics on all the values retrieved by stat()
* output json with all data for pipelining
* add checksum in the data 
* add error reporting based on rules. Look at shellcheck, this could be a good code example 
* python bindings ? :D
* read sequence of files from a pipe. how does it work ? when do we know this is the end of the stream ?
* correct discrete integer encoding tree: the tests don't pass



