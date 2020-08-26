# heapsize

heapsize provides functions to determine the size of Haskell data structures in
memory at run time, including circular data structures. All sizes are reported
in bytes.

Determining the size of simple data structures is relatively quick, but large
and complex data structures can be slow. For large and complex structures the
user may want to find a way to
and estimate the size of the full data structure from that.

heapsize is originally based on
[ghc-datasize](https://github.com/def-/ghc-datasize), but adds support for ghc
8.10.1 and beyond, as well as improving performance.
