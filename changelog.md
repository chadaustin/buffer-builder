0.2.4.4

* Add 'stdc++' as an explicit library dependency.  (Thanks Rob Dockins!)

0.2.4.3

* Fix a bug where control characters (codepoints 1 through 31) were not being escaped.

0.2.4.2

* Include test.json in sdist so benchmarks can be run from Hackage.  (Thanks Ryan Scott!)

0.2.4.1

* Fix a bug with appendEscapedJsonText and Text buffers with nonzero offsets. (Thanks Joe Lee!)

0.2.4.0

* Add the ability to calculate the output length of a BufferBuilder without allocating or writing bytes

0.2.3.0

* Add the ability to query the current buffer size
* Add the ability to return a value from a BufferBuilder

0.2.2.2

* Add the ability to percent-encode directly into a Utf8Builder
* Add Utf8Builder.unsafeAppendBufferBuilder

0.2.2.0

* Add the ability to encode custom types as JSON keys

0.2.1.0

* Fix a buffer overrun in the double serializer
* Add support for URL percent-encoding
* Tweak the BufferWriter struct to improve code generation
