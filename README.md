# UXML

UXML is a non-validating XML parser for Standard ML.

## Status

UXML can parse all valid/invalid XML files in
[XML W3C Conformance Test Suite](http://www.w3.org/XML/Test/xmlconf-20080827.html)
except for non-UTF8 files and files including a character >= $#x10000;.

UXML produces correct Canonical XML files for all valid/invalid files in the
test suite as a non-validating XML processor.
('correct' here means 'it behaves same as expat')
