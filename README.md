# UXML

UXML is a non-validating XML parser for Standard ML.

## Taste

```
- open UXML.Path; infix |>;
...
- val doc = UXML.parseString "<r><c><c1/><c1/><c3>abc</c3></c><d><d1>ccc</d1>ddd</d></r>";
val doc =
  [Element
     {attributes=[],contents=[Element #,Element #],name="r",nsprefix=NONE}]
  : UXML.content list
- UXML.Path.fromDocument doc |> child "r" |> child "c" |> child "c1" |> get;
val it =
  [Element {attributes=[],contents=[],name="c1",nsprefix=NONE},
   Element {attributes=[],contents=[],name="c1",nsprefix=NONE}]
  : UXML.content list
```

## Status

UXML can parse all valid/invalid XML files in
[XML W3C Conformance Test Suite](http://www.w3.org/XML/Test/xmlconf-20080827.html)
except for non-UTF8 files and files including a character >= $#x10000;.

UXML produces correct Canonical XML files for all valid/invalid files in the
test suite as a non-validating XML processor.
('correct' here means 'it behaves same as expat')

## Comparison with other XML parser written in SML

|                           | UXML | fxp | XML from smlnj-lib |
|---------------------------|------|-----|--------------------|
| validation against DTD    | No   | Yes | No                 |
| DTD recognition           | Yes  | Yes | No                 |
| namespace                 | Yes  | No  | No                 |
| XPath-like navigation API | Yes  | No  | No                 |

## Build Requirements

- an SML implementation with SML/NJ Library (smlnj-lib) and ML Language Processig Tools (ml-lpt)
- [proglr](https://github.com/tkob/proglr) command

## Test Requirements

- SML/NJ (with smlnj-lib and ml-lpt)
- [proglr](https://github.com/tkob/proglr) command
- autom4te command from autotools
- [rehearsal](https://github.com/tkob/rehearsal) command
- prove command from Perl
- xmlwf command from expat
