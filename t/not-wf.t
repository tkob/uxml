# Setup

```
- CM.make "test.cm";
...
val it = true : bool
- fun parse s = (UXML.parseDocument Substring.getc (Substring.full s); "well-formed") handle UXML.UXML (msg, _) => msg;
...
val parse = fn : string -> string
```

# Not well-formed XML documents

## One and Only One Document Element

This is not an explicit WFC in the spec, but a syntactic rule [1].

```
- parse "hello";
val it = "no doc element" : string
```

```
- parse "<r/><r/>";
val it = "multiple doc elements" : string
...
```

## At Most One Doctype Declaration

This is not an explicit WFC in the spec, but a syntactic rule [1] and [22].

```
- parse "<!DOCTYPE doc [<!ELEMENT doc (#PCDATA)>]><!DOCTYPE doc [<!ELEMENT doc (#PCDATA)>]><doc></doc>";
val it = "multiple doctype decls" : string
```

```
- parse "<!DOCTYPE doc [<!ELEMENT doc (#PCDATA)>]><doc></doc><!DOCTYPE doc [<!ELEMENT doc (#PCDATA)>]>";
val it = "multiple doctype decls" : string
```

## Attribute Chars

This is not an explicit WFC in the spec, but a syntactic rule [10].

```
- parse "<r att=\"a<b\"/>";
val it = "[1.10] [state=ATT_QUOT]: unexpected `<'" : string
```

```
- parse "<r att=\"a&\"/>";
val it = "[1.10] [state=ATT_QUOT]: unexpected `&'" : string
```

```
- parse "<r att=\"a&#\"/>";
val it = "[1.10] [state=ATT_QUOT]: unexpected `&#'" : string
```

```
- parse "<r att=\"a&b\"/>";
val it = "[1.10] [state=ATT_QUOT]: unexpected `&b'" : string
```

```
- parse "<r att=\"a&#b\"/>";
val it = "[1.10] [state=ATT_QUOT]: unexpected `&#b'" : string
```

```
- parse "<r att='a<b'/>";
val it = "[1.10] [state=ATT_APOS]: unexpected `<'" : string
```

```
- parse "<r att='a&'/>";
val it = "[1.10] [state=ATT_APOS]: unexpected `&'" : string
```

```
- parse "<r att='a&#'/>";
val it = "[1.10] [state=ATT_APOS]: unexpected `&#'" : string
```

```
- parse "<r att='a&b'/>";
val it = "[1.10] [state=ATT_APOS]: unexpected `&b'" : string
```

```
- parse "<r att='a&#b'/>";
val it = "[1.10] [state=ATT_APOS]: unexpected `&#b'" : string
```

## Comment Strings

This is not an explicit WFC in the spec, but a syntactic rule [15].

```
- parse "<!-- comment ---><r/>";
val it = "[1.14] [state=XML_COMMENT]: unexpected `--'" : string
```

```
- parse "<!-- \001 --><r/>";
val it = "[1.6] [state=XML_COMMENT]: unexpected `\^A'" : string
```

```
- parse "<!DOCTYPE doc [<!-- comment --->]><doc/>";
val it = "[1.29] [state=DOCTYPE_COMMENT]: unexpected `--'" : string
```

```
- parse "<!DOCTYPE doc [<!-- \001 -->]><doc/>";
val it = "[1.21] [state=DOCTYPE_COMMENT]: unexpected `\^A'" : string
```

## Element Type Match

```
- parse "<r></r2>";
val it = "WFC: Element Type Match" : string
```

## Unique Att Spec

```
- parse "<r a='1' a='2'/>";
val it = "WFC: Unique Att Spec" : string
```

```
- parse "<r ns:a='1' ns:a='2'/>";
val it = "WFC: Unique Att Spec" : string
```

```
- parse "<r xmlns='1' xmlns='2'/>";
val it = "WFC: Unique Att Spec" : string
```

```
- parse "<r xmlns:a='1' xmlns:a='2'/>";
val it = "WFC: Unique Att Spec" : string
```
