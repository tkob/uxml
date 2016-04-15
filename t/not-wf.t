# Setup

```
- CM.make "test.cm";
...
val it = true : bool
- fun parse s = (UXML.parseDocument Substring.getc (Substring.full s); true);
...
val parse = fn : string -> bool
```

# Not well-formed XML documents

## One and Only One Document Element

This is not an explicit WFC in the spec, but a syntactic rule [1].

```
- parse "hello";

uncaught exception UXML
...
```

```
- parse "<r/><r/>";

uncaught exception UXML
...
```

## At Most One Doctype Declaration

This is not an explicit WFC in the spec, but a syntactic rule [1] and [22].

```
- parse "<!DOCTYPE doc [<!ELEMENT doc (#PCDATA)>]><!DOCTYPE doc [<!ELEMENT doc (#PCDATA)>]><doc></doc>";

uncaught exception UXML
...
```

```
- parse "<!DOCTYPE doc [<!ELEMENT doc (#PCDATA)>]><doc></doc><!DOCTYPE doc [<!ELEMENT doc (#PCDATA)>]>";

uncaught exception UXML
...
```

## Element Type Match

```
- parse "<r></r2>";

uncaught exception UXML
...
```

## Unique Att Spec

```
- parse "<r a='1' a='2'/>";

uncaught exception UXML
...
```

```
- parse "<r ns:a='1' ns:a='2'/>";

uncaught exception UXML
...
```

```
- parse "<r xmlns='1' xmlns='2'/>";

uncaught exception UXML
...
```

```
- parse "<r xmlns:a='1' xmlns:a='2'/>";

uncaught exception UXML
...
```
