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
