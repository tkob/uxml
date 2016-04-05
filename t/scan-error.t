# Setup

```
- CM.make "test.cm";
...
val it = true : bool
- fun parse s = (UXML.parseDocument Substring.getc (Substring.full s); "no exn") handle (UXMLLexer.UserDeclarations.UXMLLexer s) => s;
...
val parse = fn : string -> string
```

# unexpected char at INITIAL

```
- parse "<<r/>";
val it = "1.1 unexpected `<<', state=INITIAL" : string
```

# unexpected char at STAG

```
- parse "<r !></r>";
val it = "1.4 unexpected `!', state=STAG" : string
```

# unexpected char at STAG

```
- parse "<r //></r>";
val it = "1.4 unexpected `//', state=STAG" : string
```

# unexpected char at ETAG

```
- parse "<r></r!>";
val it = "1.7 unexpected `!', state=ETAG" : string
```
