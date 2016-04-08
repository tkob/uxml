# Setup

```
- CM.make "test.cm";
...
val it = true : bool
- fun parse s = (UXML.parseDocument Substring.getc (Substring.full s); "no exn") handle UXML.UXML (msg, span) => msg;
...
val parse = fn : string -> string
```

# unexpected char at INITIAL

```
- parse "<<r/>";
val it = "[1.1] [state=INITIAL]: unexpected `<<'" : string
```

# unexpected char at STAG

```
- parse "<r !></r>";
val it = "[1.4] [state=STAG]: unexpected `!'" : string
```

# unexpected char at STAG

```
- parse "<r //></r>";
val it = "[1.4] [state=STAG]: unexpected `//'" : string
```

# unexpected char at ETAG

```
- parse "<r></r!>";
val it = "[1.7] [state=ETAG]: unexpected `!'" : string
```
