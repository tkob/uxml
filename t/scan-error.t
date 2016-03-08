# Setup

```
- CM.make "test.cm";
...
val it = true : bool
- fun parse s = (UXML.parseDocument Substring.getc (Substring.full s); true);
...
val parse = fn : string -> bool
```

# unexpected char at INITIAL

```
- parse "<<r/>";

uncaught exception Fail [Fail: 1.1 unexpected `<<', state=INITIAL]
...
```

# unexpected char at STAG

```
- parse "<r !></r>";

uncaught exception Fail [Fail: 1.4 unexpected `!', state=STAG]
...
```

# unexpected char at STAG

```
- parse "<r //></r>";

uncaught exception Fail [Fail: 1.4 unexpected `//', state=STAG]
...
```

# unexpected char at ETAG

```
- parse "<r></r!>";

uncaught exception Fail [Fail: 1.7 unexpected `!', state=ETAG]
...
```
