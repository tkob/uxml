# Setup

```
- CM.make "test.cm";
...
val it = true : bool
- fun numParses s = length (UXML.parse Substring.getc (Substring.full s));
...
val numParses = fn : string -> int
```

# Well-formed XML documents

## WF1

```
- numParses "<r/>";
val it = 1 : int
```

## WF2

```
- numParses "<r />";
val it = 1 : int
```

## WF3

```
- numParses "<r /> ";
val it = 1 : int
```

## WF4

```
- numParses "<?xml version='1.0'?><r/>";
val it = 1 : int
```

## WF5

```
- numParses "<?xml version = '1.0' ?><r/>";
val it = 1 : int
```

## WF6

```
- numParses "<?xml version=\"1.0\"?><r/>";
val it = 1 : int
```

## WF7

```
- numParses "<?xml version = \"1.0\" ?><r/>";
val it = 1 : int
```

## WF8

```
- numParses "<r></r>";
val it = 1 : int
```

## WF9

```
- numParses "<r ></r >";
val it = 1 : int
```

## WF10

```
- numParses "<r att1=''></r>";
val it = 1 : int
```

## WF11

```
- numParses "<r att1=''/>";
val it = 1 : int
```

## WF12

```
- numParses "<r att1='' ></r>";
val it = 1 : int
```

## WF13

```
- numParses "<r att1='' />";
val it = 1 : int
```

## WF14

```
- numParses "<r att1=\"\"/>";
val it = 1 : int
```

## WF15

```
- numParses "<r att1='' att2=''/>";
val it = 1 : int
```

## WF16
```
- numParses "<r><c/></r>";
val it = 1 : int
```

## WF17

```
- numParses "<r><c/><c/></r>";
val it = 1 : int
```

## WF18

```
- numParses "<r>x<c/>y<c/>z</r>";
val it = 1 : int
```

## WF19

```
- numParses "<r><![CDATA[&><]]></r>";
val it = 1 : int
```

## WF20

```
- numParses "<r><?p?></r>";
val it = 1 : int
```

## WF21

```
- numParses "<r><?p ?></r>";
val it = 1 : int
```

## WF22

```
- numParses "<r><?p abc?></r>";
val it = 1 : int
```

## WF23

```
- numParses "<r><?p ??></r>";
val it = 1 : int
```

## WF24

```
- numParses "<r><?p abc??></r>";
val it = 1 : int
```

## WF25

```
- numParses "<r><?p ?abc?></r>";
val it = 1 : int
```

## WF26

```
- numParses "<?p abc?><r/>";
val it = 1 : int
```

## WF27

```
- numParses "<r/><?p abc?>";
val it = 1 : int
```

## WF28

```
- numParses "<r>&amp;&gt;&lt;&apos;&quot;</r>";
val it = 1 : int
```

## WF29

```
- numParses "<r>&#x0021;&#33;</r>";
val it = 1 : int
```

## WF30

```
- numParses "<r><!-- declarations for <head> & <body> --></r>";
val it = 1 : int
```

## WF31

```
- numParses "<r><!----></r>";
val it = 1 : int
```

## WF32

```
- numParses "<!-- declarations for <head> & <body> --><r/>";
val it = 1 : int
```

## WF33

```
- numParses "<r/><!-- declarations for <head> & <body> -->";
val it = 1 : int
```
