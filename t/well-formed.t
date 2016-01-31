# Setup

```
- CM.make "test.cm";
...
val it = true : bool
- fun parse s = (UXML.parseDocument Substring.getc (Substring.full s); true);
...
val parse = fn : string -> bool
```

# Well-formed XML documents

## WF1

```
- parse "<r/>";
val it = true : bool
```

## WF2

```
- parse "<r />";
val it = true : bool
```

## WF3

```
- parse "<r /> ";
val it = true : bool
```

## WF4

```
- parse "<?xml version='1.0'?><r/>";
val it = true : bool
```

## WF5

```
- parse "<?xml version = '1.0' ?><r/>";
val it = true : bool
```

## WF6

```
- parse "<?xml version=\"1.0\"?><r/>";
val it = true : bool
```

## WF7

```
- parse "<?xml version = \"1.0\" ?><r/>";
val it = true : bool
```

## WF8

```
- parse "<r></r>";
val it = true : bool
```

## WF9

```
- parse "<r ></r >";
val it = true : bool
```

## WF10

```
- parse "<r att1=''></r>";
val it = true : bool
```

## WF11

```
- parse "<r att1=''/>";
val it = true : bool
```

## WF12

```
- parse "<r att1='' ></r>";
val it = true : bool
```

## WF13

```
- parse "<r att1='' />";
val it = true : bool
```

## WF14

```
- parse "<r att1=\"\"/>";
val it = true : bool
```

## WF15

```
- parse "<r att1='' att2=''/>";
val it = true : bool
```

## WF16
```
- parse "<r><c/></r>";
val it = true : bool
```

## WF17

```
- parse "<r><c/><c/></r>";
val it = true : bool
```

## WF18

```
- parse "<r>x<c/>y<c/>z</r>";
val it = true : bool
```

## WF19

```
- parse "<r><![CDATA[&><]]></r>";
val it = true : bool
```

## WF20

```
- parse "<r><?p?></r>";
val it = true : bool
```

## WF21

```
- parse "<r><?p ?></r>";
val it = true : bool
```

## WF22

```
- parse "<r><?p abc?></r>";
val it = true : bool
```

## WF23

```
- parse "<r><?p ??></r>";
val it = true : bool
```

## WF24

```
- parse "<r><?p abc??></r>";
val it = true : bool
```

## WF25

```
- parse "<r><?p ?abc?></r>";
val it = true : bool
```

## WF26

```
- parse "<?p abc?><r/>";
val it = true : bool
```

## WF27

```
- parse "<r/><?p abc?>";
val it = true : bool
```

## WF28

```
- parse "<r>&amp;&gt;&lt;&apos;&quot;</r>";
val it = true : bool
```

## WF29

```
- parse "<r>&#x0021;&#33;</r>";
val it = true : bool
```

## WF30

```
- parse "<r><!-- declarations for <head> & <body> --></r>";
val it = true : bool
```

## WF31

```
- parse "<r><!----></r>";
val it = true : bool
```

## WF32

```
- parse "<!-- declarations for <head> & <body> --><r/>";
val it = true : bool
```

## WF33

```
- parse "<r/><!-- declarations for <head> & <body> -->";
val it = true : bool
```
