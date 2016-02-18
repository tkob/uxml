# Setup

```
- CM.make "test.cm";
...
val it = true : bool
- open UXML.Path;
...
- infix |>;
infix |>
```

# childNS

```
- val doc = UXML.parseString "<r><c><c1/><c1/><c3>abc</c3></c><d><d1>ccc</d1>ddd</d></r>";
...
- val p = fromDocument doc;
...
- p |> childNS ("", "r") |> childNS ("", "c") |> childNS ("", "c1") |> get;
val it =
  [Element {attributes=[],contents=[],name="c1",nsprefix=NONE},
   Element {attributes=[],contents=[],name="c1",nsprefix=NONE}]
  : UXML.content list
```

# getAttr

```
- val doc = UXML.parseString "<r><c attr0='x' attr1='a'/><c attr1='b' attr2='y'/></r>";
...
- val p = fromDocument doc;
...
- p |> childNS ("", "r") |> childNS ("", "c") |> map (getAttr "attr1");
val it = [SOME "a",SOME "b"] : string option list
```

# getAttr

```
- val doc = UXML.parseString "<Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'><Relationship Target='./Signature.xml' Id='A5FFC797514BC' Type='http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/signature'/></Relationships>";
...
- val p = fromDocument doc;
...
- val ns = "http://schemas.openxmlformats.org/package/2006/relationships";
...
- p |> childNS (ns, "Relationships") |> childNS (ns, "Relationship") |> map (getAttr "Target");
val it = [SOME "./Signature.xml"] : string option list
```

# attr

```
- val doc = UXML.parseString "<r><c attr0='x' attr1='a'/><c attr1='b' attr2='y'/></r>";
...
- val p = fromDocument doc;
...
- p |> childNS ("", "r") |> childNS ("", "c") |> attr "attr1";
val it = ["a","b"] : string list
```

# attr

```
- val doc = UXML.parseString "<Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'><Relationship Target='./Signature.xml' Id='A5FFC797514BC' Type='http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/signature'/></Relationships>";
...
- val p = fromDocument doc;
...
- val ns = "http://schemas.openxmlformats.org/package/2006/relationships";
...
- p |> childNS (ns, "Relationships") |> childNS (ns, "Relationship") |> attr "Target";
val it = ["./Signature.xml"] : string list
```

# text

```
- val doc = UXML.parseString "<sst xmlns='http://schemas.openxmlformats.org/spreadsheetml/2006/main' count='2' uniqueCount='2'><si><t>Hello</t><phoneticPr fontId='1'/></si><si><t>World!</t><phoneticPr fontId='1'/></si></sst>";
val doc =
...
- val p = fromDocument doc;
...
- val ns = "http://schemas.openxmlformats.org/spreadsheetml/2006/main";
...
- p |> childNS (ns, "sst") |> childNS (ns, "si") |> childNS (ns, "t") |> text;
val it = ["Hello","World!"] : string list
- p |> childNS (ns, "sst") |> childNS (ns, "si") |> childNS (ns, "t") |> map getText |> map concat;
val it = ["Hello","World!"] : string list
```

# xmlconf/eduni/namespaces/1.0/007.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/007.xml");
...
- val a = "http://example.org/wine";
...
- val b = "http://Example.org/wine";
...
- val c = "http://example.org/Wine";
...
- p |> childNS ("", "foo") |> childNS ("", "bar") |> map (getAttrNS (a, "attr"));
val it = [SOME "1"] : string option list
- p |> childNS ("", "foo") |> childNS ("", "bar") |> map (getAttrNS (b, "attr"));
val it = [SOME "2"] : string option list
- p |> childNS ("", "foo") |> childNS ("", "bar") |> map (getAttrNS (c, "attr"));
val it = [SOME "3"] : string option list
```

# xmlconf/eduni/namespaces/1.0/008.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/008.xml");
...
- val a = "http://example.org/~wilbur";
...
- val b = "http://example.org/%7ewilbur";
...
- val c = "http://example.org/%7Ewilbur";
...
- p |> childNS ("", "foo") |> childNS ("", "bar") |> map (getAttrNS (a, "attr"));
val it = [SOME "1"] : string option list
- p |> childNS ("", "foo") |> childNS ("", "bar") |> map (getAttrNS (b, "attr"));
val it = [SOME "2"] : string option list
- p |> childNS ("", "foo") |> childNS ("", "bar") |> map (getAttrNS (c, "attr"));
val it = [SOME "3"] : string option list
```

# xmlconf/eduni/namespaces/1.0/001.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/001.xml");
...
- val ns = "http://example.org/namespace";
...
- p |> childNS (ns, "foo") |> get |> length;
val it = 1 : int
```

# xmlconf/eduni/namespaces/1.0/002.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/002.xml");
...
- val ns = "zarquon://example.org/namespace";
...
- p |> childNS (ns, "foo") |> get |> length;
val it = 1 : int
```

# xmlconf/eduni/namespaces/1.0/017.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/017.xml");
...
- p |> childNS ("", "foo") |> get |> length;
val it = 1 : int
```

# xmlconf/eduni/namespaces/1.0/027.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/027.xml");
...
- p |> childNS ("", "foo") |> get |> length;
val it = 1 : int
```

# xmlconf/eduni/namespaces/1.0/019.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/019.xml");
...
- val ns = "http://example.org/namespace";
...
- p |> childNS (ns, "foo") |> get |> length;
val it = 1 : int
```

# xmlconf/eduni/namespaces/1.0/020.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/020.xml");
...
- val a = "http://example.org/namespace";
...
- p |> childNS ("", "foo") |> map (getAttrNS (a, "attr"));
val it = [SOME "1"] : string option list
```

# xmlconf/eduni/namespaces/1.0/024.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/024.xml");
...
- val ns1 = "http://example.org/namespace";
...
- val ns2 = "http://example.org/other-namespace";
...
- p |> childNS (ns1, "foo") |> childNS (ns2, "foo") |> get |> length;
val it = 1 : int
```

# xmlconf/eduni/namespaces/1.0/018.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/018.xml");
...
- val ns = "http://example.org/namespace";
...
- p |> childNS (ns, "foo") |> get |> length;
val it = 1 : int
```

# xmlconf/eduni/namespaces/1.0/021.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/021.xml");
...
- val ns = "http://example.org/namespace";
...
- p |> childNS (ns, "foo") |> childNS ("", "foo") |> get |> length;
val it = 1 : int
```

# xmlconf/eduni/namespaces/1.0/022.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/022.xml");
...
- val ns1 = "http://example.org/namespace";
...
- val ns2 = "http://example.org/other-namespace";
...
- p |> childNS (ns1, "foo") |> childNS (ns2, "foo") |> get |> length;
val it = 1 : int
```

# xmlconf/eduni/namespaces/1.0/037.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/037.xml");
...
- val a = "http://example.org/~wilbur";
...
- val b = "http://example.org/~kipper";
...
- p |> childNS ("", "foo") |> childNS ("", "bar") |> map (getAttrNS (a, "attr"));
val it = [SOME "1"] : string option list
- p |> childNS ("", "foo") |> childNS ("", "bar") |> map (getAttrNS (b, "attr"));
val it = [SOME "2"] : string option list
```

# xmlconf/eduni/namespaces/1.0/038.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/038.xml");
...
- val a = "http://example.org/~wilbur";
...
- p |> childNS ("", "foo") |> childNS ("", "bar") |> map (getAttrNS (a, "attr"));
val it = [SOME "1"] : string option list
- p |> childNS ("", "foo") |> childNS ("", "bar") |> map (getAttr "attr");
val it = [SOME "2"] : string option list
```

# xmlconf/eduni/namespaces/1.0/039.xml

```
- val p = fromDocument (UXML.parseFile "xmlconf/eduni/namespaces/1.0/039.xml");
...
- val a = "http://example.org/~wilbur";
...
- val b = "http://example.org/~kipper";
...
- p |> childNS (a, "foo") |> childNS (b, "bar") |> map (getAttrNS (a, "attr"));
val it = [SOME "1"] : string option list
- p |> childNS (a, "foo") |> childNS (b, "bar") |> map (getAttr "attr");
val it = [SOME "2"] : string option list
```

