structure Parse = ParseFun(UXMLLexer)

structure UXML = struct
  type name = string
  type uri = string

  datatype document = Document of { prolog : misc list,
                                    root   : element,
                                    epilog : misc list }
       and element = Element of { nsprefix   : name option,
                                  name       : name,
                                  attributes : attribute list,
                                  contents   : content list }
       and content = CharData of string
                   | ElementContent of element
                   | MiscContent of misc
       and attribute = Attr of { nsprefix : name option,
                                 name     : name,
                                 attvalue : string }
                     | NSDecl of { nsprefix : name,
                                   uri      : uri }
       and misc = Comment of string
                | PI of { target  : string,
                          content : string }


  fun derefCharData cs : string = raise Fail "derefCharData: unimplemented"

  fun showDocument (Document {prolog, root, epilog}) =
        "{prolog = [" ^ String.concatWith "," (map showMisc prolog) ^
        "], root = " ^ showElement root ^
        ", misc = [" ^ String.concatWith "," (map showMisc epilog) ^
        "]}"
  and showMisc (Comment comment) = "(Comment \"" ^ String.toString comment ^ "\")"
    | showMisc (PI pi) = "(PI " ^ showPi pi ^ ")"
  and showPi {target, content} =
        "{target = " ^ target ^
        ", content = \"" ^ String.toString content ^
        "\"}"
  and showElement (Element {nsprefix, name, attributes, contents}) =
        "(Element {nsprefix = " ^ Option.getOpt (nsprefix, "NONE") ^
        ", name = " ^ name ^
        ", attributes = [" ^ String.concatWith ", " (map showAttribute attributes) ^
        "], contents = [" ^ String.concatWith "," (map showContent contents) ^
        "]})"
  and showAttribute (Attr {nsprefix, name, attvalue}) =
        "{nsprefix = " ^ Option.getOpt (nsprefix, "NONE") ^
        ", name = " ^ name ^
        ", attvalue = \"" ^ String.toString attvalue ^
        "\"}"
  and showNsdecl {nsprefix, uri} =
        "{nsprefix = " ^ nsprefix ^
        ", uri = \"" ^ String.toString uri ^
        "\"}"
  and showContent (CharData charData) =
        "(CharData \"" ^ String.toString charData ^ "\")"
    | showContent (ElementContent element) =
        "(ElementContent " ^ showElement element ^ ")"
    | showContent (MiscContent misc) = "(MiscContent " ^ showMisc misc ^ ")"

  fun negate pred = (fn x => not (pred x))

  fun splitName name =
        let
          val fields = String.fields (fn c => c = #":") name
        in
          case fields of
               [name] => SOME ("", name)
             | [prefix, name] => SOME (prefix, name)
             | _ => NONE
        end

  fun lookupNs (prefix, []) = NONE
    | lookupNs (prefix, {nsattname, nsattvalue}::nsdecls) =
        if prefix = nsattname then SOME nsattvalue
        else lookupNs (prefix, nsdecls)

  fun parse input1 instream =
        let
          val strm = UXMLLexer.streamifyReader input1 instream
          val sourcemap = AntlrStreamPos.mkSourcemap ()
          val parses = Parse.parse sourcemap strm
        in
          parses
        end

  fun fromDocument (Parse.Ast.Document (span, prolog, root, misc)) =
        Document { prolog = fromProlog prolog,
                   root = fromElement [{nsattname = "", nsattvalue = ""}] root,
                   epilog = fromMisc' misc }
  and fromComment (Parse.Ast.EmptyComment (span)) = Comment ""
    | fromComment (Parse.Ast.Comment (span, comment)) = Comment comment
  and fromPI (Parse.Ast.EmptyPI (span, target)) =
        PI { target = target, content = "" }
    | fromPI (Parse.Ast.PI (span, target, content)) =
        PI { target = target, content = fromChars' content }
  and fromProlog (Parse.Ast.Prolog1 (span, misc)) = fromMisc' misc
    | fromProlog (Parse.Ast.Prolog2 (span, xmldecl, misc)) = fromMisc' misc
  and fromXMLDecl (Parse.Ast.XMLDecl (span, attributes)) = fromPseudoAttr' attributes
  and fromPseudoAttr (Parse.Ast.PseudoAttr1 (span, name, attvalue)) =
        { ns = "", name = name, attvalue = attvalue }
    | fromPseudoAttr (Parse.Ast.PseudoAttr2 (span, name, attvalue)) =
        { ns = "", name = name, attvalue = attvalue }
  and fromPseudoAttr' xs = map fromPseudoAttr xs
  and fromMisc (Parse.Ast.CommetnMisc (span, comment)) =
        SOME (fromComment comment)
    | fromMisc (Parse.Ast.PIMisc (span, pi)) = SOME (fromPI pi)
    | fromMisc (Parse.Ast.SMisc (span, s)) =
        if List.all Char.isSpace (explode s) then NONE
        else raise Fail "non-space char in misc"
  and fromMisc' xs = (List.mapPartial fromMisc xs)
  and fromElement bindings (Parse.Ast.EmptyElement (span, emptyElemTag)) =
        fromEmptyElemTag bindings emptyElemTag
    | fromElement bindings (Parse.Ast.Element (span, sTag, contents, eTag)) =
        let
          val (sTagName, bindings', attributes) = fromSTag bindings sTag
          val contents = fromContent' bindings' contents
          val eTagName = fromETag bindings' eTag
          (* TODO: WFC: Element Type Match *)
        in
          Element { nsprefix = NONE, (* TODO *)
                    name = sTagName,
                    attributes = attributes,
                    contents = contents }
        end
  and fromSTag bindings (Parse.Ast.Stag (span, name, attributes)) =
        let
          val (prefix, name) =
            case splitName name of
                 SOME x => x
               | NONE => raise Fail "invalid QName"
          val (bindings', attributes) = fromAttribute' prefix bindings attributes
        in
          (name, bindings', attributes)
        end
  and fromAttribute (Parse.Ast.Attribute (span, name, attvalue)) =
        case splitName name of
             NONE => raise Fail "invalid QName"
           | SOME (prefix, name) => (prefix, name, derefCharData attvalue)
  and fromAttribute' elementPrefix bindings xs =
        let
          val triples = map fromAttribute xs
          fun isNsdecl (prefix, name, _) =
                prefix = "xmlns" orelse name = "xmlns"
          fun tripleToNsdecl ("", "xmlns", value) =
                { nsattname = "", nsattvalue = value }
            | tripleToNsdecl (prefix, name, value) =
                { nsattname = name, nsattvalue = value }
          val nsdecls = map tripleToNsdecl (List.filter isNsdecl triples)
          val bindings' = nsdecls @ bindings
          val elementNs = lookupNs (elementPrefix, bindings')
          fun resolveNs (prefix, name, value) =
                let
                  val ns = lookupNs (prefix, bindings')
                in
                  Attr { nsprefix = if ns = NONE then elementNs else ns,
                         name = name,
                         attvalue = derefCharData value }
                end
          val attributes = map resolveNs (List.filter (negate isNsdecl) triples)
        in
          (bindings', attributes)
        end
  and fromETag bindings (Parse.Ast.ETag (span, name)) = name
  and fromContent bindings (Parse.Ast.CharDataContent (span, chars)) =
        CharData (fromChars chars)
    | fromContent bindings (Parse.Ast.ElementContent (span, element)) =
        ElementContent (fromElement bindings element)
    | fromContent bindings (Parse.Ast.CDSectContent (span, cdsect)) =
        CharData cdsect
    | fromContent bindings (Parse.Ast.PIContent (span, pi)) = MiscContent (fromPI pi)
    | fromContent bindings (Parse.Ast.CommentContent (span, comment)) =
        MiscContent (fromComment comment)
  and fromContent' bindings xs = map (fromContent bindings) xs
  and fromEmptyElemTag bindings (Parse.Ast.EmptyElemTag (span, name, attributes)) =
        let
          val (prefix, name) =
            case splitName name of
                 SOME x => x
               | NONE => raise Fail "invalid QName"
          val (bindings', attributes) = fromAttribute' prefix bindings attributes
        in
          Element { nsprefix = NONE, (* TODO *)
                    name = name,
                    attributes = attributes,
                    contents = [] }
        end
  and fromChars (Parse.Ast.Chars (span, chars)) = chars
  and fromChars' xs = concat (map fromChars xs)
end
