structure Parse = ParseFun(Lexer)

structure UXML = struct
  type pi = { target : string, content : string }
  type attribute = { ns : string, name : string, attvalue : string }
  type nsdecl = { nsattname : string, nsattvalue : string }
  datatype misc = Comment of string | PI of pi
  type prolog = { xmldecl : attribute list, misc : misc list }

  datatype element = Element of {
                       ns : string,
                       name : string,
                       attributes : attribute list,
                       nsdecls : nsdecl list,
                       contents : content list }
       and content = CharData of string
                   | ElementContent of element
                   | CDSect of string
                   | PIContent of pi
                   | CommentContent of string

  type document = { prolog : prolog, root : element, misc : misc list }

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

  fun fromDocument (Parse.Ast.Document (span, prolog, root, misc)) =
        { prolog = fromProlog prolog,
          root = fromElement [] root,
          misc = fromMisc' misc }
  and fromComment (Parse.Ast.EmptyComment (span)) = ""
    | fromComment (Parse.Ast.Comment (span, comment)) = comment
  and fromPI (Parse.Ast.EmptyPI (span, target)) =
        { target = target, content = "" }
    | fromPI (Parse.Ast.PI (span, target, content)) =
        { target = target, content = fromChars' content }
  and fromProlog (Parse.Ast.Prolog1 (span, misc)) =
        { xmldecl = [], misc = fromMisc' misc }
    | fromProlog (Parse.Ast.Prolog2 (span, xmldecl, misc)) =
        { xmldecl = fromXMLDecl xmldecl, misc = fromMisc' misc }
  and fromXMLDecl (Parse.Ast.XMLDecl (span, attributes)) = fromPseudoAttr' attributes
  and fromPseudoAttr (Parse.Ast.PseudoAttr1 (span, name, attvalue)) =
        { name = name, attvalue = attvalue }
    | fromPseudoAttr (Parse.Ast.PseudoAttr2 (span, name, attvalue)) =
        { name = name, attvalue = attvalue }
  and fromPseudoAttr' xs = map fromPseudoAttr xs
  and fromMisc (Parse.Ast.CommetnMisc (span, comment)) =
        Comment (fromComment comment)
    | fromMisc (Parse.Ast.PIMisc (span, pi)) = PI (fromPI pi)
    | fromMisc (Parse.Ast.SMisc (span, s)) = raise Fail "TODO"
  and fromMisc' xs = (map fromMisc xs)
  and fromElement bindings (Parse.Ast.EmptyElement (span, emptyElemTag)) =
        fromEmptyElemTag bindings emptyElemTag
    | fromElement bindings (Parse.Ast.Element (span, sTag, contents, eTag)) =
        let
          val (sTagName, bindings', attributes, nsdecls) = fromSTag bindings sTag
          val contents = fromContent' bindings' contents
          val eTagName = fromETag bindings' eTag
          (* TODO: WFC: Element Type Match *)
        in
          Element { ns = "", (* TODO *)
                    name = sTagName,
                    attributes = attributes,
                    nsdecls = [], (* TODO *)
                    contents = contents }
        end
  and fromSTag bindings (Parse.Ast.Stag (span, name, attributes)) =
        let
          val (prefix, name) =
            case splitName name of
                 SOME x => x
               | NONE => raise Fail "invalid QName"
          val (bindings', attributes, nsdecls) = fromAttribute' prefix bindings attributes
        in
          (name, bindings', attributes, nsdecls)
        end
  and fromAttribute (Parse.Ast.Attribute (span, name, attvalue)) =
        case splitName name of
             NONE => raise Fail "invalid QName"
           | SOME (prefix, name) => (prefix, name, attvalue)
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
          (* TODO: error handling *)
          val SOME elementNs = lookupNs (elementPrefix, bindings')
          fun resolveNs (prefix, name, value) =
                let
                  (* TODO: error handling *)
                  val SOME ns = lookupNs (prefix, bindings')
                in
                  { ns = if ns = "" then elementNs else ns,
                    name = name,
                    attvalue = value }
                end
          val attributes = map resolveNs (List.filter (negate isNsdecl) triples)
        in
          (bindings', attributes, nsdecls)
        end
  and fromETag bindings (Parse.Ast.ETag (span, name)) = name
  and fromContent bindings (Parse.Ast.CharDataContent (span, chars)) =
        CharData (fromChars chars)
    | fromContent bindings (Parse.Ast.ElementContent (span, element)) =
        ElementContent (fromElement bindings element)
    | fromContent bindings (Parse.Ast.CDSectContent (span, cdsect)) = CDSect cdsect
    | fromContent bindings (Parse.Ast.PIContent (span, pi)) = PIContent (fromPI pi)
    | fromContent bindings (Parse.Ast.CommentContent (span, comment)) =
        CommentContent (fromComment comment)
  and fromContent' bindings xs = map (fromContent bindings) xs
  and fromEmptyElemTag bindings (Parse.Ast.EmptyElemTag (span, name, attributes)) =
        let
          val (prefix, name) =
            case splitName name of
                 SOME x => x
               | NONE => raise Fail "invalid QName"
          val (bindings', attributes, nsdecls) = fromAttribute' prefix bindings attributes
        in
          Element { ns = "", (* TODO *)
                    name = name,
                    attributes = attributes,
                    nsdecls = nsdecls,
                    contents = [] }
        end
  and fromChars (Parse.Ast.Chars (span, chars)) = chars
  and fromChars' xs = concat (map fromChars xs)
end
