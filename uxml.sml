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
               [_] => (NONE, name)
             | [nsprefix, name] => (SOME nsprefix, name)
             | _ => (NONE, name)
        end

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
                   root = fromElement root,
                   epilog = List.mapPartial fromMisc misc }
  and fromComment (Parse.Ast.EmptyComment (span)) = Comment ""
    | fromComment (Parse.Ast.Comment (span, comment)) = Comment comment
  and fromPI (Parse.Ast.EmptyPI (span, target)) =
        PI { target = target, content = "" }
    | fromPI (Parse.Ast.PI (span, target, content)) =
        PI { target = target, content = concat (map fromChars content) }
  and fromProlog (Parse.Ast.Prolog1 (span, misc)) = List.mapPartial fromMisc misc
    | fromProlog (Parse.Ast.Prolog2 (span, xmldecl, misc)) = List.mapPartial fromMisc misc
  and fromMisc (Parse.Ast.CommetnMisc (span, comment)) =
        SOME (fromComment comment)
    | fromMisc (Parse.Ast.PIMisc (span, pi)) = SOME (fromPI pi)
    | fromMisc (Parse.Ast.SMisc (span, s)) =
        if List.all Char.isSpace (explode s) then NONE
        else raise Fail "non-space char in misc"
  and fromElement (Parse.Ast.EmptyElement (span, emptyElemTag)) =
        fromEmptyElemTag emptyElemTag
    | fromElement (Parse.Ast.Element (span, sTag, contents, eTag)) =
        let
          val (nsprefix, name, attributes) = fromSTag sTag
          val contents = map fromContent contents
          val (nsprefix', name') = fromETag eTag
          (* TODO: WFC: Element Type Match *)
        in
          Element { nsprefix = nsprefix,
                    name = name,
                    attributes = attributes,
                    contents = contents }
        end
  and fromEmptyElemTag (Parse.Ast.EmptyElemTag (span, name, attributes)) =
        let
          val (nsprefix, name) = splitName name
          val attributes = map fromAttribute attributes
        in
          Element { nsprefix = nsprefix,
                    name = name,
                    attributes = attributes,
                    contents = [] }
        end
  and fromSTag (Parse.Ast.Stag (span, name, attributes)) =
        let
          val (nsprefix, name) = splitName name
          val attributes = map fromAttribute attributes
        in
          (nsprefix, name, attributes)
        end
  and fromETag (Parse.Ast.ETag (span, name)) = splitName name
  and fromAttribute (Parse.Ast.Attribute (span, name, attvalue)) =
        case splitName name of
             (NONE, name) =>
               Attr {nsprefix = NONE, name = name, attvalue = derefCharData attvalue}
           | (SOME "xmlns", name) =>
               NSDecl {nsprefix = name, uri = derefCharData attvalue}
           | (SOME nsprefix, name) =>
               Attr {nsprefix = SOME nsprefix, name = name, attvalue = derefCharData attvalue}
  and fromContent (Parse.Ast.CharDataContent (span, chars)) =
        CharData (fromChars chars)
    | fromContent (Parse.Ast.ElementContent (span, element)) =
        ElementContent (fromElement element)
    | fromContent (Parse.Ast.CDSectContent (span, cdsect)) =
        CharData cdsect
    | fromContent (Parse.Ast.PIContent (span, pi)) = MiscContent (fromPI pi)
    | fromContent (Parse.Ast.CommentContent (span, comment)) =
        MiscContent (fromComment comment)
  and fromChars (Parse.Ast.Chars (span, chars)) = chars
end
