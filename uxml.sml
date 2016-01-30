structure Parse = ParseFun(UXMLLexer)

structure UXML = struct
  type name = string
  type nmtoken = string
  type uri = string
  datatype atttype = CDATA
                   | ID
                   | IDREF
                   | IDREFS
                   | ENTITY
                   | ENTITIES
                   | NMTOKEN
                   | NMTOKENS
                   | NOTATION of name list
                   | ENUMERATION of nmtoken list

  datatype document = Document of { prolog : misc list,
                                    root   : content,
                                    epilog : misc list }
       and content = CharData of string
                   | Element of { nsprefix   : name option,
                                  name       : name,
                                  attributes : attribute list,
                                  contents   : content list }
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
        "], root = " ^ showContent root ^
        ", misc = [" ^ String.concatWith "," (map showMisc epilog) ^
        "]}"
  and showMisc (Comment comment) = "(Comment \"" ^ String.toString comment ^ "\")"
    | showMisc (PI pi) = "(PI " ^ showPi pi ^ ")"
  and showPi {target, content} =
        "{target = " ^ target ^
        ", content = \"" ^ String.toString content ^
        "\"}"
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
    | showContent (Element {nsprefix, name, attributes, contents}) =
        "(Element {nsprefix = " ^ Option.getOpt (nsprefix, "NONE") ^
        ", name = " ^ name ^
        ", attributes = [" ^ String.concatWith ", " (map showAttribute attributes) ^
        "], contents = [" ^ String.concatWith "," (map showContent contents) ^
        "]})"
    | showContent (MiscContent misc) = "(MiscContent " ^ showMisc misc ^ ")"

  fun splitName name =
        let
          val fields = String.fields (fn c => c = #":") name
        in
          case fields of
               [_] => (NONE, name)
             | [nsprefix, name] => (SOME nsprefix, name)
             | _ => (NONE, name)
        end

  fun parseRaw input1 instream =
        let
          (* 2.11 End-of-Line Handling *)
          fun input1' ins =
                case input1 ins of
                     NONE => NONE
                   (* translating both the two-character sequence #xD #xA and
                    * any #xD that is not followed by #xA to a single #xA
                    * character *)
                   | SOME (#"\013", ins') =>
                       (case input1 ins' of
                            NONE => SOME (#"\010", ins')
                          | SOME (#"\010", ins'') => SOME (#"\010", ins'')
                          | SOME _ => SOME (#"\010", ins'))
                   | SOME (c, ins') => SOME (c, ins')
          val strm = UXMLLexer.streamifyReader input1' instream
          val sourcemap = AntlrStreamPos.mkSourcemap ()
          val parses = Parse.parse sourcemap strm
        in
          parses
        end

  fun parse input1 instream =
        let
          val rawParse = case parseRaw input1 instream of
                              [] => raise Fail "no parses"
                            | [parse] => parse
                            | _ => raise Fail "multiple parses"
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
            | fromMisc (Parse.Ast.DoctypeMisc (span, doctype)) = NONE
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
          and fromAttribute (Parse.Ast.Attribute (span, name, attvalues)) =
                case splitName name of
                     (NONE, name) =>
                       Attr { nsprefix = NONE,
                              name = name,
                              attvalue = concat (map fromAttValue attvalues) }
                   | (SOME "xmlns", name) =>
                       NSDecl { nsprefix = name,
                                uri = concat (map fromAttValue attvalues) }
                   | (SOME nsprefix, name) =>
                       Attr { nsprefix = SOME nsprefix,
                              name = name,
                              attvalue = concat (map fromAttValue attvalues) }
          and fromAttValue (Parse.Ast.CharDataAttValue (span, charData)) =
                charData
            | fromAttValue (Parse.Ast.ReferenceAttValue (span, reference)) =
                raise Fail "fromAttValue: unimplemented"
          and fromContent (Parse.Ast.CharDataContent (span, chars)) =
                CharData (fromChars chars)
            | fromContent (Parse.Ast.ElementContent (span, element)) =
                fromElement element
            | fromContent (Parse.Ast.ReferenceContent (span, reference)) =
                raise Fail "ReferenceContent: unimplemented"
            | fromContent (Parse.Ast.CDSectContent (span, cdsect)) =
                CharData cdsect
            | fromContent (Parse.Ast.PIContent (span, pi)) = MiscContent (fromPI pi)
            | fromContent (Parse.Ast.CommentContent (span, comment)) =
                MiscContent (fromComment comment)
          and fromChars (Parse.Ast.Chars (span, chars)) = chars
        in
          fromDocument rawParse
        end

  fun parseFile fileName =
        let
         val ins = TextIO.openIn fileName
        in
          let
            val sourcemap = AntlrStreamPos.mkSourcemap' fileName
          in
            parse TextIO.StreamIO.input1 (TextIO.getInstream ins)
            before TextIO.closeIn ins
          end
          handle e => (TextIO.closeIn ins; raise e)
        end
end
