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

  (* toCanon converts a document into James Clark's Canonical XML *)
  local
    fun escapeChar #"&" = "&amp;"
      | escapeChar #"<" = "&lt;"
      | escapeChar #">" = "&gt;"
      | escapeChar #"\"" = "&quot;"
      | escapeChar #"\009" = "&#9;"
      | escapeChar #"\010" = "&#10;"
      | escapeChar #"\013" = "&#13;"
      | escapeChar c = String.str c
    fun escape s = concat (map escapeChar (explode s))

    fun makeName (ns, name) =
          case ns of
               NONE => name
             | SOME ns => ns ^ ":" ^ name

    and fromAttribute (Attr {nsprefix, name, attvalue}) =
          let
            val name = makeName (nsprefix, name)
          in
            name ^ "=\"" ^ escape attvalue ^ "\""
          end
      | fromAttribute (NSDecl {nsprefix, uri}) =
          "xmlns:" ^ nsprefix ^ "=\"" ^ escape uri ^ "\""
    and fromContent (CharData charData) = escape charData
      | fromContent (Element {nsprefix, name, attributes, contents}) =
          let
            val name = makeName (nsprefix, name)
          in
            "<" ^ name
            ^ concat (map (fn x => " " ^ fromAttribute x) attributes)
            ^ ">"
            ^ concat (map fromContent contents)
            ^ "</" ^ name ^ ">"
          end
      | fromContent (MiscContent misc) = fromMisc misc
    and fromMisc (Comment comment) = ""
      | fromMisc (PI {target, content}) = "<?" ^ target ^ " " ^ content ^ "?>"
  in
    fun toCanon (Document {prolog, root, epilog}) =
          concat (map fromMisc prolog)
          ^ fromContent root
          ^ concat (map fromMisc epilog)
  end

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

  fun encode word =
        let
          val w2c = Char.chr o Word.toInt
        in
          if Word.< (word, 0wx10000) then UTF8.encode word
          else
            String.implode [
              w2c (Word.orb (0wxf0, Word.>> (word, 0w18))),
              w2c (Word.orb (0wx80, Word.andb (Word.>> (word, 0w12), 0wx3f))),
              w2c (Word.orb (0wx80, Word.andb (Word.>> (word, 0w6), 0wx3f))),
              w2c (Word.orb (0wx80, Word.andb (word, 0wx3f)))]
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
          val intsubsets =
                let
                  val misc = case rawParse of
                                  (Parse.Ast.Document (span, Parse.Ast.Prolog1 (_, misc), _, _)) => misc
                                | (Parse.Ast.Document (span, Parse.Ast.Prolog2 (_, _, misc), _, _)) => misc
                  fun getDoctypedecl [] = NONE
                    | getDoctypedecl (Parse.Ast.DoctypeMisc (_, doctypedecl)::_) =
                        SOME doctypedecl
                    | getDoctypedecl (_::misc) = getDoctypedecl misc
                in
                  case getDoctypedecl misc of
                       NONE => []
                     | SOME (Parse.Ast.Doctypedecl1 (_, _)) => []
                     | SOME (Parse.Ast.Doctypedecl2 (_, _, _)) => []
                     | SOME (Parse.Ast.Doctypedecl3 (_, _, intsubsets)) =>
                         intsubsets
                     | SOME (Parse.Ast.Doctypedecl4 (_, _, _, intsubsets)) =>
                         intsubsets
                end
          fun lookupAtttype (elemName, attName) =
                let
                  fun unboxName (Parse.Ast.Name (_, name)) = name
                  fun lookupAtt [] = NONE
                    | lookupAtt (Parse.Ast.AttDef (_, attName', atttype, defaultdecl)::attdefs) =
                        if attName = attName' then
                          SOME (case atttype of
                               Parse.Ast.StringType _ => CDATA
                             | Parse.Ast.IdType _ => ID
                             | Parse.Ast.IdrefType _ => IDREF
                             | Parse.Ast.IdrefsType _ => IDREFS
                             | Parse.Ast.EntityType _ => ENTITY
                             | Parse.Ast.EntitiesType _ => ENTITIES
                             | Parse.Ast.NmtokenType _ => NMTOKEN
                             | Parse.Ast.NmtokensType _ => NMTOKENS
                             | Parse.Ast.NotationType (_, names) =>
                                 NOTATION (map unboxName names)
                             | Parse.Ast.Enumeration (_, nmtokens) =>
                                 ENUMERATION (map unboxName nmtokens))
                        else
                          lookupAtt attdefs
                  fun lookupElem [] = CDATA
                      (* 3.3.3 Attribute-Value Normalization
                       * All attributes for which no declaration has been read
                       * SHOULD be treated by a non-validating processor as if
                       * declared CDATA *)
                    | lookupElem (Parse.Ast.PEReferenceIntSubset _::intsubsets) = lookupElem intsubsets
                    | lookupElem (Parse.Ast.ElementdeclIntSubset _::intsubsets) = lookupElem intsubsets
                    | lookupElem (Parse.Ast.AttlistDeclIntSubset (_, Parse.Ast.AttlistDecl (_, elemName', attdefs))::intsubsets) =
                        if elemName = elemName' then
                          case lookupAtt attdefs of
                               SOME t => t
                             | NONE => lookupElem intsubsets
                        else
                          lookupElem intsubsets
                    | lookupElem (Parse.Ast.EntityDeclIntSubset _::intsubsets) = lookupElem intsubsets
                    | lookupElem (Parse.Ast.NotationDeclIntSubset _::intsubsets) = lookupElem intsubsets
                    | lookupElem (Parse.Ast.PIIntSubset _::intsubsets) = lookupElem intsubsets
                in
                  lookupElem intsubsets
                end
          fun defaultAttValues elemName =
                let
                  fun defaultAttValue (Parse.Ast.AttDef (_, attName, atttype, defaultdecl)) =
                        case defaultdecl of
                             Parse.Ast.RequiredDefaultDecl _ => NONE
                           | Parse.Ast.ImpliedDefaultDecl _ => NONE
                           | Parse.Ast.FixedDefaultDecl (span, attvalue) =>
                               SOME (attName, attvalue)
                           | Parse.Ast.DefaultDecl (span, attvalue) =>
                               SOME (attName, attvalue)
                  fun lookupElem [] = []
                    | lookupElem (Parse.Ast.PEReferenceIntSubset _::intsubsets) = lookupElem intsubsets
                    | lookupElem (Parse.Ast.ElementdeclIntSubset _::intsubsets) = lookupElem intsubsets
                    | lookupElem (Parse.Ast.AttlistDeclIntSubset (_, Parse.Ast.AttlistDecl (_, elemName', attdefs))::intsubsets) =
                        if elemName = elemName' then
                          (List.mapPartial defaultAttValue attdefs) @ (lookupElem intsubsets)
                        else
                          lookupElem intsubsets
                    | lookupElem (Parse.Ast.EntityDeclIntSubset _::intsubsets) = lookupElem intsubsets
                    | lookupElem (Parse.Ast.NotationDeclIntSubset _::intsubsets) = lookupElem intsubsets
                    | lookupElem (Parse.Ast.PIIntSubset _::intsubsets) = lookupElem intsubsets
                in
                  lookupElem intsubsets
                end
          fun lookupAttribute attName [] = NONE
            | lookupAttribute attName (Parse.Ast.Attribute (_, attName', attvalues)::attributes) =
                if attName = attName' then SOME attvalues
                else lookupAttribute attName attributes
          fun mergeDefaultAttValues defaults attributes =
                let
                  fun merge ((name, attvalues), attributes) =
                        case lookupAttribute name attributes of
                             SOME _ => attributes
                           | NONE => Parse.Ast.Attribute ((0, 0), name, attvalues)::attributes
                in
                  List.foldl merge attributes defaults
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
                  val defaults = defaultAttValues name
                  val attributes = mergeDefaultAttValues defaults attributes
                  val attributes = map (fromAttribute name) attributes
                in
                  Element { nsprefix = nsprefix,
                            name = name,
                            attributes = attributes,
                            contents = [] }
                end
          and fromSTag (Parse.Ast.Stag (span, name, attributes)) =
                let
                  val (nsprefix, name) = splitName name
                  val defaults = defaultAttValues name
                  val attributes = mergeDefaultAttValues defaults attributes
                  val attributes = map (fromAttribute name) attributes
                in
                  (nsprefix, name, attributes)
                end
          and fromETag (Parse.Ast.ETag (span, name)) = splitName name
          and fromAttribute elemName (Parse.Ast.Attribute (span, attName, attvalues)) =
                let
                  val atttype= lookupAtttype (elemName, attName)
                  val attvalue = normalizeAttValue atttype attvalues
                in
                  case splitName attName of
                       (NONE, name) =>
                         Attr { nsprefix = NONE,
                                name = name,
                                attvalue = attvalue }
                     | (SOME "xmlns", name) =>
                         NSDecl { nsprefix = name,
                                  uri = attvalue }
                     | (SOME nsprefix, name) =>
                         Attr { nsprefix = SOME nsprefix,
                                name = name,
                                attvalue = attvalue }
                end
          and normalizeAttValue atttype attvalues =
                let
                  fun normalize [] cs = concat (rev cs)
                    | normalize (Parse.Ast.CharDataAttValue (_, charData)::attvalues) cs =
                        (* For a white space character (#x20, #xD, #xA, #x9),
                         * append a space character (#x20) to the normalized
                         * value.
                         * For another character, append the character to the
                         * normalized value. *)
                        let
                          fun normalizeWhiteSpace #"\009" = " "
                            | normalizeWhiteSpace #"\010" = " "
                            | normalizeWhiteSpace #"\013" = " "
                            | normalizeWhiteSpace c = String.str c
                        in
                          normalize attvalues (String.translate normalizeWhiteSpace charData::cs)
                        end
                    | normalize (Parse.Ast.CharRefAttValue (_, charRef)::attvalues) cs =
                        normalize attvalues (encode (Word.fromInt charRef)::cs)
                    | normalize (Parse.Ast.ReferenceAttValue (_, reference)::attvalues) cs =
                        raise Fail "normalizeAttValue: unimplemented"
                  val cdataNormalized = normalize attvalues []
                in
                  if atttype = CDATA then cdataNormalized
                  else
                    String.concatWith " " (String.tokens (fn c => c = #" ") cdataNormalized)
                end
          and fromContent (Parse.Ast.CharDataContent (span, chars)) =
                CharData (fromChars chars)
            | fromContent (Parse.Ast.ElementContent (span, element)) =
                fromElement element
            | fromContent (Parse.Ast.CharRefContent (span, charRef)) =
                CharData (encode (Word.fromInt charRef))
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
