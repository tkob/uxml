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

  datatype content = CharData of string
                   | Element of { nsprefix   : name option,
                                  name       : name,
                                  attributes : attribute list,
                                  contents   : content list }
                   | Reference of string
                   | Comment of string
                   | PI of { target  : string,
                             content : string }
       and attribute = Attr of { nsprefix : name option,
                                 name     : name,
                                 attvalue : string }
                     | NSDecl of { nsprefix : name,
                                   uri      : uri }
  type document = content list

  fun showDocument contents =
        "[" ^ String.concatWith "," (map showContent contents ) ^ "]"
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
    | showContent (Comment comment) = "(Comment \"" ^ String.toString comment ^ "\")"
    | showContent (PI pi) = "(PI " ^ showPi pi ^ ")"

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

  fun parseXMLDecl input1 ins =
        let
          exception XMLDecl
          infix --
          fun (ph1 -- ph2) ins =
                let
                  val (x, ins') = ph1 ins
                  val (y, ins'') = ph2 ins'
                in
                  ((x, y), ins'')
                end
          infix ||
          fun (ph1 || ph2) ins =
                ph1 ins handle XMLDecl => ph2 ins
          infix >>
          fun (ph >> f) ins =
                let
                  val (x, ins') = ph ins
                in
                  (f x, ins')
                end
          fun empty ins = ([], ins)
          fun repeat ph ins = (ph -- repeat ph >> (op::) || empty) ins
          fun opt ph ins = ((ph >> SOME) || (empty >> (fn _ => NONE))) ins
          fun ch c ins =
               case input1 ins of
                    NONE => raise XMLDecl
                  | SOME (c', ins') =>
                      if c = c' then (c, ins')
                      else raise XMLDecl
          fun s s ins =
                let
                  fun f (c, (s, ins)) =
                        let
                          val (c, ins') = ch c ins
                        in
                          (s ^ String.str c, ins')
                        end
                in
                  List.foldl f ("", ins) (explode s)
                end
          fun space ins =
                case input1 ins of
                     NONE => raise XMLDecl
                   | SOME (#" ", ins')    => ((), ins')
                   | SOME (#"\009", ins') => ((), ins')
                   | SOME (#"\010", ins') => ((), ins')
                   | SOME (#"\013", ins') => ((), ins')
                   | SOME _ => raise XMLDecl
          val spaces = repeat space
          fun number ins =
                case input1 ins of
                     NONE => raise XMLDecl
                   | SOME (c, ins') =>
                       if c >= #"0" andalso c <= #"9" then (c, ins')
                       else raise XMLDecl
          fun alpha ins =
                case input1 ins of
                     NONE => raise XMLDecl
                   | SOME (c, ins') =>
                       if (c >= #"a" andalso c <= #"z")
                       orelse (c >= #"A" andalso c <= #"Z")
                       then (c, ins')
                       else raise XMLDecl
          val alnum = alpha || number
          val eq = spaces -- ch #"=" -- spaces
          val versionNum =
                s "1." --
                ((number -- repeat number >> (op::)) >> implode)
                >> (op^)
          val versionInfo =
                (spaces -- s "version" -- eq --
                ((((ch #"'" -- versionNum >> #2) -- ch #"'") >> #1) ||
                (((ch #"\"" -- versionNum >> #2) -- ch #"\"") >> #1)))
                >> #2
          val encName =
                alpha -- repeat (alnum || ch #"." || ch #"_" || ch #"-")
                >> implode o (op::)
          val encodingDecl =
                (spaces -- s "encoding" -- eq --
                ((((ch #"'" -- encName >> #2) -- ch #"'") >> #1) ||
                (((ch #"\"" -- encName >> #2) -- ch #"\"") >> #1)))
                >> #2
          val sdDecl =
                (spaces -- s "standalone" -- eq --
                ((((ch #"'" -- (s "yes" || s "no") >> #2) -- ch #"'") >> #1) ||
                (((ch #"\"" -- (s "yes" || s "no") >> #2) -- ch #"\"") >> #1)))
                >> #2
          val xmlDecl =
                (s "<?xml"
                -- versionInfo -- (opt encodingDecl) -- (opt sdDecl)
                -- spaces -- s "?>")
                >> (fn (((((_, versionInfo), encodingDecl), sdDecl), _), _) =>
                        (versionInfo, encodingDecl, sdDecl))
        in
          let val ((versionInfo, encodingDecl, sdDecl), ins') = xmlDecl ins in
            ({ versionInfo = versionInfo,
               encodingDecl = Option.getOpt (encodingDecl, ""),
               sdDecl = Option.getOpt (sdDecl, "") },
             ins')
          end
          handle XMLDecl =>
            ({ versionInfo = "1.0",
               encodingDecl = "UTF-8",
               sdDecl = "no" },
             ins)
        end

  fun parseRaw input1 instream =
        let
          val strm = UXMLLexer.streamifyReader input1 instream
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
          fun deref s =
                let
                  fun take f xs =
                        let
                          fun take' [] acc = (rev acc, [])
                            | take' (x::xs) acc =
                                if (f x) then take' xs (x::acc)
                                else (rev acc, x::xs)
                        in
                          take' xs []
                        end
                  fun deref' [] acc = concat (rev acc)
                    | deref' (#"&":: #"#":: #"x"::cs) acc =
                        let
                          val (digits, cs') = take Char.isHexDigit cs
                          val #";" = hd cs'
                          val SOME word =
                            StringCvt.scanString
                            (Word.scan StringCvt.HEX)
                            (implode digits)
                        in
                          deref' (tl cs') (encode word::acc)
                        end
                    | deref' (#"&":: #"#"::cs) acc =
                        let
                          val (digits, cs') = take Char.isDigit cs
                          val #";" = hd cs'
                          val SOME word =
                            StringCvt.scanString
                            (Word.scan StringCvt.DEC)
                            (implode digits)
                        in
                          deref' (tl cs') (encode word::acc)
                        end
                    | deref' (c::cs) acc = deref' cs (String.str c::acc)
                in
                  deref' (explode s) []
                end
          val intsubsets =
                let
                  val Parse.Ast.Document (_, contents) = rawParse
                  fun getDoctypedecl [] = NONE
                    | getDoctypedecl (Parse.Ast.DoctypeContent (_, doctypedecl)::_) =
                        SOME doctypedecl
                    | getDoctypedecl (_::misc) = getDoctypedecl misc
                in
                  case getDoctypedecl contents of
                       NONE => []
                     | SOME (Parse.Ast.Doctypedecl1 (_, _)) => []
                     | SOME (Parse.Ast.Doctypedecl2 (_, _, _)) => []
                     | SOME (Parse.Ast.Doctypedecl3 (_, _, intsubsets)) =>
                         intsubsets
                     | SOME (Parse.Ast.Doctypedecl4 (_, _, _, intsubsets)) =>
                         intsubsets
                end
          val symbolTable : (name * string) list =
                let
                  fun makeSymbolTable [] acc = rev acc
                    | makeSymbolTable (Parse.Ast.EntityDeclIntSubset (_,
                        Parse.Ast.GEDecl (_,
                          name,
                          Parse.Ast.EntityValueEntityDecl (_,
                            value)))::intsubsets) acc =
                        makeSymbolTable intsubsets ((name, deref value)::acc)
                    | makeSymbolTable (Parse.Ast.PEReferenceIntSubset _::_) acc =
                        rev acc
                    | makeSymbolTable (_::intsubsets) acc =
                        makeSymbolTable intsubsets acc
                in
                  makeSymbolTable intsubsets []
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
          fun lookup [] name = NONE
            | lookup (Parse.Ast.EntityDeclIntSubset (_,
                        Parse.Ast.GEDecl (_,
                          name',
                          Parse.Ast.EntityValueEntityDecl (_,
                            value)))::intsubsets) name =
                if name = name' then SOME value
                else lookup intsubsets name
            | lookup (_::intsubsets) name = lookup intsubsets name
          fun lookupEntity "amp"  = SOME "&"
            | lookupEntity "lt"   = SOME "<"
            | lookupEntity "gt"   = SOME ">"
            | lookupEntity "apos" = SOME "'"
            | lookupEntity "quot" = SOME "\""
            | lookupEntity name =
                Option.map deref (lookup intsubsets name)
          fun fromDocument (Parse.Ast.Document (span, contents)) =
                map fromContent contents
          and fromComment (Parse.Ast.EmptyComment (span)) = Comment ""
            | fromComment (Parse.Ast.Comment (span, comment)) = Comment comment
          and fromPI (Parse.Ast.EmptyPI (span, target)) =
                PI { target = target, content = "" }
            | fromPI (Parse.Ast.PI (span, target, content)) =
                PI { target = target, content = concat (map fromChars content) }
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
                   (* For a white space character (#x20, #xD, #xA, #x9),
                    * append a space character (#x20) to the normalized
                    * value.
                    * For another character, append the character to the
                    * normalized value. *)
                  fun normalizeWhiteSpace s =
                        let
                          fun normalizeWhiteSpace' #"\009" = " "
                            | normalizeWhiteSpace' #"\010" = " "
                            | normalizeWhiteSpace' #"\013" = " "
                            | normalizeWhiteSpace' c = String.str c
                        in
                          String.translate normalizeWhiteSpace' s
                        end
                  fun normalize [] cs = concat (rev cs)
                    | normalize (Parse.Ast.CharDataAttValue (_, charData)::attvalues) cs =
                        normalize attvalues (normalizeWhiteSpace charData::cs)
                    | normalize (Parse.Ast.CharRefAttValue (_, charRef)::attvalues) cs =
                        normalize attvalues (encode (Word.fromInt charRef)::cs)
                    | normalize (Parse.Ast.ReferenceAttValue (_, reference)::attvalues) cs =
                        let
                          val entityValue = case lookupEntity reference of
                                                 NONE => "&" ^ reference ^ ";"
                                               | SOME value =>
                                                   normalizeWhiteSpace value
                        in
                          normalize attvalues (entityValue::cs)
                        end
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
            | fromContent (Parse.Ast.ReferenceContent (span, "amp")) =
                CharData "&"
            | fromContent (Parse.Ast.ReferenceContent (span, "lt")) =
                CharData "<"
            | fromContent (Parse.Ast.ReferenceContent (span, "gt")) =
                CharData ">"
            | fromContent (Parse.Ast.ReferenceContent (span, "apos")) =
                CharData "'"
            | fromContent (Parse.Ast.ReferenceContent (span, "quot")) =
                CharData "\""
            | fromContent (Parse.Ast.ReferenceContent (span, reference)) =
                Reference reference
            | fromContent (Parse.Ast.CDSectContent (span, cdsect)) =
                CharData cdsect
            | fromContent (Parse.Ast.PIContent (span, pi)) = fromPI pi
            | fromContent (Parse.Ast.CommentContent (span, comment)) =
                fromComment comment
            | fromContent (Parse.Ast.DoctypeContent (_, _)) = CharData ""
          and fromChars (Parse.Ast.Chars (span, chars)) = chars
        in
          (lookupEntity, fromDocument rawParse)
        end

  fun parseDocument input1 instream =
        let
          val (xmlDecl, ins') = parseXMLDecl input1 instream
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
          val (entityResolver, contents) = parse input1' ins'
          fun trim [] = []
            | trim (CharData _::contents) = trim contents
            | trim (content::contents) = content::trim contents
        in
          (entityResolver, trim contents)
        end

  fun parseFile fileName =
        let
         val ins = TextIO.openIn fileName
        in
          let
            val sourcemap = AntlrStreamPos.mkSourcemap' fileName
          in
            parseDocument TextIO.StreamIO.input1 (TextIO.getInstream ins)
            before TextIO.closeIn ins
          end
          handle e => (TextIO.closeIn ins; raise e)
        end

  (* toCanon converts a document into James Clark's Canonical XML *)
  fun toCanon (entityResolver, contents) =
        let
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

          fun makeAttrName (Attr {nsprefix = NONE, name, ...}) = name
            | makeAttrName (Attr {nsprefix = SOME nsprefix, name, ...}) =
                nsprefix ^ ":" ^ name
            | makeAttrName (NSDecl {nsprefix, ...}) =
                "xmlns:" ^ nsprefix

          and fromAttribute (attr as Attr {nsprefix, name, attvalue}) =
                makeAttrName attr ^ "=\"" ^ escape attvalue ^ "\""
            | fromAttribute (nsdecl as NSDecl {nsprefix, uri}) =
                makeAttrName nsdecl ^ "=\"" ^ escape uri ^ "\""
          and fromContent (CharData charData) = escape charData
            | fromContent (Element {nsprefix, name, attributes, contents}) =
                let
                  val name = makeName (nsprefix, name)
                  fun gt (a, b) = makeAttrName a > makeAttrName b
                  val attributes = ListMergeSort.sort gt attributes
                in
                  "<" ^ name
                  ^ concat (map (fn x => " " ^ fromAttribute x) attributes)
                  ^ ">"
                  ^ concat (map fromContent contents)
                  ^ "</" ^ name ^ ">"
                end
            | fromContent (Reference reference) =
                let
                  val SOME entityValue = entityResolver reference
                in
                  concat (map fromContent (#2 (parse Substring.getc (Substring.full entityValue))))
                end
            | fromContent (Comment comment) = ""
            | fromContent (PI {target, content}) = "<?" ^ target ^ " " ^ content ^ "?>"
        in
          concat (map fromContent contents)
        end
end
