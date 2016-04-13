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

  datatype fragment = CharFrag of string | EntityRef of name

  datatype content = CharData of string
                   | Element of { nsprefix   : name option,
                                  name       : name,
                                  attributes : attribute list,
                                  contents   : content list }
                   | Reference of name
                   | Comment of string
                   | PI of { target  : name,
                             content : string }
       and attribute = Attr of { nsprefix : name option,
                                 name     : name,
                                 attvalue : string }
                     | NSDecl of { nsprefix : name,
                                   uri      : uri }
  type document = content list

  exception UXML of string * UXMLLexer.span

  fun showDocument contents =
        "[" ^ String.concatWith "," (map showContent contents ) ^ "]"
  and showAttribute (Attr {nsprefix, name, attvalue}) =
        "{nsprefix = " ^ Option.getOpt (nsprefix, "NONE") ^
        ", name = " ^ name ^
        ", attvalue = \"" ^ String.toString attvalue ^
        "\"}"
    | showAttribute (NSDecl {nsprefix, uri}) =
        "{nsprefix = " ^ nsprefix ^
        ", uri = " ^ uri ^
        "\"}"
  and showContent (CharData charData) =
        "(CharData \"" ^ String.toString charData ^ "\")"
    | showContent (Element {nsprefix, name, attributes, contents}) =
        "(Element {nsprefix = " ^ Option.getOpt (nsprefix, "NONE") ^
        ", name = " ^ name ^
        ", attributes = [" ^ String.concatWith ", " (map showAttribute attributes) ^
        "], contents = [" ^ String.concatWith "," (map showContent contents) ^
        "]})"
    | showContent (Reference name) =
        "(Reference " ^ name ^ ")"
    | showContent (Comment comment) = "(Comment \"" ^ String.toString comment ^ "\")"
    | showContent (PI {target, content}) =
        "(PI " ^
        "{target = " ^ target ^
        ", content = \"" ^ String.toString content ^
        "\"})"

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
               encodingDecl = Option.getOpt (encodingDecl, "UTF-8"),
               sdDecl = Option.getOpt (sdDecl, "no") },
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
          val startPos = UXMLLexer.getPos strm
          val sourcemap = AntlrStreamPos.mkSourcemap ()
          val parses = Parse.parse sourcemap strm
                handle UXMLLexer.UserDeclarations.UXMLLexer (msg, pos) =>
                  raise UXML (msg, (pos, pos))
        in
          case parses of
               [] => raise UXML ("no parses", (startPos, startPos))
             | [parse] => parse
             | _ => raise UXML ("multiple parses", (startPos, startPos))
        end

  fun splitList f l =
        let
          fun split [] left = (rev left, [])
            | split (x::xs) left =
                if f x then (rev left, x::xs)
                else split xs (x::left)
        in
          split l []
        end

  fun parse input1 instream =
        let
          val rawParse = parseRaw input1 instream
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
          val symbolTable : (name * fragment list) list =
                let
                  fun makeSymbolTable [] acc = rev acc
                    | makeSymbolTable (Parse.Ast.EntityDeclIntSubset (_,
                        Parse.Ast.GEDecl (_,
                          name,
                          Parse.Ast.EntityValueEntityDecl (_,
                            value)))::intsubsets) acc =
                        let
                          fun deref [] acc = rev acc
                            | deref (Parse.Ast.CharDataEntityValue (_, charData)::entityValues) acc =
                                deref entityValues (CharFrag charData::acc)
                            | deref (Parse.Ast.PERefEntityValue (_, reference)::entityValues) acc =
                                let
                                  val value = raise Fail "deref"
                                in
                                  deref entityValues (value::acc)
                                end
                            | deref (Parse.Ast.CharRefEntityValue (_, charRef)::entityValues) acc =
                                deref entityValues (CharFrag (encode (Word.fromInt charRef))::acc)
                            | deref (Parse.Ast.EntityRefEntityValue (_, reference)::entityValues) acc =
                                (* Entity reference is bypassed *)
                                deref entityValues (EntityRef reference::acc)
                        in
                          makeSymbolTable intsubsets ((name, deref value [])::acc)
                        end
                    | makeSymbolTable (Parse.Ast.PEReferenceIntSubset _::_) acc =
                        rev acc
                    | makeSymbolTable (_::intsubsets) acc =
                        makeSymbolTable intsubsets acc
                in
                  makeSymbolTable intsubsets []
                end
          fun lookupEntity "amp"  = SOME ([CharFrag "&"])
            | lookupEntity "lt"   = SOME ([CharFrag "<"])
            | lookupEntity "gt"   = SOME ([CharFrag ">"])
            | lookupEntity "apos" = SOME ([CharFrag "'"])
            | lookupEntity "quot" = SOME ([CharFrag "\""])
            | lookupEntity name =
                let
                  fun lookup [] = NONE
                    | lookup ((name', value)::symbolTable) =
                        if name = name' then SOME value
                        else lookup symbolTable
                in
                  lookup symbolTable
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
                    | lookupElem (Parse.Ast.PEReferenceIntSubset _::intsubsets) = CDATA
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
                    | lookupElem (Parse.Ast.PEReferenceIntSubset _::intsubsets) = []
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
          fun uniqueAttSpec attributes =
                let
                  fun getName (NSDecl {nsprefix = "", ...}) = "xmlns"
                    | getName (NSDecl {nsprefix, ...}) = "xmlns:" ^ nsprefix
                    | getName (Attr {nsprefix = NONE, name, ...}) = name
                    | getName (Attr {nsprefix = SOME nsprefix, name, ...}) = nsprefix ^ ":" ^ name
                  val attNames = map getName attributes
                  fun mem (x, []) = false
                    | mem (x, y::ys) = x = y orelse mem (x, ys)
                  fun uniq [] = true
                    | uniq (x::xs) = not (mem (x, xs)) andalso uniq xs
                in
                  uniq attNames
                end
          fun fromDocument (Parse.Ast.Document (span, contents)) =
                List.concat (map fromContent contents)
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
                  val contents = List.concat (map fromContent contents)
                  val (nsprefix', name') = fromETag eTag
                in
                  if nsprefix = nsprefix' andalso name = name' then
                    Element { nsprefix = nsprefix,
                              name = name,
                              attributes = attributes,
                              contents = contents }
                  else raise UXML ("WFC: Element Type Match", span)
                end
          and fromEmptyElemTag (Parse.Ast.EmptyElemTag (span, name, attributes)) =
                let
                  val (nsprefix, name) = splitName name
                  val defaults = defaultAttValues name
                  val attributes = mergeDefaultAttValues defaults attributes
                  val attributes = map (fromAttribute name) attributes
                  val () = if uniqueAttSpec attributes then ()
                           else raise UXML ("WFC: Unique Att Spec", span)
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
                  val () = if uniqueAttSpec attributes then ()
                           else raise UXML ("WFC: Unique Att Spec", span)
                in
                  (nsprefix, name, attributes)
                end
          and fromETag (Parse.Ast.ETag (span, name)) = splitName name
          and fromAttribute elemName (Parse.Ast.Attribute (span, attName, attvalues)) =
                let
                  val atttype = lookupAtttype (elemName, attName)
                  val attvalue = normalizeAttValue atttype attvalues
                in
                  case splitName attName of
                       (NONE, "xmlns") =>
                         NSDecl { nsprefix = "",
                                  uri = attvalue }
                     | (NONE, name) =>
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
                          val entityValue =
                            case lookupEntity reference of
                                 NONE => ""
                               | SOME fragments =>
                                   let
                                     fun toString [] = ""
                                       | toString (CharFrag charData::fragments) =
                                           charData ^ toString fragments
                                       | toString (EntityRef name::fragments) =
                                           case lookupEntity name of
                                                NONE => ""
                                              | SOME fragments' =>
                                                  toString fragments' ^ toString fragments
                                     val value = toString fragments
                                   in
                                     normalizeWhiteSpace value
                                   end
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
                [CharData (fromChars chars)]
            | fromContent (Parse.Ast.ElementContent (span, element)) =
                [fromElement element]
            | fromContent (Parse.Ast.CharRefContent (span, charRef)) =
                [CharData (encode (Word.fromInt charRef))]
            | fromContent (Parse.Ast.ReferenceContent (span, "amp")) =
                [CharData "&"]
            | fromContent (Parse.Ast.ReferenceContent (span, "lt")) =
                [CharData "<"]
            | fromContent (Parse.Ast.ReferenceContent (span, "gt")) =
                [CharData ">"]
            | fromContent (Parse.Ast.ReferenceContent (span, "apos")) =
                [CharData "'"]
            | fromContent (Parse.Ast.ReferenceContent (span, "quot")) =
                [CharData "\""]
            | fromContent (Parse.Ast.ReferenceContent (span, reference)) =
                [Reference reference]
            | fromContent (Parse.Ast.CDSectContent (span, cdsect)) =
                [CharData cdsect]
            | fromContent (Parse.Ast.PIContent (span, pi)) = [fromPI pi]
            | fromContent (Parse.Ast.CommentContent (span, comment)) =
                [fromComment comment]
            | fromContent (Parse.Ast.DoctypeContent (_, doctypedecl)) =
                fromDoctypedecl doctypedecl
          and fromDoctypedecl (Parse.Ast.Doctypedecl1 _) = []
            | fromDoctypedecl (Parse.Ast.Doctypedecl2 _) = []
            | fromDoctypedecl (Parse.Ast.Doctypedecl3 (_, _, intsubsets)) =
                List.mapPartial fromIntSubset intsubsets
            | fromDoctypedecl (Parse.Ast.Doctypedecl4 (_, _, _, intsubsets)) =
                List.mapPartial fromIntSubset intsubsets
          and fromIntSubset (Parse.Ast.PIIntSubset (_, pi)) = SOME (fromPI pi)
            | fromIntSubset _ = NONE
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
          fun resolveEntity (Element {nsprefix, name, attributes, contents}) =
                [Element { nsprefix = nsprefix,
                           name = name,
                           attributes = attributes,
                           contents = List.concat (map resolveEntity contents) }]
            | resolveEntity (Reference reference) = (
                case entityResolver reference of
                     NONE => []
                   | SOME fragments =>
                       let
                         fun toString [] = ""
                           | toString (CharFrag charData::fragments) =
                               charData ^ toString fragments
                           | toString (EntityRef name::fragments) =
                               "&" ^ name ^ ";" ^ toString fragments
                       in
                         List.concat (map resolveEntity (#2 (parse Substring.getc (Substring.full (toString fragments)))))
                       end)
            | resolveEntity content = [content]
          val contents = List.concat (map resolveEntity (trim contents))

          (* additional checking for well-formedness *)
          fun isElement (Element _) = true
            | isElement _ = false
          val (prolog, elemMisc) = splitList isElement contents
          val (elem, misc) = case elemMisc of
                                  [] => raise UXML ("no doc element", (0, 0))
                                | elem::misc => (elem, misc)
        in
          contents
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

  fun parseString s = parseDocument Substring.getc (Substring.full s)
  fun parseBytes bytes = parseString (Byte.bytesToString bytes)

  (* toCanon converts a document into James Clark's Canonical XML *)
  fun toCanon contents =
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
            | makeAttrName (NSDecl {nsprefix = "", ...}) = "xmlns"
            | makeAttrName (NSDecl {nsprefix, ...}) = "xmlns:" ^ nsprefix

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
            | fromContent (Reference reference) = ""
            | fromContent (Comment comment) = ""
            | fromContent (PI {target, content}) = "<?" ^ target ^ " " ^ content ^ "?>"
        in
          concat (map fromContent contents)
        end

  structure Path :> sig
    type node

    val |> : 'a * ('a -> 'b) -> 'b

    val fromDocument : document -> node list
    val get : node list -> content list

    val child : string -> node list -> node list
    val childNS : (string * string) -> node list -> node list

    val attr : string -> node list -> string list
    val attrNS : (string * string) -> node list -> string list

    val text : node list -> string list

    val getChild : string -> node -> node list
    val getChildNS : (string * string) -> node -> node list
    val getAttr : string -> node -> string option
    val getAttrNS : (string * string) -> node -> string option
    val getText : node -> string list
  end= struct
    datatype node = Root of content list
                  | Node of content * node

    infix |>
    fun a |> b = b a

    fun fromDocument contents = [Root contents]

    fun get nodes =
          let
            fun get' (Root contents) = raise Fail "Root"
              | get' (Node (content, node)) = content
          in
            map get' nodes
          end

    fun resolveNS (nsprefix', Node (Element {attributes, ...}, node)) =
          let
            fun lookup [] = resolveNS (nsprefix', node)
              | lookup (Attr _::attributes) = lookup attributes
              | lookup (NSDecl {nsprefix, uri}::attributes) =
                  if nsprefix = nsprefix' then uri
                  else lookup attributes
          in
            lookup attributes
          end
      | resolveNS (nsprefix, Node (_, node)) = resolveNS (nsprefix, node)
      | resolveNS (nsprefix, Root _) = ""

    fun getChildNS (uri', name') node =
          let
            fun filter (parentNode, contents) =
                  let
                    fun f (elem as Element {nsprefix, name, ...}) =
                          let
                            val nsprefix = Option.getOpt (nsprefix, "")
                            val node = Node (elem, parentNode)
                            val uri = resolveNS (nsprefix, node)
                          in
                            if uri = uri' andalso name = name'
                            then SOME node
                            else NONE
                          end
                      | f _ = NONE
                  in
                    List.mapPartial f contents
                  end
            fun childNS' (node as Root contents) =
                  filter (node, contents)
              | childNS' (node as Node (Element {contents, ...}, _)) =
                  filter (node, contents)
              | childNS' (Node (_, _)) = [] (* only elements have children *)
          in
            childNS' node
          end

    fun getChild name' node = getChildNS ("", name') node

    fun childNS (uri', name') nodes =
          List.concat (map (getChildNS (uri', name')) nodes)

    fun child name' nodes = childNS ("", name') nodes

    fun getAttr name' (Node (Element {attributes, ...}, _)) =
          let
            fun find [] = NONE
              | find (NSDecl _::attributes) = find attributes
              | find (Attr {nsprefix = NONE, name, attvalue}::attributes) =
                  if name = name' then SOME attvalue
                  else find attributes
              | find (Attr {nsprefix = SOME _, ...}::attributes) =
                  find attributes
          in
            find attributes
          end
      | getAttr _ _ = NONE

    fun getAttrNS (uri', name') (node as Node (Element {attributes, ...}, _)) =
          let
            fun find [] = NONE
              | find (NSDecl _::attributes) = find attributes
              | find (Attr {nsprefix = SOME nsprefix, name, attvalue}::attributes) =
                  let
                    val uri = resolveNS (nsprefix, node)
                  in
                    if uri = uri' andalso name = name'
                    then SOME attvalue
                    else find attributes
                  end
              | find (Attr {nsprefix = NONE, ...}::attributes) =
                  find attributes
          in
            find attributes
          end
      | getAttrNS (_, _) _ = NONE

    fun attr name' nodes =
          List.mapPartial (getAttr name') nodes

    fun attrNS (uri', name') nodes =
          List.mapPartial (getAttrNS (uri', name')) nodes

    fun getText (Root _) = []
      | getText (Node (Element {contents, ...}, _)) =
          let
            fun f (CharData charData) = SOME charData
              | f _ = NONE
          in
            List.mapPartial f contents
          end
      | getText (Node _) = []

    fun text nodes = List.concat (map getText nodes)

  end
end
