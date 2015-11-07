structure Parse = ParseFun(Lexer)

structure UXML = struct
  type pi = { target : string, content : string }
  type attribute = { name : string, attvalue : string }
  datatype misc = Comment of string | PI of pi
  type prolog = { xmldecl : attribute list, misc : misc list }

  datatype element = Element of {
                       name : string,
                       attributes : attribute list,
                       contents : content list }
       and content = CharData of string
                   | ElementContent of element
                   | CDSect of string
                   | PIContent of pi
                   | CommentContent of string

  type document = { prolog : prolog, root : element, misc : misc list }

  fun fromDocument (Parse.Ast.Document (span, prolog, root, misc)) =
        { prolog = fromProlog prolog,
          root = fromElement root,
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
  and fromElement (Parse.Ast.EmptyElement (span, emptyElemTag)) = fromEmptyElemTag emptyElemTag
    | fromElement (Parse.Ast.Element (span, sTag, contents, eTag)) =
        let
          val (sTagName, attributes) = fromSTag sTag
          val contents = fromContent' contents
          val eTagName = fromETag eTag
        in
          raise Fail "TODO"
        end
  and fromSTag (Parse.Ast.Stag (span, name, attributes)) = (name, fromAttribute' attributes)
  and fromAttribute (Parse.Ast.Attribute (span, name, attvalue)) =
        { name = name, attvalue = attvalue }
  and fromAttribute' xs = map fromAttribute xs
  and fromETag (Parse.Ast.ETag (span, name)) = name
  and fromContent (Parse.Ast.CharDataContent (span, chars)) =
        CharData (fromChars chars)
    | fromContent (Parse.Ast.ElementContent (span, element)) =
        ElementContent (fromElement element)
    | fromContent (Parse.Ast.CDSectContent (span, cdsect)) = CDSect cdsect
    | fromContent (Parse.Ast.PIContent (span, pi)) = PIContent (fromPI pi)
    | fromContent (Parse.Ast.CommentContent (span, comment)) =
        CommentContent (fromComment comment)
  and fromContent' xs = map fromContent xs
  and fromEmptyElemTag (Parse.Ast.EmptyElemTag (span, name, attributes)) =
        Element { name = name,
                  attributes = fromAttribute' attributes,
                  contents = [] }
  and fromChars (Parse.Ast.Chars (span, chars)) = chars
  and fromChars' xs = concat (map fromChars xs)
end
