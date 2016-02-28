structure Token = struct
  datatype token =
    EOF
  | S
  | LDOCTYPE
  | XMLAPOS
  | XMLQUOT
  | XMLCHARS of string
  | XMLCDSECT of string
  | XMLNAME of string
  | XMLCHARREF of int
  | XMLENTITYREF of string
  | XMLPEREF of string
  | LSTAG of string
  | RTAG
  | REMPTYTAG
  | LETAG of string
  | LXMLCOMMENT
  | RXMLCOMMENT
  | LPI of string
  | RPI
  | LBRACKET
  | RBRACKET
  | LELEMENTDECL
  | LATTLISTDECL
  | LENTITYDECL
  | LNOTATIONDECL
  | LPAREN
  | RPAREN
  | BAR
  | QUESTION
  | STAR
  | PLUS
  | COMMA
  | EQUAL
  | PERCENT
  | PUBLIC
  | SYSTEM
  | NDATA
  | CDATA_KW
  | ID
  | IDREF
  | IDREFS
  | ENTITY
  | ENTITIES
  | NMTOKEN
  | NMTOKENS
  | NOTATION
  | REQUIRED
  | IMPLIED
  | FIXED
  fun show (EOF) = "EOF"
    | show (S) = "S"
    | show (LDOCTYPE) = "LDOCTYPE"
    | show (XMLAPOS) = "XMLAPOS"
    | show (XMLQUOT) = "XMLQUOT"
    | show (XMLCHARS a) = "XMLCHARS(" ^ a ^ ")"
    | show (XMLCDSECT a) = "XMLCDSECT(" ^ a ^ ")"
    | show (XMLNAME a) = "XMLNAME(" ^ a ^ ")"
    | show (XMLCHARREF a) = "XMLCHARREF(" ^ Int.toString a ^ ")"
    | show (XMLENTITYREF a) = "XMLENTITYREF(" ^ a ^ ")"
    | show (XMLPEREF a) = "XMLPEREF(" ^ a ^ ")"
    | show (LSTAG a) = "LSTAG(" ^ a ^ ")"
    | show (RTAG) = "RTAG"
    | show (REMPTYTAG) = "REMPTYTAG"
    | show (LETAG a) = "LETAG(" ^ a ^ ")"
    | show (LXMLCOMMENT) = "LXMLCOMMENT"
    | show (RXMLCOMMENT) = "RXMLCOMMENT"
    | show (LPI a) = "LPI(" ^ a ^ ")"
    | show (RPI) = "RPI"
    | show (LBRACKET) = "LBRACKET"
    | show (RBRACKET) = "RBRACKET"
    | show (LELEMENTDECL) = "LELEMENTDECL"
    | show (LATTLISTDECL) = "LATTLISTDECL"
    | show (LENTITYDECL) = "LENTITYDECL"
    | show (LNOTATIONDECL) = "LNOTATIONDECL"
    | show (LPAREN) = "LPAREN"
    | show (RPAREN) = "RPAREN"
    | show (BAR) = "BAR"
    | show (QUESTION) = "QUESTION"
    | show (STAR) = "STAR"
    | show (PLUS) = "PLUS"
    | show (COMMA) = "COMMA"
    | show (EQUAL) = "EQUAL"
    | show (PERCENT) = "PERCENT"
    | show (PUBLIC) = "PUBLIC"
    | show (SYSTEM) = "SYSTEM"
    | show (NDATA) = "NDATA"
    | show (CDATA_KW) = "CDATA_KW"
    | show (ID) = "ID"
    | show (IDREF) = "IDREF"
    | show (IDREFS) = "IDREFS"
    | show (ENTITY) = "ENTITY"
    | show (ENTITIES) = "ENTITIES"
    | show (NMTOKEN) = "NMTOKEN"
    | show (NMTOKENS) = "NMTOKENS"
    | show (NOTATION) = "NOTATION"
    | show (REQUIRED) = "REQUIRED"
    | show (IMPLIED) = "IMPLIED"
    | show (FIXED) = "FIXED"
end
signature Lex = sig
  type strm
  eqtype pos
  type span = pos * pos
  eqtype tok
  val lex : AntlrStreamPos.sourcemap -> strm -> tok * span * strm
  val getPos : strm -> pos
end
functor ParseFun(Lex : Lex where type tok = Token.token and type pos = AntlrStreamPos.pos) = struct
  structure Ast = struct
    datatype document =
      Document of Lex.span * content list
    and entityvalue =
      CharDataEntityValue of Lex.span * string
    | PERefEntityValue of Lex.span * string
    | CharRefEntityValue of Lex.span * int
    | EntityRefEntityValue of Lex.span * string
    and attvalue =
      CharDataAttValue of Lex.span * string
    | CharRefAttValue of Lex.span * int
    | ReferenceAttValue of Lex.span * string
    and comment =
      EmptyComment of Lex.span
    | Comment of Lex.span * string
    and pi =
      EmptyPI of Lex.span * string
    | PI of Lex.span * string * chars list
    and doctypedecl =
      Doctypedecl1 of Lex.span * string
    | Doctypedecl2 of Lex.span * string * externalid
    | Doctypedecl3 of Lex.span * string * intsubset list
    | Doctypedecl4 of Lex.span * string * externalid * intsubset list
    and intsubset =
      PEReferenceIntSubset of Lex.span * string
    | ElementdeclIntSubset of Lex.span * elementdecl
    | AttlistDeclIntSubset of Lex.span * attlistdecl
    | EntityDeclIntSubset of Lex.span * entitydecl
    | NotationDeclIntSubset of Lex.span * notationdecl
    | PIIntSubset of Lex.span * pi
    and element =
      EmptyElement of Lex.span * emptyelemtag
    | Element of Lex.span * stag * content list * etag
    and stag =
      Stag of Lex.span * string * attribute list
    and attribute =
      Attribute of Lex.span * string * attvalue list
    and etag =
      ETag of Lex.span * string
    and content =
      CharDataContent of Lex.span * chars
    | ElementContent of Lex.span * element
    | CharRefContent of Lex.span * int
    | ReferenceContent of Lex.span * string
    | CDSectContent of Lex.span * string
    | PIContent of Lex.span * pi
    | CommentContent of Lex.span * comment
    | DoctypeContent of Lex.span * doctypedecl
    and emptyelemtag =
      EmptyElemTag of Lex.span * string * attribute list
    and elementdecl =
      Elementdecl of Lex.span * string * contentspec list
    and contentspec =
      XmlnameContentspec of Lex.span * string
    | LparenContentspec of Lex.span
    | RparenContentspec of Lex.span
    | BarContentspec of Lex.span
    | QuestionContentspec of Lex.span
    | StarContentspec of Lex.span
    | PlusContentspec of Lex.span
    | CommaContentspec of Lex.span
    and attlistdecl =
      AttlistDecl of Lex.span * string * attdef list
    and attdef =
      AttDef of Lex.span * string * atttype * defaultdecl
    and atttype =
      StringType of Lex.span
    | IdType of Lex.span
    | IdrefType of Lex.span
    | IdrefsType of Lex.span
    | EntityType of Lex.span
    | EntitiesType of Lex.span
    | NmtokenType of Lex.span
    | NmtokensType of Lex.span
    | NotationType of Lex.span * name list
    | Enumeration of Lex.span * name list
    and defaultdecl =
      RequiredDefaultDecl of Lex.span
    | ImpliedDefaultDecl of Lex.span
    | FixedDefaultDecl of Lex.span * attvalue list
    | DefaultDecl of Lex.span * attvalue list
    and entitydecl =
      GEDecl of Lex.span * string * entitydef
    | PEDecl of Lex.span * string * pedef
    and entitydef =
      EntityValueEntityDecl of Lex.span * entityvalue list
    | ExternalIDEntityDecl of Lex.span * externalid
    | NDataDeclEntityDecl of Lex.span * externalid * ndatadecl
    and pedef =
      EntityValuePEDef of Lex.span * entityvalue list
    | ExternalIDPEDef of Lex.span * externalid
    and externalid =
      SystemExternalID of Lex.span * string
    | PublicExternalID of Lex.span * string * string
    and ndatadecl =
      NDataDecl of Lex.span * string
    and notationdecl =
      ExternalIDNotationDecl of Lex.span * string * externalid
    | PublicIDNotationDecl of Lex.span * string * publicid
    and publicid =
      PublicID of Lex.span * string
    and chars =
      Chars of Lex.span * string
    and name =
      Name of Lex.span * string
    fun showDocument (Document (span, v0)) =
        "Document(" ^ showContent' v0 ^ ")"
    and showEntityValue (CharDataEntityValue (span, v0)) =
        "CharDataEntityValue(" ^ String.toString v0 ^ ")"
      | showEntityValue (PERefEntityValue (span, v0)) =
        "PERefEntityValue(" ^ String.toString v0 ^ ")"
      | showEntityValue (CharRefEntityValue (span, v0)) =
        "CharRefEntityValue(" ^ Int.toString v0 ^ ")"
      | showEntityValue (EntityRefEntityValue (span, v0)) =
        "EntityRefEntityValue(" ^ String.toString v0 ^ ")"
    and showEntityValue' xs =
        "[" ^ String.concatWith ", " (map showEntityValue xs) ^ "]"
    and showAttValue (CharDataAttValue (span, v0)) =
        "CharDataAttValue(" ^ String.toString v0 ^ ")"
      | showAttValue (CharRefAttValue (span, v0)) =
        "CharRefAttValue(" ^ Int.toString v0 ^ ")"
      | showAttValue (ReferenceAttValue (span, v0)) =
        "ReferenceAttValue(" ^ String.toString v0 ^ ")"
    and showAttValue' xs =
        "[" ^ String.concatWith ", " (map showAttValue xs) ^ "]"
    and showComment (EmptyComment (span)) = "EmptyComment(" ^ ")"
      | showComment (Comment (span, v0)) =
        "Comment(" ^ String.toString v0 ^ ")"
    and showPI (EmptyPI (span, v0)) = "EmptyPI(" ^ String.toString v0 ^ ")"
      | showPI (PI (span, v0, v1)) =
        "PI(" ^ String.toString v0 ^ ", " ^ showChars' v1 ^ ")"
    and showDoctypedecl (Doctypedecl1 (span, v0)) =
        "Doctypedecl1(" ^ String.toString v0 ^ ")"
      | showDoctypedecl (Doctypedecl2 (span, v0, v1)) =
        "Doctypedecl2(" ^ String.toString v0 ^ ", " ^ showExternalID v1 ^ ")"
      | showDoctypedecl (Doctypedecl3 (span, v0, v1)) =
        "Doctypedecl3(" ^ String.toString v0 ^ ", " ^ showIntSubset' v1 ^ ")"
      | showDoctypedecl (Doctypedecl4 (span, v0, v1, v2)) =
        "Doctypedecl4(" ^ String.toString v0 ^ ", " ^ showExternalID v1 ^ ", " ^ showIntSubset' v2 ^ ")"
    and showIntSubset (PEReferenceIntSubset (span, v0)) =
        "PEReferenceIntSubset(" ^ String.toString v0 ^ ")"
      | showIntSubset (ElementdeclIntSubset (span, v0)) =
        "ElementdeclIntSubset(" ^ showElementdecl v0 ^ ")"
      | showIntSubset (AttlistDeclIntSubset (span, v0)) =
        "AttlistDeclIntSubset(" ^ showAttlistDecl v0 ^ ")"
      | showIntSubset (EntityDeclIntSubset (span, v0)) =
        "EntityDeclIntSubset(" ^ showEntityDecl v0 ^ ")"
      | showIntSubset (NotationDeclIntSubset (span, v0)) =
        "NotationDeclIntSubset(" ^ showNotationDecl v0 ^ ")"
      | showIntSubset (PIIntSubset (span, v0)) =
        "PIIntSubset(" ^ showPI v0 ^ ")"
    and showIntSubset' xs =
        "[" ^ String.concatWith ", " (map showIntSubset xs) ^ "]"
    and showElement (EmptyElement (span, v0)) =
        "EmptyElement(" ^ showEmptyElemTag v0 ^ ")"
      | showElement (Element (span, v0, v1, v2)) =
        "Element(" ^ showStag v0 ^ ", " ^ showContent' v1 ^ ", " ^ showETag v2 ^ ")"
    and showStag (Stag (span, v0, v1)) =
        "Stag(" ^ String.toString v0 ^ ", " ^ showAttribute' v1 ^ ")"
    and showAttribute (Attribute (span, v0, v1)) =
        "Attribute(" ^ String.toString v0 ^ ", " ^ showAttValue' v1 ^ ")"
    and showAttribute' xs =
        "[" ^ String.concatWith ", " (map showAttribute xs) ^ "]"
    and showETag (ETag (span, v0)) = "ETag(" ^ String.toString v0 ^ ")"
    and showContent (CharDataContent (span, v0)) =
        "CharDataContent(" ^ showChars v0 ^ ")"
      | showContent (ElementContent (span, v0)) =
        "ElementContent(" ^ showElement v0 ^ ")"
      | showContent (CharRefContent (span, v0)) =
        "CharRefContent(" ^ Int.toString v0 ^ ")"
      | showContent (ReferenceContent (span, v0)) =
        "ReferenceContent(" ^ String.toString v0 ^ ")"
      | showContent (CDSectContent (span, v0)) =
        "CDSectContent(" ^ String.toString v0 ^ ")"
      | showContent (PIContent (span, v0)) = "PIContent(" ^ showPI v0 ^ ")"
      | showContent (CommentContent (span, v0)) =
        "CommentContent(" ^ showComment v0 ^ ")"
      | showContent (DoctypeContent (span, v0)) =
        "DoctypeContent(" ^ showDoctypedecl v0 ^ ")"
    and showContent' xs =
        "[" ^ String.concatWith ", " (map showContent xs) ^ "]"
    and showEmptyElemTag (EmptyElemTag (span, v0, v1)) =
        "EmptyElemTag(" ^ String.toString v0 ^ ", " ^ showAttribute' v1 ^ ")"
    and showElementdecl (Elementdecl (span, v0, v1)) =
        "Elementdecl(" ^ String.toString v0 ^ ", " ^ showContentspec' v1 ^ ")"
    and showContentspec (XmlnameContentspec (span, v0)) =
        "XmlnameContentspec(" ^ String.toString v0 ^ ")"
      | showContentspec (LparenContentspec (span)) =
        "LparenContentspec(" ^ ")"
      | showContentspec (RparenContentspec (span)) =
        "RparenContentspec(" ^ ")"
      | showContentspec (BarContentspec (span)) = "BarContentspec(" ^ ")"
      | showContentspec (QuestionContentspec (span)) =
        "QuestionContentspec(" ^ ")"
      | showContentspec (StarContentspec (span)) = "StarContentspec(" ^ ")"
      | showContentspec (PlusContentspec (span)) = "PlusContentspec(" ^ ")"
      | showContentspec (CommaContentspec (span)) = "CommaContentspec(" ^ ")"
    and showContentspec' xs =
        "[" ^ String.concatWith ", " (map showContentspec xs) ^ "]"
    and showAttlistDecl (AttlistDecl (span, v0, v1)) =
        "AttlistDecl(" ^ String.toString v0 ^ ", " ^ showAttDef' v1 ^ ")"
    and showAttDef (AttDef (span, v0, v1, v2)) =
        "AttDef(" ^ String.toString v0 ^ ", " ^ showAttType v1 ^ ", " ^ showDefaultDecl v2 ^ ")"
    and showAttDef' xs =
        "[" ^ String.concatWith ", " (map showAttDef xs) ^ "]"
    and showAttType (StringType (span)) = "StringType(" ^ ")"
      | showAttType (IdType (span)) = "IdType(" ^ ")"
      | showAttType (IdrefType (span)) = "IdrefType(" ^ ")"
      | showAttType (IdrefsType (span)) = "IdrefsType(" ^ ")"
      | showAttType (EntityType (span)) = "EntityType(" ^ ")"
      | showAttType (EntitiesType (span)) = "EntitiesType(" ^ ")"
      | showAttType (NmtokenType (span)) = "NmtokenType(" ^ ")"
      | showAttType (NmtokensType (span)) = "NmtokensType(" ^ ")"
      | showAttType (NotationType (span, v0)) =
        "NotationType(" ^ showName' v0 ^ ")"
      | showAttType (Enumeration (span, v0)) =
        "Enumeration(" ^ showName' v0 ^ ")"
    and showDefaultDecl (RequiredDefaultDecl (span)) =
        "RequiredDefaultDecl(" ^ ")"
      | showDefaultDecl (ImpliedDefaultDecl (span)) =
        "ImpliedDefaultDecl(" ^ ")"
      | showDefaultDecl (FixedDefaultDecl (span, v0)) =
        "FixedDefaultDecl(" ^ showAttValue' v0 ^ ")"
      | showDefaultDecl (DefaultDecl (span, v0)) =
        "DefaultDecl(" ^ showAttValue' v0 ^ ")"
    and showEntityDecl (GEDecl (span, v0, v1)) =
        "GEDecl(" ^ String.toString v0 ^ ", " ^ showEntityDef v1 ^ ")"
      | showEntityDecl (PEDecl (span, v0, v1)) =
        "PEDecl(" ^ String.toString v0 ^ ", " ^ showPEDef v1 ^ ")"
    and showEntityDef (EntityValueEntityDecl (span, v0)) =
        "EntityValueEntityDecl(" ^ showEntityValue' v0 ^ ")"
      | showEntityDef (ExternalIDEntityDecl (span, v0)) =
        "ExternalIDEntityDecl(" ^ showExternalID v0 ^ ")"
      | showEntityDef (NDataDeclEntityDecl (span, v0, v1)) =
        "NDataDeclEntityDecl(" ^ showExternalID v0 ^ ", " ^ showNDataDecl v1 ^ ")"
    and showPEDef (EntityValuePEDef (span, v0)) =
        "EntityValuePEDef(" ^ showEntityValue' v0 ^ ")"
      | showPEDef (ExternalIDPEDef (span, v0)) =
        "ExternalIDPEDef(" ^ showExternalID v0 ^ ")"
    and showExternalID (SystemExternalID (span, v0)) =
        "SystemExternalID(" ^ String.toString v0 ^ ")"
      | showExternalID (PublicExternalID (span, v0, v1)) =
        "PublicExternalID(" ^ String.toString v0 ^ ", " ^ String.toString v1 ^ ")"
    and showNDataDecl (NDataDecl (span, v0)) =
        "NDataDecl(" ^ String.toString v0 ^ ")"
    and showNotationDecl (ExternalIDNotationDecl (span, v0, v1)) =
        "ExternalIDNotationDecl(" ^ String.toString v0 ^ ", " ^ showExternalID v1 ^ ")"
      | showNotationDecl (PublicIDNotationDecl (span, v0, v1)) =
        "PublicIDNotationDecl(" ^ String.toString v0 ^ ", " ^ showPublicID v1 ^ ")"
    and showPublicID (PublicID (span, v0)) =
        "PublicID(" ^ String.toString v0 ^ ")"
    and showChars (Chars (span, v0)) = "Chars(" ^ String.toString v0 ^ ")"
    and showChars' xs = "[" ^ String.concatWith ", " (map showChars xs) ^ "]"
    and showName (Name (span, v0)) = "Name(" ^ String.toString v0 ^ ")"
    and showName' xs = "[" ^ String.concatWith ", " (map showName xs) ^ "]"
  end
  structure Category = struct
    datatype category =
      EOF
    | S
    | LDOCTYPE
    | XMLAPOS
    | XMLQUOT
    | XMLCHARS of string
    | XMLCDSECT of string
    | XMLNAME of string
    | XMLCHARREF of int
    | XMLENTITYREF of string
    | XMLPEREF of string
    | LSTAG of string
    | RTAG
    | REMPTYTAG
    | LETAG of string
    | LXMLCOMMENT
    | RXMLCOMMENT
    | LPI of string
    | RPI
    | LBRACKET
    | RBRACKET
    | LELEMENTDECL
    | LATTLISTDECL
    | LENTITYDECL
    | LNOTATIONDECL
    | LPAREN
    | RPAREN
    | BAR
    | QUESTION
    | STAR
    | PLUS
    | COMMA
    | EQUAL
    | PERCENT
    | PUBLIC
    | SYSTEM
    | NDATA
    | CDATA_KW
    | ID
    | IDREF
    | IDREFS
    | ENTITY
    | ENTITIES
    | NMTOKEN
    | NMTOKENS
    | NOTATION
    | REQUIRED
    | IMPLIED
    | FIXED
    | Document of Ast.document
    | EntityValue of Ast.entityvalue
    | EntityValue' of Ast.entityvalue list
    | AttValue of Ast.attvalue
    | AttValue' of Ast.attvalue list
    | Comment of Ast.comment
    | PI of Ast.pi
    | Doctypedecl of Ast.doctypedecl
    | IntSubset of Ast.intsubset
    | IntSubset' of Ast.intsubset list
    | Element of Ast.element
    | Stag of Ast.stag
    | Attribute of Ast.attribute
    | Attribute' of Ast.attribute list
    | ETag of Ast.etag
    | Content of Ast.content
    | Content' of Ast.content list
    | EmptyElemTag of Ast.emptyelemtag
    | Elementdecl of Ast.elementdecl
    | Contentspec of Ast.contentspec
    | Contentspec' of Ast.contentspec list
    | AttlistDecl of Ast.attlistdecl
    | AttDef of Ast.attdef
    | AttDef' of Ast.attdef list
    | AttType of Ast.atttype
    | DefaultDecl of Ast.defaultdecl
    | EntityDecl of Ast.entitydecl
    | EntityDef of Ast.entitydef
    | PEDef of Ast.pedef
    | ExternalID of Ast.externalid
    | NDataDecl of Ast.ndatadecl
    | NotationDecl of Ast.notationdecl
    | PublicID of Ast.publicid
    | Chars of Ast.chars
    | Chars' of Ast.chars list
    | Name of Ast.name
    | Name' of Ast.name list
    fun show (EOF) = "EOF"
      | show (S) = "S"
      | show (LDOCTYPE) = "LDOCTYPE"
      | show (XMLAPOS) = "XMLAPOS"
      | show (XMLQUOT) = "XMLQUOT"
      | show (XMLCHARS a) = "XMLCHARS(" ^ a ^ ")"
      | show (XMLCDSECT a) = "XMLCDSECT(" ^ a ^ ")"
      | show (XMLNAME a) = "XMLNAME(" ^ a ^ ")"
      | show (XMLCHARREF a) = "XMLCHARREF(" ^ Int.toString a ^ ")"
      | show (XMLENTITYREF a) = "XMLENTITYREF(" ^ a ^ ")"
      | show (XMLPEREF a) = "XMLPEREF(" ^ a ^ ")"
      | show (LSTAG a) = "LSTAG(" ^ a ^ ")"
      | show (RTAG) = "RTAG"
      | show (REMPTYTAG) = "REMPTYTAG"
      | show (LETAG a) = "LETAG(" ^ a ^ ")"
      | show (LXMLCOMMENT) = "LXMLCOMMENT"
      | show (RXMLCOMMENT) = "RXMLCOMMENT"
      | show (LPI a) = "LPI(" ^ a ^ ")"
      | show (RPI) = "RPI"
      | show (LBRACKET) = "LBRACKET"
      | show (RBRACKET) = "RBRACKET"
      | show (LELEMENTDECL) = "LELEMENTDECL"
      | show (LATTLISTDECL) = "LATTLISTDECL"
      | show (LENTITYDECL) = "LENTITYDECL"
      | show (LNOTATIONDECL) = "LNOTATIONDECL"
      | show (LPAREN) = "LPAREN"
      | show (RPAREN) = "RPAREN"
      | show (BAR) = "BAR"
      | show (QUESTION) = "QUESTION"
      | show (STAR) = "STAR"
      | show (PLUS) = "PLUS"
      | show (COMMA) = "COMMA"
      | show (EQUAL) = "EQUAL"
      | show (PERCENT) = "PERCENT"
      | show (PUBLIC) = "PUBLIC"
      | show (SYSTEM) = "SYSTEM"
      | show (NDATA) = "NDATA"
      | show (CDATA_KW) = "CDATA_KW"
      | show (ID) = "ID"
      | show (IDREF) = "IDREF"
      | show (IDREFS) = "IDREFS"
      | show (ENTITY) = "ENTITY"
      | show (ENTITIES) = "ENTITIES"
      | show (NMTOKEN) = "NMTOKEN"
      | show (NMTOKENS) = "NMTOKENS"
      | show (NOTATION) = "NOTATION"
      | show (REQUIRED) = "REQUIRED"
      | show (IMPLIED) = "IMPLIED"
      | show (FIXED) = "FIXED"
      | show (Document a) = "Document(" ^ Ast.showDocument a ^ ")"
      | show (EntityValue a) = "EntityValue(" ^ Ast.showEntityValue a ^ ")"
      | show (EntityValue' a) =
        "EntityValue'(" ^ Ast.showEntityValue' a ^ ")"
      | show (AttValue a) = "AttValue(" ^ Ast.showAttValue a ^ ")"
      | show (AttValue' a) = "AttValue'(" ^ Ast.showAttValue' a ^ ")"
      | show (Comment a) = "Comment(" ^ Ast.showComment a ^ ")"
      | show (PI a) = "PI(" ^ Ast.showPI a ^ ")"
      | show (Doctypedecl a) = "Doctypedecl(" ^ Ast.showDoctypedecl a ^ ")"
      | show (IntSubset a) = "IntSubset(" ^ Ast.showIntSubset a ^ ")"
      | show (IntSubset' a) = "IntSubset'(" ^ Ast.showIntSubset' a ^ ")"
      | show (Element a) = "Element(" ^ Ast.showElement a ^ ")"
      | show (Stag a) = "Stag(" ^ Ast.showStag a ^ ")"
      | show (Attribute a) = "Attribute(" ^ Ast.showAttribute a ^ ")"
      | show (Attribute' a) = "Attribute'(" ^ Ast.showAttribute' a ^ ")"
      | show (ETag a) = "ETag(" ^ Ast.showETag a ^ ")"
      | show (Content a) = "Content(" ^ Ast.showContent a ^ ")"
      | show (Content' a) = "Content'(" ^ Ast.showContent' a ^ ")"
      | show (EmptyElemTag a) =
        "EmptyElemTag(" ^ Ast.showEmptyElemTag a ^ ")"
      | show (Elementdecl a) = "Elementdecl(" ^ Ast.showElementdecl a ^ ")"
      | show (Contentspec a) = "Contentspec(" ^ Ast.showContentspec a ^ ")"
      | show (Contentspec' a) =
        "Contentspec'(" ^ Ast.showContentspec' a ^ ")"
      | show (AttlistDecl a) = "AttlistDecl(" ^ Ast.showAttlistDecl a ^ ")"
      | show (AttDef a) = "AttDef(" ^ Ast.showAttDef a ^ ")"
      | show (AttDef' a) = "AttDef'(" ^ Ast.showAttDef' a ^ ")"
      | show (AttType a) = "AttType(" ^ Ast.showAttType a ^ ")"
      | show (DefaultDecl a) = "DefaultDecl(" ^ Ast.showDefaultDecl a ^ ")"
      | show (EntityDecl a) = "EntityDecl(" ^ Ast.showEntityDecl a ^ ")"
      | show (EntityDef a) = "EntityDef(" ^ Ast.showEntityDef a ^ ")"
      | show (PEDef a) = "PEDef(" ^ Ast.showPEDef a ^ ")"
      | show (ExternalID a) = "ExternalID(" ^ Ast.showExternalID a ^ ")"
      | show (NDataDecl a) = "NDataDecl(" ^ Ast.showNDataDecl a ^ ")"
      | show (NotationDecl a) =
        "NotationDecl(" ^ Ast.showNotationDecl a ^ ")"
      | show (PublicID a) = "PublicID(" ^ Ast.showPublicID a ^ ")"
      | show (Chars a) = "Chars(" ^ Ast.showChars a ^ ")"
      | show (Chars' a) = "Chars'(" ^ Ast.showChars' a ^ ")"
      | show (Name a) = "Name(" ^ Ast.showName a ^ ")"
      | show (Name' a) = "Name'(" ^ Ast.showName' a ^ ")"
    fun fromToken (Token.EOF) = EOF
      | fromToken (Token.S) = S
      | fromToken (Token.LDOCTYPE) = LDOCTYPE
      | fromToken (Token.XMLAPOS) = XMLAPOS
      | fromToken (Token.XMLQUOT) = XMLQUOT
      | fromToken (Token.XMLCHARS a) = XMLCHARS a
      | fromToken (Token.XMLCDSECT a) = XMLCDSECT a
      | fromToken (Token.XMLNAME a) = XMLNAME a
      | fromToken (Token.XMLCHARREF a) = XMLCHARREF a
      | fromToken (Token.XMLENTITYREF a) = XMLENTITYREF a
      | fromToken (Token.XMLPEREF a) = XMLPEREF a
      | fromToken (Token.LSTAG a) = LSTAG a
      | fromToken (Token.RTAG) = RTAG
      | fromToken (Token.REMPTYTAG) = REMPTYTAG
      | fromToken (Token.LETAG a) = LETAG a
      | fromToken (Token.LXMLCOMMENT) = LXMLCOMMENT
      | fromToken (Token.RXMLCOMMENT) = RXMLCOMMENT
      | fromToken (Token.LPI a) = LPI a
      | fromToken (Token.RPI) = RPI
      | fromToken (Token.LBRACKET) = LBRACKET
      | fromToken (Token.RBRACKET) = RBRACKET
      | fromToken (Token.LELEMENTDECL) = LELEMENTDECL
      | fromToken (Token.LATTLISTDECL) = LATTLISTDECL
      | fromToken (Token.LENTITYDECL) = LENTITYDECL
      | fromToken (Token.LNOTATIONDECL) = LNOTATIONDECL
      | fromToken (Token.LPAREN) = LPAREN
      | fromToken (Token.RPAREN) = RPAREN
      | fromToken (Token.BAR) = BAR
      | fromToken (Token.QUESTION) = QUESTION
      | fromToken (Token.STAR) = STAR
      | fromToken (Token.PLUS) = PLUS
      | fromToken (Token.COMMA) = COMMA
      | fromToken (Token.EQUAL) = EQUAL
      | fromToken (Token.PERCENT) = PERCENT
      | fromToken (Token.PUBLIC) = PUBLIC
      | fromToken (Token.SYSTEM) = SYSTEM
      | fromToken (Token.NDATA) = NDATA
      | fromToken (Token.CDATA_KW) = CDATA_KW
      | fromToken (Token.ID) = ID
      | fromToken (Token.IDREF) = IDREF
      | fromToken (Token.IDREFS) = IDREFS
      | fromToken (Token.ENTITY) = ENTITY
      | fromToken (Token.ENTITIES) = ENTITIES
      | fromToken (Token.NMTOKEN) = NMTOKEN
      | fromToken (Token.NMTOKENS) = NMTOKENS
      | fromToken (Token.NOTATION) = NOTATION
      | fromToken (Token.REQUIRED) = REQUIRED
      | fromToken (Token.IMPLIED) = IMPLIED
      | fromToken (Token.FIXED) = FIXED
  end
  open Category
  exception Parse of category * Lex.pos * int
  fun go stateNumber stack category span =
      case stateNumber of
        144 => st144 stack category span
      | 143 => st143 stack category span
      | 141 => st141 stack category span
      | 140 => st140 stack category span
      | 137 => st137 stack category span
      | 136 => st136 stack category span
      | 135 => st135 stack category span
      | 134 => st134 stack category span
      | 132 => st132 stack category span
      | 129 => st129 stack category span
      | 127 => st127 stack category span
      | 119 => st119 stack category span
      | 117 => st117 stack category span
      | 116 => st116 stack category span
      | 115 => st115 stack category span
      | 114 => st114 stack category span
      | 112 => st112 stack category span
      | 110 => st110 stack category span
      | 108 => st108 stack category span
      | 106 => st106 stack category span
      | 105 => st105 stack category span
      | 102 => st102 stack category span
      | 98 => st98 stack category span
      | 97 => st97 stack category span
      | 88 => st88 stack category span
      | 85 => st85 stack category span
      | 84 => st84 stack category span
      | 83 => st83 stack category span
      | 82 => st82 stack category span
      | 71 => st71 stack category span
      | 70 => st70 stack category span
      | 69 => st69 stack category span
      | 66 => st66 stack category span
      | 64 => st64 stack category span
      | 62 => st62 stack category span
      | 60 => st60 stack category span
      | 58 => st58 stack category span
      | 55 => st55 stack category span
      | 54 => st54 stack category span
      | 52 => st52 stack category span
      | 50 => st50 stack category span
      | 49 => st49 stack category span
      | 48 => st48 stack category span
      | 47 => st47 stack category span
      | 45 => st45 stack category span
      | 42 => st42 stack category span
      | 38 => st38 stack category span
      | 37 => st37 stack category span
      | 30 => st30 stack category span
      | 28 => st28 stack category span
      | 26 => st26 stack category span
      | 25 => st25 stack category span
      | 24 => st24 stack category span
      | 22 => st22 stack category span
      | 20 => st20 stack category span
      | 18 => st18 stack category span
      | 16 => st16 stack category span
      | 14 => st14 stack category span
      | 9 => st9 stack category span
      | 8 => st8 stack category span
      | 3 => st3 stack category span
      | 0 => st0 stack category span
      | _ => []
  and st145r0 ((RTAG, pos6, stNum6)::(RBRACKET, pos5, stNum5)::(IntSubset' sv4, pos4, stNum4)::(LBRACKET, pos3, stNum3)::(ExternalID sv2, pos2, stNum2)::(XMLNAME sv1, pos1, stNum1)::(LDOCTYPE, pos0, stNum0)::stack) pos =
      go stNum0 stack (Doctypedecl (Ast.Doctypedecl4 ((pos0, pos), sv1, sv2, sv4))) (pos0, pos)
    | st145r0 stack pos = []
  and st144 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 144)
      in
        case category of
          RTAG => [] @ List.concat [st145r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 144) *)
      end
  and st143 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 143)
      in
        case category of
          RBRACKET => [(144, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 143) *)
      end
  and st142r0 ((RTAG, pos3, stNum3)::(ExternalID sv2, pos2, stNum2)::(XMLNAME sv1, pos1, stNum1)::(LDOCTYPE, pos0, stNum0)::stack) pos =
      go stNum0 stack (Doctypedecl (Ast.Doctypedecl2 ((pos0, pos), sv1, sv2))) (pos0, pos)
    | st142r0 stack pos = []
  and st141 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 141)
      in
        case category of
          IntSubset' _ => [(143, (stackItem::stack))]
        | IntSubset _ => [(55, (stackItem::stack))] @ List.concat [st55r0 (stackItem::stack) toPos]
        | XMLPEREF _ => [] @ List.concat [st56r0 (stackItem::stack) toPos]
        | Elementdecl _ => [] @ List.concat [st57r0 (stackItem::stack) toPos]
        | LELEMENTDECL => [(58, (stackItem::stack))]
        | AttlistDecl _ => [] @ List.concat [st59r0 (stackItem::stack) toPos]
        | LATTLISTDECL => [(60, (stackItem::stack))]
        | EntityDecl _ => [] @ List.concat [st61r0 (stackItem::stack) toPos]
        | LENTITYDECL => [(62, (stackItem::stack))]
        | NotationDecl _ => [] @ List.concat [st63r0 (stackItem::stack) toPos]
        | LNOTATIONDECL => [(64, (stackItem::stack))]
        | PI _ => [] @ List.concat [st65r0 (stackItem::stack) toPos]
        | LPI _ => [(14, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 141) *)
      end
  and st141r0 stack pos = go 141 stack (IntSubset' []) (pos, pos)
  and st140 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 140)
      in
        case category of
          XMLCHARS _ => [] @ List.concat [st53r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 140) *)
      end
  and st140r0 ((XMLCHARS sv1, pos1, stNum1)::(PUBLIC, pos0, stNum0)::stack) pos =
      go stNum0 stack (PublicID (Ast.PublicID ((pos0, pos), sv1))) (pos0, pos)
    | st140r0 stack pos = []
  and st139r0 ((RTAG, pos3, stNum3)::(PublicID sv2, pos2, stNum2)::(XMLNAME sv1, pos1, stNum1)::(LNOTATIONDECL, pos0, stNum0)::stack) pos =
      go stNum0 stack (NotationDecl (Ast.PublicIDNotationDecl ((pos0, pos), sv1, sv2))) (pos0, pos)
    | st139r0 stack pos = []
  and st138r0 ((RTAG, pos3, stNum3)::(ExternalID sv2, pos2, stNum2)::(XMLNAME sv1, pos1, stNum1)::(LNOTATIONDECL, pos0, stNum0)::stack) pos =
      go stNum0 stack (NotationDecl (Ast.ExternalIDNotationDecl ((pos0, pos), sv1, sv2))) (pos0, pos)
    | st138r0 stack pos = []
  and st137 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 137)
      in
        case category of
          XMLCHARS _ => [(140, (stackItem::stack))] @ List.concat [st140r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 137) *)
      end
  and st136 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 136)
      in
        case category of
          RTAG => [] @ List.concat [st139r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 136) *)
      end
  and st135 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 135)
      in
        case category of
          RTAG => [] @ List.concat [st138r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 135) *)
      end
  and st134 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 134)
      in
        case category of
          ExternalID _ => [(135, (stackItem::stack))]
        | SYSTEM => [(47, (stackItem::stack))]
        | PublicID _ => [(136, (stackItem::stack))]
        | PUBLIC => [(137, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 134) *)
      end
  and st133r0 ((XMLNAME sv1, pos1, stNum1)::(NDATA, pos0, stNum0)::stack) pos =
      go stNum0 stack (NDataDecl (Ast.NDataDecl ((pos0, pos), sv1))) (pos0, pos)
    | st133r0 stack pos = []
  and st132 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 132)
      in
        case category of
          XMLNAME _ => [] @ List.concat [st133r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 132) *)
      end
  and st131r0 ((NDataDecl sv1, pos1, stNum1)::(ExternalID sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (EntityDef (Ast.NDataDeclEntityDecl ((pos0, pos), sv0, sv1))) (pos0, pos)
    | st131r0 stack pos = []
  and st130r0 ((RTAG, pos3, stNum3)::(EntityDef sv2, pos2, stNum2)::(XMLNAME sv1, pos1, stNum1)::(LENTITYDECL, pos0, stNum0)::stack) pos =
      go stNum0 stack (EntityDecl (Ast.GEDecl ((pos0, pos), sv1, sv2))) (pos0, pos)
    | st130r0 stack pos = []
  and st129 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 129)
      in
        case category of
          NDataDecl _ => [] @ List.concat [st131r0 (stackItem::stack) toPos]
        | NDATA => [(132, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 129) *)
      end
  and st129r0 ((ExternalID sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (EntityDef (Ast.ExternalIDEntityDecl ((pos0, pos), sv0))) (pos0, pos)
    | st129r0 stack pos = []
  and st128r0 ((EntityValue' sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (EntityDef (Ast.EntityValueEntityDecl ((pos0, pos), sv0))) (pos0, pos)
    | st128r0 stack pos = []
  and st127 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 127)
      in
        case category of
          RTAG => [] @ List.concat [st130r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 127) *)
      end
  and st126r0 ((EntityValue' sv1, pos1, stNum1)::(EntityValue sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (EntityValue' (sv0::sv1)) (pos0, pos)
    | st126r0 stack pos = []
  and st125r0 ((RTAG, pos4, stNum4)::(PEDef sv3, pos3, stNum3)::(XMLNAME sv2, pos2, stNum2)::(PERCENT, pos1, stNum1)::(LENTITYDECL, pos0, stNum0)::stack) pos =
      go stNum0 stack (EntityDecl (Ast.PEDecl ((pos0, pos), sv2, sv3))) (pos0, pos)
    | st125r0 stack pos = []
  and st124r0 ((ExternalID sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (PEDef (Ast.ExternalIDPEDef ((pos0, pos), sv0))) (pos0, pos)
    | st124r0 stack pos = []
  and st123r0 ((XMLENTITYREF sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (EntityValue (Ast.EntityRefEntityValue ((pos0, pos), sv0))) (pos0, pos)
    | st123r0 stack pos = []
  and st122r0 ((XMLCHARREF sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (EntityValue (Ast.CharRefEntityValue ((pos0, pos), sv0))) (pos0, pos)
    | st122r0 stack pos = []
  and st121r0 ((XMLPEREF sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (EntityValue (Ast.PERefEntityValue ((pos0, pos), sv0))) (pos0, pos)
    | st121r0 stack pos = []
  and st120r0 ((XMLCHARS sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (EntityValue (Ast.CharDataEntityValue ((pos0, pos), sv0))) (pos0, pos)
    | st120r0 stack pos = []
  and st119 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 119)
      in
        case category of
          EntityValue' _ => [] @ List.concat [st126r0 (stackItem::stack) toPos]
        | EntityValue _ => [(119, (stackItem::stack))] @ List.concat [st119r0 (stackItem::stack) toPos]
        | XMLCHARS _ => [] @ List.concat [st120r0 (stackItem::stack) toPos]
        | XMLPEREF _ => [] @ List.concat [st121r0 (stackItem::stack) toPos]
        | XMLCHARREF _ => [] @ List.concat [st122r0 (stackItem::stack) toPos]
        | XMLENTITYREF _ => [] @ List.concat [st123r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 119) *)
      end
  and st119r0 stack pos = go 119 stack (EntityValue' []) (pos, pos)
  and st118r0 ((EntityValue' sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (PEDef (Ast.EntityValuePEDef ((pos0, pos), sv0))) (pos0, pos)
    | st118r0 stack pos = []
  and st117 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 117)
      in
        case category of
          RTAG => [] @ List.concat [st125r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 117) *)
      end
  and st116 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 116)
      in
        case category of
          PEDef _ => [(117, (stackItem::stack))]
        | EntityValue' _ => [] @ List.concat [st118r0 (stackItem::stack) toPos]
        | EntityValue _ => [(119, (stackItem::stack))] @ List.concat [st119r0 (stackItem::stack) toPos]
        | XMLCHARS _ => [] @ List.concat [st120r0 (stackItem::stack) toPos]
        | XMLPEREF _ => [] @ List.concat [st121r0 (stackItem::stack) toPos]
        | XMLCHARREF _ => [] @ List.concat [st122r0 (stackItem::stack) toPos]
        | XMLENTITYREF _ => [] @ List.concat [st123r0 (stackItem::stack) toPos]
        | ExternalID _ => [] @ List.concat [st124r0 (stackItem::stack) toPos]
        | SYSTEM => [(47, (stackItem::stack))]
        | PUBLIC => [(48, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 116) *)
      end
  and st116r0 stack pos = go 116 stack (EntityValue' []) (pos, pos)
  and st115 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 115)
      in
        case category of
          EntityDef _ => [(127, (stackItem::stack))]
        | EntityValue' _ => [] @ List.concat [st128r0 (stackItem::stack) toPos]
        | EntityValue _ => [(119, (stackItem::stack))] @ List.concat [st119r0 (stackItem::stack) toPos]
        | XMLCHARS _ => [] @ List.concat [st120r0 (stackItem::stack) toPos]
        | XMLPEREF _ => [] @ List.concat [st121r0 (stackItem::stack) toPos]
        | XMLCHARREF _ => [] @ List.concat [st122r0 (stackItem::stack) toPos]
        | XMLENTITYREF _ => [] @ List.concat [st123r0 (stackItem::stack) toPos]
        | SYSTEM => [(47, (stackItem::stack))]
        | PUBLIC => [(48, (stackItem::stack))]
        | ExternalID _ => [(129, (stackItem::stack))] @ List.concat [st129r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 115) *)
      end
  and st115r0 stack pos = go 115 stack (EntityValue' []) (pos, pos)
  and st114 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 114)
      in
        case category of
          XMLNAME _ => [(116, (stackItem::stack))] @ List.concat [st116r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 114) *)
      end
  and st113r0 ((RPAREN, pos2, stNum2)::(Name' sv1, pos1, stNum1)::(LPAREN, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttType (Ast.Enumeration ((pos0, pos), sv1))) (pos0, pos)
    | st113r0 stack pos = []
  and st112 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 112)
      in
        case category of
          RPAREN => [] @ List.concat [st113r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 112) *)
      end
  and st111r0 ((Name' sv2, pos2, stNum2)::(BAR, pos1, stNum1)::(Name sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Name' (sv0::sv2)) (pos0, pos)
    | st111r0 stack pos = []
  and st110 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 110)
      in
        case category of
          Name' _ => [] @ List.concat [st111r0 (stackItem::stack) toPos]
        | XMLNAME _ => [] @ List.concat [st107r0 (stackItem::stack) toPos]
        | Name _ => [(108, (stackItem::stack))] @ List.concat [st108r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 110) *)
      end
  and st109r0 ((RPAREN, pos3, stNum3)::(Name' sv2, pos2, stNum2)::(LPAREN, pos1, stNum1)::(NOTATION, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttType (Ast.NotationType ((pos0, pos), sv2))) (pos0, pos)
    | st109r0 stack pos = []
  and st108 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 108)
      in
        case category of
          BAR => [(110, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 108) *)
      end
  and st108r0 ((Name sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Name' [sv0]) (pos0, pos)
    | st108r0 stack pos = []
  and st107r0 ((XMLNAME sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Name (Ast.Name ((pos0, pos), sv0))) (pos0, pos)
    | st107r0 stack pos = []
  and st106 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 106)
      in
        case category of
          RPAREN => [] @ List.concat [st109r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 106) *)
      end
  and st105 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 105)
      in
        case category of
          Name' _ => [(106, (stackItem::stack))]
        | XMLNAME _ => [] @ List.concat [st107r0 (stackItem::stack) toPos]
        | Name _ => [(108, (stackItem::stack))] @ List.concat [st108r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 105) *)
      end
  and st104r0 ((AttValue' sv1, pos1, stNum1)::(FIXED, pos0, stNum0)::stack) pos =
      go stNum0 stack (DefaultDecl (Ast.FixedDefaultDecl ((pos0, pos), sv1))) (pos0, pos)
    | st104r0 stack pos = []
  and st103r0 ((AttValue' sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (DefaultDecl (Ast.DefaultDecl ((pos0, pos), sv0))) (pos0, pos)
    | st103r0 stack pos = []
  and st102 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 102)
      in
        case category of
          AttValue' _ => [] @ List.concat [st104r0 (stackItem::stack) toPos]
        | AttValue _ => [(30, (stackItem::stack))] @ List.concat [st30r0 (stackItem::stack) toPos]
        | XMLCHARS _ => [] @ List.concat [st31r0 (stackItem::stack) toPos]
        | XMLCHARREF _ => [] @ List.concat [st32r0 (stackItem::stack) toPos]
        | XMLENTITYREF _ => [] @ List.concat [st33r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 102) *)
      end
  and st102r0 stack pos = go 102 stack (AttValue' []) (pos, pos)
  and st101r0 ((IMPLIED, pos0, stNum0)::stack) pos =
      go stNum0 stack (DefaultDecl (Ast.ImpliedDefaultDecl ((pos0, pos)))) (pos0, pos)
    | st101r0 stack pos = []
  and st100r0 ((REQUIRED, pos0, stNum0)::stack) pos =
      go stNum0 stack (DefaultDecl (Ast.RequiredDefaultDecl ((pos0, pos)))) (pos0, pos)
    | st100r0 stack pos = []
  and st99r0 ((DefaultDecl sv2, pos2, stNum2)::(AttType sv1, pos1, stNum1)::(XMLNAME sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttDef (Ast.AttDef ((pos0, pos), sv0, sv1, sv2))) (pos0, pos)
    | st99r0 stack pos = []
  and st98 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 98)
      in
        case category of
          Name' _ => [(112, (stackItem::stack))]
        | XMLNAME _ => [] @ List.concat [st107r0 (stackItem::stack) toPos]
        | Name _ => [(108, (stackItem::stack))] @ List.concat [st108r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 98) *)
      end
  and st97 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 97)
      in
        case category of
          LPAREN => [(105, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 97) *)
      end
  and st96r0 ((NMTOKENS, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttType (Ast.NmtokensType ((pos0, pos)))) (pos0, pos)
    | st96r0 stack pos = []
  and st95r0 ((NMTOKEN, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttType (Ast.NmtokenType ((pos0, pos)))) (pos0, pos)
    | st95r0 stack pos = []
  and st94r0 ((ENTITIES, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttType (Ast.EntitiesType ((pos0, pos)))) (pos0, pos)
    | st94r0 stack pos = []
  and st93r0 ((ENTITY, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttType (Ast.EntityType ((pos0, pos)))) (pos0, pos)
    | st93r0 stack pos = []
  and st92r0 ((IDREFS, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttType (Ast.IdrefsType ((pos0, pos)))) (pos0, pos)
    | st92r0 stack pos = []
  and st91r0 ((IDREF, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttType (Ast.IdrefType ((pos0, pos)))) (pos0, pos)
    | st91r0 stack pos = []
  and st90r0 ((ID, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttType (Ast.IdType ((pos0, pos)))) (pos0, pos)
    | st90r0 stack pos = []
  and st89r0 ((CDATA_KW, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttType (Ast.StringType ((pos0, pos)))) (pos0, pos)
    | st89r0 stack pos = []
  and st88 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 88)
      in
        case category of
          DefaultDecl _ => [] @ List.concat [st99r0 (stackItem::stack) toPos]
        | REQUIRED => [] @ List.concat [st100r0 (stackItem::stack) toPos]
        | IMPLIED => [] @ List.concat [st101r0 (stackItem::stack) toPos]
        | FIXED => [(102, (stackItem::stack))] @ List.concat [st102r0 (stackItem::stack) toPos]
        | AttValue' _ => [] @ List.concat [st103r0 (stackItem::stack) toPos]
        | AttValue _ => [(30, (stackItem::stack))] @ List.concat [st30r0 (stackItem::stack) toPos]
        | XMLCHARS _ => [] @ List.concat [st31r0 (stackItem::stack) toPos]
        | XMLCHARREF _ => [] @ List.concat [st32r0 (stackItem::stack) toPos]
        | XMLENTITYREF _ => [] @ List.concat [st33r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 88) *)
      end
  and st88r0 stack pos = go 88 stack (AttValue' []) (pos, pos)
  and st87r0 ((AttDef' sv1, pos1, stNum1)::(AttDef sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttDef' (sv0::sv1)) (pos0, pos)
    | st87r0 stack pos = []
  and st86r0 ((RTAG, pos3, stNum3)::(AttDef' sv2, pos2, stNum2)::(XMLNAME sv1, pos1, stNum1)::(LATTLISTDECL, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttlistDecl (Ast.AttlistDecl ((pos0, pos), sv1, sv2))) (pos0, pos)
    | st86r0 stack pos = []
  and st85 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 85)
      in
        case category of
          AttType _ => [(88, (stackItem::stack))] @ List.concat [st88r0 (stackItem::stack) toPos]
        | CDATA_KW => [] @ List.concat [st89r0 (stackItem::stack) toPos]
        | ID => [] @ List.concat [st90r0 (stackItem::stack) toPos]
        | IDREF => [] @ List.concat [st91r0 (stackItem::stack) toPos]
        | IDREFS => [] @ List.concat [st92r0 (stackItem::stack) toPos]
        | ENTITY => [] @ List.concat [st93r0 (stackItem::stack) toPos]
        | ENTITIES => [] @ List.concat [st94r0 (stackItem::stack) toPos]
        | NMTOKEN => [] @ List.concat [st95r0 (stackItem::stack) toPos]
        | NMTOKENS => [] @ List.concat [st96r0 (stackItem::stack) toPos]
        | NOTATION => [(97, (stackItem::stack))]
        | LPAREN => [(98, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 85) *)
      end
  and st84 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 84)
      in
        case category of
          AttDef' _ => [] @ List.concat [st87r0 (stackItem::stack) toPos]
        | AttDef _ => [(84, (stackItem::stack))] @ List.concat [st84r0 (stackItem::stack) toPos]
        | XMLNAME _ => [(85, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 84) *)
      end
  and st84r0 stack pos = go 84 stack (AttDef' []) (pos, pos)
  and st83 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 83)
      in
        case category of
          RTAG => [] @ List.concat [st86r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 83) *)
      end
  and st82 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 82)
      in
        case category of
          AttDef' _ => [(83, (stackItem::stack))]
        | AttDef _ => [(84, (stackItem::stack))] @ List.concat [st84r0 (stackItem::stack) toPos]
        | XMLNAME _ => [(85, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 82) *)
      end
  and st82r0 stack pos = go 82 stack (AttDef' []) (pos, pos)
  and st81r0 ((Contentspec' sv1, pos1, stNum1)::(Contentspec sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Contentspec' (sv0::sv1)) (pos0, pos)
    | st81r0 stack pos = []
  and st80r0 ((RTAG, pos3, stNum3)::(Contentspec' sv2, pos2, stNum2)::(XMLNAME sv1, pos1, stNum1)::(LELEMENTDECL, pos0, stNum0)::stack) pos =
      go stNum0 stack (Elementdecl (Ast.Elementdecl ((pos0, pos), sv1, sv2))) (pos0, pos)
    | st80r0 stack pos = []
  and st79r0 ((COMMA, pos0, stNum0)::stack) pos =
      go stNum0 stack (Contentspec (Ast.CommaContentspec ((pos0, pos)))) (pos0, pos)
    | st79r0 stack pos = []
  and st78r0 ((PLUS, pos0, stNum0)::stack) pos =
      go stNum0 stack (Contentspec (Ast.PlusContentspec ((pos0, pos)))) (pos0, pos)
    | st78r0 stack pos = []
  and st77r0 ((STAR, pos0, stNum0)::stack) pos =
      go stNum0 stack (Contentspec (Ast.StarContentspec ((pos0, pos)))) (pos0, pos)
    | st77r0 stack pos = []
  and st76r0 ((QUESTION, pos0, stNum0)::stack) pos =
      go stNum0 stack (Contentspec (Ast.QuestionContentspec ((pos0, pos)))) (pos0, pos)
    | st76r0 stack pos = []
  and st75r0 ((BAR, pos0, stNum0)::stack) pos =
      go stNum0 stack (Contentspec (Ast.BarContentspec ((pos0, pos)))) (pos0, pos)
    | st75r0 stack pos = []
  and st74r0 ((RPAREN, pos0, stNum0)::stack) pos =
      go stNum0 stack (Contentspec (Ast.RparenContentspec ((pos0, pos)))) (pos0, pos)
    | st74r0 stack pos = []
  and st73r0 ((LPAREN, pos0, stNum0)::stack) pos =
      go stNum0 stack (Contentspec (Ast.LparenContentspec ((pos0, pos)))) (pos0, pos)
    | st73r0 stack pos = []
  and st72r0 ((XMLNAME sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Contentspec (Ast.XmlnameContentspec ((pos0, pos), sv0))) (pos0, pos)
    | st72r0 stack pos = []
  and st71 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 71)
      in
        case category of
          Contentspec' _ => [] @ List.concat [st81r0 (stackItem::stack) toPos]
        | Contentspec _ => [(71, (stackItem::stack))] @ List.concat [st71r0 (stackItem::stack) toPos]
        | XMLNAME _ => [] @ List.concat [st72r0 (stackItem::stack) toPos]
        | LPAREN => [] @ List.concat [st73r0 (stackItem::stack) toPos]
        | RPAREN => [] @ List.concat [st74r0 (stackItem::stack) toPos]
        | BAR => [] @ List.concat [st75r0 (stackItem::stack) toPos]
        | QUESTION => [] @ List.concat [st76r0 (stackItem::stack) toPos]
        | STAR => [] @ List.concat [st77r0 (stackItem::stack) toPos]
        | PLUS => [] @ List.concat [st78r0 (stackItem::stack) toPos]
        | COMMA => [] @ List.concat [st79r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 71) *)
      end
  and st71r0 stack pos = go 71 stack (Contentspec' []) (pos, pos)
  and st70 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 70)
      in
        case category of
          RTAG => [] @ List.concat [st80r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 70) *)
      end
  and st69 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 69)
      in
        case category of
          Contentspec' _ => [(70, (stackItem::stack))]
        | Contentspec _ => [(71, (stackItem::stack))] @ List.concat [st71r0 (stackItem::stack) toPos]
        | XMLNAME _ => [] @ List.concat [st72r0 (stackItem::stack) toPos]
        | LPAREN => [] @ List.concat [st73r0 (stackItem::stack) toPos]
        | RPAREN => [] @ List.concat [st74r0 (stackItem::stack) toPos]
        | BAR => [] @ List.concat [st75r0 (stackItem::stack) toPos]
        | QUESTION => [] @ List.concat [st76r0 (stackItem::stack) toPos]
        | STAR => [] @ List.concat [st77r0 (stackItem::stack) toPos]
        | PLUS => [] @ List.concat [st78r0 (stackItem::stack) toPos]
        | COMMA => [] @ List.concat [st79r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 69) *)
      end
  and st69r0 stack pos = go 69 stack (Contentspec' []) (pos, pos)
  and st68r0 ((IntSubset' sv1, pos1, stNum1)::(IntSubset sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (IntSubset' (sv0::sv1)) (pos0, pos)
    | st68r0 stack pos = []
  and st67r0 ((RTAG, pos5, stNum5)::(RBRACKET, pos4, stNum4)::(IntSubset' sv3, pos3, stNum3)::(LBRACKET, pos2, stNum2)::(XMLNAME sv1, pos1, stNum1)::(LDOCTYPE, pos0, stNum0)::stack) pos =
      go stNum0 stack (Doctypedecl (Ast.Doctypedecl3 ((pos0, pos), sv1, sv3))) (pos0, pos)
    | st67r0 stack pos = []
  and st66 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 66)
      in
        case category of
          RTAG => [] @ List.concat [st67r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 66) *)
      end
  and st65r0 ((PI sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (IntSubset (Ast.PIIntSubset ((pos0, pos), sv0))) (pos0, pos)
    | st65r0 stack pos = []
  and st64 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 64)
      in
        case category of
          XMLNAME _ => [(134, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 64) *)
      end
  and st63r0 ((NotationDecl sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (IntSubset (Ast.NotationDeclIntSubset ((pos0, pos), sv0))) (pos0, pos)
    | st63r0 stack pos = []
  and st62 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 62)
      in
        case category of
          PERCENT => [(114, (stackItem::stack))]
        | XMLNAME _ => [(115, (stackItem::stack))] @ List.concat [st115r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 62) *)
      end
  and st61r0 ((EntityDecl sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (IntSubset (Ast.EntityDeclIntSubset ((pos0, pos), sv0))) (pos0, pos)
    | st61r0 stack pos = []
  and st60 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 60)
      in
        case category of
          XMLNAME _ => [(82, (stackItem::stack))] @ List.concat [st82r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 60) *)
      end
  and st59r0 ((AttlistDecl sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (IntSubset (Ast.AttlistDeclIntSubset ((pos0, pos), sv0))) (pos0, pos)
    | st59r0 stack pos = []
  and st58 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 58)
      in
        case category of
          XMLNAME _ => [(69, (stackItem::stack))] @ List.concat [st69r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 58) *)
      end
  and st57r0 ((Elementdecl sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (IntSubset (Ast.ElementdeclIntSubset ((pos0, pos), sv0))) (pos0, pos)
    | st57r0 stack pos = []
  and st56r0 ((XMLPEREF sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (IntSubset (Ast.PEReferenceIntSubset ((pos0, pos), sv0))) (pos0, pos)
    | st56r0 stack pos = []
  and st55 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 55)
      in
        case category of
          IntSubset' _ => [] @ List.concat [st68r0 (stackItem::stack) toPos]
        | IntSubset _ => [(55, (stackItem::stack))] @ List.concat [st55r0 (stackItem::stack) toPos]
        | XMLPEREF _ => [] @ List.concat [st56r0 (stackItem::stack) toPos]
        | Elementdecl _ => [] @ List.concat [st57r0 (stackItem::stack) toPos]
        | LELEMENTDECL => [(58, (stackItem::stack))]
        | AttlistDecl _ => [] @ List.concat [st59r0 (stackItem::stack) toPos]
        | LATTLISTDECL => [(60, (stackItem::stack))]
        | EntityDecl _ => [] @ List.concat [st61r0 (stackItem::stack) toPos]
        | LENTITYDECL => [(62, (stackItem::stack))]
        | NotationDecl _ => [] @ List.concat [st63r0 (stackItem::stack) toPos]
        | LNOTATIONDECL => [(64, (stackItem::stack))]
        | PI _ => [] @ List.concat [st65r0 (stackItem::stack) toPos]
        | LPI _ => [(14, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 55) *)
      end
  and st55r0 stack pos = go 55 stack (IntSubset' []) (pos, pos)
  and st54 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 54)
      in
        case category of
          RBRACKET => [(66, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 54) *)
      end
  and st53r0 ((XMLCHARS sv2, pos2, stNum2)::(XMLCHARS sv1, pos1, stNum1)::(PUBLIC, pos0, stNum0)::stack) pos =
      go stNum0 stack (ExternalID (Ast.PublicExternalID ((pos0, pos), sv1, sv2))) (pos0, pos)
    | st53r0 stack pos = []
  and st52 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 52)
      in
        case category of
          XMLCHARS _ => [] @ List.concat [st53r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 52) *)
      end
  and st51r0 ((XMLCHARS sv1, pos1, stNum1)::(SYSTEM, pos0, stNum0)::stack) pos =
      go stNum0 stack (ExternalID (Ast.SystemExternalID ((pos0, pos), sv1))) (pos0, pos)
    | st51r0 stack pos = []
  and st50 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 50)
      in
        case category of
          LBRACKET => [(141, (stackItem::stack))] @ List.concat [st141r0 (stackItem::stack) toPos]
        | RTAG => [] @ List.concat [st142r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 50) *)
      end
  and st49 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 49)
      in
        case category of
          IntSubset' _ => [(54, (stackItem::stack))]
        | IntSubset _ => [(55, (stackItem::stack))] @ List.concat [st55r0 (stackItem::stack) toPos]
        | XMLPEREF _ => [] @ List.concat [st56r0 (stackItem::stack) toPos]
        | Elementdecl _ => [] @ List.concat [st57r0 (stackItem::stack) toPos]
        | LELEMENTDECL => [(58, (stackItem::stack))]
        | AttlistDecl _ => [] @ List.concat [st59r0 (stackItem::stack) toPos]
        | LATTLISTDECL => [(60, (stackItem::stack))]
        | EntityDecl _ => [] @ List.concat [st61r0 (stackItem::stack) toPos]
        | LENTITYDECL => [(62, (stackItem::stack))]
        | NotationDecl _ => [] @ List.concat [st63r0 (stackItem::stack) toPos]
        | LNOTATIONDECL => [(64, (stackItem::stack))]
        | PI _ => [] @ List.concat [st65r0 (stackItem::stack) toPos]
        | LPI _ => [(14, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 49) *)
      end
  and st49r0 stack pos = go 49 stack (IntSubset' []) (pos, pos)
  and st48 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 48)
      in
        case category of
          XMLCHARS _ => [(52, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 48) *)
      end
  and st47 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 47)
      in
        case category of
          XMLCHARS _ => [] @ List.concat [st51r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 47) *)
      end
  and st46r0 ((RTAG, pos2, stNum2)::(XMLNAME sv1, pos1, stNum1)::(LDOCTYPE, pos0, stNum0)::stack) pos =
      go stNum0 stack (Doctypedecl (Ast.Doctypedecl1 ((pos0, pos), sv1))) (pos0, pos)
    | st46r0 stack pos = []
  and st45 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 45)
      in
        case category of
          RTAG => [] @ List.concat [st46r0 (stackItem::stack) toPos]
        | SYSTEM => [(47, (stackItem::stack))]
        | PUBLIC => [(48, (stackItem::stack))]
        | LBRACKET => [(49, (stackItem::stack))] @ List.concat [st49r0 (stackItem::stack) toPos]
        | ExternalID _ => [(50, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 45) *)
      end
  and st44r0 ((RXMLCOMMENT, pos2, stNum2)::(XMLCHARS sv1, pos1, stNum1)::(LXMLCOMMENT, pos0, stNum0)::stack) pos =
      go stNum0 stack (Comment (Ast.Comment ((pos0, pos), sv1))) (pos0, pos)
    | st44r0 stack pos = []
  and st43r0 ((RXMLCOMMENT, pos1, stNum1)::(LXMLCOMMENT, pos0, stNum0)::stack) pos =
      go stNum0 stack (Comment (Ast.EmptyComment ((pos0, pos)))) (pos0, pos)
    | st43r0 stack pos = []
  and st42 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 42)
      in
        case category of
          RXMLCOMMENT => [] @ List.concat [st44r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 42) *)
      end
  and st41r0 ((Chars' sv1, pos1, stNum1)::(Chars sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Chars' (sv0::sv1)) (pos0, pos)
    | st41r0 stack pos = []
  and st40r0 ((RPI, pos2, stNum2)::(Chars' sv1, pos1, stNum1)::(LPI sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (PI (Ast.PI ((pos0, pos), sv0, sv1))) (pos0, pos)
    | st40r0 stack pos = []
  and st39r0 ((RPI, pos1, stNum1)::(LPI sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (PI (Ast.EmptyPI ((pos0, pos), sv0))) (pos0, pos)
    | st39r0 stack pos = []
  and st38 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 38)
      in
        case category of
          Chars' _ => [] @ List.concat [st41r0 (stackItem::stack) toPos]
        | XMLCHARS _ => [] @ List.concat [st5r0 (stackItem::stack) toPos]
        | Chars _ => [(38, (stackItem::stack))] @ List.concat [st38r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 38) *)
      end
  and st38r0 ((Chars sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Chars' [sv0]) (pos0, pos)
    | st38r0 stack pos = []
  and st37 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 37)
      in
        case category of
          RPI => [] @ List.concat [st40r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 37) *)
      end
  and st36r0 ((RTAG, pos2, stNum2)::(Attribute' sv1, pos1, stNum1)::(LSTAG sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Stag (Ast.Stag ((pos0, pos), sv0, sv1))) (pos0, pos)
    | st36r0 stack pos = []
  and st35r0 ((REMPTYTAG, pos2, stNum2)::(Attribute' sv1, pos1, stNum1)::(LSTAG sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (EmptyElemTag (Ast.EmptyElemTag ((pos0, pos), sv0, sv1))) (pos0, pos)
    | st35r0 stack pos = []
  and st34r0 ((AttValue' sv1, pos1, stNum1)::(AttValue sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttValue' (sv0::sv1)) (pos0, pos)
    | st34r0 stack pos = []
  and st33r0 ((XMLENTITYREF sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttValue (Ast.ReferenceAttValue ((pos0, pos), sv0))) (pos0, pos)
    | st33r0 stack pos = []
  and st32r0 ((XMLCHARREF sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttValue (Ast.CharRefAttValue ((pos0, pos), sv0))) (pos0, pos)
    | st32r0 stack pos = []
  and st31r0 ((XMLCHARS sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (AttValue (Ast.CharDataAttValue ((pos0, pos), sv0))) (pos0, pos)
    | st31r0 stack pos = []
  and st30 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 30)
      in
        case category of
          AttValue' _ => [] @ List.concat [st34r0 (stackItem::stack) toPos]
        | AttValue _ => [(30, (stackItem::stack))] @ List.concat [st30r0 (stackItem::stack) toPos]
        | XMLCHARS _ => [] @ List.concat [st31r0 (stackItem::stack) toPos]
        | XMLCHARREF _ => [] @ List.concat [st32r0 (stackItem::stack) toPos]
        | XMLENTITYREF _ => [] @ List.concat [st33r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 30) *)
      end
  and st30r0 stack pos = go 30 stack (AttValue' []) (pos, pos)
  and st29r0 ((AttValue' sv2, pos2, stNum2)::(EQUAL, pos1, stNum1)::(XMLNAME sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Attribute (Ast.Attribute ((pos0, pos), sv0, sv2))) (pos0, pos)
    | st29r0 stack pos = []
  and st28 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 28)
      in
        case category of
          AttValue' _ => [] @ List.concat [st29r0 (stackItem::stack) toPos]
        | AttValue _ => [(30, (stackItem::stack))] @ List.concat [st30r0 (stackItem::stack) toPos]
        | XMLCHARS _ => [] @ List.concat [st31r0 (stackItem::stack) toPos]
        | XMLCHARREF _ => [] @ List.concat [st32r0 (stackItem::stack) toPos]
        | XMLENTITYREF _ => [] @ List.concat [st33r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 28) *)
      end
  and st28r0 stack pos = go 28 stack (AttValue' []) (pos, pos)
  and st27r0 ((Attribute' sv1, pos1, stNum1)::(Attribute sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Attribute' (sv0::sv1)) (pos0, pos)
    | st27r0 stack pos = []
  and st26 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 26)
      in
        case category of
          REMPTYTAG => [] @ List.concat [st35r0 (stackItem::stack) toPos]
        | RTAG => [] @ List.concat [st36r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 26) *)
      end
  and st25 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 25)
      in
        case category of
          EQUAL => [(28, (stackItem::stack))] @ List.concat [st28r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 25) *)
      end
  and st24 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 24)
      in
        case category of
          Attribute' _ => [] @ List.concat [st27r0 (stackItem::stack) toPos]
        | Attribute _ => [(24, (stackItem::stack))] @ List.concat [st24r0 (stackItem::stack) toPos]
        | XMLNAME _ => [(25, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 24) *)
      end
  and st24r0 stack pos = go 24 stack (Attribute' []) (pos, pos)
  and st23r0 ((RTAG, pos1, stNum1)::(LETAG sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (ETag (Ast.ETag ((pos0, pos), sv0))) (pos0, pos)
    | st23r0 stack pos = []
  and st22 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 22)
      in
        case category of
          RTAG => [] @ List.concat [st23r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 22) *)
      end
  and st21r0 ((ETag sv2, pos2, stNum2)::(Content' sv1, pos1, stNum1)::(Stag sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Element (Ast.Element ((pos0, pos), sv0, sv1, sv2))) (pos0, pos)
    | st21r0 stack pos = []
  and st20 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 20)
      in
        case category of
          ETag _ => [] @ List.concat [st21r0 (stackItem::stack) toPos]
        | LETAG _ => [(22, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 20) *)
      end
  and st19r0 ((Content' sv1, pos1, stNum1)::(Content sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Content' (sv0::sv1)) (pos0, pos)
    | st19r0 stack pos = []
  and st18 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 18)
      in
        case category of
          XMLNAME _ => [(45, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 18) *)
      end
  and st17r0 ((Doctypedecl sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Content (Ast.DoctypeContent ((pos0, pos), sv0))) (pos0, pos)
    | st17r0 stack pos = []
  and st16 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 16)
      in
        case category of
          XMLCHARS _ => [(42, (stackItem::stack))]
        | RXMLCOMMENT => [] @ List.concat [st43r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 16) *)
      end
  and st15r0 ((Comment sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Content (Ast.CommentContent ((pos0, pos), sv0))) (pos0, pos)
    | st15r0 stack pos = []
  and st14 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 14)
      in
        case category of
          Chars' _ => [(37, (stackItem::stack))]
        | XMLCHARS _ => [] @ List.concat [st5r0 (stackItem::stack) toPos]
        | Chars _ => [(38, (stackItem::stack))] @ List.concat [st38r0 (stackItem::stack) toPos]
        | RPI => [] @ List.concat [st39r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 14) *)
      end
  and st13r0 ((PI sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Content (Ast.PIContent ((pos0, pos), sv0))) (pos0, pos)
    | st13r0 stack pos = []
  and st12r0 ((XMLCDSECT sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Content (Ast.CDSectContent ((pos0, pos), sv0))) (pos0, pos)
    | st12r0 stack pos = []
  and st11r0 ((XMLENTITYREF sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Content (Ast.ReferenceContent ((pos0, pos), sv0))) (pos0, pos)
    | st11r0 stack pos = []
  and st10r0 ((XMLCHARREF sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Content (Ast.CharRefContent ((pos0, pos), sv0))) (pos0, pos)
    | st10r0 stack pos = []
  and st9 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 9)
      in
        case category of
          Attribute _ => [(24, (stackItem::stack))] @ List.concat [st24r0 (stackItem::stack) toPos]
        | XMLNAME _ => [(25, (stackItem::stack))]
        | Attribute' _ => [(26, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 9) *)
      end
  and st9r0 stack pos = go 9 stack (Attribute' []) (pos, pos)
  and st8 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 8)
      in
        case category of
          Content' _ => [(20, (stackItem::stack))]
        | Content _ => [(3, (stackItem::stack))] @ List.concat [st3r0 (stackItem::stack) toPos]
        | Chars _ => [] @ List.concat [st4r0 (stackItem::stack) toPos]
        | XMLCHARS _ => [] @ List.concat [st5r0 (stackItem::stack) toPos]
        | Element _ => [] @ List.concat [st6r0 (stackItem::stack) toPos]
        | EmptyElemTag _ => [] @ List.concat [st7r0 (stackItem::stack) toPos]
        | Stag _ => [(8, (stackItem::stack))] @ List.concat [st8r0 (stackItem::stack) toPos]
        | LSTAG _ => [(9, (stackItem::stack))] @ List.concat [st9r0 (stackItem::stack) toPos]
        | XMLCHARREF _ => [] @ List.concat [st10r0 (stackItem::stack) toPos]
        | XMLENTITYREF _ => [] @ List.concat [st11r0 (stackItem::stack) toPos]
        | XMLCDSECT _ => [] @ List.concat [st12r0 (stackItem::stack) toPos]
        | PI _ => [] @ List.concat [st13r0 (stackItem::stack) toPos]
        | LPI _ => [(14, (stackItem::stack))]
        | Comment _ => [] @ List.concat [st15r0 (stackItem::stack) toPos]
        | LXMLCOMMENT => [(16, (stackItem::stack))]
        | Doctypedecl _ => [] @ List.concat [st17r0 (stackItem::stack) toPos]
        | LDOCTYPE => [(18, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 8) *)
      end
  and st8r0 stack pos = go 8 stack (Content' []) (pos, pos)
  and st7r0 ((EmptyElemTag sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Element (Ast.EmptyElement ((pos0, pos), sv0))) (pos0, pos)
    | st7r0 stack pos = []
  and st6r0 ((Element sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Content (Ast.ElementContent ((pos0, pos), sv0))) (pos0, pos)
    | st6r0 stack pos = []
  and st5r0 ((XMLCHARS sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Chars (Ast.Chars ((pos0, pos), sv0))) (pos0, pos)
    | st5r0 stack pos = []
  and st4r0 ((Chars sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Content (Ast.CharDataContent ((pos0, pos), sv0))) (pos0, pos)
    | st4r0 stack pos = []
  and st3 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 3)
      in
        case category of
          Content' _ => [] @ List.concat [st19r0 (stackItem::stack) toPos]
        | Content _ => [(3, (stackItem::stack))] @ List.concat [st3r0 (stackItem::stack) toPos]
        | Chars _ => [] @ List.concat [st4r0 (stackItem::stack) toPos]
        | XMLCHARS _ => [] @ List.concat [st5r0 (stackItem::stack) toPos]
        | Element _ => [] @ List.concat [st6r0 (stackItem::stack) toPos]
        | EmptyElemTag _ => [] @ List.concat [st7r0 (stackItem::stack) toPos]
        | Stag _ => [(8, (stackItem::stack))] @ List.concat [st8r0 (stackItem::stack) toPos]
        | LSTAG _ => [(9, (stackItem::stack))] @ List.concat [st9r0 (stackItem::stack) toPos]
        | XMLCHARREF _ => [] @ List.concat [st10r0 (stackItem::stack) toPos]
        | XMLENTITYREF _ => [] @ List.concat [st11r0 (stackItem::stack) toPos]
        | XMLCDSECT _ => [] @ List.concat [st12r0 (stackItem::stack) toPos]
        | PI _ => [] @ List.concat [st13r0 (stackItem::stack) toPos]
        | LPI _ => [(14, (stackItem::stack))]
        | Comment _ => [] @ List.concat [st15r0 (stackItem::stack) toPos]
        | LXMLCOMMENT => [(16, (stackItem::stack))]
        | Doctypedecl _ => [] @ List.concat [st17r0 (stackItem::stack) toPos]
        | LDOCTYPE => [(18, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 3) *)
      end
  and st3r0 stack pos = go 3 stack (Content' []) (pos, pos)
  and st2r0 ((Content' sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Document (Ast.Document ((pos0, pos), sv0))) (pos0, pos)
    | st2r0 stack pos = []
  and st1r0 stack pos = [(~1, stack)]
  and st0 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 0)
      in
        case category of
          Document _ => [] @ List.concat [st1r0 (stackItem::stack) toPos]
        | Content' _ => [] @ List.concat [st2r0 (stackItem::stack) toPos]
        | Content _ => [(3, (stackItem::stack))] @ List.concat [st3r0 (stackItem::stack) toPos]
        | Chars _ => [] @ List.concat [st4r0 (stackItem::stack) toPos]
        | XMLCHARS _ => [] @ List.concat [st5r0 (stackItem::stack) toPos]
        | Element _ => [] @ List.concat [st6r0 (stackItem::stack) toPos]
        | EmptyElemTag _ => [] @ List.concat [st7r0 (stackItem::stack) toPos]
        | Stag _ => [(8, (stackItem::stack))] @ List.concat [st8r0 (stackItem::stack) toPos]
        | LSTAG _ => [(9, (stackItem::stack))] @ List.concat [st9r0 (stackItem::stack) toPos]
        | XMLCHARREF _ => [] @ List.concat [st10r0 (stackItem::stack) toPos]
        | XMLENTITYREF _ => [] @ List.concat [st11r0 (stackItem::stack) toPos]
        | XMLCDSECT _ => [] @ List.concat [st12r0 (stackItem::stack) toPos]
        | PI _ => [] @ List.concat [st13r0 (stackItem::stack) toPos]
        | LPI _ => [(14, (stackItem::stack))]
        | Comment _ => [] @ List.concat [st15r0 (stackItem::stack) toPos]
        | LXMLCOMMENT => [(16, (stackItem::stack))]
        | Doctypedecl _ => [] @ List.concat [st17r0 (stackItem::stack) toPos]
        | LDOCTYPE => [(18, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 0) *)
      end
  and st0r0 stack pos = go 0 stack (Content' []) (pos, pos)
  fun parse sourcemap strm =
      let
        val pos = Lex.getPos strm
        val stacks = [(0, [])] @ List.concat [st0r0 [] pos]
        fun loop stacks strm =
            let
              val pos = Lex.getPos strm
              val (token, span, strm') = Lex.lex sourcemap strm
            in
              case token of
                Token.EOF =>
                let
                  val completeStacks = List.filter (fn (st, _) => st = ~1) stacks
                  val topCategories = map (fn (st, stack) => hd stack) completeStacks
                  fun toAst (Document sv, _, _) = SOME sv | toAst _ = NONE
                in
                  List.mapPartial toAst topCategories
                end
              | _ =>
                let
                  val category = Category.fromToken token
                  val stacks' = List.concat (map (fn (st, stack) => go st stack category span) stacks)
                in
                  loop stacks' strm'
                end
            end
      in
        loop stacks strm
      end
end
