// Portions from Java.g

grammar Scala;

options {
	backtrack = true;
	memoize = true;
	output = AST;
}

// note: try/true mis-alphabetized

tokens {
	ABSTRACT				= 'abstract'		;
	CASE					= 'case'			;
	CATCH					= 'catch'			;
	CLASS					= 'class'			;
	DEF						= 'def'				;
	DO						= 'do'				;
	ELSE					= 'else'			;
	EXTENDS					= 'extends'			;
	FALSE                   = 'false'           ;
	FINAL					= 'final'			;
	FINALLY					= 'finally'			;
	FOR						= 'for'				;
	FORSOME					= 'forSome'			;
	IF						= 'if'				;
	IMPLICIT				= 'implicit'		;
	IMPORT					= 'import'			;
	LAZY					= 'lazy'			;
	MATCH					= 'match'			;
	NEW						= 'new'				;
	NULL                    = 'null'            ;
	OBJECT					= 'object'			;
	OVERRIDE				= 'override'		;
	PACKAGE					= 'package'			;
	PRIVATE					= 'private'			;
	PROTECTED				= 'protected'		;
	REQUIRES				= 'requires'		; 		// paulp
	RETURN					= 'return'			;
	SEALED					= 'sealed'			;
	SUPER					= 'super'			;
	THIS                    = 'this'            ;
	THROW					= 'throw'			;
	TRAIT					= 'trait'			;
	TRUE                    = 'true'            ;
	TRY						= 'try'				;
	TYPE					= 'type'			;
	VAL						= 'val'				;
	VAR						= 'var'				;
	WHILE					= 'while'			;
	WITH					= 'with'			;
	YIELD					= 'yield'			;
	
	ARROW					= '\u21d2'			;		// '=>'
	AT						= '@'				;
	COLON					= ':'				;
	EQUALS					= '='				;
	GENERATED_BY			= '<-'				;
	SHARP					= '#'				;
	SUBTYPE					= '<:'				;
	SUPERTYPE				= ':>'				;
	UNDERSCORE				= '_'				;
	VIEW_BOUND				= '<%'				;
}

//
// antlr fragments are "helper" lex rules, invalid as terminals
//

fragment
JAVA_ID_START
    :  '\u0024'
    |  '\u0041'..'\u005a'
    |  '\u005f'
    |  '\u0061'..'\u007a'
    |  '\u00c0'..'\u00d6'
    |  '\u00d8'..'\u00f6'
    |  '\u00f8'..'\u00ff'
    |  '\u0100'..'\u1fff'
    |  '\u3040'..'\u318f'
    |  '\u3300'..'\u337f'
    |  '\u3400'..'\u3d2d'
    |  '\u4e00'..'\u9fff'
    |  '\uf900'..'\ufaff'
    ;

fragment
JAVA_ID_PART
	:  JAVA_ID_START
	|  '\u0030'..'\u0039'
	;

fragment	TRIPLE_QUOTE	: '"""'				;
fragment	BACKQUOTE		: '`'				;
fragment	COMMA			: ','				;
fragment	DOT				: '.'				;
fragment	DOUBLEQUOTE		: '"'				;
fragment	LBRACE			: '['				;
fragment	RBRACE			: ']'				;
fragment	LPAREN			: '('				;
fragment	RPAREN			: ')'				;
fragment	LCURLY			: '{'				;
fragment	RCURLY			: '}'				;
fragment	QUOTE			: '\''				;
fragment	SEMICOLON		: ';'				;

fragment	UPPER			: ('A'..'Z' | '$' | '_') ;		// + Unicode Lu
fragment	LOWER			: 'a'..'z' ; 					// + Unicode Ll
fragment	LETTER			: UPPER | LOWER ;				// + Unicode Lt, Lo, Nl
fragment	DIGIT			: '0'..'9' ;
fragment	NON_ZERO_DIGIT	: '1'..'9' ;
fragment	OCTAL_DIGIT		: '0'..'7' ;
fragment	HEX_DIGIT		: '0'..'9' | 'A'..'F' | 'a'..'f' ;		// paulp
fragment	EXPONENT_PART	: ('E' | 'e') ('+' | '-')? DIGIT+ ;
fragment	FLOAT_TYPE		: 'F' | 'f' | 'D' | 'd' ;
fragment	PARENTHESES		: LPAREN | RPAREN | LBRACE | RBRACE | LCURLY | RCURLY ;			// paulp
fragment	DELIMITER		: BACKQUOTE | QUOTE | DOUBLEQUOTE | DOT | SEMICOLON | COMMA ;	// paulp

// The $ character is reserved for compiler-synthesized identiﬁers. User programs 
// should not deﬁne identiﬁers which contain $ characters. 
// XXX should warn


// opchar: all printable ascii in range \u0020 - \u007F which are not in:
//   whitespace, letters, digits, parentheses, or delimiters
// plus unicode categories Sm and So (math symbols and other symbols)
fragment	OPCHAR		: ~(WHITESPACE | NL | LETTER | DIGIT | PARENTHESES | DELIMITER) ;


OP			: OPCHAR+ ;
VARID		: LOWER IDREST ;
PLAINID		: UPPER IDREST | VARID | OP ;
ID			: PLAINID | BACKQUOTE STRING_LITERAL BACKQUOTE ;
IDREST		: JAVA_ID_PART* ('_' OP)? ;

INTEGER_LITERAL		: (DECIMAL_NUMERAL | HEX_NUMERAL | OCTAL_NUMERAL) ('L' | 'l')? ;
DECIMAL_NUMERAL		: '0' | NON_ZERO_DIGIT DIGIT* ;
HEX_NUMERAL			: '0' ('x' | 'X') HEX_DIGIT+ ;				// BUG
OCTAL_NUMERAL		: '0' OCTAL_DIGIT+ ;
FP_LITERAL			: DIGIT+ '.' DIGIT* EXPONENT_PART? FLOAT_TYPE?
					| '.' DIGIT+ EXPONENT_PART? FLOAT_TYPE?
					| DIGIT+ EXPONENT_PART FLOAT_TYPE?
					| DIGIT+ FLOAT_TYPE
					;
BOOLEAN_LITERAL		: TRUE | FALSE ;
CHARACTER_LITERAL	: QUOTE ( ESCAPE_SEQUENCE | ~('\''|'\\') ) QUOTE ;
STRING_LITERAL		: DOUBLEQUOTE ( ESCAPE_SEQUENCE | ~('\\'|'"') )* DOUBLEQUOTE
					| TRIPLE_QUOTE .* TRIPLE_QUOTE
					;

fragment
ESCAPE_SEQUENCE		: '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
    				| UNICODE_ESCAPE
    				| OCTAL_ESCAPE
    				;
fragment
OCTAL_ESCAPE		: '\\' ('0'..'3') ('0'..'7') ('0'..'7')
	    			| '\\' ('0'..'7') ('0'..'7')
	    			| '\\' ('0'..'7')
	    			;
fragment
UNICODE_ESCAPE		: '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT ;



// Scala spec versions
/*
CHARACTER_LITERAL	: '\'' PRINTABLE_CHAR '\''
					| '\'' CHAR_ESCAPE_SEQ '\'' 
					;

STRING_LITERAL		: '"' STRING_ELEMENT* '"'
					| '"""' MULTI_LINE_CHARS '"""'
					;
*/			
		
// STRING_ELEMENT		: CHARACTER_LITERAL ;
		// PRINTABLE_CHAR_NO_DOUBLE_QUOTE | CHAR_ESCAPE_SEQ ;	// UNDEF
// MULTI_LINE_CHARS	: ('"'? '"'? LETTER)* ;
		// ('"'? '"'? CHAR_NO_DOUBLE_QUOTE)* ;					// UNDEF
SYMBOL_LITERAL		: QUOTE PLAINID ;

// 1.4: Multi-line comments may be nested.   XXX
COMMENT				: '/*' ( options { greedy=false; } : . )* '*/'
					| '//' ( options { greedy=false; } : . )* NL
					;
					
// A newline in a Scala source text is treated as the special token 
// NL   if the three following criteria are satisﬁed: 
// 1. The token immediately preceding the newline can terminate a statement. 
// 2. The token immediately following the newline can begin a statement. 
// 3. The token appears in a region where multiple statements are allowed. 
// Multi-page elaboration in spec: see section 1.2
// XXX TODO
NL					: '\r'? '\n' { $channel = HIDDEN; } ;	// call whatever(); here to test for NL-ness

SEMI				: ';' | NL+ ;
WHITESPACE			: (' ' | '\t' | '\r')+ { $channel = HIDDEN; } ;		// XXX
// WHITESPACE			: ('\u0020' | '\u0009' | '\u000D' | '\u000A')+ { skip(); } ;

// 
// XML mode
//

// lexical analysis switches from Scala mode to XML mode when encountering an opening
// angle bracket  <  in the following circumstance: The  <  must be preceded either by
// whitespace, an opening parenthesis or an opening brace and immediately followed by a
// character starting an XML name.
// 
// then....

// The scanner switches from XML mode to Scala mode if either 1) the XML expression or the
// XML pattern started by the initial < has been successfully parsed, or if 2) the parser
// encounters an embedded Scala expression or pattern and forces the Scanner back to normal
// mode, until the Scala expression or pattern is successfully parsed. In this case, since
// code and XML fragments can be nested, the parser has to maintain a stack that reﬂects the
// nesting of XML and Scala expressions adequately.


// end lexer					

literal				: '-'? INTEGER_LITERAL
					| '-'? FP_LITERAL
					| BOOLEAN_LITERAL
					| CHARACTER_LITERAL
					| STRING_LITERAL
					| SYMBOL_LITERAL
					| NULL
					;
					
qualId				: ID (DOT ID)* ;
ids					: ID (COMMA ID)* ;

path				: stableId
					| (ID DOT)? THIS
					;
// before eliminating left recursion, stableId looked like this:
// stableId			: ID
//					| stableId DOT ID
//					| (ID DOT)? THIS DOT ID
//					| (ID '.')? SUPER classQualifier? '.' ID
stableId			: (ID | (ID DOT)? THIS DOT ID | (ID DOT)? SUPER classQualifier? DOT ID) (DOT ID)* ;
classQualifier		: '[' ID ']' ;

type				: infixType ARROW type
					| '(' (ARROW type)? ')' ARROW type
					| infixType existentialClause?
					;
existentialClause	: FORSOME '{' existentialDcl (SEMI existentialDcl)* '}' ;
existentialDcl		: TYPE typeDcl | VAL valDcl ;
infixType			: compoundType (ID NL? compoundType)* ;
compoundType		: annotType (WITH annotType)* refinement? | refinement ;
annotType			: simpleType annotation* ;
simpleType			: (stableId | path '.' TYPE | '(' types ','? ')') (typeArgs | SHARP ID)*
					;
typeArgs			: '[' types ']' ;
types				: type (',' type)* ;
refinement			: NL? '{' refineStat (SEMI refineStat)* '}' ;
refineStat			: dcl?
 					| TYPE typeDef
					;
typePat				: type ;

ascription			: ':' compoundType
					| ':' annotation+
					| ':' '_' '*'
					;

expr				: (bindings | ID) ARROW expr | expr1 ;
expr1				: IF '(' expr ')' NL* expr (SEMI? ELSE expr)?
					| WHILE '(' expr ')' NL* expr
					| TRY '{' block '}' (CATCH '{' caseClauses '}')? (FINALLY expr)?
					| DO expr SEMI? WHILE '(' expr ')'
					| FOR ('(' enumerators ')' | '{' enumerators '}') NL* YIELD? expr
					| THROW expr
					| RETURN expr?
					| (simpleExpr '.')? ID '=' expr
					| ((literal | path | '_' | '(' (exprs ','?)? ')' | simpleExpr '.' ID | simpleExpr typeArgs) (argumentExprs)*) argumentExprs '=' expr
					| postfixExpr
					| postfixExpr ascription
					| postfixExpr MATCH '{' caseClauses '}'
					;
					
postfixExpr			: infixExpr (ID NL?)? ;
infixExpr			: (prefixExpr) (ID NL? infixExpr)* ;
prefixExpr			: ('-' | '+' | '~' | '!')? simpleExpr ;
simpleExpr			: NEW (classTemplate | templateBody) simpleExpr1
					| blockExpr simpleExpr1
					| (literal | path | '_' | '(' (exprs ','?)? ')') (argumentExprs)* '_'? simpleExpr1
					;

simpleExpr1			: ((DOT ID | typeArgs) argumentExprs* UNDERSCORE? simpleExpr1)? ;
					

exprs				: expr (',' expr)* ;
argumentExprs		: '(' (exprs ','?)? ')' | NL? blockExpr ;
blockExpr			: '{' (caseClauses | block) '}' ;
block				: (blockStat SEMI)* resultExpr? ;
blockStat			: importDcl
					| (IMPLICIT | LAZY)? def
					| localModifier* tmplDef
					| expr1?
					;
resultExpr			: expr1 | (bindings | ID ':' compoundType) ARROW block ;
					
enumerators			: generator (SEMI enumerator)* ;
enumerator			: generator | guard | VAL pattern1 '=' expr ;
generator			: pattern1 GENERATED_BY expr guard? ;

caseClauses			: caseClause+ ;
caseClause			: CASE pattern guard? ARROW block ;
guard				: IF postfixExpr ; 

pattern				: pattern1 ('|' pattern1)* ;
pattern1			: VARID ':' typePat
					| '_' ':' typePat
					| pattern2
					;
pattern2			: VARID ('@' pattern3)? | pattern3 ;
pattern3			: simplePattern					// XXX Necessary for what?
					| simplePattern (ID NL? simplePattern)*	
					;
simplePattern		: '_'
					| VARID
					| literal
					| stableId
					| stableId '(' (patterns ','?)? ')'
					| stableId '(' (patterns ',')? '_' '*' ')'
					| '(' (patterns ','?)? ')'
					// | xmlPattern									// MISSING
					;
patterns			: pattern (',' patterns)? | '_' '*' ;			// TYPO - ?

typeParamClause		: '[' variantTypeParam (',' variantTypeParam)* ']' ;
funTypeParamClause	: '[' typeParam (',' typeParam)* ']' ;
variantTypeParam	: ('+' | '-')? typeParam ;
typeParam			: (ID | '_') typeParamClause? ('>:' type)? ('<:' type)? ('<%' type)? ; // OK?
paramClauses		: paramClause* (NL? '(' IMPLICIT params ')')? ;
paramClause			: NL? '(' params? ')' ;

params				: param (',' param)* ;
param				: annotation* ID (':' paramType)? ;
paramType			: type
					| ARROW type
					| type '*'
					;

classParamClauses	: classParamClause* (NL? '(' IMPLICIT classParams ')')? ;
classParamClause	: NL? '(' classParams? ')' ;
classParams			: classParam (',' classParam)* ;
classParam			: annotation* (modifier* (VAL | VAR))? ID (':' paramType)? ;
bindings			: '(' binding (',' binding)* ')' ;
binding				: ID (':' type)? ;

modifier			: localModifier | accessModifier | OVERRIDE ;
localModifier		: ABSTRACT | FINAL | SEALED ;
accessModifier		: (PRIVATE | PROTECTED) accessQualifier? ;
accessQualifier		: '[' (ID | THIS) ']' ;

annotation			: '@' annotationExpr ;
annotationExpr		: constr (NL? '{' (nameValuePair (',' nameValuePair)* )? '}' )? ;
nameValuePair		: VAL ID '=' prefixExpr ;
templateBody		: NL? '{' selfType? templateStat (SEMI templateStat)* '}' ;
templateStat		: importDcl
					| (annotation NL?)* modifier* def
					| (annotation NL?)* modifier* dcl
					| expr?
					;
					
selfType			: ID (':' type)? ARROW
					| THIS ':' type ARROW
					;
					
importDcl			: IMPORT importExpr (',' importExpr)* ;
importExpr			: stableId '.' (ID | '_' | importSelectors) ;
importSelectors		: '{' (importSelector ',')* (importSelector | '_') '}' ;
importSelector		: ID (ARROW ID | ARROW '_')? ;

dcl					: VAL valDcl
					| VAR varDcl
					| DEF funDcl
					| TYPE NL* typeDcl
					;

valDcl				: ids ':' type ;
varDcl				: ids ':' type ;
funDcl				: funSig (':' type)? ;
funSig				: ID funTypeParamClause? paramClauses ;
typeDcl				: ID typeParamClause? ('>:' type)? ('<:' type)? ; 	// ??? Sufficient?
patVarDef			: VAL patDef | VAR varDef ;
def          		: patVarDef
					| DEF funDef
					| TYPE NL* typeDef
					| tmplDef
					;
patDef				: pattern2 (',' pattern2)* (':' type)? '=' expr ;
varDef				: patDef | ids ':' type '=' '_' ;
funDef				: funSig (':' type) '=' expr
					| funSig NL? '{' block '}'
					| THIS paramClause paramClauses ('=' constrExpr | NL? constrBlock) 
					;
typeDef				: ID typeParamClause? '=' type ;
tmplDef				: CASE? CLASS classDef
					| CASE? OBJECT objectDef
					| TRAIT traitDef
					;
					
classDef			: ID typeParamClause? annotation* accessModifier? classParamClauses classTemplateOpt ;
traitDef			: ID typeParamClause? traitTemplateOpt ;
objectDef			: ID classTemplateOpt ;
classTemplateOpt	: extends classTemplate | (extends? templateBody)? ;
traitTemplateOpt	: extends traitTemplate | (extends? templateBody)? ;
extends				: EXTENDS | '<:' ;
classTemplate		: earlyDefs? classParents templateBody? ;
traitTemplate		: earlyDefs? traitParents templateBody? ;
classParents		: constr (WITH annotType)* ;					// ??
traitParents		: annotType (WITH annotType)* ;
constr				: annotType argumentExprs* ;
earlyDefs			: '{' (earlyDef (SEMI earlyDef)*)? '}' WITH ;
earlyDef			: (annotation NL?)* modifier* patVarDef ;

constrExpr			: selfInvocation | constrBlock ;
constrBlock			: '{' selfInvocation (SEMI blockStat)* '}' ;
selfInvocation		: THIS argumentExprs+ ;

topStatSeq			: topStat (SEMI topStat)* ;
topStat				: (annotation NL?)* modifier* tmplDef
					| importDcl
					| packaging
					|
					;
packaging			: PACKAGE qualId NL? '{' topStatSeq '}' ;

compilationUnit		: (PACKAGE qualId SEMI)? topStatSeq ;

// todo add
// SimplePattern    ::= StableId  [TypePatArgs] [`(' [SeqPatterns [`,']] `)']
// TypePatArgs ::= `[' TypePatArg {`,' TypePatArg} `]'
// TypePatArg    ::=  `_' |   varid}


