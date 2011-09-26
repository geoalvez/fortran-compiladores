
package compil.fortran;

import java_cup.runtime.*;
import java.io.IOException;

import compil.fortran.FortranSym;
import static compil.fortran.FortranSym.*;

%%

%class FortranLex

%unicode
%line
%column

// %public
%final
// %abstract

%cupsym compil.fortran.FortranSym
%cup
// %cupdebug

%init{
	// TODO: code that goes to constructor
%init}

%{
	private Symbol sym(int type)
	{
		return sym(type, yytext());
	}

	private Symbol sym(int type, Object value)
	{
		return new Symbol(type, yyline, yycolumn, value);
	}

	private void error()
	throws IOException
	{
		throw new IOException("illegal text at line = "+yyline+", column = "+yycolumn+", text = '"+yytext()+"'");
	}
%}

ANY			=	.
upper_case	= [A-Z].
lower_case	= [a-z].
letter		= upper_case | lower_case.

digit		= [0-9].

ch_bof		= '\b'		% beginning-of-file %.
ch_eof		= '\e'		% end-of-file %.

id_or_constant	= letter (letter | digit)*	| digit+.

space		= '\040'			% just a space %.

ID			= letter (letter | digit)* | digit+.
LT			= ".LT." | <
LE			= ".LE." | <=
EQ			= ".EQ." | ==
NE			= ".NE." | #
GE			= ".GE." | >=
GT			= ".GT." | >
 
PLUS		= '\+'
MINUS		= '\-'
MULTIPLY	= '\*'
DIVIDE		= '\/'


%%

{ANY}		{	return sym(ANY); }
{ID} { return sym(FortranSym.ID, yytext()); }

"function"  { return sym(FortranSym.FUNCTION); }
"goto" { return sym(FortranSym.GOTO); }
"subroutine" { return sym(FortranSym.SUBROUTINE); }
"call" { return sym(FortranSym.CALL); }
"if" {return sym(FortranSym.IF);}
"then" {return sym(FortranSym.THEN);}
"else" {return sym(FortranSym.ELSE);}
"endif" {return sym(FortranSym.ENDIF);}
"do" {return sym(FortranSym.DO);}
"while" {return sym(FortranSym.WHILE);}
"true" {return sym(FortranSym.TRUE);}
"false" {return sym(FortranSym.FALSE);}

{LT} 	{ return sym(FortranSym.LT); }
{LE} 	{ return sym(FortranSym.LE); }
{EQ} 	{ return sym(FortranSym.EQ); }
{NE} 	{ return sym(FortranSym.NE); }
{GE} 	{ return sym(FortranSym.GE); }
{GT} 	{ return sym(FortranSym.GT); }


".AND." { return sym(FortranSym.AND); }
".OR." { return sym(FortranSym.OR); } 
".NOT." { return sym(FortranSym.NOT); }

{PLUS} { return sym(FortranSym.PLUS); }
{MINUS} { return sym(FortranSym.MINUS); }
{MULTIPLY} { return sym(FortranSym.MULTIPLY); }
{DIVIDE} { return sym(FortranSym.DIVIDE); }
