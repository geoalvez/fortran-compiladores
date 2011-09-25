
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

"FUNCTION"  { return sym(FortranSym.FUNCTION); }
"GOTO" { return sym(FortranSym.GOTO); }
"SUBROUTINE" { return sym(FortranSym.SUBROUTINE); }
"CALL" { return sym(FortranSym.CALL); }
"IF" {return sym(FortranSym.IF);}
"THEN" {return sym(FortranSym.THEN);}
"ELSE" {return sym(FortranSym.ELSE);}
"DO" {return sym(FortranSym.DO);}
"WHILE" {return sym(FortranSym.WHILE);}

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
