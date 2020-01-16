
//          Copyright Sanjay S, 2018.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

import std.algorithm;
import std.conv;
import std.exception;
import std.file;
import std.getopt;
import std.range;
import std.stdio;
import std.string;

import pegged.grammar;
import pegged.tohtml;

mixin(grammar(
`
ABNF:
    RuleList      <- (Rule / (CWSP* CNL))+ endOfInput
    Rule          <- RuleName DefinedAs Elements CNL
    RuleName      <- ALPHA (ALPHA / DIGIT / HYPHEN)*
    DefinedAs     <- CWSP* (FSLASHEQUALS / EQUALS) CWSP*
    Elements      <- Alternation CWSP*
    CWSP          <- WSP / (CNL WSP)
    CNL           <- Comment / CRLF
    Comment       <- SEMICOLON (WSP / VCHAR)* CRLF
    Alternation   <- Concatenation (CWSP* FSLASH CWSP* Concatenation)*
    Concatenation <- Repetition (CWSP+ Repetition)*
    Repetition    <- Repeat? Element
    Repeat        <- (DIGIT* ASTERIX DIGIT*) / DIGIT+
    Element       <- RuleName / Group / Option / CharVal / NumVal / ProseVal
    Group         <- LPAREN CWSP* Alternation CWSP* RPAREN
    Option        <- LBRACKET CWSP* Alternation CWSP* RBRACKET
    CharVal       <- iCharVal / sCharVal
    iCharVal      <- ISTR? DQUOTE DQUOTECHARS* DQUOTE
    sCharVal      <- s1CharVal / s2CharVal
    s1CharVal     <- SSTR DQUOTE DQUOTECHARS* DQUOTE
    s2CharVal     <- SQUOTE SQUOTECHARS* SQUOTE
    NumVal        <- NUMIND (BinVal / DecVal / HexVal)
    BinVal        <- BVAL BinNum ((PERIOD BinNum)+ / (HYPHEN BinNum))?
    BinNum        <- BIT+
    DecVal        <- DVAL DecNum ((PERIOD DecNum)+ / (HYPHEN DecNum))?
    DecNum        <- DIGIT+
    HexVal        <- XVAL HexNum ((PERIOD HexNum)+ / (HYPHEN HexNum))?
    HexNum        <- HEXDIG+
    ProseVal      <- LCHEVRON (CHEVRONCHARS / CRLF )* RCHEVRON

    WSP           <- SP / HTAB

    DQUOTECHARS   <- DQUOTELCHARS / DQUOTERCHARS
    SQUOTECHARS   <- SQUOTELCHARS / SQUOTERCHARS
    CHEVRONCHARS  <- CHEVRONLCHARS / CHEVRONRCHARS

    DQUOTELCHARS  <- [ !]
    DQUOTERCHARS  <- [#-~]

    SQUOTELCHARS  <- [ &]
    SQUOTERCHARS  <- [(-~]

    CHEVRONLCHARS <- [ -=]
    CHEVRONRCHARS <- [?-~]

    CRLF          <- CR? LF

    NUMIND        <- "%"

    BVAL          <- "b"
    DVAL          <- "d"
    XVAL          <- "x"

    PERIOD        <- "."

    LPAREN        <- "("
    RPAREN        <- ")"
    LBRACKET      <- "["
    RBRACKET      <- "]"
    LCHEVRON      <- "<"
    RCHEVRON      <- ">"
    ASTERIX       <- "*"
    FSLASH        <- "/"
    HYPHEN        <- "-"
    EQUALS        <- "="
    FSLASHEQUALS  <- "/="
    SEMICOLON     <- ";"
    ISTR          <- "i%"
    SSTR          <- "s%"
    VCHAR         <- [!-~]
    ALPHA         <- [A-Za-z]
    DIGIT         <- [0-9]
    BIT           <- [01]
    HEXDIG        <- [0-9A-Fa-f]
    DQUOTE        <- ["]
    SQUOTE        <- [']
    HTAB          <- [\t]
    SP            <- [ ]
    CR            <- [\r]
    LF            <- [\n]
`
));

uint lhsSpace = 16; /// Number of characters on the LHS of a rule (padded with spaces).
enum usageStr = "Usage: abnf2peg [options] <ABNF Grammar File Name>\n\nOptions:\n"; /// Usage string

void main(string[] args)
{
    try
    {
	auto cliResult = getopt(args, "lhs-space|l", "Number of characters before the rule separator (used to pad shorter rule names with spaces)", &lhsSpace);
	if (cliResult.helpWanted)
	{
	    defaultGetoptPrinter(usageStr, cliResult.options);
	    return;
	}

	checkUsage(args);

	auto abnfGrammar = readText(args[1]);

	auto p = ABNF(abnfGrammar);
	writeln(toPeg(p));
    }
    catch (Exception e)
    {
	writeln(e.msg);
    }
}

void checkUsage(string[] args)
{
    enforce(args.length <= 2, "Unexpected command line parameters: " ~ to!string(args));
    enforce(args.length == 2, "Provide the name of an ABNF grammar file. ");
    enforce(lhsSpace >= 2, "Too few characters requested for LHS side -- min allowed is 2.");
    enforce(lhsSpace <= 40, "Too many characters requested for LHS side -- max allowed is 40.");
}

string parseChildren(ParseTree p)
{
    string result = "";

    foreach(ref child; p.children)
	result ~= parseToPeg(child);

    return result;
}

string nTimes(string elem, int times, bool optional)
{
    string appendStr = (optional) ? elem ~ "?" : elem;
    string result = appendStr;

    for (int i = 1; i < times; i++)
    {
	result = result ~ " " ~ appendStr;
    }

    return result;
}

string translateNumVal(ParseTree p)
{
    int base = 10;
    string result = "";

    string[] numStrs;
    foreach(ref child; p.children)
	numStrs ~= parseToPeg(child);

    enforce(numStrs.length >= 2, "Unexpected numeric string format: " ~ to!string(numStrs));

    if (numStrs[0] == "b")
	base = 2;
    else if (numStrs[0] == "x")
	base = 16;
    else if (numStrs[0] == "d")
	base = 10;
    else
	assert(0, "Unexpected number prefix: " ~ to!string(numStrs[0]));

    if (numStrs.length == 2)
    {
	// Just a single digit
	auto num = to!int(numStrs[1], base);
	result = " [" ~ toAsciiStr(num, [ `[`, `]`, `\` ]) ~ "] ";
    }
    else if (numStrs[2] == "-" && numStrs.length == 4)
    {
	// A range
	auto min = to!int(numStrs[1], base);
	auto max = to!int(numStrs[3], base);

	result = " [" ~ toAsciiStr(min, [ `[`, `]`, `\` ]) ~ "-" ~ toAsciiStr(max, [ `[`, `]`, `\` ]) ~ "] ";
    }
    else if (numStrs[2] == ".")
    {
	// import std.array : join;
	// import std.stdio : stderr;
	// stderr.writeln(`numVal: "` ~ numStrs.join ~ `".`);
	// stderr.writeln(`separately: ` ~ numStrs.to!string);
	// stderr.writeln('\n');

	// Concatenated numbers
	result = `"`;
	for (int i = 1; i < numStrs.length; i += 2)
	{
	    auto num = to!int(numStrs[i], base);
	    result ~= toAsciiStr(num, [ `"`, `\` ]);
	}
	result ~= `"`;
    }
    else
    {
	assert(0, "Unexpected string format: " ~ to!string(numStrs));
    }

    return result;
}

string translateRule(ParseTree p)
{
    // Rule name
    string result = parseToPeg(p.children[0]);

    // Pad rule name
    result = leftJustify(result, lhsSpace, ' ');

    // The arrow
    result ~= " <- ";

    // Finally append the remaining items
    foreach(ref child; p.children[2 .. $])
	result ~= parseToPeg(child);

    return result;
}

string translateRuleName(ParseTree p)
{
    string result = parseChildren(p);

    result = tr(result, "-", "_");

    return result;
}

string translateRepetition(ParseTree p)
{
    string repStrs = parseToPeg(p.children[0]);

    if (p.children.length == 1)
	return repStrs; // Nothing to translate

    string[] repList = repStrs.split("*");
    int atleast = to!int(repList[0]);
    int atmost = to!int(repList[1]);

    // atmost cannot be less than atleast; though
    // 0 is allowed to indicate infinity
    enforce(atmost == 0 || atmost >= atleast, "Unexpected repetition values: " ~ repList[0] ~ " (left value), " ~ repList[1] ~ " (right value)");

    string elem = parseToPeg(p.children[1]);

    // Special case for "1 or more"
    if (atleast == 1 && atmost == 0)
	return elem ~ "+";

    string result = nTimes(elem, atleast, false); // Repeat atleast number of times

    if (atmost == 0) // Infinity check
    {
	if (atleast == 0)
	    result = result ~ "*";
	else
	    result = result ~ " " ~ elem ~ "*";
    }
    else if (atmost > atleast) // Pad upto the difference
	result = result ~ " " ~ nTimes(elem, atmost - atleast, true);

    return result;
}

string translateRepeat(ParseTree p)
{
    string result = parseChildren(p);

    // matches can be n, or n*, or *n, or n*n, or *
    if (p.matches.length == 1)
    {
	if (p.matches[0] == "*")
	{
	    result = "0*0";
	}
	else
	{
	    result = p.matches[0] ~ "*" ~ p.matches[0];
	}
    }
    else if (p.matches.length == 2)
    {
	if (p.matches[0] == "*")
	{
	    result = "0*" ~ p.matches[1];
	}
	else
	{
	    result = p.matches[0] ~ "*0"; // Use 0 to mean infinity
	}
    }
    else if (p.matches.length == 3)
    {
	result = p.matches[0] ~ "*" ~ p.matches[2];
    }
    else
	assert(0, "Unsupported repetition format: " ~ to!string(p.matches));

    return result;
}

string parseToPeg(ParseTree p)
{
    string result = "";

    // In below, we only have case statements for the non-terminal rules we
    // need to translate from ABNF format to PEG format. The rest are
    // handled by the default case.
    switch (p.name)
    {
    case "ABNF.Rule":
	result = translateRule(p);
	break;
    case "ABNF.RuleName":
	result = translateRuleName(p);
	break;
    case "ABNF.DefinedAs":
	result = " <- ";
	break;
    case "ABNF.Repetition":
	result = translateRepetition(p);
	break;
    case "ABNF.Repeat":
	result = translateRepeat(p);
	break;
    case "ABNF.Option":
	result = parseChildren(p) ~ "?";
	break;
    case "ABNF.CharVal":
	result = parseChildren(p);
	break;
    case "ABNF.iCharVal":
	result = parseChildren(p);
	break;
    case "ABNF.sCharVal":
	result = parseChildren(p);
	break;
    case "ABNF.s1CharVal":
	result = parseChildren(p);
	break;
    case "ABNF.s2CharVal":
	result = parseChildren(p);
	break;
    case "ABNF.NumVal":
	// Below skips the first '%' indicator of a number
	result = parseToPeg(p.children[1]);
	break;
    case "ABNF.BinVal":
    case "ABNF.DecVal":
    case "ABNF.HexVal":
	result = translateNumVal(p);
    break;

    case "ABNF.SEMICOLON":
	result = "#";
	break;
    case "ABNF.CRLF":
	result = "\n";
	break;
    case "ABNF.LBRACKET":
	result = "("; // Replace "[" with "("
	break;
    case "ABNF.RBRACKET":
	result = ")"; // Replace "]" with ")"
	break;

    default:
	if (p.children.length) // Non-terminals
	{
	    result = parseChildren(p);
	}
	else // Terminals
	{
	    result = p.matches[0];
	}
	break;
    }

    return result;
}

string toPeg(ParseTree p)
{
    return parseToPeg(p.children[0]);
}

string toAsciiStr(int num, string[] toEscape)
{
    auto errMsg = "Only values in the ASCII range are supported: ";

    import std.format : format;

    if (num <= 0x7F)
    {
	import std.algorithm : any;

	auto asciiStr = asciiTable[num];
	if (toEscape.any!((e) => e == asciiStr))
	{
	    asciiStr = `\` ~ asciiStr;
	}
	enforce(asciiStr != "", errMsg ~ to!string(num) ~ " (Hex: 0x" ~ to!string(num, 16) ~ ")");
	return asciiStr;
    }
    else if (num <= 0xFF)
    {
	return "\\x" ~ to!string(num, 16);
    }
    else if (num <= 0xFFFF)
    {
	return `\u` ~ "%04X".format(num);
    }
    else
    {
	return `\U` ~ "%08X".format(num);
    }
}

// ASCII Table as a an array of strings since that
// is most suitable for our intended use.
string[] asciiTable =
[
     "\\x00", "\\x01", "\\x02", "\\x03", "\\x04", "\\x05", "\\x06", "\\x07",
     "\\x08", "\\t", "\\n", "\\x0B", "\\x0C", "\\r", "\\x0D", "\\x0E",
     "\\x10", "\\x11", "\\x12", "\\x13", "\\x14", "\\x15", "\\x16", "\\x17",
     "\\x18", "\\x19", "\\x1A", "\\x1B", "\\x1C", "\\x1D", "\\x1E", "\\x1F",
     " ", "!", "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/",
     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?",
     "@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
     "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[", "\\", "]", "^", "_",
     "`", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
     "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "{", "|", "}", "~", "\\x7F"
];
