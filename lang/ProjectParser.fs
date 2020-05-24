// (c) psc 2020
// a parser for LiTeX, modify at your own risk

module ProjectParser

open Parser
open System

// Possible data types for use in ast
type Expr = 
| Modifiers of Modifier list
and Modifier =
| Word of string
| Paragraph
| Bold
| Underline
| Ital
| Break
| Header of string
| Size of string 
| Macro of string * string list
| MacroPhrase of string 


// recparser work-around
let expr, exprImpl = recparser()

// parse punctuation
let punctuation = (pchar '.' <|> pchar '(' <|> pchar ')' <|> pchar ',' 
<|> pchar ':' <|> pchar ';' <|> pchar '!' <|> pchar '?' <|> pchar '''
<|> pchar '"' <|> pchar '&' <|> pchar '$' <|> pchar '%' <|> pchar '{' <|> pchar '}') 

// temporary catch-all parser for all characters allowed in words 
let temp = (pws1 <|> pmany1 pletter <|> pmany1 punctuation <|> pmany1 pdigit) |>> stringify

// Parsers for possible "Modifiers"
let word : Parser<Modifier> = pmany1 temp |>> List.fold (+) "" |>> (fun e -> Word e)
let bold : Parser<Modifier> = pstr "+" |>> (fun _ -> Bold)
let ital : Parser<Modifier> = pstr "*" |>> (fun _ -> Ital)
let underline : Parser<Modifier> = pstr "_" |>> (fun _ -> Underline)
let paragraph : Parser<Modifier> = pstr "`|" |>> (fun _ -> Paragraph)
let linebreak : Parser<Modifier> = pstr "`" <|> pstr "|" |>> (fun _ -> Break)
let size : Parser<Modifier> = pbetween (pstr "=") (pstr "=") (pmany1 pdigit)|>> stringify |>> (fun e -> Size e)
let header : Parser<Modifier> = pbetween (pstr "~") (pstr "~") (pmany1 pdigit)|>> stringify |>> (fun e -> Header e)

// helper parser for potential Macro elements
let modifier = pstr "+" <|> pstr "*" <|> pstr "_" <|> (pmany1 pdigit |>> stringify)

// Macro definition parser; I know this is a comically long line, it was late at night gimme a break
let macro : Parser<Modifier> = pseq (pbetween (pbetween pws0 pws0 (pstr "MACRO")) (pstr ":=") (pbetween pws0 pws0 (pmany1 pletter) |>> stringify)) (pbetween pws0 pws0 (pmany1 modifier)) Macro

// Macro use parser
let macroPhrase : Parser<Modifier> = pbetween (pstr "#") (pstr "#") (pmany1 pletter |>> stringify) |>> (fun e -> MacroPhrase e)

// all the possible outcomes of an expr in this language
// check for special characters first to avoid everything being a word
let items = macroPhrase <|> macro <|> header <|> size <|> paragraph <|> linebreak <|> word <|> underline <|> bold <|> ital 

// A program is at least one of these expressions
exprImpl := pmany1 items 

// grammar to parse the end of file
let grammar = pleft expr peof


// parser function
let parse input = 
    let input' = prepare input
    match grammar input' with 
    | Success(res,_) -> Some res
    | Failure(pos, rule) -> 
        let msg = sprintf "Invalid expression at pos %d in rule '%s':" pos rule
        let diag = diagnosticMessage 20 pos input msg
        printf "%s" diag
        None

        
        



