// (c) psc 2020
// an interpreter for LiTeX, a language of questionable use cases and construction

module ProjectInterpreter

open ProjectParser
open System
open System.IO
open System.Diagnostics


// method to apply stored modifiers from a macro
// b should be true the first time we see the macro name
let rec evalMacroPhrase (var: string)(modifiers: string list * bool)(env: Map<string, string list * bool>) : string = 
    match modifiers with 
    |m::ms,b -> 
        if b then 
            match m with 
            | "+" -> "<b>" + evalMacroPhrase var (ms,b) env 
            | "*" -> "<i>" + evalMacroPhrase var (ms,b) env 
            | "_" -> "<u>" + evalMacroPhrase var (ms,b) env
            // if the given int is less than 7, it must be a header (font size of <7 seems infreqeunt)
            | _ -> 
                let size = m |> int
                if size < 7 then
                    "<h" + m + ">" + evalMacroPhrase var (ms,b) env
                else 
                    "<p style='font-size:" + m + "px'>" + evalMacroPhrase var (ms,b) env

        else 
            match m with 
            | "+" -> "</b>" + evalMacroPhrase var (ms,b) env 
            | "*" -> "</i>" + evalMacroPhrase var (ms,b) env 
            | "_" -> "</u>" + evalMacroPhrase var (ms,b) env 
            // if the given int is less than 7, it must be a header (font size of <7 seems infreqeunt)
            |_ -> 
                let size = m |> int
                if size < 7 then
                    "</h" + m + ">" + evalMacroPhrase var (ms,b) env
                else 
                    "</p>" + evalMacroPhrase var (ms,b) env
    |[],b -> ""


// evaluator method for the language
// integers refer to whether we have seen the modifier or not -> open/closed html tags
// recursive method; continues to build a string one ast node at a time 
let rec eval (e: Modifier list)(b: int)(i: int)(u: int)(h: int)(s: int)(env: Map<string, string list * bool>) : string = 
    match e with 
    | Word e :: f -> e + eval f b i u h s env
    | Bold :: f -> 
            if b = 0 then 
                "<b>" + eval f 1 i u h s env
            else 
                "</b>" + eval f 0 i u h s env
    | Ital :: f -> 
            if i = 0 then 
                "<i>" + eval f b 1 u h s env
            else 
                "</i>" + eval f b 0 u h s env
    | Underline :: f ->  
            if u = 0 then 
                "<u>" + eval f b i 1 h s env
            else 
                "</u>" + eval f b i 0 h s env
    | Paragraph :: f -> "</p><p>" + eval f b i u h s env
    | Break :: f -> "<br>" + eval f b i u h s env
    | Header e :: f ->
            if h = 0 then 
                "<h" + e + ">" + eval f b i u 1 s env
            else 
                "</h" + e + ">" + eval f b i u 0 s env
    | Size e :: f ->    
            if s = 0 then 
                "<p style='font-size:" + e + "px'>" + eval f b i u h 1 env
            else 
                "</p>" + eval f b i u h 0 env
    | Macro (var,xs) :: f ->  
            let env' = Map.add var (xs,false) env
            eval f b i u h s env'
    | MacroPhrase var :: f -> 
            // change the bool value stored in env 
            if Map.containsKey var env then
                let (mods, bo) = Map.find var env
                let env' = Map.remove var env 
                let env'' = Map.add var (mods, not bo) env'
                let (newMods, bo) = Map.find var env''
                evalMacroPhrase var (newMods, bo) env'' + eval f b i u h s env''
            else 
                failwith "Macro variable not recognized." 
    | [] -> ""