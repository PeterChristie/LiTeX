// (c) psc 2020
// a main method for LiTeX, bascially a large bowl of spaghetti


module CS334

open ProjectParser
open ProjectInterpreter
open System
open System.IO
open System.Diagnostics
open System.Linq


// repl functionality   
// prints ast, not the resulting HTML                             
let rec repl() =
    printf "Enter expression: " 
    let input = System.Console.ReadLine()   
    if input = "quit" then 
        printfn "bye!"
        exit 0
    else
        let ast_opt = parse input 
        match ast_opt with 
        |Some ast -> printfn "%A" ast
        |None -> ()
        repl()


// pre-processing for user input
// add header and footer HTML info to output file 
let sendToEval (ast: Modifier list) (outputFile: string) = 
    // took me 4 years to lookup how to do different colors in console
    let color = ConsoleColor.Green
    Console.ForegroundColor<-ConsoleColor.Green
    printf "\n"
    printf "Creating your HTML document..."
    // this map has a bool to keep track of how many times we have seen the macro
    // corresponds to open/closed HTML tags
    let userInput = eval ast 0 0 0 0 0 Map.empty<string, string list * bool>
    let headInput = "<!doctype html><html lang='en'><head><meta charset='utf-8'><title>" + outputFile + "</title></head><body><p>" + userInput
    let endHtml = "</p></body></html>"
    let output = headInput + endHtml
    File.WriteAllText(outputFile, System.String.Concat(output))
    Console.Beep()
    printf("...done!\n")
    printf("\n")
    Console.ResetColor()


// helper method to format line spacing
// certain lines should not produce a line break
let rec formatInput (x: String list) : String list = 
    match x with 
    |x::xs -> 
        if x = "" then 
            "|" :: formatInput xs 
        elif x.EndsWith("=") || x.EndsWith("~") || x.StartsWith("MACRO") then 
            x :: formatInputJank xs
        else 
            x + "`" :: formatInput xs 
    | [] -> []


// (yet another) helper method for string formatting
// workaround so paragraphs followed by new lines match spacing with input file
and formatInputJank (x: String list) : String list =
    match x with 
    |x::xs -> 
        if x = "" then
            "" :: formatInput xs
        elif x.EndsWith("=") || x.EndsWith("~") || x.StartsWith("MACRO") then
            x :: formatInputJank xs
        else 
            x + "`" :: formatInput xs
    |[] -> []


[<EntryPoint>]
let main argv =
    // filename given in command line
    if argv.Length = 2 then
        let inputFile = argv.[0]
        let outputFile = argv.[1]
        let text = File.ReadAllLines(inputFile) |> Array.toList
        let formattedInput = formatInput text |> List.fold (+) ""
        let ast_opt = parse formattedInput
        match ast_opt with 
            |Some ast -> sendToEval ast outputFile
            |None ->  Console.ResetColor()
        0

    // keep repl support
    elif argv.Length = 1 && argv.[0] = "repl" then
        repl()

    // print usage if user fails first time
    else
        printfn "Usage: dotnet run <input filename> <output filename> (or 'repl' to enter repl)"
        1
    


