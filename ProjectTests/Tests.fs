namespace ProjectTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser  
open ProjectInterpreter
open CS334

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ValidProgramReturnsASTList() =
        let input = "This is a valid LiTeX program. No really, it is."
        let expected = [Word "This is a valid LiTeX program. No really, it is."]
        let result = parse input
        match result with 
        | Some ast -> 
            Assert.AreEqual(expected, ast)
        | None -> 
            Assert.IsTrue false


    [<TestMethod>]
    member this.ValidASTReturnsHTMLFile () = 
        let input = [Word "This is a valid LiTeX program. No really, it is."; Break]
        let expected = "This is a valid LiTeX program. No really, it is.<br>"
        let result = eval input 0 0 0 0 0 Map.empty<string, string list * bool>
        match result with 
        | html -> 
            Assert.AreEqual(expected, html)
        