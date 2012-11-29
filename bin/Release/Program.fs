(**
% LLiteFs : a language friendly literate programming tool
% Luca Bolognese
% 22/11/2012
**)

(**
Main ideas
==========

My interest in [literate programming](http://en.wikipedia.org/wiki/Literate_programming)
comes from some realizations on my part:

* When I go back to code that I have written some time ago, I don't remember my reasoning
* When I write a blog post, my code seems to be better. Perhaps explaining things to people encourages
   me to be more precise

Unhappiness with existing tools
-------------------------------

Many of the existing tools work similarly to [CWeb] (http://www-cs-faculty.stanford.edu/~uno/cweb.html).

* They have a tangle program that goes over your file and extract something that the compiler can understand
* They have a weave program that extracts from your file something that the document generator can understand

This scheme has the unfortunate limitation of breaking your code editor. Given
that your file is not a valid code file anymore, the editor starts misbehaving (i.e. intellisense breaks).
The debugger starts to get confused (albeit people tried to remediate that with cleaver use of `#line`.
If your language has an interactive console, that would not work either.

A more modern interpretation
----------------------------

The main idea of this program is to add your narrative (in markdown format) to the comment part of a
code file. This keeps editor and debugger working.

But simply doing so it's not enough. The code would become difficult to read
because of the need to clearly indicate which parts are code-parts. Also there are some other refactorings,
explained later, that needs to be applied for the sake of producing a pleasurable document.

Hence the weave phase as been retained and this is the program that go over your code file and extracts
a nicely formatted markdown file that can then be translated to HTML, PDF, latex, etc...

Multiprogramming, multi-document format
---------------------------------------

An attempt has be made to make the program work for any programming language and any documentation format,
with the former being more of a priority given tools like Pandoc that can translate easily between different
markup formats.

Some extensions to the standard markdown format has been used to produce nicer output (i.e. code blocks, titles, ...).
These work in Pandoc and probably many other markdown processor. It seems that the community is standardizing
on a useful superset of markdown.

This scheme assumes that there is a special comment tag that can be use to differentiate narrative content from
other content, for example:

* in F#, it is (* * .. * *) without the space in the middle
* in C, C++ /** .. *//
**)

(**
Language limitations
--------------------

One of the main tenets of literate programming is that the code should be written in the order that facilitates
exposition to a human reader, not in the order that makes the compiler happy. This is very important.

If you have written a code related blog post or tried to explain a codebase to a new joiner, you would agree
that you don't start from the top of the file and go down, but jump here and there trying to better explain
the main concepts. Literate programming says that you should write code the same way.
But in our scheme, the compiler needs to be kept happy because the literate file *is* the code file.

Some ingenuity is required to achieve such goal:

* In C and C++ you can forward declare functions and classes, also class members can be in any order
* In C#, Java, VB.NET, F# (the object oriented part) you can write class members in any order
* In the functional part of F# you do have a problem (see below)

An aside: forward declaring functions in F#
-------------------------------------------

You can achieve something somehow similar to forward declaration by this dirty trick.
**)

module LLiteFs

let declare<'a>  = ref Unchecked.defaultof<'a>

(**
Whenever you want to do a forward declaration of a function , or variable, you can type:
**)

let testDeclare() =

    let add = declare<float -> float>

    let ``function I want to explain that depends on add`` nums = nums |> Seq.map !add

(**
This creates a ref to a function from float to float. It looks a bit like an Haskell type declaration.
You can then use such function as if it were actually define and delay his definition to a later point
in time when you are ready to explain it.

When you are ready, you can then do:
**)

    add := fun x -> x + 1.

(**
And use it like any normal function.
**)

    printfn "%f" (!add 3.)

(**
The syntax is not too bad. You get that often-sought Haskell like explicit type declaration and you can
regex the codebase to create an index at the end of the program.

But is it too slow? After all, there is one more indirection call for each function call.

Let's test it: enable #time in F# interactive and execute timeNormalF and timeIndirectF varying sleepTime
and howManyIer until you convince yourself that it is ok.
**)

    let sleepTime   = 50
    let howManyIter = 100
    let normalF x   = System.Threading.Thread.Sleep sleepTime
    let indirectF   = declare<int -> unit>
    indirectF      := fun x -> System.Threading.Thread.Sleep sleepTime
     
    let timeNormalF     = [1..howManyIter] |> List.iter normalF
    let timeIndirectF   = [1..howManyIter] |> List.iter !indirectF
    ()

(**
There is a problem with all of the above: it doesn't work with generic functions and curried function invocations.
The code below works in all cases, but it is ugly for the user to use. In this program I've used the beautiful, but incorrect, syntax.
**)

type Literate() =
    static member Declare<'a, 'b>  (ref : obj ref) (x : 'a) : 'b = unbox <| (unbox<obj -> obj> !ref) x
    static member Define<'a, 'b> (func : 'a -> 'b) (ref : obj ref) (f : 'a -> 'b) = ref := box (unbox<'a> >> f >> box)


(******************)
    
let rec id (x : 'a) : 'a = Literate.Declare idImpl x
and idImpl = ref null

let f () = id 100 + id 200

Literate.Define id idImpl (fun x -> x)

let r = f()

(**
Implementation
==============
At the core, this program is a simple translator that takes some code text and return a valid markdown text.
There is a global concept of options that needs to be accessible from everywhere in the program and a general
concept of language that represent the language we are trying to parse.
**)

let language    = declare<string>
let options     = declare<Map<string, obj>>
let translate   = declare<string -> string>

(**
Translation passes
------------------

This is a summary of the translation passes currently implemented, we'll review each one when writing the code.

1. Check if the first block of comment is a title block, parse it
2. Follow all the links to assemble all the files in one document
3. [Manage comment annotations] (manage-comment-annotations)

I define narrative blocks (N) as the ones comprises inside the special start comment tags and end comment tag
(excluding such tags). Code blocks (C) are all the rest.

We need a function that takes a string and returns a list with the various blocks.
**)

type Block =
| Code      of string
| Narrative of string

let blockize = declare<string -> Block list>

(**
Program Parser
--------------

I could have used regular expressions to parse the program, but it seemed ugly. I could have used FsParsec,
but that brings with it an additional dll. So I decided to roll my own parser. This has two problems:

* It doesn't allow narrative comments inside comments, in particular it doesn't allow the opening comment
* It doesn't allow opening comments in the program code (not even inside a string)

The latter in particular is troublesome. You'll need to use a trick in the code (i.e. concatenating strings)
to full this program in not seeing an opening comment.
**)

(**
First we need to define options that help us in parsing different languages. We need to know which special
syntax each language uses to represent the opening and closing of a narrative comment.
**)

type Comments = {Opening : string; Closing : string }

options := [
            "fsharp_comments", {Opening = "(**"; Closing = "**)"} :> obj
            "c_comments"     , {Opening = "/**"; Closing = "**/"} :> obj
            "cpp_comments" , {Opening = "/**"; Closing = "**/"} :> obj
            "csharp_comments", {Opening = "/**"; Closing = "**/"} :> obj
            "java_comments"  , {Opening = "/**"; Closing = "**/"} :> obj
           ] |> Map.ofList

let getOpening () = ((!options |> Map.find (!language + "_comments")) :?> Comments).Opening |> List.ofSeq
let getClosing () = ((!options |> Map.find (!language + "_comments")) :?> Comments).Closing |> List.ofSeq

(**
TODO: review these algorithms for performance when large files are parsed
**)

let startsList commentTag l =
    let c   = System.String(List.toArray commentTag)
    let t   = System.String (List.toArray l)
    t.StartsWith c

let isOpening l =
    let o   = getOpening()
    startsList o l
    
let isClosing l =
    let o   = getClosing()
    startsList o l

let remainingOpen l =
    let o   = getOpening()
    if (List.length l) <= o.Length
        then []
        else l |> List.ofSeq |> Seq.skip o.Length |> Seq.toList  

let remainingClose l =
    let o   = getClosing()
    if (List.length l) <= o.Length
        then []
        else l |> List.ofSeq |> Seq.skip o.Length |> Seq.toList  
    
(**
This is a pretty standard lexer / parser combination. Unfortunately it doesn't manage errors in a graceful
way.
TODO: consider moving to FSParsec or add error management
**)

type Token =
| OpenComment
| CloseComment
| Text of string

let tokenize source =

    let rec text acc = function
        | t when isOpening t        -> acc, t 
        | t when isClosing t        -> acc, t
        | c :: t                    -> text (acc + c.ToString()) t
        | []                        -> acc, [] 
    let rec tokenize' acc = function
        | []                        -> List.rev acc
        | t when isOpening t        -> tokenize' (OpenComment::acc)  (remainingOpen t)
        | t when isClosing t        -> tokenize' (CloseComment::acc) (remainingClose t)
        | t                         ->
            let s, t'= text "" t
            tokenize' (Text(s) :: acc) t'

    tokenize' [] (List.ofSeq source)

type Chunk =
| NarrativeChunk    of Token list
| CodeChunk         of Token list

let parse source =

    let rec parseNarrative acc = function
        | OpenComment::t        -> failwith "You cannot have open narrative comments inside narrative comments"
        | CloseComment::t       -> acc, t
        | Text(s)::t            -> parseNarrative (Text(s)::acc) t
        | []                    -> failwith "You haven't closed your last narrative comment"

    let rec parseCode acc = function
        | OpenComment::t as t'  -> acc, t'
        | CloseComment::t       -> parseCode (CloseComment::acc) t
        | Text(s)::t            -> parseCode (Text(s)::acc) t
        | []                    -> acc, []
    let rec parse' acc = function
        | OpenComment::t    ->
            let narrative, t' = parseNarrative [] t
            parse' (NarrativeChunk(narrative)::acc) t' 
        | Text(s)::t        ->
            let code, t' = parseCode [Text(s)] t
            parse' (CodeChunk(code)::acc) t'
        | CloseComment::t   -> failwith "You inserted a close narrative comment at the start of your program"
        | []                -> List.rev acc

    parse' [] (List.ofSeq source)


(**
The flattening part of the algorithm is a bit unusual. At this point we have a parse tree, but we want
to reduce it to two simple node types containing all the text.
TODO: consider managing nested comments and comments in strings (the latter has to happen in earlier phases) 
**)
 
let flatten chunks =
    let tokenToStringNarrative = function
    | OpenComment | CloseComment    -> failwith "Narrative comments cannot be nested"
    | Text(s)                       -> s

    let tokenToStringCode = function
    | OpenComment                   -> failwith "Open narrative comment cannot be in code"
    | CloseComment                  -> "**)"
    | Text(s)                       -> s

    let flattenChunk = function
    | NarrativeChunk(tokens)             -> Narrative(tokens |> List.fold (fun state token -> state + tokenToStringNarrative token) "")
    | CodeChunk(tokens)                  -> Code(tokens |> List.fold (fun state token -> state + tokenToStringCode token) "")

    chunks |> List.fold (fun state chunk -> flattenChunk chunk :: state) [] |> List.rev

(**
We are getting there, now we have a list of blocks we can operate upon
**)

blockize := (tokenize >> parse >> flatten)        
 
(**
Narrative comments phases
-------------------------

We translate the list of blocks to an array to allow mutable changes without regenerating the data structure
**)

type Phase = Block array -> Block array

let processFirstBlock   = declare<Phase>
let processLastBlock    = declare<Phase>
let processMiddleBlocks = declare<Phase>

let processPhases l       = (List.toArray >> !processFirstBlock >> !processLastBlock >> !processMiddleBlocks) l

let codeStart ()        = "\n\n```" + !language + "\n\n"
let codeEnd   ()        =  "\n\n```\n\n"

let firstCodeStart  ()  = "```" + !language + "\n\n"
let lastCodeEnd     ()  = "\n\n```"

(**
We also want to manage how many newlines there are between different blocks, so we'll trim all newlines
from the start and end of a block, and then add our own.
**)

type System.String with
    member s.TrimNl () = s.Trim([|'\r';'\n'|]) 

(**
Managing the first block
------------------------

You are encouraged to use a comment block as your first block where you specify title, author and date like:  

% title
% author(s) (separated by semicolons)
% date

For the first block
    a. If it is a code block, insert ```LANG\n\n at the top
    b. If it is a comment block put \n\n```LANG\n\n at the end
  
**)

processFirstBlock := fun blocks ->
    if blocks.Length = 0
        then blocks
        else
            let newBlock =
                match blocks.[0] with
                | Code(text)        -> Code(firstCodeStart() + text.TrimNl())
                | Narrative(text)   -> Narrative(text.TrimNl() + codeStart ())
            blocks.[0] <- newBlock
            blocks

(**
Managing the last block
-----------------------

For the last block
    b. If it is a code block, insert \n\n```\n\n at the bottom
    a. If it is a comment block, at the start \n\n```\n\n

**)
processLastBlock := fun blocks ->
    if blocks.Length = 0
        then blocks
        else
            let lastIndex = blocks.Length - 1
            let newBlock =
                match blocks.[lastIndex] with
                | Code(text)        -> Code(text.TrimNl () + lastCodeEnd ())
                | Narrative(text)   -> Narrative(text.TrimNl())
            blocks.[lastIndex] <- newBlock
            blocks

(**
Managing the intermediate blocks
--------------------------------

For each intermediate block
    a. If it is a code block, do nothing
    b. If it is a comment block
        i. At the start, \n\n```\n\n
        ii. At the end \n\n```LANG\n\n
**)

processMiddleBlocks := fun blocks ->
    let lastIndex = blocks.Length - 1
    let fix = function
        | Code(text)        -> Code(text.TrimNl ())
        | Narrative(text)  -> Narrative(codeStart () + text.TrimNl () + codeEnd ())
    blocks |> Array.mapi (fun i b -> if i <> 0 && i <> lastIndex then fix b else b)

(**
Putting everything back together
--------------------------------

Once we have the array of blocks we need to flatten them, which is trivial and finally implement our
overall translate function.
**)

let sumBlock s b2 =
    let extract = function
        | Code(text)        -> text
        | Narrative(text)   -> text

    s + extract b2

let flattenB blocks = blocks |> Array.fold sumBlock ""

translate := !blockize >> processPhases >> flattenB

(**
Parsing command line arguments
------------------------------

Here is a generic command line parser taken from [here] (http://fssnip.net/8g).
**)

open  System.Text.RegularExpressions

// parse command using regex
// if matched, return (command name, command value) as a tuple
let (|Command|_|) (s:string) =
  let r = new Regex(@"^(?:-{1,2}|\/)(?<command>\w+)[=:]*(?<value>.*)$",RegexOptions.IgnoreCase)
  let m = r.Match(s)
  if m.Success
  then 
    Some(m.Groups.["command"].Value.ToLower(), m.Groups.["value"].Value)
  else
    None

// take a sequence of argument values
// map them into a (name,value) tuple
// scan the tuple sequence and put command name into all subsequent tuples without name
// discard the initial ("","") tuple
// group tuples by name 
// convert the tuple sequence into a map of (name,value seq)
let parseArgs (args:string seq) =
  args 
  |> Seq.map (fun i -> 
                    match i with
                    | Command (n,v) -> (n,v) // command
                    | _ -> ("",i)            // data
                  )
  |> Seq.scan (fun (sn,_) (n,v) -> if n.Length>0 then (n,v) else (sn,v)) ("","")
  |> Seq.skip 1
  |> Seq.groupBy (fun (n,_) -> n)
  |> Seq.map (fun (n,s) -> (n, s |> Seq.map (fun (_,v) -> v) |> Seq.filter (fun i -> i.Length>0)))
  |> Map.ofSeq

// return Some(value) if key is found, None otherwise
let (?) (m:Map<string,_>) (p:string) = 
  if Map.containsKey p m
  then Some(m.[p])
  else None

(**
Main method
-----------

We can now tie everything together.
**)

let banner  = "LLiteFs: LLiteFs : a language friendly literate programming tool\n"
let usage   = "llitefs language inputFile outputFile" +
              "where 'language' is one of fsharp, csharp, c, cplus, java" +
              "and 'outputfile' defaults to inputfile.mkd"

[<EntryPoint>]
let main args =
    try
        printfn "%s" banner

        let pArgs       = args |> parseArgs
        let ars         = pArgs.[""] |> Seq.toArray

        language        := ars.[0]
        let inputFile   = ars.[1]
        let outputFile  = if ars.Length < 3 then inputFile + ".mkd" else ars.[2]

        let input       = System.IO.File.ReadAllText inputFile
        let output      = !translate input
        System.IO.File.WriteAllText (outputFile, output)
        0
    with
    | e ->
        printfn "%s" usage
        printfn "%A" e
        -1