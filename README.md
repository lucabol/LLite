% LLiteFs : language friendly literate programming
% Luca Bolognese
% 22/11/2012


Main ideas
==========

My interest in [literate programming](http://en.wikipedia.org/wiki/Literate_programming)
comes from some realizations on my part:

* When I go back to code that I have written some time ago, I don't remember my reasoning
* When I write a blog post, my code seems to be better. Perhaps explaining things to people encourages
  me to be more precise
* I like to think top down, but the compiler forces me to write code bottom up, starting from details and
  going to higher level concepts

Unhappiness with existing tools
-------------------------------

Many of the existing literate programming tools work similarly to the original [CWeb](http://www-cs-faculty.stanford.edu/~uno/cweb.html).

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

Hence the weave phase as been retained and what you are reading is the program that go over your code file and extracts
a nicely formatted markdown file that can then be translated to HTML, PDF, latex, etc...

*So the document you are reading is the program itself.*

Multiprogramming, multi-document format
---------------------------------------

An attempt has be made to make the program work for any programming language and any documentation format,
with the former being more of a priority given that tools like [Pandoc](http://johnmacfarlane.net/pandoc/) can translate easily between different
markup formats.

Some extensions to the standard markdown format have been used to produce nicer output (i.e. code blocks, titles, ...).
These work in Pandoc and probably many other markdown processor. It seems that the community is standardizing
on a useful superset of markdown.

This scheme assumes that there is a special comment tag that can be use to differentiate narrative content from
other content, for example:

* in F#, it is (* * .. * *) without the space in the middle
* in C, C++ /** .. *//


Language limitations
--------------------

One of the main tenets of literate programming is that the code should be written in the order that facilitates
exposition to a human reader, not in the order that makes the compiler happy. This is very important.

If you have written a blog post or tried to explain a codebase to a new joiner, you must have noticed
that you don't start from the top of the file and go down, but jump here and there trying to better explain
the main concepts. Literate programming says that you should write code the same way.
But in our version of it, the compiler needs to be kept happy because the literate file *is* the code file.

Some ingenuity is required to achieve such goal:

* In C and C++ you can forward declare functions and classes, also class members can be in any order
* In C#, Java, VB.NET, F# (the object oriented part) you can write class members in any order
* In the functional part of F# you do have a problem (see below)

An aside: forward declaring functions in F#
-------------------------------------------

You can achieve something somehow similar to forward declaration by this dirty trick.

```fsharp
module LLiteFs

let declare<'a>  = ref Unchecked.defaultof<'a>
```

Whenever you want to do a forward declaration of a function , or variable, you can type:

```fsharp
let testDeclare() =

    let add = declare<float -> float>

    let ``function I want to explain that depends on add`` nums = nums |> Seq.map !add
```

This creates a ref to a function from float to float. It looks a bit like an Haskell type declaration.
You can then use such function as if it were actually define and delay his definition to a later point
in time when you are ready to explain it.

When you are ready, you can then do:

```fsharp
    add := fun x -> x + 1.
```

And use it like any normal function.

```fsharp
    printfn "%f" (!add 3.)
```

The syntax is not too bad. You get that often-sought Haskell like explicit type declaration and you can
regex the codebase to create an index at the end of the program (not done yet).

But is it too slow? After all, there is one more indirection call for each function call.

Let's test it: enable #time in F# interactive and execute timeNormalF and timeIndirectF varying sleepTime
and howManyIer until you convince yourself that it is ok.

```fsharp
    let sleepTime   = 50
    let howManyIter = 100
    let normalF x   = System.Threading.Thread.Sleep sleepTime
    let indirectF   = declare<int -> unit>
    indirectF      := fun x -> System.Threading.Thread.Sleep sleepTime
     
    let timeNormalF     = [1..howManyIter] |> List.iter normalF
    let timeIndirectF   = [1..howManyIter] |> List.iter !indirectF
    ()
```

Unfortunately, there is a big problem with all of the above: it doesn't work with generic functions and curried function invocations.
The code below works in all cases, but it is ugly for the user to use. In this program I've used the beautiful, but incorrect, syntax.

```fsharp
type Literate() =
    static member Declare<'a, 'b>  (ref : obj ref) (x : 'a) : 'b =
        unbox <| (unbox<obj -> obj> !ref) x
    static member Define<'a, 'b> (func : 'a -> 'b) (ref : obj ref) (f : 'a -> 'b) =
        ref := box (unbox<'a> >> f >> box)

////////////////////////////////////
    
let rec id (x : 'a) : 'a = Literate.Declare idImpl x
and idImpl = ref null

let f () = id 100 + id 200

Literate.Define id idImpl (fun x -> x)

let r = f()
```

Implementation
==============

At the core, this program is a simple translator that takes some code text and return a valid markdown/whatever text.
We need to know:

* The strings that start and end a narrative comment (input symbols)
* How to translate a code block into a document. We support these variations:
    * Indented: indent them by N spaces
    * Surrounded by startCode/endCode strings

```fsharp
type CodeSymbols =
    | Indent of int                 // indentation level in whitespaces
    | Surrounded of string * string // start code * end code

type Options = {
    startNarrative  : string
    endNarrative    : string
    codeSymbols     : CodeSymbols
}

let translate   = declare<Options -> string -> string>
```

Translation passes
------------------

We need a function that takes a string and returns a list with the various blocks, so that we can then play with them.

```fsharp
type Block =
| Code      of string
| Narrative of string

let blockize = declare<Options -> string -> Block list>
```

Program Parser
--------------

I could have used regular expressions to parse the program, but it seemed ugly. I could also have used FsParsec,
but that brings with it an additional dll. So I decided to roll my own parser. This has several problems:

* It doesn't report errors nicely
* It is probably very slow
* It doesn't allow narrative comments inside comments, in particular it doesn't allow the opening comment
* It doesn't allow opening comments in the program code (not even inside a string)

The latter in particular is troublesome. You'll need to use a trick in the code (i.e. concatenating strings)
to foul this program in not seeing an opening comment, but it is inconvenient.

With all of that, it works for the sake of trying out this style of programming.


For the sake of testing, we are going to define some default fsharp options.

```fsharp
let fsharpOptions = {
    startNarrative  = "(*" + "*"
    endNarrative    = "*" + "*)"
    codeSymbols     = Surrounded("`" + "``fsharp", "``" + "`")
    }
```

The lexer is going to process list of characters. We need functions to check if a list of characters starts
with certain chars and to return the remaing list after having removed such chars.

BTW: these functions are polymorphic and could be used for other types as well

```fsharp
let rec startWith startItems listToCheck =
    match startItems, listToCheck with
    | [], _             -> true
    | _ , []            -> false
    | h1::t1, h2::t2  when h1 = h2  -> startWith t1 t2
    | _, _              -> false

let rec remove itemsToRemove listToModify =
    match itemsToRemove, listToModify with
    | [], l             -> l
    | _ , []            -> failwith "Remove not defined on an empty list"
    | h1::t1, h2::t2  when h1 = h2  -> remove t1 t2
    | _, _              -> failwith "itemsToRemove are not in the list"

let isOpening options       = startWith (List.ofSeq options.startNarrative) 
let isClosing options       = startWith (List.ofSeq options.endNarrative)
let remainingOpen options   = remove (List.ofSeq options.startNarrative)
let remainingClose options  = remove (List.ofSeq options.endNarrative)
```

This is a pretty baic lexer / parser combination.

TODO: consider moving to FSParsec or add error management

```fsharp
type Token =
| OpenComment
| CloseComment
| Text of string

let tokenize options source =

    let rec text acc = function
        | t when isOpening options t    -> acc, t 
        | t when isClosing options t    -> acc, t
        | c :: t                        -> text (acc + c.ToString()) t
        | []                            -> acc, [] 
    let rec tokenize' acc = function
        | []                            -> List.rev acc
        | t when isOpening options t    -> tokenize' (OpenComment::acc)  (remainingOpen options t)
        | t when isClosing options t    -> tokenize' (CloseComment::acc) (remainingClose options t)
        | t                         ->
            let s, t'= text "" t
            tokenize' (Text(s) :: acc) t'

    tokenize' [] (List.ofSeq source)

type Chunk =
| NarrativeChunk    of Token list
| CodeChunk         of Token list

let parse options source =

    let rec parseNarrative acc = function
        | OpenComment::t        ->
            failwith "Don't open narrative comments inside narrative comments"
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
        | CloseComment::t   ->
            failwith "Don't insert a close narrative comment at the start of your program"
        | []                -> List.rev acc

    parse' [] (List.ofSeq source)
```

The flattening part of the algorithm is a bit unusual. At this point we have a parse tree that contains tokens, but we want
to reduce it to two simple node types containing all the text in string form.
TODO: consider managing nested comments and comments in strings (the latter has to happen in earlier phases) 

```fsharp
 
let flatten options chunks =
    let tokenToStringNarrative = function
    | OpenComment | CloseComment    -> failwith "Narrative comments cannot be nested"
    | Text(s)                       -> s

    let tokenToStringCode = function
    | OpenComment                   -> failwith "Open narrative comment cannot be in code"
    | CloseComment                  -> string(options.endNarrative |> Seq.toArray)
    | Text(s)                       -> s

    let flattenChunk = function
    | NarrativeChunk(tokens)             ->
        Narrative(tokens |> List.fold (fun state token -> state + tokenToStringNarrative token) "")
    | CodeChunk(tokens)                  ->
        Code(tokens |> List.fold (fun state token -> state + tokenToStringCode token) "")

    chunks |> List.fold (fun state chunk -> flattenChunk chunk :: state) [] |> List.rev
```

We are getting there, now we have a list of blocks we can operate upon

```fsharp
blockize := fun options source -> source |> tokenize options |> parse options |> flatten options
 
```

Narrative comments phases
-------------------------

We need to process all the blocks by adding all the code tags in the right places and removing all empty blocks.

```fsharp
type Phase = Options -> Block List -> Block List

let removeEmptyBlocks   = declare<Phase>
let mergeBlocks         = declare<Phase>
let addCodeTags         = declare<Phase>

let processPhases options blockList = 
    blockList
    |> !removeEmptyBlocks options
    |> !mergeBlocks options
    |> !addCodeTags options
```

We want to manage how many newlines there are between different blocks, so we'll trim all newlines
from the start and end of a block, and then add our own.

```fsharp
let NL = System.Environment.NewLine

let newLines = [|'\n';'\r'|]

type System.String with
    member s.TrimNl () = s.Trim(newLines) 
```

Remove the empty blocks
-----------------------

There might be empty blocks in the file. For the sake of formatting them correctly, we want to remove them.

```fsharp
let extract = function
    | Code(text)        -> text
    | Narrative(text)   -> text

removeEmptyBlocks := fun options blocks ->
                        blocks |> List.filter (fun b -> (extract b).TrimNl().Trim() <> "")
```

Merge blocks
------------

Consecutive blocks of the same kind need to be merged so as not to introduce empty blocks in the chain.
TODO: make tail recursive 

```fsharp
let rec mergeBlockList = function
    | []        -> []
    | [a]       -> [a]
    | h1::h2::t -> match h1, h2 with
                   | Code(t1), Code(t2)             -> mergeBlockList (Code(t1 + NL + t2)::t)
                   | Narrative(n1), Narrative(n2)   -> mergeBlockList(Narrative(n1 + NL + n2)::t)
                   | _, _                           -> h1::mergeBlockList(h2::t)

mergeBlocks := fun options blocks -> mergeBlockList blocks
```

Adding code tags
----------------

Each code block needs a tag at the start and one at the end or it needs to be indented by N chars

```fsharp
let indent n (s:string) =
    let pad = String.replicate n " "
    pad + s.Replace(NL, NL + pad)

addCodeTags := fun options blocks ->
    match options.codeSymbols with
    | Indent(n)         ->
        blocks |> List.map (function Narrative(s) as nar -> nar | Code(s) -> Code(indent n s))
    | Surrounded(s, e)  -> 
        blocks |> List.map (function
                            | Narrative(text)   -> Narrative(NL + text.TrimNl() + NL)
                            | Code(text)        -> Code(NL + s + NL + text.TrimNl() + NL + e + NL))
```

Putting everything back together
--------------------------------

Once we have the array of blocks we need to flatten them, which is trivial and finally implement our
overall translate function.

```fsharp
let sumBlock s b2 = s + extract b2

let flattenB blocks = (blocks |> List.fold sumBlock "").TrimStart(newLines)

translate := fun options text -> text |> !blockize options |> processPhases options |> flattenB
```

Parsing command line arguments
------------------------------

Parsing command lines involves writing a function that goes from a sequence of strings to an input file name, output file name and Options record

```fsharp
let parseCommandLine = declare<string array -> string * string * Options>
```

To implement it, we are going to use a command line parser taken from [here](http://fssnip.net/8g). The parseArgs function takes a sequence of argument values
and map them into a (name,value) tuple. It scans the tuple sequence and put command name into all subsequent tuples without name and discard the initial ("","") tuple.
It then groups tuples by name and converts the tuple sequence into a map of (name,value seq)

```fsharp
open  System.Text.RegularExpressions

let (|Command|_|) (s:string) =
  let r = new Regex(@"^(?:-{1,2}|\/)(?<command>\w+)[=:]*(?<value>.*)$",RegexOptions.IgnoreCase)
  let m = r.Match(s)
  if m.Success
  then 
    Some(m.Groups.["command"].Value.ToLower(), m.Groups.["value"].Value)
  else
    None


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

let paramRetrieve (m:Map<string,_>) (p:string) = 
  if Map.containsKey p m
  then Some(m.[p])
  else None
```

Now we need a function that goes from the map of command line parameters to the input file name, output file name and options. With that we
can finally define parseCommandLine. For that, we need functions to give us the default value for parameters. 

```fsharp
let langParamsTable     = [ "fsharp", ("(*" + "*", "*" + "*)")
                            "c", ("/**", "**/")
                            "csharp", ("/**", "**/")
                            "java", ("/**", "**/")] |> Map.ofList

let getLangNoNC lang    =
    match Map.tryFind lang langParamsTable with
    | Some(no, nc) -> no, nc
    | None -> failwith (lang + " is not a valid programming language")

let safeHead errMsg s = if s |> Seq.isEmpty then failwith errMsg else s |> Seq.head 

let paramsToInputs paramsMap =
    let single p er     = match paramRetrieve paramsMap p with | Some(k) -> Some(k |> safeHead er)
                                                               | None -> None
    let get p s         = match paramRetrieve paramsMap p with |Some(k) -> k |> safeHead s
                                                               | None -> failwith s
    let defaultP p q er = match paramRetrieve paramsMap p with | Some(k) -> k |> safeHead er
                                                               | None -> q

    let inputFile       = get "" "You need to pass an input file"
    let outputFile      = defaultP  "o"
                                    (System.IO.Path.GetFileNameWithoutExtension(inputFile) + ".mkd")
                                    "You must pass a parameter to -o"

    let no, nc          = match single "l" "You must pass a language parameter to -l" with
                          | Some(l) -> getLangNoNC l
                          | None    ->
                                get "no" "The no (narrative open) parameter is mandatory, if no -l specified",
                                get "nc" "The nc (narrative close) parameter is mandatory, if no -l specified"

    let codeSymbs       = match single "indent" "You must pass a whitespace indentation number to -indent" with
                          | Some(n)     ->
                                let success, value = System.Int32.TryParse n
                                if success
                                    then Indent(value)
                                    else failwith "-i accepts just an integer value as parameter"                          
                          | None        ->
                                Surrounded(
                                    get "co" "The co (code open) parameter is mandatory, if no -indent specified",
                                    get "cc" "The cc (code close) parameter is mandatory")
    inputFile, outputFile, {
        startNarrative  = no
        endNarrative    = nc
        codeSymbols     = codeSymbs
        }

parseCommandLine := parseArgs >> paramsToInputs
```

Main method
-----------

We need a banner and a usage text to print out in case of error.

```fsharp
let banner  = "LLiteFs : language friendly literate programming\n"
let usage   = @"
Usage: llitefs inputFile parameters
where:
One of the following two sets of parameters is mandatory
    -no string : string opening a narrative comment
    -nc string : string closing a narrative comment
or
    -l language: where language is one of (fsharp, csharp, c, java)

One of the following two sets of parameters is mandatory
    -co string : string opening a code block
    -cc string : string closing a code block
or
    -indent N  : indent the code by N whitespaces

The following parameters are optional:
    -o outFile : defaults to the input file name with mkd extension"
```

We can then write main as the only side effect function in the program.

```fsharp
let myMain args =
    try
        printfn "%s" banner

        let inputFile, outputFile, options = !parseCommandLine args
        let input       = System.IO.File.ReadAllText inputFile
        let output      = !translate options input
        System.IO.File.WriteAllText (outputFile, output)
        0
    with
    | e ->
        printfn "%s" "Failure"
        printfn "%s" e.Message 
        printfn "%s" usage
#if DEBUG 
        printfn "\nDetailed Error Below:\n%A" e
#endif
        -1
```
