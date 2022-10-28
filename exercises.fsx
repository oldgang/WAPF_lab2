let SHOW x = printf "%A\n" x
// ### Exercise 2.1
// ## Implement `parseNumber` function.
// ## You might find following functions useful:
// ## `ToCharArray()` (String member), `Array.forall`, `System.Char.IsDigit`, `System.Int32.Parse`.

// #### --------------- Your code goes below ---------------
let parseNumber (value: string): int option = 
    let chars = value.ToCharArray()
    if(Array.forall(System.Char.IsDigit) chars) then
        Some(System.Int32.Parse(value))
    else
        None
let ``exercise 2.1`` = parseNumber "42"

// #### Value of ``exercise 2.1``

SHOW ``exercise 2.1``

// ### Exercise 2.2
// ## Declare `splitBy` function - a wrapper function arround `Split` method from `String` object.
// ## Hints: Use `Split` method from `String` and `Array.toList` function to convert array to list type.
// #### --------------- Your code goes below ---------------
let splitBy (separator: char) (str: string): string list =
    str.Split(separator)
    |> Array.toList

let ``exercise 2.2`` = "1,3,5,8,10" |> splitBy ','

// #### Value of ``exercise 2.2``
SHOW ``exercise 2.2``

// ### Exercise 3.1
// ##Define `Operator` and `Symbol` Discriminated Union Types.
// ##
// ##`Symbol` should use `Operator` as field in one case
// ##

// #### --------------- Your code goes below ---------------
// `Int` is used here only so that the code compiles.
// Remove it and instead define proper Discriminated Union cases:
// Operator might be one of the following: Plus, Minus, Multiply or Divide
type Operator = 
| Plus
| Minus
| Multiply
| Divide
| LeftParenthesis
| RightParenthesis

// Same as above:
// Symbol might be either a NumSymbol (with int) or OpSymbol (with Operator)

type Symbol = 
| NumSymbol of int
| OpSymbol of Operator

// ### Exercise 3.2
// # With help of pattern matching, implement `apply` function.

// #### --------------- Your code goes below ---------------
let apply (operator: Operator) (left: int) (right: int): int = 
    match operator with 
    | Plus -> left + right
    | Minus -> left - right
    | Multiply -> left * right
    | Divide -> left / right
    | LeftParenthesis -> failwith "Not Implemented"
    | RightParenthesis -> failwith "Not Implemented"

// test the function, e.g. `apply Divide 15 4`
let ``exercise 3.2`` = apply Multiply 15 4

// #### Value of ``exercise 3.2``
SHOW ``exercise 3.2``

// ### Exercise 3.3
// ##Implement `parseSymbol` - try parse all operators first, and then in nested `match` expression use `parseNumber` function

// #### --------------- Your code goes below ---------------
let parseSymbol (token: string): Symbol option = 
    match token with
    | "+" -> Some(OpSymbol Plus)
    | "-" -> Some(OpSymbol Minus)
    | "*" -> Some(OpSymbol Multiply)
    | "/" -> Some(OpSymbol Divide)
    | "(" -> Some(OpSymbol LeftParenthesis)
    | ")" -> Some(OpSymbol RightParenthesis)
    | x -> 
        match parseNumber(x) with
        | Some(int) -> Some(NumSymbol int)
        | None -> None

let ``exercise 3.3`` =
    List.map parseSymbol [ "+"; "/"; "12"; "uups" ]
// #### Value of ``exercise 3.3``
SHOW ``exercise 3.3``

// ### Helper function "sequenceOpts"
// ##if all elements are Some values, return Some of those values
// ##otherwise if there's at least one None, return None

let rec sequenceOpts (optionals: 'a option list): 'a list option =
    match optionals with
    | [] -> Some []
    | None :: _ -> None
    | Some h :: t -> sequenceOpts t |> Option.map (fun t -> h :: t)

// ### Exercise 3.4
// ##Implement `parseSymbols`. Useful functions: `List.map`, `sequenceOpts` as well as `splitBy` and `parseSymbol`

// #### --------------- Your code goes below ---------------
let parseSymbols (expression: string): Symbol list option = 
    splitBy ' ' expression
    |> List.map parseSymbol
    |> sequenceOpts

let ``exercise 3.4`` = "1 2 / +" |> parseSymbols

// #### Value of ``exercise 3.4``
SHOW ``exercise 3.4``

// ### Homework 4.1
// ##Implement `computeonp` function (AiSD or [Wiki](https://pl.wikipedia.org/wiki/Odwrotna_notacja_polska)). Hint: `::` is "right-associative"

// #### --------------- Your code goes below ---------------
let rec computeonp (stack: int list) (symbols: Symbol list): int option = 
    match symbols with
    | [] -> 
        match stack with
        | [ result ] -> Some(result)
        | _ -> None
    | NumSymbol number :: tail ->  computeonp (number :: stack) tail
    | OpSymbol op :: tail -> 
        match stack with
        | a :: b :: tail2 -> computeonp ((apply op b a) :: tail2) tail
        | _ -> None

// test the function, e.g. `computeonp [] [NumSymbol 4; NumSymbol 2; OpSymbol Multiply]`
// Important!!!! Replace the None with commented out assignment o computeonp
let ``exercise 4.1``: int option = computeonp [] [NumSymbol 4; NumSymbol 2; OpSymbol Multiply]

// #### Value of ``exercise 4.1``
SHOW ``exercise 4.1``

// ### Homework 4.2
// ##Using `parseSymbols` and `compute`, write `onp` function

// #### --------------- Your code goes below ---------------
let onp (expression: string): int option = 
    match parseSymbols expression with
    | Some(symbols) -> computeonp [] symbols
    | _ -> None

let ``exercise 4.2`` = onp "2 7 + 3 / 14 3 - 4 * + 3 +"

// #### Value of ``exercise 4.2``
SHOW ``exercise 4.2``

// ### Homework 4.3
// ##Implement `conv2onp` function (AiSD or (https://pl.wikipedia.org/wiki/Odwrotna_notacja_polska)).

// #### --------------- Your code goes below ---------------
let conv2onp (expression: string): Symbol list option = 
    let rec conv2onp' (result: Symbol list) (stack: Operator list) (symbols: Symbol list) : Symbol list option =
        match symbols, stack with
        | NumSymbol number :: tail, _ -> conv2onp' (result @ [ NumSymbol number ]) stack tail
        | OpSymbol LeftParenthesis :: tail, _ -> conv2onp' result (LeftParenthesis :: stack) tail
        | OpSymbol RightParenthesis :: tail, LeftParenthesis :: tail2 -> conv2onp' result tail2 tail
        | OpSymbol RightParenthesis :: _, op :: tail2 -> conv2onp' (result @ [ OpSymbol op ]) tail2 symbols
        | OpSymbol op1 :: _, op2 :: tail2 when compare op2 op1 >= 1 ->
            conv2onp' (result @ [ OpSymbol op2 ]) tail2 symbols
        | OpSymbol op1 :: tail, _ -> conv2onp' result (op1 :: stack) tail
        | [], _ -> Some(result @ (List.map OpSymbol stack))

    match parseSymbols expression with
    | Some symbols -> conv2onp' [] [] symbols
    | None -> None

let ``exercise 4.3`` = conv2onp "( 2 + 5 ) * 3 - 4 * ( 16 + 5 )"

// #### Value of ``exercise 4.3``

SHOW ``exercise 4.3``

// ### Homework 4.4

// #### --------------- Your code goes below ---------------
let compute (expression: string): int option = None

let ``exercise 4.4`` =
    compute "( 2 + 5 ) * 3 - 4 * ( ( 16 - 1 ) * 2 + 5 )"

// #### Value of ``exercise 4.4``
SHOW ``exercise 4.4``

