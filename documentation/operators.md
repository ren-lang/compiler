# Operators

Cherry has a handful of operators you will have seen a hundred times before, and
a couple that might be new to you. Many of these operators have a direct JavaScript
counterpart, but unlike JavaScript, operators in Cherry can be passed around and
used as functions. This makes it possible to write code like:

```js
Array.foldl (+) 0 [ 1, 2, 3, 4 ] 
//=> 10
```

When used this way, operators can even be partially applied like any other
function:

```js
Array.map ((*) 2) [ 1, 2, 3, 4 ] 
//=> [ 2, 4, 6, 8 ] 
```

---

## The Operator Table

| Operator  | Example       | Function          | JavaScript?       |
| --------- | ------------- | ----------------- | ----------------- |
| `\|>`     | `x \|> f`     | Function.pipe     |                   |
| `>>`      | `f >> g`      | Function.compose  |                   |
| `;`       | `x; y`        | Function.discard  | `,`               |
| `+`       | `1 + 2`       | Math.add          | `+`               |
| `-`       | `10 - 3`      | Math.sub          | `-`               |
| `*`       | `5 * 2`       | Math.mul          | `*`               |
| `/`       | `8 / 10`      | Math.div          | `/`               |
| `^`       | `2 ^ 10`      | Math.pow          | `**`              |
| `%`       | `7 % 2`       | Math.mod          | `%`               |
| `==`      | `x == y`      | Compare.eq        | `===`             |
| `!=`      | `x != y`      | Compare.notEq     | `!==`             |
| `<`       | `x < y`       | Compare.lt        | `<`               |
| `<=`      | `x <= y`      | Compare.lte       | `<=`              |
| `>`       | `x > y`       | Compare.gt        | `>`               |
| `>=`      | `x >= y`      | Compare.gte       | `>=`              |
| `&&`      | `x && y`      | Logic.and         | `&&`              |
| `\|\|`    | `x \|\| y`    | Logic.or          | `\|\|`            |
| `::`      | `x :: [y]`    | Array.cons        | `[ x, ...y ]`     |
| `++`      | `[x] ++ [y]`  | Array.join        | `[ ...x, ...y ]`  |

---

## The Pipe `(|>)`  Operator

The pipe operator was made popular by F# although it exists in many many
languages these days: Elm, Rust, Gleam, Elixir, ReasonML, and so on... In fact,
if you've spent much time in a UNIX shell then you've already come across the 
pipe operator, although there it is `|`.

The semantics are remarkably basic. It is a binary operator that takes the value
on its left hand side, and applies the function on the right hand side to it:

```
x |> f == f x
```

Although simple, the pipe operator can be a cause of some frustration when hopping
between languages. Not everyone has agreed on where to place the piped value when
it is being piped into a function that has multiple arguments. Consider this
example:

```
x |> f y
```

What should this be equivalent to; `f x y` or `f y x`? And here we have the difference
between **pipe-first** and **pipe-last**. Cherry, like Elm and FSharp adopts the
_pipe-last_ approach

---

## The Discard `(;)` Operator

Many of these operators behave as you'd expect them to (or at least, how you'd
expect them to behave in JavaScript). Cherry's semicolon operator, known as
`discard` has a curious parallel to JavaScript. Not many developers know about
JavaScript's commar operator `,`, but it is used to group multiple expressions in
places where there can typically be only one.

Consider this piece of JavaScript:

```js
const f = (x) => {
    return Console.log(x), x
}
```

Typically, `return` expects a single expression the follow it but the comma 
operator is used here to provide _two_ expressions. This snippet is largely
nonsensical in JavaScript, the language has statements and so we could just as
easily put the console log _before_ the return statement and just return `x`.
Cherry however, is an _expression-oriented_ language. We don't have statements
at all!

With Cherry's discard operator, `;`, we can emulate a style of programming that
many JavaScript developers may already be used to, while still adhering to the
expression-oriented semantics of the language.

That same snippet of JavaScript looks like this in Cherry:

```
fun f = x =>
    Console.log x;
    x
```

We dropped the parentheses and `return`, but by-and-large we have a pretty
convincing approximation of what we might write in JavaScript:

```js
const f = x => {
    console.log(x);
    return x
}
```

All that is to say the discard operator is a way of smoothing the transition to
expression-oriented programming. Over time, you may find you don't need to use
it much at all!