# Syntax

Cherry's syntax is inspired by functional programming language such as Elm or
Haskell. For many deveopers, the syntax of those languages can end up being an
annoying hurdle to overcome before they can be productive. Cherry attempts to
remain familiar to JavaScript developers while adopting a clearer and simpler
syntax.

---

## Functions

Function application is denoted by juxtaposition in Cherry. This is in contrast
to JavaScript where function application requires wrapping arguments in parentheses
and separation with commas. This example in Cherry:

```
f x y
```

is equivalent to this example in JavaScript:

```js
f(x, y)
```

For many developers this can feel profoundly _wrong_. We're so used to see the
second style in mainstream languages such as C, Java, Rust, Go, ... that it can
feel incredibly alien when learning Cherry's style. This style isn't unique to
Cherry, though, and can be found in the majority of functional programming languages
such as Elm, Haskell, F#, and OCaml. 

Although we might argue that Cherry's syntax is clearer and less cluttered than
traditional C-style function application, there is also a practical reason for
this choice. 

In Cherry (and indeed, all of the languages we just listed that share this style),
functions are _curried_ by default. Instead of a function taking four arguments,
for example, it is actually a function that takes a single argument that returns
_another_ function that takes a single argument and so on, until all four arguments
have been applied and the function body can be evaluated.

```
fun foo = x y z => ...
```

might be written in JavaScript as:

```js
const foo = x => y => z => ...
```

This may seem inconsequential at first, but consider how this function would be
called in JavaScript:

```js
foo(a)(b)(c)
```

At this point, we may as well remove the parentheses as they aren't doing anything
useful anyway and what we're left with is Cherry's simple syntax:

```
foo a b c
```