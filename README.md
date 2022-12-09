# ren-lang/compiler

> **Move fast ~~and break things~~.** A pragmatic functional programming language for the modern Web.

<img src="https://raw.githubusercontent.com/ren-lang/assets/main/square-centre-primary.png" width="250" align="right">

This is the core compiler for the [Ren](https://ren-lang.github.io) programming language: by it's own it doesn't do much. If you want to write and run Ren code, you should take a look at the [playground](https://github.com/ren-lang/playground) for now instead. As this is likely one of the first repos you'll come across if you're looking into Ren, we've provided an overview of the language [below](#language-overview) so you can see what all the fuss is about!

**If you'd like to get involved or you have some questions, come and join the official Ren [discord server](https://discord.gg/Uv5tbwdqRA)**! You can also find us in the `#ren` channel on the /r/ProgrammingLanguages [discord server](https://discord.gg/4Kjt3ZE).

## Language Overview

Ren is a gradually typed functional programming that compiles to JavaScript. The JavaScript community is slowly moving towards adopting a more functional style of programming. Keeping functions pure, avoiding side effects, embracing immutable data structures: these are common practices in the world of JavaScript today, but it can feel like you're working against the language a little bit at times.

Ren is an attempt to tidy up JavaScript and shape it up into a productive, simple-to-use, and simple-to-understand programming language. It retains many of the features you might like from JavaScript, while removing many of the footguns and escape hatches. So let's see what we're dealing with:

```html
<script type="module" src="https://cdn.example.com/ren-web-compiler.mjs"></script>
<script type="application/ren">
  ...
</script>
```

Yep, you can start using Ren straight from a web page by just including a `<script>` tag!

Now that you've seen what Ren looks like, here's a list of some of the most interesting or notable features:

- Immutable-by-default.
- Expression-oriented.
- Gradual type system with...
  - ...algebraic data types
  - ...row polymorphism
  - ...opt-in for full safe static typing.
  - ...**no** manual type casts.
- Pattern matching with support for...
  - ...guards (`is [ x, y ] if x < y => ...`)
  - ...type patterns (`is @Number n => ...`)
  - ...string extraction (`` is `${protocol}://www.${domain}.${tld}` => ... ``).
- Simple JavaScript FFI.
- Automatic currying and partial application.
- A pipe operator (`|>`) and heavy emphasis on pipe-friendly APIs.
- Placeholder syntax to support...
  - ...positional piping (`y |> f x _ z`)
  - ...anonymous pattern matching (`where _ is ...`)
  - ...partial operator application (`_ < 10`).
- Compiles to readable modern JavaScript (ES6).

## Development

Ren is written in [**Gleam**](https://github.com/gleam-lang/gleam), another friendly functional programming language capable of compiling to both JavaScript and Erlang. The goal is to leverage both of Gleam's current targets, as well as a potential native target in the future, and as such the compiler core is written in pure Gleam.
