# ren-lang/compiler

<img src="https://raw.githubusercontent.com/ren-lang/assets/main/square-centre-primary.png" width="250" align="right">

> **A pragmatic functional programming language for the modern Web.**

This is the compiler for the [Ren](https://ren-lang.github.io) programming language, distributed as an Elm package. If you want to write and run Ren code, you should take a look at the [playground](https://github.com/ren-lang/playground) for now instead. As this is likely one of the first repos you'll come across if you're looking into Ren, we've provided an overview of the language [below](#language-overview) so you can see what all the fuss is about! There's also a [reference module](./reference/syntax.ren) that serves as a brief walkthrough of Ren's syntax and features.

**If you'd like to get involved or you have some questions, come and join the official Ren [discord server](https://discord.gg/Uv5tbwdqRA)**! You can also find us in the `#ren` channel on the /r/ProgrammingLanguages [discord server](https://discord.gg/4Kjt3ZE).

## What is this package for?

It's often useful to have the parser, emitter, or entire compiler available as an API or package. Third-party programs like editor tooling, static analysis, or code generators can call out to relevant parts of the compiler without reinventing the wheel. In the Elm community, tools like `elm-review` and `elm-analyse` all depend on another package, `elm-syntax`, but this package is not official and may be wrong or out of date compared to the actual language implementation!

By providing the compiler and its guts as a package, we can hopefully encourage the community to experiment by creating all sorts of tooling and utilities without having to reinvent the wheel.

## How do I use this package?

This is an **Elm** package, so first and foremost make sure you have Elm installed. Then, all you need to do is install this like you would any other package:

```
$ elm install ren-lang/compiler
```

Writing CLIs and interfacing with things like the filesystem can be a bit awkward in Elm, as there are no native libraries for it and the JavaScript FFI is a bit different compared to most other languages. If you're stuck on what to do next you could take a look at the [Advent of Code](https://github.com/ren-lang/aoc) repo as a starting point, it's an Elm app too!

---

## Language Overview

Ren is a gradually typed functional programming that compiles to JavaScript. The JavaScript community is slowly moving towards adopting more and more functional programming features and idioms. Keeping functions pure, avoiding side effects, embracing immutable data structures: these are common practices in the world of JavaScript today, but it can feel like you're working against the langauge a little bit at times.

Ren is an attempt to tidy up JavaScript and shape it up into a productive, simple to use, and simple to understand programming language. It retains many of the features you might like from JavaScript, while removing many of the footguns and escape hatches. So let's see what we're dealing with:

```html
<script type="module" src="https://cdn.jsdelivr.com/gh/ren-lang/web-compiler/dist/compiler.js"></script>
<script type="application/ren">
  import "ren/stdlib/console" as Console
  import "ren/stdlib/random" as Random
  import "ren/stdlib/time" as Time
  
  type Platform
    = #browser
    | #cdn
    | #npm
  
  let playground : String = "https://ren-lang.github.io/playground"
  let cdn : String = "https://cdn.jsdelivr.com/gh/ren-lang/web-compiler/dist/compiler.js"
  let package : String = "@ren-lang/cli"
  
  let to_string : Platform -> String = platform =>
    where platform
      is #browser => 
        `in your browser using the playground at ${playground}.`
  
      is #cdn =>
        `in a <script> tag using the CDN at ${cdn}.`
 
      is #npm =>
        `locally by installing the npm package ${package}.`
  
  let choose_platform : Number -> Platform = n =>
    if n < 1 then
      #browser
                  
    else if n < 2 then
      #cdn
  
    else
      #npm
  
  run Time.every 1000 (_ => 
    Random.between 0 2 
      |> choose_platform 
      |> to_string
      |> `You can try Ren ${_}`
      |> Console.log
  )
</script>
```

Now that you've seen what Ren looks like, here's a list of some of the more interesting or notable features:

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
  - ...string extraction (``is `${protocol}://www.${domain}.${tld}` => ...``).
- Simple JavaScript FFI.
- Automatic currying and partial application.
- A pipe operator (`|>`) and heavy emphasis on pipe-friendly APIs.
- Placeholder syntax to support...
  -  ...positional piping (`y |> f x _ z`)
  -  ...anonymous pattern matching (`where _ is ...`)
  -  ...partial operator application (`_ < 10`).
- Compiles to readable modern JavaScript (ES6).  
