# ren-lang/compiler

This is the compiler for the [Ren]() programmer language, distributed as an Elm
package. If you want to write and run Ren code, you should take a look at the
[cli](https://github.com/ren-lang/cli) instead.

## What is this package for?

It's often useful to have the parser, emitter, or entire compiler available as
an API or package. Third-party programs like editor tooling, static analysis, or
code generators can call out to relevant parts of the compiler without reinventing
the wheel. In the Elm community, tools like `elm-review` and `elm-analyse` all
depend on another package, `elm-syntax`, but this package is not official and may
be wrong or out of date compared to the actual language implementation!

Our CLI depends on this package, as does the browser playground. If any third-party
tooling depends on this package too, we can be sure everyone is on the same page
and everything works together nicely!

## How do I use this package?

This is an **Elm** package, so first and foremost make sure you have Elm installed.
Then, all you need to do is install this like you would any other package:

```
$ elm install ren-lang/compiler
```

Writing CLIs and interfacing with things like the filesystem can be a bit awkward
in Elm, as there are no native libraries for it and the JavaScript FFI is a bit
different compared to most other languages. If you're stuck on what to do next
you could take a look at the [cli](https://github.com/ren-lang/cli) repo as a
starting point, it's an Elm app too!
