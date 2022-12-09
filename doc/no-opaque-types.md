# Why Ren doesn't have opaque types

In this post we'll be exploring Ren's structural type system, how it differs from
from the type systems used by similar programming languages, and the implications
it has on how we should design APIs and achieve type safety in Ren.

For the rest of this post I'll be writing examples mostly in **Ren**, **Elm**,
and **Gleam**. Don't panic if you're not familiar with Elm or Gleam, we won't be
doing anything too complex here! If you've come across an "ML" language like OCaml
or Haskell, you should feel at home reading Elm code and Gleam has a friendly
Rust-style syntax that will be familiar to a lot of developers.

To kick things off, we need to ask a question:

## What are opaque types?

If you're coming to Ren from a language like Elm or Gleam then you might already
have heard about _opaque types_. For those not in the know, consider a type like
`UserId` below.

> CODE HERE

A common problem arises from types like these: how do we stop users creating
invalid IDs like `UserId(-1)`? One way to solve this is to make the type opaque
– expose the type's name, but none of its constructors – and then provide another
way of constructing an ID that validates the input.

> CODE HERE

Users can now import this module and call `fromInt`/`from_int` to try and
construct a `UserId`. If the input is invalid, they would get back nothing!
Crucially, users no longer have the ability to construct a `UserId` directly:
they are _forced_ to go through our validation API.

This sort of approach to API design is incredibly common, and when writing Elm or
Gleam code I tend to make everything opaque by default. It's a lot easier to
expose the constructors in the future than it is to take them away from code
already relying on them, and we can add additional layers of validation without
changing any code at the call sites.

This technique possible in Elm and Gleam (and others) because their custom types
are _nominal_. When we create our `UserId` type, the constructor is specific to
that type. If we had _another_ `UserId` type defined in another module – from
another package, for example – they would not be compatible. In fact, trying to
use them interchangeably would be a type error!

> CODE HERE

Opaque types aren't possible in Ren because types are defined by their _shape_
rather than their name.

## A detour on structural type systems

## Safety through specificity

## Revisiting `UserId`

So now we've learned how defining structurally rich types can help is achieve
type safety by reducing or eliminating the possibility to express invalid values
altogether, but how does this relate back to our original `UserId` type?

There is no deep structure to model here, so what we've learned so far can't help
us prevent users creating invlid IDs like `:user_id -1`. Fortunately, there is a
trick we can pull to help the structural type system work for us: **symbols**.

```rs
# Convenient alias so consumers can write `UserId.T` instead of `UserId.UserId`.
pub type UserId =
    | :user_id @Id Num

pub let from : Num -> Option.T UserId = num =>
    if num < 0 then
        :none
    
    else
        :some (:user_id @Id num)
```

Symbols are literal values and types that are unique to the module they're defined
in, so while the `:user_id` constructor is available anywhere in our program, the
`@Id` symbol is only available inside the `UserId` module!
