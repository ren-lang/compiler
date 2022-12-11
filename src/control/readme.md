# What goes here?

We can model custom control flow in functional programming but just... using
functions! We can insert scary words like "monad" and "applicative" but all that's
important to know is if you're modelling something that implements error handling
or branching then you probably have something that belongs in here.

Things in this directory **aren't** directly related to Ren, but are things we
need anyway. For example, the `control/parser` modules defines a general parser
combinator library – we use it to parse Ren code, but it's not specific to Ren.

One useful litmus test would be to ask if the module would be useful in other
codebases that pull in the compiler core. If the answer is yes, it probably
doesn't belong in here.
