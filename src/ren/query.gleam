// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/function
import ren/data/error.{Error}

// TYPES -----------------------------------------------------------------------

/// A `Provider` imbues our compiler core with the basic functionality it needs
/// to interact with the outside world. It abstracts things like file read/writes
/// and console logging, and means we can use the same compiler core in Node, Deno,
/// and the browser.
///
pub type Provider {
  Provider(
    read: fn(String) -> Result(String, Error),
    write: fn(String, String) -> Result(Nil, Error),
    stdout: fn(String) -> Nil,
    stderr: fn(String) -> Nil,
    log: fn(String) -> Nil,
  )
}

/// A `Query` represents a core building block of our compiler architecture.
/// Fundamentally a `Query` is simply a function that takes an environment and
/// returns a – potentially updated – environment along with some `Result`.
///
/// While that doesn't sound particularly exciting on the surface, by combining
/// and composing queries with the functions in this module we can start to
/// put together complex compiler pipelines from simple building blocks.
///
/// ❓ Why is this a type alias? Generally I dislike type aliases in part because
/// they leak implementation details to consumers of the type. However, Gleam
/// doesn't do automatic unboxing of so-called "newtypes" (custom types with only
/// one variant) so preferring a type alias is a tiny bit more efficient.
///
pub type Query(a, env) =
  fn(Provider, env) -> #(env, Result(a, Error))

// RUNNING A QUERY -------------------------------------------------------------

/// Execute a query with the given environment and provider. This is the main
/// entry point for running a query.
///
pub fn run(q: Query(a, env), provider: Provider, env: env) -> Result(a, Error) {
  q(provider, env).1
}

// CONSTRUCTORS ----------------------------------------------------------------

/// A `Query` that always returns the same value when run. This is mostly useful
/// when used in combination with functions like `do`: perhaps you want to run
/// a query, discard the result, and return something else.
///
pub fn return(val: a) -> Query(a, env) {
  fn(_, env) { #(env, Ok(val)) }
}

/// The opposite of `return`. This creates a `Query` that always fails with the
/// given error. 
///
pub fn throw(error: Error) -> Query(a, env) {
  fn(_, env) { #(env, Error(error)) }
}

/// Occasionally you might want to treat a function that returns a `Result` but
/// doesn't depend on the environment as a `Query`. This function allows you to
/// do that.
///
pub fn from_result(res: Result(a, Error)) -> Query(a, env) {
  fn(_, env) { #(env, res) }
}

// PROVIDERS -------------------------------------------------------------------
// 
// We saw how the `Providers` record is used to abstract away the different ways
// to do IO in ou queries. These functions, then, give us access to the different
// IO operations afforded by a provider.
//
// ```gleam
// import ren/query.{Query}
// import ren/query/lex
// import ren/query/parse
// import ren/query/fmt
// 
// fn example() {
//   let path = "src/example.ren"
// 
//   query.read(path)
//   |> query.then(lex.run)
//   |> query.then(parse.mod)
//   |> query.then(fmt.run)
//   |> query.then(query.write(path))
// }
// ```
//

/// Read a file from the given path using a `Provider`s read function.
///
pub fn read(path: String) -> Query(String, env) {
  fn(provider: Provider, env) {
    case provider.read(path) {
      Ok(val) -> #(env, Ok(val))
      Error(error) -> #(env, Error(error))
    }
  }
}

/// Write a file to the given path using a `Provider`s write function. This is
/// organised so the file's contents is the argument that can be piped into the
/// function as the path is usually fixed or known for a given query.
///
pub fn write(file: String, path: String) -> Query(Nil, env) {
  fn(provider: Provider, env) {
    case provider.write(file, path) {
      Ok(_) -> #(env, Ok(Nil))
      Error(error) -> #(env, Error(error))
    }
  }
}

///
///
pub fn stdout(msg: String) -> Query(Nil, env) {
  fn(provider: Provider, env) {
    provider.stdout(msg)
    #(env, Ok(Nil))
  }
}

///
///
pub fn stderr(msg: String) -> Query(Nil, env) {
  fn(provider: Provider, env) {
    provider.stderr(msg)
    #(env, Ok(Nil))
  }
}

/// Sometimes you might want a query to print some logs or diagnostics, but you 
/// don't want or need to include that in the stdout stream. By having a separate
/// `log` function, a `Provider` can choose to surpress or divert logs if it wants
/// to.
///
pub fn log(msg: String) -> Query(Nil, env) {
  fn(provider: Provider, env) {
    provider.log(msg)
    #(env, Ok(Nil))
  }
}

// MANIPULATIONS ---------------------------------------------------------------

/// Run a query and then replace its result with the given value.
///
pub fn replace(a: Query(a, env), with b: b) -> Query(b, env) {
  use _ <- do(a)
  return(b)
}

/// Run a query and then, if it fails, replace its error with the given error.
///
pub fn replace_error(query: Query(a, env), with e: Error) -> Query(a, env) {
  fn(provider, env) {
    case query(provider, env) {
      #(env, Ok(val)) -> #(env, Ok(val))
      #(env, Error(_)) -> #(env, Error(e))
    }
  }
}

/// Run a query and then transform its result with the given function.
///
/// 💡 In languages like PureScript or Haskell, this is often called `fmap` or
/// `<$>`.
///
pub fn map(query: Query(a, env), f: fn(a) -> b) -> Query(b, env) {
  use a <- do(query)
  return(f(a))
}

///
/// 💡 In other functional languages this might go by a few names. You might see
/// it called `bind`, `flatMap`, `chain`, `andThen`, or even `>>=`. Closer to home,
/// many Gleam modules graviate to calling this `then`, because it reads nicely
/// in a pipeline. We chose to go with `do` because it reads nicely when using
/// the `use` syntax instead.
/// 
pub fn do(query: Query(a, env), then f: fn(a) -> Query(b, env)) -> Query(b, env) {
  fn(provider, env) {
    case query(provider, env) {
      #(env, Ok(val)) -> f(val)(provider, env)
      #(env, Error(error)) -> #(env, Error(error))
    }
  }
}

/// In `Gleam` it is quite common for a type to define monadic bind as a function
/// called `then` because it reads very nicely in a pipe. Although we encourage
/// the use of the new `use` syntax in tandem with `do` for better readability,
/// sometimes a pipe is the best approach so this function is provided as an 
/// alias for `do`.
///
pub fn then(query: Query(a, env), do f: fn(a) -> Query(b, env)) -> Query(b, env) {
  do(query, f)
}

/// This function is like `do` or `then` except it allows us to handle the cases
/// where a query fails by giving us the `Result` returned by the query just run.
///
/// We could simply fail again by using `throw` on the error, but we could also
/// attempt to recover from certain errors by running another query or transforming
/// the error in some way.
///
pub fn attempt(
  query: Query(a, env),
  k: fn(Result(a, Error)) -> Query(b, env),
) -> Query(b, env) {
  fn(provider, env) {
    case query(provider, env) {
      #(env, Ok(val)) -> k(Ok(val))(provider, env)
      #(env, Error(error)) -> k(Error(error))(provider, env)
    }
  }
}

/// Run a list of queries and then combine their result back into a list. If any
/// of the queries fail, the entire operation fails.
///
/// 💡 In languages like PureScript or Haskell this operation is commonly known
/// as `sequence`.
///
pub fn all(queries: List(Query(a, env))) -> Query(List(a), env) {
  each(queries, function.identity)
}

/// Convert each item in a list into a query and then run it. We'll combine the
/// results back into a list like `sequence` does along the way. If any of the
/// queries fail, the entire operation fails.
///
/// 💡 In languages like PureScript or Haskell this operation is commonly known
/// as `traverse`.
///
pub fn each(items: List(a), f: fn(a) -> Query(b, env)) -> Query(List(b), env) {
  use queries, item <- list.fold_right(items, return([]))
  use query <- do(f(item))

  map(queries, list.prepend(_, query))
}
