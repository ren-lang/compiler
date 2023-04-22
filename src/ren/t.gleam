//// The types in this module do dual duty as both the AST for type annotations
//// as well as the underlying type system for Ren. Ren's type system is a standard
//// typed lambda calculus with two notable additions:
////
//// * Row polymorphism giving rise to extensible records and polymorphic variants
//// * Gradual typing via a built-in any type: `*`.
////
//// 💡 If you're looking for how type inference works, then you've come to the
//// wrong place! Go check out `ren/query/check` instead.
////
//// ❓ Why is this module named `t`? Gleam doesn't allow modules to be named
//// `type` because it's a reserved word. Alternatives would be naming the module
//// any one of `type_` or `tipe` or `typ` but at that point I thought it best to
//// just opt for the purest alternative: `t`.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/bit_string
import gleam/list
import gleam/option.{None, Option, Some}
import gleam/set.{Set}
import ren/t/subst.{Subst}

// TYPES -----------------------------------------------------------------------

/// Ren's type system is relatively simple, but it's capable of expressing a whole
/// host of cool and useful things. Each type is explained in detail below, but
/// the gist of it is this...
///
/// Ren supports:
/// * Concrete types      – Num, Str
/// * Type variables      – a, b
/// * Parameterised types – Array Num, Result e a
/// * Record types        – { x: Num, y: Str }
/// * Variant types       – X Num | Y Str
/// 
pub type Type {
  /// Type application is kinda what it says on the tin: one type applied to
  /// another. That's how we end up with parameterised types like `Array Num`:
  /// that'd be:
  ///
  /// ```gleam
  /// Application(Concrete("Array"), [ Concrete("Num") ])
  /// ```
  ///
  Application(Type, List(Type))
  /// A concrete type is some type that we have a name for. It could refer to an
  /// external type like `Num` or some other named type like `Result`!
  ///
  Concrete(String)
  /// Functions in Ren are automatically curried - which means all functions are
  /// just chains of unary functions - so that's how they're represented in the
  /// type system!
  ///
  Function(Type, Type)
  /// A record is a projection of a `Row` with an AND operation applied to each
  /// field. If we pretend the syntax for a row is `{l: t | r}` then given a
  /// row like:
  ///
  /// ```text
  /// {a: Num | {b: Bool | {c: Str | {}}}}
  /// ```
  ///
  /// Then treating this Row like a record is like saying `a: Num` AND `b: Bool`
  /// AND `c: Str`, or in a conventional record syntax: `{a: Num, b: Bool, c: Str}`.
  ///
  Record(Row)
  /// Type variables allow Ren express *parametric polymorphism*. That's a fancy
  /// way of saying generics. With it, we can express types like `Array a` or
  /// functions like `a -> a`.
  ///
  Variable(String)
  /// A variant is a projection of a `Row` with an OR operation applied to each
  /// field. As we saw with the record example, given the row:
  ///
  /// ```text
  /// {a: Num | {b: Bool | {c: Str | {}}}}
  /// ```
  ///
  /// Treating it like a variant is like saying `a: Num` OR `b: Bool` OR `c: Str`,
  /// or in a more conventional syntax: `A Num | B Bool | C Str`.
  ///
  Variant(Row)
}

/// If you've read the docs for `Type` you might be wondering what a `Row` actually
/// is. It's actually quite simple, a `Row` can either be empty, or it can be a 
/// label paired to some type, and then a reference to another `Row`. That's it!
///
/// As you've probably seen, though, it turns out we can actually represent some
/// useful things as Rows, like records and variants, but also other things beyond
/// the scope of Ren like modules and effects.
///
/// 💡 If you'd like to do some homework on this sort of thing, the secret phrase
/// you're looking for is ✨ "row polymorphism" ✨.
/// 
pub type Row {
  Empty
  Extend(String, Type, Row)
  RowVariable(String)
}

// CONSTANTS -------------------------------------------------------------------

///
///
pub const num = Concrete("Num")

///
///
pub const str = Concrete("Str")

///
///
pub const bool = Concrete("Bool")

///
///
pub const unit = Concrete("Unit")

///
///
pub const any = Concrete("*")

///
///
pub const a = Variable("a")

///
///
pub const b = Variable("b")

///
///
pub const c = Variable("c")

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn app(t: Type, args: List(Type)) -> Type {
  case args {
    [] -> t
    _ -> Application(t, args)
  }
}

///
///
pub fn con(name: String) -> Type {
  Concrete(name)
}

///
///
pub fn fun(args: List(Type), return: Type) -> Type {
  case args {
    [] -> return
    [arg, ..rest] -> Function(arg, fun(rest, return))
  }
}

///
///
pub fn record(fields: List(#(String, Type))) -> Type {
  Record(create(fields))
}

///
///
pub fn var(name: String) -> Type {
  Variable(name)
}

///
///
pub fn variant(fields: List(#(String, Type))) -> Type {
  Variant(create(fields))
}

/// Generate a type variable name from a given index. Starting from 1 this generates
/// "a", "b", "c", ... "z", "aa", "ab", "ac", ... "az", "ba", "bb", "bc", ...
///
/// There are more robust ways of generating type variables but this is pretty
/// much fine as long as you can tolerate the slightly funky code. This is called
/// `fresh` because it is expected that you'll use this in tandem with some state
/// that will keep a counter of how many variables you've generated so far, thus
/// ensuring every time you incremenet the counter you get a "fresh" variable
/// name.
///
pub fn fresh(n: Int) -> String {
  // Whatever muppet tries to call this with a negative number deserves to have
  // the compiler crash on them.
  let assert True = n >= 0

  case n >= 26 {
    True -> fresh(n / 26 - 1) <> fresh(n % 26)

    False -> {
      let n = n + 97
      // Gleam doesn't have a simple `string.from_charcode` function, so we have
      // to be a bit creative. Here we're telling Gleam to treat out `n` as a
      // `BitString` (that's binary), and *then* converting *that* to a string.
      //
      // In other scenarios that might fail but in this case we know for sure
      // what we're producing is going to be a valid string because of the other
      // logic we have going on.
      let assert Ok(var) = bit_string.to_string(<<n:int>>)
      var
    }
  }
}

// QUERIES ---------------------------------------------------------------------

/// This function extracts a set of all the type variables in a given type. For
/// example, given the type `a -> b -> a` this will return the set `{"a", "b"}`.
///
pub fn free(t: Type) -> Set(String) {
  case t {
    Application(t, ts) -> set.union(free(t), free_in_list(ts))
    Concrete(_) -> set.new()
    Function(t1, t2) -> set.union(free(t1), free(t2))
    Record(r) -> free_in_row(r)
    Variable(v) -> set.from_list([v])
    Variant(r) -> free_in_row(r)
  }
}

fn free_in_list(ts: List(Type)) -> Set(String) {
  case ts {
    [] -> set.new()
    [t, ..ts] -> set.union(free(t), free_in_list(ts))
  }
}

fn free_in_row(r: Row) -> Set(String) {
  case r {
    Empty -> set.new()
    Extend(_, t, r) -> set.union(free(t), free_in_row(r))
    RowVariable(v) -> set.from_list([v])
  }
}

// MANIPULATIONS ---------------------------------------------------------------

/// Substitute all occurrences of a given type variable with another type if it
/// appears in the substitute map. This is used during type inference and unifcation
/// as substitutions are built up and applied.
///
pub fn substitute(t: Type, subst: Subst(Type)) -> Type {
  case t {
    Application(t, ts) ->
      Application(substitute(t, subst), substitute_in_list(ts, subst))
    Concrete(_) -> t
    Function(t1, t2) -> Function(substitute(t1, subst), substitute(t2, subst))
    Record(r) -> Record(substitute_in_row(r, subst))
    Variable(v) -> option.unwrap(subst.lookup(subst, v), t)
    Variant(r) -> Variant(substitute_in_row(r, subst))
  }
}

fn substitute_in_list(ts: List(Type), subst: Subst(Type)) -> List(Type) {
  case ts {
    [] -> []
    [t, ..ts] -> [substitute(t, subst), ..substitute_in_list(ts, subst)]
  }
}

fn substitute_in_row(r: Row, subst: Subst(Type)) -> Row {
  case r {
    Empty -> r
    Extend(l, t, r) ->
      Extend(l, substitute(t, subst), substitute_in_row(r, subst))
    RowVariable(v) ->
      case subst.lookup(subst, v) {
        None -> r
        Some(Record(row)) -> row
        Some(Variant(row)) -> row
        _ -> r
      }
  }
}

// ROW OPERATIONS --------------------------------------------------------------
// There are a handful of standard operations we can perform on rows. There aren't
// many but they're enough to encompass all the sort of things you might want to
// express in Ren code.

fn create(fields: List(#(String, Type))) -> Row {
  use r, f <- list.fold_right(fields, Empty)
  let #(l, t) = f
  extend(r, l, t)
}

/// Extending a Row adds another field to it. If the field already exists, it 
/// will be shadowed. This is contrary to some other type systems that explicitly
/// require the field to be absent from the row being extended.
///
/// If we take the syntax we made up for Rows earlier, then Row extension looks
/// like this:
///
/// ```text
/// {l: _ | _} : ∀ra. a -> {r} -> {l: a | r}
/// ```
///
pub fn extend(row: Row, label: String, t: Type) -> Row {
  Extend(label, t, row)
}

/// Selecting a field is just record access, but for types! Obviously, if the
/// field is missing there's nothing to select and we need to return `None`.
///
/// ```text
/// _.l : ∀r. {l: a | r} -> a
/// ```
///
pub fn select(row: Row, label: String) -> Option(Type) {
  case row {
    Extend(l, t, _) if label == l -> Some(t)
    Extend(_, _, r) -> select(r, label)
    Empty -> None
    RowVariable(_) -> None
  }
}

/// This one is typically quite uncommon. I'm not even entirely sure we want to
/// expose this one to surface Ren code, but this is how we *remove* a field from
/// some Row.
///
/// ```text
/// _/l : ∀r. {l: a | r} -> {r}
/// ```
///
pub fn remove(row: Row, label: String) -> Row {
  case row {
    Extend(l, _, r) if label == l -> r
    Extend(l, t, r) -> Extend(l, t, remove(r, label))
    Empty -> row
    RowVariable(_) -> row
  }
}

/// An update allows us to transform the type of a field to something else. We
/// could actually just repurpose `extend` for this, and shadow the existing field
/// but right now we *will* assume the invariant that the label exists in the Row
/// holds.
///
/// /// ```text
/// {l: _ | _} : ∀rab. b -> {l: a | r} -> {l: b | r}
/// ```
///
/// Looking at the type above, we can see that we expect to have some label `l`
/// with type `a`, and we'll replace it with a new type `b`.
///
pub fn update(row: Row, label: String, t1: Type) -> Row {
  case row {
    Extend(l, _, r) if label == l -> Extend(l, t1, r)
    Extend(l, t2, r) -> Extend(l, t2, update(r, label, t1))
    Empty -> row
    RowVariable(_) -> row
  }
}
