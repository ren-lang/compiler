//// The following module is a mostly faithful port of the great `elm-safe-recursion`
//// package by Micah Hahn. You can find the original source and documentation
//// here:
////
////   https://github.com/micahhahn/elm-safe-recursion/tree/2.0.0
////
//// The original package is licensed under the BSD 3-Clause License, reproduced
//// below:
//// 
////   BSD 3-Clause License
////   
////   Copyright (c) 2022, NoRedInk
////   All rights reserved.
////   
////   Redistribution and use in source and binary forms, with or without
////   modification, are permitted provided that the following conditions are met:
////   
////   * Redistributions of source code must retain the above copyright notice, this
////     list of conditions and the following disclaimer.
////   
////   * Redistributions in binary form must reproduce the above copyright notice,
////     this list of conditions and the following disclaimer in the documentation
////     and/or other materials provided with the distribution.
////   
////   * Neither the name of the copyright holder nor the names of its
////     contributors may be used to endorse or promote products derived from
////     this software without specific prior written permission.
////   
////   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
////   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
////   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
////   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
////   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
////   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
////   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
////   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
////   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
////   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
////
////
//// The point of this module is to make it simple to write stack-safe recursive
//// algorithms without worrying about how to make your code tail-recursive. Coupled
//// with Gleam's great `use` syntax, it makes it super easy to write recursive code
//// mostly in a way that you want to write anyway.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/list

// TYPES -----------------------------------------------------------------------

///
///
pub opaque type Rec(a, r, t) {
  Rec(r, fn(t) -> Rec(a, r, t))
  Base(a)
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn base(a: a) -> Rec(a, r, t) {
  Base(a)
}

///
///
pub fn rec(r: r) -> Rec(a, r, a) {
  Rec(r, Base)
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn map(step: Rec(a, r, t), f: fn(a) -> b) -> Rec(b, r, t) {
  case step {
    Base(a) -> Base(f(a))
    Rec(r, cont) -> Rec(r, fn(a) { map(cont(a), f) })
  }
}

///
///
pub fn do(step: Rec(a, r, t), f: fn(a) -> Rec(b, r, t)) -> Rec(b, r, t) {
  case step {
    Base(a) -> f(a)
    Rec(r, cont) -> Rec(r, fn(a) { do(cont(a), f) })
  }
}

///
///
pub fn step(r: r, cont: fn(t) -> Rec(a, r, t)) -> Rec(a, r, t) {
  Rec(r, cont)
}

///
///
pub fn list(items: List(r), next: fn(List(a)) -> Rec(a, r, a)) -> Rec(a, r, a) {
  use xs <- fold([], items, list.prepend)
  next(list.reverse(xs))
}

///
///
pub fn fold(
  acc: a,
  items: List(r),
  f: fn(a, t) -> a,
  next: fn(a) -> Rec(b, r, t),
) -> Rec(b, r, t) {
  case items {
    [] -> next(acc)
    [item, ..rest] -> step(item, fn(t) { fold(f(acc, t), rest, f, next) })
  }
}

///
///
pub fn traverse(
  items: List(x),
  proj: fn(x) -> Rec(a, r, t),
  next: fn(List(a)) -> Rec(b, r, t),
) -> Rec(b, r, t) {
  do_traverse([], items, proj, next)
}

fn do_traverse(
  acc: List(a),
  stack: List(x),
  proj: fn(x) -> Rec(a, r, t),
  next: fn(List(a)) -> Rec(b, r, t),
) -> Rec(b, r, t) {
  case stack {
    [] -> next(list.reverse(acc))
    [item, ..rest] ->
      do(proj(item), fn(a) { do_traverse([a, ..acc], rest, proj, next) })
  }
}

// CONVERSIONS -----------------------------------------------------------------

///
///
pub fn run(init: r, proj: fn(r) -> Rec(a, r, a)) -> a {
  do_run(proj(init), [], proj)
}

fn do_run(
  step: Rec(a, r, a),
  stack: List(fn(a) -> Rec(a, r, a)),
  proj: fn(r) -> Rec(a, r, a),
) -> a {
  case step, stack {
    Base(a), [] -> a
    Base(a), [next, ..rest] -> do_run(next(a), rest, proj)
    Rec(r, cont), _ -> do_run(proj(r), [cont, ..stack], proj)
  }
}
