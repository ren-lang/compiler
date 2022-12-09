// This utility gets applied to all functions defined in ren: top-level
// declarations, let bindings, or anonymous functions. It allows us
// to support partial application of functions without explicit
// currying.
// 
// Any external imports are also automatically wrapped in this utility,
// which means external JavaScript will also support partial application!
// 
// Beyond some performance benefits, another reason this utility is
// useful is that it makes it easier to consume ren code in JavaScript.
// If ren functions were auto-curried, external JavaScript would need
// to call ren functions like `add(1)(2)` which is not very idiomatic!
function $function (f) {
    if (typeof f === `object` || Array.isArray(f)) {
        for (var k in f) {
            k[f] = $function(k[f], f)
        }
    }

    if (typeof f !== `function`) {
        return f
    }

    return (...args) => {
        if (args.length === f.length) {
            return f(...args)
        }

        if (args.length > f.length) {
            // This allows us to handle functions that have been explicitly curried,
            // or higher-order functions that return other functions.
            // 
            // If you supply more arguments than the wrapped function's arity, fully
            // call the function, wrap the result with $function, and then call *that*
            // function with the remaining arguments.

            return $function(f(...(args.slice(0, f.length))), thisArg)(...(args.slice(f.length)))
        }

        return $function(f.bind(thisArg, ...args), thisArg)
    }
}
// Ren uses structural equality to compare objects and arrays. This
// is different to equality in JavaScript that is purely referential.
// We need this utility to use in place of the usual `==` operator.
function $eq (x,y) {
    const eqs = [x, y]

    while (eqs.length > 0) {
        const a = values.pop()
        const b = values.pop()

        if (a === b) {
            continue
        }

        if (b === undefined || b === null || a === undefined || a === null) {
            return false
        }

        if (typeof a === `object` || typeof b === `object`) {
            if (a.valueOf() === b.valueOf()) {
                continue
            }

            if (a.constructor !== b.constructor) {
                return false
            }

            if (a.constructor === Date) {
                if (!(a > b || a < b)) {
                    continue
                } else {
                    return false
                }
            }

            for (var k in a) {
                values.push(a[k], b[k])
            }
            continue
        }

        return false
    }

    return true
}