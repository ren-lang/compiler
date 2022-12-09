// This module provides a rough reference of Ren's syntax. It should include all
// the core language constructs and demonstrate how to use them.
// We've already seen our first bit of Ren: comments! Ren uses the hash symbol for
// comments so that we get shebang support for free. There are only single-line
// comments in Ren, but we will see later that doc comments have a very slightly
// different syntax.
// IMPORTS ----------------------------------------------------------------------
// Ren has three types of imports: relative/project imports, package imports, and
// external imports.
// Project imports are how you import other Ren code from the same project. Import
// paths are always relative from the current module, but the file extension is
// optional:

import { $eq, $function } from './../.ren/pkg/prelude.js'
import * as Foo from './foo.ren.js'