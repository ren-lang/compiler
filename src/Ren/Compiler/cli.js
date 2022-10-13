import * as Chalk from 'chalk'
import * as Fs from 'node:fs/promises'
import * as Gitly from 'gitly'
import * as Path from 'node:path'
import * as Process from 'node:process'
import * as Readline from 'node:readline'

import { Elm } from './CLI.elm'
import { FFI } from './ffi.js'

// 

FFI.addModule('Chalk', Chalk)
FFI.addModule('Console', console)
FFI.addModule('Fs', {
    ...Fs,
    exists: path => Fs.stat(path).then(_ => true).catch(_ => Promise.resolve(false)),
    isFile: path => Fs.stat(path).then(stat => stat.isFile()),
    isDir: path => Fs.stat(path).then(stat => stat.isDirectory())
})
FFI.addModule('Gitly', Gitly)
FFI.addModule('Path', Path)
FFI.addModule('Process', Process)

//

const readline = Readline.createInterface({ input: Process.stdin, output: Process.stdout })
const compiler = Elm.Ren.Compiler.CLI.init({
    flags: Process.argv.slice(2)
})

compiler.ports.stdout?.subscribe(console.log)
compiler.ports.stderr?.subscribe(console.error)

readline.on('line', line => compiler.ports.stdin?.send(line))
