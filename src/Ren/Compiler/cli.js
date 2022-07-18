#!/usr/bin/env node

import * as Chalk from 'chalk'
import * as ChildProcess from 'child_process'
import * as Fs from 'fs'
import * as Gitly from 'gitly'
import * as Path from 'path'
import * as Process from 'process'

import { Elm } from './Cli.elm'

setImmediate(() => {
    const compiler = Elm.Ren.Compiler.Cli.init({
        flags: {
            chalk: makeProxy('FFI.Chalk', Chalk),
            fs: makeProxy('FFI.Fs', {
                isFile(path) {
                    return Fs.lstatSync(path).isFile()
                },
                isDirectory(path) {
                    return Fs.lstatSync(path).isDirectory()
                },
                ...Fs,
            }),
            gitly: makeProxy('FFI.Gitly', {
                fetch(author, repo, dest) {
                    const options = {
                        temp: Path.join(OS.homedir(), '.ren', 'deps'),
                        throw: true,
                    }

                    Fs.mkdirSync(dest, { recursive: true })
                    Gitly.default(`${author}/${repo}`, dest, options)
                        .then(() => {
                            console.log(`🎉 Successfully fetched ${author}/${repo}.`)

                            Fs.mkdirSync(`${dest}/.ren/deps`, { recursive: true })
                            ChildProcess.exec(`cd ${dest} && ren make`, (err, _) => {
                                if (err) return Promise.reject(err)
                            })
                        })
                        .catch(() => {
                            console.error(`🚨 Could not fetch ${author}/${repo}.`)
                        })
                },
            }),
            path: makeProxy('FFI.Path', Path),
            process: makeProxy('FFI.Process', Process),
        }
    })

    compiler.ports?.stdout?.subscribe((msg) => {
        console.log(msg)
    })

    compiler.ports?.stderr?.subscribe((msg) => {
        console.error(msg)
    })

    compiler.ports?.exec?.subscribe(([path, args]) => {
        import(path)
            .then(({ main }) => {
                let result = typeof main == 'function' ? main(args) : main

                if (result != undefined) {
                    console.log(result)
                }
            }).catch(console.error)
    })

    compiler.ports?.eval?.subscribe((src) => {
        const mod = 'data:text/javascript;base64,' + btoa(src)

        import(mod)
            .then(({ $eval }) => console.dir($eval()))
            .catch(console.error)
    })
})

// UTILS -----------------------------------------------------------------------

const makeProxy = (key, obj) => {
    const error = (e) =>
        new Error(
            `Uh oh, it looks like there was an internal error with ${key}. Please` +
            'open an issue at https://github.com/ren-lang/compiler quoting: \n\n' +
            e,
        )

    return new Proxy(obj, {
        get(target, prop) {
            if (prop == key) return true

            try {
                const { method, args } = JSON.parse(prop)

                return typeof target[method] == 'function'
                    ? target[method](...args)
                    : target[method]
            } catch (e) {
                throw error(e)
            }
        },

        has(target, prop) {
            if (prop == key) return true

            try {
                const { method } = JSON.parse(prop)
                return method in target
            } catch (e) {
                throw error(e)
            }
        },
    })
}