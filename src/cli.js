const Compiler = require('./CLI/Compiler.elm').Elm.CLI.Compiler
const Filesystem = require('fs').promises
const Package = require('../package.json')
const Path = require('path')
const Sade = require('sade')

const program = Sade('cherry')
    .version(Package.version)

    // Build command
    .command('build <src>')
    .describe('')
    .option('--debug, -d', '')
    .action(async (src, options) => {
        const path = Path.resolve(src)
        const stat = await Filesystem.stat(src)

        console.log(options)

        const compiler = Compiler.init({
            flags: {
                debug: options.debug
            }
        })

        compiler.ports.fromGenerator.subscribe(writeFile)
        compiler.ports.fromError.subscribe(console.error)

        if (stat.isFile()) {
            buildFile(compiler, path)
        } else if (stat.isDirectory()) {
            buildDirectory(compiler, path)
        } else {
            
        }
    })

    // REPL comand
    .command('repl')
    .describe('')
    .action(() => {
        console.log('This is the REPL.')
    })

    // Run the program
    .parse(process.argv)


// BUILD -----------------------------------------------------------------------


async function buildFile (compiler, path) {
    if (!path.endsWith('.cherry')) {
        console.error('I can only compile .cherry files.')
    } else try {
        const source = await Filesystem.readFile(path, { encoding: 'utf8' })
        const name = Path.basename(path, '.cherry')
        compiler.ports.toParser.send({ source, name, path: Path.dirname(path) })
    } catch (e) {
        console.error(e)
    }
}

async function buildDirectory (compiler, path) {

}

async function writeFile ({ source, name, path } = {}) {
    if (!source || !name || !path) {
        console.error('')
    } else try {
        await Filesystem.writeFile(
            Path.resolve(path, name),
            source,
            { encoding: 'utf8' }
        )
    } catch (e) {
        console.error(e)
    }
}