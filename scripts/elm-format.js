#!/usr/bin/env node

// A work around to https://github.com/avh4/elm-format/issues/352

// * Use it in place of elm-format (only some flags are supported)
// * Run it from a directory containing node_modules/elm-format/bin/elm-format (but you can change it below)


function extractFirstRegexMatch(regex, elmCode) {

    var match = elmCode.match(regex);
    if (!match)
        return null

    return {
        before: elmCode.slice(0, match.index),
        matchedGroups: [].slice.call(match, 1),
        after: elmCode.slice(match.index + match[0].length),
    };
}


function reduceIndent(targetIndent, finalIndent, code) {
    var deltaIndent = targetIndent.length - finalIndent.length;

    var lines = code.split('\n');

    for (var i = 0; i < lines.length && (lines[i] == "" || lines[i].startsWith(targetIndent)); i++) {
        lines[i] = lines[i].slice(deltaIndent);
    }

    return lines.join('\n');
}


function alignDoNotation(elmCode) {

    const doNotationRegex = /([ ]+)(.*[<][|])[\n]([ ]+)([\\][_a-zA-Z0-9(){}, ]+ ->[\n])/;

    var stuff;
    while (stuff = extractFirstRegexMatch(doNotationRegex, elmCode)) {

        var firstLineIndent = stuff.matchedGroups[0]
        var firstLineContent = stuff.matchedGroups[1]
        var secondLineIndent = stuff.matchedGroups[2]
        var lambdaArgument = stuff.matchedGroups[3]

        // TODO assert firstLineIndent < secondLineIndent

        var finalIndent = firstLineIndent;
        var targetIndent = secondLineIndent + ' '.repeat(4);

        elmCode =
            stuff.before
            +
            firstLineIndent + firstLineContent + ' ' + lambdaArgument
            +
            reduceIndent(targetIndent, finalIndent, stuff.after)
    }

    return elmCode
}


//
// Command line stuff
//


function assert_(test, message) {
    if (test) return;

    console.error('HACK:', message);
    process.exit(-1);
}


function exitOnError(error) {
    if (!error) return;

    console.error(error.message);
    process.exit(-1);
}


function mainForCommandLine() {

    const fs = require('fs');

    //
    // Parse args
    //
    let inputIsStdin = false;
    let inputFile = null;
    let outputFile = null;

    for (let i = 2; i < process.argv.length; i++) {
        let [option, arg] = process.argv.slice(i);

        ({
            '--output': () => { outputFile = arg; i++; },
            '--stdin': () => { inputIsStdin = true; },
            '--validate': () => { assert_(false, '--validate is not supported.'); },
            '--yes': () => { },
        }[option] || (() => { inputFile = option; })
        )();
    }

    assert_(inputFile || inputIsStdin, 'I need an input file or --stdin');

    if (!inputIsStdin && !outputFile) {
        outputFile = inputFile;
    }

    const inputStream =
        inputIsStdin
            ? process.stdin
            // TODO YOLO
            : fs.openSync(inputFile);


    //
    // Run elm-format
    //
    let cp = require('child_process');
    let result = cp.spawnSync('elm-format', ['--stdin'], {
        stdio: [inputStream, 'pipe', process.stderr],
        encoding: 'utf-8',
    });

    exitOnError(result.error);

    if (result.status !== 0) {
        process.exit(result.status);
    }

    //
    // alignDoNotation()
    //
    const hackFormattedElmCode = alignDoNotation(result.stdout);

    //
    // Write
    //
    if (!outputFile) {
        process.stdout.write(hackFormattedElmCode);
    } else {
        fs.writeFile(outputFile, hackFormattedElmCode, 'utf-8', (err) => {
            exitOnError(err);
        });
    }
};


if (require.main === module) {
    mainForCommandLine();
}