import treestand
import std/os

importGrammar(currentSourcePath.parentDir.parentDir.parentDir / "tests" / "fixtures" / "json" / "grammar.js")

when isMainModule:
    # A reasonably complex JSON sample
    let jsonStr = """
    {
    "project": "treestand",
    "version": 1.0,
    "features": [
        "parser-generator", 
        "grammar-compilation",
        "runtime-library"
    ],
    "meta": {
        "author": "yesdrx",
        "active": true,
        "score": 99.5,
        "dependencies": null
    },
    "unicode": "\u00A9 2025"
    }
    """

    let tree = parseJson(jsonStr) # parseJson is defined by importGrammar
    echo "Parse Tree:"
    echo tree
    echo "========================================"
    echo "Success!"