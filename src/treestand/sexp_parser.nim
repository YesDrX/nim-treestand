# Generated parser for sexp
# This file is auto-generated - do not edit

{.push warning[UnusedImport]: off.}
import parser_types
import std/times
import std/options

import std/strutils

{.pop.}
{.hint[Path]: on.}

type
  TerminalSymbol* = enum
    tsEOF = 0
    tsPattern = 1
    tsLParen = 2
    tsRParen = 3
    tsIdentifier = 4
    tsGrammarSymbol = 5
    tsPattern_6 = 6
    tsPattern_7 = 7
    tsGrammarSymbol_8 = 8
    tsPattern_9 = 9
    tsPattern_10 = 10
    tsDot = 11
    tsPattern_12 = 12

  NonTerminalSymbol* = enum
    ntProgram = 0
    ntExpression = 1
    ntList = 2
    ntAtom = 3
    ntString = 4
    ntWildcard = 5
    ntCapture = 6
    ntField = 7
    ntAnchor = 8
    ntComment = 9
    ntProgram_repeat1 = 10
    ntString_repeat1 = 11

# GrammarSymbol names for debugging and error messages
const terminalSymbolNames* = [
  "EOF",
  "/\\s/",
  "\'(\'",
  "\')\'",
  "/[a-zA-Z_][a-zA-Z0-9_\\-\\.]*/",
  "\'\"\'",
  "/[^\"\\\\\\n]+/",
  "/\\\\./",
  "\'_\'",
  "/@[a-zA-Z0-9_\\-\\.]+/",
  "/[a-zA-Z0-9_\\-]+:/",
  "\'.\'",
  "/;.*/",
]

const nonTerminalSymbolNames* = [
  "program",
  "expression",
  "list",
  "atom",
  "string",
  "wildcard",
  "capture",
  "field",
  "anchor",
  "comment",
  "program_repeat1",
  "string_repeat1",
]

const terminalSymbolMetadata* = [
  parser_types.SymbolMetadata(named: false),  # EOF
  parser_types.SymbolMetadata(named: true),  # "/\\s/"
  parser_types.SymbolMetadata(named: false),  # "\'(\'"
  parser_types.SymbolMetadata(named: false),  # "\')\'"
  parser_types.SymbolMetadata(named: true),  # "/[a-zA-Z_][a-zA-Z0-9_\\-\\.]*/"
  parser_types.SymbolMetadata(named: false),  # "\'\"\'"
  parser_types.SymbolMetadata(named: true),  # "/[^\"\\\\\\n]+/"
  parser_types.SymbolMetadata(named: true),  # "/\\\\./"
  parser_types.SymbolMetadata(named: false),  # "\'_\'"
  parser_types.SymbolMetadata(named: true),  # "/@[a-zA-Z0-9_\\-\\.]+/"
  parser_types.SymbolMetadata(named: true),  # "/[a-zA-Z0-9_\\-]+:/"
  parser_types.SymbolMetadata(named: false),  # "\'.\'"
  parser_types.SymbolMetadata(named: true),  # "/;.*/"
]

const nonTerminalSymbolMetadata* = [
  parser_types.SymbolMetadata(named: true),  # program
  parser_types.SymbolMetadata(named: true),  # expression
  parser_types.SymbolMetadata(named: true),  # list
  parser_types.SymbolMetadata(named: true),  # atom
  parser_types.SymbolMetadata(named: true),  # string
  parser_types.SymbolMetadata(named: true),  # wildcard
  parser_types.SymbolMetadata(named: true),  # capture
  parser_types.SymbolMetadata(named: true),  # field
  parser_types.SymbolMetadata(named: true),  # anchor
  parser_types.SymbolMetadata(named: true),  # comment
  parser_types.SymbolMetadata(named: true),  # program_repeat1
  parser_types.SymbolMetadata(named: true),  # string_repeat1
]


# Compact Parse Table Arrays
const parseTableActions* = @[
  (parser_types.t(2), parser_types.s(4u32)),
  (parser_types.t(5), parser_types.s(5u32)),
  (parser_types.t(0), parser_types.r(parser_types.nt(0), 0u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.s(7u32)),
  (parser_types.t(11), parser_types.s(8u32)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.s(9u32)),
  (parser_types.t(8), parser_types.s(1u32)),
  (parser_types.t(10), parser_types.s(10u32)),
  (parser_types.t(2), parser_types.r(parser_types.nt(5), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.r(parser_types.nt(5), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(0), parser_types.r(parser_types.nt(5), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.r(parser_types.nt(5), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(11), parser_types.r(parser_types.nt(5), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.r(parser_types.nt(5), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(3), parser_types.r(parser_types.nt(5), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(10), parser_types.r(parser_types.nt(5), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(8), parser_types.r(parser_types.nt(5), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(0), parser_types.acc()),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(2), parser_types.r(parser_types.nt(1), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.r(parser_types.nt(1), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(0), parser_types.r(parser_types.nt(1), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.r(parser_types.nt(1), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(11), parser_types.r(parser_types.nt(1), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.r(parser_types.nt(1), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(3), parser_types.r(parser_types.nt(1), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(10), parser_types.r(parser_types.nt(1), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(8), parser_types.r(parser_types.nt(1), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(2), parser_types.s(4u32)),
  (parser_types.t(5), parser_types.s(5u32)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.s(7u32)),
  (parser_types.t(11), parser_types.s(8u32)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.s(9u32)),
  (parser_types.t(8), parser_types.s(1u32)),
  (parser_types.t(10), parser_types.s(10u32)),
  (parser_types.t(5), parser_types.s(12u32)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(6), parser_types.s(13u32)),
  (parser_types.t(7), parser_types.s(13u32)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(2), parser_types.s(4u32)),
  (parser_types.t(5), parser_types.s(5u32)),
  (parser_types.t(0), parser_types.r(parser_types.nt(0), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.s(7u32)),
  (parser_types.t(11), parser_types.s(8u32)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.s(9u32)),
  (parser_types.t(8), parser_types.s(1u32)),
  (parser_types.t(10), parser_types.s(10u32)),
  (parser_types.t(2), parser_types.r(parser_types.nt(6), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.r(parser_types.nt(6), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(0), parser_types.r(parser_types.nt(6), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.r(parser_types.nt(6), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(11), parser_types.r(parser_types.nt(6), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.r(parser_types.nt(6), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(3), parser_types.r(parser_types.nt(6), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(8), parser_types.r(parser_types.nt(6), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(10), parser_types.r(parser_types.nt(6), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(2), parser_types.r(parser_types.nt(8), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.r(parser_types.nt(8), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(0), parser_types.r(parser_types.nt(8), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.r(parser_types.nt(8), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(11), parser_types.r(parser_types.nt(8), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.r(parser_types.nt(8), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(3), parser_types.r(parser_types.nt(8), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(8), parser_types.r(parser_types.nt(8), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(10), parser_types.r(parser_types.nt(8), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(2), parser_types.r(parser_types.nt(3), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.r(parser_types.nt(3), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(0), parser_types.r(parser_types.nt(3), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.r(parser_types.nt(3), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(11), parser_types.r(parser_types.nt(3), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.r(parser_types.nt(3), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(3), parser_types.r(parser_types.nt(3), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(10), parser_types.r(parser_types.nt(3), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(8), parser_types.r(parser_types.nt(3), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(2), parser_types.r(parser_types.nt(7), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.r(parser_types.nt(7), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(0), parser_types.r(parser_types.nt(7), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.r(parser_types.nt(7), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(11), parser_types.r(parser_types.nt(7), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.r(parser_types.nt(7), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(3), parser_types.r(parser_types.nt(7), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(8), parser_types.r(parser_types.nt(7), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(10), parser_types.r(parser_types.nt(7), 1u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(2), parser_types.s(4u32)),
  (parser_types.t(5), parser_types.s(5u32)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.s(7u32)),
  (parser_types.t(11), parser_types.s(8u32)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.s(9u32)),
  (parser_types.t(3), parser_types.s(15u32)),
  (parser_types.t(8), parser_types.s(1u32)),
  (parser_types.t(10), parser_types.s(10u32)),
  (parser_types.t(2), parser_types.r(parser_types.nt(4), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.r(parser_types.nt(4), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(0), parser_types.r(parser_types.nt(4), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.r(parser_types.nt(4), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(11), parser_types.r(parser_types.nt(4), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.r(parser_types.nt(4), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(3), parser_types.r(parser_types.nt(4), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(10), parser_types.r(parser_types.nt(4), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(8), parser_types.r(parser_types.nt(4), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.s(16u32)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(6), parser_types.s(17u32)),
  (parser_types.t(7), parser_types.s(17u32)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(2), parser_types.r(parser_types.nt(10), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.r(parser_types.nt(10), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(0), parser_types.r(parser_types.nt(10), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.r(parser_types.nt(10), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(11), parser_types.r(parser_types.nt(10), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.r(parser_types.nt(10), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(3), parser_types.r(parser_types.nt(10), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(10), parser_types.r(parser_types.nt(10), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(8), parser_types.r(parser_types.nt(10), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(2), parser_types.r(parser_types.nt(2), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.r(parser_types.nt(2), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(0), parser_types.r(parser_types.nt(2), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.r(parser_types.nt(2), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(11), parser_types.r(parser_types.nt(2), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.r(parser_types.nt(2), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(3), parser_types.r(parser_types.nt(2), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(10), parser_types.r(parser_types.nt(2), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(8), parser_types.r(parser_types.nt(2), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(2), parser_types.r(parser_types.nt(4), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.r(parser_types.nt(4), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(0), parser_types.r(parser_types.nt(4), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(9), parser_types.r(parser_types.nt(4), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(11), parser_types.r(parser_types.nt(4), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(4), parser_types.r(parser_types.nt(4), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(3), parser_types.r(parser_types.nt(4), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(8), parser_types.r(parser_types.nt(4), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(10), parser_types.r(parser_types.nt(4), 3u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(5), parser_types.r(parser_types.nt(11), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(1), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
  (parser_types.t(6), parser_types.r(parser_types.nt(11), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(7), parser_types.r(parser_types.nt(11), 2u32, 0, 0, none(parser_types.Associativity), 0)),
  (parser_types.t(12), parser_types.ParseAction(kind: parser_types.pakShiftExtra)),
]

const parseTableGotos* = @[
  (parser_types.nt(5), 3u32),
  (parser_types.nt(8), 3u32),
  (parser_types.nt(1), 6u32),
  (parser_types.nt(7), 3u32),
  (parser_types.nt(3), 3u32),
  (parser_types.nt(4), 3u32),
  (parser_types.nt(2), 3u32),
  (parser_types.nt(10), 6u32),
  (parser_types.nt(6), 3u32),
  (parser_types.nt(0), 2u32),
  (parser_types.nt(5), 3u32),
  (parser_types.nt(7), 3u32),
  (parser_types.nt(1), 11u32),
  (parser_types.nt(3), 3u32),
  (parser_types.nt(8), 3u32),
  (parser_types.nt(4), 3u32),
  (parser_types.nt(2), 3u32),
  (parser_types.nt(6), 3u32),
  (parser_types.nt(10), 11u32),
  (parser_types.nt(11), 13u32),
  (parser_types.nt(5), 3u32),
  (parser_types.nt(8), 3u32),
  (parser_types.nt(1), 14u32),
  (parser_types.nt(7), 3u32),
  (parser_types.nt(3), 3u32),
  (parser_types.nt(4), 3u32),
  (parser_types.nt(2), 3u32),
  (parser_types.nt(6), 3u32),
  (parser_types.nt(5), 3u32),
  (parser_types.nt(1), 14u32),
  (parser_types.nt(7), 3u32),
  (parser_types.nt(3), 3u32),
  (parser_types.nt(8), 3u32),
  (parser_types.nt(4), 3u32),
  (parser_types.nt(2), 3u32),
  (parser_types.nt(6), 3u32),
]

const parseTableIndex* = [
  (actionStart: 0i32, actionLen: 10i32, gotoStart: 0i32, gotoLen: 10i32, lexState: 0i32),
  (actionStart: 10i32, actionLen: 11i32, gotoStart: 10i32, gotoLen: 0i32, lexState: 1i32),
  (actionStart: 21i32, actionLen: 3i32, gotoStart: 10i32, gotoLen: 0i32, lexState: 2i32),
  (actionStart: 24i32, actionLen: 11i32, gotoStart: 10i32, gotoLen: 0i32, lexState: 1i32),
  (actionStart: 35i32, actionLen: 9i32, gotoStart: 10i32, gotoLen: 9i32, lexState: 0i32),
  (actionStart: 44i32, actionLen: 5i32, gotoStart: 19i32, gotoLen: 1i32, lexState: 3i32),
  (actionStart: 49i32, actionLen: 10i32, gotoStart: 20i32, gotoLen: 8i32, lexState: 0i32),
  (actionStart: 59i32, actionLen: 11i32, gotoStart: 28i32, gotoLen: 0i32, lexState: 1i32),
  (actionStart: 70i32, actionLen: 11i32, gotoStart: 28i32, gotoLen: 0i32, lexState: 1i32),
  (actionStart: 81i32, actionLen: 11i32, gotoStart: 28i32, gotoLen: 0i32, lexState: 1i32),
  (actionStart: 92i32, actionLen: 11i32, gotoStart: 28i32, gotoLen: 0i32, lexState: 1i32),
  (actionStart: 103i32, actionLen: 10i32, gotoStart: 28i32, gotoLen: 8i32, lexState: 1i32),
  (actionStart: 113i32, actionLen: 11i32, gotoStart: 36i32, gotoLen: 0i32, lexState: 1i32),
  (actionStart: 124i32, actionLen: 5i32, gotoStart: 36i32, gotoLen: 0i32, lexState: 3i32),
  (actionStart: 129i32, actionLen: 11i32, gotoStart: 36i32, gotoLen: 0i32, lexState: 1i32),
  (actionStart: 140i32, actionLen: 11i32, gotoStart: 36i32, gotoLen: 0i32, lexState: 1i32),
  (actionStart: 151i32, actionLen: 11i32, gotoStart: 36i32, gotoLen: 0i32, lexState: 1i32),
  (actionStart: 162i32, actionLen: 5i32, gotoStart: 36i32, gotoLen: 0i32, lexState: 3i32),
]

const productionInfos* = @[
  parser_types.ProductionInfo(
    symbol: parser_types.nt(0),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(0),
    fieldCount: 0u32,
    childCount: 0u32,
    fieldNames: @[
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(1),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(1),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(1),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(1),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(1),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(1),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(1),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(2),
    fieldCount: 3u32,
    childCount: 3u32,
    fieldNames: @[
      "",
      "",
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(3),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(4),
    fieldCount: 3u32,
    childCount: 3u32,
    fieldNames: @[
      "",
      "",
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(4),
    fieldCount: 2u32,
    childCount: 2u32,
    fieldNames: @[
      "",
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(5),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(6),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(7),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(8),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(9),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(10),
    fieldCount: 2u32,
    childCount: 2u32,
    fieldNames: @[
      "",
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(10),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(11),
    fieldCount: 2u32,
    childCount: 2u32,
    fieldNames: @[
      "",
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(11),
    fieldCount: 2u32,
    childCount: 2u32,
    fieldNames: @[
      "",
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(11),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
  parser_types.ProductionInfo(
    symbol: parser_types.nt(11),
    fieldCount: 1u32,
    childCount: 1u32,
    fieldNames: @[
      "",
    ]
  ),
]


const lexStates* = @[
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 9,
        maxChar: 10,
        nextState: 4,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 13,
        maxChar: 13,
        nextState: 4,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 32,
        maxChar: 32,
        nextState: 4,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 34,
        maxChar: 34,
        nextState: 5,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 40,
        maxChar: 40,
        nextState: 6,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 45,
        maxChar: 45,
        nextState: 7,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 46,
        maxChar: 46,
        nextState: 8,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 48,
        maxChar: 57,
        nextState: 7,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 59,
        maxChar: 59,
        nextState: 9,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 64,
        maxChar: 64,
        nextState: 10,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 65,
        maxChar: 90,
        nextState: 11,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 95,
        maxChar: 95,
        nextState: 12,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 97,
        maxChar: 122,
        nextState: 11,
        isSeparator: false
      ),
    ],
    acceptSymbol: -1
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 9,
        maxChar: 10,
        nextState: 4,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 13,
        maxChar: 13,
        nextState: 4,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 32,
        maxChar: 32,
        nextState: 4,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 34,
        maxChar: 34,
        nextState: 5,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 40,
        maxChar: 40,
        nextState: 6,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 41,
        maxChar: 41,
        nextState: 13,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 45,
        maxChar: 45,
        nextState: 7,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 46,
        maxChar: 46,
        nextState: 8,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 48,
        maxChar: 57,
        nextState: 7,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 59,
        maxChar: 59,
        nextState: 9,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 64,
        maxChar: 64,
        nextState: 10,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 65,
        maxChar: 90,
        nextState: 11,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 95,
        maxChar: 95,
        nextState: 12,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 97,
        maxChar: 122,
        nextState: 11,
        isSeparator: false
      ),
    ],
    acceptSymbol: -1
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 9,
        maxChar: 10,
        nextState: 4,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 13,
        maxChar: 13,
        nextState: 4,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 32,
        maxChar: 32,
        nextState: 4,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 59,
        maxChar: 59,
        nextState: 9,
        isSeparator: false
      ),
    ],
    acceptSymbol: -1
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 0,
        maxChar: 8,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 9,
        maxChar: 9,
        nextState: 15,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 10,
        maxChar: 10,
        nextState: 4,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 11,
        maxChar: 12,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 13,
        maxChar: 13,
        nextState: 15,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 14,
        maxChar: 31,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 32,
        maxChar: 32,
        nextState: 15,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 33,
        maxChar: 33,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 34,
        maxChar: 34,
        nextState: 5,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 35,
        maxChar: 58,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 59,
        maxChar: 59,
        nextState: 16,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 60,
        maxChar: 91,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 92,
        maxChar: 92,
        nextState: 17,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 93,
        maxChar: 1114111,
        nextState: 14,
        isSeparator: false
      ),
    ],
    acceptSymbol: -1
  ),
  parser_types.LexState(
    transitions: @[
    ],
    acceptSymbol: 0
  ),
  parser_types.LexState(
    transitions: @[
    ],
    acceptSymbol: 4
  ),
  parser_types.LexState(
    transitions: @[
    ],
    acceptSymbol: 1
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 45,
        maxChar: 45,
        nextState: 7,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 48,
        maxChar: 57,
        nextState: 7,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 58,
        maxChar: 58,
        nextState: 18,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 65,
        maxChar: 90,
        nextState: 7,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 95,
        maxChar: 95,
        nextState: 7,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 97,
        maxChar: 122,
        nextState: 7,
        isSeparator: false
      ),
    ],
    acceptSymbol: -1
  ),
  parser_types.LexState(
    transitions: @[
    ],
    acceptSymbol: 10
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 0,
        maxChar: 9,
        nextState: 9,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 11,
        maxChar: 1114111,
        nextState: 9,
        isSeparator: false
      ),
    ],
    acceptSymbol: 11
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 45,
        maxChar: 45,
        nextState: 19,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 46,
        maxChar: 46,
        nextState: 19,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 48,
        maxChar: 57,
        nextState: 19,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 65,
        maxChar: 90,
        nextState: 19,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 95,
        maxChar: 95,
        nextState: 19,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 97,
        maxChar: 122,
        nextState: 19,
        isSeparator: false
      ),
    ],
    acceptSymbol: -1
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 45,
        maxChar: 45,
        nextState: 11,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 46,
        maxChar: 46,
        nextState: 20,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 48,
        maxChar: 57,
        nextState: 11,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 58,
        maxChar: 58,
        nextState: 18,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 65,
        maxChar: 90,
        nextState: 11,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 95,
        maxChar: 95,
        nextState: 11,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 97,
        maxChar: 122,
        nextState: 11,
        isSeparator: false
      ),
    ],
    acceptSymbol: 3
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 45,
        maxChar: 45,
        nextState: 11,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 46,
        maxChar: 46,
        nextState: 20,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 48,
        maxChar: 57,
        nextState: 11,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 58,
        maxChar: 58,
        nextState: 18,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 65,
        maxChar: 90,
        nextState: 11,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 95,
        maxChar: 95,
        nextState: 11,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 97,
        maxChar: 122,
        nextState: 11,
        isSeparator: false
      ),
    ],
    acceptSymbol: 3
  ),
  parser_types.LexState(
    transitions: @[
    ],
    acceptSymbol: 2
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 0,
        maxChar: 9,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 11,
        maxChar: 33,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 35,
        maxChar: 91,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 93,
        maxChar: 1114111,
        nextState: 14,
        isSeparator: false
      ),
    ],
    acceptSymbol: 5
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 0,
        maxChar: 9,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 11,
        maxChar: 33,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 35,
        maxChar: 91,
        nextState: 14,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 93,
        maxChar: 1114111,
        nextState: 14,
        isSeparator: false
      ),
    ],
    acceptSymbol: 0
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 0,
        maxChar: 9,
        nextState: 16,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 11,
        maxChar: 33,
        nextState: 16,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 34,
        maxChar: 34,
        nextState: 9,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 35,
        maxChar: 91,
        nextState: 16,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 92,
        maxChar: 92,
        nextState: 9,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 93,
        maxChar: 1114111,
        nextState: 16,
        isSeparator: false
      ),
    ],
    acceptSymbol: 5
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 0,
        maxChar: 9,
        nextState: 21,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 11,
        maxChar: 1114111,
        nextState: 21,
        isSeparator: false
      ),
    ],
    acceptSymbol: -1
  ),
  parser_types.LexState(
    transitions: @[
    ],
    acceptSymbol: 9
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 45,
        maxChar: 45,
        nextState: 19,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 46,
        maxChar: 46,
        nextState: 19,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 48,
        maxChar: 57,
        nextState: 19,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 65,
        maxChar: 90,
        nextState: 19,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 95,
        maxChar: 95,
        nextState: 19,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 97,
        maxChar: 122,
        nextState: 19,
        isSeparator: false
      ),
    ],
    acceptSymbol: 8
  ),
  parser_types.LexState(
    transitions: @[
      parser_types.LexTransition(
        minChar: 45,
        maxChar: 45,
        nextState: 20,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 46,
        maxChar: 46,
        nextState: 20,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 48,
        maxChar: 57,
        nextState: 20,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 65,
        maxChar: 90,
        nextState: 20,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 95,
        maxChar: 95,
        nextState: 20,
        isSeparator: false
      ),
      parser_types.LexTransition(
        minChar: 97,
        maxChar: 122,
        nextState: 20,
        isSeparator: false
      ),
    ],
    acceptSymbol: 3
  ),
  parser_types.LexState(
    transitions: @[
    ],
    acceptSymbol: 6
  ),
]

proc newLexer*(input: string): Lexer =
  result = Lexer(input: input, pos: 0, row: 0, col: 0)

proc advance(lexer: Lexer, count: int = 1) =
  for _ in 0..<count:
    if lexer.pos < lexer.input.len:
      if lexer.input[lexer.pos] == '\n':
        inc lexer.row
        lexer.col = 0
      else:
        inc lexer.col
      inc lexer.pos

proc nextToken*(lexer: Lexer, validExternalSymbols: set[int16] = {}, lexState: int = 0): Token =
  # Skip whitespace and extras (preserve start for scanning)
  let startPosBeforeSkip = lexer.pos
  
  # Skip whitespace
  while lexer.pos < lexer.input.len and lexer.input[lexer.pos] in {'\t', ' ', '\r', '\n'}:
    advance(lexer)
  # Check for EOF
  if lexer.pos >= lexer.input.len:
    let pt = Point(row: lexer.row, column: lexer.col)
    return Token(kind: terminal(int(tsEOF)), text: "", startPos: lexer.pos, endPos: lexer.pos, startPoint: pt, endPoint: pt)
  
  # Skip extras (tokens defined as skippable in grammar)
  var skipAgain = true
  while skipAgain:
    skipAgain = false
    
    # Skip whitespace again
    while lexer.pos < lexer.input.len and lexer.input[lexer.pos] in {'\t', ' ', '\r', '\n'}:
      advance(lexer)
    
    if lexer.pos >= lexer.input.len:
      let pt = Point(row: lexer.row, column: lexer.col)
      return Token(kind: terminal(int(tsEOF)), text: "", startPos: lexer.pos, endPos: lexer.pos, startPoint: pt, endPoint: pt)
    
    # Try to match each extra symbol
    # Try to skip extra: terminal(1)
    block tryExtra1:
      let extraStartPos = lexer.pos
      let extraStartRow = lexer.row
      let extraStartCol = lexer.col
      var extraState = lexState
      var extraLastAccept = -1
      var extraAcceptPos = extraStartPos
      var extraAcceptRow = extraStartRow
      var extraAcceptCol = extraStartCol
      
      while lexer.pos < lexer.input.len:
        let ch = lexer.input[lexer.pos].int
        
        if extraState < lexStates.len and lexStates[extraState].acceptSymbol >= 0:
          extraLastAccept = lexStates[extraState].acceptSymbol
          extraAcceptPos = lexer.pos
          extraAcceptRow = lexer.row
          extraAcceptCol = lexer.col
        
        var foundTrans = false
        if extraState < lexStates.len:
          for trans in lexStates[extraState].transitions:
            if ch >= trans.minChar and ch <= trans.maxChar:
              extraState = trans.nextState
              advance(lexer)
              foundTrans = true
              break
        
        if not foundTrans:
          break
      
      if extraState < lexStates.len and lexStates[extraState].acceptSymbol >= 0:
        extraLastAccept = lexStates[extraState].acceptSymbol
        extraAcceptPos = lexer.pos
        extraAcceptRow = lexer.row
        extraAcceptCol = lexer.col
      
      if extraLastAccept == 0:
        debugEchoMsg "Matched extra {extra.index}: " & lexer.input[startPosBeforeSkip..<extraAcceptPos]
        lexer.pos = extraAcceptPos
        lexer.row = extraAcceptRow
        lexer.col = extraAcceptCol
        skipAgain = true
        break tryExtra1
      else:
        # Not this extra, restore position
        lexer.pos = extraStartPos
        lexer.row = extraStartRow
        lexer.col = extraStartCol
    # Try to skip extra: terminal(12)
    block tryExtra12:
      let extraStartPos = lexer.pos
      let extraStartRow = lexer.row
      let extraStartCol = lexer.col
      var extraState = lexState
      var extraLastAccept = -1
      var extraAcceptPos = extraStartPos
      var extraAcceptRow = extraStartRow
      var extraAcceptCol = extraStartCol
      
      while lexer.pos < lexer.input.len:
        let ch = lexer.input[lexer.pos].int
        
        if extraState < lexStates.len and lexStates[extraState].acceptSymbol >= 0:
          extraLastAccept = lexStates[extraState].acceptSymbol
          extraAcceptPos = lexer.pos
          extraAcceptRow = lexer.row
          extraAcceptCol = lexer.col
        
        var foundTrans = false
        if extraState < lexStates.len:
          for trans in lexStates[extraState].transitions:
            if ch >= trans.minChar and ch <= trans.maxChar:
              extraState = trans.nextState
              advance(lexer)
              foundTrans = true
              break
        
        if not foundTrans:
          break
      
      if extraState < lexStates.len and lexStates[extraState].acceptSymbol >= 0:
        extraLastAccept = lexStates[extraState].acceptSymbol
        extraAcceptPos = lexer.pos
        extraAcceptRow = lexer.row
        extraAcceptCol = lexer.col
      
      if extraLastAccept == 11:
        debugEchoMsg "Matched extra {extra.index}: " & lexer.input[startPosBeforeSkip..<extraAcceptPos]
        lexer.pos = extraAcceptPos
        lexer.row = extraAcceptRow
        lexer.col = extraAcceptCol
        skipAgain = true
        break tryExtra12
      else:
        # Not this extra, restore position
        lexer.pos = extraStartPos
        lexer.row = extraStartRow
        lexer.col = extraStartCol
  
  let startPos = lexer.pos
  let startPoint = Point(row: lexer.row, column: lexer.col)
  var state = lexState
  var lastAcceptPos = startPos
  var lastAcceptRow = lexer.row
  var lastAcceptCol = lexer.col
  var lastAcceptSymbol = -1
  
  # DFA traversal
  while lexer.pos < lexer.input.len:
    let ch = lexer.input[lexer.pos].int
    
    # Check if current state accepts
    if state < lexStates.len and lexStates[state].acceptSymbol >= 0:
      lastAcceptPos = lexer.pos
      lastAcceptRow = lexer.row
      lastAcceptCol = lexer.col
      lastAcceptSymbol = lexStates[state].acceptSymbol
    
    # Find matching transition
    var foundTransition = false
    if state < lexStates.len:
      for trans in lexStates[state].transitions:
        if ch >= trans.minChar and ch <= trans.maxChar:
          state = trans.nextState
          advance(lexer)
          foundTransition = true
          break
    
    # No transition found - stop
    if not foundTransition:
      break
  
  # Check final state
  if state < lexStates.len and lexStates[state].acceptSymbol >= 0:
    lastAcceptPos = lexer.pos
    lastAcceptRow = lexer.row
    lastAcceptCol = lexer.col
    lastAcceptSymbol = lexStates[state].acceptSymbol
  
  # Return token if we accepted
  if lastAcceptSymbol >= 0:
    let text = lexer.input[startPos..<lastAcceptPos]
    lexer.pos = lastAcceptPos
    lexer.row = lastAcceptRow
    lexer.col = lastAcceptCol
    return Token(kind: terminal(lastAcceptSymbol + 1), text: text, startPos: startPos, endPos: lastAcceptPos, startPoint: startPoint, endPoint: Point(row: lexer.row, column: lexer.col))
  
  # No match - error token (consume one char)
  advance(lexer)
  return Token(kind: terminal(int(tsEOF)), text: lexer.input[startPos..<lexer.pos], startPos: startPos, endPos: lexer.pos, startPoint: startPoint, endPoint: Point(row: lexer.row, column: lexer.col))

const externalTokenBase* = 13
const externalExtraTokens*: set[int16] = {}
include parser_runtime

proc newParser*(input: string): Parser =
  var parser = Parser(
    lexer: newLexer(input),
    stacks: @[]
  )
  parser.stacks.add(@[(state: 0, node: ParseNode(nil))])  # Initial state
  # Calculate valid external symbols for initial state (State 0)
  var validExternal: set[int16] = {}
  if parseTableIndex.len > 0:
    let idx = parseTableIndex[0]
    for i in 0 ..< idx.actionLen:
      let (sym, _) = parseTableActions[idx.actionStart + i]
      if sym.kind == skTerminal:
        if sym.terminalIndex >= externalTokenBase:
          validExternal.incl(sym.terminalIndex.int16)
  parser.lookahead = parser.lexer.nextToken(validExternal, parseTableIndex[0].lexState)
  return parser

proc parse*(parser: var Parser): ParseNode =
  return runGenericGLR(parser)

proc parseSexp*(input: string): ParseNode =
  var parser = newParser(input)
  return parser.parse()
