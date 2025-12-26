## Grammar data structures representing different stages of the generation pipeline.

import rules, nfa, options
import std/tables as stdtables

type
  VariableType* = enum
    ## Classifies the role of a grammar variable (Rule).
    vtHidden     ## Internal rule, not exposed in the CST (prefixed with `_`).
    vtAuxiliary  ## Generated helper rule (e.g. for repetition or sequences).
    vtAnonymous  ## String literal or pattern (e.g. "if", "\d+").
    vtNamed      ## Standard named rule defined in the grammar.

  PrecedenceEntry* = object
    ## Helper structure for defining precedence orderings explicitly.
    case isName*: bool
    of true:
      name*: string
    of false:
      symbol*: string

  ReservedWordContext*[T] = object
    ## Defines a scope where certain words are reserved.
    name*: string
    reservedWords*: seq[T]

  Variable* = object
    ## Represents a high-level rule definition from the user's input grammar.
    name*: string
    kind*: VariableType
    rule*: Rule

  InputGrammar* = object
    ## The raw grammar definition provided by the user (or DSL).
    ## This is the input to the `prepareGrammar` phase.
    name*: string                                ## Examples: "json", "python".
    variables*: seq[Variable]                    ## List of all defined rules.
    extraSymbols*: seq[Rule]                     ## Whitespace, comments to be skipped.
    expectedConflicts*: seq[seq[string]]         ## Documented LR conflicts to suppress warnings.
    precedenceOrderings*: seq[seq[PrecedenceEntry]]
    externalTokens*: seq[Rule]                   ## Tokens handled by external scanner.
    variablesToInline*: seq[string]              ## Rules to inline into their callers.
    supertypeSymbols*: seq[string]               ## Rules grouped as supertypes (for query API).
    wordToken*: Option[string]                   ## Keyword tokenizer optimization.
    reservedWords*: seq[ReservedWordContext[Rule]]

  LexicalVariable* = object
    ## A refined variable for the Lexical Grammar (Lexer).
    ## These are typically Terminals (tokens).
    name*: string
    kind*: VariableType
    implicitPrecedence*: int32
    startState*: uint32          ## Starting state in the Lexer NFA/DFA.
    rule*: Rule

  LexicalGrammar* = object
    ## The portion of the grammar handled by the Lexer.
    ## Consists of Regex/String rules converted to NFA.
    nfa*: Nfa
    variables*: seq[LexicalVariable]

  ReservedWordSetId* = distinct uint

  ProductionStep* = object
    ## A single element in a production sequence (RHS).
    symbol*: GrammarSymbol
    precedence*: Precedence
    associativity*: Option[GrammarAssociativity]
    alias*: Option[Alias]
    fieldName*: Option[string]
    reservedWordSetId*: ReservedWordSetId

  Production* = object
    ## A single alternative in a Syntax Variable (e.g. one branch of a Choice).
    steps*: seq[ProductionStep]
    dynamicPrecedence*: int32
    precedence*: int32  # Static precedence
    associativity*: Option[GrammarAssociativity]

  InlinedProductionMap* = object
    productions*: seq[Production]
    productionMap*: stdtables.Table[(pointer, uint32), seq[int]]

  SyntaxVariable* = object
    ## A variable in the Syntax Grammar (Parser).
    ## Typically Non-Terminals defined by sequences of ProductionSteps.
    name*: string
    kind*: VariableType
    productions*: seq[Production]
    isNullable*: bool

  ExternalToken* = object
    name*: string
    kind*: VariableType
    correspondingInternalToken*: Option[GrammarSymbol]

  SyntaxGrammar* = object
    ## The processed grammar ready for Parse Table generation.
    ## Variables here are Non-Terminals composed of other Symbols (Terminals/Non-Terminals).
    variables*: seq[SyntaxVariable]
    extraSymbols*: seq[GrammarSymbol]
    expectedConflicts*: seq[seq[GrammarSymbol]]
    externalTokens*: seq[ExternalToken]
    variablesToInline*: seq[GrammarSymbol]
    supertypeSymbols*: seq[GrammarSymbol]
    wordToken*: Option[GrammarSymbol]
    precedenceOrderings*: seq[seq[PrecedenceEntry]]
    reservedWordSets*: seq[TokenSet]

const
  NoReservedWords* = ReservedWordSetId(high(uint))
