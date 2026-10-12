namespace RealTree.Jyro

open System
open System.Collections.Generic
open System.Runtime.InteropServices

/// <summary>
/// Severity level of a diagnostic message
/// </summary>
type MessageSeverity =
    | Info = 0
    | Warning = 1
    | Error = 2

/// <summary>
/// The stage of the pipeline that produced a diagnostic. A host reads it from <c>DiagnosticMessage.Stage</c> to know where to
/// look: a parse error is in the text, a link error in the names and the
/// functions the host gave, a runtime error in the data.
/// </summary>
type MessageStage =
    /// <summary>
    /// Decoding a <c>.jyrx</c> artifact, and every refusal to read one.
    /// </summary>
    | Deserialize = 0
    /// <summary>
    /// Reading the source text into a program.
    /// </summary>
    | Parse = 1
    /// <summary>
    /// The rules a program is checked against before it is linked.
    /// </summary>
    | Validate = 2
    /// <summary>
    /// Resolving the names of functions, and the checks that need them.
    /// </summary>
    | Link = 3
    /// <summary>
    /// Building the executable form, and writing an artifact.
    /// </summary>
    | Compile = 4
    /// <summary>
    /// Running the program.
    /// </summary>
    | Execute = 5

/// <summary>
/// Diagnostic message codes organized by compilation stage. A code has four
/// digits: the first names the pipeline stage, the second the category, and
/// the last two the error. It is written with the prefix JM, as in JM5200,
/// division by zero.
/// </summary>
type MessageCode =
    // Lexical Analysis Errors (1xxx)
    // 12xx - Literal errors
    /// <summary>
    /// A string literal has no closing quote (<c>"</c> or <c>'</c>) before the source ends.
    /// </summary>
    | UnterminatedString = 1200
    /// <summary>
    /// A string literal holds an escape the lexer does not know (<c>\q</c>, or <c>\u</c> without four hex digits), or half of a UTF-16 surrogate pair.
    /// </summary>
    | InvalidEscapeSequence = 1201

    // Parsing Errors (2xxx)
    // 20xx - General
    /// <summary>
    /// Catch-all for a parser syntax error other than an unterminated string or an invalid number literal.
    /// </summary>
    | UnknownParserError = 2000
    /// <summary>
    /// The source ended before the parser could read further; the place is the empty range after its last code unit, and there is no argument.
    /// </summary>
    | UnexpectedEndOfSource = 2001
    /// <summary>
    /// The source text nests deeper than the maximum nesting depth of 256; the one argument is the maximum, and the place is the first token of the first node or parenthesized expression above it.
    /// </summary>
    | SourceNestingLimitExceeded = 2002
    /// <summary>
    /// A <c>(</c> follows, on the same line, an expression that is not an identifier written without parentheses: a call is made only on a name; the place is the <c>(</c>, and there is no argument.
    /// </summary>
    | CallTargetNotName = 2003
    // 22xx - Literal/format errors
    /// <summary>
    /// A numeric literal is out of range: a hex/binary literal exceeds <c>Int64.MaxValue</c>, or a decimal literal evaluates to infinity or NaN.
    /// </summary>
    | InvalidNumberFormat = 2200

    // Validation Errors (3xxx)
    // 30xx - General
    // 31xx - Variable declaration
    /// <summary>
    /// An identifier names a variable that is not <c>Input</c>/<c>Output</c> and is not declared in the current or any enclosing scope.
    /// </summary>
    | UndeclaredVariable = 3100
    /// <summary>
    /// A <c>var</c> re-declares a name already bound in the same scope, or a function declaration reuses another function's name.
    /// </summary>
    | VariableAlreadyDeclared = 3101
    /// <summary>
    /// A script binds the name <c>Input</c> or <c>Output</c> as a variable, parameter, loop variable, or match binding; both are reserved for the host.
    /// </summary>
    | ReservedName = 3103
    // 32xx - Assignment / Delete
    /// <summary>
    /// The left-hand side of an assignment is not a variable, property, or index expression (e.g. a literal or call result).
    /// </summary>
    | InvalidAssignmentTarget = 3200
    /// <summary>
    /// The operand of <c>delete</c> is not a property or index expression; a bare variable or literal cannot be deleted.
    /// </summary>
    | InvalidDeleteTarget = 3201
    /// <summary>
    /// An assignment carries a type hint together with a compound operator (<c>+=</c>, <c>-=</c>, <c>*=</c>, <c>/=</c>, <c>%=</c>, <c>&amp;=</c>) instead of plain <c>=</c>.
    /// </summary>
    | TypeHintOnCompoundAssignment = 3202
    /// <summary>
    /// An assignment attaches a type hint to a name (or <c>obj.prop</c>) that was already assigned earlier in the script.
    /// </summary>
    | DuplicateTypeHint = 3203
    /// <summary>
    /// The operand of <c>++</c> or <c>--</c> is not a variable, property, or index expression.
    /// </summary>
    | InvalidIncrementTarget = 3204
    // 33xx - Type
    /// <summary>
    /// Either operand of <c>+</c> is a string literal, or a variable whose type hint or literal initializer marks it as a string.
    /// </summary>
    | StringOperandInAddition = 3301
    /// <summary>
    /// A <c>var</c> declares a type hint other than <c>any</c> but has no initializer expression.
    /// </summary>
    | TypedVariableNeedsInitializer = 3302
    // 34xx - Control flow
    /// <summary>
    /// A <c>break</c> or <c>continue</c> statement appears outside any <c>while</c>, <c>for</c>, or <c>foreach</c> loop.
    /// </summary>
    | LoopStatementOutsideOfLoop = 3400
    /// <summary>
    /// A <c>foreach</c> iterates over a literal that is <c>null</c>, a number, or a boolean, none of which are iterable.
    /// </summary>
    | NotIterableLiteral = 3402
    // 35xx - Functions
    /// <summary>
    /// A function is declared inside a loop, conditional, or switch block instead of at the top level of the script.
    /// </summary>
    | FunctionNotAtTopLevel = 3500
    /// <summary>
    /// A function is declared inside the body of another function.
    /// </summary>
    | NestedFunctionDeclaration = 3501
    /// <summary>
    /// Two parameters of the same function declaration share the same name.
    /// </summary>
    | DuplicateParameterName = 3502
    // 3503 was ReservedParameterName, which held the Input/Output rule for
    // named-function parameters and nowhere else - the other five sites at
    // which a script binds a name accepted both. The rule now has one code,
    // ReservedName (3103), in the band about names a script declares. Not
    // reused: a retired number stays retired so a host that logged one never
    // reads it back as something else - which is also why ReservedName is 3103
    // and not the free-looking 3102, retired from ReservedIdentifier.
    /// <summary>
    /// <c>Output</c> is referenced inside a function or lambda body, where the host's output object is not accessible.
    /// </summary>
    | OutputAccessInFunction = 3504
    /// <summary>
    /// An assignment, <c>delete</c>, or <c>++</c>/<c>--</c> targets <c>Input</c> or a path rooted at it (e.g. <c>Input.a = 1</c>), which the host owns read-only.
    /// </summary>
    | InputIsReadOnly = 3508
    /// <summary>
    /// A <c>return</c> statement appears outside any function body.
    /// </summary>
    | ReturnOutsideFunction = 3505
    /// <summary>
    /// A required parameter with no default value follows an optional parameter in the same function's parameter list.
    /// </summary>
    | RequiredParamAfterOptional = 3506
    /// <summary>
    /// A function parameter's default value is an expression other than a literal, e.g. a variable or a call.
    /// </summary>
    | DefaultValueNotLiteral = 3507
    /// <summary>
    /// A script compiled as a library has a top-level statement other than a function or union declaration.
    /// </summary>
    | LibraryStatementForbidden = 3509
    // 36xx - Unions/Match
    /// <summary>
    /// A union is declared inside a loop, conditional, or switch block instead of at the top level of the script.
    /// </summary>
    | UnionNotAtTopLevel = 3600
    /// <summary>
    /// A union declares a variant name already used by a different, previously declared union.
    /// </summary>
    | DuplicateVariantName = 3601
    /// <summary>
    /// A <c>match</c> with no <c>default</c> case does not cover every variant declared by the matched union.
    /// </summary>
    | MatchNotExhaustive = 3602
    /// <summary>
    /// The same variant name appears in two cases of the same <c>match</c>.
    /// </summary>
    | MatchDuplicateCase = 3603
    /// <summary>
    /// A match case binds a different number of names than the number of fields its variant declares.
    /// </summary>
    | MatchBindingCountMismatch = 3604
    /// <summary>
    /// A union is declared inside the body of a function.
    /// </summary>
    | UnionInsideFunction = 3605
    /// <summary>
    /// A match case names a variant that is not declared in any known union.
    /// </summary>
    | MatchUnknownVariant = 3606
    /// <summary>
    /// A match case is named <c>default</c>; <c>match</c> has no fallback case, so cases must cover the union's variants explicitly.
    /// </summary>
    | MatchWithDefault = 3607
    /// <summary>
    /// A union declaration reuses a name already used by another union earlier in the program.
    /// </summary>
    | DuplicateUnionName = 3608
    /// <summary>
    /// A union variant declares a field named <c>_variant</c>, which is the tag every constructed variant carries.
    /// </summary>
    | ReservedFieldName = 3609
    /// <summary>
    /// A union variant declares two fields of the same name; a type hint does not change a field's name.
    /// </summary>
    | DuplicateFieldName = 3610
    /// <summary>
    /// A match case gives the same name at two of its positions; <c>_</c> is an ordinary name.
    /// </summary>
    | MatchDuplicateBinding = 3611

    // Linking Errors (4xxx)
    // 40xx - General
    // 4000 was UnknownLinkerError, a free-text catch-all; it is retired and
    // does not come back (JYRO-ADR-130).
    /// <summary>
    /// The call check of a host function (<c>IJyroCallCheck.CheckCall</c>) refused a call at link time;
    /// the arguments are the reason the host gave, then its values.
    /// </summary>
    | HostLinkError = 4001
    // 41xx - Function resolution
    /// <summary>
    /// A call, or an artifact relink's required-function check, names a function undefined by the script, any library, the stdlib, or the host.
    /// </summary>
    | UndefinedFunction = 4100
    /// <summary>
    /// Two functions, or two variant constructors, share a name in the same script or library; a cross-kind clash uses a different code.
    /// </summary>
    | DuplicateFunction = 4101
    /// <summary>
    /// A script or library function declaration reuses the name of a built-in function: a name of the
    /// standard library inventory, whether or not the host added the standard library; a
    /// function the host application registered; or a further standard library function.
    /// </summary>
    | FunctionOverride = 4102
    /// <summary>
    /// A script function declaration reuses the name of a function already defined by a loaded library.
    /// </summary>
    | FunctionShadowsLibrary = 4103
    /// <summary>
    /// A called function's effect exceeds the maximum effect the host allows for that script or library context.
    /// </summary>
    | FunctionEffectExceedsMax = 4104
    /// <summary>
    /// A host-registered custom function has the same name as an included standard-library function.
    /// </summary>
    | HostFunctionOverridesStdlib = 4105
    /// <summary>
    /// The maximum effect cannot be applied to a host function: reading its <c>Effect</c> threw, or gave
    /// none of the four effects.
    /// </summary>
    | MaxEffectNotApplicable = 4106
    // 42xx - Argument validation
    /// <summary>
    /// A call supplies fewer positional arguments than the function's minimum parameter count.
    /// </summary>
    | TooFewArguments = 4200
    /// <summary>
    /// A call supplies more positional arguments than the function's maximum parameter count.
    /// </summary>
    | TooManyArguments = 4201
    /// <summary>
    /// The same named argument is supplied more than once in a single call.
    /// </summary>
    | DuplicateNamedArgument = 4202
    /// <summary>
    /// A named argument's key does not match any parameter of the called function.
    /// </summary>
    | UnknownNamedArgument = 4203
    /// <summary>
    /// A call using named arguments omits a required, non-optional parameter.
    /// </summary>
    | MissingRequiredNamedArgument = 4204
    // 43xx - Union linking
    /// <summary>
    /// A union variant's constructor name matches an already-registered function name.
    /// </summary>
    | VariantShadowsFunction = 4300
    /// <summary>
    /// A function declaration's name matches an already-declared union variant constructor.
    /// </summary>
    | FunctionShadowsVariant = 4301

    // Execution / Runtime Errors (5xxx)
    // 50xx - General / System
    // 5000 was ScriptReturn, thrown by `exit` inside a function and caught by
    // execute() as clean termination. It was control flow in the diagnostic
    // catalog: unreportable, untranslatable, and carved out by name from
    // every test that walks this enum. It is now JyroScriptExit, an exception
    // type of its own.
    // RuntimeError is the catch-all the runtime assigns to any exception that
    // escapes a statement unhandled - including a host function that crashed.
    // A host function reporting a deliberate failure should use
    // HostFunctionError instead, so the two stay distinguishable. The one
    // standard library use of it by name is `Split` with an empty separator
    // (spec 91.8.7), which names no code: it gives the diagnostic `Replace`
    // gives for an empty `oldValue` (91.4.7), the runtime's own for the
    // exception the platform raises there.
    /// <summary>
    /// An unhandled exception of the host platform escaped script execution, such as one thrown by a crashing host function.
    /// </summary>
    | RuntimeError = 5002
    /// <summary>
    /// The host requested cancellation through the execution's <c>CancellationToken</c>.
    /// </summary>
    | CanceledByHost = 5003
    /// <summary>
    /// A script executed a <c>fail</c> statement, with or without an explicit message.
    /// </summary>
    | ScriptFailure = 5004
    /// <summary>
    /// A write targeted an array or object the host froze with <c>Freeze()</c>, other than the Input root.
    /// </summary>
    | ValueIsReadOnly = 5005
    /// <summary>
    /// A host function threw an exception made by <c>JyroError.Host</c> to report a
    /// deliberate failure; the arguments are its reason, then its values.
    /// </summary>
    | HostFunctionError = 5006
    /// <summary>
    /// A write reached the script's Input, which is read-only. Distinct from
    /// ValueIsReadOnly so a script author is told which read-only value they hit. The validator catches a direct <c>Input.x = 1</c> earlier and
    /// more precisely (InputIsReadOnly, at validation time); this one covers the
    /// case it cannot see - a write reached through an alias, since <c>var t =
    /// Input.a</c> copies the reference, not the value.
    /// </summary>
    | InputValueIsReadOnly = 5007

    // 51xx - Type & Casting
    /// <summary>
    /// A value assigned to a type-hinted variable does not match the hint and cannot be coerced to it.
    /// </summary>
    | InvalidType = 5100
    /// <summary>
    /// An <c>As*</c>/<c>To*</c> conversion was called for a type the runtime value cannot be converted to.
    /// </summary>
    | InvalidCast = 5102
    /// <summary>
    /// A function implementation read a positional argument at an index beyond what the caller supplied.
    /// </summary>
    | ArgumentNotProvided = 5103
    /// <summary>
    /// A supplied argument's runtime type does not match the type a function's implementation requires.
    /// </summary>
    | ArgumentTypeMismatch = 5104
    /// <summary>
    /// A higher-order function's callback argument is null or is not a lambda value.
    /// </summary>
    | CallbackExpected = 5105
    /// <summary>
    /// A lambda callback's declared parameter count does not match the number of arguments the caller passes it.
    /// </summary>
    | CallbackArityMismatch = 5106

    // 52xx - Arithmetic & Operators
    /// <summary>
    /// The right operand of <c>/</c> evaluated to zero.
    /// </summary>
    | DivisionByZero = 5200
    /// <summary>
    /// The right operand of <c>%</c> evaluated to zero.
    /// </summary>
    | ModuloByZero = 5201
    /// <summary>
    /// An arithmetic or relational operator (+, -, *, /, %, &lt;, &lt;=, &gt;, &gt;=) was applied to operand types that do not support it.
    /// </summary>
    | UnsupportedBinaryOperation = 5206
    /// <summary>
    /// The unary <c>-</c> operator was applied to a value that is not a number.
    /// </summary>
    | UnsupportedUnaryOperation = 5207
    /// <summary>
    /// An operand of the <c>&amp;</c> operator, or an element of <c>Join</c>'s <c>values</c>, is an array, an object or a function,
    /// which has no string form to concatenate. Its arguments are the operation (<c>&amp;</c>, or <c>Join()</c>) and the type.
    /// </summary>
    | ConcatWithInvalidType = 5209
    /// <summary>
    /// An operand of the <c>+</c> operator is a string; <c>+</c> is numeric-only, use <c>&amp;</c> to concatenate.
    /// </summary>
    | StringOperandInAdditionRuntime = 5210

    // 53xx - Index & Property Access
    /// <summary>
    /// A position in an array or a string is negative or beyond the operand's valid bound: an array
    /// index to write at, insert at or remove, or a position a standard library function is given.
    /// </summary>
    | IndexOutOfRange = 5300
    /// <summary>
    /// A write index is the wrong type for its container: not a number for an array, not a string for an object.
    /// </summary>
    | InvalidIndexType = 5308
    /// <summary>
    /// A property write (<c>x.y = v</c>) targeted a value that is not an object.
    /// </summary>
    | SetPropertyOnNonObject = 5306
    /// <summary>
    /// An index write (<c>x[i] = v</c>) targeted a value that is neither an array nor an object.
    /// </summary>
    | SetIndexOnNonContainer = 5307
    /// <summary>
    /// A <c>delete</c> targeted a value that is not an object, such as an array element (<c>delete a[0]</c>).
    /// </summary>
    | DeleteOnNonObject = 5309
    /// <summary>
    /// The index of <c>s[i]</c>, <c>a[i]</c> or <c>a[i] = v</c> is a number that is not an
    /// integer. Its own code rather than
    /// <c>IntegerRequired</c>, whose arguments name a function and a parameter that
    /// an index operator does not have: the one argument is
    /// the index.
    /// </summary>
    | IntegerIndexRequired = 5310

    // 54xx - Type Checking & Iteration
    /// <summary>
    /// A <c>foreach</c> loop's collection expression evaluated to a value that is not an array, object, or string.
    /// </summary>
    | NotIterable = 5402
    /// <summary>
    /// The subject of a <c>match</c> is not a value of a variant that one of its cases names.
    /// </summary>
    | MatchSubjectNotVariant = 5403

    // 55xx - Function & Script Resolution
    /// <summary>
    /// A user-function shell was invoked before its compiled body was attached; an internal invariant, not a script error.
    /// </summary>
    | UndefinedFunctionRuntime = 5500

    // 56xx - Schema Validation (stdlib)
    // 5600 and 5601 are retired (InvalidExpressionSyntax, InvalidNumberParse);
    // see tests/Jyro.CSharp.Tests/RetiredMessageCodes.txt.
    /// <summary>
    /// <c>ValidateSchema</c> refused its schema, whatever the data: a keyword that
    /// the standard library does not support, a keyword whose value does not
    /// have the form draft 2020-12 gives, a <c>$ref</c> that does not
    /// start with <c>#</c> - resolving it would read a file or open a network
    /// connection, which no script may do - or that
    /// does not resolve to an object or a boolean, and a chain of
    /// steps that returns to where it starts. The arguments are the
    /// function, the keyword, and its value or the step that closes the chain.
    /// </summary>
    | SchemaInvalid = 5602

    // 57xx - String & Pattern Operations (stdlib)
    /// <summary>
    /// A <c>Regex*</c> stdlib function's match ran longer than the fixed one-second timeout.
    /// </summary>
    | RegexTimeout = 5700
    /// <summary>
    /// A <c>Regex*</c> stdlib function received a pattern that is not valid regular expression syntax.
    /// </summary>
    | RegexInvalidPattern = 5701
    /// <summary>
    /// A string function was asked to build a string whose length comes from a
    /// count argument rather than from the size of an input, and longer than
    /// the maximum value size: <c>PadLeft</c>, <c>PadRight</c> and <c>RandomString</c>.
    /// </summary>
    | GeneratedLengthExceeded = 5702
    /// <summary>
    /// <c>RandomString</c> received an explicit but empty <c>characterSet</c> argument.
    /// </summary>
    | EmptyCharacterSet = 5703
    /// <summary>
    /// The first argument to <c>IndexOf</c> or <c>Contains</c> is neither a string nor an array (and not null).
    /// </summary>
    | StringOrArrayRequired = 5704
    /// <summary>
    /// The <c>padding</c> argument to <c>PadLeft</c> or <c>PadRight</c> is not exactly one UTF-16 code unit.
    /// </summary>
    | InvalidPadCharacter = 5705
    /// <summary>
    /// The argument to <c>Length</c> is a boolean, a number or a function value:
    /// neither a string, an array, an object nor null. Its arguments are the function and the actual
    /// type; the accepted types are the template's.
    /// </summary>
    | StringArrayOrObjectRequired = 5706

    // 58xx - Date & Time Operations (stdlib)
    // 5800 was DateParseError, raised by ParseDate on unparseable input.
    // ParseDate now returns null instead, so nothing could produce it; see the
    // parse-failure rule on FromJsonFunction.
    /// <summary>
    /// <c>FormatDate</c>'s format argument has a run of one ASCII letter that is
    /// not a format token (<c>yyyy</c>, <c>MM</c>, <c>M</c>, <c>dd</c>, <c>d</c>, <c>HH</c>, <c>H</c>,
    /// <c>mm</c>, <c>m</c>, <c>ss</c>, <c>s</c>, <c>fff</c>) and is not made only of <c>T</c> or only of
    /// <c>Z</c>, or a single quote with no closing single quote.
    /// The format is not a format string of the host platform: <c>dddd, yyyy</c> raises it.
    /// </summary>
    | DateFormatStringInvalid = 5801
    /// <summary>
    /// A date-string argument to <c>FormatDate</c>, <c>DateAdd</c>, <c>DateDiff</c>, or
    /// <c>DatePart</c> is a string the date reader cannot read, as <c>ParseDate</c>
    /// cannot: it does not match the RFC 3339 date and time grammar the
    /// standard library reads, or its UTC instant is outside the range
    /// <c>0001-01-01T00:00:00.000Z</c> to <c>9999-12-31T23:59:59.999Z</c>.
    /// </summary>
    | DateFormatInvalid = 5802
    // 5803 was DateAddAmountNotInteger, one function's private spelling of
    // IntegerRequired. Its template took no arguments, so it could not report
    // the value it rejected.
    /// <summary>
    /// The <c>unit</c> argument to <c>DateAdd</c> or <c>DateDiff</c> is not one of the
    /// fourteen unit names - day, days, week, weeks, month,
    /// months, year, years, hour, hours, minute, minutes, second, seconds -
    /// compared with no case sensitivity for ASCII letters only.
    /// </summary>
    | DateUnitInvalid = 5804
    /// <summary>
    /// The <c>part</c> argument to <c>DatePart</c> is not a recognized part (year, month, day, hour, minute, second, dayofweek, dayofyear).
    /// </summary>
    | DatePartInvalid = 5805
    /// <summary>
    /// A date computation landed outside the range a date can represent.
    ///
    /// The amount is a valid integer and the unit is a valid unit; it is the
    /// result that has nowhere to go - <c>DateAdd(d, 20000, "years")</c>. Distinct
    /// from <c>IntegerOutOfRange</c>, which is about the argument not fitting an
    /// <c>int</c>: this one is reached only after that check has passed, and the two
    /// are corrected differently.
    /// </summary>
    | DateOutOfRange = 5806

    // 59xx - Resource Limits, Encoding & Collection Operations
    /// <summary>
    /// The script executed more statements than the configured <c>MaxStatements</c> limit allows.
    /// </summary>
    | StatementLimitExceeded = 5900
    /// <summary>
    /// The cumulative loop-iteration count across the run exceeded <c>MaxLoopIterations</c>.
    /// </summary>
    | LoopIterationLimitExceeded = 5901
    /// <summary>
    /// A Jyro function call would push the call stack deeper than the configured <c>MaxCallDepth</c>.
    /// </summary>
    | CallDepthLimitExceeded = 5902
    /// <summary>
    /// Wall-clock execution time exceeded the configured <c>MaxExecutionTime</c> budget.
    /// </summary>
    | ExecutionTimeLimitExceeded = 5903
    /// <summary>
    /// A numeric argument expected to be a whole number has a fractional part.
    /// </summary>
    | IntegerRequired = 5904
    /// <summary>
    /// A relational query operator (<c>&lt;</c>, <c>&lt;=</c>, <c>&gt;</c>, <c>&gt;=</c>) compared two values that are not both numbers or both strings.
    /// </summary>
    | IncomparableTypes = 5905
    /// <summary>
    /// The operator string given to a query function like <c>WhereByField</c> or <c>CountIf</c> is not one of <c>==</c>, <c>!=</c>, <c>&lt;</c>, <c>&lt;=</c>, <c>&gt;</c>, <c>&gt;=</c>.
    /// </summary>
    | UnsupportedComparisonOperator = 5906
    /// <summary>
    /// A non-negative integer argument was negative, or a <c>for</c> loop's step was non-positive or non-integer.
    /// </summary>
    | NonNegativeIntegerRequired = 5907
    // 5908 was Base64DecodeError. Base64Decode now returns null on malformed
    // input rather than raising, so nothing could produce it.
    /// <summary>
    /// The <c>mode</c> argument to <c>Round</c> is not <c>halfUp</c>, <c>halfEven</c>, or <c>halfDown</c>.
    /// </summary>
    | InvalidRoundingMode = 5909
    /// <summary>
    /// A walk of a value went past the maximum nesting depth of 128: equality, cloning, freezing, serialization, flatten and the library's other walks; reading JSON text (the host's <c>FromJson</c> for text and for a stream, and the script's <c>FromJson</c>); converting a host value (<c>FromObject</c>, <c>WithInput</c>, <c>ExecuteScript</c>). A cyclic host object raises it at its back-reference, before the depth is reached.
    /// </summary>
    | ValueNestingLimitExceeded = 5910
    /// <summary>
    /// An integer argument was integral and in the right sign, but too large to
    /// convert. <c>IntegerRequired</c> cannot cover this: 1e20 is an integer, so
    /// the integrality check passes and the conversion then overflows.
    /// </summary>
    | IntegerOutOfRange = 5911
    // 5912 was InvalidRandomRange, which named one function for a condition
    // that belongs to any function taking an ordered pair. `Clamp` hit the same
    // condition and had nowhere to report it, so rather than mint a second code
    // for one concept the general form replaced it - see InvalidRange (5916).
    /// <summary>
    /// An aggregate was handed an array element that is neither a number nor
    /// null. Distinct from <c>ArgumentTypeMismatch</c>, which describes a whole
    /// argument and so has nowhere to report which element was wrong.
    /// </summary>
    | NumericElementRequired = 5913
    /// <summary>
    /// An argument fell outside the domain on which the operation is defined,
    /// so the result has no finite value - <c>Log(0)</c>, <c>SquareRoot(-1)</c>.
    ///
    /// Split from <c>NumberOverflow</c> for the reason <c>IntegerRequired</c> and
    /// <c>IntegerOutOfRange</c> are split: the two are corrected differently. A
    /// domain error is fixed by guarding the input, which a script can test
    /// for before calling; an overflow is fixed by scaling the computation,
    /// which it cannot.
    /// </summary>
    | NumberDomainError = 5914
    /// <summary>
    /// The result of an operation on finite operands has no representable
    /// finite magnitude - <c>Power(1e300, 3)</c>, <c>1e300 / 1e-28</c>.
    ///
    /// Reported rather than returned because a Jyro number is finite: see the
    /// rationale recorded for <c>DivisionByZero</c>, which is written about
    /// producing <c>Infinity</c> or <c>NaN</c> and not merely about a zero divisor.
    /// </summary>
    | NumberOverflow = 5915
    /// <summary>
    /// A function was given values that make no range - bounds that bound none,
    /// <c>RandomInt(5, 3)</c> and <c>RandomInt(3, 3)</c> (half-open), <c>Clamp(5, 10, 1)</c>,
    /// or a zero step, <c>Range(1, 5, 0)</c>.
    ///
    /// The arguments are the function, then each parameter's name and value in
    /// turn - "min/max" for <c>RandomInt</c> and for <c>Clamp</c>, <c>step</c>
    /// alone for <c>Range</c>, which has no second value and gives no second pair.
    /// </summary>
    | InvalidRange = 5916
    /// <summary>
    /// Text handed to the engine as JSON is not well-formed JSON: the text of
    /// <c>FromJson</c>, or of another host API member that reads JSON.
    /// </summary>
    | InvalidJson = 5917
    /// <summary>
    /// A string holds an unpaired surrogate, so an encoding step cannot carry
    /// it without changing it.
    ///
    /// A Jyro string is a sequence of UTF-16 code units, and a lone surrogate
    /// is a legal code unit that is not a legal character. It orders,
    /// compares and equals itself perfectly well, so unlike a non-finite number
    /// it is not broken as a value - it is specifically the conversion to UTF-8
    /// that has nowhere to put it. <c>Encoding.UTF8</c> and <c>Utf8JsonWriter</c> both
    /// would substitute the replacement character silently, which is a wrong
    /// answer reported as success; this is that step declining instead.
    /// </summary>
    | UnpairedSurrogate = 5918
    /// <summary>
    /// A string, an array or an object that an operation would make or grow
    /// is above the maximum value size, or a value a host
    /// converts is above it. The argument is the maximum.
    ///
    /// Its own code rather than <c>GeneratedLengthExceeded</c>, which reports a
    /// request made by a number argument and keeps its smaller bound of
    /// 1,000,000.
    /// </summary>
    | ValueSizeLimitExceeded = 5919
    /// <summary>
    /// The <c>direction</c> argument to <c>SortByField</c> is not <c>"asc"</c> or <c>"desc"</c>,
    /// with no case sensitivity: the check of one of a closed set of strings, as
    /// <c>InvalidRoundingMode</c> is.
    /// </summary>
    | InvalidSortDirection = 5920
    /// <summary>
    /// A host value given to a host API member, or as <c>Input</c>, holds a number
    /// that is NaN. No operation computed it, so it
    /// is not <c>NumberDomainError</c>, and it has no argument: the code says what
    /// is wrong.
    /// </summary>
    | HostNumberNaN = 5921
    /// <summary>
    /// A host value given to a host API member, or as <c>Input</c>, holds a number
    /// that is an infinity, positive or negative.
    /// No argument, as for <c>HostNumberNaN</c>.
    /// </summary>
    | HostNumberInfinite = 5922
    /// <summary>
    /// A host value given to a host API member, or as <c>Input</c>, is or holds a
    /// delegate, reflection metadata (<c>System.Type</c> and every other
    /// <c>MemberInfo</c>, <c>Assembly</c>, <c>Module</c>, <c>ParameterInfo</c>) or a task
    /// (<c>Task</c>, <c>Task&lt;T&gt;</c>, <c>ValueTask</c>, <c>ValueTask&lt;T&gt;</c>), which no Jyro value
    /// stands for. No argument, as for <c>HostNumberNaN</c>: nothing the specification names stands in that place.
    /// </summary>
    | HostValueNotConvertible = 5923

    // Compilation Errors (6xxx)
    // 60xx - General
    /// <summary>
    /// An unexpected exception of the host platform (not a <c>JyroRuntimeException</c> or a timeout) escaped the compiler pipeline.
    /// </summary>
    | UnknownCompilerError = 6000
    /// <summary>
    /// <c>JyroCompiler.Compile</c> or <c>JyroCompiler.CompileToArtifact</c> was called with no
    /// source text set.
    /// </summary>
    | NoSourceProvided = 6001
    /// <summary>
    /// A pipeline stage reported success but produced no program. An internal
    /// invariant violation, not a fault in the script.
    /// </summary>
    | InternalStageFailure = 6002
    /// <summary>
    /// Compilation itself exceeded the time budget. Distinct from
    /// <c>ExecutionTimeLimitExceeded</c>, a runtime code, so that a host routing
    /// by subsystem sends it to compilation: nothing had run yet.
    /// </summary>
    | CompilationTimeLimitExceeded = 6003

    // Artifact Errors (7xxx)
    // Writing and loading a .jyrx artifact is its own pipeline stage, so its
    // failures get their own band rather than borrowing the parser's.
    /// <summary>
    /// A <c>.jyrx</c> artifact's byte length exceeds the format's maximum file size, checked before parsing begins.
    /// </summary>
    | ArtifactTooLarge = 7000
    /// <summary>
    /// The first four bytes of a <c>.jyrx</c> artifact are not the magic bytes of the format.
    /// </summary>
    | ArtifactInvalidMagic = 7001
    /// <summary>
    /// The version field in a <c>.jyrx</c> header does not match the format version this build supports.
    /// </summary>
    | ArtifactVersionUnsupported = 7002
    /// <summary>
    /// Reading a <c>.jyrx</c> artifact failed after the size/magic/version checks passed - e.g. a truncated stream or oversized function count.
    /// </summary>
    | ArtifactUnreadable = 7003
    /// <summary>
    /// An artifact carries no record of the effect ceiling it was built under,
    /// so the loading host re-applies its own. Raised at load, not at write.
    /// </summary>
    | ArtifactEffectExceedsMax = 7004
    /// <summary>
    /// <c>CompileToArtifact()</c> was called with a library script configured; a <c>.jyrx</c> artifact cannot carry library context.
    /// </summary>
    | ArtifactCannotEncodeLibraries = 7005
    /// <summary>
    /// The load-side counterpart of 7005. An artifact can never reference a
    /// library function, because writing one with a library configured is
    /// refused, so a library supplied to a load is configuration that cannot
    /// apply. It is reported rather than dropped, so that no host believes a
    /// policy is in force that is not.
    /// </summary>
    | ArtifactCannotUseLibraries = 7006
    /// <summary>
    /// The program contains something the <c>.jyrx</c> encoding cannot represent -
    /// a string or list longer than the format's count fields can address, or
    /// a literal of a kind the encoding does not cover.
    ///
    /// A refusal the writer decided on, and therefore separate from
    /// <c>ArtifactUnwritable</c> below, on the same ground that <c>HostFunctionError</c>
    /// is separate from <c>RuntimeError</c>: one names a condition the engine
    /// recognized and can describe, the other is whatever escaped. Collapsing
    /// them would leave a host unable to tell "shorten this literal" from
    /// "report a bug".
    ///
    /// The write-side counterpart of the read side's specific codes (7000-7002).
    /// </summary>
    | ArtifactCannotEncode = 7007
    /// <summary>
    /// Something unexpected escaped while writing an artifact.
    ///
    /// The write-side mirror of <c>ArtifactUnreadable</c> (7003), so that a host
    /// routing on the code looks at the half of the pipeline that failed.
    /// </summary>
    | ArtifactUnwritable = 7008

// Text written on one line by a `ToString` (JYRO-REQ-1884).
module internal OneLine =
    // `text` with a backslash as two, and each control character (general
    // category Cc) and U+2028 and U+2029 as `\u` and the four lowercase
    // hexadecimal digits of its code point; a null `text` is empty.
    let escape (text: string | null) : string =
        match text with
        | null -> ""
        | text ->
            let sb = Text.StringBuilder(text.Length)
            for c in text do
                if c = '\\' then sb.Append("\\\\") |> ignore
                elif Char.IsControl c || int c = 0x2028 || int c = 0x2029 then
                    sb.Append("\\u").Append((int c).ToString("x4", Globalization.CultureInfo.InvariantCulture)) |> ignore
                else sb.Append(c) |> ignore
            sb.ToString()

/// <summary>
/// A place in the source text: a line, a column and the length of the span.
///
/// A struct with value equality and nothing structural.
/// No location has one spelling, a <c>SourceLocation?</c> with no value: there is
/// no <c>None</c> member standing in for one.
/// </summary>
[<Struct; CustomEquality; NoComparison>]
type SourceLocation(line: int, column: int, length: int) =
    /// <summary>
    /// The 1-based line number.
    /// </summary>
    member _.Line : int = line
    /// <summary>
    /// The 1-based column number.
    /// </summary>
    member _.Column : int = column
    /// <summary>
    /// The length of the span, in UTF-16 code units; 0 when only a point is
    /// known.
    /// </summary>
    member _.Length : int = length

    /// <summary>
    /// Whether <c>other</c> has the same line, column and length.
    /// </summary>
    member _.Equals(other: SourceLocation) : bool =
        line = other.Line && column = other.Column && length = other.Length

    /// <summary>
    /// Whether <c>other</c> is a <c>SourceLocation</c> equal to this one.
    /// </summary>
    override this.Equals(other: obj | null) : bool =
        match other with
        | :? SourceLocation as location -> this.Equals(location)
        | _ -> false

    override _.GetHashCode() : int = HashCode.Combine(line, column, length)

    /// <summary>
    /// <c>Ln L, Col C</c>, in decimal.
    /// </summary>
    override _.ToString() : string =
        String.Format(Globalization.CultureInfo.InvariantCulture, "Ln {0}, Col {1}", line, column)

    interface IEquatable<SourceLocation> with
        member this.Equals(other: SourceLocation) = this.Equals(other)

/// <summary>
/// Interface for hosts to provide localized message templates.
///
/// Return <c>false</c> for a code to fall back to the default English message.
/// Its one method is a <c>Try*</c> method with an <c>out</c> parameter, which a C#
/// host implements as it would any <c>TryGetValue</c>.
/// </summary>
type IMessageTemplateProvider =
    /// <summary>
    /// Attempts to resolve a localized template for <c>code</c>; returns false
    /// when none is available.
    /// </summary>
    abstract member TryGetTemplate: code: MessageCode * [<Out; System.Diagnostics.CodeAnalysis.MaybeNullWhen(false)>] template: byref<string> -> bool

// Default English message templates keyed by MessageCode, in the curried
// forms the five assemblies call. A host reaches them through the
// `MessageTemplates` type after this module, which has its name; F# compiles
// this one as `MessageTemplatesModule`, and resolves `MessageTemplates.get`
// here and `MessageTemplates.Get` on the type.
// Each template uses {0}, {1}, etc. for positional arguments.
module internal MessageTemplates =
    let private templates = Dictionary<MessageCode, string>()

    do
        // Lexer
        templates.[MessageCode.UnterminatedString] <- "Unterminated string literal"
        templates.[MessageCode.InvalidEscapeSequence] <- "Invalid escape sequence in string literal: {0}"

        // Parser
        templates.[MessageCode.UnknownParserError] <- "Syntax error: unexpected {0}"
        templates.[MessageCode.UnexpectedEndOfSource] <- "Syntax error: unexpected end of source"
        templates.[MessageCode.SourceNestingLimitExceeded] <- "Exceeded the maximum source nesting depth of {0}"
        templates.[MessageCode.CallTargetNotName] <- "A call must be made on a function name, not on another expression"
        templates.[MessageCode.InvalidNumberFormat] <- "Invalid number format: '{0}'"

        // Validator
        templates.[MessageCode.UndeclaredVariable] <- "Undeclared variable '{0}'"
        templates.[MessageCode.VariableAlreadyDeclared] <- "Variable '{0}' is already declared"
        templates.[MessageCode.InvalidAssignmentTarget] <- "Invalid assignment target"
        templates.[MessageCode.InvalidDeleteTarget] <- "Invalid delete target"
        templates.[MessageCode.TypeHintOnCompoundAssignment] <- "Type hint is not allowed on a compound assignment"
        templates.[MessageCode.DuplicateTypeHint] <- "Type hint on '{0}' must appear on its first assignment"
        templates.[MessageCode.InvalidIncrementTarget] <- "Invalid increment or decrement target"
        templates.[MessageCode.StringOperandInAddition] <- "The '+' operator does not accept a string operand"
        templates.[MessageCode.TypedVariableNeedsInitializer] <- "Typed variable '{0}' must have an initializer"
        // {0} is the statement's keyword as the source writes it.
        templates.[MessageCode.LoopStatementOutsideOfLoop] <- "'{0}' statement outside of loop"
        templates.[MessageCode.NotIterableLiteral] <- "Value of type {0} is not iterable"
        templates.[MessageCode.FunctionNotAtTopLevel] <- "Function '{0}' must be declared at the top level"
        templates.[MessageCode.NestedFunctionDeclaration] <- "Function declarations cannot be nested inside other functions"
        // {0} is the repeated parameter, {1} the function that declares it,
        // which a lambda does not have: last and bare, so the message reads
        // with or without it (JYRO-ADR-128 point 2).
        templates.[MessageCode.DuplicateParameterName] <- "Duplicate parameter '{0}' {1}"
        templates.[MessageCode.ReservedName] <- "Cannot bind '{0}'; Input and Output are supplied by the host"
        templates.[MessageCode.OutputAccessInFunction] <- "Cannot access Output inside a function or lambda body"
        templates.[MessageCode.InputIsReadOnly] <- "Cannot modify Input."
        templates.[MessageCode.ReturnOutsideFunction] <- "'return' is only valid inside a function"
        templates.[MessageCode.RequiredParamAfterOptional] <- "Required parameter '{0}' cannot follow optional parameter '{1}' in function '{2}'"
        templates.[MessageCode.DefaultValueNotLiteral] <- "Default value for parameter '{0}' in function '{1}' must be a literal"
        templates.[MessageCode.LibraryStatementForbidden] <- "A library script may hold only function and union declarations"
        templates.[MessageCode.UnionNotAtTopLevel] <- "Union '{0}' must be declared at the top level"
        templates.[MessageCode.DuplicateVariantName] <- "Variant '{0}' is already defined in union '{1}'"
        templates.[MessageCode.MatchNotExhaustive] <- "Match on union '{0}' is not exhaustive; missing case(s): {1} {2} {3} {4} {5} {6} {7} {8} {9}"
        templates.[MessageCode.MatchDuplicateCase] <- "Duplicate case '{0}' in match statement"
        templates.[MessageCode.MatchBindingCountMismatch] <- "Case '{0}' has {1} binding(s) but variant '{0}' has {2} field(s)"
        templates.[MessageCode.UnionInsideFunction] <- "Union declarations cannot be inside functions"
        templates.[MessageCode.MatchUnknownVariant] <- "Unknown variant '{0}' in match case"
        templates.[MessageCode.MatchWithDefault] <- "'default' is not allowed in a match"
        templates.[MessageCode.DuplicateUnionName] <- "Union '{0}' is already defined"
        templates.[MessageCode.ReservedFieldName] <- "Field name '_variant' in variant '{0}' is reserved for the variant tag"
        templates.[MessageCode.DuplicateFieldName] <- "Field '{0}' is declared more than once in variant '{1}'"
        templates.[MessageCode.MatchDuplicateBinding] <- "Name '{0}' is bound more than once in case '{1}'"

        // Linker
        templates.[MessageCode.UndefinedFunction] <- "Undefined function '{0}'"
        templates.[MessageCode.DuplicateFunction] <- "Function '{0}' is already defined"
        templates.[MessageCode.FunctionOverride] <- "Function '{0}' overrides a built-in function"
        templates.[MessageCode.FunctionShadowsLibrary] <- "Function '{0}' is already provided by a library"
        templates.[MessageCode.HostFunctionOverridesStdlib] <- "Custom function '{0}' has the same name as a standard-library function"
        templates.[MessageCode.FunctionEffectExceedsMax] <- "Call to '{0}' with effect {1} exceeds the maximum effect {2}"
        // {0} is the function, {1} the maximum in force (JYRO-REQ-1723).
        templates.[MessageCode.MaxEffectNotApplicable] <- "The maximum effect {1} cannot be applied to '{0}': its effect cannot be read"
        templates.[MessageCode.TooFewArguments] <- "Function '{0}' requires at least {1} arguments, but {2} were provided"
        templates.[MessageCode.TooManyArguments] <- "Function '{0}' accepts at most {1} arguments, but {2} were provided"
        templates.[MessageCode.DuplicateNamedArgument] <- "Duplicate named argument '{0}' in call to '{1}'"
        templates.[MessageCode.UnknownNamedArgument] <- "Unknown parameter '{0}' in call to '{1}'"
        templates.[MessageCode.MissingRequiredNamedArgument] <- "Required parameter '{0}' not provided in call to '{1}'"
        // {0} is the reason a host function's call check gave to
        // `JyroError.Host`, a name such as TableUnknown; {1} on are its values
        // (JYRO-ADR-130), as for HostFunctionError.
        templates.[MessageCode.HostLinkError] <- "A host check refused a call with {0} {1} {2} {3} {4} {5} {6} {7} {8} {9}"
        templates.[MessageCode.VariantShadowsFunction] <- "Variant constructor '{0}' shadows built-in function '{0}'"
        templates.[MessageCode.FunctionShadowsVariant] <- "Function '{0}' shadows variant constructor '{0}'"

        // Runtime - General
        // {0} is the function whose call raised it, with `()`; a RuntimeError
        // that no call raised has no argument (spec 130.6.2).
        templates.[MessageCode.RuntimeError] <- "Execution stopped on an unexpected error {0}"
        templates.[MessageCode.CanceledByHost] <- "Script execution was canceled by the host"
        templates.[MessageCode.ScriptFailure] <- "{0}"
        templates.[MessageCode.ValueIsReadOnly] <- "Cannot modify a read-only value"
        // {0} is the reason a host function gave to `JyroError.Host`, a
        // name such as AmountNotPositive; {1} on are its values
        // (JYRO-ADR-124). The default names the reason and the first nine
        // values; every value stays in the arguments, and a host that wants a
        // sentence makes it in its own template from the reason.
        templates.[MessageCode.HostFunctionError] <- "A host function reported {0} {1} {2} {3} {4} {5} {6} {7} {8} {9}"
        templates.[MessageCode.InputValueIsReadOnly] <- "Cannot write to Input."

        // Runtime - Type & Casting
        // {0} is the value's type as TypeOf gives it, {1} the hinted type. No
        // argument names the target, which the diagnostic's place shows
        // (spec 30.12.9, JYRO-ADR-153 (3)).
        templates.[MessageCode.InvalidType] <- "Cannot give a value of type {0} to a target of type {1}"
        templates.[MessageCode.InvalidCast] <- "Cannot cast {0} to {1}"
        templates.[MessageCode.ArgumentNotProvided] <- "Argument {0} not provided"
        templates.[MessageCode.ArgumentTypeMismatch] <- "Expected {0} but got {1}"
        templates.[MessageCode.CallbackExpected] <- "{0} expected a lambda callback but got {1}"
        templates.[MessageCode.CallbackArityMismatch] <- "{0} calls its callback with {1} argument(s), but the callback declares {2}"

        // Runtime - Arithmetic & Operators
        templates.[MessageCode.DivisionByZero] <- "Division by zero"
        templates.[MessageCode.ModuloByZero] <- "Modulo by zero"
        templates.[MessageCode.UnsupportedBinaryOperation] <- "Unsupported binary operation {0} for {1} and {2}"
        templates.[MessageCode.UnsupportedUnaryOperation] <- "Unsupported unary operation {0} for {1}"
        templates.[MessageCode.ConcatWithInvalidType] <- "{0} cannot convert {1} to string"
        templates.[MessageCode.StringOperandInAdditionRuntime] <- "The '+' operator requires numeric operands but received a string"

        // Runtime - Index & Property Access
        // Raised for a position in an array or in a string (spec 90.3.4), so
        // the fixed words name neither kind of operand.
        templates.[MessageCode.IndexOutOfRange] <- "{0} position {1} is out of bounds for a length of {2}"
        templates.[MessageCode.SetPropertyOnNonObject] <- "Cannot set property '{0}' on value of type {1}"
        templates.[MessageCode.SetIndexOnNonContainer] <- "Cannot set index on value of type {0}"
        templates.[MessageCode.DeleteOnNonObject] <- "Cannot delete '{0}' from a value of type {1}; delete removes an object property (use RemoveAt for an array)"
        templates.[MessageCode.IntegerIndexRequired] <- "An index must be an integer. Received: {0}"
        templates.[MessageCode.InvalidIndexType] <- "Cannot write to a value of type {0} at an index of type {1}; an array is indexed by a number and an object by a string"

        // Runtime - Type Checking & Iteration
        templates.[MessageCode.NotIterable] <- "Value of type {0} is not iterable"
        templates.[MessageCode.MatchSubjectNotVariant] <- "The subject of a match is not a value of a variant that one of its cases names"

        // Runtime - Function & Script Resolution
        templates.[MessageCode.UndefinedFunctionRuntime] <- "Undefined function '{0}'"

        // Runtime - Schema Validation
        templates.[MessageCode.SchemaInvalid] <- "{0}: invalid schema - keyword '{1}' has value '{2}'"

        // Runtime - String & Pattern Operations
        templates.[MessageCode.RegexTimeout] <- "{0}: Pattern matching timed out"
        templates.[MessageCode.RegexInvalidPattern] <- "{0}: Invalid regex pattern '{1}'"
        templates.[MessageCode.GeneratedLengthExceeded] <- "{0} length {1} exceeds maximum allowed value of {2}"
        templates.[MessageCode.EmptyCharacterSet] <- "{0} character set cannot be empty"
        templates.[MessageCode.StringOrArrayRequired] <- "{0} requires a string or array as first argument"
        templates.[MessageCode.InvalidPadCharacter] <- "{0} requires a pad character of exactly one code unit. Received: '{1}'"
        templates.[MessageCode.StringArrayOrObjectRequired] <- "{0} requires a string, an array, an object or null, but got {1}"

        // Runtime - Date & Time Operations
        templates.[MessageCode.DateFormatStringInvalid] <- "Invalid date format string: '{0}'"
        templates.[MessageCode.DateFormatInvalid] <- "Invalid date format for {1}: '{0}'"
        templates.[MessageCode.DateUnitInvalid] <- "Invalid date unit: '{0}'. Valid units: day, days, week, weeks, month, months, year, years, hour, hours, minute, minutes, second, seconds"
        templates.[MessageCode.DatePartInvalid] <- "Invalid date part: '{0}'. Valid parts: year, month, day, hour, minute, second, dayofweek, dayofyear"
        templates.[MessageCode.DateOutOfRange] <- "{0} produced a date outside the representable range: {1} {2} from '{3}'"

        // Runtime - Resource Limits, Encoding & Collection
        templates.[MessageCode.StatementLimitExceeded] <- "Script execution exceeded maximum statement limit of {0}"
        templates.[MessageCode.LoopIterationLimitExceeded] <- "Script execution exceeded maximum loop iteration limit of {0}"
        templates.[MessageCode.CallDepthLimitExceeded] <- "Script execution exceeded maximum call depth limit of {0}"
        templates.[MessageCode.ExecutionTimeLimitExceeded] <- "Script execution exceeded maximum time limit of {0}ms"
        templates.[MessageCode.CompilationTimeLimitExceeded] <- "Compilation exceeded maximum time limit of {0}ms"
        templates.[MessageCode.IntegerRequired] <- "{0} requires an integer {1}. Received: {2}"
        templates.[MessageCode.IncomparableTypes] <- "Cannot compare {0} and {1}"
        templates.[MessageCode.UnsupportedComparisonOperator] <- "Unsupported comparison operator: '{0}'. Supported operators are: ==, !=, <, <=, >, >="
        // {1} is the parameter's name (or `step` of a `for`), {2} the value
        // received and {3} the least value accepted: 0, or 1 for a step.
        templates.[MessageCode.NonNegativeIntegerRequired] <- "{0} requires an integer {1} of at least {3}. Received: {2}"
        // {0} is the mode given, which a function value has no text for, so
        // it can be absent (spec 130.6.2, JYRO-ADR-128 item 1). Unquoted, so
        // the shape without it does not read as the empty-string mode.
        templates.[MessageCode.InvalidRoundingMode] <- "Invalid rounding mode {0}. Valid modes: halfUp, halfEven, halfDown"
        templates.[MessageCode.ValueNestingLimitExceeded] <- "Exceeded the maximum value nesting depth of {0}"
        // {0} is the value, {1} and {2} the least and the greatest, then the
        // function and the parameter when there are such: a host's
        // conversion has neither, and `render` removes the placeholders it
        // has no argument for (JYRO-ADR-128).
        templates.[MessageCode.IntegerOutOfRange] <- "{0} is outside the range {1} to {2} {3} {4}"
        // {0} is the function, then one name and value pair or two: `Range`
        // gives `step` and its value alone, `Clamp` and `RandomInt` their two
        // bounds. The second pair, which can be absent, comes last and is
        // removed by `render` with the space before it (JYRO-ADR-128), so the
        // wording names no relation between the values: `Clamp` refuses
        // min > max, `RandomInt`'s half-open range min >= max too
        // (spec 93.15.6), and `Range` a zero step (92.13.10).
        templates.[MessageCode.InvalidRange] <- "{0} cannot make a range from {1} {2} {3} {4}"
        templates.[MessageCode.InvalidJson] <- "Input is not valid JSON"
        // {0} is the index, {1} the function whose call raised it, which a
        // host member and a statement do not have (JYRO-ADR-128).
        templates.[MessageCode.UnpairedSurrogate] <- "An unpaired surrogate at index {0} has no UTF-8 form {1}"
        templates.[MessageCode.ValueSizeLimitExceeded] <- "A value would exceed the maximum value size of {0}"
        templates.[MessageCode.InvalidSortDirection] <- "Invalid sort direction '{0}'. Valid directions: asc, desc"
        templates.[MessageCode.HostNumberNaN] <- "A host value holds a number that is NaN, which is not a Jyro number"
        templates.[MessageCode.HostNumberInfinite] <- "A host value holds a number that is an infinity, which is not a Jyro number"
        templates.[MessageCode.HostValueNotConvertible] <-
            "A host value is or holds a delegate, reflection metadata or a task, which has no Jyro value"
        templates.[MessageCode.NumericElementRequired] <- "{0} requires numbers or null; element {1} is neither"
        // {0} is the operation; then, for a function, each parameter's name and
        // value, one or two pairs, or, for an operator, each operand's value
        // (JYRO-ADR-123). The placeholders a diagnostic has no argument for are
        // removed by `render`, so one template reads for every shape.
        templates.[MessageCode.NumberDomainError] <- "{0} is not defined for {1} {2} {3} {4}"
        templates.[MessageCode.NumberOverflow] <- "{0} produced a value too large to represent as a number for {1} {2} {3} {4}"

        // Compiler
        templates.[MessageCode.NoSourceProvided] <- "No source code provided"
        templates.[MessageCode.InternalStageFailure] <- "A stage of the pipeline produced no result"
        templates.[MessageCode.ArtifactTooLarge] <- "Artifact size {0} exceeds the maximum of {1} bytes"
        templates.[MessageCode.ArtifactInvalidMagic] <- "Not a .jyrx artifact: expected magic bytes 'JYRX', found '{0}'"
        templates.[MessageCode.ArtifactVersionUnsupported] <- "Unsupported .jyrx format version {0}, expected {1}"
        // ArtifactUnreadable, ArtifactUnwritable, UnknownCompilerError and
        // InternalStageFailure have no argument: the exception that stopped
        // the work is the host platform's, and neither its type nor its
        // message is a name the specification gives (spec 130.6.2).
        templates.[MessageCode.ArtifactUnreadable] <- "Failed to read .jyrx artifact"
        templates.[MessageCode.ArtifactEffectExceedsMax] <- "Artifact requires '{0}' with effect {1}, which exceeds the maximum effect {2}"
        templates.[MessageCode.ArtifactCannotEncodeLibraries] <- "The .jyrx format cannot encode library scripts"
        templates.[MessageCode.ArtifactCannotUseLibraries] <- "A .jyrx artifact cannot reference library functions, so a configured library cannot apply to it. Remove the library, or compile from source."
        // {0} names the part of the program that the format cannot hold, as
        // spec 150.11 names it (String, List, FunctionTable, ObjectLiteral,
        // If, FuncDef, Call, UnionVariant, Literal); {1} is its count, or for
        // a Literal its type as TypeOf gives it.
        templates.[MessageCode.ArtifactCannotEncode] <- "The .jyrx format cannot encode this program: {0} {1} is beyond what the format can hold"
        templates.[MessageCode.ArtifactUnwritable] <- "Failed to write .jyrx artifact"
        templates.[MessageCode.UnknownCompilerError] <- "Internal compiler error. Please report this at https://github.com/RealTree-Systems/jyro-lang/issues"

    // Gets the default English template for a message code, or "{0}" if not found.
    let get (code: MessageCode) : string =
        match templates.TryGetValue(code) with
        | true, t -> t
        | _ -> "{0}"

    // Whether a code has a default English template, and what it is.
    //
    // The `Try*`/`out` shape rather than `string option`, matching
    // `IMessageTemplateProvider` and `IDictionary.TryGetValue`: this is the
    // same question in the idiom both languages already use for it.
    let tryGet (code: MessageCode, [<Out>] template: byref<string>) : bool =
        templates.TryGetValue(code, &template)

    // `defaults`, a third lookup door returning the whole table as an
    // IReadOnlyDictionary, was deleted. It had no caller anywhere, and it was
    // not read-only in any useful sense - the value was the mutable dictionary
    // itself under an interface, so a host holding it observed the module's
    // internal state rather than a snapshot. `get` (total, with a sentinel for
    // a code that has none) and `tryGet` (partial, for asking whether one
    // exists) are two doors with a stated difference; a third that hands out
    // the table was one too many.

    let inline private isAsciiDigit (c: char) = c >= '0' && c <= '9'

    let inline private isWhiteSpace (c: char) = Unicode15.isWhiteSpace (int c)

    // The placeholder of spec 130.6.3 that starts at `start` of `template`:
    // White_Space code points, `{`, an index of ASCII digits, an optional
    // alignment (`,`, White_Space, an optional `-`, ASCII digits), an
    // optional format (`:` and code units other than `{` and `}`), `}`.
    // Gives the end (one past the `}`), the end of the white space, the
    // index text, the alignment and the format, or `ValueNone` when no
    // placeholder starts there. A regular expression was used before, but
    // .NET's `\d` matches every decimal digit, U+0663 included, where the
    // clause allows only `0` to `9`.
    let private tryPlaceholder (template: string) (start: int)
        : struct (int * int * string * string * string) voption =
        let n = template.Length
        let mutable p = start
        while p < n && isWhiteSpace template.[p] do p <- p + 1
        let wsEnd = p
        if p >= n || template.[p] <> '{' then ValueNone
        else
            p <- p + 1
            let indexStart = p
            while p < n && isAsciiDigit template.[p] do p <- p + 1
            if p = indexStart then ValueNone
            else
                let index = template.Substring(indexStart, p - indexStart)
                let mutable ok = true
                let mutable alignment = ""
                if p < n && template.[p] = ',' then
                    let alignStart = p
                    p <- p + 1
                    while p < n && isWhiteSpace template.[p] do p <- p + 1
                    if p < n && template.[p] = '-' then p <- p + 1
                    let digitsStart = p
                    while p < n && isAsciiDigit template.[p] do p <- p + 1
                    if p = digitsStart then ok <- false
                    else alignment <- template.Substring(alignStart, p - alignStart)
                let mutable format = ""
                if ok && p < n && template.[p] = ':' then
                    let formatStart = p
                    p <- p + 1
                    while p < n && template.[p] <> '{' && template.[p] <> '}' do p <- p + 1
                    format <- template.Substring(formatStart, p - formatStart)
                if ok && p < n && template.[p] = '}' then
                    ValueSome (struct (p + 1, wsEnd, index, alignment, format))
                else ValueNone

    // Whether the index, ASCII digits read in decimal with leading zeros
    // permitted, is less than `count`. An index too long for an `int` is
    // not.
    let private indexBelow (index: string) (count: int) : int voption =
        let mutable value = 0L
        let mutable i = 0
        while i < index.Length && value < int64 count do
            value <- value * 10L + int64 (int index.[i] - int '0')
            i <- i + 1
        if value < int64 count then ValueSome (int value) else ValueNone

    // The string of an argument for a placeholder with `alignment` and
    // `format` (spec 130.6.3): `String.Format` under the invariant culture,
    // or, when that throws `FormatException`, the string of the argument
    // that spec 160.7.16 gives, `Convert.ToString` under the invariant
    // culture.
    let private argumentString (arg: obj | null) (alignment: string) (format: string) : string =
        let inv = System.Globalization.CultureInfo.InvariantCulture
        try String.Format(inv, "{0" + alignment + format + "}", arg)
        with :? FormatException ->
            match Convert.ToString(arg, inv) with
            | null -> ""
            | text -> text

    // The message without the White_Space code points at its start and end.
    let private trimWhiteSpace (text: string) : string =
        let mutable first = 0
        let mutable last = text.Length - 1
        while first <= last && isWhiteSpace text.[first] do first <- first + 1
        while last >= first && isWhiteSpace text.[last] do last <- last - 1
        text.Substring(first, last - first + 1)

    // Renders `template` with `args`, or returns it unchanged when there is
    // nothing to substitute.
    //
    // A placeholder with no argument to fill it is removed, with the white
    // space before it. That is a supported shape, not a fault: where what an
    // argument would name does not exist, the diagnostic has no argument for
    // it (spec 130.6.2), and a code whose argument can be absent puts it last
    // in a template worded to read without it (JYRO-ADR-123, JYRO-ADR-128),
    // as `NumberDomainError`, `UnpairedSurrogate`, `RuntimeError` and a
    // lambda's `DuplicateParameterName` do. A template whose wording needs the
    // missing argument still reads wrong once it is removed, and nothing here
    // can tell; the template has to be worded for it.
    let render (template: string) (args: (obj | null)[] | null) : string =
        // A null array is the empty one. `DiagnosticMessage` is a record, so a
        // host can construct one directly with `Args = null`, and `Message`
        // renders through here - which made the *message* of a diagnostic throw
        // rather than read.
        let args : (obj | null)[] = match args with | null -> Array.empty | args -> args
        if isNull template || not (template.Contains("{") || template.Contains("}")) then
            template
        else
            // One substitution for every template, the default ones and a
            // host's alike (spec 130.6.3): each placeholder with an argument
            // is replaced by it, with its alignment and format; each with
            // none is removed with the white space before it; `{{` and `}}`
            // are the braces they escape. `String.Format` refused a template
            // that names an argument the diagnostic does not have, and the
            // template came back whole, so a host's `{5}` reached the reader.
            //
            // The template is read from its first code unit to its last,
            // taking at each position the first row of the clause's table
            // whose text starts there: `{{`, `}}`, a placeholder with the
            // White_Space before it, then any other code unit as it is.
            let mutable removed = false
            let sb = System.Text.StringBuilder(template.Length)
            let n = template.Length
            let mutable pos = 0
            while pos < n do
                let c = template.[pos]
                if c = '{' && pos + 1 < n && template.[pos + 1] = '{' then
                    sb.Append('{') |> ignore
                    pos <- pos + 2
                elif c = '}' && pos + 1 < n && template.[pos + 1] = '}' then
                    sb.Append('}') |> ignore
                    pos <- pos + 2
                elif c = '{' || isWhiteSpace c then
                    match tryPlaceholder template pos with
                    | ValueSome (struct (stop, wsEnd, index, alignment, format)) ->
                        match indexBelow index args.Length with
                        | ValueSome i ->
                            sb.Append(template, pos, wsEnd - pos)
                              .Append(argumentString args.[i] alignment format) |> ignore
                        | ValueNone -> removed <- true
                        pos <- stop
                    | ValueNone ->
                        sb.Append(c) |> ignore
                        pos <- pos + 1
                else
                    sb.Append(c) |> ignore
                    pos <- pos + 1
            let rendered = sb.ToString()
            if removed then trimWhiteSpace rendered else rendered

    // The default English rendering of a code and its arguments.
    let format (code: MessageCode) (args: (obj | null)[] | null) : string =
        render (get code) args

    // The message of a `ScriptFailure` of a `fail` with no value (spec
    // 50.11.4). That diagnostic has no argument, and the template `{0}`
    // renders empty without one, so the text is held here and
    // `DiagnosticMessage.Message` gives it for a `ScriptFailure` whose
    // arguments are empty.
    [<Literal>]
    let ScriptFailureWithoutValue = "Script execution failed"

/// <summary>
/// Default English message templates keyed by MessageCode.
/// Each template uses {0}, {1}, etc. for positional arguments.
///
/// A static class rather than a module: F# cannot
/// put <c>ParamArray</c> on a module function, so <c>Render</c> and <c>Format</c> reached a C#
/// host with a required <c>object[]</c>. As members each has an overload without
/// values and one with a <c>params</c> array.
/// </summary>
[<AbstractClass; Sealed>]
type MessageTemplates =
    /// <summary>
    /// Gets the default English template for a message code, or "{0}" if not found.
    /// </summary>
    static member Get(code: MessageCode) : string = MessageTemplates.get code

    /// <summary>
    /// Whether a code has a default English template, and what it is.
    /// </summary>
    static member TryGet(code: MessageCode, [<Out; System.Diagnostics.CodeAnalysis.MaybeNullWhen(false)>] template: byref<string>) : bool =
        MessageTemplates.tryGet (code, &template)

    /// <summary>
    /// Renders <c>template</c> with no values: any placeholders are stripped, so the
    /// reader gets a sentence rather than a format string.
    /// </summary>
    static member Render(template: string) : string =
        MessageTemplates.render template Array.empty

    /// <summary>
    /// Renders <c>template</c> with <c>args</c>, or returns it unchanged when there is
    /// nothing to substitute. A null array is the empty one.
    /// </summary>
    static member Render(template: string, [<ParamArray>] args: (obj | null)[] | null) : string =
        MessageTemplates.render template args

    /// <summary>
    /// The default English rendering of a code whose template takes no values.
    /// </summary>
    static member Format(code: MessageCode) : string =
        MessageTemplates.format code Array.empty

    /// <summary>
    /// The default English rendering of a code and its arguments. A null
    /// array is the empty one.
    /// </summary>
    static member Format(code: MessageCode, [<ParamArray>] args: (obj | null)[] | null) : string =
        MessageTemplates.format code args

/// <summary>
/// A diagnostic message from compilation or execution.
///
/// A sealed class with no public constructor: a host
/// makes one through the factories <c>Error</c>, <c>Warning</c> and <c>Info</c>, each of
/// which takes the stage. The diagnostic holds its stage itself, so every
/// reference to it, and every diagnostic <c>Equals</c> to it, has the same stage.
/// </summary>
[<Sealed>]
type DiagnosticMessage internal (code: MessageCode, severity: MessageSeverity, stage: MessageStage,
                                 args: (obj | null)[], location: Nullable<SourceLocation>) =

    /// <summary>
    /// The code identifying what this diagnostic reports.
    /// </summary>
    member _.Code : MessageCode = code

    /// <summary>
    /// The diagnostic's severity.
    /// </summary>
    member _.Severity : MessageSeverity = severity

    /// <summary>
    /// The stage that produced this diagnostic. A diagnostic the pipeline makes
    /// has the stage its code has wherever it is raised, unless
    /// the pipeline gave it another, where the stage depends on when the
    /// diagnostic was made rather than on its code - a lexical error raised
    /// while parsing, a refusal met while an artifact is read, a compilation
    /// time limit reached or an internal error raised in any stage, a runtime
    /// error with a code of an earlier stage. A diagnostic a
    /// host makes has the stage it was given.
    /// </summary>
    member _.Stage : MessageStage = stage

    /// <summary>
    /// Positional arguments substituted into the code's message template.
    /// </summary>
    member _.Args : (obj | null)[] = args

    /// <summary>
    /// The place in the source, or no value where the diagnostic has none.
    /// </summary>
    member _.Location : Nullable<SourceLocation> = location

    // The stage a diagnostic of `code` has when nothing about the moment it
    // was made says otherwise (spec 160.1.12), decided by the code's name,
    // never by its number (JYRO-ADR-112): two codes whose numbers are close
    // may have different stages, and a new code must be placed here by name
    // or it is a runtime error. The rows follow the table of 160.1.12:
    //   * the codes the Link row names - a call's name resolved, its
    //     arguments matched, its effect checked, a host function colliding
    //     with the standard library, a variant colliding with a function;
    //   * the rejections of a source text before it runs - the validator's
    //     codes, and `DuplicateFunction`, `FunctionOverride` and
    //     `FunctionShadowsLibrary`, decided before any call is resolved from
    //     the declarations of the script and its libraries together with the
    //     names registered for the compilation: `FunctionOverride` rejects a
    //     function named after a 90.2 inventory function and also one named
    //     after a host function or a further standard library function the
    //     host registered (80.3.2), so the Validate pass is given those names
    //     (JYRO-ADR-115, JYRO-ADR-116) - and the refusal `NoSourceProvided` -
    //     `Validate`;
    //   * a refusal to read an artifact `Deserialize`, a refusal to write one
    //     `Compile`;
    //   * every runtime error `Execute`.
    // `UnknownParserError`, `UnknownCompilerError`, `InternalStageFailure`
    // and `CompilationTimeLimitExceeded` have the stage that was running,
    // which the stage that makes them gives them (`WithStage`); the stage
    // here is only where each is made when nothing gives another.
    static member internal StageOf(code: MessageCode) : MessageStage =
        match code with
        // Parse: a parse error (130.3) and every parse-time rejection.
        | MessageCode.UnterminatedString
        | MessageCode.InvalidEscapeSequence
        | MessageCode.UnknownParserError
        | MessageCode.UnexpectedEndOfSource
        | MessageCode.SourceNestingLimitExceeded
        | MessageCode.CallTargetNotName
        | MessageCode.InvalidNumberFormat -> MessageStage.Parse
        // Validate: every other rejection of a source text before it runs.
        | MessageCode.UndeclaredVariable
        | MessageCode.VariableAlreadyDeclared
        | MessageCode.ReservedName
        | MessageCode.InvalidAssignmentTarget
        | MessageCode.InvalidDeleteTarget
        | MessageCode.TypeHintOnCompoundAssignment
        | MessageCode.DuplicateTypeHint
        | MessageCode.InvalidIncrementTarget
        | MessageCode.StringOperandInAddition
        | MessageCode.TypedVariableNeedsInitializer
        | MessageCode.LoopStatementOutsideOfLoop
        | MessageCode.NotIterableLiteral
        | MessageCode.FunctionNotAtTopLevel
        | MessageCode.NestedFunctionDeclaration
        | MessageCode.DuplicateParameterName
        | MessageCode.OutputAccessInFunction
        | MessageCode.InputIsReadOnly
        | MessageCode.ReturnOutsideFunction
        | MessageCode.RequiredParamAfterOptional
        | MessageCode.DefaultValueNotLiteral
        | MessageCode.LibraryStatementForbidden
        | MessageCode.UnionNotAtTopLevel
        | MessageCode.DuplicateVariantName
        | MessageCode.MatchNotExhaustive
        | MessageCode.MatchDuplicateCase
        | MessageCode.MatchBindingCountMismatch
        | MessageCode.UnionInsideFunction
        | MessageCode.MatchUnknownVariant
        | MessageCode.MatchWithDefault
        | MessageCode.DuplicateUnionName
        | MessageCode.ReservedFieldName
        | MessageCode.DuplicateFieldName
        | MessageCode.MatchDuplicateBinding
        | MessageCode.DuplicateFunction
        | MessageCode.FunctionOverride
        | MessageCode.FunctionShadowsLibrary
        | MessageCode.NoSourceProvided -> MessageStage.Validate
        // Link: the codes the Link row names.
        | MessageCode.UndefinedFunction
        | MessageCode.FunctionEffectExceedsMax
        | MessageCode.MaxEffectNotApplicable
        | MessageCode.HostFunctionOverridesStdlib
        | MessageCode.TooFewArguments
        | MessageCode.TooManyArguments
        | MessageCode.DuplicateNamedArgument
        | MessageCode.UnknownNamedArgument
        | MessageCode.MissingRequiredNamedArgument
        | MessageCode.VariantShadowsFunction
        | MessageCode.FunctionShadowsVariant
        // A refusal by a host function's call check (JYRO-ADR-130).
        | MessageCode.HostLinkError
        | MessageCode.ArtifactEffectExceedsMax -> MessageStage.Link
        // Deserialize: a refusal to decode an artifact.
        | MessageCode.ArtifactTooLarge
        | MessageCode.ArtifactInvalidMagic
        | MessageCode.ArtifactVersionUnsupported
        | MessageCode.ArtifactUnreadable
        | MessageCode.ArtifactCannotUseLibraries -> MessageStage.Deserialize
        // Compile: a refusal to write an artifact; and, where no running
        // stage was given, the internal errors of the compiler.
        | MessageCode.ArtifactCannotEncodeLibraries
        | MessageCode.ArtifactCannotEncode
        | MessageCode.ArtifactUnwritable
        | MessageCode.UnknownCompilerError
        | MessageCode.InternalStageFailure
        | MessageCode.CompilationTimeLimitExceeded -> MessageStage.Compile
        // Execute: a runtime error, and ScriptFailure.
        | MessageCode.RuntimeError
        | MessageCode.CanceledByHost
        | MessageCode.ScriptFailure
        | MessageCode.ValueIsReadOnly
        | MessageCode.HostFunctionError
        | MessageCode.InputValueIsReadOnly
        | MessageCode.InvalidType
        | MessageCode.InvalidCast
        | MessageCode.ArgumentNotProvided
        | MessageCode.ArgumentTypeMismatch
        | MessageCode.CallbackExpected
        | MessageCode.CallbackArityMismatch
        | MessageCode.DivisionByZero
        | MessageCode.ModuloByZero
        | MessageCode.UnsupportedBinaryOperation
        | MessageCode.UnsupportedUnaryOperation
        | MessageCode.ConcatWithInvalidType
        | MessageCode.StringOperandInAdditionRuntime
        | MessageCode.IndexOutOfRange
        | MessageCode.InvalidIndexType
        | MessageCode.SetPropertyOnNonObject
        | MessageCode.SetIndexOnNonContainer
        | MessageCode.DeleteOnNonObject
        | MessageCode.IntegerIndexRequired
        | MessageCode.NotIterable
        | MessageCode.MatchSubjectNotVariant
        | MessageCode.UndefinedFunctionRuntime
        | MessageCode.SchemaInvalid
        | MessageCode.RegexTimeout
        | MessageCode.RegexInvalidPattern
        | MessageCode.GeneratedLengthExceeded
        | MessageCode.EmptyCharacterSet
        | MessageCode.StringOrArrayRequired
        | MessageCode.InvalidPadCharacter
        | MessageCode.StringArrayOrObjectRequired
        | MessageCode.DateFormatStringInvalid
        | MessageCode.DateFormatInvalid
        | MessageCode.DateUnitInvalid
        | MessageCode.DatePartInvalid
        | MessageCode.DateOutOfRange
        | MessageCode.StatementLimitExceeded
        | MessageCode.LoopIterationLimitExceeded
        | MessageCode.CallDepthLimitExceeded
        | MessageCode.ExecutionTimeLimitExceeded
        | MessageCode.IntegerRequired
        | MessageCode.IncomparableTypes
        | MessageCode.UnsupportedComparisonOperator
        | MessageCode.NonNegativeIntegerRequired
        | MessageCode.InvalidRoundingMode
        | MessageCode.ValueNestingLimitExceeded
        | MessageCode.IntegerOutOfRange
        | MessageCode.NumericElementRequired
        | MessageCode.NumberDomainError
        | MessageCode.NumberOverflow
        | MessageCode.InvalidRange
        | MessageCode.InvalidJson
        | MessageCode.UnpairedSurrogate
        | MessageCode.ValueSizeLimitExceeded
        | MessageCode.InvalidSortDirection
        | MessageCode.HostNumberNaN
        | MessageCode.HostNumberInfinite
        | MessageCode.HostValueNotConvertible -> MessageStage.Execute
        // A number that names no member (a host's cast): no row names it,
        // and it cannot be a rejection of a stage before the run.
        | _ -> MessageStage.Execute

    // This diagnostic with `stage` in place of its own: a new diagnostic,
    // equal to this one in every field, whose stage is `stage`.
    member internal this.WithStage(newStage: MessageStage) : DiagnosticMessage =
        if stage = newStage then this
        else DiagnosticMessage(code, severity, newStage, args, location)

    /// <summary>
    /// The default English rendering, derived from the code's template rather
    /// than stored.
    ///
    /// A diagnostic travels as a code plus its arguments and becomes a string
    /// only here, so there is no second copy of the English for a call site to
    /// write differently. A host supplying
    /// <c>IMessageTemplateProvider</c> substitutes the same <c>Args</c> into its own
    /// template, so a translation is a translation of this sentence.
    ///
    /// One code is not rendered: a <c>ScriptFailure</c> with no argument is a <c>fail</c>
    /// with no value, whose message is <c>Script execution failed</c>
    /// while its arguments are empty.
    /// </summary>
    member _.Message : string =
        if code = MessageCode.ScriptFailure && args.Length = 0 then
            MessageTemplates.ScriptFailureWithoutValue
        else
            MessageTemplates.format code args

    // `[JMxxxx] Ln L, Col C: message` when the diagnostic has a location
    // whose line is above 0, and `[JMxxxx]: message` otherwise: what
    // `DiagnosticFormatter.FormatMessage` gives (JYRO-ADR-198 item 1).
    static member internal Format(codeText: string, location: Nullable<SourceLocation>, message: string) : string =
        let inv = Globalization.CultureInfo.InvariantCulture
        if location.HasValue && location.Value.Line > 0 then
            String.Format(inv, "[{0}] Ln {1}, Col {2}: {3}", codeText, location.Value.Line, location.Value.Column, message)
        else
            String.Format(inv, "[{0}]: {1}", codeText, message)

    // The code as `JMxxxx`, its number to four digits.
    static member internal CodeText(code: MessageCode) : string =
        "JM" + (int code).ToString("D4", Globalization.CultureInfo.InvariantCulture)

    // The invariant string of an argument, which equality compares
    // (JYRO-ADR-97).
    static member private ArgText(arg: obj | null) : string =
        match Convert.ToString(arg, Globalization.CultureInfo.InvariantCulture) with
        | null -> ""
        | text -> text

    /// <summary>
    /// Whether <c>other</c> has the same code, severity, stage and location, and
    /// arguments of the same count whose invariant strings are the same
    /// position by position. A null <c>other</c> is not equal.
    /// </summary>
    member _.Equals(other: DiagnosticMessage | null) : bool =
        match other with
        | null -> false
        | other ->
            code = other.Code
            && severity = other.Severity
            && stage = other.Stage
            && location.Equals(other.Location)
            && args.Length = other.Args.Length
            && Array.forall2 (fun a b -> String.Equals(DiagnosticMessage.ArgText a, DiagnosticMessage.ArgText b, StringComparison.Ordinal))
                   args other.Args

    /// <summary>
    /// Whether <c>other</c> is a <c>DiagnosticMessage</c> equal to this one.
    /// </summary>
    override this.Equals(other: obj | null) : bool =
        match other with
        | :? DiagnosticMessage as message -> this.Equals(message)
        | _ -> false

    override _.GetHashCode() : int =
        let hash = HashCode()
        hash.Add(code)
        hash.Add(severity)
        hash.Add(stage)
        hash.Add(location)
        hash.Add(args.Length)
        for arg in args do hash.Add(DiagnosticMessage.ArgText arg, StringComparer.Ordinal)
        hash.ToHashCode()

    /// <summary>
    /// One line: the prefix <c>DiagnosticFormatter.FormatMessage</c> gives, then
    /// <c>Message</c> escaped: a backslash as two backslashes, and
    /// each control character (general category Cc) and U+2028 and U+2029 as
    /// <c>\u</c> and the 4 lowercase hexadecimal digits of its code point. A message
    /// can carry text from the script, and written as it is it could forge a
    /// second line of a log. With nothing to escape it is what <c>FormatMessage</c>
    /// gives.
    /// </summary>
    override this.ToString() : string =
        DiagnosticMessage.Format(DiagnosticMessage.CodeText code, location, OneLine.escape this.Message)

    interface IEquatable<DiagnosticMessage> with
        member this.Equals(other: DiagnosticMessage | null) = this.Equals(other)

    static member private Make(severity, code, stage, args: (obj | null)[] | null, location) =
        DiagnosticMessage(code, severity, stage,
                          (match args with | null -> Array.empty | args -> Array.copy args),
                          location)

    // A diagnostic the pipeline makes, of severity `Error` and with the stage
    // its code has (`StageOf`); a stage that makes one whose stage is not its
    // code's gives it its own with `WithStage`.
    static member internal Of(code: MessageCode, ?args: (obj | null)[] | null, ?location: Nullable<SourceLocation>) : DiagnosticMessage =
        DiagnosticMessage.Make(
            MessageSeverity.Error, code, DiagnosticMessage.StageOf code,
            (match args with | Some a -> a | None -> null),
            (match location with | Some l -> l | None -> Nullable()))

    /// <summary>
    /// Creates an Error-severity diagnostic of <c>code</c> and <c>stage</c>, with
    /// <c>args</c>, and no location.
    /// </summary>
    static member Error(code: MessageCode, stage: MessageStage, [<ParamArray>] args: (obj | null)[] | null) : DiagnosticMessage =
        DiagnosticMessage.Make(MessageSeverity.Error, code, stage, args, Nullable())

    /// <summary>
    /// Creates an Error-severity diagnostic of <c>code</c> and <c>stage</c>, at
    /// <c>location</c>, with <c>args</c>.
    /// </summary>
    static member Error(code: MessageCode, stage: MessageStage, location: Nullable<SourceLocation>, [<ParamArray>] args: (obj | null)[] | null) : DiagnosticMessage =
        DiagnosticMessage.Make(MessageSeverity.Error, code, stage, args, location)

    /// <summary>
    /// Creates a Warning-severity diagnostic of <c>code</c> and <c>stage</c>, with
    /// <c>args</c>, and no location.
    /// </summary>
    static member Warning(code: MessageCode, stage: MessageStage, [<ParamArray>] args: (obj | null)[] | null) : DiagnosticMessage =
        DiagnosticMessage.Make(MessageSeverity.Warning, code, stage, args, Nullable())

    /// <summary>
    /// Creates a Warning-severity diagnostic of <c>code</c> and <c>stage</c>, at
    /// <c>location</c>, with <c>args</c>.
    /// </summary>
    static member Warning(code: MessageCode, stage: MessageStage, location: Nullable<SourceLocation>, [<ParamArray>] args: (obj | null)[] | null) : DiagnosticMessage =
        DiagnosticMessage.Make(MessageSeverity.Warning, code, stage, args, location)

    /// <summary>
    /// Creates an Info-severity diagnostic of <c>code</c> and <c>stage</c>, with
    /// <c>args</c>, and no location.
    /// </summary>
    static member Info(code: MessageCode, stage: MessageStage, [<ParamArray>] args: (obj | null)[] | null) : DiagnosticMessage =
        DiagnosticMessage.Make(MessageSeverity.Info, code, stage, args, Nullable())

    /// <summary>
    /// Creates an Info-severity diagnostic of <c>code</c> and <c>stage</c>, at
    /// <c>location</c>, with <c>args</c>.
    /// </summary>
    static member Info(code: MessageCode, stage: MessageStage, location: Nullable<SourceLocation>, [<ParamArray>] args: (obj | null)[] | null) : DiagnosticMessage =
        DiagnosticMessage.Make(MessageSeverity.Info, code, stage, args, location)

/// <summary>
/// The exception that reports a failed pipeline: the one type
/// a host catches for every exception that carries a Jyro code.
///
/// <c>JyroCompiler.CompileOrThrow()</c>, <c>JyroRunner.ExecuteOrThrow()</c> and
/// <c>JyroResult&lt;T&gt;.GetValueOrThrow()</c> throw it with the diagnostics of the
/// failed result, and <c>JyroRuntimeException</c> derives from it with the one
/// diagnostic of its code. A host does not construct it.
/// </summary>
type JyroException internal (message: string, messages: IReadOnlyList<DiagnosticMessage>) =
    inherit Exception(message)

    // The first diagnostic of severity Error: there is always one.
    let firstError =
        messages |> Seq.tryFind (fun m -> m.Severity = MessageSeverity.Error)

    // An exception holding `messages`, at least one of severity `Error`,
    // whose message joins theirs.
    internal new(messages: seq<DiagnosticMessage>) =
        let held = Seq.toArray messages
        if not (held |> Array.exists (fun m -> m.Severity = MessageSeverity.Error)) then
            invalidArg (nameof messages) "A JyroException holds at least one diagnostic of severity Error."
        JyroException(held |> Array.map (fun m -> m.Message) |> String.concat "; ",
                      Array.AsReadOnly(held) :> IReadOnlyList<DiagnosticMessage>)

    /// <summary>
    /// The diagnostics of the failure, at least one of severity <c>Error</c>.
    /// </summary>
    member _.Messages : IReadOnlyList<DiagnosticMessage> = messages

    /// <summary>
    /// The stage of the first diagnostic of severity <c>Error</c> in <c>Messages</c>.
    /// </summary>
    member _.Stage : MessageStage =
        match firstError with
        | Some m -> m.Stage
        | None -> MessageStage.Execute

    /// <summary>
    /// The place of the first diagnostic of severity <c>Error</c> in <c>Messages</c>,
    /// or no value when it has none.
    /// </summary>
    member _.Location : Nullable<SourceLocation> =
        match firstError with
        | Some m -> m.Location
        | None -> Nullable()

/// <summary>
/// Result of a compilation or execution phase.
///
/// A host reads a result through <c>IsSuccess</c>, <c>HasValue</c>, <c>Messages</c> and
/// <c>TryGetValue</c>, and builds one through the <c>Success</c> and <c>Failure</c>
/// factories; it has no public or protected constructor.
///
/// Not sealed: <c>JyroExecutionResult</c>, the result of a run, derives from
/// <c>JyroResult&lt;JyroValue&gt;</c> and adds the exit message, which no other result
/// has.
/// </summary>
type JyroResult<'T when 'T : not null> internal (value: 'T, hasValue: bool, messages: IReadOnlyList<DiagnosticMessage>, isSuccess: bool) =

    /// <summary>
    /// True if and only if <c>Messages</c> carries no message of <c>Error</c> severity.
    /// </summary>
    member _.IsSuccess = isSuccess

    /// <summary>
    /// The value when <c>IsSuccess</c> is true; otherwise throws a
    /// <c>JyroException</c> whose <c>Messages</c> are this result's <c>Messages</c>.
    /// </summary>
    member _.GetValueOrThrow() : 'T =
        if isSuccess then value
        else raise (JyroException(messages))

    // The value as held, present or not, for a type that derives from this
    // one.
    member internal _.HeldValue : 'T = value

    /// <summary>
    /// Whether a value is present. Distinct from <c>IsSuccess</c>: a value is always
    /// present on success, and may also be present on failure - see the
    /// <c>Failure(value, messages)</c> overload below, which exists because a script
    /// that populates <c>Output</c> and then breaches a limit leaves partial results
    /// worth reading.
    /// </summary>
    member _.HasValue = hasValue

    /// <summary>
    /// The diagnostics of the result: on a failure at least one of severity
    /// <c>Error</c>, and on a success none of that severity.
    /// </summary>
    member _.Messages = messages

    /// <summary>
    /// The value, when there is one. The door both languages use: <c>HasValue</c>
    /// answers the same question without an out-parameter, and there is
    /// deliberately no bare <c>Value</c> property - one that returned <c>default</c> when
    /// absent would be read by accident instead of asked about.
    /// </summary>
    member _.TryGetValue([<Out; System.Diagnostics.CodeAnalysis.MaybeNullWhen(false)>] result: byref<'T>) : bool =
        if hasValue then
            result <- value
            true
        else
            result <- Unchecked.defaultof<'T>
            false

    // The option view, for F# call sites inside this repository.
    //
    // `internal` on purpose. It is the ergonomic form for F# - `match
    // r.TryValue with Some v` - but an `FSharpOption` on the *public* surface
    // is the single thing this type is shaped to keep away from a C# host, and
    // a public member cannot be one language's convenience only. External F#
    // hosts use `TryGetValue`, which F# supports directly.
    member internal _.TryValue : 'T option = if hasValue then Some value else None

    static member private ContainsError(messages: IReadOnlyList<DiagnosticMessage>) =
        messages |> Seq.exists (fun m -> m.Severity = MessageSeverity.Error)

    /// <summary>
    /// Creates a successful result carrying <c>value</c> and no message. The
    /// success indicator of every result is derived from its messages: it is
    /// true if and only if the list carries no message of <c>Error</c> severity.
    /// </summary>
    static member Success(value: 'T) =
        JyroResult<'T>(value, true, Array.empty, true)

    /// <summary>
    /// Creates a result carrying <c>value</c> and accompanying <c>messages</c>, such
    /// as warnings. A success holds no error: a message of severity <c>Error</c>
    /// is refused with an <c>ArgumentException</c> naming <c>messages</c>, as <c>Failure</c>
    /// refuses a list with none.
    /// </summary>
    static member Success(value: 'T, messages: seq<DiagnosticMessage>) =
        if obj.ReferenceEquals(messages, null) then
            raise (ArgumentNullException(nameof messages))
        let msgs = Seq.toArray messages :> IReadOnlyList<DiagnosticMessage>
        if JyroResult<'T>.ContainsError msgs then
            invalidArg (nameof messages)
                "A success holds no message of Error severity; make a result with an error through Failure."
        JyroResult<'T>(value, true, msgs, true)

    // Both no-value overloads carry an explicit `JyroResult<'T>` return
    // annotation. Nothing in their signatures otherwise mentions `'T`, so F#
    // generalizes the inferred return to a *fresh* type variable and emits
    // each as a generic method `Failure<a>` - at which point
    // `JyroResult<JyroValue>.Failure(msg)` fails from C# with CS0411,
    // because there is nothing to infer `a` from.
    // The `Failure` factories used to hard-code `isSuccess = false` while
    // `Success` genuinely derived it, so the comment above - "derived from the
    // messages rather than taken on trust" - was true of one half of the type.
    // `Failure(emptySequence)` was therefore a publicly constructible result
    // with `IsSuccess = false` and no Error message to justify it, breaking
    // the "if and only if" in exactly the direction the derivation was added
    // to close.
    //
    // Requiring the error rather than deriving `false` from its absence: a
    // failure with nothing to say is not a reportable state, and returning
    // `IsSuccess = true` from a method named `Failure` would be a worse answer
    // than refusing. This is a mistake in the caller's own code, so it is
    // refused at the call - the same rule `JyroString(null)` follows.
    static member private RequireErrorMessage(messages: IReadOnlyList<DiagnosticMessage>) =
        if obj.ReferenceEquals(messages, null) then
            raise (ArgumentNullException("messages"))
        if not (JyroResult<'T>.ContainsError messages) then
            invalidArg "messages"
                "A failure carries at least one message of Error severity; \
                 IsSuccess is derived from the messages, so a failure with none \
                 could not report itself as one."

    /// <summary>
    /// Creates a failed result with no value, from <c>messages</c>; at least one
    /// must be Error severity, or the call throws. <c>Failure(message)</c> is the
    /// single-message form.
    ///
    /// A bare <c>null</c> argument is ambiguous in C# (CS0121) between the sequence
    /// and the single-message overloads, and both refuse null anyway. Pass a
    /// message or a sequence; to probe the refusal, pass a typed null such as
    /// <c>(DiagnosticMessage[])null</c>.
    /// </summary>
    static member Failure(messages: seq<DiagnosticMessage>) : JyroResult<'T> =
        if obj.ReferenceEquals(messages, null) then
            raise (ArgumentNullException(nameof messages))
        let msgs = Seq.toArray messages :> IReadOnlyList<DiagnosticMessage>
        JyroResult<'T>.RequireErrorMessage(msgs)
        JyroResult<'T>(Unchecked.defaultof<'T>, false, msgs, not (JyroResult<'T>.ContainsError msgs))

    static member Failure(message: DiagnosticMessage) : JyroResult<'T> =
        JyroResult<'T>.Failure([| message |])

    /// <summary>
    /// A failure that still carries a value: execution began, produced
    /// something, and then failed. A script that populates <c>Output</c>
    /// incrementally and then breaches a limit leaves partial results and
    /// audit trail in it, and those matter most when the run did not succeed.
    ///
    /// A bare <c>null</c> as the second argument is ambiguous in C# (CS0121)
    /// between the sequence and the single-message overloads, and both refuse
    /// null anyway. Pass a message or a sequence; to probe the refusal, pass a
    /// typed null such as <c>(DiagnosticMessage[])null</c>.
    /// </summary>
    static member Failure(value: 'T, messages: seq<DiagnosticMessage>) =
        if obj.ReferenceEquals(messages, null) then
            raise (ArgumentNullException(nameof messages))
        let msgs = Seq.toArray messages :> IReadOnlyList<DiagnosticMessage>
        JyroResult<'T>.RequireErrorMessage(msgs)
        JyroResult<'T>(value, true, msgs, not (JyroResult<'T>.ContainsError msgs))

    static member Failure(value: 'T, message: DiagnosticMessage) =
        JyroResult<'T>.Failure(value, [| message |])

// In-repo destructuring for `JyroResult`, standing in for the record patterns
// the type allowed before it became a class. `Failed` is tested first, so a
// three-branch match reads in the order the pipeline cares about: a failure,
// a value, or the internal-invariant violation of neither.
//
// Internal, for the same reason `TryValue` is: it rests on an option.
[<AutoOpen>]
module internal JyroResultPattern =

    let (|Failed|Produced|Empty|) (result: JyroResult<'T>) =
        if not result.IsSuccess then Failed result.Messages
        else
            match result.TryValue with
            | Some value -> Produced value
            | None -> Empty


/// <summary>
/// A diagnostic in the shape a host writes out: for a JSON API, a UI, a log.
///
/// A sealed class with no public constructor and read-only, typed properties; a host gets one from <c>DiagnosticFormatter.ToStructured</c>.
/// </summary>
[<Sealed>]
type StructuredDiagnostic internal (code: string, numericCode: int, severity: MessageSeverity, stage: MessageStage,
                                    subsystem: string, message: string, args: (obj | null)[],
                                    location: Nullable<SourceLocation>) =
    /// <summary>
    /// The code as text, <c>JM</c> and its number to four digits, e.g. <c>JM5200</c>.
    /// </summary>
    member _.Code : string = code
    /// <summary>
    /// The numeric value of the message code, e.g. 5200.
    /// </summary>
    member _.NumericCode : int = numericCode
    /// <summary>
    /// The severity of the diagnostic.
    /// </summary>
    member _.Severity : MessageSeverity = severity
    /// <summary>
    /// The stage of the pipeline that produced the diagnostic.
    /// </summary>
    member _.Stage : MessageStage = stage
    /// <summary>
    /// The name of the band of the code: <c>lexer</c>, <c>parser</c>, <c>validator</c>,
    /// <c>linker</c>, <c>runtime</c>, <c>compiler</c> or <c>artifact</c>.
    /// </summary>
    member _.Subsystem : string = subsystem
    /// <summary>
    /// The rendered message text.
    /// </summary>
    member _.Message : string = message
    /// <summary>
    /// Positional arguments substituted into the message template.
    /// </summary>
    member _.Args : (obj | null)[] = args
    /// <summary>
    /// The place in the source, or no value where the diagnostic has none.
    /// </summary>
    member _.Location : Nullable<SourceLocation> = location

    /// <summary>
    /// One line: the code, the severity, the stage, the subsystem, the place
    /// when there is one, and the message with each control character and
    /// U+2028 and U+2029 written as <c>\u</c> and four lowercase hexadecimal
    /// digits, a backslash as two.
    /// </summary>
    override _.ToString() : string =
        let place = if location.HasValue then " " + location.Value.ToString() else ""
        String.Format(Globalization.CultureInfo.InvariantCulture, "{0} {1} {2} {3}{4}: {5}",
                      code, severity, stage, subsystem, place, OneLine.escape message)

/// <summary>
/// Formatting utilities for diagnostic messages.
/// </summary>
module DiagnosticFormatter =
    // The name of the band of a code (JYRO-ADR-198 item 4). Every band the
    // enum defines appears here, so no member of `MessageCode` falls through
    // to `unknown`, which only a number outside every band gives.
    let private codeToSubsystem (code: int) : string =
        match code with
        | c when c >= 1000 && c < 2000 -> "lexer"
        | c when c >= 2000 && c < 3000 -> "parser"
        | c when c >= 3000 && c < 4000 -> "validator"
        | c when c >= 4000 && c < 5000 -> "linker"
        | c when c >= 5000 && c < 6000 -> "runtime"
        | c when c >= 6000 && c < 7000 -> "compiler"
        | c when c >= 7000 && c < 8000 -> "artifact"
        | _ -> "unknown"

    /// <summary>
    /// Format a MessageCode as <c>JM</c> and its number to four digits, e.g. <c>JM5200</c>.
    /// </summary>
    [<CompiledName("FormatCode")>]
    let formatCode (code: MessageCode) : string =
        DiagnosticMessage.CodeText code

    /// <summary>
    /// Format a DiagnosticMessage as its code in brackets, then <c>Ln</c> and the
    /// line, <c>Col</c> and the column, a colon and the message, e.g.
    /// <c>[JM5200] Ln 3, Col 9: message</c>; with no location with a line above
    /// 0, the code in brackets, a colon and the message.
    /// </summary>
    [<CompiledName("FormatMessage")>]
    let formatMessage (msg: DiagnosticMessage) : string =
        if obj.ReferenceEquals(msg, null) then raise (ArgumentNullException(nameof msg))
        DiagnosticMessage.Format(formatCode msg.Code, msg.Location, msg.Message)

    /// <summary>
    /// Format a DiagnosticMessage using a host-provided template provider for localization.
    /// Falls back to the pre-formatted English message if no template is found.
    /// A <c>ScriptFailure</c> always gives its own message, whatever the provider holds.
    /// </summary>
    [<CompiledName("FormatLocalized")>]
    let formatLocalized (provider: IMessageTemplateProvider) (msg: DiagnosticMessage) : string =
        if obj.ReferenceEquals(provider, null) then raise (ArgumentNullException(nameof provider))
        if obj.ReferenceEquals(msg, null) then raise (ArgumentNullException(nameof msg))
        let message =
            // A host's template never makes the message of a `ScriptFailure`
            // (spec 130.6.3, 160.7.17): the text of a `fail` value is the
            // author's, copied unchanged and not read as a template, and a
            // `fail` with no value keeps `Script execution failed` (spec
            // 50.11.4). So the provider is not asked for that code.
            //
            // `render` rather than `String.Format` directly: a host template
            // whose placeholders have no arguments to fill used to be returned
            // verbatim, so a literal "{0}" reached the reader. It also survives
            // a host template with more placeholders than the diagnostic
            // carries, which no amount of care on our side can prevent.
            let mutable template = Unchecked.defaultof<string>
            if msg.Code = MessageCode.ScriptFailure then
                msg.Message
            elif provider.TryGetTemplate(msg.Code, &template) && not (isNull template) then
                MessageTemplates.render template msg.Args
            else
                msg.Message
        DiagnosticMessage.Format(formatCode msg.Code, msg.Location, message)

    /// <summary>
    /// Convert a DiagnosticMessage to a structured diagnostic for JSON serialization.
    /// </summary>
    [<CompiledName("ToStructured")>]
    let toStructured (msg: DiagnosticMessage) : StructuredDiagnostic =
        if obj.ReferenceEquals(msg, null) then raise (ArgumentNullException(nameof msg))
        let numCode = int msg.Code
        StructuredDiagnostic(formatCode msg.Code, numCode, msg.Severity, msg.Stage, codeToSubsystem numCode,
                             msg.Message, msg.Args, msg.Location)


// The stage a compilation has entered, shared between the thread that runs
// it and the thread that waits for it (JYRO-ADR-100).
//
// A compilation time limit can end any stage before Execute, and the
// diagnostic it gives names the stage that was running (spec 100.9.3,
// 160.1.12). The pipeline runs on a worker thread that the waiting thread
// abandons when the limit is reached, so the waiting thread cannot ask the
// worker where it is: each stage publishes itself here before it starts, and
// the waiting thread reads the last one published.
//
// It also carries the signal by which the waiting thread tells an abandoned
// worker to stop. A stage checks it as it enters and in its long loops, so a
// compilation nobody is waiting for any more stops at its next check instead
// of running on.
[<Sealed>]
type internal CompilationStage(first: MessageStage, token: System.Threading.CancellationToken) =
    // The first stage of the path from the start, not from when the worker
    // enters it: a limit can be reached before the worker thread has run at
    // all, and the stage that was running then is the one it was about to
    // start - Deserialize for a load, Parse otherwise.
    [<VolatileField>]
    let mutable current = first

    // A cell no one cancels, for a compilation run without a time limit.
    new() = CompilationStage(MessageStage.Parse, System.Threading.CancellationToken.None)

    // The stage last entered.
    member _.Current = current

    // Records that `stage` is starting, after stopping the compilation if
    // it has been abandoned.
    member _.Enter(stage: MessageStage) =
        token.ThrowIfCancellationRequested()
        current <- stage

    // Stops the compilation if it has been abandoned.
    member _.Check() = token.ThrowIfCancellationRequested()

    // The cell of the compilation running on this thread, if it runs under
    // a time limit: the worker thread of a timed compilation installs it
    // (`RunAmbient`), so the long loops of every stage - the parser's
    // statements, the validator's, the linker's calls, the reader's and the
    // writer's statements - check it (`CheckAmbient`) without each stage
    // being handed it (JYRO-ADR-100).
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private ambient : CompilationStage

    // Runs `work` with `stage` as the cell of this thread's compilation, and
    // puts back the one it replaced when `work` ends.
    static member RunAmbient(stage: CompilationStage, work: unit -> 'T) : 'T =
        let previous = CompilationStage.ambient
        CompilationStage.ambient <- stage
        try work ()
        finally CompilationStage.ambient <- previous

    // Stops the compilation running on this thread if it has been abandoned;
    // nothing when no timed compilation runs on it.
    static member CheckAmbient() =
        let stage = CompilationStage.ambient
        if not (obj.ReferenceEquals(stage, null)) then stage.Check()

    // `msgs`, each given the stage now running. For the diagnostics whose
    // stage is the moment they were made, not their code: a refusal met
    // while an artifact is read. (The parser gives its own diagnostics the
    // stage Parse, JYRO-ADR-111.)
    member _.Stamp(msgs: IReadOnlyList<DiagnosticMessage>) : DiagnosticMessage[] =
        let stage = current
        msgs |> Seq.map (fun m -> m.WithStage stage) |> Seq.toArray
