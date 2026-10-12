/// The table of the numbers of MessageCode of 160.7.12, copied from the book,
/// shared by the tests of 160.7.12 (S160_07_ArtifactInfoAndCodes.fs) and 130.1.1
/// (S130_Diagnostics.fs). Written from the book alone (FLOORS 4.1.1).
module RealTree.Jyro.Spec.Tests.MessageCodeTable

/// The table of 160.7.12 as JYRO-REQ-2138 gives it (which replaces JYRO-REQ-1716 and adds
/// 4106 MaxEffectNotApplicable, the code of a compilation that cannot apply the effect maximum,
/// 120.5, JYRO-REQ-2136; 1716 added 2002 SourceNestingLimitExceeded, 100.10.4; 1678 added
/// 4001 HostLinkError, JYRO-REQ-2119), copied from the book: number,
/// member. Never read from the implementation or from PublicApi.approved.txt (10.1.1, d-553).
let messageCodeTable : (int * string) list =
    [ 1200, "UnterminatedString"; 1201, "InvalidEscapeSequence"; 2000, "UnknownParserError"; 2001, "UnexpectedEndOfSource"; 2002, "SourceNestingLimitExceeded"
      2200, "InvalidNumberFormat"
      3100, "UndeclaredVariable"; 3101, "VariableAlreadyDeclared"; 3103, "ReservedName"
      3200, "InvalidAssignmentTarget"; 3201, "InvalidDeleteTarget"; 3202, "TypeHintOnCompoundAssignment"
      3203, "DuplicateTypeHint"; 3204, "InvalidIncrementTarget"; 3301, "StringOperandInAddition"
      3302, "TypedVariableNeedsInitializer"; 3400, "LoopStatementOutsideOfLoop"; 3402, "NotIterableLiteral"
      3500, "FunctionNotAtTopLevel"; 3501, "NestedFunctionDeclaration"; 3502, "DuplicateParameterName"
      3504, "OutputAccessInFunction"; 3505, "ReturnOutsideFunction"; 3506, "RequiredParamAfterOptional"
      3507, "DefaultValueNotLiteral"; 3508, "InputIsReadOnly"; 3509, "LibraryStatementForbidden"
      3600, "UnionNotAtTopLevel"; 3601, "DuplicateVariantName"; 3602, "MatchNotExhaustive"; 3603, "MatchDuplicateCase"
      3604, "MatchBindingCountMismatch"; 3605, "UnionInsideFunction"; 3606, "MatchUnknownVariant"; 3607, "MatchWithDefault"
      3608, "DuplicateUnionName"; 3609, "ReservedFieldName"
      4001, "HostLinkError"
      4100, "UndefinedFunction"; 4101, "DuplicateFunction"; 4102, "FunctionOverride"; 4103, "FunctionShadowsLibrary"
      4104, "FunctionEffectExceedsMax"; 4105, "HostFunctionOverridesStdlib"; 4106, "MaxEffectNotApplicable"
      4200, "TooFewArguments"; 4201, "TooManyArguments"; 4202, "DuplicateNamedArgument"; 4203, "UnknownNamedArgument"
      4204, "MissingRequiredNamedArgument"; 4300, "VariantShadowsFunction"; 4301, "FunctionShadowsVariant"
      5002, "RuntimeError"; 5003, "CanceledByHost"; 5004, "ScriptFailure"; 5005, "ValueIsReadOnly"
      5006, "HostFunctionError"; 5007, "InputValueIsReadOnly"
      5100, "InvalidType"; 5102, "InvalidCast"; 5103, "ArgumentNotProvided"; 5104, "ArgumentTypeMismatch"
      5105, "CallbackExpected"; 5106, "CallbackArityMismatch"
      5200, "DivisionByZero"; 5201, "ModuloByZero"; 5206, "UnsupportedBinaryOperation"; 5207, "UnsupportedUnaryOperation"
      5209, "ConcatWithInvalidType"; 5210, "StringOperandInAdditionRuntime"
      5300, "IndexOutOfRange"; 5306, "SetPropertyOnNonObject"; 5307, "SetIndexOnNonContainer"; 5308, "InvalidIndexType"
      5309, "DeleteOnNonObject"; 5402, "NotIterable"; 5403, "MatchSubjectNotVariant"; 5500, "UndefinedFunctionRuntime"
      5602, "SchemaInvalid"
      5700, "RegexTimeout"; 5701, "RegexInvalidPattern"; 5702, "GeneratedLengthExceeded"; 5703, "EmptyCharacterSet"
      5704, "StringOrArrayRequired"; 5705, "InvalidPadCharacter"
      5801, "DateFormatStringInvalid"; 5802, "DateFormatInvalid"; 5804, "DateUnitInvalid"; 5805, "DatePartInvalid"
      5806, "DateOutOfRange"
      5900, "StatementLimitExceeded"; 5901, "LoopIterationLimitExceeded"; 5902, "CallDepthLimitExceeded"
      5903, "ExecutionTimeLimitExceeded"; 5904, "IntegerRequired"; 5905, "IncomparableTypes"
      5906, "UnsupportedComparisonOperator"; 5907, "NonNegativeIntegerRequired"; 5909, "InvalidRoundingMode"
      5910, "ValueNestingLimitExceeded"; 5911, "IntegerOutOfRange"; 5913, "NumericElementRequired"
      5914, "NumberDomainError"; 5915, "NumberOverflow"; 5916, "InvalidRange"; 5917, "InvalidJson"
      5918, "UnpairedSurrogate"; 5919, "ValueSizeLimitExceeded"; 5920, "InvalidSortDirection"
      6000, "UnknownCompilerError"; 6001, "NoSourceProvided"; 6002, "InternalStageFailure"
      6003, "CompilationTimeLimitExceeded"
      7000, "ArtifactTooLarge"; 7001, "ArtifactInvalidMagic"; 7002, "ArtifactVersionUnsupported"; 7003, "ArtifactUnreadable"
      7004, "ArtifactEffectExceedsMax"; 7005, "ArtifactCannotEncodeLibraries"; 7006, "ArtifactCannotUseLibraries"
      7007, "ArtifactCannotEncode"; 7008, "ArtifactUnwritable" ]

/// Guards the copy of the table, not the build: the book says it has 122 rows
/// and no two rows share a number (160.7.12, JYRO-REQ-2138).
let tableAsCopiedIsWhole () : unit =
    if messageCodeTable.Length <> 122
       || (messageCodeTable |> List.map fst |> List.distinct).Length <> 122
       || (messageCodeTable |> List.map snd |> List.distinct).Length <> 122 then
        failwithf "The test's copy of the table of 160.7.12 is not 122 distinct rows (%d)" messageCodeTable.Length

/// The table's number of the member `name`, or None when the table has no row for it.
let numberOf (name: string) : int option =
    messageCodeTable |> List.tryFind (fun (_, n) -> n = name) |> Option.map fst
