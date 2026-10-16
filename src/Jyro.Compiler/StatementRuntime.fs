namespace RealTree.Jyro.Compilation

open RealTree.Jyro
open RealTree.Jyro.Ast

/// The runtime checks of statements that compiled code calls: written once
/// here as ordinary methods rather than spelled out as expression trees.
/// Internal; expression trees compile with visibility checks relaxed.
[<AbstractClass; Sealed>]
type internal StatementRuntime =

    /// The checks a `for` loop makes after it has evaluated `start`, `end`
    /// and `step`, before its first iteration and also when it would run
    /// none, in this order (spec 50.5.9, JYRO-ADR-19): `start` is a number,
    /// then `end` is a number (50.5.7, `UnsupportedBinaryOperation`, with the
    /// comparison the loop makes and the types of its two operands), then
    /// `step` is a positive integer (50.5.6, `NonNegativeIntegerRequired`).
    static member CheckForValues(start: JyroValue, finish: JyroValue, step: JyroValue, ascending: bool) : unit =
        let comparison = Operators.binaryOpToString (if ascending then LessThan else GreaterThan)
        if start.ValueType <> JyroValueType.Number || finish.ValueType <> JyroValueType.Number then
            JyroError.raiseRuntime MessageCode.UnsupportedBinaryOperation
                [| box comparison
                   box (TypeNames.typeOfName start.ValueType)
                   box (TypeNames.typeOfName finish.ValueType) |]
        let positiveInteger =
            match step with
            | :? JyroNumber as n -> n.IsInteger && JyroNumber.Compare(n, JyroNumber(0)) > 0
            | _ -> false
        if not positiveInteger then
            // The statement's keyword, the name `step`, the step's value and
            // the least step accepted, each its own argument (spec 130.6.2,
            // JYRO-ADR-125 item 2).
            JyroError.raiseRuntime MessageCode.NonNegativeIntegerRequired
                [| box "for"; box "step"; box step; box 1 |]

    /// The `k`-th value of a `for` loop: `start + k * step` in a `to` loop
    /// (spec 50.5.4), `start - k * step` in a `downto` loop (50.5.5), each
    /// computed from `start` by the operators rather than by stepping the
    /// previous value, so an inexact `start` neither drifts nor stalls. `k`
    /// is exact; the product and the sum are what `*` and `+`/`-` give.
    static member ForValue(start: JyroValue, step: JyroValue, k: int64, ascending: bool) : JyroValue =
        let offset = (JyroNumber(k) :> JyroValue).EvaluateBinary(Multiply, step)
        start.EvaluateBinary((if ascending then Add else Subtract), offset)

    /// The variant a `match` subject is a value of: the string of its
    /// `_variant` property when it is an object and that property is a
    /// string, and `null` otherwise (spec 50.9.9, JYRO-ADR-9).
    static member VariantOf(subject: JyroValue) : string | null =
        if isNull (box subject) || subject.ValueType <> JyroValueType.Object then null
        else
            match subject.GetProperty("_variant") with
            | :? JyroString as s -> s.Value
            | _ -> null

    /// The error of a `match` whose subject is not a value of a variant that
    /// one of its cases names (spec 50.9.9).
    static member SubjectNotVariant() : JyroValue =
        JyroError.raiseRuntime MessageCode.MatchSubjectNotVariant Array.empty

    /// `delete target[index]` (spec 50.12). An object is indexed by a
    /// string: any other key names no property, `InvalidIndexType`, and is
    /// not read as a missing property (50.12.5). A value that is not an
    /// object has no properties for any key to name, `DeleteOnNonObject`
    /// whatever the key (50.12.4, JYRO-ADR-19).
    static member DeleteAtIndex(target: JyroValue, index: JyroValue) : unit =
        if target.ValueType = JyroValueType.Object then
            match index with
            | :? JyroString as s -> target.DeleteProperty(s.Value) |> ignore
            | _ ->
                JyroError.raiseRuntime MessageCode.InvalidIndexType
                    [| box (TypeNames.typeOfName JyroValueType.Object)
                       box (TypeNames.typeOfName index.ValueType) |]
        else
            // The key is only the message's first argument, made without
            // raising: a key with no text is named by its type, and the error
            // stays DeleteOnNonObject (JYRO-ADR-161).
            target.DeleteProperty(ValueWalk.MessageText(index)) |> ignore
