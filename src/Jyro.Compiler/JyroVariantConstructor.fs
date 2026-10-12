namespace RealTree.Jyro.Compilation

open RealTree.Jyro
open RealTree.Jyro.Ast

open System.Collections.Generic

/// A variant constructor function generated from a union declaration.
/// Creates a tagged JyroObject with _variant discriminator and named fields.
/// Consumers recover the union name from `ctx.VariantToUnion`, the field
/// names from `variant.Fields`, and the variant name from `IJyroFunction.Name`.
///
/// `fields` holds each field's name and the type its argument is coerced to,
/// `None` for a field with no hint or the hint `any`: an argument to a hinted
/// field is coerced to the hint as 30.12 gives, as a hinted parameter's is
/// (spec 50.10.8, 80.6.1).
type internal JyroVariantConstructor(variantName: string, signature: JyroFunctionSignature, fields: (string * JyroValueType option) list) =

    interface IJyroFunction with
        member _.Name = variantName
        member _.Signature = signature
        member _.Effect = FunctionEffect.Pure
        member _.Execute(args: IReadOnlyList<JyroValue>, _ctx: JyroExecutionContext) =
            let obj = JyroObject()
            obj.SetProperty("_variant", JyroString(variantName))
            fields |> List.iteri (fun i (fieldName, hint) ->
                let value = if i < args.Count then args.[i] else JyroNull.Instance :> JyroValue
                let value =
                    match hint with
                    | Some targetType -> JyroValue.CoerceToType(value, targetType)
                    | None -> value
                obj.SetProperty(fieldName, value))
            obj :> JyroValue

module internal JyroVariantConstructor =
    /// The type a field's argument is coerced to: none for no hint or `any`.
    let private coercionOf (hint: JyroType option) : JyroValueType option =
        match hint with
        | None
        | Some AnyType -> None
        | Some NumberType -> Some JyroValueType.Number
        | Some StringType -> Some JyroValueType.String
        | Some BooleanType -> Some JyroValueType.Boolean
        | Some ObjectType -> Some JyroValueType.Object
        | Some ArrayType -> Some JyroValueType.Array
        | Some NullType -> Some JyroValueType.Null
        | Some FunctionType -> Some JyroValueType.Function

    let create (variant: UnionVariant) : JyroVariantConstructor =
        let paramDefs =
            variant.Fields |> List.map (fun (pName, typeHint) ->
                Parameter.Required(pName, JyroUserFunction.paramTypeFromJyroType typeHint))
        let signature = JyroFunctionSignature.Create(paramDefs, ParameterType.Object)
        JyroVariantConstructor(variant.Name, signature, variant.Fields |> List.map (fun (n, h) -> n, coercionOf h))
