namespace RealTree.Jyro


// Shorthands the standard library uses to declare its signatures. Internal:
// a host declares a signature with `JyroFunctionSignature.Create`
// (JYRO-ADR-192 item 5).
//
// Every helper takes its parameter names from the call site. They used to be
// hardcoded - `value` for unary, `left`/`right` for binary - and a parameter
// name is not private detail: the linker resolves a named-argument call
// against `Signature.Parameters`, so those literals were the caller-visible
// API of 58 of the 97 standard library functions.
//
// Names must avoid the reserved keywords of Part 20, since a named-argument
// label is an identifier - which rules out the obvious `array`, `object`,
// `string` and `number`.
module internal Signatures =
    // A signature with one required parameter.
    let unary (argName: string) (argType: ParameterType) (returnType: ParameterType) : JyroFunctionSignature =
        JyroFunctionSignature.Create(returnType, Parameter.Required(argName, argType))

    // A signature with two required parameters.
    let binary (arg1Name: string) (arg1Type: ParameterType) (arg2Name: string) (arg2Type: ParameterType) (returnType: ParameterType) : JyroFunctionSignature =
        JyroFunctionSignature.Create(
            returnType, Parameter.Required(arg1Name, arg1Type), Parameter.Required(arg2Name, arg2Type))

    // A signature with the parameters given, in order.
    let create (parameters: seq<Parameter>) (returnType: ParameterType) : JyroFunctionSignature =
        JyroFunctionSignature.Create(parameters, returnType)
