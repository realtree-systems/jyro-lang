/// Helpers for the tests of the shape of the .NET host API (160.7), shared by the files S160_09_*.fs.
/// Written from the specification alone (FLOORS 4.1.1): the shipped assemblies are read by reflection, the
/// documentation by the XML file that the build publishes beside Jyro.Api.dll, the README by its path.
module RealTree.Jyro.Spec.Tests.S160_09_Reflect

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open System.Xml.Linq
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// The five shipped assemblies (the four inner ones are copied into the package beside Jyro.Api).
let shipped : Assembly list =
    [ "Jyro.Core"; "Jyro.Parser"; "Jyro.Compiler"; "Jyro.Stdlib"; "Jyro.Api" ]
    |> List.map (fun (name: string) -> Assembly.Load(name))

/// Every public type of the shipped assemblies.
let exportedTypes : Type list =
    shipped |> List.collect (fun a -> a.GetExportedTypes() |> List.ofArray)

/// Every public class (not interface, enum, struct or delegate) of the shipped assemblies.
let exportedClasses : Type list =
    exportedTypes
    |> List.filter (fun t -> t.IsClass && not (typeof<Delegate>.IsAssignableFrom t))

let rec typeName (t: Type) : string =
    if t.IsByRef then typeName (t.GetElementType())
    elif t.IsArray then typeName (t.GetElementType()) + "[]"
    elif t.IsGenericType then
        let stem = t.Name.Split('`').[0]
        stem + "<" + String.Join(", ", t.GetGenericArguments() |> Array.map typeName) + ">"
    else t.Name

let private declaredFlags =
    BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly

/// A readable signature of a declared member (accessors are folded into their property or event).
let signature (m: MemberInfo) : string =
    let ps (xs: ParameterInfo[]) = String.Join(", ", xs |> Array.map (fun p -> typeName p.ParameterType))
    match m with
    | :? ConstructorInfo as c -> sprintf ".ctor(%s)" (ps (c.GetParameters()))
    | :? PropertyInfo as p ->
        let setter = if isNull (p.GetSetMethod()) then "" else " set"
        let accessor = if isNull p.GetMethod then p.SetMethod else p.GetMethod
        let kind = if accessor.IsStatic then "static " else ""
        sprintf "%s%s : %s { get%s }" kind p.Name (typeName p.PropertyType) setter
    | :? MethodInfo as mi ->
        sprintf "%s%s(%s) : %s" (if mi.IsStatic then "static " else "") mi.Name (ps (mi.GetParameters())) (typeName mi.ReturnType)
    | :? FieldInfo as f -> sprintf "%sfield %s : %s" (if f.IsStatic then "static " else "") f.Name (typeName f.FieldType)
    | other -> sprintf "%A %s" other.MemberType other.Name

/// The signatures of the public members that `t` itself declares, sorted, with the accessors folded in.
let declaredPublic (t: Type) : string list =
    t.GetMembers(declaredFlags)
    |> Array.filter (fun m ->
        match m with
        | :? MethodInfo as mi -> not mi.IsSpecialName
        | _ -> true)
    |> Array.map signature
    |> Array.sort
    |> List.ofArray

/// The names of every public member of `t`, inherited ones included.
let publicMemberNames (t: Type) : string list =
    t.GetMembers(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static)
    |> Array.map (fun m -> m.Name)
    |> Array.distinct
    |> List.ofArray

/// The constructors of `t` that a host can call: public, protected, or protected-or-internal.
let reachableConstructors (t: Type) : ConstructorInfo list =
    t.GetConstructors(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
    |> Array.filter (fun c -> c.IsPublic || c.IsFamily || c.IsFamilyOrAssembly)
    |> List.ofArray

/// The public type of the shipped assemblies with this simple name, if any.
let tryExported (name: string) : Type option =
    exportedTypes |> List.tryFind (fun t -> t.Name = name)

/// The public type of the shipped assemblies with this simple name, failing the test when there is none.
let exported (name: string) : Type =
    match tryExported name with
    | Some t -> t
    | None -> Values.fail (sprintf "The shipped assemblies have no public type named %s" name)

/// The public instance method `name` of `t` with exactly these parameter types, failing the test when absent.
let methodOf (t: Type) (name: string) (parameters: Type list) : MethodInfo =
    match t.GetMethod(name, BindingFlags.Public ||| BindingFlags.Instance, null, Array.ofList parameters, null) with
    | null -> Values.fail (sprintf "%s has no public instance method %s(%s)" t.Name name (String.Join(", ", parameters |> List.map typeName)))
    | m -> m

/// The public static method `name` of `t` with exactly these parameter types, failing the test when absent.
let staticOf (t: Type) (name: string) (parameters: Type list) : MethodInfo =
    match t.GetMethod(name, BindingFlags.Public ||| BindingFlags.Static, null, Array.ofList parameters, null) with
    | null -> Values.fail (sprintf "%s has no public static method %s(%s)" t.Name name (String.Join(", ", parameters |> List.map typeName)))
    | m -> m

/// Invoke `m`, letting an exception it throws reach the caller as itself, not wrapped.
let invoke (m: MethodBase) (target: obj) (args: obj[]) : obj =
    try
        match m with
        | :? ConstructorInfo as c -> c.Invoke args
        | _ -> m.Invoke(target, args)
    with :? TargetInvocationException as e when not (isNull e.InnerException) ->
        System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(e.InnerException).Throw()
        null

// -- The README and the XML documentation -----------------------------------------------------------------

let repositoryRoot () : string =
    let mutable dir = DirectoryInfo(AppContext.BaseDirectory)
    while not (isNull dir) && not (File.Exists(Path.Combine(dir.FullName, "Jyro.slnx"))) do
        dir <- dir.Parent
    if isNull dir then Values.fail ("Jyro.slnx was not found above " + AppContext.BaseDirectory)
    dir.FullName

/// The text of the README that ships with the package (src/Jyro.Api/README.md).
let apiReadme () : string =
    File.ReadAllText(Path.Combine(repositoryRoot (), "src", "Jyro.Api", "README.md"))

let private squeeze (s: string) : string = Regex.Replace(s, @"\s+", " ").Trim()

/// The XML documentation of Jyro.Api, as member name (`M:RealTree.Jyro.JyroCompiler.Compile`) to its text
/// (summary, remarks and returns, markup removed). The build publishes the file beside the assembly.
let xmlDocs () : (string * string) list =
    let path = Path.Combine(AppContext.BaseDirectory, "Jyro.Api.xml")
    if not (File.Exists path) then
        Values.fail (sprintf "The XML documentation of Jyro.Api is not published beside the assembly (%s); the requirement asks for what it says" path)
    // A type that Jyro.Api re-exports is documented in the file of the assembly that defines it (Jyro.Core.xml
    // for JyroPipelineStats), so every file of a shipped assembly that the build publishes is read.
    [ "Jyro.Api"; "Jyro.Core"; "Jyro.Compiler"; "Jyro.Parser"; "Jyro.Stdlib" ]
    |> List.map (fun name -> Path.Combine(AppContext.BaseDirectory, name + ".xml"))
    |> List.filter File.Exists
    |> List.collect (fun file ->
        XDocument.Load(file).Descendants("member")
        |> Seq.map (fun m -> m.Attribute(XName.Get "name").Value, squeeze m.Value)
        |> List.ofSeq)

/// The documentation entries of the type `t` and of all its members.
let docsOfType (t: Type) : (string * string) list =
    let full = t.FullName
    xmlDocs ()
    |> List.filter (fun (name, _) ->
        let body = name.Substring 2
        body = full || body.StartsWith(full + ".", StringComparison.Ordinal) || body.StartsWith(full + "#", StringComparison.Ordinal))

/// The documentation entries of the public methods named `methodName` of `t`.
let docsOfMethod (t: Type) (methodName: string) : (string * string) list =
    let prefix = "M:" + t.FullName + "." + methodName
    xmlDocs ()
    |> List.filter (fun (name, _) ->
        name = prefix || name.StartsWith(prefix + "(", StringComparison.Ordinal))

// -- Nullable annotations ------------------------------------------------------------------------------------

/// True when the type is one whose null state the annotations describe: a reference type, or an unconstrained
/// type parameter. A value type and `void` have none.
let isAnnotated (t: Type) : bool =
    let t = if t.IsByRef then t.GetElementType() else t
    t <> typeof<Void> && not t.IsPointer && (t.IsGenericParameter || not t.IsValueType)
