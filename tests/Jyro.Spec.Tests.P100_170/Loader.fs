/// The loader of a compiled artifact, reached directly: the sealed class
/// `RealTree.Jyro.JyroArtifactLoader`, with the constructor `JyroArtifactLoader(byte[] artifact)`, the
/// members `WithStdlib`, `AddFunction`, `WithMaxEffect`, `WithMaxCompilationTime`, `WithStats` and
/// `Load() : JyroResult<CompiledProgram>`, and the member `JyroCompiler.CompileToArtifact() :
/// JyroResult<byte[]>` that writes an artifact (JYRO-REQ-1896, JYRO-ADR-199).
///
/// The tests of JYRO-REQ-2066, 2136, 2137, 2139 and 2140 load through here, as the verification plans of
/// those requirements say ("with the load as JyroArtifactLoader.Load()").
module RealTree.Jyro.Spec.Tests.Loader

open System
open RealTree.Jyro
open RealTree.Jyro.Spec

/// `new JyroArtifactLoader(bytes)`, as an object.
let create (bytes: byte[]) : obj = box (JyroArtifactLoader(bytes))

/// `loader.Load()`.
let load (loader: obj) : JyroResult<CompiledProgram> = (loader :?> JyroArtifactLoader).Load()

/// A loader of `bytes` set up as `options` say, and its `Load()`. The options a loader has no member for
/// are not applied: libraries (the loader has no member that adds one, JYRO-ADR-199 item 2) and
/// `ConfigureCompiler`. `Load()` is called once.
let loadWith (options: RunOptions) (bytes: byte[]) : JyroResult<CompiledProgram> =
    if not options.Libraries.IsEmpty then
        Values.fail "JyroArtifactLoader has no member that adds a library script (JYRO-REQ-1896): a load test must not give libraries"
    (Script.loaderFor options bytes).Load()

/// Load `bytes` under `options`, then run the program under `options` (as `Script.runBytesWith` does).
/// A failed load comes back as a failed result carrying its diagnostics.
let runWith (options: RunOptions) (bytes: byte[]) : JyroResult<JyroValue> =
    let loaded = loadWith options bytes
    match loaded.TryGetValue() with
    | true, program when loaded.IsSuccess -> (Script.runnerFor options program).Execute()
    | _ -> JyroResult<JyroValue>.Failure(loaded.Messages)

/// `JyroCompiler.CompileToArtifact()` of `source` under `options`: the artifact the compilation writes.
let compileToArtifactWith (options: RunOptions) (source: string) : JyroResult<byte[]> =
    (Script.compilerFor options).WithSource(source).CompileToArtifact()

/// `compiler.CompileToArtifact()`, for a compiler that is already set up.
let compileToArtifactOf (compiler: JyroCompiler) : JyroResult<byte[]> = compiler.CompileToArtifact()

/// `JyroCompiler.CompileToArtifact()` of `source` with the defaults.
let compileToArtifact (source: string) : JyroResult<byte[]> = compileToArtifactWith Script.defaults source
