/// The xUnit collection for tests that change process-wide state (the console,
/// the trace listeners, the host time zone): its tests never run in parallel
/// with any other test.
namespace RealTree.Jyro.Spec.Tests

open Xunit

[<CollectionDefinition("ProcessWide", DisableParallelization = true)>]
type ProcessWideCollection() = class end
