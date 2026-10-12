namespace RealTree.Jyro.Tests

open FsCheck
open FsCheck.FSharp
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

/// Generators for runtime property tests.
///
/// Unlike the parser tests, runtime tests generally don't generate whole
/// programs - they generate the *input data* for a hand-crafted script.
/// Most generators here produce `JyroValue`s of a specific kind.
module RuntimeGenerators =

    /// A finite non-NaN number.
    ///
    /// Finiteness is the language rule, not a convenience for the generator: a
    /// Jyro number is finite and `JyroNumber`'s double constructor rejects NaN
    /// and the infinities, so a generator producing them would be producing
    /// values no script can hold. The filters that drop non-finite floats
    /// before building numbers, here and throughout `StdlibLaws`, say the same
    /// thing and are load-bearing for the same reason.
    ///
    /// They used to read as the suite being written *around* a hole - which is
    /// how nine audit lenses missed that `Log(0)` produced an unserializable
    /// result. What closes that gap is not deleting the filters but asserting
    /// the refusals directly; see `FiniteNumbers.fs`.
    let genFiniteNumber : Gen<float> =
        Gen.frequency [
            5, Gen.choose(-1000, 1000) |> Gen.map float
            2, gen {
                let! n = Gen.choose(-100000, 100000)
                let! d = Gen.choose(1, 1000)
                return float n / float d
            }
            1, Gen.elements [ 0.0; 1.0; -1.0; 0.5 ]
        ]

    /// A short printable string.
    let genShortString : Gen<string> =
        gen {
            let! len = Gen.choose(0, 8)
            let chars =
                [ for c in 'a' .. 'z' -> c ]
                @ [ for c in 'A' .. 'Z' -> c ]
                @ [ for c in '0' .. '9' -> c ]
                @ [ ' '; '-'; '_' ]
            let! cs = Gen.listOfLength len (Gen.elements chars)
            return System.String(Array.ofList cs)
        }

    /// Primitive JyroValues: null / bool / number / string.
    let genPrimitive : Gen<JyroValue> =
        Gen.oneof [
            Gen.constant (JyroNull.Instance :> JyroValue)
            Gen.elements [ true; false ] |> Gen.map (fun b -> JyroBoolean.FromBoolean(b) :> JyroValue)
            genFiniteNumber |> Gen.map (fun n -> JyroNumber(n) :> JyroValue)
            genShortString |> Gen.map (fun s -> JyroString(s) :> JyroValue)
        ]

    /// An array of finite numbers (tight enough that stdlib tests complete
    /// quickly but large enough to exercise boundary cases).
    let genNumberArray : Gen<JyroValue> =
        gen {
            let! n = Gen.choose(0, 10)
            let! xs = Gen.listOfLength n genFiniteNumber
            let arr = JyroArray()
            for x in xs do
                arr.Add(JyroNumber(x))
            return arr :> JyroValue
        }

    /// An array of primitive values (mixed types).
    let genMixedArray : Gen<JyroValue> =
        gen {
            let! n = Gen.choose(0, 8)
            let! xs = Gen.listOfLength n genPrimitive
            let arr = JyroArray()
            for x in xs do
                arr.Add(x)
            return arr :> JyroValue
        }

    /// Malformed / hostile JSON literals. Covers the shapes an attacker is
    /// likely to feed to `FromJson`: trailing commas, lone high/low surrogate
    /// escapes, absurdly wide numbers, missing closing brackets. These are
    /// NOT valid JSON - the property using them asserts the parser rejects
    /// cleanly via a diagnostic rather than throwing.
    let genHostileJsonLiteral : Gen<string> =
        Gen.elements [
            "{\"a\": 1,}"                          // trailing comma in object
            "[1, 2, 3,]"                           // trailing comma in array
            "{\"a\": \"\\uD800\"}"                 // lone high surrogate
            "{\"a\": \"\\uDC00\"}"                 // lone low surrogate
            "{\"a\": 1e999999}"                    // absurd exponent
            "{\"a\": 99999999999999999999999999}"  // over-wide integer
            "{\"a\": 1"                            // missing closing brace
            "[1, 2"                                // missing closing bracket
            "{'a': 1}"                             // single quotes (not JSON)
            "{a: 1}"                               // unquoted key
            "{\"a\": NaN}"                         // NaN literal
            "{\"a\": Infinity}"                    // Infinity literal
            "{\"a\": undefined}"                   // undefined
            "{\"a\": 01}"                          // leading-zero number
            ""                                     // empty
            "null\0null"                           // embedded NUL
        ]

    type RuntimeArbitraries =
        static member FiniteNumber() : Arbitrary<float> = genFiniteNumber |> Arb.fromGen
        static member ShortString() : Arbitrary<string> = genShortString |> Arb.fromGen
