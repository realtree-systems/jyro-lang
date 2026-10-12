module RealTree.Jyro.Benchmarks.BenchmarkHelpers

open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

// ---------------------------------------------------------------------------
// Script constants at varying complexity levels
// ---------------------------------------------------------------------------

/// Minimal script: single assignment
let trivialScript = "Output.x = 1 + 2"

/// Simple script: variables, arithmetic, conditional
let simpleScript = """
var x = 10
var y = 20
var z = x * y + 5
if z > 100 then
    Output.result = z
else
    Output.result = 0
end
"""

/// Medium script: functions, loops, conditionals, objects
let mediumScript = """
func Factorial(n)
    if n <= 1 then
        return 1
    end
    return n * Factorial(n - 1)
end

var results = []
for i in 1 to 11 do
    results = Append(results, {n: i, factorial: Factorial(i)})
end
Output.results = results
Output.sum = 0
foreach r in results do
    Output.sum += r.factorial
end
"""

/// Complex script: real-world order-processing pipeline using stdlib
let complexScript = """
var processed = []
foreach order in Input.orders do
    var lineItems = []
    var subtotal = 0

    foreach item in order.items do
        var lineTotal = item.price * item.qty
        lineItems = Append(lineItems, Merge([item, {total: lineTotal}]))
        subtotal += lineTotal
    end

    var discount = 0
    var couponCode = order.coupon ?? "NONE"
    if order.coupon != null and HasProperty(Input.discounts, order.coupon) then
        discount = subtotal * Input.discounts[order.coupon]
    end
    var finalTotal = subtotal - discount

    var multiplier = Input.tierMultipliers[order.customer.tier] ?? 1.0
    var points = Floor(finalTotal * multiplier)

    processed = Append(processed, {
        id: order.id,
        customer: order.customer.name,
        itemCount: Length(order.items),
        subtotal: subtotal,
        coupon: couponCode,
        discount: discount,
        total: finalTotal,
        loyaltyPoints: points
    })
end
Output.processed = processed

var allTotals = Map(processed, p => p.total)
Output.summary = {
    orderCount: Length(processed),
    revenue: Sum(allTotals),
    averageOrder: Average(allTotals),
    largestOrder: Max(allTotals),
    totalPoints: Sum(Map(processed, p => p.loyaltyPoints))
}

Output.goldOrders = Map(
    Where(Input.orders, o => o.customer.tier == "gold"),
    o => o.id
)
"""

/// Simple loop script for resource limiter benchmarks (uses Input.n as iteration count)
let loopScript = """
var sum = 0
for i in 1 to Input.n do
    sum += i
end
Output.result = sum
"""

/// Stdlib-heavy script for array operations (uses Input.items array)
let stdlibArrayScript = """
var mapped = Map(Input.items, x => x * 2)
var filtered = Where(mapped, x => x > 50)
var sorted = Sort(filtered)
Output.result = sorted
Output.count = Length(sorted)
Output.total = Sum(sorted)
"""

/// Stdlib-heavy script for string operations
let stdlibStringScript = """
var parts = Split(Input.input, ",")
var trimmed = Map(parts, s => Trim(s))
var upper = Map(trimmed, s => ToUpper(s))
Output.result = Join(upper, " | ")
Output.count = Length(upper)
"""

// ---------------------------------------------------------------------------
// Data generators - return fresh instances each call
// ---------------------------------------------------------------------------

let emptyData () = JyroObject() :> JyroValue

let simpleData () =
    let obj = JyroObject()
    obj :> JyroValue

let orderPipelineData () =
    JyroValue.FromJson("""
    {
        "orders": [
            {
                "id": "ORD-001",
                "customer": {"name": "Alice", "tier": "gold"},
                "coupon": "SAVE10",
                "items": [
                    {"sku": "WIDGET-A", "price": 25.00, "qty": 2},
                    {"sku": "GADGET-B", "price": 49.99, "qty": 1}
                ]
            },
            {
                "id": "ORD-002",
                "customer": {"name": "Bob", "tier": "silver"},
                "coupon": null,
                "items": [
                    {"sku": "WIDGET-A", "price": 25.00, "qty": 5},
                    {"sku": "GIZMO-C", "price": 15.00, "qty": 3}
                ]
            },
            {
                "id": "ORD-003",
                "customer": {"name": "Charlie", "tier": "gold"},
                "coupon": "SAVE20",
                "items": [
                    {"sku": "GADGET-B", "price": 49.99, "qty": 2},
                    {"sku": "GIZMO-C", "price": 15.00, "qty": 10}
                ]
            }
        ],
        "discounts": {"SAVE10": 0.10, "SAVE20": 0.20},
        "tierMultipliers": {"gold": 2.0, "silver": 1.5, "bronze": 1.0}
    }
    """)

let loopData (n: int) =
    let obj = JyroObject()
    obj.SetProperty("n", JyroNumber(float n))
    obj :> JyroValue

let arrayData (n: int) =
    let arr = JyroArray()
    for i in 1..n do
        arr.Add(JyroNumber(float i))
    let obj = JyroObject()
    obj.SetProperty("items", arr)
    obj :> JyroValue

let stringData (n: int) =
    let parts = [| for i in 1..n -> $"  item-{i}  " |]
    let obj = JyroObject()
    obj.SetProperty("input", JyroString(System.String.Join(",", parts)))
    obj :> JyroValue

// ---------------------------------------------------------------------------
// Compilation helpers
// ---------------------------------------------------------------------------

let private stdlibFunctions = StdlibRegistry.getAll ()

let compileOrFail (source: string) =
    match Compiler.compileFromSource source [] [] FunctionEffect.Mutating FunctionEffect.NonDeterministic with
    | Produced compiled -> compiled
    | r -> failwith $"Compilation failed: %A{r.Messages}"

let compileWithStdlibOrFail (source: string) =
    match Compiler.compileFromSource source stdlibFunctions [] FunctionEffect.Mutating FunctionEffect.NonDeterministic with
    | Produced compiled -> compiled
    | r -> failwith $"Compilation failed: %A{r.Messages}"

let compileToBytesOrFail (source: string) =
    match Compiler.compileToJyrx source stdlibFunctions FunctionEffect.Mutating with
    | Produced bytes -> bytes
    | r -> failwith $"CompileToJyrx failed: %A{r.Messages}"
