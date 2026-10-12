namespace RealTree.Jyro

open System
open System.Collections
open System.Collections.Generic

// The properties of an object, in the order in which they were added,
// earliest first (spec 30.5.9, JYRO-ADR-6).
//
// `Dictionary<K, V>` was used here and enumerates in that order only until
// a key is removed: the next key added takes the freed slot, so after
// `o = {a: 1, b: 2}  delete o.a  o.a = 3` it enumerated `a` before `b`. A
// property removed and added again is a new addition and goes after every
// property the object has.
//
// The entries are kept in two lists in the order they were added, with an
// index from key to position. A removal leaves a gap, which enumeration
// passes over, so it costs no more than a dictionary removal; the gaps are
// closed up when a key is next added and they are at least half the lists,
// so the work of closing them is paid for by the removals that made them.
//
// As with `Dictionary`, adding a key while the properties are being
// enumerated invalidates the enumeration, and writing to a key the object
// already has or removing one does not.
[<Sealed>]
type internal OrderedProperties<'V>(capacity: int) =
    let index = Dictionary<string, int>(capacity)
    let keys = ResizeArray<string>(capacity)
    let values = ResizeArray<'V>(capacity)
    // The number of gaps that removals left in `keys` and `values`.
    let mutable gaps = 0
    let mutable version = 0

    // Closes up the gaps. Counts each entry moved for the execution check
    // (spec 100.2.5).
    let compact () =
        let mutable write = 0
        for read = 0 to keys.Count - 1 do
            ExecutionCheck.Tick()
            let key = keys.[read]
            if not (isNull key) then
                if write <> read then
                    keys.[write] <- key
                    values.[write] <- values.[read]
                    index.[key] <- write
                write <- write + 1
        keys.RemoveRange(write, keys.Count - write)
        values.RemoveRange(write, values.Count - write)
        gaps <- 0

    new() = OrderedProperties<'V>(0)

    // The number of properties.
    member _.Count = index.Count

    // Changes when a key is added or every key removed; read by the
    // enumerator, which refuses to go on after such a change.
    member internal _.Version = version
    member internal _.KeyList = keys
    member internal _.ValueList = values

    member _.ContainsKey(key: string) = index.ContainsKey key

    member _.TryGetValue(key: string, [<Runtime.InteropServices.Out>] value: byref<'V>) : bool =
        let mutable i = 0
        if index.TryGetValue(key, &i) then
            value <- values.[i]
            true
        else
            value <- Unchecked.defaultof<'V>
            false

    // Writes `value` at `key`: in place when the key is present, which keeps
    // its position, and after every property otherwise.
    member _.Set(key: string, value: 'V) =
        let mutable i = 0
        if index.TryGetValue(key, &i) then values.[i] <- value
        else
            if gaps > 0 && gaps * 2 >= keys.Count then compact ()
            index.Add(key, keys.Count)
            keys.Add key
            values.Add value
            version <- version + 1

    // Removes the property at `key`, returning whether it was present.
    member _.Remove(key: string) : bool =
        let mutable i = 0
        if index.TryGetValue(key, &i) then
            index.Remove key |> ignore
            if index.Count = 0 then
                keys.Clear()
                values.Clear()
                gaps <- 0
            else
                keys.[i] <- null
                values.[i] <- Unchecked.defaultof<'V>
                gaps <- gaps + 1
            true
        else false

    member _.Clear() =
        index.Clear()
        keys.Clear()
        values.Clear()
        gaps <- 0
        version <- version + 1

    // An enumerator of the properties in order. A struct, so a walk that
    // holds it in a local allocates nothing for it.
    member this.GetEnumerator() = new OrderedPropertiesEnumerator<'V>(this)

    // The keys, in order.
    member this.Keys : IEnumerable<string> =
        seq {
            for kvp in this :> IEnumerable<KeyValuePair<string, 'V>> do
                yield kvp.Key
        }

    // The values, in order.
    member this.Values : IEnumerable<'V> =
        seq {
            for kvp in this :> IEnumerable<KeyValuePair<string, 'V>> do
                yield kvp.Value
        }

    // The values, in order, as a new array: a snapshot.
    member _.ValuesArray() : 'V[] =
        let result = Array.zeroCreate<'V> index.Count
        let mutable write = 0
        for read = 0 to keys.Count - 1 do
            ExecutionCheck.Tick()
            if not (isNull keys.[read]) then
                result.[write] <- values.[read]
                write <- write + 1
        result

    interface IEnumerable<KeyValuePair<string, 'V>> with
        member this.GetEnumerator() : IEnumerator<KeyValuePair<string, 'V>> =
            this.GetEnumerator() :> IEnumerator<KeyValuePair<string, 'V>>
        member this.GetEnumerator() : IEnumerator =
            this.GetEnumerator() :> IEnumerator

// The enumerator of `OrderedProperties`: each property in order, passing
// over the gaps that removals left.
and [<Struct>] internal OrderedPropertiesEnumerator<'V> =
    val private owner: OrderedProperties<'V>
    val private version: int
    val mutable private position: int
    val mutable private current: KeyValuePair<string, 'V>

    new(owner: OrderedProperties<'V>) =
        { owner = owner
          version = owner.Version
          position = -1
          current = Unchecked.defaultof<KeyValuePair<string, 'V>> }

    member e.MoveNext() : bool =
        if e.owner.Version <> e.version then
            raise (InvalidOperationException("Collection was modified; enumeration operation may not execute."))
        let keys = e.owner.KeyList
        let mutable found = false
        e.position <- e.position + 1
        while not found && e.position < keys.Count do
            let key = keys.[e.position]
            if isNull key then
                // A run of gaps is work that grows with the removals made
                // (spec 100.2.5).
                ExecutionCheck.Tick()
                e.position <- e.position + 1
            else
                e.current <- KeyValuePair(key, e.owner.ValueList.[e.position])
                found <- true
        if not found then e.current <- Unchecked.defaultof<KeyValuePair<string, 'V>>
        found

    member e.Current : KeyValuePair<string, 'V> = e.current

    interface IEnumerator<KeyValuePair<string, 'V>> with
        member e.Current : KeyValuePair<string, 'V> = e.current
        member e.Current : obj = box e.current
        member e.MoveNext() = e.MoveNext()
        member e.Reset() =
            e.position <- -1
            e.current <- Unchecked.defaultof<KeyValuePair<string, 'V>>
        member _.Dispose() = ()
