namespace Utils

open Fable.PowerPack

module PromiseModule =
    let map f p =
        promise {
            let! r = p
            return f r
        }

module List =
    let tryTake num (items:'a list) =
        if items.Length <= num then items
        else items |> List.take num
