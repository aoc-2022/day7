open System.IO
let lines = File.ReadAllLines "/tmp/aoc/input" |> Array.toList // [ for n in a : for b in c : yield c ]

lines |> List.map (printf "%A")