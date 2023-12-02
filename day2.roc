app "day2"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "data/day2/p1-sample.txt" as p1Sample : Str,
        "data/day2/p1-full.txt" as p1Full : Str,
        "data/day2/p2-sample.txt" as p2Sample : Str,
        "data/day2/p2-full.txt" as p2Full : Str,
    ]
    provides [main] to pf

main =
    p1 = part1 p1Full
    p2 = part2 p2Full
    Stdout.line "\(Num.toStr p1)\n\(Num.toStr p2)"

expect
    res = part1 p1Sample
    res == 8

part1 : Str -> U32
part1 = \data ->
    data
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.map \line ->
        {before, after} = Str.splitFirst line ":" |> unwrap
        valid = lineValid after
        if valid then
            before
            |> Str.splitFirst " "
            |> unwrap
            |> .after
            |> Str.toU32
            |> unwrap
        else
            0
    |> List.sum

lineValid = \after->
    set <- Str.split after ";" |> List.all
    cubes <- set |> Str.trim |> Str.split "," |> List.all
    {before: countStr, after: color} =
        cubes
        |> Str.trim
        |> Str.splitFirst " "
        |> unwrap

    count = Str.toU32 countStr |> unwrap
    when color is
        "red" -> count <= 12
        "green" -> count <= 13
        "blue" -> count <= 14
        _ -> crash "invalid color: \(color)"


expect
    res = part2 p2Sample
    res == 2286

part2 : Str -> U32
part2 = \data ->
    data
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.map \line ->
        {after} = Str.splitFirst line ":" |> unwrap
        Str.split after ";"
        |> List.map counts
        |> List.join
        |> List.walk {r: 0, b: 0, g: 0} \old, new -> {r: Num.max old.r new.r, g: Num.max old.g new.g, b: Num.max old.b new.b}
    |> List.map \{r, g, b} -> r * g * b
    |> List.sum

counts = \set ->
    cubes <- set |> Str.trim |> Str.split "," |> List.map
    {before: countStr, after: color} =
        cubes
        |> Str.trim
        |> Str.splitFirst " "
        |> unwrap

    count = Str.toU32 countStr |> unwrap
    when color is
        "red" -> {r: count, g: 0, b: 0}
        "green" -> {r: 0, g: count, b: 0}
        "blue" -> {r: 0, g: 0, b: count}
        _ -> crash "invalid color: \(color)"

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ -> crash "bad unwrap"
