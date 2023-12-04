app "day4"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "data/day4/p1-sample.txt" as p1Sample : Str,
        "data/day4/p1-full.txt" as p1Full : Str,
        "data/day4/p2-sample.txt" as p2Sample : Str,
        "data/day4/p2-full.txt" as p2Full : Str,
    ]
    provides [main] to pf

main =
    p1 = part1 p1Full
    p2 = part2 p2Full
    Stdout.line "\(Num.toStr p1)\n\(Num.toStr p2)"

expect
    res = part1 p1Sample
    res == 13

part1 : Str -> U32
part1 = \str ->
    str
    |> parse
    |> List.keepIf \len -> len > 0
    |> List.map \len -> Num.shiftLeftBy 1 (len - 1)
    |> List.sum

parse = \str ->
    str
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.map \line ->
        {before: card, after: winning} = Str.splitFirst line "|" |> unwrap
        {after: cardNumbers} = Str.splitFirst card ":" |> unwrap

        Set.intersection (parseStrNums cardNumbers) (parseStrNums winning)
        |> Set.len
        |> Num.toU8


parseStrNums = \str ->
    str
    |> Str.split " "
    |> List.map Str.trim
    |> List.dropIf Str.isEmpty
    |> List.mapTry Str.toU32
    |> unwrap
    |> Set.fromList

expect
    res = part2 p2Sample
    res == 30

part2 : Str -> U32
part2 = \str ->
    counts = str |> parse
    len = List.len counts

    List.walkWithIndex counts (List.repeat 1 len) \copies, count, baseIndex ->
        copy = List.get copies baseIndex |> unwrap
        List.range {start: At (baseIndex + 1), end: Length (Num.toNat count)}
        |> List.walk copies \state, i ->
            List.update state i \x -> x + copy 
    |> List.sum
        

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            dbg res
            crash "bad unwrap"
