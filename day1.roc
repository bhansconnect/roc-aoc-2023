app "day1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "data/day1/p1-sample.txt" as sample : Str,
        "data/day1/p1-full.txt" as full : Str,
    ]
    provides [main] to pf

main =
    part1 full
    |> Num.toStr
    |> Stdout.line

expect
    res = part1 sample
    res == 142

part1 : Str -> U32
part1 = \data ->
    data
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.map \line ->
        line
        |> Str.toUtf8
        |> List.keepIf isDigit
        |> List.map \x -> x - '0'
        |> extractNum
    |> List.sum

extractNum = \list ->
    when (List.first list, List.last list) is
        (Ok x, Ok y) -> Num.toU32 (10 * x + y)
        _ -> crash "List was empty somehow"

isDigit = \x ->
    x >= '0' && x <= '9'

