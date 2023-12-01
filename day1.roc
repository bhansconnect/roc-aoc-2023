app "day1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "data/day1/p1-sample.txt" as p1Sample : Str,
        "data/day1/p1-full.txt" as p1Full : Str,
        "data/day1/p2-sample.txt" as p2Sample : Str,
        "data/day1/p2-full.txt" as p2Full : Str,
    ]
    provides [main] to pf

main =
    p1 = part1 p1Full
    p2 = part2 p2Full
    Stdout.line "\(Num.toStr p1)\n\(Num.toStr p2)"

expect
    res = part1 p1Sample
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

part2 : Str -> U32
part2 = \data ->
    data
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.map \line ->
        line
        |> Str.toUtf8
        |> matchNums []
        |> extractNum
    |> List.sum

expect
    res = part2 p2Sample
    res == 281

matchNums = \current, out ->
    next = List.dropFirst current 1
    when current is
        ['e', 'i', 'g', 'h', 't', ..] -> matchNums next (List.append out 8)
        ['s', 'e', 'v', 'e', 'n', ..] -> matchNums next (List.append out 7)
        ['t', 'h', 'r', 'e', 'e', ..] -> matchNums next (List.append out 3)
        ['f', 'i', 'v', 'e', ..] -> matchNums next (List.append out 5)
        ['f', 'o', 'u', 'r', ..] -> matchNums next (List.append out 4)
        ['n', 'i', 'n', 'e', ..] -> matchNums next (List.append out 9)
        ['o', 'n', 'e', ..] -> matchNums next (List.append out 1)
        ['s', 'i', 'x', ..] -> matchNums next (List.append out 6)
        ['t', 'w', 'o', ..] -> matchNums next (List.append out 2)
        [x, ..] if isDigit x -> matchNums next (List.append out (x - '0'))
        [_, ..] -> matchNums next out
        [] -> out

