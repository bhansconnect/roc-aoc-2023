app "day5"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "data/day5/p1-sample.txt" as p1Sample : Str,
        "data/day5/p1-full.txt" as p1Full : Str,
        "data/day5/p2-sample.txt" as p2Sample : Str,
        "data/day5/p2-full.txt" as p2Full : Str,
    ]
    provides [main] to pf

main =
    p1 = part1 p1Full
    p2 = part2 p2Full
    Stdout.line "\(Num.toStr p1)\n\(Num.toStr p2)"

expect
    res = part1 p1Sample
    res == 35

part1 : Str -> U64
part1 = \str ->
    data = Str.split str "\n\n"
    seeds = parseSeedNums data
    maps = parseMaps data
    
    seeds
    |> List.map (findLocation maps)
    |> List.min
    |> unwrap

expect
    res = part2 p2Sample
    res == 46

part2 : Str -> U64
part2 = \str ->
    data = Str.split str "\n\n"
    seeds =
        data
        |> parseSeedNums 
        |> groupRanges
    maps = parseMaps data
    
    finder = findLocation maps

    max, range <- List.walk seeds Num.maxU64
    innerMax, i <- walkRange range max
    val = finder i
    if val < innerMax then
        val
    else
        innerMax

walkRange = \{start, len}, state, fn ->
    end = start + len
    helper = \current, i ->
        if i < end then
            next = fn current i
            helper next (i + 1)
        else
            current
    helper state start

parseSeedNums = \data ->
    data
    |> List.first
    |> unwrap
    |> Str.splitFirst ":"
    |> unwrap
    |> .after
    |> parseStrNums

groupRanges = \seeds ->
    helper = \s, ranges ->
        when s is
            [start, len, .. as rest] ->
                helper rest (List.append ranges {start, len})
            [_] -> crash "one element left?"
            [] -> ranges

    helper seeds []
        

parseMaps = \data ->
    map <- data
            |> List.dropFirst 1
            |> List.map Str.trim
            |> List.dropIf Str.isEmpty
            |> List.map
    entry <- map
            |> Str.split "\n"
            |> List.dropFirst 1
            |> List.map
    entry
    |> parseStrNums
    |> toEntryTuple

findLocation = \maps ->
    \baseSeed ->
        lastSeed, map <- List.walk maps baseSeed
        currentSeed, {dest, src, len} <- List.walkUntil map lastSeed
        if src <= currentSeed && currentSeed < src + len then
            Break (currentSeed - src + dest)
        else
            Continue currentSeed


toEntryTuple = \list ->
    when list is
        [dest, src, len] -> {dest, src, len}
        _ -> crash "bad entry"

parseStrNums = \str ->
    str
    |> Str.split " "
    |> List.map Str.trim
    |> List.dropIf Str.isEmpty
    |> List.mapTry Str.toU64
    |> unwrap

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ -> crash "bad unwrap"
