app "day3"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "data/day3/p1-sample.txt" as p1Sample : Str,
        "data/day3/p1-full.txt" as p1Full : Str,
        "data/day3/p2-sample.txt" as p2Sample : Str,
        "data/day3/p2-full.txt" as p2Full : Str,
    ]
    provides [main] to pf

main =
    p1 = part1 p1Full
    p2 = part2 p2Full
    Stdout.line "\(Num.toStr p1)\n\(Num.toStr p2)"

expect
    res = part1 p1Sample
    res == 4361

part1 : Str -> U32
part1 = \str ->
    data =
        str
        |> Str.toUtf8
        |> List.dropIf \x -> x == '\n'

    size =
        str
        |> Str.split "\n"
        |> List.dropIf Str.isEmpty
        |> List.len

    sizeI32 = Num.toI32 size

    loadNum = \start, end ->
        data
        |> List.sublist { start, len: end - start + 1 }
        |> List.map \x -> x - '0'
        |> List.walk 0 \accum, x -> accum * 10 + (Num.toU32 x)

    data
    |> List.walkWithIndex { start: None, nums: [] } \{ start, nums }, val, i ->
        digit = isDigit val
        when start is
            None ->
                if digit then
                    { start: Index i, nums }
                else
                    { start: None, nums }

            Index index ->
                if !digit then
                    end = i - 1
                    num = loadNum index end
                    { start: None, nums: List.append nums { start: index, end, num } }
                else
                    { start: start, nums }
    |> \{ start, nums } ->
        # handle edge case of number at end.
        when start is
            Index index ->
                end = (List.len data) - 1
                num = loadNum index end
                List.append nums { start: index, end, num }

            _ ->
                nums
    |> List.map \{ start, end, num } ->
        startRow = (start // size) |> Num.toI32
        startCol = (start % size) |> Num.toI32

        endRow = (end // size) |> Num.toI32
        endCol = (end % size) |> Num.toI32

        touchingStart = (
            Num.max (startRow - 1) 0,
            Num.max (startCol - 1) 0,
        )
        touchingEnd = (
            Num.min (endRow + 1) (sizeI32 - 1),
            Num.min (endCol + 1) (sizeI32 - 1),
        )
        { touchingStart, touchingEnd, num }
    |> List.keepIf \{ touchingStart, touchingEnd } ->
        containsSymbol data sizeI32 touchingStart touchingEnd
    |> List.map .num
    |> List.sum

containsSymbol = \data, size, (startRow, startCol), (endRow, endCol) ->
    List.range { start: At startRow, end: At endRow }
    |> List.map \row ->
        List.range { start: At startCol, end: At endCol }
        |> List.map \col ->
            index = Num.toNat (row * size + col)
            sym = List.get data index |> unwrap
            sym
    |> List.join
    |> List.any validSymbol

validSymbol = \x ->
    !(isDigit x) && x != '.'

expect
    res = part2 p2Sample
    res == 467835

part2 : Str -> U32
part2 = \str ->
    data =
        str
        |> Str.toUtf8
        |> List.dropIf \x -> x == '\n'

    size =
        str
        |> Str.split "\n"
        |> List.dropIf Str.isEmpty
        |> List.len

    sizeI32 = Num.toI32 size

    loadNum = \start, end ->
        data
        |> List.sublist { start, len: end - start + 1 }
        |> List.map \x -> x - '0'
        |> List.walk 0 \accum, x -> accum * 10 + (Num.toU32 x)

    data
    |> List.walkWithIndex { start: None, nums: [] } \{ start, nums }, val, i ->
        digit = isDigit val
        when start is
            None ->
                if digit then
                    { start: Index i, nums }
                else
                    { start: None, nums }

            Index index ->
                if !digit then
                    end = i - 1
                    num = loadNum index end
                    { start: None, nums: List.append nums { start: index, end, num } }
                else
                    { start: start, nums }
    |> \{ start, nums } ->
        # handle edge case of number at end.
        when start is
            Index index ->
                end = (List.len data) - 1
                num = loadNum index end
                List.append nums { start: index, end, num }

            _ ->
                nums
    |> List.map \{ start, end, num } ->
        startRow = (start // size) |> Num.toI32
        startCol = (start % size) |> Num.toI32

        endRow = (end // size) |> Num.toI32
        endCol = (end % size) |> Num.toI32

        touchingStart = (
            Num.max (startRow - 1) 0,
            Num.max (startCol - 1) 0,
        )
        touchingEnd = (
            Num.min (endRow + 1) (sizeI32 - 1),
            Num.min (endCol + 1) (sizeI32 - 1),
        )
        { touchingStart, touchingEnd, num }
    |> List.map \{ touchingStart, touchingEnd, num } ->
        gears = touchingGears data sizeI32 touchingStart touchingEnd
        { num, gears }
    |> List.walk (Dict.empty {}) \dict, { gears, num } ->
        List.walk gears dict \innerDict, gear ->
            when Dict.get innerDict gear is
                Ok nums ->
                    Dict.insert innerDict gear (List.append nums num)

                Err _ ->
                    Dict.insert innerDict gear [num]
    |> Dict.values
    |> List.keepIf \x -> List.len x == 2
    |> List.map List.product
    |> List.sum

touchingGears = \data, size, (startRow, startCol), (endRow, endCol) ->
    List.range { start: At startRow, end: At endRow }
    |> List.map \row ->
        List.range { start: At startCol, end: At endCol }
        |> List.map \col ->
            index = Num.toNat (row * size + col)
            sym = List.get data index |> unwrap
            { sym, index }
    |> List.join
    |> List.keepIf \{ sym } -> sym == '*'
    |> List.map .index

isDigit = \x ->
    x >= '0' && x <= '9'

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ -> crash "bad unwrap"
