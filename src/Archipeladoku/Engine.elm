module Archipeladoku.Engine exposing (..)

import Dict exposing (Dict)
import MultiDict exposing (MultiDict)
import Random
import Random.List
import Set exposing (Set)


type CellValue
    = Given Int
    | UserInput Int
    | Empty


type alias Game =
    { boards : Dict Int Board
    }


type alias Board =
    { solution : MultiDict Int Int
    , current : MultiDict Int CellValue
    , blockSize : Int
    , puzzleAreas : List PuzzleAreas
    }


type alias Area =
    { startRow : Int
    , startCol : Int
    , endRow : Int
    , endCol : Int
    }


type alias PuzzleAreas =
    { blocks : List Area
    , rows : List Area
    , cols : List Area
    }


type alias GenerateArgs =
    { blockSize : Int
    , overlap : Int
    , numberOfBoards : Int
    , seed : Random.Seed
    }


generate : GenerateArgs -> Result String Board
generate args =
    if args.blockSize < 2 || args.blockSize > 5 then
        Err "Block size must be between 2 and 5."

    else if args.overlap < 0 || args.overlap >= args.blockSize * args.blockSize then
        Err "Overlap must be non-negative and less than the board size."

    else if args.numberOfBoards < 1 then
        Err "Number of boards must be at least 1."

    else
        generateWithValidArgs args


generateWithValidArgs : GenerateArgs -> Result String Board
generateWithValidArgs args =
    let
        positions : List ( Int, Int )
        positions =
            positionBoards args.blockSize args.overlap args.numberOfBoards

        boardSize : Int
        boardSize =
            args.blockSize * args.blockSize

        puzzleAreas : List PuzzleAreas
        puzzleAreas =
            List.map
                (\( row, col ) ->
                    buildPuzzleAreas args.blockSize row col
                )
                positions

        cellAreas : MultiDict Int (List Area)
        cellAreas =
            List.foldl
                (\area acc ->
                    let
                        areaCells : List ( Int, Int )
                        areaCells =
                            getAreaCells area
                    in
                    List.foldl
                        (\( row, col ) acc2 ->
                            let
                                existingAreas : List Area
                                existingAreas =
                                    MultiDict.get row col acc2
                                        |> Maybe.withDefault []
                            in
                            MultiDict.insert row col (area :: existingAreas) acc2
                        )
                        acc
                        areaCells
                )
                Dict.empty
                (List.concatMap
                    (\puzzleArea ->
                        puzzleArea.blocks ++ puzzleArea.rows ++ puzzleArea.cols
                    )
                    puzzleAreas
                )

        cells : Dict String ( Int, Int )
        cells =
            List.foldl
                (\( startRow, startCol ) dict1 ->
                    List.foldl
                        (\row dict2 ->
                            List.foldl
                                (\col dict3 ->
                                    MultiDict.insert row col ( row, col ) dict3
                                )
                                dict2
                                (List.range startCol (startCol + boardSize - 1))
                        )
                        dict1
                        (List.range startRow (startRow + boardSize - 1))
                )
                Dict.empty
                positions
                |> Dict.values
                |> List.concatMap Dict.values
                |> List.foldl
                    (\cell acc ->
                        Dict.insert
                            (positionToKey cell)
                            cell
                            acc
                    )
                    Dict.empty

        allNumbersForSize : Set Int
        allNumbersForSize =
            allNumbers args.blockSize

        initialPossibilities : Dict String ( ( Int, Int ), (Set Int) )
        initialPossibilities =
            Dict.foldl
                (\cellKey cell map ->
                    Dict.insert
                        cellKey
                        ( cell, allNumbersForSize )
                        map
                )
                Dict.empty
                cells

        peerMap : Dict String (Set String)
        peerMap =
            Dict.foldl
                (\cellKey cell map ->
                    Dict.insert cellKey (getPeers cellAreas cell) map
                )
                Dict.empty
                cells

        solutionResult : Result String (MultiDict Int Int)
        solutionResult =
            tryPlacingNumbers
                { blockSize = args.blockSize
                , peerMap = peerMap
                , placed = Dict.empty
                , possibilities = initialPossibilities
                , seed = args.seed
                }
    in
    Result.map
        (\solution ->
            { solution = solution
            , current = MultiDict.map Given solution
            , blockSize = args.blockSize
            , puzzleAreas = puzzleAreas
            }
        )
        solutionResult


getPeers : MultiDict Int (List Area) -> ( Int, Int ) -> Set String
getPeers cellAreas ( row, col ) =
    MultiDict.get row col cellAreas
        |> Maybe.withDefault []
        |> List.concatMap getAreaCells
        |> List.map positionToKey
        |> Set.fromList
        |> Set.remove (positionToKey ( row, col ))


positionToKey : ( Int, Int ) -> String
positionToKey ( row, col ) =
    String.fromInt row ++ "," ++ String.fromInt col


positionBoards : Int -> Int -> Int -> List ( Int, Int )
positionBoards blockSize overlap numberOfBoards =
    let
        boardSize : Int
        boardSize =
            blockSize * blockSize

        stepSize : Int
        stepSize =
            boardSize - overlap

        -- 1. HELPER: Calculate the number of valid checkerboard
        --    spots in a grid of a given side length.
        --    N=1 -> 1 spot
        --    N=2 -> 2 spots
        --    N=3 -> 5 spots
        --    N=4 -> 8 spots
        spotsInGrid : Int -> Int
        spotsInGrid side =
            (side * side)
                |> toFloat
                |> (\n -> n / 2.0)
                |> ceiling

        -- 2. HELPER: Find the smallest grid side length N
        --    that can hold numberOfBoards.
        findSideLength : Int -> Int
        findSideLength currentSide =
            if spotsInGrid currentSide >= numberOfBoards then
                currentSide

            else
                findSideLength (currentSide + 1)

        -- 3. Calculate the side length we need to search.
        --    e.g., n=3 -> 3. e.g., n=5 -> 3.
        searchGridSideLength : Int
        searchGridSideLength =
            findSideLength 1

        -- 4. Create a list of all coordinates in that grid
        allGridCoords : List ( Int, Int )
        allGridCoords =
            List.range 0 (searchGridSideLength - 1)
                |> List.concatMap
                    (\gridRow ->
                        List.map
                            (Tuple.pair gridRow)
                            (List.range 0 (searchGridSideLength - 1))
                   )

        -- 5. A filter that only keeps "checkerboard" positions
        isCornerOverlap : ( Int, Int ) -> Bool
        isCornerOverlap ( gridRow, gridCol ) =
            modBy 2 (gridRow + gridCol) == 0

        mapToPosition : ( Int, Int ) -> ( Int, Int )
        mapToPosition ( gridRow, gridCol ) =
            ( gridRow * stepSize + 1, gridCol * stepSize + 1 )

    in
    allGridCoords
        |> List.filter isCornerOverlap
        |> List.take numberOfBoards
        |> List.map mapToPosition


buildPuzzleAreas : Int -> Int -> Int -> PuzzleAreas
buildPuzzleAreas blockSize startRow startCol =
    { blocks =
        List.range 0 (blockSize - 1)
            |> List.concatMap
                (\blockRowOffset ->
                    List.range 0 (blockSize - 1)
                        |> List.map
                            (\blockColOffset ->
                                { startRow = startRow + blockRowOffset * blockSize
                                , startCol = startCol + blockColOffset * blockSize
                                , endRow = startRow + (blockRowOffset + 1) * blockSize - 1
                                , endCol = startCol + (blockColOffset + 1) * blockSize - 1
                                }
                            )
                )
    , rows =
        List.range startRow (blockSize * blockSize + startRow - 1)
            |> List.map
                (\row ->
                    { startRow = row
                    , startCol = startCol
                    , endRow = row
                    , endCol = startCol + blockSize * blockSize - 1
                    }
                )
    , cols =
        List.range startCol (blockSize * blockSize + startCol - 1)
            |> List.map
                (\col ->
                    { startRow = startRow
                    , startCol = col
                    , endRow = startRow + blockSize * blockSize - 1
                    , endCol = col
                    }
                )
    }


type alias PlaceNumberArgs =
    { blockSize : Int
    , peerMap : Dict String (Set String)
    , placed : MultiDict Int Int
    , possibilities : Possibilities
    , seed : Random.Seed
    }


type alias Possibilities =
    Dict String ( ( Int, Int ), Set Int )


tryPlacingNumbers : PlaceNumberArgs -> Result String (MultiDict Int Int)
tryPlacingNumbers args =
    case findBestCell args.possibilities of
        Nothing ->
            Ok args.placed

        Just cell ->
            let
                ( shuffledNumbers, nextSeed ) =
                    Random.step (Random.List.shuffle cell.numbers) args.seed

                peers : Set String
                peers =
                    Dict.get cell.key args.peerMap
                        |> Maybe.withDefault Set.empty
            in
            List.foldl
                (\number acc ->
                    case acc of
                        Ok placedDict ->
                            Ok placedDict

                        Err _ ->
                            let
                                possibilitiesWithoutSelf : Possibilities
                                possibilitiesWithoutSelf =
                                    Dict.remove cell.key args.possibilities
                            in
                            case propagateConstraint peers number possibilitiesWithoutSelf of
                                Nothing ->
                                    Err "Branch failed"

                                Just propagatedPossibilities ->
                                    tryPlacingNumbers
                                        { args
                                            | placed = MultiDict.insert cell.row cell.col number args.placed
                                            , possibilities = propagatedPossibilities
                                            , seed = nextSeed
                                        }
                )
                (Err "No solution found in this branch")
                shuffledNumbers


findBestCell : Possibilities -> Maybe { key : String, row : Int, col : Int, numbers : List Int }
findBestCell possibilities =
    Dict.foldl
        (\cellKey ( ( row, col ), numbers ) currentBest ->
            let
                setSize : Int
                setSize =
                    Set.size numbers
            in
            case currentBest of
                Nothing ->
                    Just
                        { key = cellKey
                        , row = row
                        , col = col
                        , numbers = numbers
                        , bestSize = setSize
                        }

                Just best ->
                    if setSize < best.bestSize then
                        Just
                            { key = cellKey
                            , row = row
                            , col = col
                            , numbers = numbers
                            , bestSize = setSize
                            }

                    else
                        currentBest
        )
        Nothing
        possibilities
        |> Maybe.map
            (\record ->
                { key = record.key
                , row = record.row
                , col = record.col
                , numbers = Set.toList record.numbers
                }
            )


propagateConstraint : Set String -> Int -> Possibilities -> Maybe Possibilities
propagateConstraint peers number possibilities =
    Set.foldl
        (\peerKey currentPossibilities ->
            currentPossibilities
                |> Maybe.andThen
                    (\map ->
                        case Dict.get peerKey map of
                            Nothing ->
                                Just map

                            Just ( cellKey, oldSet ) ->
                                let
                                    newSet : Set Int
                                    newSet =
                                        Set.remove number oldSet
                                in
                                if Set.isEmpty newSet then
                                    Nothing

                                else
                                    Just (Dict.insert peerKey ( cellKey, newSet ) map)
                    )
        )
        (Just possibilities)
        peers


allNumbers : Int -> Set Int
allNumbers blockSize =
    List.range 1 (blockSize * blockSize)
        |> Set.fromList


getCell : MultiDict Int CellValue -> Int -> Int -> CellValue
getCell board row col =
    MultiDict.get row col board
        |> Maybe.withDefault Empty


cellValueToInt : CellValue -> Maybe Int
cellValueToInt cellValue =
    case cellValue of
        Given v ->
            Just v

        UserInput v ->
            Just v

        Empty ->
            Nothing


getAreaValues : Area -> MultiDict Int comparable -> Set comparable
getAreaValues area dict =
    let
        rows : List Int
        rows =
            List.range area.startRow area.endRow

        cols : List Int
        cols =
            List.range area.startCol area.endCol
    in
    List.foldl
        (\row rowSet ->
            case Dict.get row dict of
                Just rowDict ->
                    List.foldl
                        (\col colSet ->
                            case Dict.get col rowDict of
                                Just val ->
                                    Set.insert val colSet

                                Nothing ->
                                    colSet
                        )
                        rowSet
                        cols

                Nothing ->
                    rowSet
        )
        Set.empty
        rows


getAreasAt : Int -> Int -> (PuzzleAreas -> List Area) -> List PuzzleAreas -> List Area
getAreasAt row col areaFun puzzleAreas =
    List.concatMap
        (\puzzleArea ->
            List.filter
                (\area ->
                    row >= area.startRow
                        && row <= area.endRow
                        && col >= area.startCol
                        && col <= area.endCol
                )
                (areaFun puzzleArea)
        )
        puzzleAreas


getAllAreasAt : Int -> Int -> List PuzzleAreas -> List Area
getAllAreasAt row col puzzleAreas =
    List.concatMap
        (\areaFun ->
            getAreasAt row col areaFun puzzleAreas
        )
        [ .blocks, .rows, .cols ]


getAreaCells : Area -> List ( Int, Int )
getAreaCells area =
    List.concatMap
        (\row ->
            List.map
                (Tuple.pair row)
                (List.range area.startCol area.endCol)
        )
        (List.range area.startRow area.endRow)


getCellText : Int -> CellValue -> String
getCellText blockSize cellValue =
    let
        maybeInt : Maybe Int
        maybeInt =
            cellValueToInt cellValue
    in
    case blockSize of
        2 ->
            maybeInt
                |> Maybe.map String.fromInt
                |> Maybe.withDefault " "

        3 ->
            maybeInt
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""

        4 ->
            case maybeInt of
                Just v ->
                    if v <= 9 then
                        String.fromInt (v - 1)

                    else
                        Char.fromCode (v - 10 + Char.toCode 'A')
                            |> String.fromChar

                Nothing ->
                    ""

        5 ->
            case maybeInt of
                Just v ->
                    Char.fromCode (v + Char.toCode 'A')
                        |> String.fromChar

                Nothing ->
                    ""

        _ ->
            ""
