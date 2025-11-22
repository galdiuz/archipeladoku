module Archipeladoku.Engine exposing (..)

import Dict exposing (Dict)
import List.Extra
import Random
import Random.List
import Set exposing (Set)


type alias Board =
    { blockSize : Int
    , cellBlocks : Dict ( Int, Int ) (List Area)
    , givens : Dict ( Int, Int ) Int
    , puzzleAreas : PuzzleAreas
    , solution : Dict ( Int, Int ) Int
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
    , seed : Int
    }


type BoardGenerationState
    = Generating ClusterGenerationState
    | Failed String
    | Completed Board


generate : GenerateArgs -> BoardGenerationState
generate args =
    if not (Set.member args.blockSize validBlockSizes) then
        Failed "Invalid block size. Valid sizes are 4, 6, 8, 9, 12, and 16."

    -- TODO: Fix
    else if args.overlap < 0 || args.overlap > args.blockSize then
        Failed "Overlap must be non-negative and less than or equal to block size."

    else if args.numberOfBoards < 1 then
        Failed "Number of boards must be at least 1."

    else
        generateWithValidArgs args


validBlockSizes : Set Int
validBlockSizes =
    Set.fromList [ 4, 6, 8, 9, 12, 16 ]


generateWithValidArgs : GenerateArgs -> BoardGenerationState
generateWithValidArgs args =
    let
        positions : List ( Int, Int )
        positions =
            positionBoards args.blockSize args.overlap args.numberOfBoards

        puzzleAreas : List PuzzleAreas
        puzzleAreas =
            List.map
                (\( row, col ) ->
                    buildPuzzleAreas args.blockSize row col
                )
                positions

        buildAreasMap : (PuzzleAreas -> List Area) -> Dict ( Int, Int ) (List Area)
        buildAreasMap mapFun =
            List.foldl
                (\area acc ->
                    let
                        areaCells : List ( Int, Int )
                        areaCells =
                            getAreaCells area
                    in
                    List.foldl
                        (\cell acc2 ->
                            let
                                existingAreas : List Area
                                existingAreas =
                                    Dict.get cell acc2
                                        |> Maybe.withDefault []
                            in
                            Dict.insert cell (area :: existingAreas) acc2
                        )
                        acc
                        areaCells
                )
                Dict.empty
                (List.concatMap
                    mapFun
                    puzzleAreas
                )

        cellAreas : Dict ( Int, Int ) (List Area)
        cellAreas =
            buildAreasMap
                (\puzzleArea ->
                    puzzleArea.blocks ++ puzzleArea.rows ++ puzzleArea.cols
                )

        blockAreasMap : Dict ( Int, Int ) (List Area)
        blockAreasMap =
            buildAreasMap .blocks

        cells : Set ( Int, Int )
        cells =
            List.foldl
                (\( startRow, startCol ) set1 ->
                    List.foldl
                        (\row set2 ->
                            List.foldl
                                (\col set3 ->
                                    Set.insert ( row, col ) set3
                                )
                                set2
                                (List.range startCol (startCol + args.blockSize - 1))
                        )
                        set1
                        (List.range startRow (startRow + args.blockSize - 1))
                )
                Set.empty
                positions

        peerMap : PeerMap
        peerMap =
            Set.foldl
                (\cell acc ->
                    Dict.insert cell (getPeers cellAreas cell) acc
                )
                Dict.empty
                cells

        generateClusterArgs : GenerateClusterArgs
        generateClusterArgs =
            { allCells = cells
            , allNumbers = allNumbersForSize args.blockSize
            , blockAreasMap = blockAreasMap
            , blockSize = args.blockSize
            , peerMap = peerMap
            , puzzleAreas = joinPuzzleAreas puzzleAreas
            , positions = positions
            }

        maxClusterSize : Int
        maxClusterSize =
            case args.blockSize of
                4 ->
                    2

                9 ->
                    2

                16 ->
                    1

                _ ->
                    1

        ( groupedPositions, newSeed ) =
            groupPositions maxClusterSize positions ( [], Random.initialSeed args.seed )
    in
    Generating
        { givens = Dict.empty
        , remainingClusters = groupedPositions
        , seed = newSeed
        , solution = Dict.empty
        , args = generateClusterArgs
        }
        |> continueGeneration


continueGeneration : BoardGenerationState -> BoardGenerationState
continueGeneration state =
    case state of
        Generating clusterState ->
            case clusterState.remainingClusters of
                [] ->
                    Completed
                        { blockSize = clusterState.args.blockSize
                        , cellBlocks = clusterState.args.blockAreasMap
                        , givens = clusterState.givens
                        , puzzleAreas = clusterState.args.puzzleAreas
                        , solution = clusterState.solution
                        }

                positionGroup :: remainingGroups ->
                    let
                        updatedClusterStateResult =
                            generateCluster clusterState.args positionGroup clusterState
                                |> Result.map
                                    (\newState ->
                                        { newState
                                            | remainingClusters = remainingGroups
                                        }
                                    )
                    in
                    case updatedClusterStateResult of
                        Ok newClusterState ->
                            if List.isEmpty remainingGroups then
                                Completed
                                    { blockSize = newClusterState.args.blockSize
                                    , cellBlocks = clusterState.args.blockAreasMap
                                    , givens = newClusterState.givens
                                    , puzzleAreas = clusterState.args.puzzleAreas
                                    , solution = newClusterState.solution
                                    }

                            else
                                Generating newClusterState

                        Err errMsg ->
                            Failed errMsg

        Failed errMsg ->
            Failed errMsg

        Completed board ->
            Completed board


groupPositions :
    Int
    -> List ( Int, Int )
    -> ( List (List ( Int, Int )), Random.Seed )
    -> ( List (List ( Int, Int )), Random.Seed )
groupPositions maxClusterSize remaining ( clusters, currentSeed ) =
    ( List.map List.singleton remaining, currentSeed )
    -- TODO: Implement proper grouping logic. Must consider proximity of positions.
    -- if List.isEmpty remaining then
    --     ( clusters, currentSeed )
    --
    -- else
    --     let
    --         ( positionsInCluster, nextSeed ) =
    --             Random.step (Random.int 1 maxClusterSize) currentSeed
    --
    --         ( cluster, nextRemaining ) =
    --             List.Extra.splitAt positionsInCluster remaining
    --     in
    --     groupPositions maxClusterSize nextRemaining ( cluster :: clusters, nextSeed )


type alias GenerateClusterArgs =
    { allCells : Set ( Int, Int )
    , allNumbers : Set Int
    , blockAreasMap : Dict ( Int, Int ) (List Area)
    , blockSize : Int
    , peerMap : PeerMap
    , puzzleAreas : PuzzleAreas
    , positions : List ( Int, Int )
    }


type alias ClusterGenerationState =
    { givens : Dict ( Int, Int ) Int
    , remainingClusters : List (List ( Int, Int ))
    , seed : Random.Seed
    , solution : Dict ( Int, Int ) Int
    , args : GenerateClusterArgs
    }


generateCluster :
    GenerateClusterArgs
    -> List ( Int, Int )
    -> ClusterGenerationState
    -> Result String ClusterGenerationState
generateCluster args positions inputState =
    let
        clusterCells : Set ( Int, Int )
        clusterCells =
            List.foldl
                (\( startRow, startCol ) set1 ->
                    List.foldl
                        (\row set2 ->
                            List.foldl
                                (\col set3 ->
                                    Set.insert ( row, col ) set3
                                )
                                set2
                                (List.range startCol (startCol + args.blockSize - 1))
                        )
                        set1
                        (List.range startRow (startRow + args.blockSize - 1))
                )
                Set.empty
                positions

        allPossibilities : Possibilities
        allPossibilities =
            Set.foldl
                (\cell acc ->
                    Dict.insert cell args.allNumbers acc
                )
                Dict.empty
                clusterCells
    in
    Set.foldl
        (\cell result ->
            Result.andThen
                (propagateSolution inputState.solution args.peerMap cell)
                result
        )
        (Ok allPossibilities)
        args.allCells
        |> Result.andThen
            (\initialPossibilities ->
                tryPlacingNumbers
                    { blockSize = args.blockSize
                    , peerMap = args.peerMap
                    , placed = Dict.empty
                    , possibilities = initialPossibilities
                    , seed = inputState.seed
                    }
            )
        |> Result.map
            (\placedNumbers ->
                removeGivenNumbers
                    args
                    clusterCells
                    (Set.filter
                        (\cell ->
                            not (Dict.member cell inputState.solution)
                        )
                        clusterCells
                    )
                    { inputState
                        | givens =
                            Dict.foldl
                                (\cell value acc ->
                                    if Dict.member cell inputState.solution then
                                        acc

                                    else
                                        Dict.insert cell value acc
                                )
                                inputState.givens
                                placedNumbers.solution
                        , seed = placedNumbers.seed
                        , solution = Dict.union inputState.solution placedNumbers.solution
                    }
            )


getPeers : Dict ( Int, Int ) (List Area) -> ( Int, Int ) -> Set ( Int, Int )
getPeers cellAreas cell =
    Dict.get cell cellAreas
        |> Maybe.withDefault []
        |> List.concatMap getAreaCells
        |> Set.fromList
        |> Set.remove cell


propagateSolution :
   Dict ( Int, Int ) Int
   -> PeerMap
   -> ( Int, Int )
   -> Possibilities
   -> Result String Possibilities
propagateSolution solution peerMap cell possibilities =
    case Dict.get cell solution of
        Just numberInSolution ->
            let
                peers : Set ( Int, Int )
                peers =
                    Dict.get cell peerMap
                        |> Maybe.withDefault Set.empty
            in
            case propagatePossibilities peers numberInSolution possibilities of
                Just propagatedPossibilities ->
                    Dict.insert cell (Set.singleton numberInSolution) propagatedPossibilities
                        |> Ok

                Nothing ->
                    Err "Unsolvable puzzle configuration"

        Nothing ->
            Ok possibilities


positionBoards : Int -> Int -> Int -> List ( Int, Int )
positionBoards blockSize overlap numberOfBoards =
    let
        stepSize : Int
        stepSize =
            blockSize - overlap

        spotsInGrid : Int -> Int
        spotsInGrid side =
            (side * side)
                |> toFloat
                |> (\n -> n / 2.0)
                |> ceiling

        findSideLength : Int -> Int
        findSideLength currentSide =
            if spotsInGrid currentSide >= numberOfBoards then
                currentSide

            else
                findSideLength (currentSide + 1)

        searchGridSideLength : Int
        searchGridSideLength =
            findSideLength 1

        allGridCoords : List ( Int, Int )
        allGridCoords =
            List.range 0 (searchGridSideLength - 1)
                |> List.concatMap
                    (\gridRow ->
                        List.map
                            (Tuple.pair gridRow)
                            (List.range 0 (searchGridSideLength - 1))
                   )

        isCornerOverlap : ( Int, Int ) -> Bool
        isCornerOverlap ( gridRow, gridCol ) =
            modBy 2 (gridRow + gridCol) == 0

        mapToCell : ( Int, Int ) -> ( Int, Int )
        mapToCell ( gridRow, gridCol ) =
            ( gridRow * stepSize + 1, gridCol * stepSize + 1 )

    in
    allGridCoords
        |> List.filter isCornerOverlap
        |> List.take numberOfBoards
        |> List.map mapToCell


buildPuzzleAreas : Int -> Int -> Int -> PuzzleAreas
buildPuzzleAreas blockSize startRow startCol =
    let
        ( blockWidth, blockHeight ) =
            blockSizeToDimensions blockSize
    in
    { blocks =
        List.range 0 (blockSize - 1)
            |> List.concatMap
                (\blockRowOffset ->
                    List.range 0 (blockSize - 1)
                        |> List.map
                            (\blockColOffset ->
                                { startRow = startRow + blockRowOffset * blockHeight
                                , startCol = startCol + blockColOffset * blockWidth
                                , endRow = startRow + (blockRowOffset + 1) * blockHeight - 1
                                , endCol = startCol + (blockColOffset + 1) * blockWidth - 1
                                }
                            )
                )
    , rows =
        List.range startRow (blockSize + startRow - 1)
            |> List.map
                (\row ->
                    { startRow = row
                    , startCol = startCol
                    , endRow = row
                    , endCol = startCol + blockSize - 1
                    }
                )
    , cols =
        List.range startCol (blockSize + startCol - 1)
            |> List.map
                (\col ->
                    { startRow = startRow
                    , startCol = col
                    , endRow = startRow + blockSize - 1
                    , endCol = col
                    }
                )
    }


blockSizeToDimensions : Int -> ( Int, Int )
blockSizeToDimensions blockSize =
    case blockSize of
        4 ->
            ( 2, 2 )

        6 ->
            ( 3, 2 )

        8 ->
            ( 4, 2 )

        9 ->
            ( 3, 3 )

        12 ->
            ( 4, 3 )

        16 ->
            ( 4, 4 )

        _ ->
            ( 1, 1 )


joinPuzzleAreas : List PuzzleAreas -> PuzzleAreas
joinPuzzleAreas puzzleAreas =
    { blocks =
        List.concatMap .blocks puzzleAreas
            |> List.Extra.unique
    , rows =
        List.concatMap .rows puzzleAreas
            |> List.Extra.unique
    , cols =
        List.concatMap .cols puzzleAreas
            |> List.Extra.unique
    }


type alias PlaceNumberArgs =
    { blockSize : Int
    , peerMap : PeerMap
    , placed : Dict ( Int, Int ) Int
    , possibilities : Possibilities
    , seed : Random.Seed
    }


type alias PeerMap =
    Dict ( Int, Int ) (Set ( Int, Int ))


type alias Possibilities =
    Dict ( Int, Int ) (Set Int)


tryPlacingNumbers : PlaceNumberArgs -> Result String { solution : Dict ( Int, Int ) Int, seed : Random.Seed }
tryPlacingNumbers args =
    case findBestCell args.possibilities of
        Nothing ->
            Ok { solution = args.placed, seed = args.seed }

        Just { cell, numbers } ->
            let
                ( shuffledNumbers, nextSeed ) =
                    Random.step (Random.List.shuffle numbers) args.seed

                peers : Set ( Int, Int )
                peers =
                    Dict.get cell args.peerMap
                        |> Maybe.withDefault Set.empty
            in
            List.foldl
                (\number acc ->
                    case acc of
                        Ok placedDict ->
                            Ok placedDict

                        Err _ ->
                            let
                                possibilitiesWithoutCell : Possibilities
                                possibilitiesWithoutCell =
                                    Dict.remove cell args.possibilities
                            in
                            case propagatePossibilities peers number possibilitiesWithoutCell of
                                Nothing ->
                                    Err "Branch failed"

                                Just propagatedPossibilities ->
                                    tryPlacingNumbers
                                        { args
                                            | placed = Dict.insert cell number args.placed
                                            , possibilities = propagatedPossibilities
                                            , seed = nextSeed
                                        }
                )
                (Err "No solution found in this branch")
                shuffledNumbers


findBestCell : Possibilities -> Maybe { cell : ( Int, Int ), numbers : List Int }
findBestCell possibilities =
    Dict.foldl
        (\cell numbers currentBest ->
            let
                setSize : Int
                setSize =
                    Set.size numbers
            in
            case currentBest of
                Nothing ->
                    Just
                        { cell = cell
                        , numbers = numbers
                        , bestSize = setSize
                        }

                Just best ->
                    if setSize < best.bestSize then
                        Just
                            { cell = cell
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
                { cell = record.cell
                , numbers = Set.toList record.numbers
                }
            )


propagatePossibilities : Set ( Int, Int ) -> Int -> Possibilities -> Maybe Possibilities
propagatePossibilities peers number possibilities =
    Set.foldl
        (\peer currentPossibilities ->
            currentPossibilities
                |> Maybe.andThen
                    (\map ->
                        case Dict.get peer map of
                            Nothing ->
                                Just map

                            Just oldSet ->
                                let
                                    newSet : Set Int
                                    newSet =
                                        Set.remove number oldSet
                                in
                                if Set.isEmpty newSet then
                                    Nothing

                                else
                                    Just (Dict.insert peer newSet map)
                    )
        )
        (Just possibilities)
        peers


removeGivenNumbers :
    GenerateClusterArgs
    -> Set ( Int, Int )
    -> Set ( Int, Int )
    -> ClusterGenerationState
    -> ClusterGenerationState
removeGivenNumbers args clusterCells cellsToRemoveFrom inputState =
    let
        allPossibilities : Possibilities
        allPossibilities =
            Set.foldl
                (\cell acc ->
                    Dict.insert cell args.allNumbers acc
                )
                Dict.empty
                clusterCells

        ( shuffledCells, nextSeed ) =
            Random.step
                (Random.List.shuffle (Set.toList cellsToRemoveFrom))
                inputState.seed
    in
    List.foldl
        (\cell state ->
            let
                givensWithoutCell : Dict ( Int, Int ) Int
                givensWithoutCell =
                    Dict.remove cell state.givens

                otherSolutionExists : Bool
                otherSolutionExists =
                    findOtherSolution
                        { allNumbers = args.allNumbers
                        , allPossibilities = allPossibilities
                        , cells = clusterCells
                        , givens = givensWithoutCell
                        , peerMap = args.peerMap
                        , removedCell = cell
                        , removedNumber =
                            Dict.get cell state.solution
                                |> Maybe.withDefault 0
                        }
            in
            if not otherSolutionExists then
                { state
                    | givens = givensWithoutCell
                }

            else
                state
        )
        { inputState | seed = nextSeed }
        shuffledCells


type alias FindOtherSolutionArgs =
    { allNumbers : Set Int
    , allPossibilities : Possibilities
    , cells : Set ( Int, Int )
    , givens : Dict ( Int, Int ) Int
    , peerMap : PeerMap
    , removedCell : ( Int, Int )
    , removedNumber : Int
    }


findOtherSolution : FindOtherSolutionArgs -> Bool
findOtherSolution { allNumbers, allPossibilities, cells, givens, peerMap, removedCell, removedNumber } =
    let
        propagatedPossibilities : Maybe Possibilities
        propagatedPossibilities =
            Set.foldl
                (\cell acc ->
                    Maybe.andThen
                        (\possMap ->
                            let
                                peers : Set ( Int, Int )
                                peers =
                                    Dict.get cell peerMap
                                        |> Maybe.withDefault Set.empty

                                validNumbers : Set Int
                                validNumbers =
                                    Set.foldl
                                        (\peer validSet ->
                                            case Dict.get peer givens of
                                                Just givenNumber ->
                                                    Set.remove givenNumber validSet

                                                Nothing ->
                                                    validSet
                                        )
                                        allNumbers
                                        peers
                            in
                            if Set.isEmpty validNumbers then
                                Nothing

                            else
                                Just (Dict.insert cell validNumbers possMap)
                        )
                        acc
                )
                (Just Dict.empty)
                (Set.diff cells (Set.fromList (Dict.keys givens)))
                |> Maybe.map
                    (\possibilities ->
                        Dict.update
                            removedCell
                            (Maybe.map (Set.remove removedNumber))
                            possibilities
                    )
    in
    case propagatedPossibilities of
        Just initialPossibilities ->
            findOtherSolutionRecursive peerMap initialPossibilities

        Nothing ->
            False


findOtherSolutionRecursive : PeerMap -> Possibilities -> Bool
findOtherSolutionRecursive peerMap possibilities =
    case findBestCell possibilities of
        Nothing ->
            True

        Just { cell, numbers } ->
            let
                peers : Set ( Int, Int )
                peers =
                    Dict.get cell peerMap
                        |> Maybe.withDefault Set.empty

                possibilitiesWithoutCell : Possibilities
                possibilitiesWithoutCell =
                    Dict.remove cell possibilities
            in
            List.foldl
                (\number foundSolution ->
                    if foundSolution then
                        True

                    else
                        case propagatePossibilities peers number possibilitiesWithoutCell of
                            Nothing ->
                                False

                            Just propagatedMap ->
                                findOtherSolutionRecursive peerMap propagatedMap
                )
                False
                numbers


allNumbersForSize : Int -> Set Int
allNumbersForSize blockSize =
    List.range 1 blockSize
        |> Set.fromList


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


getAreaCells : Area -> List ( Int, Int )
getAreaCells area =
    List.concatMap
        (\row ->
            List.map
                (Tuple.pair row)
                (List.range area.startCol area.endCol)
        )
        (List.range area.startRow area.endRow)


getAreaValues : Area -> Dict ( Int, Int ) a -> List a
getAreaValues area dict =
    getAreaCells area
        |> List.filterMap (\cell -> Dict.get cell dict)
