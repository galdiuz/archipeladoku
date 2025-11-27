module Archipeladoku.Engine exposing (..)

import Bitwise
import Dict exposing (Dict)
import List.Extra
import Order.Extra
import Random
import Random.List
import Set exposing (Set)
import Set.Extra


type alias Board =
    { blockSize : Int
    , cellBlocks : Dict ( Int, Int ) (List Area)
    , givens : Dict ( Int, Int ) Int
    , puzzleAreas : PuzzleAreas
    , solution : Dict ( Int, Int ) Int
    , unlockCount : Int
    , unlockOrder : List ( Int, Int )
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
    , unlockedBlocks : Int
    }


type alias PeerMap =
    Dict ( Int, Int ) (Set ( Int, Int ))


type alias Possibilities =
    Dict ( Int, Int ) (Set Int)


type BoardGenerationState
    = Generating ClusterGenerationState
    | Failed String
    | Completed Board


type alias ClusterGenerationState =
    { allCells : Set ( Int, Int )
    , allNumbers : Set Int
    , blockAreasMap : Dict ( Int, Int ) (List Area)
    , blockSize : Int
    , blockUnlockOrder : List ( Int, Int )
    , cellAreasMap : Dict ( Int, Int ) (List Area)
    , givens : Dict ( Int, Int ) Int
    , peerMap : PeerMap
    , puzzleAreas : PuzzleAreas
    , remainingClusters : List (List ( Int, Int ))
    , seed : Random.Seed
    , solution : Dict ( Int, Int ) Int
    , unlockCount : Int
    }


type alias Bitmask =
    Int


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

        joinedPuzzleAreas : PuzzleAreas
        joinedPuzzleAreas =
            joinPuzzleAreas puzzleAreas

        ( clusters, groupSeed ) =
            groupPositions maxClusterSize positions ( [], Random.initialSeed args.seed )

        ( blockUnlockOrder, newSeed ) =
            buildBlockUnlockOrder
                args.unlockedBlocks
                args.blockSize
                joinedPuzzleAreas.blocks
                clusters
                groupSeed
    in
    Generating
        { allCells = cells
        , allNumbers = allNumbersForSize args.blockSize
        , blockAreasMap = blockAreasMap
        , blockSize = args.blockSize
        , blockUnlockOrder = blockUnlockOrder
        , cellAreasMap = cellAreas
        , givens = Dict.empty
        , peerMap = peerMap
        , puzzleAreas = joinedPuzzleAreas
        , remainingClusters = sortClustersByUnlockOrder blockUnlockOrder clusters
        , seed = newSeed
        , solution = Dict.empty
        , unlockCount = args.unlockedBlocks
        }
        |> continueGeneration


continueGeneration : BoardGenerationState -> BoardGenerationState
continueGeneration state =
    case state of
        Generating clusterState ->
            case clusterState.remainingClusters of
                [] ->
                    Completed
                        { blockSize = clusterState.blockSize
                        , cellBlocks = clusterState.blockAreasMap
                        , givens = clusterState.givens
                        , puzzleAreas = clusterState.puzzleAreas
                        , solution = clusterState.solution
                        , unlockOrder = clusterState.blockUnlockOrder
                        , unlockCount = clusterState.unlockCount
                        }

                positionGroup :: remainingGroups ->
                    let
                        updatedClusterStateResult =
                            generateCluster positionGroup clusterState
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
                                    { blockSize = newClusterState.blockSize
                                    , cellBlocks = newClusterState.blockAreasMap
                                    , givens = newClusterState.givens
                                    , puzzleAreas = newClusterState.puzzleAreas
                                    , solution = newClusterState.solution
                                    , unlockOrder = newClusterState.blockUnlockOrder
                                    , unlockCount = newClusterState.unlockCount
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


buildBlockUnlockOrder :
    Int
    -> Int
    -> List Area
    -> List (List ( Int, Int ))
    -> Random.Seed
    -> ( List ( Int, Int ), Random.Seed )
buildBlockUnlockOrder unlocked blockSize blocks clusterPositions initialSeed =
    let
        ( blockWidth, blockHeight ) =
            blockSizeToDimensions blockSize

        initialOrder : List ( Int, Int )
        initialOrder =
            List.map
                (\area ->
                    ( area.startRow, area.startCol )
                )
                blocks

        clusters : List ( Int, Set ( Int, Int ) )
        clusters =
            List.foldl
                (\positions acc ->
                    let
                        blocksInCluster : Set ( Int, Int )
                        blocksInCluster =
                            List.foldl
                                (\( row, col ) ->
                                    Set.union
                                        (List.range 0 (blockWidth - 1)
                                            |> List.concatMap
                                                (\rowOffset ->
                                                    List.range 0 (blockHeight - 1)
                                                        |> List.map
                                                            (\colOffset ->
                                                                ( row + rowOffset * blockHeight
                                                                , col + colOffset * blockWidth
                                                                )
                                                            )
                                                )
                                            |> Set.fromList
                                        )
                                )
                                Set.empty
                                positions
                    in
                    { blocks = Set.diff acc.blocks blocksInCluster
                    , clusters =
                        (Set.intersect acc.blocks blocksInCluster)
                            :: acc.clusters
                    }
                )
                { blocks = Set.fromList initialOrder
                , clusters = []
                }
                clusterPositions
                |> .clusters
                |> List.reverse
                |> List.indexedMap
                    (\index cluster ->
                        ( index, cluster )
                    )

        blockClusterMap : Dict ( Int, Int ) (List Int)
        blockClusterMap =
            List.foldl
                (\( clusterIndex, clusterBlocks ) dictAcc ->
                    Set.foldl
                        (\block innerDict ->
                            Dict.update
                                block
                                (\maybeList ->
                                    Just (clusterIndex :: Maybe.withDefault [] maybeList)
                                )
                                innerDict
                        )
                        dictAcc
                        clusterBlocks
                )
                Dict.empty
                clusters

        orderLength : Int
        orderLength =
            List.length initialOrder

        swapAttempts : Int
        swapAttempts =
            orderLength * 100
    in
    List.foldl
        (\_ ( order, seed ) ->
            let
                ( first, stepSeed ) =
                    Random.step (Random.int unlocked (orderLength - 1)) seed

                ( second, nextSeed ) =
                    Random.step (Random.int unlocked (orderLength - 1)) stepSeed

                newOrder : List ( Int, Int )
                newOrder =
                    List.Extra.swapAt first second order
            in
            if isValidBlockOrder newOrder unlocked clusters blockClusterMap then
                ( newOrder, nextSeed )

            else
                ( order, nextSeed )
        )
        ( initialOrder, initialSeed )
        (List.range 1 swapAttempts)


isValidBlockOrder :
    List ( Int, Int )
    -> Int
    -> List ( Int, Set ( Int, Int ) )
    -> Dict ( Int, Int ) (List Int)
    -> Bool
isValidBlockOrder unlockOrder initialVisibleCount clusters blockMap =
    let
        initialCounts : Dict Int Int
        initialCounts =
            clusters
                |> List.map (\( idx, blocks ) -> ( idx, Set.size blocks ))
                |> Dict.fromList

        totalClusters : Int
        totalClusters =
            List.length clusters

        runSimulation :
            List ( Int, Int )
            -> Int
            -> Dict Int Int
            -> Dict Int Int
            -> Int
            -> Bool
        runSimulation order credits counts rewards solvedCount =
            if solvedCount == totalClusters then
                True

            else if credits <= 0 then
                False

            else
                case order of
                    [] ->
                        False

                    currentBlock :: remainingOrder ->
                        let
                            affectedClusters : List Int
                            affectedClusters =
                                Dict.get currentBlock blockMap
                                    |> Maybe.withDefault []

                            ( newCounts, earnedCredits, newSolvedCount ) =
                                List.foldl
                                    (\clusterIdx ( cMap, creditAcc, sCount ) ->
                                        case Dict.get clusterIdx cMap of
                                            Just count ->
                                                if count == 1 then
                                                    let
                                                        reward : Int
                                                        reward =
                                                            Dict.get clusterIdx rewards
                                                                |> Maybe.withDefault 0
                                                    in
                                                    ( Dict.remove clusterIdx cMap
                                                    , creditAcc + reward
                                                    , sCount + 1
                                                    )

                                                else
                                                    ( Dict.insert clusterIdx (count - 1) cMap
                                                    , creditAcc
                                                    , sCount
                                                    )

                                            Nothing ->
                                                ( cMap, creditAcc, sCount )
                                    )
                                    ( counts, 0, solvedCount )
                                    affectedClusters
                        in
                        runSimulation
                            remainingOrder
                            (credits - 1 + earnedCredits)
                            newCounts
                            rewards
                            newSolvedCount
    in
    runSimulation
        unlockOrder
        initialVisibleCount
        initialCounts
        initialCounts
        0


sortClustersByUnlockOrder :
    List ( Int, Int )
    -> List (List ( Int, Int ))
    -> List (List ( Int, Int ))
sortClustersByUnlockOrder unlockOrder clusters =
    List.foldl
        (\position acc ->
            let
                newUnlockedBlocks : Set ( Int, Int )
                newUnlockedBlocks =
                    Set.insert position acc.unlockedBlocks

                ( unlockedClusters, remainingClusters ) =
                    List.partition
                        (\clusterBlocks ->
                            Set.Extra.isSubsetOf newUnlockedBlocks (Set.fromList clusterBlocks)
                        )
                        acc.remainingClusters
            in
            { unlockedBlocks = newUnlockedBlocks
            , sortedClusters = List.append acc.sortedClusters unlockedClusters
            , remainingClusters = remainingClusters
            }
        )
        { unlockedBlocks = Set.empty
        , sortedClusters = []
        , remainingClusters = clusters
        }
        unlockOrder
        |> .sortedClusters


generateCluster :
    List ( Int, Int )
    -> ClusterGenerationState
    -> Result String ClusterGenerationState
generateCluster clusterPositions inputState =
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
                                (List.range startCol (startCol + inputState.blockSize - 1))
                        )
                        set1
                        (List.range startRow (startRow + inputState.blockSize - 1))
                )
                Set.empty
                clusterPositions

        allPossibilities : Possibilities
        allPossibilities =
            Set.foldl
                (\cell acc ->
                    Dict.insert cell inputState.allNumbers acc
                )
                Dict.empty
                clusterCells
    in
    Set.foldl
        (\cell result ->
            Result.andThen
                (propagateSolution inputState.solution inputState.peerMap cell)
                result
        )
        (Ok allPossibilities)
        inputState.allCells
        |> Result.andThen
            (\initialPossibilities ->
                tryPlacingNumbers
                    { blockSize = inputState.blockSize
                    , peerMap = inputState.peerMap
                    , placed = Dict.empty
                    , possibilities = initialPossibilities
                    , seed = inputState.seed
                    }
            )
        |> Result.map
            (\placedNumbers ->
                removeGivenNumbers
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

        isCornerOverlap : ( Int, Int ) -> Bool
        isCornerOverlap ( gridRow, gridCol ) =
            modBy 2 (gridRow + gridCol) == 0

        stepSize : Int
        stepSize =
            blockSize - overlap

        mapToCell : ( Int, Int ) -> ( Int, Int )
        mapToCell ( gridRow, gridCol ) =
            ( gridRow * stepSize + 1, gridCol * stepSize + 1 )

        gridSideLength : Int
        gridSideLength =
            findSideLength 1

        allGridCoords : List ( Int, Int )
        allGridCoords =
            List.range 0 (gridSideLength - 1)
                |> List.concatMap
                    (\gridRow ->
                        List.map
                            (Tuple.pair gridRow)
                            (List.range 0 (gridSideLength - 1))
                   )
    in
    allGridCoords
        |> List.filter isCornerOverlap
        |> List.take numberOfBoards
        |> List.map mapToCell
        |> List.sortWith
            (Order.Extra.breakTies
                [ (\( row1, col1 ) ( row2, col2 ) ->
                    compare (row1 + col1) (row2 + col2)
                  )
                , (\( row1, col1 ) ( row2, col2 ) ->
                    compare (max row1 col1) (max row2 col2)
                  )
                ]
            )


buildPuzzleAreas : Int -> Int -> Int -> PuzzleAreas
buildPuzzleAreas blockSize startRow startCol =
    let
        ( blockWidth, blockHeight ) =
            blockSizeToDimensions blockSize
    in
    { blocks =
        List.range 0 (blockWidth - 1)
            |> List.concatMap
                (\blockRowOffset ->
                    List.range 0 (blockHeight - 1)
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
    Set ( Int, Int )
    -> Set ( Int, Int )
    -> ClusterGenerationState
    -> ClusterGenerationState
removeGivenNumbers clusterCells cellsToRemoveFrom inputState =
    if inputState.blockSize <= 9 then
        removeGivenNumbersBacktrack clusterCells cellsToRemoveFrom inputState

    else
        removeGivenNumbersLogical clusterCells cellsToRemoveFrom inputState


removeGivenNumbersBacktrack :
    Set ( Int, Int )
    -> Set ( Int, Int )
    -> ClusterGenerationState
    -> ClusterGenerationState
removeGivenNumbersBacktrack clusterCells cellsToRemoveFrom inputState =
    let
        allPossibilities : Possibilities
        allPossibilities =
            Set.foldl
                (\cell acc ->
                    Dict.insert cell inputState.allNumbers acc
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
                        { allNumbers = state.allNumbers
                        , allPossibilities = allPossibilities
                        , cells = clusterCells
                        , givens = givensWithoutCell
                        , peerMap = state.peerMap
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


removeGivenNumbersLogical :
    Set ( Int, Int )
    -> Set ( Int, Int )
    -> ClusterGenerationState
    -> ClusterGenerationState
removeGivenNumbersLogical clusterCells cellsToRemoveFrom inputState =
    let
        allPossibilities : Possibilities
        allPossibilities =
            Set.foldl
                (\cell acc ->
                    Dict.insert cell inputState.allNumbers acc
                )
                Dict.empty
                clusterCells

        ( shuffledCells, nextSeed ) =
            Random.step
                (Random.List.shuffle (Set.toList cellsToRemoveFrom))
                inputState.seed
    in
    List.foldl
        (\cellGroup state ->
            List.Extra.stoppableFoldl
                (\cells _ ->
                    let
                        givensWithoutCells : Dict ( Int, Int ) Int
                        givensWithoutCells =
                            List.foldl
                                (\cell dict ->
                                    Dict.remove cell dict
                                )
                                state.givens
                                cells

                        solverPossibilities : Maybe Possibilities
                        solverPossibilities =
                            Dict.foldl
                                (\cell numbers acc ->
                                    Maybe.andThen
                                        (\possMap ->
                                            let
                                                peers : Set ( Int, Int )
                                                peers =
                                                    Dict.get cell state.peerMap
                                                        |> Maybe.withDefault Set.empty
                                            in
                                            propagatePossibilities peers numbers possMap
                                                |> Maybe.map (Dict.remove cell)
                                        )
                                        acc
                                )
                                (Just allPossibilities)
                                givensWithoutCells

                        canSolve : Bool
                        canSolve =
                            case solverPossibilities of
                                Just possMap ->
                                    canSolveWithLogic
                                        { cellAreasMap = state.cellAreasMap
                                        , peerMap = state.peerMap
                                        , possibilities = possMap
                                        , removedCells = cells
                                        }

                                Nothing ->
                                    False
                    in
                    if canSolve then
                        { state
                            | givens = givensWithoutCells
                        }
                            |> List.Extra.Stop

                    else
                        List.Extra.Continue state
                )
                state
                (List.Extra.inits cellGroup
                    |> List.drop 1
                    |> List.reverse
                )

        )
        { inputState | seed = nextSeed }
        (List.Extra.groupsOf 4 shuffledCells)


type alias SolveWithLogicArgs =
    { cellAreasMap : Dict ( Int, Int ) (List Area)
    , peerMap : PeerMap
    , possibilities : Possibilities
    , removedCells : List ( Int, Int )
    }


type SolveWithLogicResult
    = Solved
    | Stuck
    | Progress Possibilities


canSolveWithLogic : SolveWithLogicArgs -> Bool
canSolveWithLogic args =
    List.Extra.stoppableFoldl
        (\solveFunc _ ->
            case solveFunc args of
                Solved ->
                    List.Extra.Stop True

                Stuck ->
                    List.Extra.Continue False

                Progress newPossibilities ->
                    canSolveWithLogic
                        { args
                            | possibilities = newPossibilities
                        }
                        |> List.Extra.Stop
        )
        False
        [ solveNakedSingles
        , solveHiddenSingles
        ]


solveNakedSingles : SolveWithLogicArgs -> SolveWithLogicResult
solveNakedSingles args =
    let
        nakedSingles : List ( ( Int, Int ), Int )
        nakedSingles =
            Dict.foldl
                (\cell options acc ->
                    if Set.size options == 1 then
                        case Set.toList options of
                            [ val ] ->
                                ( cell, val ) :: acc

                            _ ->
                                acc

                    else
                        acc
                )
                []
                args.possibilities
    in
    case nakedSingles of
        [] ->
            if Dict.isEmpty args.possibilities then
                Solved

            else
                Stuck

        singles ->
            let
                nextState : Maybe Possibilities
                nextState =
                    List.foldl
                        (\( cell, val ) accMaybe ->
                            accMaybe
                                |> Maybe.andThen
                                    (\currentPoss ->
                                        let
                                            peers =
                                                Dict.get cell args.peerMap
                                                    |> Maybe.withDefault Set.empty
                                        in
                                        propagatePossibilities peers val currentPoss
                                            |> Maybe.map (Dict.remove cell)
                                    )
                        )
                        (Just args.possibilities)
                        singles
            in
            case nextState of
                Just newPossibilities ->
                    Progress newPossibilities

                Nothing ->
                    Stuck


solveHiddenSingles : SolveWithLogicArgs -> SolveWithLogicResult
solveHiddenSingles args =
    let
        checkTargetInArea : ( Int, Int ) -> Possibilities -> List ( Int, Int ) -> Maybe Int
        checkTargetInArea targetCell possibilities areaCells =
            case Dict.get targetCell possibilities of
                Nothing ->
                    Nothing

                Just targetCandidates ->
                    let
                        othersMask : Bitmask
                        othersMask =
                            List.foldl
                                (\peer accMask ->
                                    if peer == targetCell then
                                        accMask

                                    else
                                        case Dict.get peer possibilities of
                                            Just set ->
                                                Bitwise.or accMask (numbersToMask set)

                                            Nothing ->
                                                accMask
                                )
                                0
                                areaCells

                        targetMask : Bitmask
                        targetMask =
                            numbersToMask targetCandidates

                        uniqueMask : Bitmask
                        uniqueMask =
                            Bitwise.and targetMask (Bitwise.complement othersMask)
                    in
                    if uniqueMask == 0 then
                        Nothing

                    else
                        Just (maskToFirstNumber uniqueMask)

        maskToFirstNumber : Int -> Int
        maskToFirstNumber mask =
            let
                lsb : Bitmask
                lsb =
                    Bitwise.and mask (Bitwise.complement (mask - 1))
            in
            bitToNum lsb 1

        bitToNum : Bitmask -> Int -> Int
        bitToNum bit current =
            if bit == 1 then
                current

            else
                bitToNum (Bitwise.shiftRightZfBy 1 bit) (current + 1)

        hiddenSingle : Maybe ( ( Int, Int ), Int )
        hiddenSingle =
            List.foldl
                (\target acc ->
                    case acc of
                        Just _ ->
                            acc

                        Nothing ->
                            List.foldl
                                (\area innerAcc ->
                                    case innerAcc of
                                        Just _ ->
                                            innerAcc

                                        Nothing ->
                                            case checkTargetInArea target args.possibilities area of
                                                Just number ->
                                                    Just ( target, number )

                                                Nothing ->
                                                    Nothing
                                )
                                Nothing
                                (Dict.get target args.cellAreasMap
                                    |> Maybe.withDefault []
                                    |> List.map getAreaCells
                                )
                )
                Nothing
                args.removedCells
    in
    case hiddenSingle of
        Just ( cell, number ) ->
            let
                peers : Set ( Int, Int )
                peers =
                    Dict.get cell args.peerMap
                        |> Maybe.withDefault Set.empty
            in
            case propagatePossibilities peers number args.possibilities of
                Just newPossibilities ->
                    Progress (Dict.remove cell newPossibilities)

                Nothing ->
                    Stuck

        Nothing ->
            Stuck


numbersToMask : Set Int -> Bitmask
numbersToMask numbers =
    Set.foldl
        (\number acc ->
            Bitwise.or acc (Bitwise.shiftLeftBy (number - 1) 1)
        )
        0
        numbers


maskToNumbers : Bitmask -> List Int
maskToNumbers mask =
    let
        helper : Bitmask -> Int -> List Int -> List Int
        helper m currentVal acc =
            if m == 0 then
                acc
            else
                let
                    newAcc =
                        if Bitwise.and m 1 == 1 then
                            currentVal :: acc

                        else
                            acc
                in
                helper (Bitwise.shiftRightZfBy 1 m) (currentVal + 1) newAcc
    in
    helper mask 1 []


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
