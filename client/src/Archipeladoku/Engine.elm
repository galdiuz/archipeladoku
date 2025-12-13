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
    , boards : List Area
    , rows : List Area
    , cols : List Area
    }


type alias GenerateArgs =
    { blockSize : Int
    , difficulty : Int
    , numberOfBoards : Int
    , seed : Int
    }


type alias PeerMap =
    Dict ( Int, Int ) (Set ( Int, Int ))


type alias Possibilities =
    Dict ( Int, Int ) (Set Int)


type BoardGenerationState
    = PlacingNumbers ClusterGenerationState
    | RemovingGivens ClusterGenerationState
    | RestoringGivens ClusterGenerationState
    | Failed String
    | Completed Board


type alias ClusterGenerationState =
    { allCells : Set ( Int, Int )
    , allClusters : List (List ( Int, Int ))
    , allNumbers : Set Int
    , blockSize : Int
    , blockUnlockOrder : List ( Int, Int )
    , cellAreasMap : Dict ( Int, Int ) (List Area)
    , cellsToRemoveGivensFrom : Set ( Int, Int )
    , cellsToRestoreGivensTo : Set ( Int, Int )
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
            positionBoards args.blockSize args.numberOfBoards

        puzzleAreas : List PuzzleAreas
        puzzleAreas =
            List.map
                (\( row, col ) ->
                    buildPuzzleAreas args.blockSize row col
                )
                positions

        cellAreas : Dict ( Int, Int ) (List Area)
        cellAreas =
            getAreasFromPuzzleAreas
                puzzleAreas
                (\puzzleArea ->
                    puzzleArea.blocks ++ puzzleArea.rows ++ puzzleArea.cols
                )
                |> buildCellAreasMap

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

        unlockedBlocks : Int
        unlockedBlocks =
            getInitialUnlockedBlocksCount args.blockSize

        ( clusters, groupSeed ) =
            groupPositions maxClusterSize positions ( [], Random.initialSeed args.seed )

        joinedPuzzleAreas : PuzzleAreas
        joinedPuzzleAreas =
            joinPuzzleAreas puzzleAreas

        ( blockUnlockOrder, newSeed ) =
            buildBlockUnlockOrder
                unlockedBlocks
                args.blockSize
                joinedPuzzleAreas.blocks
                clusters
                groupSeed
    in
    PlacingNumbers
        { allCells = cells
        , allClusters = clusters
        , allNumbers = allNumbersForSize args.blockSize
        , blockSize = args.blockSize
        , blockUnlockOrder = blockUnlockOrder
        , cellAreasMap = cellAreas
        , cellsToRemoveGivensFrom = cells
        , cellsToRestoreGivensTo = cells
        , givens = Dict.empty
        , peerMap = peerMap
        , puzzleAreas = joinedPuzzleAreas
        , remainingClusters = clusters
        , seed = newSeed
        , solution = Dict.empty
        , unlockCount = unlockedBlocks
        }


type alias Args2 =
    { blockSize : Int
    , blockUnlockOrder : List ( Int, Int )
    , clusters : List (List ( Int, Int ))
    , seed : Int
    , unlockedBlocks : Int
    }


generateFromServer : Args2 -> BoardGenerationState
generateFromServer args =
    let
        positions : List ( Int, Int )
        positions =
            List.concat args.clusters

        puzzleAreas : List PuzzleAreas
        puzzleAreas =
            List.map
                (\( row, col ) ->
                    buildPuzzleAreas args.blockSize row col
                )
                positions

        cellAreas : Dict ( Int, Int ) (List Area)
        cellAreas =
            getAreasFromPuzzleAreas
                puzzleAreas
                (\puzzleArea ->
                    puzzleArea.blocks ++ puzzleArea.rows ++ puzzleArea.cols
                )
                |> buildCellAreasMap

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

        joinedPuzzleAreas : PuzzleAreas
        joinedPuzzleAreas =
            joinPuzzleAreas puzzleAreas

        peerMap : PeerMap
        peerMap =
            Set.foldl
                (\cell acc ->
                    Dict.insert cell (getPeers cellAreas cell) acc
                )
                Dict.empty
                cells
    in
    PlacingNumbers
        { allCells = cells
        , allClusters = args.clusters
        , allNumbers = allNumbersForSize args.blockSize
        , blockSize = args.blockSize
        , blockUnlockOrder = args.blockUnlockOrder
        , cellAreasMap = cellAreas
        , cellsToRemoveGivensFrom = cells
        , cellsToRestoreGivensTo = cells
        , givens = Dict.empty
        , peerMap = peerMap
        , puzzleAreas = joinedPuzzleAreas
        , remainingClusters = args.clusters
        , seed = Random.initialSeed args.seed
        , solution = Dict.empty
        , unlockCount = args.unlockedBlocks
        }


getAreasFromPuzzleAreas : List PuzzleAreas -> (PuzzleAreas -> List Area) -> List Area
getAreasFromPuzzleAreas puzzleAreas mapFun =
    List.concatMap
        mapFun
        puzzleAreas


buildCellAreasMap : List Area -> Dict ( Int, Int ) (List Area)
buildCellAreasMap areas =
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
        areas


positionBoards : Int -> Int -> List ( Int, Int )
positionBoards blockSize numberOfBoards =
    let
        ( overlapRows, overlapCols ) =
            blockSizeToOverlap blockSize

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

        mapToCell : ( Int, Int ) -> ( Int, Int )
        mapToCell ( gridRow, gridCol ) =
            ( gridRow * (blockSize - overlapRows) + 1
            , gridCol * (blockSize - overlapCols) + 1
            )

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
        ( blockRows, blockCols ) =
            blockSizeToDimensions blockSize
    in
    { blocks =
        List.range 0 (blockCols - 1)
            |> List.concatMap
                (\row ->
                    List.range 0 (blockRows - 1)
                        |> List.map
                            (\col ->
                                { startRow = startRow + row * blockRows
                                , startCol = startCol + col * blockCols
                                , endRow = startRow + (row + 1) * blockRows - 1
                                , endCol = startCol + (col + 1) * blockCols - 1
                                }
                            )
                )
    , boards =
        { startRow = startRow
        , startCol = startCol
        , endRow = startRow + blockSize - 1
        , endCol = startCol + blockSize - 1
        }
            |> List.singleton
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


getInitialUnlockedBlocksCount : Int -> Int
getInitialUnlockedBlocksCount blockSize =
    let
        ( blockRows, blockCols ) =
            blockSizeToDimensions blockSize

        ( overlapRows, overlapCols ) =
            blockSizeToOverlap blockSize
    in
    if modBy blockRows overlapRows == 0 && modBy blockCols overlapCols == 0 then
        blockSize * 2 - 1

    else
        blockSize * 2


groupPositions :
    Int
    -> List ( Int, Int )
    -> ( List (List ( Int, Int )), Random.Seed )
    -> ( List (List ( Int, Int )), Random.Seed )
groupPositions maxClusterSize remaining ( clusters, currentSeed ) =
    -- TODO: Implement proper grouping logic. Must consider proximity of positions.
    ( List.map List.singleton remaining, currentSeed )


buildBlockUnlockOrder :
    Int
    -> Int
    -> List Area
    -> List (List ( Int, Int ))
    -> Random.Seed
    -> ( List ( Int, Int ), Random.Seed )
buildBlockUnlockOrder unlocked blockSize blockAreas clusterPositions seed =
    let
        ( blockRows, blockCols ) =
            blockSizeToDimensions blockSize

        allBlocks : Set ( Int, Int )
        allBlocks =
            List.map
                (\area ->
                    ( area.startRow, area.startCol )
                )
                blockAreas
                |> Set.fromList

        fillers : Set ( Int, Int )
        fillers =
            List.range 1 (unlocked - 1)
                |> List.map
                    (\i ->
                        ( -i, -i )
                    )
                |> Set.fromList

        blocksFromPosition : ( Int, Int ) -> Set ( Int, Int )
        blocksFromPosition ( row, col ) =
            List.range 0 (blockCols - 1)
                |> List.concatMap
                    (\blockRow ->
                        List.range 0 (blockRows - 1)
                            |> List.map
                                (\blockCol ->
                                    ( row + blockRow * blockRows
                                    , col + blockCol * blockCols
                                    )
                                )
                    )
                |> Set.fromList

        clusters : Dict Int { id : Int, blocks : Set ( Int, Int ), remaining : Int }
        clusters =
            List.foldl
                (\positions acc ->
                    let
                        blocksInCluster : Set ( Int, Int )
                        blocksInCluster =
                            List.foldl
                                (\position set ->
                                    Set.union (blocksFromPosition position) set
                                )
                                Set.empty
                                positions
                    in
                    { unassignedBlocks = Set.diff acc.unassignedBlocks blocksInCluster
                    , clusters =
                        (Set.intersect acc.unassignedBlocks blocksInCluster)
                            :: acc.clusters
                    }
                )
                { unassignedBlocks = allBlocks
                , clusters = []
                }
                clusterPositions
                |> .clusters
                |> List.reverse
                |> List.indexedMap
                    (\idx clusterBlocks ->
                        ( idx
                        , { id = idx
                          , blocks = clusterBlocks
                          , remaining = Set.size clusterBlocks
                          }
                        )
                    )
                |> Dict.fromList
    in
    buildBlockUnlockOrderRecursive
        { clusters = clusters
        , credits = unlocked
        , order = []
        , remainingBlocks = Set.union allBlocks fillers
        , seed = seed
        }
        |> (\state ->
            ( state.order ++ Set.toList state.remainingBlocks, state.seed )
        )


type alias BuildBlockUnlockOrderState =
    { clusters : Dict Int { id : Int, blocks : Set ( Int, Int ), remaining : Int }
    , credits : Int
    , order : List ( Int, Int )
    , remainingBlocks : Set ( Int, Int )
    , seed : Random.Seed
    }


buildBlockUnlockOrderRecursive : BuildBlockUnlockOrderState -> BuildBlockUnlockOrderState
buildBlockUnlockOrderRecursive state =
    let
        weightedClusters : List ( Float, { id : Int, blocks : Set ( Int, Int ), remaining : Int } )
        weightedClusters =
            List.map
                (\cluster ->
                    ( if cluster.remaining > state.credits then
                        0

                      else if Set.member ( 1, 1 ) cluster.blocks then
                        100000000

                      else
                        state.credits - cluster.remaining + 1
                    , cluster
                    )
                )
                (Dict.values state.clusters)
                |> List.map (Tuple.mapFirst toFloat)

        targetTuple : Maybe ( { id : Int, blocks : Set ( Int, Int ), remaining : Int }, Random.Seed )
        targetTuple =
            case weightedClusters of
                ( _, single ) :: [] ->
                    ( single, state.seed )
                        |> Just

                first :: remaining ->
                    Random.step
                        (Random.weighted first remaining)
                        state.seed
                        |> Just

                [] ->
                    Nothing
    in
    case targetTuple of
        Just ( targetCluster, seed1 ) ->
            let
                remainingCredits : Int
                remainingCredits =
                    state.credits - targetCluster.remaining

                remainingBlocksWithoutTarget : Set ( Int, Int )
                remainingBlocksWithoutTarget =
                    Set.diff state.remainingBlocks targetCluster.blocks

                targetBlocksToAddToOrder : Set ( Int, Int )
                targetBlocksToAddToOrder =
                    Set.intersect state.remainingBlocks targetCluster.blocks

                ( randomBudget, seed2 ) =
                    Random.step
                        (Random.int 0 remainingCredits)
                        seed1

                ( ( randomBlocks, remainingBlocksWithoutRandom ), seed3 ) =
                    Random.step
                        (Random.List.choices randomBudget (Set.toList remainingBlocksWithoutTarget))
                        seed2
                        |> Tuple.mapFirst (Tuple.mapSecond Set.fromList)

                ( shuffledBlocks, seed4 ) =
                    Random.step
                        (Random.List.shuffle (Set.toList targetBlocksToAddToOrder ++ randomBlocks))
                        seed3
            in
            { state
                | clusters =
                    state.clusters
                        |> Dict.remove targetCluster.id
                        |> Dict.map
                            (\_ cluster ->
                                { cluster
                                    | remaining =
                                        (Set.intersect remainingBlocksWithoutRandom cluster.blocks
                                            |> Set.size
                                        )
                                }
                            )
                , credits = remainingCredits + Set.size targetCluster.blocks - List.length randomBlocks
                , order = state.order ++ shuffledBlocks
                , remainingBlocks = remainingBlocksWithoutRandom
                , seed = seed4
            }
                |> buildBlockUnlockOrderRecursive

        Nothing ->
            state


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


continueGeneration : BoardGenerationState -> BoardGenerationState
continueGeneration state =
    case state of
        PlacingNumbers clusterState ->
            case clusterState.remainingClusters of
                [] ->
                    RemovingGivens
                        { clusterState
                            | remainingClusters =
                                sortClustersByUnlockOrder
                                    clusterState.blockUnlockOrder
                                    clusterState.allClusters
                        }

                cluster :: remainingClusters ->
                    case placeNumbersInCluster cluster clusterState of
                        Ok newClusterState ->
                            if List.isEmpty remainingClusters then
                                RemovingGivens
                                    { newClusterState
                                        | remainingClusters =
                                            sortClustersByUnlockOrder
                                                newClusterState.blockUnlockOrder
                                                newClusterState.allClusters
                                    }

                            else
                                PlacingNumbers
                                    { newClusterState
                                        | remainingClusters = remainingClusters
                                    }

                        Err errMsg ->
                            Failed errMsg

        RemovingGivens clusterState ->
            case clusterState.remainingClusters of
                [] ->
                    RestoringGivens
                        { clusterState
                            | remainingClusters = clusterState.allClusters
                        }

                cluster :: remainingClusters ->
                    let
                        newClusterState : ClusterGenerationState
                        newClusterState =
                            removeGivenNumbers
                                (getClusterCells clusterState.blockSize cluster)
                                clusterState
                    in
                    if List.isEmpty remainingClusters then
                        RestoringGivens
                            { newClusterState
                                | remainingClusters = newClusterState.allClusters
                            }

                    else
                        RemovingGivens
                            { newClusterState
                                | remainingClusters = remainingClusters
                            }

        RestoringGivens clusterState ->
            case clusterState.remainingClusters of
                [] ->
                    Completed
                        { blockSize = clusterState.blockSize
                        , givens = clusterState.givens
                        , puzzleAreas = clusterState.puzzleAreas
                        , solution = clusterState.solution
                        , unlockOrder = clusterState.blockUnlockOrder
                        , unlockCount = clusterState.unlockCount
                        }

                cluster :: remainingClusters ->
                    let
                        newClusterState : ClusterGenerationState
                        newClusterState =
                            restoreGivenNumbers
                                (getClusterCells clusterState.blockSize cluster)
                                clusterState
                    in
                    if List.isEmpty remainingClusters then
                        Completed
                            { blockSize = newClusterState.blockSize
                            , givens = newClusterState.givens
                            , puzzleAreas = newClusterState.puzzleAreas
                            , solution = newClusterState.solution
                            , unlockOrder = newClusterState.blockUnlockOrder
                            , unlockCount = newClusterState.unlockCount
                            }

                    else
                        RestoringGivens
                            { newClusterState
                                | remainingClusters = remainingClusters
                            }

        Failed errMsg ->
            Failed errMsg

        Completed board ->
            Completed board


getClusterCells : Int -> List ( Int, Int ) -> Set ( Int, Int )
getClusterCells blockSize positions =
    List.foldl
        (\( startRow, startCol ) set1 ->
            List.foldl
                (\row set2 ->
                    List.foldl
                        (\col set3 ->
                            Set.insert ( row, col ) set3
                        )
                        set2
                        (List.range startCol (startCol + blockSize - 1))
                )
                set1
                (List.range startRow (startRow + blockSize - 1))
        )
        Set.empty
        positions


placeNumbersInCluster :
    List ( Int, Int )
    -> ClusterGenerationState
    -> Result String ClusterGenerationState
placeNumbersInCluster clusterPositions state =
    let
        clusterCells : Set ( Int, Int )
        clusterCells =
            getClusterCells
                state.blockSize
                clusterPositions

        allPossibilities : Possibilities
        allPossibilities =
            Set.foldl
                (\cell acc ->
                    Dict.insert cell state.allNumbers acc
                )
                Dict.empty
                clusterCells
    in
    Set.foldl
        (\cell result ->
            Result.andThen
                (propagateSolution
                    state.solution
                    state.peerMap
                    cell
                )
                result
        )
        (Ok allPossibilities)
        state.allCells
        |> Result.andThen
            (\initialPossibilities ->
                tryPlacingNumbers
                    { blockSize = state.blockSize
                    , peerMap = state.peerMap
                    , placed = Dict.empty
                    , possibilities = initialPossibilities
                    , seed = state.seed
                    }
            )
        |> Result.map
            (\placedNumbers ->
                { state
                    | givens =
                        Dict.foldl
                            (\cell value acc ->
                                if Dict.member cell state.solution then
                                    acc

                                else
                                    Dict.insert cell value acc
                            )
                            state.givens
                            placedNumbers.solution
                    , seed = placedNumbers.seed
                    , solution = Dict.union state.solution placedNumbers.solution
                }
            )


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


getPeers : Dict ( Int, Int ) (List Area) -> ( Int, Int ) -> Set ( Int, Int )
getPeers cellAreas cell =
    Dict.get cell cellAreas
        |> Maybe.withDefault []
        |> List.concatMap getAreaCells
        |> Set.fromList
        |> Set.remove cell


blockSizeToDimensions : Int -> ( Int, Int )
blockSizeToDimensions blockSize =
    case blockSize of
        4 ->
            ( 2, 2 )

        6 ->
            ( 2, 3 )

        8 ->
            ( 2, 4 )

        9 ->
            ( 3, 3 )

        12 ->
            ( 3, 4 )

        16 ->
            ( 4, 4 )

        _ ->
            ( 1, 1 )


blockSizeToOverlap : Int -> ( Int, Int )
blockSizeToOverlap blockSize =
    case blockSize of
        4 ->
            ( 1, 1 )

        6 ->
            ( 2, 2 )

        8 ->
            ( 2, 2 )

        9 ->
            ( 3, 3 )

        12 ->
            ( 3, 4 )

        16 ->
            ( 4, 4 )

        _ ->
            ( 0, 0 )


joinPuzzleAreas : List PuzzleAreas -> PuzzleAreas
joinPuzzleAreas puzzleAreas =
    { blocks =
        List.concatMap .blocks puzzleAreas
            |> List.Extra.unique
    , boards =
        List.concatMap .boards puzzleAreas
            |> List.Extra.unique
    , rows =
        List.concatMap .rows puzzleAreas
            |> List.Extra.unique
    , cols =
        List.concatMap .cols puzzleAreas
            |> List.Extra.unique
    }


removeGivenNumbers :
    Set ( Int, Int )
    -> ClusterGenerationState
    -> ClusterGenerationState
removeGivenNumbers clusterCells inputState =
    if inputState.blockSize <= 9 then
        removeGivenNumbersBacktrack clusterCells inputState

    else
        removeGivenNumbersLogical clusterCells inputState


removeGivenNumbersBacktrack :
    Set ( Int, Int )
    -> ClusterGenerationState
    -> ClusterGenerationState
removeGivenNumbersBacktrack clusterCells inputState =
    let
        allPossibilities : Possibilities
        allPossibilities =
            Set.foldl
                (\cell acc ->
                    Dict.insert cell inputState.allNumbers acc
                )
                Dict.empty
                clusterCells

        cellsToRemoveFrom : Set ( Int, Int )
        cellsToRemoveFrom =
            Set.intersect inputState.cellsToRemoveGivensFrom clusterCells

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
                        , givens =
                            givensWithoutCell
                                |> Dict.filter (\c _ -> Set.member c clusterCells)
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
        { inputState
            | cellsToRemoveGivensFrom =
                Set.diff inputState.cellsToRemoveGivensFrom clusterCells
            , seed = nextSeed
        }
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
    -> ClusterGenerationState
    -> ClusterGenerationState
removeGivenNumbersLogical clusterCells inputState =
    let
        allPossibilities : Possibilities
        allPossibilities =
            Set.foldl
                (\cell acc ->
                    Dict.insert cell inputState.allNumbers acc
                )
                Dict.empty
                clusterCells

        cellsToRemoveFrom : Set ( Int, Int )
        cellsToRemoveFrom =
            Set.intersect inputState.cellsToRemoveGivensFrom clusterCells

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
        { inputState
            | cellsToRemoveGivensFrom =
                Set.diff inputState.cellsToRemoveGivensFrom clusterCells
            , seed = nextSeed
        }
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


restoreGivenNumbers :
    Set ( Int, Int )
    -> ClusterGenerationState
    -> ClusterGenerationState
restoreGivenNumbers clusterCells inputState =
    let
        cellsToRestore : Set ( Int, Int )
        cellsToRestore =
            Set.intersect inputState.cellsToRestoreGivensTo clusterCells

        targetNumberOfGivens : Int
        targetNumberOfGivens =
            case inputState.blockSize of
                4 ->
                    0

                6 ->
                    0

                8 ->
                    0

                9 ->
                    (38 / 81) * toFloat (Set.size clusterCells)
                        |> round

                12 ->
                    0

                16 ->
                    0

                _ ->
                    0

        ( shuffledCells, nextSeed ) =
            Random.step
                (Random.List.shuffle (Set.toList cellsToRestore))
                inputState.seed
    in
    List.Extra.stoppableFoldl
        (\cell state ->
            let
                givensInCluster : Int
                givensInCluster =
                    Set.size
                        (Set.intersect
                            (Set.fromList (Dict.keys state.givens))
                            clusterCells
                        )

                canRestore : Bool
                canRestore =
                    Dict.get cell state.cellAreasMap
                        |> Maybe.withDefault []
                        |> List.any
                            (\area ->
                                let
                                    areaCells : List ( Int, Int )
                                    areaCells =
                                        getAreaCells area
                                in
                                List.all
                                    (\areaCell ->
                                        Dict.member areaCell state.givens
                                            || areaCell == cell
                                    )
                                    areaCells
                            )
                        |> not
            in
            if givensInCluster >= targetNumberOfGivens then
                List.Extra.Stop state

            else if canRestore then
                { state
                    | givens =
                        Dict.insert cell
                            (Dict.get cell state.solution
                                |> Maybe.withDefault 0
                            )
                            state.givens
                }
                    |> List.Extra.Continue

            else
                List.Extra.Continue state
        )
        { inputState
            | cellsToRestoreGivensTo =
                Set.diff inputState.cellsToRestoreGivensTo clusterCells
            , seed = nextSeed
        }
        shuffledCells


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
