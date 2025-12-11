interface Area {
    startRow: number
    startCol: number
    endRow: number
    endCol: number
}


interface PuzzleAreas {
    boards: Area[]
    blocks: Area[]
    rows: Area[]
    cols: Area[]
}


type Cell = [number, number]


type CellIndex = number


type Peers = number[]


type PeerMap = number[][]


type PossibilitiesMap = Int32Array


interface GenerateArgs {
    blockSize: number
    numberOfBoards: number
    seed: number
}


interface BlockOrderCluster {
    id: number
    blocks: Map<number, Cell>
    remaining: number
    weight: number
}


type BoardGenerationState =
    | PlacingNumbers
    | RemovingGivens
    | RestoringGivens
    | Completed
    | Failed


interface PlacingNumbers {
    type: 'PlacingNumbers'
    state: ClusterGenerationState
}


interface RemovingGivens {
    type: 'RemovingGivens'
    state: ClusterGenerationState
}


interface RestoringGivens {
    type: 'RestoringGivens'
    state: ClusterGenerationState
}


interface Completed {
    type: 'Completed'
    blockSize: number
    givens: EncodedCellValue[]
    solution: EncodedCellValue[]
    puzzleAreas: EncodedPuzzleAreas
    unlockCount: number
    unlockOrder: Cell[]
}


type EncodedCellValue = [number, number, number]


interface EncodedPuzzleAreas {
    boards: EncodedArea[]
    blocks: EncodedArea[]
    rows: EncodedArea[]
    cols: EncodedArea[]
}


type EncodedArea = [number, number, number, number]


interface Failed {
    type: 'Failed'
    reason: string
}


interface ClusterGenerationState {
    allCells: Set<Cell>
    allCellIndices: Set<CellIndex>
    allClusters: Cell[][]
    blockSize: number
    blockUnlockOrder: Cell[]
    cellAreasMap: Area[][]
    cellIndicesToRemoveGivensFrom: Set<CellIndex>
    cellIndicesToRestoreGivensFrom: Set<CellIndex>
    givens: Int32Array
    puzzleAreas: PuzzleAreas
    peerMap: PeerMap
    random: () => number
    remainingClusters: Cell[][]
    solution: Int32Array
    solutionHistory: SolutionHistory[]
}


interface SolutionHistory {
    solution: Int32Array
    cluster: Cell[]
}


const maxWidth: number = 250
const totalArraySize: number = maxWidth * maxWidth


export function initGeneration(args: GenerateArgs): BoardGenerationState {
    if (args.numberOfBoards < 1 || args.numberOfBoards > 100) {
        return {
            type: 'Failed',
            reason: 'Number of boards must be between 1 and 100',
        }
    }

    if (![4, 6, 8, 9, 12, 16].includes(args.blockSize)) {
        return {
            type: 'Failed',
            reason: 'Unsupported block size',
        }
    }

    const positions: Cell[] = positionBoards(args.blockSize, args.numberOfBoards)

    const boardPuzzleAreas: PuzzleAreas[] = []
    for (const [startRow, startCol] of positions) {
        const puzzleAreas = buildPuzzleAreas(args.blockSize, startRow, startCol)
        boardPuzzleAreas.push(puzzleAreas)
    }
    const puzzleAreas: PuzzleAreas = joinPuzzleAreas(boardPuzzleAreas)
    const areas: Area[] = [...puzzleAreas.blocks, ...puzzleAreas.rows, ...puzzleAreas.cols]
    const cellAreasMap: Area[][] = buildCellAreasMap(areas)
    const cells: Set<Cell> = new Set()
    const cellIndices: Set<CellIndex> = new Set()

    for (const board of puzzleAreas.boards) {
        const boardCells: Cell[] = getCellsInArea(board)
        for (const [row, col] of boardCells) {
            const cellIndex: CellIndex = getCellIndex(row, col)
            cells.add([row, col])
            cellIndices.add(cellIndex)
        }
    }

    const random: () => number = createRandomGenerator(args.seed)
    const peerMap: PeerMap = buildPeerMap(cells, cellAreasMap)
    const clusters: Cell[][] = buildClusters(positions, random)
    const blockUnlockOrder: Cell[] = buildBlockUnlockOrder(
        args.blockSize,
        17, // TODO: adjust for other block sizes
        puzzleAreas.blocks,
        clusters,
        random
    )

    return {
        type: 'PlacingNumbers',
        state: {
            allCells: cells,
            allCellIndices: cellIndices,
            allClusters: clusters,
            blockSize: args.blockSize,
            blockUnlockOrder: blockUnlockOrder,
            cellAreasMap: cellAreasMap,
            cellIndicesToRemoveGivensFrom: new Set(cellIndices),
            cellIndicesToRestoreGivensFrom: new Set(cellIndices),
            givens: new Int32Array(totalArraySize),
            puzzleAreas: puzzleAreas,
            peerMap: peerMap,
            random: random,
            remainingClusters: clusters.slice(),
            solution: new Int32Array(totalArraySize),
            solutionHistory: [],
        },
    }
}


function positionBoards(blockSize: number, numberOfBoards: number): Cell[] {
    const [overlapRows, overlapCols] = blockSizeToOverlap(blockSize)

    const spotsInGrid = function(side: number): number {
        return Math.ceil(side * side / 2)
    }

    const findSideLength = function(side: number): number {
        if (spotsInGrid(side) >= numberOfBoards) {
            return side
        } else {
            return findSideLength(side + 1)
        }
    }

    const isCornerOverlap = function(row: number, col: number, side: number): boolean {
        return (row + col) % 2 == 0
    }

    const mapToCell = function(row: number, col: number): Cell {
        return [
            row * (blockSize - overlapRows) + 1,
            col * (blockSize - overlapCols) + 1,
        ]
    }

    const gridSideLength = findSideLength(1)

    let gridPositions: Cell[] = []
    for (let r = 0; r < gridSideLength; r++) {
        for (let c = 0; c < gridSideLength; c++) {
            if (isCornerOverlap(r, c, gridSideLength)) {
                gridPositions.push(mapToCell(r, c))
            }
        }
    }

    gridPositions = gridPositions.slice(0, numberOfBoards)

    return gridPositions
}


function buildPuzzleAreas(blockSize: number, startRow: number, startCol: number): PuzzleAreas {
    const [blockRows, blockCols] = blockSizeToDimensions(blockSize)

    const blocks: Area[] = []
    for (let r = 0; r < blockRows; r++) {
        for (let c = 0; c < blockCols; c++) {
            blocks.push({
                startRow: startRow + r * blockRows,
                startCol: startCol + c * blockCols,
                endRow: startRow + (r + 1) * blockRows - 1,
                endCol: startCol + (c + 1) * blockCols - 1,
            })
        }
    }

    const rows: Area[] = []
    for (let r = 0; r < blockRows * blockCols; r++) {
        rows.push({
            startRow: startRow + r,
            startCol: startCol,
            endRow: startRow + r,
            endCol: startCol + blockRows * blockCols - 1,
        })
    }

    const cols: Area[] = []
    for (let c = 0; c < blockRows * blockCols; c++) {
        cols.push({
            startRow: startRow,
            startCol: startCol + c,
            endRow: startRow + blockRows * blockCols - 1,
            endCol: startCol + c,
        })
    }

    return {
        boards: [{
            startRow: startRow,
            startCol: startCol,
            endRow: startRow + blockRows * blockCols - 1,
            endCol: startCol + blockRows * blockCols - 1,
        }],
        blocks: blocks,
        rows: rows,
        cols: cols,
    }
}


function joinPuzzleAreas(puzzleAreasList: PuzzleAreas[]): PuzzleAreas {
    const boards: Area[] = []
    const blocks: Area[] = []
    const rows: Area[] = []
    const cols: Area[] = []

    for (const puzzleAreas of puzzleAreasList) {
        boards.push(...puzzleAreas.boards)
        blocks.push(...puzzleAreas.blocks)
        rows.push(...puzzleAreas.rows)
        cols.push(...puzzleAreas.cols)
    }

    return {
        boards: boards,
        blocks: blocks,
        rows: rows,
        cols: cols,
    }
}


function buildCellAreasMap(areas: Area[]): Area[][] {
    const cellAreasMap: Area[][] = Array.from({ length: totalArraySize }, () => [])

    for (const area of areas) {
        const cellIndices: CellIndex[] = getCellIndicesInArea(area)
        for (const cellIndex of cellIndices) {
            cellAreasMap[cellIndex]!.push(area)
        }
    }

    return cellAreasMap
}


function getCellsInArea(area: Area): Cell[] {
    const cells: Cell[] = []
    for (let r = area.startRow; r <= area.endRow; r++) {
        for (let c = area.startCol; c <= area.endCol; c++) {
            cells.push([r, c])
        }
    }

    return cells
}


function getCellIndicesInArea(area: Area): CellIndex[] {
    const cellIndices: CellIndex[] = []
    for (let r = area.startRow; r <= area.endRow; r++) {
        for (let c = area.startCol; c <= area.endCol; c++) {
            const cellIndex: CellIndex = getCellIndex(r, c)
            cellIndices.push(cellIndex)
        }
    }

    return cellIndices
}


function buildPeerMap(cells: Set<Cell>, cellAreasMap: Area[][]): PeerMap {
    const peerMap: PeerMap = Array.from({ length: totalArraySize }, () => [])

    for (const [row, col] of cells) {
        const cellIndex: CellIndex = getCellIndex(row, col)
        const areas: Area[] = cellAreasMap[cellIndex]!
        const peerSet: Set<number> = new Set()

        for (const area of areas) {
            const areaIndices: CellIndex[] = getCellIndicesInArea(area)
            for (const areaIndex of areaIndices) {
                if (areaIndex !== cellIndex) {
                    peerSet.add(areaIndex)
                }
            }
        }

        peerMap[cellIndex] = Array.from(peerSet)
    }

    return peerMap
}


function buildClusters(positions: Cell[], random: () => number): Cell[][] {
    // Placeholder for cluster building logic
    return positions.map(pos => [pos])
}


function buildBlockUnlockOrder(
    blockSize: number,
    initialUnlocked: number,
    allBlocks: Area[],
    clusters: Cell[][],
    random: () => number,
): Cell[] {
    const blocksToAssign: Map<number, Cell> = new Map()
    const remainingBlocks: Map<number, Cell> = new Map()
    const blockOrderClusters: Map<number, BlockOrderCluster> = new Map()
    const fillerCount = initialUnlocked
    let fillersAdded = false

    for (const block of allBlocks) {
        const key = getCellIndex(block.startRow, block.startCol)
        blocksToAssign.set(key, [block.startRow, block.startCol])
        remainingBlocks.set(key, [block.startRow, block.startCol])
    }

    for (let i = 0; i < clusters.length; i++) {
        const cluster = clusters[i]!
        const blockOrderCluster: BlockOrderCluster = {
            id: i,
            blocks: new Map<number, Cell>(),
            remaining: 0,
            weight: 0,
        }
        blockOrderClusters.set(i, blockOrderCluster)

        for (const [startRow, startCol] of cluster) {
            let boardBlocks = buildPuzzleAreas(blockSize, startRow, startCol).blocks

            for (const block of boardBlocks) {
                const blockKey = getCellIndex(block.startRow, block.startCol)
                if (!blocksToAssign.has(blockKey)) {
                    continue
                }

                blockOrderCluster.blocks.set(blockKey, [block.startRow, block.startCol])
                blockOrderCluster.remaining += 1
                blocksToAssign.delete(blockKey)
            }
        }
    }

    let credits: number = initialUnlocked
    const unlockOrder: Cell[] = []

    while (blockOrderClusters.size > 0) {
        for (const cluster of blockOrderClusters.values()) {
            if (cluster.remaining > credits) {
                cluster.weight = 0
            } else if (cluster.id === 0) {
                cluster.weight = 1000000000
            } else {
                cluster.weight = credits - cluster.remaining + 1
            }
        }

        const blockOrderClustersArray = Array.from(blockOrderClusters.values())
        const targetCluster = pickCluster(blockOrderClustersArray, random)
        const targetBlocks = intersectMaps(targetCluster.blocks, remainingBlocks)
        const remainingBlocksExcludingTarget = diffMaps(remainingBlocks, targetCluster.blocks)
        const remainingCredits = credits - targetCluster.remaining
        const randomBudgetMax = Math.min(remainingCredits, remainingBlocksExcludingTarget.size)
        const randomBudget = Math.floor(random() * randomBudgetMax)
        const randomBlocks = randomSample(remainingBlocksExcludingTarget, randomBudget, random)

        const blocksToAdd: Cell[] = Array.from(targetBlocks.values())
            .concat(Array.from(randomBlocks.values()))

        for (let i = 0; i < blocksToAdd.length; i++) {
            const j = i + Math.floor(random() * (blocksToAdd.length - i))
            ;[blocksToAdd[i], blocksToAdd[j]] = [blocksToAdd[j]!, blocksToAdd[i]!]
        }

        for (const block of blocksToAdd) {
            unlockOrder.push(block)
            const blockKey = block[0] > 0 ? getCellIndex(block[0], block[1]) : block[0]
            remainingBlocks.delete(blockKey)
        }

        blockOrderClusters.delete(targetCluster.id)
        credits = remainingCredits + targetCluster.blocks.size - randomBlocks.size

        for (const cluster of blockOrderClusters.values()) {
            cluster.remaining = intersectMaps(cluster.blocks, remainingBlocks).size
        }

        if (!fillersAdded && unlockOrder.length >= initialUnlocked) {
            fillersAdded = true
            for (let i = -1; i > -fillerCount - 1; i--) {
                remainingBlocks.set(i, [i, i])
            }
        }
    }

    for (const block of remainingBlocks.values()) {
        unlockOrder.push(block)
    }

    return unlockOrder
}


function pickCluster(clusters: BlockOrderCluster[], random: () => number): BlockOrderCluster {
    const totalWeight = clusters.reduce((sum, cluster) => sum + cluster.weight, 0)
    let r = random() * totalWeight
    for (const cluster of clusters) {
        if (r < cluster.weight) {
            return cluster
        }
        r -= cluster.weight
    }
    return clusters[clusters.length - 1]!
}


function randomSample<K, V>(items: Map<K, V>, sampleSize: number, random: () => number): Map<K, V> {
    const itemArray = Array.from(items.entries())

    for (let i = 0; i < sampleSize; i++) {
        const j = i + Math.floor(random() * (itemArray.length - i))
        ;[itemArray[i], itemArray[j]] = [itemArray[j]!, itemArray[i]!]
    }

    const sampledItems = new Map<K, V>()
    for (let i = 0; i < sampleSize; i++) {
        const [key, value] = itemArray[i]!
        sampledItems.set(key, value)
    }

    return sampledItems
}


export function generate(boardState: BoardGenerationState): BoardGenerationState {
    let cluster: Cell[]

    switch (boardState.type) {
        case 'PlacingNumbers':
            if (boardState.state.remainingClusters.length == 0) {
                return {
                    type: 'Failed',
                    reason: 'No remaining clusters to place numbers in',
                }
            }

            cluster = boardState.state.remainingClusters.shift()!
            const currentSolution = new Int32Array(boardState.state.solution)

            try {
                placeNumbersInCluster(cluster, boardState.state)

                boardState.state.solutionHistory.push({
                    solution: currentSolution,
                    cluster: cluster,
                })

            } catch (e) {
                if (boardState.state.solutionHistory.length == 0) {
                    return {
                        type: 'Failed',
                        reason: e instanceof Error ? e.message : 'Unknown error',
                    }
                }

                const previousSolution = boardState.state.solutionHistory.pop()!
                boardState.state.solution.set(previousSolution.solution)
                boardState.state.remainingClusters.unshift(cluster)
                boardState.state.remainingClusters.unshift(previousSolution.cluster)

                return {
                    type: 'PlacingNumbers',
                    state: boardState.state,
                }
            }

            if (boardState.state.remainingClusters.length == 0) {
                // TODO: Sort by unlock order?
                boardState.state.givens = boardState.state.solution.slice()
                boardState.state.remainingClusters = boardState.state.allClusters.slice()
                boardState.state.solutionHistory = []

                return {
                    type: 'RemovingGivens',
                    state: boardState.state,
                }
            } else {
                return {
                    type: 'PlacingNumbers',
                    state: boardState.state,
                }
            }

        case 'RemovingGivens':
            if (boardState.state.remainingClusters.length == 0) {
                return {
                    type: 'Failed',
                    reason: 'No remaining clusters to remove givens from',
                }
            }

            cluster = boardState.state.remainingClusters.shift()!

            removeGivenNumbers(cluster, boardState.state)

            if (boardState.state.remainingClusters.length == 0) {
                boardState.state.remainingClusters = boardState.state.allClusters.slice()

                return {
                    type: 'RestoringGivens',
                    state: boardState.state,
                }

            } else {
                return {
                    type: 'RemovingGivens',
                    state: boardState.state,
                }
            }

        case 'RestoringGivens':
            if (boardState.state.remainingClusters.length == 0) {
                return {
                    type: 'Failed',
                    reason: 'No remaining clusters to restore givens to',
                }
            }

            cluster = boardState.state.remainingClusters.shift()!

            restoreGivenNumbers(cluster, boardState.state)

            if (boardState.state.remainingClusters.length == 0) {
                return clusterStateToCompleted(boardState.state)
            } else {
                return {
                    type: 'RestoringGivens',
                    state: boardState.state,
                }
            }

        case 'Completed':
            return boardState

        case 'Failed':
            return boardState
    }
}


function placeNumbersInCluster(cluster: Cell[], state: ClusterGenerationState): void {
    const possibilitiesMap: PossibilitiesMap = new Int32Array(totalArraySize)

    for (const cellIndex of state.allCellIndices) {
        possibilitiesMap[cellIndex] = (1 << state.blockSize) - 1
    }

    for (const cellIndex of state.allCellIndices) {
        propagateSolution(possibilitiesMap, state.solution, state.peerMap, cellIndex)
    }

    const clusterCellIndices: Set<CellIndex> = getClusterCellIndices(state.blockSize, cluster)

    const result: boolean = tryPlacingNumbers(state.solution, possibilitiesMap, clusterCellIndices, state)

    if (!result) {
        throw new Error('Failed to place numbers in cluster')
    }
}


function propagateSolution(
    possibilitiesMap: PossibilitiesMap,
    solution: Int32Array,
    peerMap: PeerMap,
    cellIndex: CellIndex
): void {
    const number = solution[cellIndex]!

    if (number === 0) {
        return
    }

    const peers: Peers = peerMap[cellIndex]!

    const changes: number[] | null = propagatePossibilities(peers, number, possibilitiesMap)

    if (changes === null) {
        throw new Error('Contradiction encountered during propagation')
    }
}


function propagatePossibilities(
    peers: number[],
    number: number,
    possibilitiesMap: PossibilitiesMap,
): number[] | null {
    const changes: number[] = []
    const removeMask = ~(1 << (number - 1))

    for (const peerIndex of peers) {
        const oldVal = possibilitiesMap[peerIndex]!

        if ((oldVal & ~removeMask) !== 0) {
            const newVal = oldVal & removeMask

            if (newVal === 0) {
                revertPossibilities(changes, number, possibilitiesMap)

                return null
            }

            possibilitiesMap[peerIndex] = newVal
            changes.push(peerIndex)
        }
    }

    return changes
}


function revertPossibilities(
    changes: number[],
    number: number,
    possibilitiesMap: PossibilitiesMap,
): void {
    const addMask = 1 << (number - 1)

    for (const peerIndex of changes) {
        possibilitiesMap[peerIndex] = possibilitiesMap[peerIndex]! | addMask
    }
}


function getClusterCellIndices(blockSize: number, cluster: Cell[]): Set<CellIndex> {
    const clusterCellIndices: Set<CellIndex> = new Set()

    for (const [startRow, startCol] of cluster) {
        for (let r = 0; r < blockSize; r++) {
            for (let c = 0; c < blockSize; c++) {
                const cellIndex: CellIndex = getCellIndex(startRow + r, startCol + c)
                clusterCellIndices.add(cellIndex)
            }
        }
    }

    return clusterCellIndices
}


function tryPlacingNumbers(
    solution: Int32Array,
    possibilitiesMap: PossibilitiesMap,
    clusterCellIndices: Set<CellIndex>,
    state: ClusterGenerationState
): boolean {
    const candidateCells: CellIndex[] = []

    for (const cellIndex of clusterCellIndices) {
        if (solution[cellIndex]! === 0) {
            candidateCells.push(cellIndex)
        }
    }

    const bestCell: CellIndex | null = findBestCell(candidateCells, possibilitiesMap)

    if (bestCell === null) {
        return true
    }

    const bestCellPossibilities: number = possibilitiesMap[bestCell]!
    const possibleNumbers: number[] = numbersFromBitmask(bestCellPossibilities)

    // Shuffle possible numbers
    for (let i = possibleNumbers.length - 1; i > 0; i--) {
        const j = Math.floor(state.random() * (i + 1))
        ;[possibleNumbers[i], possibleNumbers[j]] = [possibleNumbers[j]!, possibleNumbers[i]!]
    }

    for (const number of possibleNumbers) {
        solution[bestCell] = number

        const changes: number[] | null = propagatePossibilities(
            state.peerMap[bestCell]!,
            number,
            possibilitiesMap
        )

        if (changes === null) {
            solution[bestCell] = 0

            continue
        }

        const result: boolean = tryPlacingNumbers(solution, possibilitiesMap, clusterCellIndices, state)

        if (result) {
            return true
        } else {
            revertPossibilities(changes, number, possibilitiesMap)
            solution[bestCell] = 0
        }
    }

    return false
}


function findBestCell(candidateCells: CellIndex[], possibilitiesMap: PossibilitiesMap): CellIndex | null {
    let bestCell: CellIndex | null = null
    let bestCount: number = Infinity

    for (const cellIndex of candidateCells) {
        const possibilities = possibilitiesMap[cellIndex]!
        const count = countSetBits(possibilities)

        if (count < bestCount) {
            bestCount = count
            bestCell = cellIndex
        }
    }

    return bestCell
}


function removeGivenNumbers(cluster: Cell[], state: ClusterGenerationState): void {
    if (state.blockSize <= 9) {
        removeGivenNumbersBacktracking(cluster, state)
    } else {
        removeGivenNumbersLogical(cluster, state)
    }
}


function removeGivenNumbersBacktracking(
    cluster: Cell[],
    state: ClusterGenerationState
): void {
    const solverBuffer: Int32Array = new Int32Array(totalArraySize)

    const clusterCellIndices: Set<CellIndex> = getClusterCellIndices(state.blockSize, cluster)

    const cellsToRemoveFrom: Set<CellIndex> = intersectSets(
        clusterCellIndices,
        state.cellIndicesToRemoveGivensFrom
    )
    state.cellIndicesToRemoveGivensFrom = diffSets(
        state.cellIndicesToRemoveGivensFrom,
        cellsToRemoveFrom
    )
    const indicesToRemove: number[] = []

    for (const cellIndex of cellsToRemoveFrom) {
        if (state.givens[cellIndex]! !== 0) {
            indicesToRemove.push(cellIndex)
        }
    }

    for (let i = indicesToRemove.length - 1; i > 0; i--) {
        const j = Math.floor(state.random() * (i + 1))
        ;[indicesToRemove[i], indicesToRemove[j]] = [indicesToRemove[j]!, indicesToRemove[i]!]
    }

    for (const cellIndex of indicesToRemove) {
        const originalValue: number = state.solution[cellIndex]!
        state.givens[cellIndex] = 0
        solverBuffer.set(state.givens)

        const possibilitiesMap: PossibilitiesMap = new Int32Array(totalArraySize)

        for (const idx of clusterCellIndices) {
            possibilitiesMap[idx] = (1 << state.blockSize) - 1
        }

        for (const idx of clusterCellIndices) {
            propagateSolution(possibilitiesMap, solverBuffer, state.peerMap, idx)
        }

        possibilitiesMap[cellIndex]! &= ~(1 << (originalValue - 1))

        const hasOtherSolution: boolean = tryPlacingNumbers(
            solverBuffer,
            possibilitiesMap,
            clusterCellIndices,
            state
        )

        if (hasOtherSolution) {
            state.givens[cellIndex] = originalValue
        }
    }
}


function removeGivenNumbersLogical(
    cluster: Cell[],
    state: ClusterGenerationState
): void {
    // Placeholder for logical removal of given numbers
}


function restoreGivenNumbers(
    cluster: Cell[],
    state: ClusterGenerationState
): void {

    const clusterCellIndices: Set<CellIndex> = getClusterCellIndices(state.blockSize, cluster)

    let targetGivens: number = 0;

    switch (state.blockSize) {
        case 9:
            targetGivens = Math.round((38 / 81) * clusterCellIndices.size)

            break
    }

    let currentGivens: number = 0
    for (const cellIndex of clusterCellIndices) {
        if (state.givens[cellIndex]! !== 0) {
            currentGivens += 1
        }
    }

    const cellsToRestoreFrom: Set<CellIndex> = intersectSets(
        clusterCellIndices,
        state.cellIndicesToRestoreGivensFrom
    )
    state.cellIndicesToRestoreGivensFrom = diffSets(
        state.cellIndicesToRestoreGivensFrom,
        cellsToRestoreFrom
    )
    const indicesToRestore: number[] = []

    for (const cellIndex of cellsToRestoreFrom) {
        if (state.givens[cellIndex]! === 0) {
            indicesToRestore.push(cellIndex)
        }
    }

    for (let i = indicesToRestore.length - 1; i > 0; i--) {
        const j = Math.floor(state.random() * (i + 1))
        ;[indicesToRestore[i], indicesToRestore[j]] = [indicesToRestore[j]!, indicesToRestore[i]!]
    }


    for (const cellIndex of indicesToRestore) {
        if (currentGivens >= targetGivens) {
            break
        }

        let canRestore: boolean = true
        const cellAreas: Area[] = state.cellAreasMap[cellIndex]!

        // Check if restoring this given would make any area fully given
        for (const area of cellAreas) {
            const areaCellIndices: CellIndex[] = getCellIndicesInArea(area)
            let givenCount: number = 0

            for (const areaCellIndex of areaCellIndices) {
                if (state.givens[areaCellIndex]! !== 0) {
                    givenCount += 1
                }
            }

            if (givenCount + 1 >= areaCellIndices.length) {
                canRestore = false
                break
            }
        }

        if (!canRestore) {
            continue
        }

        state.givens[cellIndex] = state.solution[cellIndex]!
        currentGivens += 1
    }
}


function clusterStateToCompleted(state: ClusterGenerationState): Completed {
    let givens: [number, number, number][] = []
    let solution: [number, number, number][] = []

    for (let [row, col] of state.allCells) {
        const cellIndex: CellIndex = getCellIndex(row, col)
        let number: number = state.solution[cellIndex]!

        solution.push([row, col, number])

        if (state.givens[cellIndex]!) {
            givens.push([row, col, number])
        }
    }

    let encodedPuzzleAreas: EncodedPuzzleAreas = {
        boards: state.puzzleAreas.boards.map(encodeArea),
        blocks: state.puzzleAreas.blocks.map(encodeArea),
        rows: state.puzzleAreas.rows.map(encodeArea),
        cols: state.puzzleAreas.cols.map(encodeArea),
    }

    return {
        type: 'Completed',
        blockSize: state.blockSize,
        givens: givens,
        puzzleAreas: encodedPuzzleAreas,
        solution: solution,
        unlockCount: 17,
        unlockOrder: state.blockUnlockOrder,
    }
}


function encodeArea(area: Area): [number, number, number, number] {
    return [area.startRow, area.startCol, area.endRow, area.endCol]
}


function countSetBits(n: number): number {
    let count = 0
    while (n) {
        count += n & 1
        n >>= 1
    }
    return count
}


function numbersFromBitmask(bitmask: number): number[] {
    const numbers: number[] = []
    let num = 1
    while (bitmask) {
        if (bitmask & 1) {
            numbers.push(num)
        }
        bitmask >>= 1
        num += 1
    }

    return numbers
}


function getCellIndex(row: number, col: number): CellIndex {
    return (row - 1) * maxWidth + (col - 1)
}


function blockSizeToDimensions(blockSize: number): [number, number] {
    switch (blockSize) {
        case 4:
            return [2, 2]
        case 6:
            return [2, 3]
        case 8:
            return [2, 4]
        case 9:
            return [3, 3]
        case 12:
            return [3, 4]
        case 16:
            return [4, 4]
        default:
            throw new Error('Unsupported block size')
    }
}


function blockSizeToOverlap(blockSize: number): [number, number] {
    switch (blockSize) {
        case 4:
            return [1, 1]
        case 6:
            return [2, 2]
        case 8:
            return [2, 2]
        case 9:
            return [3, 3]
        case 12:
            return [3, 4]
        case 16:
            return [4, 4]
        default:
            throw new Error('Unsupported block size')
    }
}


function diffSets<T>(setA: Set<T>, setB: Set<T>): Set<T> {
    const difference: Set<T> = new Set()

    for (const item of setA) {
        if (!setB.has(item)) {
            difference.add(item)
        }
    }

    return difference
}


function intersectSets<T>(setA: Set<T>, setB: Set<T>): Set<T> {
    const intersection: Set<T> = new Set()
    const [smallerSet, largerSet] = setA.size < setB.size ? [setA, setB] : [setB, setA]

    for (const item of smallerSet) {
        if (largerSet.has(item)) {
            intersection.add(item)
        }
    }

    return intersection
}


function diffMaps<K, V>(mapA: Map<K, V>, mapB: Map<K, V>): Map<K, V> {
    const difference: Map<K, V> = new Map()

    for (const [key, value] of mapA) {
        if (!mapB.has(key)) {
            difference.set(key, value)
        }
    }

    return difference
}


function intersectMaps<K, V>(mapA: Map<K, V>, mapB: Map<K, V>): Map<K, V> {
    const intersection: Map<K, V> = new Map()

    for (const [key, value] of mapA) {
        if (mapB.has(key)) {
            intersection.set(key, value)
        }
    }

    return intersection
}


// Mulberry32 random number generator
function createRandomGenerator(seed: number): () => number {
    return function() {
        let t = seed += 0x6D2B79F5
        t = Math.imul(t ^ (t >>> 15), t | 1)
        t ^= t + Math.imul(t ^ (t >>> 7), t | 61)
        return ((t ^ (t >>> 14)) >>> 0) / 4294967296
    }
}
