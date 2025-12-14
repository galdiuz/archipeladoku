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


type GenerateArgs = GenerateLocalArgs | GenerateServerArgs


interface GenerateLocalArgs {
    blockSize: number
    difficulty: number
    numberOfBoards: number
    seed: number
}


interface GenerateServerArgs {
    blockSize: number
    blockUnlockOrder: Cell[]
    clusters: Cell[][]
    difficulty: number
    seed: number
}


interface BlockOrderCluster {
    id: number
    blocks: Map<number, Cell>
    reward: number
    weight: number
}


type BoardGenerationState =
    | PlacingNumbers
    | RemovingGivens
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


interface Completed {
    type: 'Completed'
    blockSize: number
    givens: EncodedCellValue[]
    solution: EncodedCellValue[]
    puzzleAreas: EncodedPuzzleAreas
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
    cellBlockIndicesMap: CellIndex[][][]
    cellColIndicesMap: CellIndex[][][]
    cellRowIndicesMap: CellIndex[][][]
    cellIndicesToRemoveGivensFrom: Set<CellIndex>
    difficulty: number
    givens: Int32Array
    puzzleAreas: PuzzleAreas
    peerMap: PeerMap
    rng: () => number
    remainingClusters: Cell[][]
    solution: Int32Array
    solutionHistory: SolutionHistory[]
}


interface SolutionHistory {
    solution: Int32Array
    cluster: Cell[]
}


const maxWidth: number = 180 // Supports up to 98 blocks of size 16x16 with overlaps
const totalArraySize: number = maxWidth * maxWidth


export function initGeneration(args: GenerateArgs): BoardGenerationState {
    if (![4, 6, 8, 9, 12, 16].includes(args.blockSize)) {
        return {
            type: 'Failed',
            reason: 'Unsupported block size',
        }
    }

    const numberOfBoards = "numberOfBoards" in args
        ? Math.min(Math.max(1, args.numberOfBoards), 98)
        : 1
    const positions: Cell[] = "clusters" in args
        ? args.clusters.reduce((acc, cluster) => acc.concat(cluster), [])
        : positionBoards(args.blockSize, numberOfBoards)

    const boardPuzzleAreas: PuzzleAreas[] = []
    for (const [startRow, startCol] of positions) {
        const puzzleAreas = buildPuzzleAreas(args.blockSize, startRow, startCol)
        boardPuzzleAreas.push(puzzleAreas)
    }
    const puzzleAreas: PuzzleAreas = joinPuzzleAreas(boardPuzzleAreas)
    const areas: Area[] = [...puzzleAreas.blocks, ...puzzleAreas.rows, ...puzzleAreas.cols]
    const cellAreaIndicesMap: CellIndex[][][] = buildCellAreaIndicesMap(areas)
    const cellBlockIndicesMap: CellIndex[][][] = buildCellAreaIndicesMap(puzzleAreas.blocks)
    const cellRowIndicesMap: CellIndex[][][] = buildCellAreaIndicesMap(puzzleAreas.rows)
    const cellColIndicesMap: CellIndex[][][] = buildCellAreaIndicesMap(puzzleAreas.cols)
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

    const rng: () => number = createRandomGenerator(args.seed)
    const peerMap: PeerMap = buildPeerMap(cells, cellAreaIndicesMap)
    const clusters: Cell[][] = "clusters" in args
        ? args.clusters
        : buildClusters(positions, rng)
    const blockUnlockOrder: Cell[] = "blockUnlockOrder" in args
        ? args.blockUnlockOrder
        : buildBlockUnlockOrder(
            args.blockSize,
            positions.length,
            puzzleAreas.blocks,
            clusters,
            rng
        )

    return {
        type: 'PlacingNumbers',
        state: {
            allCells: cells,
            allCellIndices: cellIndices,
            allClusters: clusters,
            blockSize: args.blockSize,
            blockUnlockOrder: blockUnlockOrder,
            cellBlockIndicesMap: cellBlockIndicesMap,
            cellColIndicesMap: cellColIndicesMap,
            cellRowIndicesMap: cellRowIndicesMap,
            cellIndicesToRemoveGivensFrom: new Set(cellIndices),
            difficulty: args.difficulty,
            givens: new Int32Array(totalArraySize),
            puzzleAreas: puzzleAreas,
            peerMap: peerMap,
            rng: rng,
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
    for (let r = 0; r < blockCols; r++) {
        for (let c = 0; c < blockRows; c++) {
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
    const boards: Map<number, Area> = new Map()
    const blocks: Map<number, Area> = new Map()
    const rows: Map<number, Area> = new Map()
    const cols: Map<number, Area> = new Map()

    for (const puzzleAreas of puzzleAreasList) {
        for (const board of puzzleAreas.boards) {
            const key = getCellIndex(board.startRow, board.startCol)
            boards.set(key, board)
        }
        for (const block of puzzleAreas.blocks) {
            const key = getCellIndex(block.startRow, block.startCol)
            blocks.set(key, block)
        }
        for (const row of puzzleAreas.rows) {
            const key = getCellIndex(row.startRow, row.startCol)
            rows.set(key, row)
        }
        for (const col of puzzleAreas.cols) {
            const key = getCellIndex(col.startRow, col.startCol)
            cols.set(key, col)
        }
    }

    return {
        boards: Array.from(boards.values()),
        blocks: Array.from(blocks.values()),
        rows: Array.from(rows.values()),
        cols: Array.from(cols.values()),
    }
}


function buildCellAreaIndicesMap(areas: Area[]): CellIndex[][][] {
    const map: CellIndex[][][] = Array.from({ length: totalArraySize }, () => [])

    for (const area of areas) {
        const cellIndices: CellIndex[] = getCellIndicesInArea(area)
        for (const cellIndex of cellIndices) {
            map[cellIndex]!.push(cellIndices)
        }
    }

    return map
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


function buildPeerMap(cells: Set<Cell>, cellAreaIndicesMap: CellIndex[][][]): PeerMap {
    const peerMap: PeerMap = Array.from({ length: totalArraySize }, () => [])

    for (const [row, col] of cells) {
        const cellIndex: CellIndex = getCellIndex(row, col)
        const areas: CellIndex[][] = cellAreaIndicesMap[cellIndex]!
        const peerSet: Set<number> = new Set()

        for (const areaIndices of areas) {
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


function buildClusters(positions: Cell[], rng: () => number): Cell[][] {
    // Placeholder for cluster building logic
    return positions.map(pos => [pos])
}


function buildBlockUnlockOrder(
    blockSize: number,
    numberOfBoards: number,
    allBlocks: Area[],
    clusters: Cell[][],
    rng: () => number,
): Cell[] {
    const assignedBlocks: Set<number> = new Set()
    const remainingBlocks: Map<number, Cell> = new Map()
    const blockOrderClusters: Map<number, BlockOrderCluster> = new Map()
    const fillerCount = blockSize // Initial blocks
        + numberOfBoards // One per board
        + numberOfBoards * blockSize * 2 // One per row and column
    let fillersAdded = false

    for (const block of allBlocks) {
        const key = getCellIndex(block.startRow, block.startCol)
        remainingBlocks.set(key, [block.startRow, block.startCol])
    }

    for (let i = 0; i < clusters.length; i++) {
        const cluster = clusters[i]!
        const blockOrderCluster: BlockOrderCluster = {
            id: i,
            blocks: new Map<number, Cell>(),
            reward: 0,
            weight: 0,
        }
        blockOrderClusters.set(i, blockOrderCluster)

        for (const [startRow, startCol] of cluster) {
            let boardBlocks = buildPuzzleAreas(blockSize, startRow, startCol).blocks
            blockOrderCluster.reward += 1 // Reward for solving the board
            blockOrderCluster.reward += blockSize * 2 // Reward for solving rows and columns

            for (const block of boardBlocks) {
                const blockKey = getCellIndex(block.startRow, block.startCol)
                blockOrderCluster.blocks.set(blockKey, [block.startRow, block.startCol])
            }
        }
    }

    let credits: number = blockSize
    const unlockOrder: Cell[] = []

    while (blockOrderClusters.size > 0) {
        for (const cluster of blockOrderClusters.values()) {
            const remaining = intersectMaps(cluster.blocks, remainingBlocks).size
            if (remaining > credits) {
                cluster.weight = 0
            } else if (cluster.id === 0) {
                cluster.weight = 1000000000
            } else {
                cluster.weight = credits - remaining + 1
            }
        }

        const blockOrderClustersArray = Array.from(blockOrderClusters.values())
        const targetCluster = pickCluster(blockOrderClustersArray, rng)
        const targetBlocks = intersectMaps(targetCluster.blocks, remainingBlocks)
        const remainingBlocksExcludingTarget = diffMaps(remainingBlocks, targetCluster.blocks)
        const remainingCredits = credits - targetBlocks.size
        const randomBudgetMax = Math.min(remainingCredits, remainingBlocksExcludingTarget.size)
        const randomBudget = Math.floor(rng() * randomBudgetMax)
        const randomBlocks = randomSample(remainingBlocksExcludingTarget, randomBudget, rng)
        const blocksToAdd: Cell[] = Array.from(targetBlocks.values())
            .concat(Array.from(randomBlocks.values()))

        shuffleArray(blocksToAdd, rng)

        for (const block of blocksToAdd) {
            unlockOrder.push(block)
            const blockKey = block[0] > 0 ? getCellIndex(block[0], block[1]) : block[0]
            remainingBlocks.delete(blockKey)
        }

        appendToSet(assignedBlocks, targetCluster.blocks.keys())
        blockOrderClusters.delete(targetCluster.id)
        credits = remainingCredits
            + targetCluster.blocks.size
            + targetCluster.reward
            - randomBlocks.size

        for (const cluster of blockOrderClusters.values()) {
            for (const assignedBlockKey of assignedBlocks) {
                cluster.blocks.delete(assignedBlockKey)
            }
        }

        if (!fillersAdded) {
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


function pickCluster(clusters: BlockOrderCluster[], rng: () => number): BlockOrderCluster {
    const totalWeight = clusters.reduce((sum, cluster) => sum + cluster.weight, 0)
    let r = rng() * totalWeight
    for (const cluster of clusters) {
        if (r < cluster.weight) {
            return cluster
        }
        r -= cluster.weight
    }

    return clusters[clusters.length - 1]!
}


function randomSample<K, V>(items: Map<K, V>, sampleSize: number, rng: () => number): Map<K, V> {
    const itemArray = Array.from(items.entries())

    for (let i = 0; i < sampleSize; i++) {
        const j = i + Math.floor(rng() * (itemArray.length - i))
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
                return clusterStateToCompleted(boardState.state)
            } else {
                return {
                    type: 'RemovingGivens',
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
    const possibilitiesMap: PossibilitiesMap = createPossibilitiesMap(
        state.blockSize,
        state.peerMap,
        state.allCellIndices,
        state.solution
    )

    const clusterCellIndices: Set<CellIndex> = getClusterCellIndices(state.blockSize, cluster)

    const result: boolean = solveWithBacktracking(state.solution, possibilitiesMap, clusterCellIndices, state)

    if (!result) {
        throw new Error('Failed to place numbers in cluster')
    }
}


function createPossibilitiesMap(
    blockSize: number,
    peerMap: PeerMap,
    cellIndices: Set<CellIndex>,
    solution: Int32Array,
): PossibilitiesMap {
    const possibilitiesMap: PossibilitiesMap = new Int32Array(totalArraySize)

    for (const cellIndex of cellIndices) {
        possibilitiesMap[cellIndex] = (1 << blockSize) - 1
    }

    for (const cellIndex of cellIndices) {
        propagateSolution(possibilitiesMap, solution, peerMap, cellIndex)
    }

    return possibilitiesMap
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


function solveWithBacktracking(
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
    const possibleNumbers: number[] = numbersFromBits(bestCellPossibilities)

    shuffleArray(possibleNumbers, state.rng)

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

        const result: boolean = solveWithBacktracking(solution, possibilitiesMap, clusterCellIndices, state)

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

    shuffleArray(indicesToRemove, state.rng)

    removeGivenNumbersLogical(cluster, state, clusterCellIndices, indicesToRemove)
}


// Unused
function removeGivenNumbersBacktracking(
    cluster: Cell[],
    state: ClusterGenerationState,
    clusterCellIndices: Set<CellIndex>,
    indicesToRemove: number[],
): void {
    const solutionBuffer: Int32Array = new Int32Array(totalArraySize)

    for (const cellIndex of indicesToRemove) {
        const originalValue: number = state.solution[cellIndex]!
        state.givens[cellIndex] = 0
        solutionBuffer.set(state.givens)

        const possibilitiesMap: PossibilitiesMap = createPossibilitiesMap(
            state.blockSize,
            state.peerMap,
            clusterCellIndices,
            solutionBuffer
        )

        // Remove the original value from possibilities to force finding an alternative solution
        possibilitiesMap[cellIndex]! &= ~(1 << (originalValue - 1))

        const hasOtherSolution: boolean = solveWithBacktracking(
            solutionBuffer,
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
    state: ClusterGenerationState,
    clusterCellIndices: Set<CellIndex>,
    indicesToRemove: number[],
): void {
    const solutionBuffer: Int32Array = new Int32Array(totalArraySize)

    const clusterAreasList: PuzzleAreas[] = []
    for (const [row, col] of cluster) {
        clusterAreasList.push(buildPuzzleAreas(state.blockSize, row, col))
    }
    const clusterPuzzleAreas: PuzzleAreas = joinPuzzleAreas(clusterAreasList)
    const clusterAreaIndices: CellIndex[][] = []
    const clusterBlockIndices: CellIndex[][] = []
    const clusterLineIndices: CellIndex[][] = []

    for (const area of [...clusterPuzzleAreas.blocks, ...clusterPuzzleAreas.rows, ...clusterPuzzleAreas.cols]) {
        clusterAreaIndices.push(getCellIndicesInArea(area))
    }
    for (const area of clusterPuzzleAreas.blocks) {
        clusterBlockIndices.push(getCellIndicesInArea(area))
    }
    for (const area of [...clusterPuzzleAreas.rows, ...clusterPuzzleAreas.cols]) {
        clusterLineIndices.push(getCellIndicesInArea(area))
    }

    // Restore givens to a solvable state first
    while (true) {
        solutionBuffer.set(state.givens)

        const possibilitiesMap: PossibilitiesMap = createPossibilitiesMap(
            state.blockSize,
            state.peerMap,
            clusterCellIndices,
            solutionBuffer
        )

        const isSolvable: boolean = solveWithLogic(
            solutionBuffer,
            possibilitiesMap,
            clusterCellIndices,
            clusterAreaIndices,
            clusterBlockIndices,
            clusterLineIndices,
            state
        )

        if (isSolvable) {
            break
        }

        const emptyIndices: number[] = []
        for (const cellIndex of clusterCellIndices) {
            if (state.givens[cellIndex]! === 0) {
                emptyIndices.push(cellIndex)
            }
        }

        const restoreIndex = findBestCell(emptyIndices, possibilitiesMap)!
        state.givens[restoreIndex] = state.solution[restoreIndex]!
        indicesToRemove.push(restoreIndex)
    }

    for (const cellIndex of indicesToRemove) {
        const originalValue: number = state.solution[cellIndex]!
        state.givens[cellIndex] = 0
        solutionBuffer.set(state.givens)

        const possibilitiesMap: PossibilitiesMap = createPossibilitiesMap(
            state.blockSize,
            state.peerMap,
            clusterCellIndices,
            solutionBuffer
        )

        const isSolvable: boolean = solveWithLogic(
            solutionBuffer,
            possibilitiesMap,
            clusterCellIndices,
            clusterAreaIndices,
            clusterBlockIndices,
            clusterLineIndices,
            state
        )

        if (!isSolvable) {
            state.givens[cellIndex] = originalValue
        }
    }
}


function solveWithLogic(
    solution: Int32Array,
    possibilitiesMap: PossibilitiesMap,
    cellIndices: Set<CellIndex>,
    areaIndices: CellIndex[][],
    blockIndices: CellIndex[][],
    lineIndices: CellIndex[][],
    state: ClusterGenerationState,
): boolean {
    let madeProgress: boolean = true

    const functionsToApply = [
        () => applyNakedSingles(solution, possibilitiesMap, cellIndices, state),
        () => applyHiddenSingles(solution, possibilitiesMap, cellIndices, areaIndices, state),
    ]

    if (state.difficulty >= 2) {
        functionsToApply.push(() => applyPointingPairs(solution, possibilitiesMap, blockIndices, state))
        functionsToApply.push(() => applyBoxLineReduction(solution, possibilitiesMap, lineIndices, state))
    }

    if (state.difficulty >= 3) {
        functionsToApply.push(() => applyNakedPairs(solution, possibilitiesMap, areaIndices))
        functionsToApply.push(() => applyHiddenPairs(solution, possibilitiesMap, areaIndices, state))
        // TODO: Split naked and hidden pairs into separate difficulties
        // TODO: naked triples, hidden triples
    }

    while (madeProgress) {
        madeProgress = false

        for (const fun of functionsToApply) {
            if (fun()) {
                madeProgress = true

                break
            }
        }
    }

    let solved: boolean = true
    for (const cellIndex of cellIndices) {
        if (solution[cellIndex]! === 0) {
            solved = false

            break
        }
    }

    return solved
}


function applyNakedSingles(
    solution: Int32Array,
    possibilitiesMap: PossibilitiesMap,
    cellIndices: Set<CellIndex>,
    state: ClusterGenerationState
): boolean {
    let madeProgress: boolean = false

    for (const cellIndex of cellIndices) {
        if (solution[cellIndex]! !== 0) {
            continue
        }

        const possibilities: number = possibilitiesMap[cellIndex]!
        if (countSetBits(possibilities) !== 1) {
            continue
        }

        const number = numberFromBits(possibilities)
        solution[cellIndex] = number
        propagateSolution(possibilitiesMap, solution, state.peerMap, cellIndex)
        madeProgress = true
    }

    return madeProgress
}


function applyHiddenSingles(
    solution: Int32Array,
    possibilitiesMap: PossibilitiesMap,
    cellIndices: Set<CellIndex>,
    areaIndices: CellIndex[][],
    state: ClusterGenerationState,
): boolean {
    let madeProgress: boolean = false
    const counts = new Int32Array(state.blockSize + 1)
    const positions = new Int32Array(state.blockSize + 1)

    for (const areaCellIndices of areaIndices) {
        counts.fill(0)

        for (const cellIndex of areaCellIndices) {
            if (solution[cellIndex]! !== 0) {
                continue
            }

            const possibilities: number = possibilitiesMap[cellIndex]!
            for (let num = 1; num <= state.blockSize; num++) {
                if ((possibilities & (1 << (num - 1))) !== 0) {
                    counts[num]! += 1
                    positions[num] = cellIndex
                }
            }
        }

        for (let num = 1; num <= state.blockSize; num++) {
            if (counts[num] === 1) {
                const targetCellIndex: CellIndex = positions[num]!
                if (cellIndices.has(targetCellIndex) && solution[targetCellIndex]! === 0) {
                    solution[targetCellIndex] = num
                    propagateSolution(possibilitiesMap, solution, state.peerMap, targetCellIndex)
                    madeProgress = true
                }
            }
        }
    }

    return madeProgress
}


function applyPointingPairs(
    solution: Int32Array,
    possibilitiesMap: PossibilitiesMap,
    blocks: CellIndex[][],
    state: ClusterGenerationState,
): boolean {
    let madeProgress: boolean = false
    const positions: number[][] = Array.from({ length: state.blockSize + 1 }, () => [])

    for (const blockIndices of blocks) {
        for (let n = 1; n <= state.blockSize; n++) {
            positions[n]!.length = 0
        }

        for (const cellIndex of blockIndices) {
            if (solution[cellIndex]! !== 0) {
                continue
            }

            const possibilities: number = possibilitiesMap[cellIndex]!
            for (let n = 1; n <= state.blockSize; n++) {
                if ((possibilities & (1 << (n - 1))) !== 0) {
                    positions[n]!.push(cellIndex)
                }
            }
        }

        for (let n = 1; n <= state.blockSize; n++) {
            const numberCells: CellIndex[] = positions[n]!

            if (numberCells.length < 2) {
                continue
            }

            const firstCellIndex = numberCells[0]!
            const firstRow = Math.floor(firstCellIndex / maxWidth)
            const allSameRow = numberCells.every(cellIndex => Math.floor(cellIndex / maxWidth) === firstRow)

            if (allSameRow) {
                const rows: CellIndex[][] = state.cellRowIndicesMap[firstCellIndex]!
                const removeMask = ~(1 << (n - 1))

                for (const row of rows) {
                    for (const cellIndex of row) {
                        if (numberCells.includes(cellIndex)) {
                            continue
                        }

                        if (solution[cellIndex]! !== 0) {
                            continue
                        }

                        const oldPossibilities = possibilitiesMap[cellIndex]!
                        const newPossibilities = oldPossibilities & removeMask

                        if (newPossibilities !== oldPossibilities) {
                            possibilitiesMap[cellIndex] = newPossibilities
                            madeProgress = true
                        }
                    }
                }
            }

            const firstCol = numberCells[0]! % maxWidth
            const allSameCol = numberCells.every(cellIndex => cellIndex % maxWidth === firstCol)

            if (allSameCol) {
                const cols: CellIndex[][] = state.cellColIndicesMap[firstCellIndex]!
                const removeMask = ~(1 << (n - 1))

                for (const col of cols) {
                    for (const cellIndex of col) {
                        if (numberCells.includes(cellIndex)) {
                            continue
                        }

                        if (solution[cellIndex]! !== 0) {
                            continue
                        }

                        const oldPossibilities = possibilitiesMap[cellIndex]!
                        const newPossibilities = oldPossibilities & removeMask

                        if (newPossibilities !== oldPossibilities) {
                            possibilitiesMap[cellIndex] = newPossibilities
                            madeProgress = true
                        }
                    }
                }
            }
        }
    }

    return madeProgress
}


function applyBoxLineReduction(
    solution: Int32Array,
    possibilitiesMap: PossibilitiesMap,
    lines: CellIndex[][],
    state: ClusterGenerationState,
): boolean {
    let madeProgress: boolean = false

    const positions: number[][] = Array.from({ length: state.blockSize + 1 }, () => [])

    for (const lineIndices of lines) {
        for (let n = 1; n <= state.blockSize; n++) {
            positions[n]!.length = 0
        }

        for (const cellIndex of lineIndices) {
            if (solution[cellIndex]! !== 0) {
                continue
            }

            const possibilities: number = possibilitiesMap[cellIndex]!
            for (let n = 1; n <= state.blockSize; n++) {
                if ((possibilities & (1 << (n - 1))) !== 0) {
                    positions[n]!.push(cellIndex)
                }
            }
        }

        for (let n = 1; n <= state.blockSize; n++) {
            const numberCells: CellIndex[] = positions[n]!

            if (numberCells.length < 2) {
                continue
            }

            const firstCellIndex = numberCells[0]!
            let commonBlocks: CellIndex[][] = [...state.cellBlockIndicesMap[firstCellIndex]!]

            for (let i = 1; i < numberCells.length; i++) {
                const cellIndex = numberCells[i]!
                const cellBlocks = state.cellBlockIndicesMap[cellIndex]!

                commonBlocks = commonBlocks.filter(block => cellBlocks.includes(block))

                if (commonBlocks.length === 0) {
                    break
                }
            }

            if (commonBlocks.length === 0) {
                continue
            }

            const removeMask = ~(1 << (n - 1))

            for (const block of commonBlocks) {
                for (const cellIndex of block) {
                    if (numberCells.includes(cellIndex)) {
                        continue
                    }

                    if (solution[cellIndex]! !== 0) {
                        continue
                    }

                    const oldPossibilities = possibilitiesMap[cellIndex]!
                    const newPossibilities = oldPossibilities & removeMask

                    if (newPossibilities !== oldPossibilities) {
                        possibilitiesMap[cellIndex] = newPossibilities
                        madeProgress = true
                    }
                }
            }
        }
    }

    return madeProgress
}


function applyNakedPairs(
    solution: Int32Array,
    possibilitiesMap: PossibilitiesMap,
    areaIndices: CellIndex[][],
): boolean {
    let madeProgress: boolean = false

    for (const areaCellIndices of areaIndices) {
        for (let i = 0; i < areaCellIndices.length; i++) {
            const cellIndexA = areaCellIndices[i]!

            if (solution[cellIndexA]! !== 0) {
                continue
            }

            const possibilitiesA = possibilitiesMap[cellIndexA]!

            if (countSetBits(possibilitiesA) !== 2) {
                continue
            }

            for (let j = i + 1; j < areaCellIndices.length; j++) {
                const cellIndexB = areaCellIndices[j]!

                if (solution[cellIndexB]! !== 0) {
                    continue
                }

                const possibilitiesB = possibilitiesMap[cellIndexB]!

                if (possibilitiesA !== possibilitiesB) {
                    continue
                }

                for (let k = 0; k < areaCellIndices.length; k++) {
                    if (k === i || k === j) {
                        continue
                    }

                    const targetIndex = areaCellIndices[k]!

                    if (solution[targetIndex]! !== 0) {
                        continue
                    }

                    const oldPossibilities = possibilitiesMap[targetIndex]!
                    const newPossibilities = oldPossibilities & ~possibilitiesA

                    if (newPossibilities !== oldPossibilities) {
                        possibilitiesMap[targetIndex] = newPossibilities
                        madeProgress = true
                    }
                }
            }
        }
    }

    return madeProgress
}


function applyHiddenPairs(
    solution: Int32Array,
    possibilitiesMap: PossibilitiesMap,
    areaIndices: CellIndex[][],
    state: ClusterGenerationState,
): boolean {
    let madeProgress: boolean = false
    // Index is number, value is array of cell indices where the number can go
    const positions: number[][] = Array.from({ length: state.blockSize + 1 }, () => [])

    for (const areaCellIndices of areaIndices) {
        // Reset positions
        for (let num = 1; num < positions.length; num++) {
            positions[num]!.length = 0
        }

        for (const cellIndex of areaCellIndices) {
            if (solution[cellIndex]! !== 0) {
                continue
            }

            const possibilities: number = possibilitiesMap[cellIndex]!
            for (let num = 1; num < positions.length; num++) {
                if ((possibilities & (1 << (num - 1))) !== 0) {
                    positions[num]!.push(cellIndex)
                }
            }
        }

        for (let numA = 1; numA < positions.length; numA++) {
            const posA = positions[numA]!

            if (posA.length !== 2) {
                continue
            }

            for (let numB = numA + 1; numB < positions.length; numB++) {
                const posB = positions[numB]!

                if (posB.length !== 2
                    || posA[0] !== posB[0]
                    || posA[1] !== posB[1]
                ) {
                    continue
                }

                const keepMask = (1 << (numA - 1)) | (1 << (numB - 1))

                for (const cellIndex of posA) {
                    const oldPossibilities = possibilitiesMap[cellIndex]!
                    const newPossibilities = oldPossibilities & keepMask

                    if (newPossibilities !== oldPossibilities) {
                        possibilitiesMap[cellIndex] = newPossibilities
                        madeProgress = true
                    }
                }
            }
        }
    }

    return madeProgress
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


function numberFromBits(bitmask: number): number {
    if (bitmask === 0) {
        return 0
    }

    return Math.log2(bitmask & -bitmask) + 1
}


function numbersFromBits(bitmask: number): number[] {
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


function shuffleArray<T>(array: T[], rng: () => number): void {
    for (let i = array.length - 1; i > 0; i--) {
        const j = Math.floor(rng() * (i + 1))
        ;[array[i], array[j]] = [array[j]!, array[i]!]
    }
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


function appendToSet<T>(setA: Set<T>, setB: Iterable<T>): void {
    for (const item of setB) {
        setA.add(item)
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
