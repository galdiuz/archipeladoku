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
    type: 'RemovingGivens'
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
    allClusters: Cell[][]
    blockSize: number
    cellsToRemoveGivensFrom: Set<Cell>
    givens: Int32Array
    puzzleAreas: PuzzleAreas
    peerMap: PeerMap
    random: () => number
    remainingClusters: Cell[][]
    solution: Int32Array
}


const maxWidth: number = 300
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

    for (const board of puzzleAreas.boards) {
        const boardCells: Cell[] = getCellsInArea(board)
        for (const cell of boardCells) {
            cells.add(cell)
        }
    }

    const random: () => number = createRandomGenerator(args.seed)
    const peerMap: PeerMap = buildPeerMap(cells, cellAreasMap)
    const clusters: Cell[][] = buildClusters(positions, random)

    return {
        type: 'PlacingNumbers',
        state: {
            allCells: cells,
            allClusters: clusters,
            blockSize: args.blockSize,
            cellsToRemoveGivensFrom: cells,
            givens: new Int32Array(totalArraySize),
            puzzleAreas: puzzleAreas,
            peerMap: peerMap,
            random: random,
            remainingClusters: clusters,
            solution: new Int32Array(totalArraySize),
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
        const cells: Cell[] = getCellsInArea(area)
        for (const [row, col] of cells) {
            const cellIndex: CellIndex = getCellIndex(row, col)
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


function buildPeerMap(cells: Set<Cell>, cellAreasMap: Area[][]): PeerMap {
    const peerMap: PeerMap = Array.from({ length: totalArraySize }, () => [])

    for (const [row, col] of cells) {
        const cellIndex: CellIndex = getCellIndex(row, col)
        const areas: Area[] = cellAreasMap[cellIndex]!
        const peerSet: Set<number> = new Set()

        for (const area of areas) {
            const areaCells: Cell[] = getCellsInArea(area)
            for (const [peerRow, peerCol] of areaCells) {
                const peerIndex: number = getCellIndex(peerRow, peerCol)
                if (peerIndex !== cellIndex) {
                    peerSet.add(peerIndex)
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


export function generate(boardState: BoardGenerationState): BoardGenerationState {
    let cluster: Cell[]

    switch (boardState.type) {
        case 'PlacingNumbers':
            if (boardState.state.remainingClusters.length == 0) {
                return {
                    type: 'RemovingGivens',
                    state: boardState.state,
                }
            }

            cluster = boardState.state.remainingClusters[0]!
            boardState.state.remainingClusters = boardState.state.remainingClusters.slice(1)

            try {
                placeNumbersInCluster(cluster, boardState.state)
            } catch (e) {
                return {
                    type: 'Failed',
                    reason: e instanceof Error ? e.message : 'Unknown error',
                }
            }

            if (boardState.state.remainingClusters.length == 0) {
                // TODO: Sort by unlock order?
                boardState.state.givens = boardState.state.solution.slice()
                boardState.state.remainingClusters = boardState.state.allClusters

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
                return clusterStateToCompleted(boardState.state)
            }

            cluster = boardState.state.remainingClusters[0]!
            boardState.state.remainingClusters = boardState.state.remainingClusters.slice(1)

            removeGivenNumbers(cluster, boardState.state)


            if (boardState.state.remainingClusters.length == 0) {
                boardState.state.remainingClusters = boardState.state.allClusters

                return clusterStateToCompleted(boardState.state)
            } else {
                // return {
                //     type: 'Failed',
                //     reason: 'Generation logic not implemented',
                // }
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
    const possibilitiesMap: PossibilitiesMap = new Int32Array(totalArraySize)

    for (const boardArea of state.puzzleAreas.boards) {
        const boardCells: Cell[] = getCellsInArea(boardArea)
        for (const [row, col] of boardCells) {
            const cellIndex: CellIndex = getCellIndex(row, col)
            possibilitiesMap[cellIndex] = (1 << state.blockSize) - 1
        }
    }

    for (const [row, col] of state.allCells) {
        const cellIndex: CellIndex = getCellIndex(row, col)
        propagateSolution(possibilitiesMap, state.solution, state.peerMap, cellIndex)
    }

    const clusterCells: Set<Cell> = getClusterCells(state.blockSize, cluster)

    const result: boolean = tryPlacingNumbers(state.solution, possibilitiesMap, clusterCells, state)

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


function getClusterCells(blockSize: number, cluster: Cell[]): Set<Cell> {
    const clusterCells: Set<Cell> = new Set()

    for (const [startRow, startCol] of cluster) {
        for (let r = 0; r < blockSize; r++) {
            for (let c = 0; c < blockSize; c++) {
                clusterCells.add([startRow + r, startCol + c])
            }
        }
    }

    return clusterCells
}


function tryPlacingNumbers(
    solution: Int32Array,
    possibilitiesMap: PossibilitiesMap,
    clusterCells: Set<Cell>,
    state: ClusterGenerationState
): boolean {
    const candidateCells: Cell[] = []
    for (const cell of clusterCells) {
        const [row, col] = cell
        const cellIndex: CellIndex = getCellIndex(row, col)
        if (solution[cellIndex]! === 0) {
            candidateCells.push(cell)
        }
    }

    const bestCell: Cell | null = findBestCell(candidateCells, possibilitiesMap)

    if (bestCell === null) {
        return true
    }

    const bestCellIndex: number = getCellIndex(bestCell[0], bestCell[1])
    const bestCellPossibilities: number = possibilitiesMap[bestCellIndex]!
    const possibleNumbers: number[] = numbersFromBitmask(bestCellPossibilities)

    // Shuffle possible numbers
    for (let i = possibleNumbers.length - 1; i > 0; i--) {
        const j = Math.floor(state.random() * (i + 1))
        ;[possibleNumbers[i], possibleNumbers[j]] = [possibleNumbers[j]!, possibleNumbers[i]!]
    }

    for (const number of possibleNumbers) {
        solution[bestCellIndex] = number

        const changes: number[] | null = propagatePossibilities(
            state.peerMap[bestCellIndex]!,
            number,
            possibilitiesMap
        )

        if (changes === null) {
            solution[bestCellIndex] = 0

            continue
        }

        const result: boolean = tryPlacingNumbers(solution, possibilitiesMap, clusterCells, state)

        if (result) {
            return true
        } else {
            revertPossibilities(changes, number, possibilitiesMap)
            solution[bestCellIndex] = 0
        }
    }

    return false
}


function findBestCell(candidateCells: Cell[], possibilitiesMap: PossibilitiesMap): Cell | null {
    let bestCell: Cell | null = null
    let bestCount: number = Infinity

    for (let [row, col] of candidateCells) {
        const cellIndex: CellIndex = getCellIndex(row, col)
        const possibilities = possibilitiesMap[cellIndex]!
        const count = countSetBits(possibilities)

        if (count < bestCount) {
            bestCount = count
            bestCell = [row, col]
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

    const clusterCells: Set<Cell> = getClusterCells(state.blockSize, cluster)
    const clusterCellIndices: number[] = []

    for (const [row, col] of clusterCells) {
        const cellIndex: CellIndex = getCellIndex(row, col)
        clusterCellIndices.push(cellIndex)
    }

    // TODO: Won't work because it's Set<Cell>, should use the indices instead
    const cellsToRemoveFrom: Set<Cell> = intersectSets(
        clusterCells,
        state.cellsToRemoveGivensFrom
    )
    state.cellsToRemoveGivensFrom = diffSets(
        state.cellsToRemoveGivensFrom,
        cellsToRemoveFrom
    )
    const indicesToRemove: number[] = []

    for (const [row, col] of cellsToRemoveFrom) {
        const cellIndex: CellIndex = getCellIndex(row, col)
        if (state.givens[cellIndex]! !== 0) {
            indicesToRemove.push(cellIndex)
        }
    }

    for (const cellIndex of indicesToRemove) {
        const originalValue: number = state.solution[cellIndex]!
        state.givens[cellIndex] = 0
        solverBuffer.set(state.givens)

        const possibilitiesMap: PossibilitiesMap = new Int32Array(totalArraySize)
        // TODO: build possibilities map without the number being removed

        for (const [row, col] of clusterCells) {
            const idx: number = getCellIndex(row, col)
            propagateSolution(possibilitiesMap, solverBuffer, state.peerMap, idx)
        }

        const hasOtherSolution: boolean = tryPlacingNumbers(
            solverBuffer,
            possibilitiesMap,
            clusterCells,
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
        // givens: solution, // TODO: temporarily return full solution as givens
        puzzleAreas: encodedPuzzleAreas,
        solution: solution,
        unlockCount: 0,
        unlockOrder: [],
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
    return (row - 1) * 300 + (col - 1)
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


// Mulberry32 random number generator
function createRandomGenerator(seed: number): () => number {
    return function() {
        let t = seed += 0x6D2B79F5
        t = Math.imul(t ^ (t >>> 15), t | 1)
        t ^= t + Math.imul(t ^ (t >>> 7), t | 61)
        return ((t ^ (t >>> 14)) >>> 0) / 4294967296
    }
}
