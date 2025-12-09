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
    | Completed
    | Failed


interface PlacingNumbers {
    type: "PlacingNumbers"
    state: ClusterGenerationState
}


interface RemovingGivens {
    type: "RemovingGivens"
    state: ClusterGenerationState
}


interface Completed {
    type: "Completed"
}


interface Failed {
    type: "Failed"
    reason: string
}


interface ClusterGenerationState {
    allCells: Set<Cell>
    allClusters: Cell[][]
    blockSize: number
    givens: Int32Array
    puzzleAreas: PuzzleAreas
    peerMap: PeerMap
    random: () => number
    remainingClusters: Cell[][]
    solution: Int32Array
}


let maxWidth: number = 300
let totalArraySize: number = maxWidth * maxWidth


export function initGeneration(args: GenerateArgs): BoardGenerationState {
    if (args.numberOfBoards < 1 || args.numberOfBoards > 100) {
        return {
            type: "Failed",
            reason: "Number of boards must be between 1 and 100",
        }
    }

    if (![4, 6, 8, 9, 12, 16].includes(args.blockSize)) {
        return {
            type: "Failed",
            reason: "Unsupported block size",
        }
    }

    let positions: Cell[] = positionBoards(args.blockSize, args.numberOfBoards)

    let boardPuzzleAreas: PuzzleAreas[] = []
    for (let [startRow, startCol] of positions) {
        let puzzleAreas = buildPuzzleAreas(args.blockSize, startRow, startCol)
        boardPuzzleAreas.push(puzzleAreas)
    }
    let puzzleAreas: PuzzleAreas = joinPuzzleAreas(boardPuzzleAreas)
    let areas: Area[] = [...puzzleAreas.blocks, ...puzzleAreas.rows, ...puzzleAreas.cols]
    let cellAreasMap: Area[][] = buildCellAreasMap(areas)
    let cells: Set<Cell> = new Set()

    for (let board of puzzleAreas.boards) {
        let boardCells: Cell[] = getCellsInArea(board)
        for (let cell of boardCells) {
            cells.add(cell)
        }
    }

    let random: () => number = createRandomGenerator(args.seed)
    let peerMap: PeerMap = buildPeerMap(cells, cellAreasMap)
    let clusters: Cell[][] = buildClusters(positions, random)

    return {
        type: "PlacingNumbers",
        state: {
            allCells: cells,
            allClusters: clusters,
            blockSize: args.blockSize,
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

    let blocks: Area[] = []
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

    let rows: Area[] = []
    for (let r = 0; r < blockRows * blockCols; r++) {
        rows.push({
            startRow: startRow + r,
            startCol: startCol,
            endRow: startRow + r,
            endCol: startCol + blockRows * blockCols - 1,
        })
    }

    let cols: Area[] = []
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
    let boards: Area[] = []
    let blocks: Area[] = []
    let rows: Area[] = []
    let cols: Area[] = []

    for (let puzzleAreas of puzzleAreasList) {
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
    let cellAreasMap: Area[][] = Array.from({ length: totalArraySize }, () => [])

    for (let area of areas) {
        let cells: Cell[] = getCellsInArea(area)
        for (let [row, col] of cells) {
            let cellIndex: number = getCellIndex(row, col)
            cellAreasMap[cellIndex]!.push(area)
        }
    }

    return cellAreasMap
}


function getCellsInArea(area: Area): Cell[] {
    let cells: Cell[] = []
    for (let r = area.startRow; r <= area.endRow; r++) {
        for (let c = area.startCol; c <= area.endCol; c++) {
            cells.push([r, c])
        }
    }

    return cells
}


function buildPeerMap(cells: Set<Cell>, cellAreasMap: Area[][]): PeerMap {
    let peerMap: PeerMap = Array.from({ length: totalArraySize }, () => [])

    for (let [row, col] of cells) {
        let cellIndex: number = getCellIndex(row, col)
        let areas: Area[] = cellAreasMap[cellIndex]!
        let peerSet: Set<number> = new Set()

        for (let area of areas) {
            let areaCells: Cell[] = getCellsInArea(area)
            for (let [peerRow, peerCol] of areaCells) {
                let peerIndex: number = getCellIndex(peerRow, peerCol)
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
    switch (boardState.type) {
        case "PlacingNumbers":
            if (boardState.state.remainingClusters.length == 0) {
                return {
                    type: "RemovingGivens",
                    state: boardState.state,
                }
            }

            let cluster: Cell[] = boardState.state.remainingClusters[0]!
            boardState.state.remainingClusters = boardState.state.remainingClusters.slice(1)

            try {
                placeNumbersInCluster(cluster, boardState.state)
            } catch (e) {
                return {
                    type: "Failed",
                    reason: e instanceof Error ? e.message : "Unknown error",
                }
            }

            if (boardState.state.remainingClusters.length == 0) {
                return {
                    type: "RemovingGivens",
                    state: boardState.state,
                }
            } else {
                return {
                    type: "PlacingNumbers",
                    state: boardState.state,
                }
            }

        case "RemovingGivens":
            return {
                type: "Failed",
                reason: "Generation logic not implemented",
            }

        case "Completed":
            return boardState

        case "Failed":
            return boardState
    }
}


function placeNumbersInCluster(cluster: Cell[], state: ClusterGenerationState): void {
    let possibilitiesMap: PossibilitiesMap = new Int32Array(totalArraySize)

    for (let boardArea of state.puzzleAreas.boards) {
        let boardCells: Cell[] = getCellsInArea(boardArea)
        for (let [row, col] of boardCells) {
            let cellIndex: number = getCellIndex(row, col)
            possibilitiesMap[cellIndex] = (1 << state.blockSize) - 1
        }
    }

    for (let [row, col] of state.allCells) {
        let cellIndex: number = getCellIndex(row, col)
        propagateSolution(state.solution, state.peerMap, cellIndex)
    }

    // run tryPlaceNumbers
}


//function tryPlaceNumbers(


function findBestCell(candidateCells: Cell[], possibilitiesMap: PossibilitiesMap): Cell | null {
    let bestCell: Cell | null = null
    let bestCount: number = Infinity

    for (let [row, col] of candidateCells) {
        let cellIndex: number = getCellIndex(row, col)
        let possibilities = possibilitiesMap[cellIndex]!
        let count = countSetBits(possibilities)

        if (count < bestCount) {
            bestCount = count
            bestCell = [row, col]
        }
    }

    return bestCell
}


function countSetBits(n: number): number {
    let count = 0
    while (n) {
        count += n & 1
        n >>= 1
    }
    return count
}


function propagateSolution(solution: Int32Array, peerMap: PeerMap, cellIndex: number): void {
    const number = solution[cellIndex]!

    if (number === 0) {
        return
    }

    const peers: Peers = peerMap[cellIndex]!

    const changes: number[] | null = propagatePossibilities(peers, number, solution)

    if (changes === null) {
        throw new Error("Contradiction encountered during propagation")
    }
}


function propagatePossibilities(
    peers: number[],
    number: number,
    possibilitiesMap: PossibilitiesMap,
): number[] | null {
    const changes: number[] = []
    const removeMask = ~(1 << (number - 1))

    for (let peerIndex of peers) {
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

    for (let peerIndex of changes) {
        possibilitiesMap[peerIndex] = possibilitiesMap[peerIndex]! | addMask
    }
}


function getCellIndex(row: number, col: number): number {
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
            throw new Error("Unsupported block size")
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
            throw new Error("Unsupported block size")
    }
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
