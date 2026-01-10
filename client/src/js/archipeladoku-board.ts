interface BoardData {
    cells: [number, number, CellValue][]
    blocks: Area[]
    boards: Area[]
    blockSize: number
    selectedCell: { row: number; col: number } | null
    numberMap: [number, string][]
    errors: CellError[]
    colorScheme: string
}


interface CellError {
    row: number
    col: number
    details: CellErrorDetails
}


type CellErrorDetails = CellErrorNumber | CellErrorCandidates


interface CellErrorNumber {
    type: 'number'
    cells: [number, number][]
}


interface CellErrorCandidates {
    type: 'candidates'
    errors: CellErrorCandidatesError[]
}


interface CellErrorCandidatesError {
    number: number
    cells: [number, number][]
}


interface CellValueEmpty {
    type: 'empty'
    dimmed: boolean
}


interface CellValueHidden {
    type: 'hidden'
    dimmed: boolean
}


interface CellValueGiven {
    type: 'given'
    number: number
    dimmed: boolean
}


interface CellValueSingle {
    type: 'single'
    number: number
    dimmed: boolean
}


interface CellValueCandidates {
    type: 'candidates'
    numbers: number[]
    dimmed: boolean
    dimmedNumbers: number[]
}


type CellValue
    = CellValueEmpty
    | CellValueHidden
    | CellValueGiven
    | CellValueSingle
    | CellValueCandidates


const colors = {
    mainBg: { light: 'hsl(120 20 90)', dark: 'hsl(120 20 10)' },
    grid: { light: 'hsl(0 0 50)', dark: 'hsl(0 0 50)' },
    border: { light: 'hsl(0 0 40)', dark: 'hsl(0 0 70)' },
    cellBg: { light: 'hsl(0 0 90)', dark: 'hsl(0 0 10)' },
    cellBgHover: { light: 'hsl(0 0 85)', dark: 'hsl(0 0 20)' },
    cellBgActive: { light: 'hsl(0 0 80)', dark: 'hsl(0 0 5)' },
    cellBgTransparent: { light: 'hsl(0 0 90 / 0)', dark: 'hsl(0 0 10 / 0)' },
    cellHidden: { light: 'hsl(0 0 80)', dark: 'hsl(0 0 40)' },
    cellHiddenHover: { light: 'hsl(0 0 75)', dark: 'hsl(0 0 50)' },
    cellHiddenActive: { light: 'hsl(0 0 70)', dark: 'hsl(0 0 35)' },
    cellDimmed: { light: 'hsl(0 0 0 / 0.3)', dark: 'hsl(0 0 0 / 0.5)' },
    crack: { light: 'hsl(0 0 20)', dark: 'hsl(0 0 20)' },
    selection: { light: 'hsl(0 0 0)', dark: 'hsl(0 0 100)' },
    shadow: { light: 'hsl(0 0 0 / 0.2)', dark: 'hsl(0 0 0 / 0.8)' },
    text: { light: 'hsl(0 0 5)', dark: 'hsl(0 0 95)' },
    textError: { light: 'hsl(0 80 40)', dark: 'hsl(0 80 70)' },
}


const hues = {
    1: 0,
    2: 22,
    3: 45,
    4: 67,
    5: 90,
    6: 112,
    7: 135,
    8: 157,
    9: 180,
    10: 202,
    11: 225,
    12: 247,
    13: 270,
    14: 292,
    15: 315,
    16: 337,
}


const cellHue = {
    4: {
        1: hues[1],
        2: hues[4],
        3: hues[6],
        4: hues[11],
    },
    6: {
        1: hues[1],
        2: hues[3],
        3: hues[6],
        4: hues[9],
        5: hues[12],
        6: hues[15],
    },
    8: {
        1: hues[1],
        2: hues[2],
        3: hues[4],
        4: hues[6],
        5: hues[9],
        6: hues[11],
        7: hues[13],
        8: hues[15],
    },
    9: {
        1: hues[1],
        2: hues[2],
        3: hues[4],
        4: hues[5],
        5: hues[8],
        6: hues[10],
        7: hues[11],
        8: hues[13],
        9: hues[15],
    },
    12: {
        1: hues[1],
        2: hues[2],
        3: hues[4],
        4: hues[5],
        5: hues[7],
        6: hues[9],
        7: hues[10],
        8: hues[11],
        9: hues[12],
        10: hues[13],
        11: hues[14],
        12: hues[16],
    },
    16: {
        1: hues[1],
        2: hues[2],
        3: hues[3],
        4: hues[4],
        5: hues[5],
        6: hues[6],
        7: hues[7],
        8: hues[8],
        9: hues[9],
        10: hues[10],
        11: hues[11],
        12: hues[12],
        13: hues[13],
        14: hues[14],
        15: hues[15],
        16: hues[16],
    },
}


const cellLightness = {
    light: {
        initial: 80,
        lighter: 0,
        darker: 0,
        from: 5,
        to: -5,
        hover: -10,
        active: -15,
    },
    dark: {
        initial: 25,
        lighter: 5,
        darker: -5,
        from: 10,
        to: 0,
        hover: 10,
        active: -5,
    },
}


const candidateFontSizeMultiplier = {
    4: 0.45,
    6: 0.3,
    8: 0.3,
    9: 0.3,
    12: 0.25,
    16: 0.25,
}


type ColorScheme = 'light' | 'dark'


type CellState = 'regular' | 'hover' | 'active'


type NumberState = 'regular' | 'error'


const numberOffset = 0.05


interface Area {
    startRow: number
    startCol: number
    endRow: number
    endCol: number
}


interface GameAnimation {
    id: string
    startTime: number
    duration?: number
    onUpdate: (progress: number) => void
    onComplete?: () => void
}


const easing = {
    easeInOutQuad: (t: number): number => {
        return t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t
    },
}


const crackPaths = [
    [[0.50, 0.00], [0.20, 0.00], [0.35, 0.20], [0.30, 0.35], [0.50, 0.50]],
    [[1.00, 0.00], [0.70, 0.00], [0.60, 0.20], [0.65, 0.40], [0.50, 0.50]],
    [[1.00, 1.00], [1, 0.45], [0.85, 0.35], [0.65, 0.60], [0.50, 0.50]],
    [[0.50, 1.00], [0.80, 1], [0.75, 0.80], [0.55, 0.65], [0.50, 0.50]],
    [[0.00, 1.00], [0.25, 1], [0.15, 0.85], [0.40, 0.75], [0.50, 0.50]],
    [[0.00, 0.00], [0.00, 0.55], [0.15, 0.45], [0.35, 0.55], [0.50, 0.50]],
]


const spriteOffsets = {
    base: {
        given: { x: 0, y: 0 },
        givenNumber: { x: 0, y: 3 },
        userValue: { x: 0, y: 5 },
        userValueNumber: { x: 0, y: 8 },
        candidate: { x: 0, y: 10 },
        candidateNumber: { x: 0, y: 13 },
        background: { x: 0, y: 15 },
        hidden: { x: 1, y: 15 },
        sparkle: { x: 2, y: 15 },
        crack: { x: 3, y: 15 },
        shard: { x: 4, y: 15 },
        selection: { x: 6, y: 15 },
    },
    state: {
        none: { x: 0, y: 0 },
        hover: { x: 0, y: 1 },
        active: { x: 0, y: 2 },
        error: { x: 0, y: 1 },
        1: { x: 0, y: 0 },
        2: { x: 0, y: 1 },
        3: { x: 0, y: 2 },
        4: { x: 1, y: 0 },
        5: { x: 1, y: 1 },
        6: { x: 1, y: 2 },
    },
} as const


function getSpriteX(
    spriteSize: number,
    baseKey: keyof typeof spriteOffsets.base,
    stateKey?: keyof typeof spriteOffsets.state,
) {
    const baseOffset = spriteOffsets.base[baseKey].x
    const stateOffset = stateKey ? spriteOffsets.state[stateKey].x : 0

    return (baseOffset + stateOffset) * spriteSize
}


function getSpriteY(
    spriteSize: number,
    baseKey: keyof typeof spriteOffsets.base,
    stateKey?: keyof typeof spriteOffsets.state,
) {
    const baseOffset = spriteOffsets.base[baseKey].y
    const stateOffset = stateKey ? spriteOffsets.state[stateKey].y : 0

    return (baseOffset + stateOffset) * spriteSize
}


const rowLabelAlphabet = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M',
    'N', 'P', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
];
const rowLabelBase = rowLabelAlphabet.length;


function getCellColor(blockSize: number, value: number, scheme: ColorScheme, state: CellState = 'regular'): [string, string] {
    const sizeHues = cellHue[blockSize as keyof typeof cellHue]
    const hue = sizeHues[value as keyof typeof sizeHues]
    const saturation = 60
    const lightnessSpec = cellLightness[scheme as keyof typeof cellLightness]
    let lightness: number = lightnessSpec.initial

    if (state === 'hover') {
        lightness += lightnessSpec.hover
    } else if (state === 'active') {
        lightness += lightnessSpec.active
    }

    if (hue >= 30 && hue < 180) {
        lightness += lightnessSpec.darker
    } else if (hue >= 200 && hue < 280) {
        lightness += lightnessSpec.lighter
    }

    return [
        `hsl(${hue} ${saturation} ${lightness + lightnessSpec.from})`,
        `hsl(${hue} ${saturation} ${lightness + lightnessSpec.to})`,
    ]
}


function getSystemColorScheme(): ColorScheme {
    return window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches
        ? 'dark'
        : 'light'
}


function mapRange(value: number, start: number, end: number): number {
    const clamped = Math.max(start, Math.min(end, value))

    return (clamped - start) / (end - start)
}


function compareMaps<K, V>(map1: Map<K, V>, map2: Map<K, V>): boolean {
    if (map1 === map2) {
        return true
    }

    if (map1.size !== map2.size) {
        return false
    }

    for (const [key, value] of map1) {
        if (!map2.has(key) || map2.get(key) !== value) {
            return false
        }
    }

    return true
}


class ArchipeladokuBoard extends HTMLElement {
    canvas: HTMLCanvasElement
    ctx: CanvasRenderingContext2D
    spriteCanvas: HTMLCanvasElement
    spriteCtx: CanvasRenderingContext2D
    spriteScale: number = 1
    renderedSpriteScale: number = 0
    colHeaderCanvas: HTMLCanvasElement
    colHeaderCtx: CanvasRenderingContext2D
    rowHeaderCanvas: HTMLCanvasElement
    rowHeaderCtx: CanvasRenderingContext2D
    headerCanvasPadding: number = 16
    blockSize: number = 9
    blocks: Area[] = []
    boards: Area[] = []
    cells: Map<string, CellValue> = new Map()
    boardRows: number = 0
    boardCols: number = 0
    cellErrors: Map<string, [number, number][]> = new Map()
    colorScheme: ColorScheme = 'dark'
    viewport: { x: number; y: number; scale: number }
    renderRequested: boolean = false
    hoveredCell: { row: number, col: number } | null = null
    activeCell: { row: number, col: number } | null = null
    selectedCell: { row: number; col: number } | null = null
    cellSize: number = 48
    cellGap: number = 1
    borderWidth: number = 3

    activePointers: Map<number, PointerEvent> = new Map()
    lastPinchDistance: number = 0
    isDragging: boolean = false
    isPinching: boolean = false
    dragStart: { x: number; y: number } = { x: 0, y: 0 }
    viewStart: { x: number; y: number } = { x: 0, y: 0 }
    minScale: number = 0.1
    maxScale: number = 4
    velocity: { x: number; y: number } = { x: 0, y: 0 }
    lastPointerTime: number = 0
    lastPointerPosition: { x: number; y: number } = { x: 0, y: 0 }
    friction: number = 0.97
    flingSpeed: number = 4
    animations: Map<string, GameAnimation> = new Map()
    isAnimating: boolean = false
    cellShineEffects: Map<string, any> = new Map()
    cellShatterEffects: Map<string, any> = new Map()
    numberMap: Map<number, string> = new Map()


    constructor() {
        super()
        this.attachShadow({ mode: 'open' })
        this.canvas = document.createElement('canvas')
        this.ctx = this.canvas.getContext('2d', { alpha: false } )!
        this.spriteCanvas = document.createElement('canvas')
        this.spriteCtx = this.spriteCanvas.getContext('2d', { alpha: true } )!
        this.colHeaderCanvas = document.createElement('canvas')
        this.colHeaderCtx = this.colHeaderCanvas.getContext('2d', { alpha: true } )!
        this.rowHeaderCanvas = document.createElement('canvas')
        this.rowHeaderCtx = this.rowHeaderCanvas.getContext('2d', { alpha: true } )!
        this.viewport = { x: 60, y: 60, scale: 1 }
    }


    connectedCallback() {
        this.shadowRoot!.appendChild(this.canvas)
        this.resize()
        window.addEventListener('resize', () => this.resize())
        this.render()
        requestAnimationFrame(() => {
            this.resize()
        })

        this.canvas.addEventListener('pointerdown', (e: PointerEvent) => this.handlePointerDown(e))
        window.addEventListener('pointermove', (e: PointerEvent) => this.handlePointerMove(e))
        window.addEventListener('pointerup', (e: PointerEvent) => this.handlePointerUp(e))
        window.addEventListener('pointercancel', (e: PointerEvent) => this.handlePointerUp(e))
        this.canvas.addEventListener('wheel', (e: WheelEvent) => this.handleWheel(e), { passive: false })
        this.canvas.style.touchAction = 'none'

        this.colorScheme = getSystemColorScheme()

        // this.shadowRoot!.appendChild(this.spriteCanvas)
        // this.spriteCanvas.style.position = 'absolute'
        // this.spriteCanvas.style.left = '4px'
        // this.spriteCanvas.style.bottom = '4px'
        // this.spriteCanvas.style.pointerEvents = 'none'

        // this.shadowRoot!.appendChild(this.colHeaderCanvas)
        // this.colHeaderCanvas.style.position = 'absolute'
        // this.colHeaderCanvas.style.left = '4px'
        // this.colHeaderCanvas.style.top = '4px'
        // this.colHeaderCanvas.style.pointerEvents = 'none'
    }


    set data(value: BoardData) {
        this.boardRows = Math.max(...value.cells.map(c => c[0]))
        this.boardCols = Math.max(...value.cells.map(c => c[1]))
        this.blocks = value.blocks
        this.boards = value.boards
        this.selectedCell = value.selectedCell
        this.blockSize = value.blockSize

        this.cells.clear()
        for (const [row, col, cellValue] of value.cells) {
            this.cells.set(`${row},${col}`, cellValue)
        }

        const numberMap = new Map<number, string>()
        for (const [num, str] of value.numberMap) {
            numberMap.set(num, str)
        }

        if (!compareMaps(this.numberMap, numberMap)) {
            this.numberMap = numberMap
            this.renderedSpriteScale = 0
        }

        this.cellErrors.clear()
        for (const error of value.errors) {
            if (error.details.type === 'number') {
                this.cellErrors.set(`${error.row},${error.col}`, error.details.cells)
            } else if (error.details.type === 'candidates') {
                for (const candidateError of error.details.errors) {
                    this.cellErrors.set(
                        `${error.row},${error.col},${candidateError.number}`,
                        candidateError.cells
                    )
                }
            }
        }

        const prevColorScheme = this.colorScheme
        if (value.colorScheme === 'light' || value.colorScheme === 'dark') {
            this.colorScheme = value.colorScheme
        } else {
            this.colorScheme = getSystemColorScheme()
        }

        if (this.colorScheme !== prevColorScheme) {
            this.renderedSpriteScale = 0
        }

        this.requestRender()
    }


    getGridCoordinates(x: number, y: number): { row: number; col: number } | null {
        const worldX = (x - this.viewport.x) / this.viewport.scale + 1
        const worldY = (y - this.viewport.y) / this.viewport.scale + 1

        const cellSizeWithGap = this.cellSize + this.cellGap
        const maxX = this.boardCols * cellSizeWithGap
        const maxY = this.boardRows * cellSizeWithGap

        if (worldX < 0 || worldY < 0 || worldX >= maxX || worldY >= maxY) {
            return null
        }

        const col = Math.floor(worldX / cellSizeWithGap) + 1
        const row = Math.floor(worldY / cellSizeWithGap) + 1

        return { row, col }
    }


    getGridCoordinatesFromEvent(e: MouseEvent): { row: number; col: number } | null {
        const rect = this.canvas.getBoundingClientRect()
        const mouseX = e.clientX - rect.left
        const mouseY = e.clientY - rect.top

        return this.getGridCoordinates(mouseX, mouseY)
    }


    handlePointerDown(e: PointerEvent) {
        this.activePointers.set(e.pointerId, e)

        if (this.animations.has('fling')) {
            this.animations.delete('fling')
            this.velocity = { x: 0, y: 0 }
        }

        if (this.activePointers.size === 1) {
            this.isDragging = false
            this.dragStart = { x: e.clientX, y: e.clientY }
            this.viewStart = { x: this.viewport.x, y: this.viewport.y }
            this.activeCell = this.getGridCoordinatesFromEvent(e)
        }

        this.requestRender()
    }


    handlePointerMove = (e: PointerEvent) => {
        if (!this.activePointers.has(e.pointerId)) {
            const coords = this.getGridCoordinatesFromEvent(e)

            if (coords) {
                if (!this.hoveredCell
                    || this.hoveredCell.row !== coords.row
                    || this.hoveredCell.col !== coords.col
                ) {
                    this.hoveredCell = coords
                    this.requestRender()
                }
            } else {
                if (this.hoveredCell) {
                    this.hoveredCell = null
                    this.requestRender()
                }
            }

            return
        }

        this.activePointers.set(e.pointerId, e)

        const pointers = Array.from(this.activePointers.values())

        if (pointers.length === 1 && !this.isPinching) {
            const pointer = pointers[0]!
            const deltaX = pointer.clientX - this.dragStart.x
            const deltaY = pointer.clientY - this.dragStart.y
            const dragThreshold = 5

            if (!this.isDragging
                && (Math.abs(deltaX) > dragThreshold || Math.abs(deltaY) > dragThreshold)
            ) {
                this.isDragging = true
                this.activeCell = null
            }

            if (this.isDragging) {
                const now = performance.now()
                const timeDelta = now - this.lastPointerTime

                if (timeDelta > 0) {
                    this.velocity.x = (pointer.clientX - this.lastPointerPosition.x) / timeDelta
                    this.velocity.y = (pointer.clientY - this.lastPointerPosition.y) / timeDelta
                }

                this.lastPointerTime = now
                this.lastPointerPosition = { x: pointer.clientX, y: pointer.clientY }

                this.viewport.x = this.viewStart.x + deltaX
                this.viewport.y = this.viewStart.y + deltaY

                this.constrainViewport()

                this.requestRender()
            }
        } else if (pointers.length === 2) {
            this.isPinching = true
            this.handlePinch(pointers[0]!, pointers[1]!)
        }
    }


    handlePointerUp = (e: PointerEvent) => {
        if (this.activePointers.size === 1 && !this.isDragging && this.activeCell) {
            const coords = this.getGridCoordinatesFromEvent(e)

            if (coords
                && coords.row === this.activeCell.row
                && coords.col === this.activeCell.col
            ) {
                this.dispatchEvent(new CustomEvent('cellselected', {
                    detail: { ...this.activeCell },
                    bubbles: true,
                    composed: true,
                }))
            }
        }

        if (this.isDragging && this.activePointers.size === 1) {
            this.startFling()
        }

        this.activePointers.delete(e.pointerId)

        if (this.activePointers.size < 2) {
            this.lastPinchDistance = 0
        }

        if (this.activePointers.size === 0) {
            this.isPinching = false
        }

        this.isDragging = false
        this.activeCell = null
        this.requestRender()
    }


    startFling() {
        this.addAnimation({
            'id': 'fling',
            'startTime': performance.now(),
            'onUpdate': (progress: number) => {
                this.viewport.x += this.velocity.x * this.flingSpeed
                this.viewport.y += this.velocity.y * this.flingSpeed

                this.constrainViewport()

                this.velocity.x *= this.friction
                this.velocity.y *= this.friction

                if (Math.abs(this.velocity.x) < 0.01 && Math.abs(this.velocity.y) < 0.01) {
                    this.animations.delete('fling')
                }
            }
        })
    }


    handlePinch(e1: PointerEvent, e2: PointerEvent) {
        const rect = this.canvas.getBoundingClientRect()
        const centerX = (e1.clientX + e2.clientX) / 2 - rect.left
        const centerY = (e1.clientY + e2.clientY) / 2 - rect.top

        const deltaX = e1.clientX - e2.clientX
        const deltaY = e1.clientY - e2.clientY
        const distance = Math.hypot(deltaX, deltaY)

        if (this.lastPinchDistance > 0) {
            const zoomFactor = distance / this.lastPinchDistance
            this.zoom(centerX, centerY, zoomFactor)
        }

        this.lastPinchDistance = distance
    }


    handleWheel(e: WheelEvent) {
        e.preventDefault()

        const rect = this.canvas.getBoundingClientRect()
        const x = e.clientX - rect.left
        const y = e.clientY - rect.top

        const delta = -e.deltaY
        const zoomFactor = Math.exp(delta * 0.0015)

        this.zoom(x, y, zoomFactor)
    }


    zoom(centerX: number, centerY: number, zoomFactor: number) {
        const newScale = Math.min(Math.max(this.viewport.scale * zoomFactor, this.minScale), this.maxScale)

        if (newScale === this.viewport.scale) {
            return
        }

        const worldX = (centerX - this.viewport.x) / this.viewport.scale
        const worldY = (centerY - this.viewport.y) / this.viewport.scale

        this.viewport.scale = newScale
        this.viewport.x = centerX - worldX * this.viewport.scale
        this.viewport.y = centerY - worldY * this.viewport.scale

        this.constrainViewport()

        this.requestRender()
    }


    zoomCenter(zoomFactor: number) {
        const width = this.canvas.width / window.devicePixelRatio
        const height = this.canvas.height / window.devicePixelRatio
        this.zoom(width / 2, height / 2, zoomFactor)
    }


    moveCellIntoView(row: number, col: number) {
        const cellSizeWithGap = this.cellSize + this.cellGap
        const padding = cellSizeWithGap + this.cellGap

        const cellX = (col - 1) * cellSizeWithGap
        const cellY = (row - 1) * cellSizeWithGap

        const width = this.canvas.width / window.devicePixelRatio
        const height = this.canvas.height / window.devicePixelRatio

        const viewLeft = -this.viewport.x / this.viewport.scale
        const viewTop = -this.viewport.y / this.viewport.scale
        const viewRight = (width - this.viewport.x) / this.viewport.scale
        const viewBottom = (height - this.viewport.y) / this.viewport.scale

        if (cellX < viewLeft + padding * 2) {
            this.viewport.x = -(cellX - padding * 2) * this.viewport.scale
        } else if (cellX + this.cellSize > viewRight - padding) {
            this.viewport.x = -(cellX + this.cellSize + padding - width / this.viewport.scale) * this.viewport.scale
        }

        if (cellY < viewTop + padding * 2) {
            this.viewport.y = -(cellY - padding * 2) * this.viewport.scale
        } else if (cellY + this.cellSize > viewBottom - padding) {
            this.viewport.y = -(cellY + this.cellSize + padding - height / this.viewport.scale) * this.viewport.scale
        }

        this.constrainViewport()
        this.requestRender()
    }


    centerOnCell(row: number, col: number) {
        const cellSizeWithGap = this.cellSize + this.cellGap

        const cellX = (col - 1) * cellSizeWithGap + this.cellSize / 2
        const cellY = (row - 1) * cellSizeWithGap + this.cellSize / 2

        const width = this.canvas.width / window.devicePixelRatio
        const height = this.canvas.height / window.devicePixelRatio

        this.viewport.x = width / 2 - cellX * this.viewport.scale
        this.viewport.y = height / 2 - cellY * this.viewport.scale

        this.constrainViewport()
        this.requestRender()
    }


    setViewport(x: number, y: number, scale: number) {
        this.viewport.x = x
        this.viewport.y = y
        this.viewport.scale = scale
        this.constrainViewport()

        this.requestRender()
    }


    constrainViewport() {
        const width = this.canvas.width / window.devicePixelRatio
        const height = this.canvas.height / window.devicePixelRatio

        const cellSizeWithGap = this.cellSize + this.cellGap
        const padding = 120
        const worldWidth = this.boardCols * cellSizeWithGap * this.viewport.scale
        const worldHeight = this.boardRows * cellSizeWithGap * this.viewport.scale

        const minX = padding - worldWidth
        const minY = padding - worldHeight
        const maxX = width - padding
        const maxY = height - padding

        this.viewport.x = Math.min(Math.max(this.viewport.x, minX), maxX)
        this.viewport.y = Math.min(Math.max(this.viewport.y, minY), maxY)
    }


    resize() {
        const dpr = window.devicePixelRatio || 1
        const width = this.clientWidth
        const height = this.clientHeight

        this.canvas.width = width * dpr
        this.canvas.height = height * dpr
        this.canvas.style.width = `${width}px`
        this.canvas.style.height = `${height}px`

        this.ctx.setTransform(dpr, 0, 0, dpr, 0, 0)

        this.requestRender()
    }


    animateCells(cells: [number, number][], type: 'shine' | 'shatter') {
        let i = 0
        for (const [row, col] of cells) {
            if (type === 'shine') {
                const sparkle1X = this.cellSize / 8 * (Math.random() * 2 + 1)
                const sparkle1Y = this.cellSize / 8 * (Math.random() * 2 + 1)
                const sparkle1Angle = Math.random() * Math.PI * 2
                const sparkle2X = this.cellSize / 8 * (Math.random() * 2 + 5)
                const sparkle2Y = this.cellSize / 8 * (Math.random() * 2 + 3)
                const sparkle2Angle = Math.random() * Math.PI * 2
                const sparkle3X = this.cellSize / 8 * (Math.random() * 2 + 3)
                const sparkle3Y = this.cellSize / 8 * (Math.random() * 2 + 5)
                const sparkle3Angle = Math.random() * Math.PI * 2

                this.cellShineEffects.delete(`${row},${col}`)

                const delay = i * 60

                this.addAnimation({
                    id: `cell-shine-${row}-${col}`,
                    startTime: performance.now() + delay,
                    duration: 1000,
                    onUpdate: (t: number) => {
                        const shineProgress = easing.easeInOutQuad(mapRange(t, 0, 0.5))
                        const sparkle1Progress = easing.easeInOutQuad(mapRange(t, 0.2, 0.8))
                        const sparkle2Progress = easing.easeInOutQuad(mapRange(t, 0.3, 0.9))
                        const sparkle3Progress = easing.easeInOutQuad(mapRange(t, 0.4, 1))

                        this.cellShineEffects.set(`${row},${col}`, {
                            shineProgress,
                            sparkle1Progress,
                            sparkle1X,
                            sparkle1Y,
                            sparkle1Angle,
                            sparkle2Progress,
                            sparkle2X,
                            sparkle2Y,
                            sparkle2Angle,
                            sparkle3Progress,
                            sparkle3X,
                            sparkle3Y,
                            sparkle3Angle,
                        })
                    },
                    onComplete: () => {
                        this.cellShineEffects.delete(`${row},${col}`)
                    },
                })
            } else if (type === 'shatter') {
                const delay = i * 100
                const shardAngles = new Map<number, number>()
                const shardSpeeds = new Map<number, number>()
                const shardRotations = new Map<number, number>()

                for (let j = 1; j <= 6; j++) {
                    const randomAngle = Math.random() * Math.PI / 2 - Math.PI / 4
                    shardAngles.set(j, randomAngle + Math.PI / 6 * (j * 2 + 7))
                    shardSpeeds.set(j, Math.random() * 8 + 2)
                    shardRotations.set(j, (Math.random() * 10 + 5) * (Math.random() > 0.5 ? 1 : -1))
                }

                this.cellShatterEffects.set(`${row},${col}`, {
                    row,
                    col,
                    crackProgress: 0,
                    crackFrame: 0,
                    shardProgress: 0,
                    shardAngles,
                    shardSpeeds,
                    shardRotations,
                })

                this.addAnimation({
                    id: `cell-shatter-${row}-${col}`,
                    startTime: performance.now() + delay,
                    duration: 2000,
                    onUpdate: (t: number) => {
                        const crackProgress = mapRange(t, 0, 0.2)
                        const crackFrame = Math.floor(crackProgress * 2) + 1
                        const shardProgress = mapRange(t, 0.4, 1)

                        this.cellShatterEffects.set(`${row},${col}`, {
                            row,
                            col,
                            crackProgress,
                            crackFrame,
                            shardProgress,
                            shardAngles,
                            shardSpeeds,
                            shardRotations,
                        })
                    },
                    onComplete: () => {
                        this.cellShatterEffects.delete(`${row},${col}`)
                    },
                })
            }
            i++
        }
    }


    requestRender() {
        if (this.renderRequested || this.isAnimating) {
            return
        }

        this.renderRequested = true

        requestAnimationFrame(() => {
            this.render()
            this.renderRequested = false
        })
    }


    addAnimation(animation: GameAnimation) {
        this.animations.set(animation.id, animation)

        if (!this.isAnimating) {
            this.isAnimating = true
            requestAnimationFrame(this.handleAnimationFrame)
        }
    }


    handleAnimationFrame = (timestamp: number) => {
        if (this.animations.size === 0) {
            this.isAnimating = false

            return
        }

        this.updateAnimations(timestamp)
        this.render()

        requestAnimationFrame(this.handleAnimationFrame)
    }


    updateAnimations(timestamp: number) {
        for (const [id, animation] of this.animations) {
            if (animation.startTime > timestamp) {
                continue
            }

            const elapsed = timestamp - animation.startTime
            const progress = animation.duration
                ? Math.min(elapsed / animation.duration, 1)
                : 0

            animation.onUpdate(progress)

            if (progress >= 1) {
                this.animations.delete(id)
                if (animation.onComplete) {
                    animation.onComplete()
                }
            }
        }
    }


    render() {
        this.renderSprites()

        const { ctx, canvas, viewport } = this

        const dpr = window.devicePixelRatio || 1
        const width = canvas.width / dpr
        const height = canvas.height / dpr
        const spriteSize = this.cellSize * this.spriteScale * dpr

        ctx.fillStyle = colors.mainBg[this.colorScheme]
        ctx.fillRect(0, 0, width, height)

        ctx.save()
        ctx.translate(viewport.x, viewport.y)
        ctx.scale(viewport.scale, viewport.scale)

        const minX = -viewport.x / viewport.scale
        const minY = -viewport.y / viewport.scale
        const maxX = (width - viewport.x) / viewport.scale
        const maxY = (height - viewport.y) / viewport.scale

        const cellSizeWithGap = this.cellSize + this.cellGap

        const startCol = Math.max(1, Math.floor(minX / cellSizeWithGap) + 1)
        const startRow = Math.max(1, Math.floor(minY / cellSizeWithGap) + 1)
        const endCol = Math.min(this.boardCols, Math.ceil(maxX / cellSizeWithGap))
        const endRow = Math.min(this.boardRows, Math.ceil(maxY / cellSizeWithGap))

        for (const board of this.boards) {
            const boardX = (board.startCol - 1) * cellSizeWithGap - this.cellGap
            const boardY = (board.startRow - 1) * cellSizeWithGap - this.cellGap
            const boardWidth = (board.endCol - board.startCol + 1) * cellSizeWithGap + this.cellGap
            const boardHeight = (board.endRow - board.startRow + 1) * cellSizeWithGap + this.cellGap

            ctx.fillStyle = colors.grid[this.colorScheme]
            ctx.fillRect(boardX, boardY, boardWidth, boardHeight)
        }

        for (let row = startRow; row <= endRow; row++) {
            for (let col = startCol; col <= endCol; col++) {
                this.renderCell(ctx, row, col)

                if (this.cellShineEffects.has(`${row},${col}`)) {
                    this.renderCellShine(ctx, row, col)
                }

                if (this.cellShatterEffects.has(`${row},${col}`)) {
                    this.renderCellShatterCracks(ctx, row, col)
                }
            }
        }

        for (const block of this.blocks) {
            const blockX = (block.startCol - 1) * cellSizeWithGap
            const blockY = (block.startRow - 1) * cellSizeWithGap
            const blockWidth = (block.endCol - block.startCol + 1) * cellSizeWithGap
            const blockHeight = (block.endRow - block.startRow + 1) * cellSizeWithGap

            ctx.strokeStyle = colors.border[this.colorScheme]
            ctx.lineWidth = this.borderWidth
            ctx.strokeRect(blockX - (this.cellGap / 2), blockY - (this.cellGap / 2), blockWidth, blockHeight)
        }

        if (this.selectedCell) {
            const row = this.selectedCell.row
            const col = this.selectedCell.col

            // Render cell again so it's on top of block borders
            this.renderCell(ctx, row, col)
            if (this.cellShatterEffects.has(`${row},${col}`)) {
                this.renderCellShatterCracks(ctx, row, col)
            }

            const selX = (col - 1) * cellSizeWithGap
            const selY = (row - 1) * cellSizeWithGap
            const sourceX = getSpriteX(spriteSize, 'selection')
            const sourceY = getSpriteY(spriteSize, 'selection')
            ctx.drawImage(
                this.spriteCanvas,
                sourceX,
                sourceY,
                spriteSize * 3,
                spriteSize * 3,
                selX - this.cellSize,
                selY - this.cellSize,
                this.cellSize * 3,
                this.cellSize * 3
            )
        }

        for (const effect of this.cellShatterEffects.values()) {
            if (effect.shardProgress > 0
                && effect.row >= startRow - 5
                && effect.row <= endRow + 5
                && effect.col >= startCol - 5
                && effect.col <= endCol + 5
            ) {
                this.renderCellShatterShards(ctx, effect.row, effect.col)
            }
        }

        this.renderGridHeaders(ctx, startRow, endRow, startCol, endCol)

        ctx.restore()
    }


    renderCell(ctx: CanvasRenderingContext2D, row: number, col: number) {
        const cell = this.cells.get(`${row},${col}`)

        if (!cell) {
            return
        }

        const dpr = window.devicePixelRatio || 1
        const cellType = cell.type
        const cellSizeWithGap = this.cellSize + this.cellGap
        const spriteSize = this.cellSize * this.spriteScale * dpr
        const x = (col - 1) * cellSizeWithGap
        const y = (row - 1) * cellSizeWithGap

        const isActive = this.activeCell && this.activeCell.row === row && this.activeCell.col === col
        const isHovered = this.hoveredCell && this.hoveredCell.row === row && this.hoveredCell.col === col
        const spriteState = isActive ? 'active' : isHovered ? 'hover' : 'none'

        switch (cellType) {
            case 'empty': {
                const sourceX = getSpriteX(spriteSize, 'background', spriteState)
                const sourceY = getSpriteY(spriteSize, 'background', spriteState)

                ctx.drawImage(
                    this.spriteCanvas,
                    sourceX,
                    sourceY,
                    spriteSize,
                    spriteSize,
                    x,
                    y,
                    this.cellSize,
                    this.cellSize
                )

                if (cell.dimmed) {
                    ctx.fillStyle = colors.cellDimmed[this.colorScheme]
                    ctx.fillRect(x, y, this.cellSize, this.cellSize)
                }

                break
            }
            case 'hidden': {
                const sourceX = getSpriteX(spriteSize, 'hidden', spriteState)
                const sourceY = getSpriteY(spriteSize, 'hidden', spriteState)

                ctx.drawImage(
                    this.spriteCanvas,
                    sourceX,
                    sourceY,
                    spriteSize,
                    spriteSize,
                    x,
                    y,
                    this.cellSize,
                    this.cellSize
                )

                if (cell.dimmed) {
                    ctx.fillStyle = colors.cellDimmed[this.colorScheme]
                    ctx.fillRect(x, y, this.cellSize, this.cellSize)
                }

                break
            }
            case 'given':
            case 'single': {
                const spriteType = cellType === 'given' ? 'given' : 'userValue'
                const sourceX = (cell.number - 1) * spriteSize
                const sourceY = getSpriteY(spriteSize, spriteType, spriteState)
                const cellError = this.cellErrors.get(`${row},${col}`)
                const numberState = cellError ? 'error' : 'none'

                ctx.drawImage(
                    this.spriteCanvas,
                    sourceX,
                    getSpriteY(spriteSize, spriteType, spriteState),
                    spriteSize,
                    spriteSize,
                    x,
                    y,
                    this.cellSize,
                    this.cellSize
                )

                ctx.drawImage(
                    this.spriteCanvas,
                    sourceX,
                    getSpriteY(spriteSize, `${spriteType}Number`, numberState),
                    spriteSize,
                    spriteSize,
                    x,
                    y,
                    this.cellSize,
                    this.cellSize
                )

                if (cellError) {
                    this.renderCellError(ctx, row, col, cellError)
                }

                if (cell.dimmed) {
                    ctx.fillStyle = colors.cellDimmed[this.colorScheme]
                    ctx.fillRect(x, y, this.cellSize, this.cellSize)
                }

                break
            }
            case 'candidates': {
                const bgSourceX = getSpriteX(spriteSize, 'background', spriteState)
                const bgSourceY = getSpriteY(spriteSize, 'background', spriteState)

                ctx.drawImage(
                    this.spriteCanvas,
                    bgSourceX,
                    bgSourceY,
                    spriteSize,
                    spriteSize,
                    x,
                    y,
                    this.cellSize,
                    this.cellSize
                )

                for (const number of cell.numbers) {
                    const sourceX = (number - 1) * spriteSize
                    const sourceY = getSpriteY(spriteSize, 'candidate', spriteState)
                    const cellError = this.cellErrors.get(`${row},${col},${number}`)
                    const numberState = cellError ? 'error' : 'none'

                    ctx.drawImage(
                        this.spriteCanvas,
                        sourceX,
                        getSpriteY(spriteSize, 'candidate', spriteState),
                        spriteSize,
                        spriteSize,
                        x,
                        y,
                        this.cellSize,
                        this.cellSize
                    )

                    ctx.drawImage(
                        this.spriteCanvas,
                        sourceX,
                        getSpriteY(spriteSize, 'candidateNumber', numberState),
                        spriteSize,
                        spriteSize,
                        x,
                        y,
                        this.cellSize,
                        this.cellSize
                    )

                    if (cellError) {
                        this.renderCandidateError(ctx, row, col, number, cellError)
                    }

                    if (!cell.dimmed && cell.dimmedNumbers.includes(number)) {
                        this.renderCandidateDimmedOverlay(ctx, x, y, this.cellSize, number)
                    }
                }

                if (cell.dimmed) {
                    ctx.fillStyle = colors.cellDimmed[this.colorScheme]
                    ctx.fillRect(x, y, this.cellSize, this.cellSize)
                }

                break
            }
        }
    }


    renderCellError(
        ctx: CanvasRenderingContext2D,
        row: number,
        col: number,
        contextCells: [number, number][],
    ) {
        const cellX = (col - 1) * (this.cellSize + this.cellGap) + this.cellSize / 2
        const cellY = (row - 1) * (this.cellSize + this.cellGap) + this.cellSize / 2

        ctx.strokeStyle = colors.textError[this.colorScheme]
        ctx.lineWidth = 4

        for (const [contextRow, contextCol] of contextCells) {
            const contextX = (contextCol - 1) * (this.cellSize + this.cellGap) + this.cellSize / 2
            const contextY = (contextRow - 1) * (this.cellSize + this.cellGap) + this.cellSize / 2
            const angle = Math.atan2(contextY - cellY, contextX - cellX)
            const startAngle = angle - Math.PI / 6
            const endAngle = angle + Math.PI / 6

            ctx.beginPath()
            ctx.moveTo(
                cellX + Math.cos(startAngle) * (this.cellSize / 2 * 0.85),
                cellY + Math.sin(startAngle) * (this.cellSize / 2 * 0.85)
            )
            ctx.arc(
                cellX,
                cellY,
                this.cellSize / 2 * 0.85,
                startAngle,
                endAngle
            )
            ctx.stroke()
        }
    }


    renderCandidateError(
        ctx: CanvasRenderingContext2D,
        row: number,
        col: number,
        value: number,
        contextCells: [number, number][],
    ) {
        const [ rows, columns ] = this.blockSizeToDimensions(this.blockSize)
        const size = this.cellSize
        const cellX = (col - 1) * (this.cellSize + this.cellGap)
        const cellY = (row - 1) * (this.cellSize + this.cellGap)
        const subX = cellX + size / (columns * 2) + (size / columns) * ((value - 1) % columns)
        const subY = cellY + size / (rows * 2) + (size / rows) * Math.floor((value - 1) / columns)
        const subSize = size / (columns * 2)

        ctx.strokeStyle = colors.textError[this.colorScheme]
        ctx.lineWidth = 2

        for (const [contextRow, contextCol] of contextCells) {
            const contextX = (contextCol - 1) * (this.cellSize + this.cellGap) + this.cellSize / 2
            const contextY = (contextRow - 1) * (this.cellSize + this.cellGap) + this.cellSize / 2
            const angle = Math.atan2(contextY - subY, contextX - subX)
            const startAngle = angle - Math.PI / 6
            const endAngle = angle + Math.PI / 6

            ctx.beginPath()
            ctx.moveTo(
                subX + Math.cos(startAngle) * (subSize * 0.85),
                subY + Math.sin(startAngle) * (subSize * 0.85)
            )
            ctx.arc(
                subX,
                subY,
                subSize * 0.85,
                startAngle,
                endAngle
            )
            ctx.stroke()
        }
    }


    renderCellShine(
        ctx: CanvasRenderingContext2D,
        row: number,
        col: number,
    ) {
        const effects = this.cellShineEffects.get(`${row},${col}`)
        const cellX = (col - 1) * (this.cellSize + this.cellGap)
        const cellY = (row - 1) * (this.cellSize + this.cellGap)

        this.renderShineEffect(
            ctx,
            cellX,
            cellY,
            this.cellSize,
            effects.shineProgress
        )
        this.renderSparkleEffect(
            ctx,
            cellX + effects.sparkle1X,
            cellY + effects.sparkle1Y,
            effects.sparkle1Angle,
            this.cellSize,
            effects.sparkle1Progress,
        )
        this.renderSparkleEffect(
            ctx,
            cellX + effects.sparkle2X,
            cellY + effects.sparkle2Y,
            effects.sparkle2Angle,
            this.cellSize,
            effects.sparkle2Progress,
        )
        this.renderSparkleEffect(
            ctx,
            cellX + effects.sparkle3X,
            cellY + effects.sparkle3Y,
            effects.sparkle3Angle,
            this.cellSize,
            effects.sparkle3Progress,
        )
    }


    renderShineEffect(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
        progress: number
    ) {
        if (progress <= 0 || progress >= 1) {
            return
        }

        ctx.save();

        ctx.beginPath();
        ctx.rect(x, y, size, size);
        ctx.clip();

        const offset = (progress * (size * 3)) - (size / 4);
        const lineWidth = size / 2;

        const gradient = ctx.createLinearGradient(0, 0, lineWidth, 0);
        gradient.addColorStop(0, "rgba(255, 255, 255, 0)");
        gradient.addColorStop(0.5, "rgba(255, 255, 255, 0.8)");
        gradient.addColorStop(1, "rgba(255, 255, 255, 0)");
        ctx.fillStyle = gradient;

        ctx.translate(x + offset, y - size * 0.5);
        ctx.rotate(Math.PI / 4);

        ctx.fillRect(0, 0, lineWidth, size * 2)

        ctx.restore();
    }


    renderSparkleEffect(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        angle: number,
        size: number,
        progress: number
    ) {
        if (progress <= 0 || progress >= 1) {
            return
        }

        const spriteSize = size * this.spriteScale
        const sparkleSize = size * 0.3 * (0.2 + 0.8 * Math.sin(progress * Math.PI))

        ctx.save()

        ctx.translate(x, y)
        ctx.rotate(progress * Math.PI * 0.75 + angle)
        ctx.translate(-x, -y)

        ctx.globalAlpha = 0.5 + 0.5 * Math.sin(progress * Math.PI)

        ctx.drawImage(
            this.spriteCanvas,
            getSpriteX(spriteSize, 'sparkle'),
            getSpriteY(spriteSize, 'sparkle'),
            spriteSize,
            spriteSize,
            x - (sparkleSize / 2),
            y - (sparkleSize / 2),
            sparkleSize,
            sparkleSize
        )

        ctx.restore()
    }


    renderCellShatterCracks(
        ctx: CanvasRenderingContext2D,
        row: number,
        col: number,
    ) {
        const effects = this.cellShatterEffects.get(`${row},${col}`)
        const cellX = (col - 1) * (this.cellSize + this.cellGap)
        const cellY = (row - 1) * (this.cellSize + this.cellGap)

        if (effects.shardProgress > 0) {
            return
        }

        this.renderCrackEffect(
            ctx,
            cellX,
            cellY,
            this.cellSize,
            effects.crackFrame,
        )
    }


    renderCellShatterShards(
        ctx: CanvasRenderingContext2D,
        row: number,
        col: number,
    ) {
        const effects = this.cellShatterEffects.get(`${row},${col}`)
        const cellX = (col - 1) * (this.cellSize + this.cellGap)
        const cellY = (row - 1) * (this.cellSize + this.cellGap)

        if (effects.shardProgress == 0) {
            return
        }

        for (let i = 1; i <= 6; i++) {
            const angle = effects.shardAngles.get(i)!
            const speed = effects.shardSpeeds.get(i)!
            const rotation = effects.shardRotations.get(i)!
            this.renderShardEffect(
                ctx,
                cellX,
                cellY,
                this.cellSize,
                i as 1 | 2 | 3 | 4 | 5 | 6,
                angle,
                speed,
                rotation,
                effects.shardProgress
            )
        }
    }


    renderCrackEffect(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
        frame: 0 | 1 | 2 | 3,
    ) {
        const spriteSize = size * this.spriteScale

        this.renderCellHidden(
            ctx,
            x,
            y,
            size,
            'regular'
        )

        if (frame == 0) {
            return
        }

        const sourceX = getSpriteX(spriteSize, 'crack')
        const sourceY = getSpriteY(spriteSize, 'crack', frame)

        ctx.drawImage(
            this.spriteCanvas,
            sourceX,
            sourceY,
            spriteSize,
            spriteSize,
            x,
            y,
            size,
            size
        )
    }

    renderShardEffect(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
        shard: 1 | 2 | 3 | 4 | 5 | 6,
        angle: number,
        speed: number,
        rotation: number,
        progress: number,
    ) {
        if (progress <= 0 || progress >= 1) {
            return
        }

        const spriteSize = size * this.spriteScale
        const sourceX = getSpriteX(spriteSize, 'shard', shard)
        const sourceY = getSpriteY(spriteSize, 'shard', shard)

        const travelDistance = size * speed * progress
        const offsetX = Math.cos(angle) * travelDistance
        const offsetY = Math.sin(angle) * travelDistance

        const shardAngles = [270, 330, 30, 90, 150, 210]
        const shardRad = shardAngles[shard - 1]! * (Math.PI / 180)

        ctx.save()

        const centerX = x + size / 2 + offsetX + Math.cos(shardRad) * (size / 3)
        const centerY = y + size / 2 + offsetY + Math.sin(shardRad) * (size / 3)

        ctx.translate(centerX, centerY)
        ctx.rotate(progress * rotation)
        ctx.translate(-centerX, -centerY)

        ctx.globalAlpha = progress > 0.75 ? 1 - ((progress - 0.75) * 4) : 1

        ctx.drawImage(
            this.spriteCanvas,
            sourceX,
            sourceY,
            spriteSize,
            spriteSize,
            x + offsetX,
            y + offsetY,
            size,
            size
        )

        ctx.restore()
    }


    renderGridHeaders(
        ctx: CanvasRenderingContext2D,
        startRow: number,
        endRow: number,
        startCol: number,
        endCol: number,
    ) {
        const dpr = window.devicePixelRatio || 1
        const spriteSize = this.cellSize * this.spriteScale * dpr
        const cellSizeWithGap = this.cellSize + this.cellGap
        const viewportX = -this.viewport.x / this.viewport.scale - this.cellGap
        const viewportY = -this.viewport.y / this.viewport.scale - this.cellGap
        const sourceX = getSpriteX(spriteSize, 'selection')
        const sourceY = getSpriteY(spriteSize, 'selection')

        ctx.drawImage(
            this.rowHeaderCanvas,
            0,
            0,
            this.rowHeaderCanvas.width,
            this.rowHeaderCanvas.height,
            viewportX - this.headerCanvasPadding,
            -this.headerCanvasPadding,
            this.cellSize + this.headerCanvasPadding * 2,
            cellSizeWithGap * this.boardRows - this.cellGap + this.headerCanvasPadding * 2
        )
        if (this.selectedCell) {
            const selX = (this.selectedCell.col - 1) * cellSizeWithGap
            const selY = (this.selectedCell.row - 1) * cellSizeWithGap

            ctx.drawImage(
                this.spriteCanvas,
                sourceX,
                sourceY,
                spriteSize * 3,
                spriteSize * 3,
                viewportX - this.cellSize,
                selY - this.cellSize,
                this.cellSize * 3,
                this.cellSize * 3
            )
        }

        ctx.drawImage(
            this.colHeaderCanvas,
            0,
            0,
            this.colHeaderCanvas.width,
            this.colHeaderCanvas.height,
            -this.headerCanvasPadding,
            viewportY - this.headerCanvasPadding,
            cellSizeWithGap * this.boardCols - this.cellGap + this.headerCanvasPadding * 2,
            this.cellSize + this.headerCanvasPadding * 2
        )

        if (this.selectedCell) {
            const selX = (this.selectedCell.col - 1) * cellSizeWithGap
            const selY = (this.selectedCell.row - 1) * cellSizeWithGap

            ctx.drawImage(
                this.spriteCanvas,
                sourceX,
                sourceY,
                spriteSize * 3,
                spriteSize * 3,
                selX - this.cellSize,
                viewportY - this.cellSize,
                this.cellSize * 3,
                this.cellSize * 3
            )
        }
    }


    renderSprites() {
        this.spriteScale = Math.max(Math.round(this.viewport.scale * 4) / 4, 0.25)

        if (this.renderedSpriteScale === this.spriteScale) {
            return
        }
        this.renderedSpriteScale = this.spriteScale

        const ctx = this.spriteCtx
        const canvas = this.spriteCanvas

        const cellSize = this.cellSize * this.spriteScale
        const blockSize = this.blockSize
        const dpr = window.devicePixelRatio || 1
        const rows = 18

        canvas.width = (blockSize + 1) * cellSize * dpr
        canvas.height = cellSize * dpr * rows
        canvas.style.width = `${(blockSize + 1) * cellSize}px`
        canvas.style.height = `${cellSize * rows}px`
        ctx.setTransform(dpr, 0, 0, dpr, 0, 0)

        const spriteX = (
            baseKey: keyof typeof spriteOffsets.base,
            stateKey?: keyof typeof spriteOffsets.state,
        ) => getSpriteX(cellSize, baseKey, stateKey)
        const spriteY = (
            baseKey: keyof typeof spriteOffsets.base,
            stateKey?: keyof typeof spriteOffsets.state,
        ) => getSpriteY(cellSize, baseKey, stateKey)

        for (let i = 1; i <= blockSize; i++) {
            const x = (i - 1) * cellSize

            this.renderGiven(ctx, blockSize, x, spriteY('given'), cellSize, i, 'regular')
            this.renderGiven(ctx, blockSize, x, spriteY('given', 'hover'), cellSize, i, 'hover')
            this.renderGiven(ctx, blockSize, x, spriteY('given', 'active'), cellSize, i, 'active')
            this.renderGivenNumber(ctx, blockSize, x, spriteY('givenNumber'), cellSize, i, 'regular')
            this.renderGivenNumber(ctx, blockSize, x, spriteY('givenNumber', 'error'), cellSize, i, 'error')
            this.renderUserValue(ctx, blockSize, x, spriteY('userValue'), cellSize, i, 'regular')
            this.renderUserValue(ctx, blockSize, x, spriteY('userValue', 'hover'), cellSize, i, 'hover')
            this.renderUserValue(ctx, blockSize, x, spriteY('userValue', 'active'), cellSize, i, 'active')
            this.renderUserValueNumber(ctx, blockSize, x, spriteY('userValueNumber'), cellSize, i, 'regular')
            this.renderUserValueNumber(ctx, blockSize, x, spriteY('userValueNumber', 'error'), cellSize, i, 'error')
            this.renderCandidate(ctx, blockSize, x, spriteY('candidate'), cellSize, i, 'regular')
            this.renderCandidate(ctx, blockSize, x, spriteY('candidate', 'hover'), cellSize, i, 'hover')
            this.renderCandidate(ctx, blockSize, x, spriteY('candidate', 'active'), cellSize, i, 'active')
            this.renderCandidateNumber(ctx, blockSize, x, spriteY('candidateNumber'), cellSize, i, 'regular')
            this.renderCandidateNumber(ctx, blockSize, x, spriteY('candidateNumber', 'error'), cellSize, i, 'error')
        }

        this.renderCellBackground(ctx, spriteX('background'), spriteY('background'), cellSize, 'regular')
        this.renderCellBackground(ctx, spriteX('background'), spriteY('background', 'hover'), cellSize, 'hover')
        this.renderCellBackground(ctx, spriteX('background'), spriteY('background', 'active'), cellSize, 'active')
        this.renderCellHidden(ctx, spriteX('hidden'), spriteY('hidden'), cellSize, 'regular')
        this.renderCellHidden(ctx, spriteX('hidden'), spriteY('hidden', 'hover'), cellSize, 'hover')
        this.renderCellHidden(ctx, spriteX('hidden'), spriteY('hidden', 'active'), cellSize, 'active')

        this.renderSparkleSprite(ctx, spriteX('sparkle'), spriteY('sparkle'), cellSize)

        this.renderCrackSprite(ctx, spriteX('crack'), spriteY('crack', 1), cellSize, 1)
        this.renderCrackSprite(ctx, spriteX('crack'), spriteY('crack', 2), cellSize, 2)
        this.renderCrackSprite(ctx, spriteX('crack'), spriteY('crack', 3), cellSize, 3)

        this.renderShardSprite(ctx, spriteX('shard', 1), spriteY('shard', 1), cellSize, 1)
        this.renderShardSprite(ctx, spriteX('shard', 2), spriteY('shard', 2), cellSize, 2)
        this.renderShardSprite(ctx, spriteX('shard', 3), spriteY('shard', 3), cellSize, 3)
        this.renderShardSprite(ctx, spriteX('shard', 4), spriteY('shard', 4), cellSize, 4)
        this.renderShardSprite(ctx, spriteX('shard', 5), spriteY('shard', 5), cellSize, 5)
        this.renderShardSprite(ctx, spriteX('shard', 6), spriteY('shard', 6), cellSize, 6)

        this.renderSelection(ctx, spriteX('selection'), spriteY('selection'), cellSize)

        this.renderHeaderSprites()
    }


    renderGiven(
        ctx: CanvasRenderingContext2D,
        blockSize: number,
        x: number,
        y: number,
        size: number,
        value: number,
        state: CellState = 'regular',
    ) {
        const gradient = ctx.createLinearGradient(x, y, x, y + size)
        const color = getCellColor(blockSize, value, this.colorScheme, state)
        gradient.addColorStop(0, color[0])
        gradient.addColorStop(1, color[1])
        ctx.fillStyle = gradient
        ctx.fillRect(x, y, size, size)
    }


    renderGivenNumber(
        ctx: CanvasRenderingContext2D,
        blockSize: number,
        x: number,
        y: number,
        size: number,
        value: number,
        state: NumberState = 'regular',
    ) {
        const fontSize = size * 0.75
        ctx.save()
        ctx.shadowColor = colors.cellBg[this.colorScheme]
        ctx.shadowBlur = 4 * this.spriteScale
        ctx.fillStyle = state == 'error' ? colors.textError[this.colorScheme] : colors.text[this.colorScheme]
        ctx.font = `bold ${fontSize}px Arial`
        ctx.textAlign = 'center'
        ctx.textBaseline = 'middle'
        ctx.fillText(
            this.numberToString(value),
            x + size / 2,
            y + size / 2 + fontSize * numberOffset
        )
        ctx.restore()
    }


    renderUserValue(
        ctx: CanvasRenderingContext2D,
        blockSize: number,
        x: number,
        y: number,
        size: number,
        value: number,
        state: CellState = 'regular',
    ) {
        // Background
        this.renderCellBackground(ctx, x, y, size, state)

        // Circle background
        const gradient = ctx.createLinearGradient(x, y, x, y + size)
        const color = getCellColor(blockSize, value, this.colorScheme, state)
        gradient.addColorStop(0, color[0])
        gradient.addColorStop(1, color[1])
        ctx.fillStyle = gradient
        ctx.beginPath()
        ctx.moveTo(x + size / 2, y)
        ctx.arc(x + size / 2, y + size / 2, (size / 2) * 0.95, 0, Math.PI * 2)
        ctx.fill()

        // Fade background
        // const fadeGradient = ctx.createRadialGradient(
        //     x + size / 2,
        //     y + size / 2,
        //     0,
        //     x + size / 2,
        //     y + size / 2,
        //     size / 2
        // )
        // const cellBg = colors.cellBg.dark
        // const cellBgTransparent = colors.cellBgTransparent[this.colorScheme]
        // fadeGradient.addColorStop(0.85, cellBgTransparent)
        // fadeGradient.addColorStop(1, cellBg)
        // ctx.fillStyle = fadeGradient
        // ctx.beginPath()
        // ctx.moveTo(x + size / 2, y)
        // ctx.arc(x + size / 2, y + size / 2, (size / 2) * 0.95, 0, Math.PI * 2)
        // ctx.fill()
    }


    renderUserValueNumber(
        ctx: CanvasRenderingContext2D,
        blockSize: number,
        x: number,
        y: number,
        size: number,
        value: number,
        state: NumberState = 'regular',
    ) {
        const fontSize = size * 0.75
        ctx.save()
        ctx.shadowColor = colors.cellBg[this.colorScheme]
        ctx.shadowBlur = 4 * this.spriteScale
        ctx.fillStyle = state == 'error' ? colors.textError[this.colorScheme] : colors.text[this.colorScheme]
        ctx.font = `${fontSize}px Times New Roman`
        ctx.textAlign = 'center'
        ctx.textBaseline = 'middle'
        ctx.fillText(this.numberToString(value), x + size / 2, y + size / 2 + fontSize * 0.05)
        ctx.restore()
    }


    renderCandidate(
        ctx: CanvasRenderingContext2D,
        blockSize: number,
        x: number,
        y: number,
        size: number,
        value: number,
        state: CellState = 'regular',
    ) {
        const [ rows, columns ] = this.blockSizeToDimensions(blockSize)
        const subX = x + size / (columns * 2) + (size / columns) * ((value - 1) % columns)
        const subY = y + size / (rows * 2) + (size / rows) * Math.floor((value - 1) / columns)
        const subSize = size / (columns * 2)

        const gradient = ctx.createLinearGradient(subX, subY, subX, subY + subSize)
        const color = getCellColor(blockSize, value, this.colorScheme, state)
        gradient.addColorStop(0, color[0])
        gradient.addColorStop(1, color[1])
        ctx.fillStyle = gradient
        ctx.beginPath()
        ctx.moveTo(x + size / 2, y)
        ctx.arc(
            subX,
            subY,
            subSize * 0.95,
            0,
            Math.PI * 2
        )
        ctx.fill()
    }


    renderCandidateNumber(
        ctx: CanvasRenderingContext2D,
        blockSize: number,
        x: number,
        y: number,
        size: number,
        value: number,
        state: NumberState = 'regular',
    ) {
        const fontSize = size * candidateFontSizeMultiplier[blockSize as keyof typeof candidateFontSizeMultiplier]
        const [ rows, columns ] = this.blockSizeToDimensions(blockSize)
        const subX = x + size / (columns * 2) + (size / columns) * ((value - 1) % columns)
        const subY = y + size / (rows * 2) + (size / rows) * Math.floor((value - 1) / columns)

        ctx.save()
        ctx.shadowColor = colors.cellBg[this.colorScheme]
        ctx.shadowBlur = 4 * this.spriteScale
        ctx.fillStyle = state == 'error' ? colors.textError[this.colorScheme] : colors.text[this.colorScheme]
        ctx.font = `${fontSize}px Times New Roman`
        ctx.textAlign = 'center'
        ctx.textBaseline = 'middle'
        ctx.fillText(this.numberToString(value), subX, subY + fontSize * 0.05)
        ctx.restore()
    }


    renderCandidateDimmedOverlay(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
        value: number,
    ) {
        const [ rows, columns ] = this.blockSizeToDimensions(this.blockSize)
        const subX = x + size / (columns * 2) + (size / columns) * ((value - 1) % columns)
        const subY = y + size / (rows * 2) + (size / rows) * Math.floor((value - 1) / columns)

        ctx.fillStyle = colors.cellDimmed[this.colorScheme]
        ctx.beginPath()
        ctx.moveTo(x + size / 2, y)
        ctx.arc(
            subX,
            subY,
            (size / (columns * 2)) * 0.95,
            0,
            Math.PI * 2
        )
        ctx.fill()
    }


    renderCellBackground(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
        state: CellState = 'regular',
    ) {
        ctx.fillStyle = state === 'hover' ? colors.cellBgHover[this.colorScheme]
            : state === 'active' ? colors.cellBgActive[this.colorScheme]
            : colors.cellBg[this.colorScheme]
        ctx.fillRect(x, y, size, size)
    }


    renderCellHidden(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
        state: CellState = 'regular',
    ) {
        ctx.fillStyle = state === 'hover' ? colors.cellHiddenHover[this.colorScheme]
            : state === 'active' ? colors.cellHiddenActive[this.colorScheme]
            : colors.cellHidden[this.colorScheme]
        ctx.fillRect(x, y, size, size)
    }


    renderSparkleSprite(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
    ) {
        ctx.fillStyle = 'white'
        ctx.strokeStyle = 'black'
        ctx.lineWidth = 1 * this.spriteScale

        ctx.save();

        ctx.beginPath();
        ctx.rect(x, y, size, size);
        ctx.clip();

        ctx.beginPath()
        ctx.moveTo(x + size * 0.5, y + size * 0.05)
        ctx.lineTo(x + size * 0.6, y + size * 0.4)
        ctx.lineTo(x + size * 0.95, y + size * 0.5)
        ctx.lineTo(x + size * 0.6, y + size * 0.6)
        ctx.lineTo(x + size * 0.5, y + size * 0.95)
        ctx.lineTo(x + size * 0.4, y + size * 0.6)
        ctx.lineTo(x + size * 0.05, y + size * 0.5)
        ctx.lineTo(x + size * 0.4, y + size * 0.4)
        ctx.closePath()

        ctx.fill()
        ctx.stroke()

        ctx.restore();
    }


    renderCrackSprite(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
        step: number,
    ) {
        ctx.strokeStyle = colors.crack[this.colorScheme]
        ctx.lineWidth = 1 * this.spriteScale

        ctx.save();

        ctx.beginPath();
        ctx.rect(x, y, size, size);
        ctx.clip();

        for (let i = 1; i < step + 1; i++) {
            for (const path of crackPaths) {
                ctx.beginPath()
                ctx.moveTo(x + path[i]![0]! * size, y + path[i]![1]! * size)
                ctx.lineTo(x + path[i + 1]![0]! * size, y + path[i + 1]![1]! * size)
                ctx.stroke()
            }
        }

        ctx.restore();
    }


    renderShardSprite(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
        shardIndex: number,
    ) {
        const path1Index = shardIndex - 1
        const path2Index = shardIndex % crackPaths.length

        const path1 = crackPaths[path1Index]!
        const path2 = crackPaths[path2Index]!
        const lineWidth = 1 * this.spriteScale

        ctx.save();

        ctx.beginPath();
        ctx.rect(x, y, size, size);
        ctx.clip();

        const points: [number, number][] = [
            [x + path1[0]![0]! * size, y + path1[0]![1]! * size],
            [x + path1[1]![0]! * size, y + path1[1]![1]! * size],
            [x + path1[2]![0]! * size, y + path1[2]![1]! * size],
            [x + path1[3]![0]! * size, y + path1[3]![1]! * size],
            [x + path1[4]![0]! * size, y + path1[4]![1]! * size],
            [x + path2[3]![0]! * size, y + path2[3]![1]! * size],
            [x + path2[2]![0]! * size, y + path2[2]![1]! * size],
            [x + path2[1]![0]! * size, y + path2[1]![1]! * size],
        ]

        ctx.beginPath()
        for (let [px, py] of points) {
            if (px === 0) {
                px += lineWidth / 2
            } else if (px === size) {
                px -= lineWidth / 2
            }
            if (py === 0) {
                py += lineWidth / 2
            } else if (py === size) {
                py -= lineWidth / 2
            }
            ctx.lineTo(px, py)
        }
        ctx.closePath()

        ctx.fillStyle = colors.cellHidden[this.colorScheme]
        ctx.strokeStyle = colors.crack[this.colorScheme]
        ctx.lineWidth = lineWidth

        ctx.fill()
        ctx.stroke()

        ctx.restore();
    }


    renderSelection(
        ctx: CanvasRenderingContext2D,
        cornerX: number,
        cornerY: number,
        size: number,
    ) {
        const dpr = window.devicePixelRatio || 1
        const x = cornerX + size
        const y = cornerY + size
        const padding = 40 * this.spriteScale * dpr

        ctx.save()

        ctx.beginPath()
        ctx.rect(x - padding, y - padding, size + padding * 2, size + padding * 2)
        ctx.rect(x, y + size, size, -size)
        ctx.clip()

        ctx.shadowColor = colors.selection[this.colorScheme]
        ctx.shadowBlur = 4 * this.spriteScale * dpr
        ctx.strokeStyle = colors.selection[this.colorScheme]
        ctx.fillStyle = colors.selection[this.colorScheme]

        for (let i = 0; i < 4; i++) {
            ctx.shadowBlur += 4 * this.spriteScale * dpr
            ctx.fillRect(x, y, size, size)
        }

        ctx.restore()
    }


    renderHeaderSprites() {
        const colCanvas = this.colHeaderCanvas
        const rowCanvas = this.rowHeaderCanvas
        const colCtx = this.colHeaderCtx
        const rowCtx = this.rowHeaderCtx

        const dpr = window.devicePixelRatio || 1
        const cellSize = this.cellSize * this.spriteScale
        const cellGap = this.cellGap * this.spriteScale
        const borderWidth = this.borderWidth * this.spriteScale
        const cellSizeWithGap = (this.cellSize + this.cellGap) * this.spriteScale
        const padding = this.headerCanvasPadding * this.spriteScale

        const spriteSize = this.cellSize * this.spriteScale * dpr
        const sourceX = getSpriteX(spriteSize, 'background')
        const sourceY = getSpriteY(spriteSize, 'background')
        const fontSize = cellSize * 0.5

        // Column canvas
        const colWidth = cellSizeWithGap * this.boardCols + padding * 2
        const colHeight = cellSizeWithGap + padding * 2
        colCanvas.width = colWidth * dpr
        colCanvas.height = colHeight * dpr
        colCtx.setTransform(dpr, 0, 0, dpr, 0, 0)

        // Column shadow
        colCtx.save()
        colCtx.shadowColor = colors.shadow[this.colorScheme]
        colCtx.shadowBlur = 16 * this.spriteScale
        colCtx.fillStyle = colors.shadow[this.colorScheme]
        colCtx.fillRect(
            padding - (cellGap / 2),
            padding - (cellGap / 2),
            cellSizeWithGap * this.boardCols + cellGap,
            cellSize + cellGap
        )
        colCtx.restore()

        // Column grid
        colCtx.fillStyle = colors.grid[this.colorScheme]
        colCtx.fillRect(
            padding,
            padding,
            cellSizeWithGap * this.boardCols,
            cellSize
        )

        // Column labels
        for (let col = 1; col <= this.boardCols; col++) {
            const x = (col - 1) * cellSizeWithGap + padding
            const y = 0 + padding

            colCtx.drawImage(
                this.spriteCanvas,
                sourceX,
                sourceY,
                spriteSize,
                spriteSize,
                x,
                y,
                cellSize,
                cellSize
            )

            colCtx.fillStyle = colors.text[this.colorScheme]
            colCtx.font = `${fontSize}px Times New Roman`
            colCtx.textAlign = 'center'
            colCtx.textBaseline = 'middle'
            colCtx.fillText(
                col.toString(),
                x + cellSize / 2,
                y + cellSize / 2 + fontSize * 0.05
            )
        }

        // Column border
        colCtx.strokeStyle = colors.border[this.colorScheme]
        colCtx.lineWidth = borderWidth
        colCtx.strokeRect(
            padding - (cellGap / 2),
            padding - (cellGap / 2),
            cellSizeWithGap * this.boardCols + cellGap,
            cellSize + cellGap
        )

        // Row canvas
        const rowWidth = cellSizeWithGap + padding * 2
        const rowHeight = cellSizeWithGap * this.boardCols + padding * 2
        rowCanvas.width = rowWidth * dpr
        rowCanvas.height = rowHeight * dpr
        rowCanvas.style.width = `${rowWidth}px`
        rowCanvas.style.height = `${rowHeight}px`
        rowCtx.setTransform(dpr, 0, 0, dpr, 0, 0)

        // Row shadow
        rowCtx.save()
        rowCtx.shadowColor = colors.shadow[this.colorScheme]
        rowCtx.shadowBlur = 16 * this.spriteScale
        rowCtx.fillStyle = colors.shadow[this.colorScheme]
        rowCtx.fillRect(
            padding - (cellGap / 2),
            padding - (cellGap / 2),
            cellSize + cellGap,
            cellSizeWithGap * this.boardCols + cellGap
        )
        rowCtx.restore()

        // Row grid
        rowCtx.fillStyle = colors.grid[this.colorScheme]
        rowCtx.fillRect(
            padding,
            padding,
            cellSize,
            cellSizeWithGap * this.boardCols
        )

        // Row labels
        for (let row = 1; row <= this.boardRows; row++) {
            const x = 0 + padding
            const y = (row - 1) * cellSizeWithGap + padding

            rowCtx.drawImage(
                this.spriteCanvas,
                sourceX,
                sourceY,
                spriteSize,
                spriteSize,
                x,
                y,
                cellSize,
                cellSize
            )

            rowCtx.fillStyle = colors.text[this.colorScheme]
            rowCtx.font = `${fontSize}px Times New Roman`
            rowCtx.textAlign = 'center'
            rowCtx.textBaseline = 'middle'
            rowCtx.fillText(
                this.numberToRowLabel(row),
                x + cellSize / 2,
                y + cellSize / 2 + fontSize * 0.05
            )
        }

        // Row border
        rowCtx.strokeStyle = colors.border[this.colorScheme]
        rowCtx.lineWidth = borderWidth
        rowCtx.strokeRect(
            padding - (cellGap / 2),
            padding - (cellGap / 2),
            cellSize + cellGap,
            cellSizeWithGap * this.boardCols + cellGap
        )
    }


    blockSizeToDimensions(blockSize: number): [number, number] {
        switch (blockSize) {
            case 4:
                return [2, 2]
            case 6:
                return [2, 3]
            case 8:
                return [3, 3]
            case 9:
                return [3, 3]
            case 12:
                return [3, 4]
            case 16:
                return [4, 4]
            default:
                throw new Error(`Unsupported block size: ${blockSize}`)
        }
    }


    numberToString(num: number): string {
        if (this.numberMap.has(num)) {
            return this.numberMap.get(num)!
        } else if (num < 10) {
            return num.toString()
        } else if (num == 10) {
            return '0'
        } else {
            return String.fromCharCode('A'.charCodeAt(0) + (num - 11))
        }
    }


    numberToRowLabel(row: number): string {
        if (row <= 0) {
            return ''
        }

        let result = ''
        let current = row

        while (current > 0) {
            const index = (current - 1) % rowLabelBase
            result = rowLabelAlphabet[index] + result
            current = Math.floor((current - 1) / rowLabelBase)
        }

        return result
    }
}

customElements.define('archipeladoku-board', ArchipeladokuBoard)
