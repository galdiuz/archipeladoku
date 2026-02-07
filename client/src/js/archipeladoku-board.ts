interface BoardData {
    cells: [number, number, CellValue][]
    blocks: Area[]
    boards: Area[]
    blockSize: number
    selectedCell: { row: number; col: number } | null
    colorMap: [number, number][]
    numberMap: [number, string][]
    errors: CellError[]
    colorScheme: string
    discoTrap: boolean
    tunnelVisionTrap: boolean
    fireworks: boolean
    animationsEnabled: boolean
    candidateLayout: number
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


interface CellValueTunnel {
    type: 'tunnel'
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
    | CellValueTunnel
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
    cellHidden: { light: 'hsl(0 0 70)', dark: 'hsl(0 0 40)' },
    cellHiddenHover: { light: 'hsl(0 0 65)', dark: 'hsl(0 0 50)' },
    cellHiddenActive: { light: 'hsl(0 0 60)', dark: 'hsl(0 0 35)' },
    cellTunnel: { light: 'hsl(0 0 80)', dark: 'hsl(0 0 20)' },
    cellTunnelHover: { light: 'hsl(0 0 75)', dark: 'hsl(0 0 30)' },
    cellTunnelActive: { light: 'hsl(0 0 70)', dark: 'hsl(0 0 15)' },
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
    loop?: boolean
    onUpdate: (progress: number) => void
    onComplete?: () => void
}


const easing = {
    easeInOutQuad: (t: number): number => {
        return t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t
    },
    easeIn: (t: number): number => {
        return t * t
    },
    easeOut: (t: number): number => {
        return t * (2 - t)
    },
    easeOutSin: (t: number): number => {
        return Math.sin((t * Math.PI) / 2)
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


interface Spotlight {
    id: number
    originXRatio: number
    lengthRatio: number
    widthAngle: number
    angle: number
    hue: number
}


type FireworkParticle = BlastParticle | TrailParticle


interface BlastParticle {
    id: number
    x: number
    y: number
    startX: number
    startY: number
    vx: number
    vy: number
    vz: number
    size: number
    initialSize: number
    hue: number
    progress: number
    deathT: number
}


interface TrailParticle {
    id: number
    x: number
    y: number
    startX: number
    startY: number
    vx: number
    vy: number
    size: number
    initialSize: number
    hue: number
    progress: number
    spawnT: number
    deathT: number
}


const spriteOffsets = {
    base: {
        given: { x: 0, y: 0 },
        givenNumber: { x: 0, y: 3 },
        userValue: { x: 0, y: 5 },
        userValueNumber: { x: 0, y: 8 },
        candidateNumber: { x: 0, y: 10 },
        sparkle: { x: 0, y: 12 },
        crack: { x: 1, y: 12 },
        shard: { x: 2, y: 12 },
        selection: { x: 4, y: 12 },
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


function clamp(value: number, min: number, max: number): number {
    return Math.min(Math.max(value, min), max)
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


function shuffleArray<T>(array: T[]): void {
    for (let i = array.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1))
        ;[array[i], array[j]] = [array[j]!, array[i]!]
    }
}


function fireworkBezierPoint(t: number, p0x: number, p0y: number, p2x: number, p2y: number): [number, number] {
    const dx = p0x - p2x
    const p1x = (p0x + p2x) / 2
    const p1y = p2y
    const invT = 1 - t

    return [
        invT * invT * p0x + 2 * invT * t * p1x + t * t * p2x,
        invT * invT * p0y + 2 * invT * t * p1y + t * t * p2y,
    ]
}


class ArchipeladokuBoard extends HTMLElement {
    canvas: HTMLCanvasElement
    ctx: CanvasRenderingContext2D
    spriteCanvas: HTMLCanvasElement
    spriteCtx: CanvasRenderingContext2D
    spriteScale: number = 1
    renderedDpr: number = 0
    renderedSpriteScale: number = 0
    colHeaderCanvas: HTMLCanvasElement
    colHeaderCtx: CanvasRenderingContext2D
    rowHeaderCanvas: HTMLCanvasElement
    rowHeaderCtx: CanvasRenderingContext2D
    fireworksCanvas: HTMLCanvasElement
    fireworksCtx: CanvasRenderingContext2D
    headerCanvasPadding: number = 16
    blockSize: number = 9
    blocks: Area[] = []
    boards: Area[] = []
    cells: Map<string, CellValue> = new Map()
    boardRows: number = 0
    boardCols: number = 0
    blockRows: number = 0
    blockCols: number = 0
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
    colorMap: Map<number, number> = new Map()
    oldColorMap: Map<number, number> = new Map()
    oldColorMapOpacity: number = 0
    numberMap: Map<number, string> = new Map()
    discoTrap: boolean = false
    discoTrapSpotlights: Spotlight[] = []
    tunnelVisionTrap: boolean = false
    tunnelVisionTrapRadius: number = 0
    tunnelVisionTrapOpacity: number = 0
    candidateSubXMap: Map<number, number> = new Map()
    candidateSubYMap: Map<number, number> = new Map()
    candidateSubSize: number = 0
    fireworkAnimationCounter: number = 0
    fireworkFadeCounter: number = 0
    fireworkParticleCounter: number = 0
    fireworkParticles: Map<number, FireworkParticle> = new Map()
    fireworks: boolean = false
    lastFireworkFadeTime: number = 0
    animationsEnabled: boolean = true
    candidateLayout: number = 0


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
        this.fireworksCanvas = document.createElement('canvas')
        this.fireworksCtx = this.fireworksCanvas.getContext('2d', { alpha: true } )!
        this.viewport = { x: 60, y: 60, scale: 1 }

        const style = document.createElement('style')
        style.textContent = `
            :host {
                display: block;
                width: 100%;
                height: 100%;
                position: relative;
                overflow: hidden;
            }

            canvas {
                display: block;
                width: 100%;
                height: 100%;
                touch-action: none;
            }
        `
        this.shadowRoot!.appendChild(style)
    }


    connectedCallback() {
        this.shadowRoot!.appendChild(this.canvas)
        this.resize()
        window.addEventListener('resize', () => this.resize())
        this.render(performance.now())
        requestAnimationFrame(() => {
            this.resize()
        })

        this.canvas.addEventListener('pointerdown', (e: PointerEvent) => this.handlePointerDown(e))
        window.addEventListener('pointermove', (e: PointerEvent) => this.handlePointerMove(e))
        window.addEventListener('pointerup', (e: PointerEvent) => this.handlePointerUp(e))
        window.addEventListener('pointercancel', (e: PointerEvent) => this.handlePointerUp(e))
        this.canvas.addEventListener('wheel', (e: WheelEvent) => this.handleWheel(e), { passive: false })

        this.colorScheme = getSystemColorScheme()

        const watchDPR = () => {
          const dpr = window.devicePixelRatio
          const mqString = `(resolution: ${dpr}dppx)`
          const media = window.matchMedia(mqString)

          media.addEventListener("change", () => {
            this.requestRender()
            watchDPR()
          }, { once: true })
        }
        watchDPR()

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

        const [ blockRows, blockCols ] = this.blockSizeToDimensions(value.blockSize)
        this.blockRows = blockRows
        this.blockCols = blockCols

        if (value.candidateLayout !== this.candidateLayout) {
            this.candidateLayout = value.candidateLayout
            this.renderedSpriteScale = 0
        }

        for (let i = 1; i <= this.blockSize; i++) {
            const size = this.cellSize
            const subX = size / blockCols * ((i - 1) % blockCols)
            const subYOffset = blockRows !== blockCols ? size / (blockRows * blockCols * 2) : 0
            const subY = value.candidateLayout === 0
                ? ((size / blockRows) * Math.floor((i - 1) / blockCols) + subYOffset)
                : ((size / blockRows) * (blockRows - 1 - Math.floor((i - 1) / blockCols)) + subYOffset)
            this.candidateSubXMap.set(i, subX)
            this.candidateSubYMap.set(i, subY)
            this.candidateSubSize = size / blockCols
        }

        this.cells.clear()
        for (const [row, col, cellValue] of value.cells) {
            this.cells.set(`${row},${col}`, cellValue)
        }

        const numberMap = new Map<number, string>()
        for (const [key, val] of value.numberMap) {
            numberMap.set(key, val)
        }

        if (!compareMaps(this.numberMap, numberMap)) {
            this.numberMap = numberMap
            this.renderedSpriteScale = 0
        }

        const colorMap = new Map<number, number>()
        for (const [key, val] of value.colorMap) {
            colorMap.set(key, val)
        }

        if (!compareMaps(this.colorMap, colorMap)) {
            this.oldColorMap = this.colorMap
            this.colorMap = colorMap
            this.startColorMapTransition()
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

        if (!value.animationsEnabled && this.animationsEnabled) {
            this.animations.clear()
            this.cellShineEffects.clear()
            this.cellShatterEffects.clear()
            this.discoTrapSpotlights = []
            this.tunnelVisionTrapRadius = 0
            this.tunnelVisionTrapOpacity = 0
            this.fireworkParticles.clear()
            this.fireworks = false
            this.discoTrap = false
            this.tunnelVisionTrap = false
            if (this.fireworksCanvas.width && this.fireworksCanvas.height) {
                this.fireworksCtx.clearRect(0, 0, this.fireworksCanvas.width, this.fireworksCanvas.height)
            }
        }
        this.animationsEnabled = value.animationsEnabled

        if (!this.discoTrap && value.discoTrap && this.clientWidth && this.animationsEnabled) {
            this.discoTrap = true
            this.startDiscoTrapAnimation()
        } else if (this.discoTrap && !value.discoTrap) {
            this.discoTrap = false
            this.stopDiscoTrapAnimation()
        }

        if (!this.tunnelVisionTrap && value.tunnelVisionTrap && this.animationsEnabled) {
            this.tunnelVisionTrap = true
            this.startTunnelVisionTrapAnimation()
        } else if (this.tunnelVisionTrap && !value.tunnelVisionTrap) {
            this.tunnelVisionTrap = false
            this.stopTunnelVisionTrapAnimation()
        }

        if (!this.fireworks && value.fireworks && this.animationsEnabled) {
            this.fireworks = true
            this.startFireworksTimer()
        } else if (this.fireworks && !value.fireworks) {
            this.fireworks = false
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
        this.fireworksCanvas.width = width
        this.fireworksCanvas.height = height

        this.ctx.setTransform(dpr, 0, 0, dpr, 0, 0)

        this.requestRender()
    }


    animateCells(cells: [number, number][], type: 'shine' | 'shatter') {
        if (type === 'shine') {
            let minRow = 1000
            let minCol = 1000
            let maxRow = 0
            let maxCol = 0
            for (const [row, col] of cells) {
                minRow = row < minRow ? row : minRow
                minCol = col < minCol ? col : minCol
                maxRow = row > maxRow ? row : maxRow
                maxCol = col > maxCol ? col : maxCol
            }
            minRow -= 1
            minCol -= 1
            maxRow += 1
            maxCol += 1
            const size = this.cellSize
            const minSum = minRow + minCol
            const maxSum = maxRow + maxCol
            const span = maxSum - minSum
            const totalTravelDistance = span * size
            const sparkles = new Map<string, Map<number, any>>()

            for (const [row, col] of cells) {
                const cellSparkles = new Map<number, any>()
                cellSparkles.set(1, {
                    x: this.cellSize / 8 * (Math.random() * 2 + 1),
                    y: this.cellSize / 8 * (Math.random() * 2 + 1),
                    angle: Math.random() * Math.PI * 2,
                    direction: Math.random() > 0.5 ? 1 : -1,
                    progress: 0,
                    delay: 0,
                })
                cellSparkles.set(2, {
                    x: this.cellSize / 8 * (Math.random() * 2 + 5),
                    y: this.cellSize / 8 * (Math.random() * 2 + 3),
                    angle: Math.random() * Math.PI * 2,
                    direction: Math.random() > 0.5 ? 1 : -1,
                    progress: 0,
                    delay: 0.05,
                })
                cellSparkles.set(3, {
                    x: this.cellSize / 8 * (Math.random() * 2 + 3),
                    y: this.cellSize / 8 * (Math.random() * 2 + 5),
                    angle: Math.random() * Math.PI * 2,
                    direction: Math.random() > 0.5 ? 1 : -1,
                    progress: 0,
                    delay: 0.10,
                })
                sparkles.set(`${row},${col}`, cellSparkles)
            }

            this.addAnimation({
                id: `cell-shine-${minRow}-${minCol}-${maxRow}-${maxCol}`,
                startTime: performance.now(),
                duration: 2000,
                onUpdate: (t: number) => {
                    const sparkleDuration = 0.4
                    const shineProgress = easing.easeInOutQuad(mapRange(t, 0, 0.5))
                    const totalOffset = shineProgress * totalTravelDistance + size * 0.5

                    for (const [row, col] of cells) {
                        const cellSum = row + col
                        const cellOffset = (cellSum - minSum) * size
                        const localOffset = totalOffset - cellOffset

                        const relativePos = (cellSum - minSum) / span
                        const triggerTime = relativePos * 0.4
                        const sparkleProgress = Math.min(1, (t - triggerTime) / sparkleDuration)

                        for (const sparkle of sparkles.get(`${row},${col}`)!.values()) {
                            sparkle.progress = easing.easeInOutQuad(
                                clamp((t - triggerTime - sparkle.delay) / sparkleDuration, 0, 1)
                            )
                        }

                        const shineIsActive = localOffset > -size && localOffset < size * 3
                        const sparkleIsActive = sparkleProgress > 0 && sparkleProgress < 1
                        let cellShineEffects = this.cellShineEffects.get(`${row},${col}`)

                        if (shineIsActive || sparkleIsActive) {
                            if (!cellShineEffects) {
                                cellShineEffects = new Map<string, any>()
                                this.cellShineEffects.set(`${row},${col}`, cellShineEffects)
                            }

                            cellShineEffects.set(`${minRow},${minCol},${maxRow},${maxCol}`, {
                                offset: localOffset,
                                sparkles: sparkles.get(`${row},${col}`),
                            })
                        } else if (cellShineEffects) {
                            cellShineEffects.delete(`${minRow},${minCol},${maxRow},${maxCol}`)
                            if (cellShineEffects.size === 0) {
                                this.cellShineEffects.delete(`${row},${col}`)
                            }
                        }
                    }
                },
            })
        } else if (type === 'shatter') {
            let i = 0

            shuffleArray(cells)

            for (const [row, col] of cells) {
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

                i++
            }
        }
    }


    startColorMapTransition() {
        this.oldColorMapOpacity = 1
        this.addAnimation({
            id: 'color-map-transition',
            startTime: performance.now(),
            duration: 500,
            onUpdate: (t: number) => {
                this.oldColorMapOpacity = 1 - easing.easeInOutQuad(t)
            },
            onComplete: () => {
                this.oldColorMap.clear()
                this.oldColorMapOpacity = 0
            },
        })
    }


    startDiscoTrapAnimation() {
        this.discoTrapSpotlights = []
        const numSpotlights = Math.floor(Math.sqrt(this.clientWidth) / 3) + 3

        for (let i = 0; i < numSpotlights; i++) {
            const originXRatio = 0.025 + 0.95 * (i / (numSpotlights - 1)) + (Math.random() - 0.5) * 0.05
            const angle = Math.atan2(
                -this.clientHeight * 2,
                (this.clientWidth / 2) - (originXRatio * this.clientWidth)
            )
            const sweepRange = Math.PI / 8
            const hue = Math.floor(Math.random() * 360)
            const hueDirection = Math.random() > 0.5 ? 1 : -1
            const timeOffset = Math.random()
            const lengthRatio = 0.2 + Math.random() * 0.2
            const spotlight = {
                id: i,
                originXRatio: originXRatio,
                lengthRatio: 0,
                widthAngle: Math.random() * (Math.PI / 8) + (Math.PI / 16),
                angle: angle,
                hue: hue,
            }
            this.discoTrapSpotlights.push(spotlight)

            this.addAnimation({
                id: `disco-trap-${i}`,
                startTime: performance.now(),
                duration: 8000 + Math.random() * 4000,
                loop: true,
                onUpdate: (t: number) => {
                    spotlight.angle = angle + sweepRange * Math.sin((t + timeOffset) * Math.PI * 2)
                    spotlight.hue = (hue + t * 360 * hueDirection) % 360
                }
            })
            this.addAnimation({
                id: `disco-trap-start-${i}`,
                startTime: performance.now(),
                duration: 2000,
                onUpdate: (t: number) => {
                    spotlight.lengthRatio = lengthRatio * t
                }
            })
        }
    }


    stopDiscoTrapAnimation() {
        for (const spotlight of this.discoTrapSpotlights) {
            const lengthRatio = spotlight.lengthRatio
            this.addAnimation({
                id: `disco-trap-stop-${spotlight.id}`,
                startTime: performance.now(),
                duration: 2000,
                onUpdate: (t: number) => {
                    spotlight.lengthRatio = lengthRatio * (1 - t)
                },
                onComplete: () => {
                    if (!this.discoTrap) {
                        this.animations.delete(`disco-trap-${spotlight.id}`)
                        this.discoTrapSpotlights = []
                    }
                },
            })
        }
    }


    startTunnelVisionTrapAnimation() {
        this.addAnimation({
            id: 'tunnel-vision-trap',
            startTime: performance.now(),
            duration: 1000,
            onUpdate: (t: number) => {
                this.tunnelVisionTrapRadius = 8 - 7 * easing.easeOut(t)
                this.tunnelVisionTrapOpacity = 0.5 * easing.easeOut(t)
            }
        })
    }


    stopTunnelVisionTrapAnimation() {
        this.addAnimation({
            id: 'tunnel-vision-trap',
            startTime: performance.now(),
            duration: 1000,
            onUpdate: (t: number) => {
                this.tunnelVisionTrapRadius = 1 + 7 * easing.easeIn(t)
                this.tunnelVisionTrapOpacity = 0.5 - 0.5 * easing.easeIn(t)
            }
        })
    }


    startFireworksTimer() {
        if (!this.fireworks) {
            return
        }

        this.startFireworkAnimation()

        this.addAnimation({
            id: 'fireworks-timer',
            startTime: performance.now(),
            duration: 200 + Math.random() * 1300,
            onUpdate: (progress: number) => {},
            onComplete: () => {
                this.startFireworksTimer()
            },
        })
    }


    startFireworkAnimation() {
        this.fireworkAnimationCounter += 1

        const blastParticles: BlastParticle[] = []
        const trailParticles = new Map<number, TrailParticle>()
        const hue = Math.floor(Math.random() * 360)
        const hue2 = hue + 30 + Math.floor(Math.random() * 300)
        const blastX = this.clientWidth! * Math.random() * 0.8 + this.clientWidth! * 0.1
        const blastY = this.clientHeight! * Math.random() * 0.5 + this.clientHeight! * 0.1
        const minBlastSize = Math.pow(this.clientWidth!, 0.45) / 10
        const maxBlastSize = minBlastSize * 1.5
        const blastSize = minBlastSize + Math.random() * (maxBlastSize - minBlastSize)
        const particleCount = Math.floor(60 + blastSize * 120)
        const startX = Math.min(this.clientWidth!, Math.max(0, blastX - 150 + Math.random() * 300))
        const startY = this.clientHeight!
        const colorVariant = Math.floor(Math.random() * 4)
        const gravity = 100
        const airResistance = 0.6
        const dragExp = 10
        const rocketAngle = Math.atan2(blastY - startY, blastX - startX)
        const trailAngleVar = Math.floor(Math.random() * Math.PI / 4 - Math.PI / 8)
        const trailWaveMult = 15 + Math.random() * 20
        const totalTrailParticles = (this.clientHeight! - blastY) / 2.5
        const goldenRatio = (1 + Math.sqrt(5)) / 2
        const rocket = {
            x: startX,
            y: startY,
        }
        let lastT = 0
        let progressSinceLast = 0

        for (let i = 0; i < particleCount; i++) {
            const z = 1 - (i / (particleCount - 1)) * 2
            const radiusAtZ = Math.sqrt(1 - z * z)
            const theta = (Math.PI * 2 * goldenRatio) * i
            const dirX = Math.cos(theta) * radiusAtZ
            const dirY = Math.sin(theta) * radiusAtZ
            const dirZ = z
            const speed = blastSize * (0.98 + Math.random() * 0.04) * 25
            const baseSize = 1.5 + Math.random()

            this.fireworkParticleCounter += 1
            const particle = {
                id: this.fireworkParticleCounter,
                x: blastX,
                y: blastY,
                startX: blastX,
                startY: blastY,
                vx: dirX * speed,
                vy: dirY * speed,
                vz: dirZ * speed,
                size: baseSize,
                initialSize: baseSize,
                hue: colorVariant == 0 ? hue
                    : colorVariant == 1 ? (i % 2 === 0 ? hue : hue2)
                    : colorVariant == 2 ? (Math.abs(dirZ) > 0.7 ? hue : hue2)
                    : (Math.abs(dirZ) > 0.9 || Math.abs(dirZ) < 0.4 ? hue : hue2),
                progress: 0,
                deathT: 0.85 + Math.random() * 0.15,
            }

            blastParticles.push(particle)
            this.fireworkParticles.set(particle.id, particle)
        }

        this.addAnimation({
            id: 'fireworks-' + this.fireworkAnimationCounter,
            startTime: performance.now(),
            duration: 3500,
            onUpdate: (progress: number) => {
                const rocketT: number = easing.easeOutSin(mapRange(progress, 0, 0.45))

                const [rocketX, rocketY] = fireworkBezierPoint(rocketT, startX, startY, blastX, blastY)

                progressSinceLast += (rocketT - lastT)
                const numParticlesToSpawn = Math.floor(totalTrailParticles * progressSinceLast)

                for (let i = 0; i < numParticlesToSpawn; i++) {
                    progressSinceLast -= 1 / totalTrailParticles

                    const t = lastT + (rocketT - lastT) * (i / numParticlesToSpawn)
                    const [px, py] = fireworkBezierPoint(t, startX, startY, blastX, blastY)
                    const speed = this.clientHeight! / 100
                    const angle = rocketAngle - Math.PI
                        + (Math.sin(t * trailWaveMult) * trailAngleVar)

                    this.fireworkParticleCounter += 1
                    const particle = {
                        id: this.fireworkParticleCounter,
                        x: px,
                        y: py,
                        startX: px,
                        startY: py,
                        vx: Math.cos(angle) * speed,
                        vy: Math.sin(angle) * speed,
                        size: 2,
                        initialSize: 2,
                        hue: (colorVariant == 0 || this.fireworkParticleCounter % 2 === 0) ? hue : hue2,
                        progress: 0,
                        spawnT: progress,
                        deathT: progress + 0.1,
                    }
                    trailParticles.set(particle.id, particle)
                    this.fireworkParticles.set(particle.id, particle)
                }

                rocket.x = rocketX
                rocket.y = rocketY
                lastT = rocketT

                for (const particle of trailParticles.values()) {
                    const t = mapRange(progress, particle.spawnT, particle.deathT)
                    particle.x = particle.startX + particle.vx * t
                    particle.y = particle.startY + particle.vy * t
                    particle.size = particle.initialSize * (1 - t)
                    particle.progress = t
                    if (t >= 1) {
                        trailParticles.delete(particle.id)
                        this.fireworkParticles.delete(particle.id)
                    }
                }

                for (const particle of blastParticles) {
                    const t = mapRange(progress, 0.45, 1)
                    const dragT = (1 - Math.pow(airResistance, t * dragExp)) / (1 - airResistance)

                    const horizontalBlast = particle.vx * dragT
                    particle.x = particle.startX + horizontalBlast

                    const verticalBlast = particle.vy * dragT
                    const gravityDrop = 0.5 * gravity * Math.pow(t, 2)
                    particle.y = particle.startY + verticalBlast + gravityDrop

                    const depthBlast = particle.vz * dragT
                    const depthFactor = 1 + depthBlast * 0.003
                    const fadeFactor = 1 - mapRange(t, 0.5, particle.deathT)
                    particle.size = particle.initialSize * depthFactor * fadeFactor
                    particle.progress = t
                }
            },
            onComplete: () => {
                for (const particle of blastParticles) {
                    this.fireworkParticles.delete(particle.id)
                }
                for (const particle of trailParticles.values()) {
                    this.fireworkParticles.delete(particle.id)
                }
            }
        })
    }


    requestRender() {
        if (this.renderRequested || this.isAnimating) {
            return
        }

        this.renderRequested = true

        requestAnimationFrame((timestamp: number) => {
            this.render(timestamp)
            this.renderRequested = false
        })
    }


    addAnimation(animation: GameAnimation) {
        if (!this.animationsEnabled) {
            return
        }

        this.animations.set(animation.id, animation)

        if (!this.isAnimating) {
            this.isAnimating = true
            requestAnimationFrame(this.handleAnimationFrame)
        }
    }


    handleAnimationFrame = (timestamp: number) => {
        if (this.animations.size === 0) {
            this.isAnimating = false
            this.render(timestamp)

            return
        }

        this.updateAnimations(timestamp)
        this.render(timestamp)

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
                if (animation.loop) {
                    animation.startTime = timestamp
                } else {
                    this.animations.delete(id)
                }
                if (animation.onComplete) {
                    animation.onComplete()
                }
            }
        }
    }


    render(timestamp: number) {
        this.renderSprites()

        const { ctx, canvas, viewport } = this

        const dpr = window.devicePixelRatio || 1
        const width = canvas.width / dpr
        const height = canvas.height / dpr
        const spriteSize = this.getSpriteSize()

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
                this.renderCellBackground(ctx, row, col, this.colorMap)
            }
        }

        if (this.oldColorMapOpacity > 0) {
            ctx.save()
            ctx.globalAlpha = this.oldColorMapOpacity
            for (let row = startRow; row <= endRow; row++) {
                for (let col = startCol; col <= endCol; col++) {
                    this.renderCellBackground(ctx, row, col, this.oldColorMap)
                }
            }
            ctx.restore()
        }

        for (let row = startRow; row <= endRow; row++) {
            for (let col = startCol; col <= endCol; col++) {
                if (this.selectedCell
                    && this.selectedCell.row === row
                    && this.selectedCell.col === col
                ) {
                    continue
                }

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

            this.renderCellBackground(ctx, row, col, this.colorMap)
            if (this.oldColorMapOpacity > 0) {
                ctx.save()
                ctx.globalAlpha = this.oldColorMapOpacity
                this.renderCellBackground(ctx, row, col, this.oldColorMap)
                ctx.restore()
            }

            this.renderCell(ctx, row, col)

            if (this.cellShineEffects.has(`${row},${col}`)) {
                this.renderCellShine(ctx, row, col)
            }

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

        if (this.tunnelVisionTrapOpacity > 0) {
            this.renderTunnelVisionTrap(ctx)
        }

        if (this.discoTrapSpotlights.length > 0) {
            ctx.save()
            ctx.globalCompositeOperation = this.colorScheme === 'dark' ? 'lighten' : 'hard-light'
            for (const spotlight of this.discoTrapSpotlights) {
                this.renderDiscoTrapSpotlight(ctx, spotlight)
            }
            ctx.restore()
        }

        this.renderGridHeaders(ctx, startRow, endRow, startCol, endCol)
        this.renderFireworks(ctx, timestamp)

        ctx.restore()
    }


    renderCellBackground(ctx: CanvasRenderingContext2D, row: number, col: number, colorMap?: Map<number, number>) {
        const cell = this.cells.get(`${row},${col}`)

        if (!cell) {
            return
        }

        const cellSizeWithGap = this.cellSize + this.cellGap
        const spriteSize = this.getSpriteSize()
        const x = (col - 1) * cellSizeWithGap
        const y = (row - 1) * cellSizeWithGap

        const isActive = this.activeCell && this.activeCell.row === row && this.activeCell.col === col
        const isHovered = this.hoveredCell && this.hoveredCell.row === row && this.hoveredCell.col === col
        const spriteState = isActive ? 'active' : isHovered ? 'hover' : 'none'

        switch (cell.type) {
            case 'empty': {
                if (colorMap == this.oldColorMap) {
                    break
                }

                ctx.fillStyle = spriteState === 'hover' ? colors.cellBgHover[this.colorScheme]
                    : spriteState === 'active' ? colors.cellBgActive[this.colorScheme]
                    : colors.cellBg[this.colorScheme]
                ctx.fillRect(x, y, this.cellSize, this.cellSize)

                break
            }
            case 'hidden': {
                if (colorMap == this.oldColorMap) {
                    break
                }

                ctx.fillStyle = spriteState === 'hover' ? colors.cellHiddenHover[this.colorScheme]
                    : spriteState === 'active' ? colors.cellHiddenActive[this.colorScheme]
                    : colors.cellHidden[this.colorScheme]
                ctx.fillRect(x, y, this.cellSize, this.cellSize)

                break
            }
            case 'tunnel': {
                if (colorMap == this.oldColorMap) {
                    break
                }

                ctx.fillStyle = spriteState === 'hover' ? colors.cellTunnelHover[this.colorScheme]
                    : spriteState === 'active' ? colors.cellTunnelActive[this.colorScheme]
                    : colors.cellTunnel[this.colorScheme]
                ctx.fillRect(x, y, this.cellSize, this.cellSize)

                break
            }
            case 'given':
            case 'single': {
                const spriteType = cell.type === 'given' ? 'given' : 'userValue'
                const bgNumber = colorMap?.get(cell.number) ?? cell.number
                const sourceX = (bgNumber - 1) * spriteSize
                const sourceY = getSpriteY(spriteSize, spriteType, spriteState)
                const cellError = this.cellErrors.get(`${row},${col}`)
                const numberState = cellError ? 'error' : 'none'

                if (spriteType === 'userValue' && colorMap != this.oldColorMap) {
                    ctx.fillStyle = spriteState === 'hover' ? colors.cellBgHover[this.colorScheme]
                        : spriteState === 'active' ? colors.cellBgActive[this.colorScheme]
                        : colors.cellBg[this.colorScheme]
                    ctx.fillRect(x, y, this.cellSize, this.cellSize)
                }

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

                break
            }
            case 'candidates': {
                if (colorMap != this.oldColorMap) {
                    ctx.fillStyle = spriteState === 'hover' ? colors.cellBgHover[this.colorScheme]
                        : spriteState === 'active' ? colors.cellBgActive[this.colorScheme]
                        : colors.cellBg[this.colorScheme]
                    ctx.fillRect(x, y, this.cellSize, this.cellSize)
                }

                for (const number of cell.numbers) {
                    const bgNumber = colorMap?.get(number) ?? number
                    const sourceX = (bgNumber - 1) * spriteSize
                    const sourceY = getSpriteY(spriteSize, 'userValue', spriteState)
                    const subX = x + this.candidateSubXMap.get(number)!
                    const subY = y + this.candidateSubYMap.get(number)!

                    ctx.drawImage(
                        this.spriteCanvas,
                        sourceX,
                        sourceY,
                        spriteSize,
                        spriteSize,
                        subX,
                        subY,
                        this.candidateSubSize,
                        this.candidateSubSize
                    )
                }

                break
            }
        }
    }


    renderCell(ctx: CanvasRenderingContext2D, row: number, col: number) {
        const cell = this.cells.get(`${row},${col}`)

        if (!cell) {
            return
        }

        const cellSizeWithGap = this.cellSize + this.cellGap
        const spriteSize = this.getSpriteSize()
        const x = (col - 1) * cellSizeWithGap
        const y = (row - 1) * cellSizeWithGap

        switch (cell.type) {
            case 'empty': {
                if (cell.dimmed) {
                    ctx.fillStyle = colors.cellDimmed[this.colorScheme]
                    ctx.fillRect(x, y, this.cellSize, this.cellSize)
                }

                break
            }
            case 'hidden': {
                if (cell.dimmed) {
                    ctx.fillStyle = colors.cellDimmed[this.colorScheme]
                    ctx.fillRect(x, y, this.cellSize, this.cellSize)
                }

                break
            }
            case 'given':
            case 'single': {
                const spriteType = cell.type === 'given' ? 'givenNumber' : 'userValueNumber'
                const cellError = this.cellErrors.get(`${row},${col}`)
                const numberState = cellError ? 'error' : 'none'
                const sourceX = (cell.number - 1) * spriteSize
                const sourceY = getSpriteY(spriteSize, spriteType, numberState)

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
                for (const number of cell.numbers) {
                    const cellError = this.cellErrors.get(`${row},${col},${number}`)
                    const numberState = cellError ? 'error' : 'none'
                    const sourceX = (number - 1) * spriteSize
                    const sourceY = getSpriteY(spriteSize, 'candidateNumber', numberState)

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

                    if (cellError) {
                        this.renderCandidateError(ctx, row, col, number, cellError)
                    }

                    if (!cell.dimmed && cell.dimmedNumbers.includes(number)) {
                        this.renderCandidateDimmedOverlay(ctx, x, y, number)
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
        const rows = this.blockRows
        const columns = this.blockCols
        const size = this.cellSize
        const cellX = (col - 1) * (this.cellSize + this.cellGap)
        const cellY = (row - 1) * (this.cellSize + this.cellGap)
        const subX = cellX + size / (columns * 2) + this.candidateSubXMap.get(value)!
        const subY = cellY + size / (rows * 2) + this.candidateSubYMap.get(value)!
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
        const effectsMap = this.cellShineEffects.get(`${row},${col}`)!
        const cellX = (col - 1) * (this.cellSize + this.cellGap)
        const cellY = (row - 1) * (this.cellSize + this.cellGap)

        for (const effects of effectsMap.values()) {
            this.renderShineEffect(
                ctx,
                cellX,
                cellY,
                effects.offset
            )

            for (const sparkle of effects.sparkles.values()) {
                this.renderSparkleEffect(
                    ctx,
                    cellX + sparkle.x,
                    cellY + sparkle.y,
                    sparkle.angle,
                    sparkle.direction,
                    sparkle.progress,
                )
            }
        }
    }


    renderShineEffect(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        offset: number
    ) {
        const size = this.cellSize;

        ctx.save();

        ctx.beginPath();
        ctx.rect(x, y, size, size);
        ctx.clip();

        const lineWidth = size / 2;

        const gradient = ctx.createLinearGradient(0, 0, lineWidth, 0);
        gradient.addColorStop(0, "rgba(255, 255, 255, 0)");
        gradient.addColorStop(0.5, "rgba(255, 255, 255, 0.8)");
        gradient.addColorStop(1, "rgba(255, 255, 255, 0)");
        ctx.fillStyle = gradient;

        ctx.translate(x + offset, y - size * 0.4);
        ctx.rotate(Math.PI / 4);

        ctx.fillRect(0, 0, lineWidth, size * 2)

        ctx.restore();
    }


    renderSparkleEffect(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        angle: number,
        direction: number,
        progress: number
    ) {
        if (progress <= 0 || progress >= 1) {
            return
        }

        const spriteSize = this.getSpriteSize()
        const sparkleSize = this.cellSize * 0.3 * (0.2 + 0.8 * Math.sin(progress * Math.PI))

        ctx.save()

        ctx.translate(x, y)
        ctx.rotate(progress * Math.PI * 0.75 * direction + angle)
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
        frame: 0 | 1 | 2 | 3,
    ) {
        const spriteSize = this.getSpriteSize()

        ctx.fillStyle = colors.cellHidden[this.colorScheme]
        ctx.fillRect(x, y, this.cellSize, this.cellSize)

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
            this.cellSize,
            this.cellSize
        )
    }

    renderShardEffect(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        shard: 1 | 2 | 3 | 4 | 5 | 6,
        angle: number,
        speed: number,
        rotation: number,
        progress: number,
    ) {
        if (progress <= 0 || progress >= 1) {
            return
        }

        const size = this.cellSize
        const spriteSize = this.getSpriteSize()
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


    renderDiscoTrapSpotlight(
        ctx: CanvasRenderingContext2D,
        spotlight: Spotlight,
    ) {
        const dpr = window.devicePixelRatio || 1
        const startX = (this.clientWidth * spotlight.originXRatio - this.viewport.x) / this.viewport.scale
        const startY = (this.clientHeight - this.viewport.y + 25) / this.viewport.scale

        const length = this.clientHeight * spotlight.lengthRatio / this.viewport.scale
        const leftAngle = spotlight.angle - (spotlight.widthAngle / 2)
        const rightAngle = spotlight.angle + (spotlight.widthAngle / 2)

        const endLeftX = startX + Math.cos(leftAngle) * length
        const endLeftY = startY - Math.sin(leftAngle) * length
        const endRightX = startX + Math.cos(rightAngle) * length
        const endRightY = startY - Math.sin(rightAngle) * length

        ctx.beginPath()
        ctx.moveTo(startX, startY)
        ctx.arc(startX, startY, length, leftAngle, rightAngle, false)
        ctx.closePath()

        const gradient = ctx.createRadialGradient(
            startX,
            startY,
            0,
            startX,
            startY,
            length
        )
        const hue = spotlight.hue
        const lum0 = this.colorScheme === 'dark' ? 100 : 80
        const lum = this.colorScheme === 'dark' ? 60 : 30
        gradient.addColorStop(0, `hsl(${hue} 100 ${lum0} / 1)`)
        gradient.addColorStop(0.3, `hsl(${hue} 100 ${lum} / 0.6)`)
        gradient.addColorStop(0.6, `hsl(${hue} 100 ${lum} / 0.2)`)
        gradient.addColorStop(1, `hsl(${hue} 100 ${lum} / 0)`)
        ctx.fillStyle = gradient
        ctx.fill()
    }


    renderTunnelVisionTrap(
        ctx: CanvasRenderingContext2D,
    ) {
        if (!this.selectedCell) {
            return
        }

        const row = this.selectedCell.row
        const col = this.selectedCell.col
        const cellSizeWithGap = this.cellSize + this.cellGap
        const centerX = (col - 1) * cellSizeWithGap + this.cellSize / 2
        const centerY = (row - 1) * cellSizeWithGap + this.cellSize / 2
        const blockSizeRadius = {
            4: this.cellSize * 1.6,
            6: this.cellSize * 2.2,
            8: this.cellSize * 2.2,
            9: this.cellSize * 2.2,
            12: this.cellSize * 2.8,
            16: this.cellSize * 3.2,
        }
        const innerRadius = blockSizeRadius[this.blockSize as keyof typeof blockSizeRadius]
            * this.tunnelVisionTrapRadius
        const outerRadius = innerRadius * 1.5

        const gradient = ctx.createRadialGradient(
            centerX,
            centerY,
            innerRadius,
            centerX,
            centerY,
            outerRadius
        )
        gradient.addColorStop(0.7, 'hsl(0 0 0 / 0)')
        gradient.addColorStop(1, `hsl(0 0 0 / ${this.tunnelVisionTrapOpacity})`)

        ctx.fillStyle = gradient
        ctx.fillRect(
            -this.viewport.x / this.viewport.scale,
            -this.viewport.y / this.viewport.scale,
            this.clientWidth / this.viewport.scale,
            this.clientHeight / this.viewport.scale
        )
    }


    renderFireworks(
        ctx: CanvasRenderingContext2D,
        timestamp: number,
    ) {
        if (!this.canvas.width || !this.canvas.height) {
            return
        }

        const dpr = window.devicePixelRatio || 1
        const fCtx = this.fireworksCtx

        const fadeDiff = timestamp - this.lastFireworkFadeTime
        const applyFade = fadeDiff >= 20

        if (applyFade) {
            this.lastFireworkFadeTime = timestamp - (fadeDiff % 20)
            this.fireworkFadeCounter += 1
            const alpha = this.fireworkFadeCounter % 50 == 0 ? 0.5 : 0.3
            fCtx.globalCompositeOperation = 'destination-out'
            fCtx.fillStyle = `hsl(0 0 0 / ${alpha})`
            fCtx.fillRect(0, 0, this.fireworksCanvas.width, this.fireworksCanvas.height)
        }

        fCtx.globalCompositeOperation = this.colorScheme === 'dark' ? 'lighter' : 'source-over'

        for (const particle of this.fireworkParticles.values()) {
            if (particle.progress == 0) {
                continue
            }
            const baseLightness = this.colorScheme === 'dark' ? 65 : 55
            const lightness = baseLightness - (particle.progress * 20)
            fCtx.fillStyle = `hsl(${particle.hue} 100 ${lightness})`
            fCtx.fillRect(particle.x, particle.y, particle.size, particle.size / dpr)
        }

        const viewportX = -this.viewport.x / this.viewport.scale - this.cellGap
        const viewportY = -this.viewport.y / this.viewport.scale - this.cellGap

        ctx.save()
        ctx.globalCompositeOperation = this.colorScheme === 'dark' ? 'lighter' : 'source-over'
        ctx.drawImage(
            this.fireworksCanvas,
            0,
            0,
            this.fireworksCanvas.width,
            this.fireworksCanvas.height,
            viewportX,
            viewportY,
            this.canvas.width / this.viewport.scale / dpr,
            this.canvas.height / this.viewport.scale / dpr,
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
        const spriteSize = this.getSpriteSize()
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
                viewportX - this.cellSize - (this.borderWidth - this.cellGap),
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
                viewportY - this.cellSize - (this.borderWidth - this.cellGap),
                this.cellSize * 3,
                this.cellSize * 3
            )
        }
    }


    renderSprites() {
        this.spriteScale = Math.max(Math.round(this.viewport.scale * 4) / 4, 0.25)
        const dpr = window.devicePixelRatio || 1

        if (this.renderedSpriteScale === this.spriteScale && this.renderedDpr === dpr) {
            return
        }
        this.renderedSpriteScale = this.spriteScale
        this.renderedDpr = dpr

        const ctx = this.spriteCtx
        const canvas = this.spriteCanvas

        const cellSize = this.cellSize * this.spriteScale
        const columns = Math.max(this.blockSize, 9)
        const rows = 15

        canvas.width = cellSize * dpr * columns
        canvas.height = cellSize * dpr * rows
        canvas.style.width = `${cellSize * columns}px`
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

        for (let number = 1; number <= this.blockSize; number++) {
            const x = (number - 1) * cellSize

            this.renderGivenSprite(ctx, x, spriteY('given'), cellSize, number, 'regular')
            this.renderGivenSprite(ctx, x, spriteY('given', 'hover'), cellSize, number, 'hover')
            this.renderGivenSprite(ctx, x, spriteY('given', 'active'), cellSize, number, 'active')
            this.renderGivenNumberSprite(ctx, x, spriteY('givenNumber'), cellSize, number, 'regular')
            this.renderGivenNumberSprite(ctx, x, spriteY('givenNumber', 'error'), cellSize, number, 'error')
            this.renderUserValueSprite(ctx, x, spriteY('userValue'), cellSize, number, 'regular')
            this.renderUserValueSprite(ctx, x, spriteY('userValue', 'hover'), cellSize, number, 'hover')
            this.renderUserValueSprite(ctx, x, spriteY('userValue', 'active'), cellSize, number, 'active')
            this.renderUserValueNumberSprite(ctx, x, spriteY('userValueNumber'), cellSize, number, 'regular')
            this.renderUserValueNumberSprite(ctx, x, spriteY('userValueNumber', 'error'), cellSize, number, 'error')
            this.renderCandidateNumberSprite(ctx, x, spriteY('candidateNumber'), cellSize, number, 'regular')
            this.renderCandidateNumberSprite(ctx, x, spriteY('candidateNumber', 'error'), cellSize, number, 'error')
        }

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

        this.renderSelectionSprite(ctx, spriteX('selection'), spriteY('selection'), cellSize)

        this.renderHeaderSprites()
    }


    renderGivenSprite(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
        number: number,
        state: CellState = 'regular',
    ) {
        const gradient = ctx.createLinearGradient(x, y, x, y + size)
        const color = getCellColor(this.blockSize, number, this.colorScheme, state)
        gradient.addColorStop(0, color[0])
        gradient.addColorStop(1, color[1])
        ctx.fillStyle = gradient
        ctx.fillRect(x, y, size, size)
    }


    renderGivenNumberSprite(
        ctx: CanvasRenderingContext2D,
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


    renderUserValueSprite(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
        number: number,
        state: CellState = 'regular',
    ) {
        const gradient = ctx.createLinearGradient(x, y, x, y + size)
        const color = getCellColor(this.blockSize, number, this.colorScheme, state)
        gradient.addColorStop(0, color[0])
        gradient.addColorStop(1, color[1])
        ctx.fillStyle = gradient
        ctx.beginPath()
        ctx.moveTo(x + size / 2, y)
        ctx.arc(x + size / 2, y + size / 2, (size / 2) * 0.95, 0, Math.PI * 2)
        ctx.fill()
    }


    renderUserValueNumberSprite(
        ctx: CanvasRenderingContext2D,
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


    renderCandidateNumberSprite(
        ctx: CanvasRenderingContext2D,
        x: number,
        y: number,
        size: number,
        value: number,
        state: NumberState = 'regular',
    ) {
        const fontSize = size * candidateFontSizeMultiplier[this.blockSize as keyof typeof candidateFontSizeMultiplier]
        const rows = this.blockRows
        const columns = this.blockCols
        const subX = x + size / (columns * 2) + (size / columns) * ((value - 1) % columns)
        const subY = this.candidateLayout === 0
            ? (y + size / (rows * 2) + (size / rows) * Math.floor((value - 1) / columns))
            : (y + size - (size / (rows * 2)) - (size / rows) * Math.floor((value - 1) / columns))

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
        value: number,
    ) {
        const size = this.cellSize
        const rows = this.blockRows
        const columns = this.blockCols
        const subX = x + size / (columns * 2) + this.candidateSubXMap.get(value)!
        const subY = y + size / (rows * 2) + this.candidateSubYMap.get(value)!

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


    renderSelectionSprite(
        ctx: CanvasRenderingContext2D,
        cornerX: number,
        cornerY: number,
        size: number,
    ) {
        const dpr = window.devicePixelRatio || 1
        const x = cornerX + size
        const y = cornerY + size

        ctx.save()

        ctx.beginPath()
        ctx.rect(cornerX, cornerY, size * 3, size * 3)
        ctx.rect(x, y, size, size)
        ctx.clip('evenodd')

        const blurAmount = 4 * this.spriteScale * dpr
        ctx.shadowColor = colors.selection[this.colorScheme]
        ctx.shadowBlur = blurAmount
        ctx.strokeStyle = colors.selection[this.colorScheme]
        ctx.fillStyle = colors.selection[this.colorScheme]

        for (let i = 0; i < 4; i++) {
            ctx.shadowBlur += blurAmount
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

        const spriteSize = this.getSpriteSize()
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

            colCtx.fillStyle = colors.cellBg[this.colorScheme]
            colCtx.fillRect(x, y, cellSize, cellSize)

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
        const rowHeight = cellSizeWithGap * this.boardRows + padding * 2
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
            cellSizeWithGap * this.boardRows + cellGap
        )
        rowCtx.restore()

        // Row grid
        rowCtx.fillStyle = colors.grid[this.colorScheme]
        rowCtx.fillRect(
            padding,
            padding,
            cellSize,
            cellSizeWithGap * this.boardRows
        )

        // Row labels
        for (let row = 1; row <= this.boardRows; row++) {
            const x = 0 + padding
            const y = (row - 1) * cellSizeWithGap + padding

            rowCtx.fillStyle = colors.cellBg[this.colorScheme]
            rowCtx.fillRect(x, y, cellSize, cellSize)

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
            cellSizeWithGap * this.boardRows + cellGap
        )
    }


    getSpriteSize(): number {
        return this.cellSize * this.spriteScale * (window.devicePixelRatio || 1)
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
