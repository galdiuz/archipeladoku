import Panzoom from 'panzoom'

class PanzoomBoardWrapper extends HTMLElement {
    constructor() {
        super()
        this.panzoomInstance = null
    }

    connectedCallback() {
        const board = this.querySelector('.board-cells')
        const corner = this.querySelector('.board-corner')
        const rows = this.querySelector('.board-rows-header')
        const cols = this.querySelector('.board-columns-header')

        if (!board || !rows || !cols) {
            return
        }

        this.panzoomInstance = Panzoom(board, {
            maxZoom: 3,
            minZoom: 0.20,
            startScale: 1,
            bounds: true,
            boundsPadding: 0.2,
            zoomDoubleClickSpeed: 1,
            filterKey: () => true,
            onTouch: () => false,
        })

        this.addEventListener('wheel', this.panzoomInstance.zoomWithWheel)

        this.panzoomInstance.on('transform', (e) => {
            const { x, y, scale } = e.getTransform()

            corner.style.transform = `scale(${scale})`
            cols.style.transform = `translate(${x}px, 0) scale(${scale})`
            rows.style.transform = `translate(0, ${y}px) scale(${scale})`
        })
    }

    disconnectedCallback() {
        if (this.panzoomInstance) {
            this.removeEventListener('wheel', this.panzoomInstance.zoomWithWheel)
            this.panzoomInstance = null
        }
    }
}

customElements.define('panzoom-board-wrapper', PanzoomBoardWrapper)
