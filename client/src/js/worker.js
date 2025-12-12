import * as generator from './generator.ts'


let lastProgress = Date.now()


self.onmessage = function(event) {
    console.log(event.data.type, event.data.data)

    switch (event.data.type) {
        case 'generateBoard':
            generate(event.data.data)

            break

        default:
            console.log('Unknown message type:', event.data.type)
    }
}


function generate(args) {
    let state = generator.initGeneration(args)

    while (true) {
        switch (state.type) {
            case 'Completed':
                self.postMessage({
                    type: 'sendBoard',
                    data: state
                })

                return

            case 'Failed':
                return

            case 'PlacingNumbers':
                sendProgress(state)

                break

            case 'RemovingGivens':
                sendProgress(state)
                break
        }

        state = generator.generate(state)
    }
}


function sendProgress(state) {
    if (Date.now() - lastProgress < 100) {
        return
    }

    lastProgress = Date.now()
    let totalBoards = state.state.allClusters
        .reduce((sum, cluster) => sum + cluster.length, 0)
    let remainingBoards = state.state.remainingClusters
        .reduce((sum, cluster) => sum + cluster.length, 0)

    let label, percent

    switch (state.type) {
        case 'PlacingNumbers':
            label = 'Placing numbers'
            percent = 100 - (remainingBoards / totalBoards) * 100

            break

        case 'RemovingGivens':
            label = 'Removing givens'
            percent = 100 - (remainingBoards / totalBoards) * 100

            break

        case 'RestoringGivens':
            label = 'Restoring givens'
            percent = 100 - (remainingBoards / totalBoards) * 100

            break
    }

    let data = {
        label: label,
        percent: percent,
    }

    self.postMessage({ type: 'sendProgress', data: data })
}
