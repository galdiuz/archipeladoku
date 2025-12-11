import * as generator from './generator.ts';


self.onmessage = function(event) {
    console.log(event.data.type, event.data.data);

    switch (event.data.type) {
        case 'generateBoard':
            generate(event.data.data)

            break;

        case 'generateFromServer':
            // worker.ports.receiveGenerateArgs2.send(event.data.data);

            break;

        default:
            console.log('Unknown message type:', event.data.type);
    }
};


function generate(args) {
    let state = generator.initGeneration(args)
    console.log('Initial generation state:', state);

    while (true) {
        console.log('Generation state:', state);
        switch (state.type) {
            case 'Completed':
                self.postMessage({
                    type: 'sendBoard',
                    data: state
                });

                return

            case 'Failed':
                return

            case 'PlacingNumbers':
                sendProgress(state);

                break

            case 'RemovingGivens':
                sendProgress(state);
                break
        }

        state = generator.generate(state);
    }
}


function sendProgress(state) {
    let totalBoards = state.state.allClusters
        .reduce((sum, cluster) => sum + cluster.length, 0)
    let remainingBoards = state.state.remainingClusters
        .reduce((sum, cluster) => sum + cluster.length, 0)

    let label, percent

    switch (state.type) {
        case 'PlacingNumbers':
            label = 'Placing numbers';
            percent = 34 - (remainingBoards / totalBoards) * 34;

            break

        case 'RemovingGivens':
            label = 'Removing givens';
            percent = 67 - (remainingBoards / totalBoards) * 33;

            break

        case 'RestoringGivens':
            label = 'Restoring givens';
            percent = 100 - (remainingBoards / totalBoards) * 33;

            break
    }

    let data = {
        label: label,
        percent: percent,
    }

    self.postMessage({ type: 'sendProgress', data: data });
}
