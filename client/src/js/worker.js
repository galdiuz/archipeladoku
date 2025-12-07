import { Elm } from '../Archipeladoku/Worker.elm';

const worker = Elm.Archipeladoku.Worker.init({
    flags: {}
});

self.onmessage = function(event) {
    console.log(event.data.type, event.data.data);

    switch(event.data.type) {
        case 'generateBoard':
            worker.ports.receiveGenerateArgs.send(event.data.data);

            break;

        case 'generateFromServer':
            worker.ports.receiveGenerateArgs2.send(event.data.data);

            break;

        default:
            console.log('Unknown message type:', event.data.type);
    }
};

worker.ports.sendBoard.subscribe(function(data) {
    self.postMessage({ type: 'sendBoard', data: data });
});

worker.ports.sendProgress.subscribe(function(data) {
    self.postMessage({ type: 'sendProgress', data: data });
});

worker.ports.log?.subscribe(function(text) {
    console.log('[Worker]', text);
});
