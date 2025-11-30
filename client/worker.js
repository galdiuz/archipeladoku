importScripts('app.js');

const app = Elm.Archipeladoku.Worker.init({
    flags: {}
});

self.onmessage = function(event) {
    console.log(event.data.type, event.data.data);

    switch(event.data.type) {
        case 'generateBoard':
            app.ports.receiveGenerateArgs.send(event.data.data);

        case 'generateFromServer':
            app.ports.receiveGenerateArgs2.send(event.data.data);

            break;

        default:
            console.log('Unknown message type:', event.data.type);
    }
};

app.ports.sendBoard.subscribe(function(data) {
    self.postMessage({ type: 'sendBoard', data: data });
});
