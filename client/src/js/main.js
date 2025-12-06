import * as Archipelago from 'archipelago.js';
import { Elm } from '../Archipeladoku/UI.elm';
import Worker from './worker.js?worker';

const client = new Archipelago.Client();
window.client = client; // For debugging purposes
let uiContainer = document.getElementById('ui-container');

let ui = Elm.Archipeladoku.UI.init({
    node: uiContainer,
    flags: {
        seed: Math.floor(Math.random() * 2147483647)
    }
});
let worker = new Worker(Worker);

ui.ports.generateBoard && ui.ports.generateBoard.subscribe(data => {
    worker.postMessage({ type: 'generateBoard', data: data });
});

ui.ports.checkLocation && ui.ports.checkLocation.subscribe(data => {
    try {
        client.check(data);
    } catch (error) {
        console.error('Check location error:', error);
    }
});

ui.ports.scoutLocations && ui.ports.scoutLocations.subscribe(ids => {
    try {
        client.scout(ids)
            .then(scoutedItems => {
                let data = [];
                for (let item of scoutedItems) {
                    data.push({
                        locationId: item.locationId,
                        locationName: item.locationName,
                        itemName: item.name,
                        playerName: item.receiver.alias,
                        gameName: item.game,
                        itemClass: item.flags,
                    });
                }
                console.log('Scouted items', data)
                ui.ports.receiveScoutedItems.send(data);
            });
    } catch (error) {
        console.error('Scout location error:', error);
    }
});

client.items.on('itemsReceived', items => {
    console.log('Received items:', items.map(item => item.name));
    ui.ports.receiveItems.send(items.map(item => item.id));
});

client.room.on('locationsChecked', locations => {
    console.log('Locations checked:', locations);
    ui.ports.receiveCheckedLocations.send(locations);
});

ui.ports.connect && ui.ports.connect.subscribe(data => {
    client.login(data.host, data.player, 'Archipeladoku', { password: data.password })
        .then(slotData => {
            console.log('Slot Data:', slotData);
            worker.postMessage({ type: 'generateFromServer', data: slotData });
        })
        .catch(error => {
            console.error('Connection error:', error);
        });
})

worker.onmessage = function(event) {
    console.log(event.data.type, event.data.data);

    switch(event.data.type) {
        case 'sendBoard':
            ui.ports.receiveBoard.send(event.data.data);

            break;

        default:
            console.log('Unknown message type:', event.data.type);
    }
};
