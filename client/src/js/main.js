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
        client.scout(ids, 2)
            .then(scoutedItems => {
                let data = [];
                for (let item of scoutedItems) {
                    data.push({
                        locationId: item.locationId,
                        locationName: item.locationName,
                        itemId: item.id,
                        itemName: item.name,
                        playerName: item.receiver.alias,
                        gameName: item.game,
                        itemClass: item.flags,
                    });
                }
                ui.ports.receiveScoutedItems.send(data);
            });
    } catch (error) {
        console.error('Scout location error:', error);
    }
});

ui.ports.hintForItem && ui.ports.hintForItem.subscribe(itemName => {
    client.messages.say(`!hint ${itemName}`);
});

client.items.on('itemsReceived', items => {
    ui.ports.receiveItems.send(items.map(item => item.id));
});

client.room.on('locationsChecked', locations => {
    ui.ports.receiveCheckedLocations.send(locations);
});

client.room.on('hintPointsUpdated', (oldValue, newValue) => {
    ui.ports.receiveHintPoints.send(newValue);
});

client.room.on('hintCostUpdated', (oldCost, newCost) => {
    ui.ports.receiveHintCost.send(newCost);
});

client.items.on('hintsInitialized', hints => {
    let data = [];
    for (let hint of hints) {
        if (hint.item.receiver.name != client.name) {
            continue;
        }
        data.push({
            locationId: hint.item.locationId,
            locationName: hint.item.locationName,
            itemId: hint.item.id,
            itemName: hint.item.name,
            playerName: hint.item.sender.alias,
            gameName: hint.item.game,
            itemClass: hint.item.flags,
        });
    }
    ui.ports.receiveHints.send(data);
});

client.items.on('hintReceived', _ => {
    let data = [];
    for (let hint of client.items.hints) {
        if (hint.item.receiver.name != client.name) {
            continue;
        }
        data.push({
            locationId: hint.item.locationId,
            locationName: hint.item.locationName,
            itemId: hint.item.id,
            itemName: hint.item.name,
            playerName: hint.item.sender.alias,
            gameName: hint.item.game,
            itemClass: hint.item.flags,
        });
    }
    ui.ports.receiveHints.send(data);
});

client.messages.on('message', message => {
    console.log('Message received:', message);
    // ui.ports.receiveMessage.send(message);
});

ui.ports.connect && ui.ports.connect.subscribe(data => {
    client.login(data.host, data.player, 'Archipeladoku', { password: data.password })
        .then(slotData => {
            console.log('Slot Data:', slotData);
            ui.ports.receiveHintCost.send(client.room.hintCost);
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
