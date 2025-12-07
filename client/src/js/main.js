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

worker.onmessage = function(event) {
    // console.log(event.data.type, event.data.data);

    switch(event.data.type) {
        case 'sendBoard':
            ui.ports.receiveBoard.send(event.data.data);

            break;

        case 'sendProgress':
            ui.ports.receiveGenerationProgress.send(event.data.data);

            break;

        default:
            console.log('Unknown message type:', event.data.type);
    }
};

ui.ports.connect?.subscribe(data => {
    client.login(data.host, data.player, 'Archipeladoku', { password: data.password })
        .then(slotData => {
            console.log('Slot Data:', slotData);
            ui.ports.receiveHintCost.send(client.room.hintCost);
            ui.ports.receiveConnectionStatus.send(true);
            worker.postMessage({ type: 'generateFromServer', data: slotData });
        })
        .catch(error => {
            console.error('Connection error:', error);
            ui.ports.receiveConnectionStatus.send(false);
        });
})

ui.ports.generateBoard?.subscribe(data => {
    worker.postMessage({ type: 'generateBoard', data: data });
});

ui.ports.checkLocation?.subscribe(data => {
    try {
        client.check(data);
    } catch (error) {
        console.error('Check location error:', error);
    }
});

ui.ports.scoutLocations?.subscribe(ids => {
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

ui.ports.sendMessage?.subscribe(text => {
    client.messages.say(text);
});

ui.ports.hintForItem?.subscribe(itemName => {
    client.messages.say(`!hint ${itemName}`);
});

ui.ports.log?.subscribe(text => {
    console.log('[UI]', text);
});

client.socket.on('disconnected', () => {
    ui.ports.receiveConnectionStatus.send(false);
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

client.messages.on('adminCommand', (text, nodes) => {
    console.log('Admin command received:', text, nodes);
    ui.ports.receiveMessage.send({ type: 'adminCommand', nodes: nodes });
});

client.messages.on('chat', (text, player, nodes) => {
    console.log('Chat message received:', text, player, nodes);
    ui.ports.receiveMessage.send({ type: 'chat', player: player, nodes: nodes });
});

client.messages.on('collected', (text, player, nodes) => {
    console.log('Collected message received:', text, player, nodes);
    ui.ports.receiveMessage.send({ type: 'collected', player: player, nodes: nodes });
});

client.messages.on('connected', (text, player, tags, nodes) => {
    console.log('Connected message received:', text, player, tags, nodes);
    ui.ports.receiveMessage.send({ type: 'connected', nodes: nodes });
});

client.messages.on('countdown', (text, value, nodes) => {
    console.log('Countdown message received:', text, value, nodes);
    ui.ports.receiveMessage.send({ type: 'countdown', value: value, nodes: nodes });
});

client.messages.on('disconnected', (text, player, nodes) => {
    console.log('Disconnected message received:', text, player, nodes);
    ui.ports.receiveMessage.send({ type: 'disconnected', player: player, nodes: nodes });
});

client.messages.on('goaled', (text, player, nodes) => {
    console.log('Goaled message received:', text, player, nodes);
    ui.ports.receiveMessage.send({ type: 'goaled', player: player, nodes: nodes });
});

client.messages.on('itemCheated', (text, item, nodes) => {
    console.log('Item cheated message received:', text, item, nodes);
    ui.ports.receiveMessage.send({ type: 'itemCheated', item: item, nodes: nodes });
});

client.messages.on('itemHinted', (text, item, found, nodes) => {
    console.log('Item hinted message received:', text, item, found, nodes);
    ui.ports.receiveMessage.send({ type: 'itemHinted', item: item, found: found, nodes: nodes });
});

client.messages.on('itemSent', (text, item, nodes) => {
    console.log('Item sent message received:', text, item, nodes);
    ui.ports.receiveMessage.send({ type: 'itemSent', item: item, nodes: nodes });
});

client.messages.on('released', (text, player, nodes) => {
    console.log('Released message received:', text, player, nodes);
    ui.ports.receiveMessage.send({ type: 'released', nodes: nodes });
});

client.messages.on('serverChat', (text, nodes) => {
    console.log('Server chat message received:', text, nodes);
    ui.ports.receiveMessage.send({ type: 'serverChat', nodes: nodes });
});

client.messages.on('tagsUpdated', (text, player, tags, nodes) => {
    console.log('Tags updated message received:', text, player, tags, nodes);
    ui.ports.receiveMessage.send({ type: 'tagsUpdated', player: player, tags: tags, nodes: nodes });
});

client.messages.on('tutorial', (text, nodes) => {
    console.log('Tutorial message received:', text, nodes);
    ui.ports.receiveMessage.send({ type: 'tutorial', nodes: nodes });
});

client.messages.on('userCommand', (text, nodes) => {
    console.log('User command message received:', text, nodes);
    ui.ports.receiveMessage.send({ type: 'userCommand', nodes: nodes });
});
