import * as Archipelago from 'archipelago.js'
import { Elm } from '../Archipeladoku.elm'
import Worker from './worker.mjs?worker'
import './archipeladoku-board.ts'

const client = new Archipelago.Client()
window.client = client // For debugging purposes
let appContainer = document.getElementById('app-container')

let app = Elm.Archipeladoku.init({
    node: appContainer,
    flags: {
        seed: Math.floor(Math.random() * 2147483647),
        localStorage: { ...localStorage },
    }
})
let worker = new Worker(Worker)

worker.onmessage = function(event) {
    switch(event.data.type) {
        case 'sendBoard':
            app.ports.receiveGeneratedBoard.send(event.data.data)

            break

        case 'sendProgress':
            app.ports.receiveGenerationProgress.send(event.data.data)

            break

        default:
            console.log('Unknown message type:', event.data.type)
    }
}

function itemData(item) {
    return {
        locationId: item.locationId,
        locationName: item.locationName,
        itemId: item.id,
        itemName: item.name,
        senderAlias: item.sender.alias,
        senderName: item.sender.name,
        receiverAlias: item.receiver.alias,
        receiverName: item.receiver.name,
        gameName: item.game,
        itemClass: item.flags,
    }
}

app.ports.centerViewOnCell?.subscribe(cell => {
    const viewport = document.querySelector('archipeladoku-board')
    const [ row, col ] = cell

    if (!viewport) {
        return
    }

    viewport.centerOnCell(row, col)
})

app.ports.connect?.subscribe(data => {
    client.login(data.host, data.player, 'Archipeladoku', { password: data.password })
        .then(slotData => {
            app.ports.receiveHintCost.send(client.room.hintCost)
            app.ports.receiveConnectionStatus.send(true)
            app.ports.receiveSlotData.send(slotData)
            worker.postMessage({ type: 'generateBoard', data: slotData })
        })
        .catch(error => {
            console.error('Connection error:', error)
            app.ports.receiveConnectionStatus.send(false)
        })
})

app.ports.generateBoard?.subscribe(data => {
    worker.postMessage({ type: 'generateBoard', data: data })
})

app.ports.checkLocation?.subscribe(data => {
    try {
        client.check(data)
    } catch (error) {
        console.error('Check location error:', error)
    }
})

app.ports.goal?.subscribe(() => {
    client.goal()
})

app.ports.moveCellIntoView?.subscribe(cell => {
    const viewport = document.querySelector('archipeladoku-board')

    if (!viewport) {
        return
    }

    const [ row, col ] = cell

    viewport.moveCellIntoView(row, col)
})

app.ports.scoutLocations?.subscribe(ids => {
    try {
        client.scout(ids, 2)
            .then(scoutedItems => {
                let data = []
                for (let item of scoutedItems) {
                    data.push(itemData(item))
                }
                app.ports.receiveHints.send(data)
            })
    } catch (error) {
        console.error('Scout location error:', error)
    }
})

app.ports.sendMessage?.subscribe(text => {
    client.messages.say(text)
})

app.ports.sendPlayingStatus?.subscribe(() => {
    client.updateStatus(Archipelago.clientStatuses.playing)
})

app.ports.setLocalStorage?.subscribe(kv => {
    const [ key, value ] = kv
    localStorage.setItem(key, value)
})

app.ports.hintForItem?.subscribe(itemName => {
    client.messages.say(`!hint ${itemName}`)
})

app.ports.log?.subscribe(text => {
    console.log('[UI]', text)
})

function triggerAnimation(data) {
    const { cells, type } = data
    const viewport = document.querySelector('archipeladoku-board')

    if (!viewport) {
        return
    }

    viewport.animateCells(cells, type)
}

window.triggerAnimation = triggerAnimation

app.ports.triggerAnimation?.subscribe(triggerAnimation)

app.ports.zoom?.subscribe(data => {
    const { id, scaleMult } = data
    const viewport = document.querySelector('archipeladoku-board')

    if (!viewport) {
        return
    }

    // TODO: Center on cell

    viewport.zoomCenter(scaleMult)
})

app.ports.zoomReset?.subscribe(() => {
    const viewport = document.querySelector('archipeladoku-board')

    if (!viewport) {
        return
    }

    viewport.setViewport(60, 60, 1.0)

})

client.socket.on('disconnected', () => {
    app.ports.receiveConnectionStatus.send(false)
})

client.items.on('itemsReceived', items => {
    app.ports.receiveItems.send(items.map(item => item.id))
    const data = []
    for (let item of items) {
        data.push(itemData(item))
    }
    app.ports.receiveHints.send(data)
})

client.room.on('locationsChecked', locations => {
    app.ports.receiveCheckedLocations.send(locations)
})

client.room.on('hintPointsUpdated', (oldValue, newValue) => {
    app.ports.receiveHintPoints.send(newValue)
})

client.room.on('hintCostUpdated', (oldCost, newCost) => {
    app.ports.receiveHintCost.send(newCost)
})

client.items.on('hintsInitialized', hints => {
    const data = []
    for (let hint of hints) {
        data.push(itemData(hint.item))
    }
    app.ports.receiveHints.send(data)
})

client.items.on('hintReceived', _ => {
    let data = []
    for (let hint of client.items.hints) {
        data.push(itemData(hint.item))
    }
    app.ports.receiveHints.send(data)
})

client.messages.on('adminCommand', (text, nodes) => {
    app.ports.receiveMessage.send({ type: 'adminCommand', nodes: nodes })
})

client.messages.on('chat', (text, player, nodes) => {
    app.ports.receiveMessage.send({ type: 'chat', player: player, nodes: nodes })
})

client.messages.on('collected', (text, player, nodes) => {
    app.ports.receiveMessage.send({ type: 'collected', player: player, nodes: nodes })
})

client.messages.on('connected', (text, player, tags, nodes) => {
    app.ports.receiveMessage.send({ type: 'connected', nodes: nodes })
})

client.messages.on('countdown', (text, value, nodes) => {
    app.ports.receiveMessage.send({ type: 'countdown', value: value, nodes: nodes })
})

client.messages.on('disconnected', (text, player, nodes) => {
    app.ports.receiveMessage.send({ type: 'disconnected', player: player, nodes: nodes })
})

client.messages.on('goaled', (text, player, nodes) => {
    app.ports.receiveMessage.send({ type: 'goaled', player: player, nodes: nodes })
})

client.messages.on('itemCheated', (text, item, nodes) => {
    app.ports.receiveMessage.send({ type: 'itemCheated', item: item, nodes: nodes })
})

client.messages.on('itemHinted', (text, item, found, nodes) => {
    app.ports.receiveMessage.send({ type: 'itemHinted', item: item, found: found, nodes: nodes })
})

client.messages.on('itemSent', (text, item, nodes) => {
    app.ports.receiveMessage.send({ type: 'itemSent', item: item, nodes: nodes })
})

client.messages.on('released', (text, player, nodes) => {
    app.ports.receiveMessage.send({ type: 'released', nodes: nodes })
})

client.messages.on('serverChat', (text, nodes) => {
    app.ports.receiveMessage.send({ type: 'serverChat', nodes: nodes })
})

client.messages.on('tagsUpdated', (text, player, tags, nodes) => {
    app.ports.receiveMessage.send({ type: 'tagsUpdated', player: player, tags: tags, nodes: nodes })
})

client.messages.on('tutorial', (text, nodes) => {
    app.ports.receiveMessage.send({ type: 'tutorial', nodes: nodes })
})

client.messages.on('userCommand', (text, nodes) => {
    app.ports.receiveMessage.send({ type: 'userCommand', nodes: nodes })
})
