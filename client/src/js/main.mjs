import * as Archipelago from 'archipelago.js'
import { Elm } from '../Archipeladoku/UI.elm'
import Worker from './worker.mjs?worker'
import './panzoom-board-wrapper.mjs'

const client = new Archipelago.Client()
window.client = client // For debugging purposes
let uiContainer = document.getElementById('ui-container')

let ui = Elm.Archipeladoku.UI.init({
    node: uiContainer,
    flags: {
        seed: Math.floor(Math.random() * 2147483647)
    }
})
let worker = new Worker(Worker)

worker.onmessage = function(event) {
    switch(event.data.type) {
        case 'sendBoard':
            ui.ports.receiveBoard.send(event.data.data)

            break

        case 'sendProgress':
            ui.ports.receiveGenerationProgress.send(event.data.data)

            break

        default:
            console.log('Unknown message type:', event.data.type)
    }
}

ui.ports.connect?.subscribe(data => {
    client.login(data.host, data.player, 'Archipeladoku', { password: data.password })
        .then(slotData => {
            ui.ports.receiveHintCost.send(client.room.hintCost)
            ui.ports.receiveConnectionStatus.send(true)
            worker.postMessage({ type: 'generateBoard', data: slotData })
        })
        .catch(error => {
            console.error('Connection error:', error)
            ui.ports.receiveConnectionStatus.send(false)
        })
})

ui.ports.generateBoard?.subscribe(data => {
    worker.postMessage({ type: 'generateBoard', data: data })
})

ui.ports.checkLocation?.subscribe(data => {
    try {
        client.check(data)
    } catch (error) {
        console.error('Check location error:', error)
    }
})

ui.ports.goal?.subscribe(() => {
    client.goal()
})

ui.ports.moveCellIntoView?.subscribe(cellId => {
    const cellElement = document.getElementById(cellId)
    const viewport = document.querySelector('panzoom-board-wrapper')

    if (!cellElement || !viewport) {
        return
    }

    const panzoom = viewport.panzoomInstance
    const currentTransform = panzoom.getTransform()
    const cellRect = cellElement.getBoundingClientRect()
    const viewportRect = viewport.getBoundingClientRect()

    const padding = 48 * currentTransform.scale
    const headerOffset = 48 * currentTransform.scale

    let deltaX = 0
    let deltaY = 0

    if (cellRect.left < viewportRect.left + padding + headerOffset) {
        deltaX = viewportRect.left + padding + headerOffset - cellRect.left
    } else if (cellRect.right > viewportRect.right - padding) {
        deltaX = viewportRect.right - padding - cellRect.right
    }

    if (cellRect.top < viewportRect.top + padding + headerOffset) {
        deltaY = viewportRect.top + padding + headerOffset - cellRect.top
    } else if (cellRect.bottom > viewportRect.bottom - padding) {
        deltaY = viewportRect.bottom - padding - cellRect.bottom
    }

    if (Math.abs(deltaX) > 0 || Math.abs(deltaY) > 0) {

        const newX = currentTransform.x + deltaX
        const newY = currentTransform.y + deltaY

        panzoom.smoothMoveTo(newX, newY)
    }
})

ui.ports.scoutLocations?.subscribe(ids => {
    try {
        client.scout(ids, 2)
            .then(scoutedItems => {
                let data = []
                for (let item of scoutedItems) {
                    data.push({
                        locationId: item.locationId,
                        locationName: item.locationName,
                        itemId: item.id,
                        itemName: item.name,
                        playerName: item.receiver.alias,
                        gameName: item.game,
                        itemClass: item.flags,
                    })
                }
                ui.ports.receiveScoutedItems.send(data)
            })
    } catch (error) {
        console.error('Scout location error:', error)
    }
})

ui.ports.sendMessage?.subscribe(text => {
    client.messages.say(text)
})

ui.ports.hintForItem?.subscribe(itemName => {
    client.messages.say(`!hint ${itemName}`)
})

ui.ports.log?.subscribe(text => {
    console.log('[UI]', text)
})

function triggerAnimation(data) {
    const { ids, type } = data

    ids.forEach((id, i) => {
        const element = document.getElementById(id)
        if (!element) {
            return
        }

        if (type === 'shine') {
            element.style.backgroundImage =
                `linear-gradient(
                    135deg,
                    transparent 40%,
                    rgba(255, 255, 255, 0.6) 50%,
                    transparent 60%
                )`
            element.style.backgroundSize = '200% 100%'
            element.style.backgroundRepeat = 'no-repeat'
            element.style.backgroundPosition = '200% 0'

            const keyframes = [
                { backgroundPosition: '200% 0' },
                { backgroundPosition: '-100% 0' }
            ]
            const options = {
                duration: 500,
                delay: i * 60,
                easing: 'ease-in-out',
            }

            const animation = element.animate(keyframes, options)

            animation.onfinish = () => {
                element.style.backgroundImage = ''
                element.style.backgroundSize = ''
                element.style.backgroundRepeat = ''
                element.style.backgroundPosition = ''
            }
        } else if (type === 'shatter') {
            const panzoom = document.querySelector('panzoom-board-wrapper').panzoomInstance
            const scale = panzoom.getTransform().scale
            const rect = element.getBoundingClientRect()
            const width = rect.width
            const height = rect.height

            const rows = 3
            const cols = 3
            const shardWidth = Math.ceil(width / cols)
            const shardHeight = Math.ceil(height / rows)

            const container = document.body

            for (let row = 0; row < rows; row++) {
                for (let col = 0; col < cols; col++) {
                    const shard = document.createElement('div')
                    shard.classList.add('shard')
                    shard.style.width = `${shardWidth}px`
                    shard.style.height = `${shardHeight}px`

                    const initialX = rect.left + window.scrollX + col * shardWidth
                    const initialY = rect.top + window.scrollY + row * shardHeight

                    shard.style.left = `${initialX}px`
                    shard.style.top = `${initialY}px`

                    container.appendChild(shard)

                    const centerX = rect.left + width / 2
                    const centerY = rect.top + height / 2
                    const shardCenterX = initialX + shardWidth / 2
                    const shardCenterY = initialY + shardHeight / 2


                    let dirX = shardCenterX - centerX
                    let dirY = shardCenterY - centerY

                    const distance = Math.sqrt(dirX * dirX + dirY * dirY) || 1
                    dirX = dirX / distance + (Math.random() - 0.5) * 0.5
                    dirY = dirY / distance + (Math.random() - 0.5) * 0.5

                    const travelDist = (50 + Math.random() * 50) * scale
                    const rotation = -360 + Math.random() * 720

                    const keyframes = [
                        {
                            transform: `translate(0px, 0px) rotate(0deg) scale(1)`,
                            opacity: 1
                        },
                        {
                            transform: `translate(${dirX * travelDist}px, ${dirY * travelDist}px) rotate(${rotation}deg) scale(0.5)`,
                            opacity: 0
                        }
                    ]
                    const options = {
                        duration: 1500 + Math.random() * 500,
                        easing: 'cubic-bezier(0.25, 0.46, 0.45, 0.94)',
                        fill: 'forwards',
                    }

                    const animation = shard.animate(keyframes, options)

                    animation.onfinish = () => {
                        shard.remove()
                    }
                }
            }
        }
    })
}

ui.ports.triggerAnimation?.subscribe(triggerAnimation)

client.socket.on('disconnected', () => {
    ui.ports.receiveConnectionStatus.send(false)
})

client.items.on('itemsReceived', items => {
    ui.ports.receiveItems.send(items.map(item => item.id))
})

client.room.on('locationsChecked', locations => {
    ui.ports.receiveCheckedLocations.send(locations)
})

client.room.on('hintPointsUpdated', (oldValue, newValue) => {
    ui.ports.receiveHintPoints.send(newValue)
})

client.room.on('hintCostUpdated', (oldCost, newCost) => {
    ui.ports.receiveHintCost.send(newCost)
})

client.items.on('hintsInitialized', hints => {
    let data = []
    for (let hint of hints) {
        if (hint.item.receiver.name != client.name) {
            continue
        }
        data.push({
            locationId: hint.item.locationId,
            locationName: hint.item.locationName,
            itemId: hint.item.id,
            itemName: hint.item.name,
            playerName: hint.item.sender.alias,
            gameName: hint.item.game,
            itemClass: hint.item.flags,
        })
    }
    ui.ports.receiveHints.send(data)
})

client.items.on('hintReceived', _ => {
    let data = []
    for (let hint of client.items.hints) {
        if (hint.item.receiver.name != client.name) {
            continue
        }
        data.push({
            locationId: hint.item.locationId,
            locationName: hint.item.locationName,
            itemId: hint.item.id,
            itemName: hint.item.name,
            playerName: hint.item.sender.alias,
            gameName: hint.item.game,
            itemClass: hint.item.flags,
        })
    }
    ui.ports.receiveHints.send(data)
})

client.messages.on('adminCommand', (text, nodes) => {
    ui.ports.receiveMessage.send({ type: 'adminCommand', nodes: nodes })
})

client.messages.on('chat', (text, player, nodes) => {
    ui.ports.receiveMessage.send({ type: 'chat', player: player, nodes: nodes })
})

client.messages.on('collected', (text, player, nodes) => {
    ui.ports.receiveMessage.send({ type: 'collected', player: player, nodes: nodes })
})

client.messages.on('connected', (text, player, tags, nodes) => {
    ui.ports.receiveMessage.send({ type: 'connected', nodes: nodes })
})

client.messages.on('countdown', (text, value, nodes) => {
    ui.ports.receiveMessage.send({ type: 'countdown', value: value, nodes: nodes })
})

client.messages.on('disconnected', (text, player, nodes) => {
    ui.ports.receiveMessage.send({ type: 'disconnected', player: player, nodes: nodes })
})

client.messages.on('goaled', (text, player, nodes) => {
    ui.ports.receiveMessage.send({ type: 'goaled', player: player, nodes: nodes })
})

client.messages.on('itemCheated', (text, item, nodes) => {
    ui.ports.receiveMessage.send({ type: 'itemCheated', item: item, nodes: nodes })
})

client.messages.on('itemHinted', (text, item, found, nodes) => {
    ui.ports.receiveMessage.send({ type: 'itemHinted', item: item, found: found, nodes: nodes })
})

client.messages.on('itemSent', (text, item, nodes) => {
    ui.ports.receiveMessage.send({ type: 'itemSent', item: item, nodes: nodes })
})

client.messages.on('released', (text, player, nodes) => {
    ui.ports.receiveMessage.send({ type: 'released', nodes: nodes })
})

client.messages.on('serverChat', (text, nodes) => {
    ui.ports.receiveMessage.send({ type: 'serverChat', nodes: nodes })
})

client.messages.on('tagsUpdated', (text, player, tags, nodes) => {
    ui.ports.receiveMessage.send({ type: 'tagsUpdated', player: player, tags: tags, nodes: nodes })
})

client.messages.on('tutorial', (text, nodes) => {
    ui.ports.receiveMessage.send({ type: 'tutorial', nodes: nodes })
})

client.messages.on('userCommand', (text, nodes) => {
    ui.ports.receiveMessage.send({ type: 'userCommand', nodes: nodes })
})
