// All connected clients (tabs or windows)
let allPorts = [];

// Fetch the Elm.Worker
importScripts('/js/worker.js');

const worker = Elm.Worker.init();
worker.ports.stateChanged.subscribe((newState) => {
    allPorts.forEach(port => port.postMessage(newState));
});

function onNewPort(message) {
    const data = message.data;
    switch (data.command) {
        case "reset":
            worker.ports.reset.send(null);
            break;

        default:
            console.log($`Unknown command: '${data.command}'`);
            break;
    }
}


onconnect = (evt) => {
    const newPorts = evt.ports;
    newPorts.forEach((port) => {
        port.onmessage = onNewPort;
    });
    allPorts = allPorts.concat(newPorts);
};





