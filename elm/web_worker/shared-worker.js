console.log('shared worker start...');

let allPorts = [];

onconnect = (evt) => {
    allPorts = allPorts.concat(evt.ports);
}


// Fetch the Elm.Worker
importScripts('/js/worker.js');

const worker = Elm.Worker.init();
console.log("worker init...");
worker.ports.stateChanged.subscribe((newState) => {
    // console.log({curr: newState});
    allPorts.forEach(port => port.postMessage(newState));
});