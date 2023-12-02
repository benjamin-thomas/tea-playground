let ports = [];

onconnect = (evt) => {
    const newPort = evt.ports[0];
    ports.push(newPort);

    // newPort.addEventListener('message', (evt) => {
    //     const message = evt.data;

    //     // Send the message to all connected ports!
    //     ports.forEach((port) => port.postMessage(message));
    // })

    newPort.start();
}
let i = 0;

function doCount() {
    i = i + 1;
    // postMessage(i);
    ports.forEach(port => port.postMessage(i))
    setTimeout("doCount()", 500)
}

doCount();