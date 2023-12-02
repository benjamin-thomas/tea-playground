let i = 0;

function doCount() {
    i = i + 1;
    postMessage(i);
    setTimeout("doCount()", 500)
}

doCount();