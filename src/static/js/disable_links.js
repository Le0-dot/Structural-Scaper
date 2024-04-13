function stop_event(e) {
    e.preventDefault();
    return false;
}

for(var link of document.links) {
    link.onclick = stop_event;
}
