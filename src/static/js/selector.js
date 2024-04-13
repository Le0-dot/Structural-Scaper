function elementSelector(element) {
    var tag = element.tagName.toLowerCase();
    var id = element.id ? "#" + element.id : "";
    var classes = "";
    if(element.className) {
        for(var c of element.className.trim().split(/\s+/)) {
            classes += "." + c;
        }
    }
    return tag + id + classes;
}

function generateQuerySelector(element) {
    const list = [];
    var tag = element;
    while(tag.tagName.toLowerCase() != "html") {
        list.push(elementSelector(tag));
        tag = tag.parentNode;
    }
    list.push("html");

    return list.reverse().join(" ");
    // const list = []
    //
    // var tag = element;
    // list.push({
    //     "tag": tag.tagName.toLowerCase(),
    //     "id": tag.id ? tag.id : null,
    //     "classes": tag.className ? tag.className.trim().split(/\s+/) : [],
    // });
    //
    // do {
    //     tag = tag.parentNode;
    //
    //     var selector = {
    //         "tag": tag.tagName.toLowerCase(),
    //         "id": tag.id ? tag.id : null,
    //         "classes": tag.className ? tag.className.trim().split(/\s+/) : [],
    //     };
    //     list.push(selector);
    //
    // } while(tag.tagName.toLowerCase() != "html");
    //
    // return list.reverse();
}

function goToDetail(element) {
    var selector = generateQuerySelector(element);
    selector = encodeURIComponent(selector);
    window.location.href = "/select/details?selector=" + selector;
}

window.onclick = e => goToDetail(e.target);
