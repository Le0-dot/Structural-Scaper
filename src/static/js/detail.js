function selectorToStr() {
    var rows = document.getElementById("selector-table-body").children;
    const selector = [];
    for(var i = 0; i < rows.length; ++i) {
        const [tag, id, classes] = rows[i].children;
        var tag_info = [tag.textContent];
        if (id.children.length != 0 && id.children[0].checked) {
            tag_info.push("#" + id.children[1].textContent);
        }
        for (var j = 0; j < classes.children.length; ++j) {
            class_ = classes.children[j];
            if(class_.children[0].checked) {
                tag_info.push("." + class_.children[1].textContent);
            }
        }
        selector.push(tag_info.join(""));
    }
    return selector.join(" ");
}

function goToNext() {
    var selector = selectorToStr();
    selector = encodeURIComponent(selector);
    window.location.href = '/select/next?selector=' + selector;
}