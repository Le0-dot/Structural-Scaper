function selectorToStr() {
    const rows = document.getElementById("selector-table-body").children;
    const selector = [];
    for(var i = 0; i < rows.length; ++i) {
        const [tag, id, classes] = rows[i].children;
        if(!tag.children[0].checked) {
            continue;
        }
        const tag_info = [tag.children[1].textContent.trim()];
        if (id.children.length != 0 && id.children[0].checked) {
            tag_info.push("#" + id.children[1].textContent.trim());
        }
        for (var j = 0; j < classes.children.length; ++j) {
            class_ = classes.children[j];
            if(class_.children[0].checked) {
                tag_info.push("." + class_.children[1].textContent.trim());
            }
        }
        selector.push(tag_info.join(""));
    }
    return selector.join(" ");
}
