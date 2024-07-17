export {
    accumulateText,
}
function accumulateText(node) {
    const offset = node.ind
    const get = (node) => {
        const ind = node.ind - offset
        const newlines = 1
        return " ".repeat(ind * 4) + node.text +
            "\n".repeat(node.newlines + newlines)
    }
    const callback = (child, i, a) => {
        newlines = child.newlines
        if (child.isBranch()) {
            return get(child) + runner(child)
        } else {
            return get(child)
        }
    }
    const runner = (node) => {
        return node.children.map(callback).join("")
    }

    let newlines
    let ind = node.ind
    const text = callback(node).trimEnd()
    // throw drawBorder(text)
    return {
        text,
        newlines,
        ind,
    }
}
