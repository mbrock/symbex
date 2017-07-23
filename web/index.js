let draw = (tree, box) => {
  if (tree.tag == "Two") {
    box.className = "branch body"
    // let sub = document.createTextNode("branch")
    // box.appendChild(sub)
    tree.contents.forEach(x => {
      let sub = document.createElement("DIV")
      sub.onclick = e => { sub.classList.toggle("open"); e.stopPropagation() }
      draw(x, sub)
      box.appendChild(sub)
    })
  } else if (tree.tag == "One") {
    box.className = `result ${tree.contents[1].tag}`
    draw(tree.contents[0]["storage"], box)
  } else if (tree.tag == "Actual") {
    box.className = `result`
    draw(tree.contents, box)
  } else if (tree.tag) {
    let sub = document.createTextNode(translate(tree.tag))
    box.appendChild(sub)
    if (tree.contents !== undefined) {
      if (tree.contents.forEach)
        tree.contents.forEach(x => {
          let sub = document.createElement("DIV")
          draw(x, sub)
          box.appendChild(sub)
        })
      else {
        let sub = document.createElement("DIV")
        draw(tree.contents, sub)
        box.appendChild(sub)
      }
    }
  } else {
    box.innerText = JSON.stringify(tree)
  }
}

let translations = {
  Equality: "eq?",
  TheCaller: "caller",
  TheCalldatasize: "calldatasize",
  Actual: "word",
  IsGreaterThan: ">",
  IsLessThan: "<",
  Conjunction: "and",
  Disjunction: "or",
  StorageAt: "sload",
  TheHashOf: "keccak256",
  WithCalldata: "with-calldata",
  Null: "initial",
  TheTimestamp: "timestamp",
  TheCalldataWord: "calldataload",
  Exponentiation: "^",
  Negation: "not",
  Plus: "+",
  Minus: "-",
  With: "store",
  ArbitrarilyAltered: "external-call-occurred",
}

let translate = x => translations[x] || x
