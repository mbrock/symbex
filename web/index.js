let paths = {}

let draw = (tree, path, args = {}) => {
  if (tree.tag == "Two") {
    return (
      <div className="branch body" {...args}>{
        tree.contents.map(
          (x, i) => draw(x, path + i, {
            "data-open": !!paths[path + i] || undefined,
            onClick: function (e) {
              paths[path + i] = !paths[path + i]
              redraw()
              e.stopPropagation()
            }
          })
        )
      }</div>
    )
  } else if (tree.tag == "One") {
    return (
      <div className="result ${tree.contents[1].tag}" {...args}>
        {draw(tree.contents[0].storage, path)}
      </div>
    )
  } else if (tree.tag == "Actual") {
    return (
      <div className="result" {...args}>
        {draw(tree.contents, path)}
      </div>
    )
  } else if (tree.annotation !== undefined) {
    if (tree.annotation) {
      return (
        <div {...args}>
          {draw(tree.annotation, path + "a", {
            className: "annotation-name",
            "data-open": !!paths[path + "a"] || undefined,
            onClick: function (e) {
              paths[path + "a"] = !paths[path + "a"]
              redraw()
              e.stopPropagation()
            }
          })}
          {draw(tree.value, path + "v", {
            className: "annotation-value",
            "data-open": !!paths[path + "a"] || undefined,
          })}
        </div>
      )
    } else {
      return draw(tree.value, path, args)
    }
  } else if (tree.tag) {
    return (
      <div {...args}>
        {translate(tree.tag)}
        {
          tree.contents && tree.contents.map
            ? tree.contents.map
                ((x, i) => draw(x, path + "x" + i))
            : (tree.contents ? draw(tree.contents, path + "y") : null)
        }
      </div>
    )
  } else if (tree.map) {
    return (
      <div {...args}>
        {translate(tree.tag)}
        {tree.map((x, i) => draw(x, path + "z" + i))}
      </div>
    )
  } else {
    return (
      <span {...args}>{JSON.stringify(tree)}</span>
    )
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
  Null: "unchanged",
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

let render = (x, container) => {
  ReactDOM.render(draw(x, ""), container)
}

let redraw = () => render(multisig, root)

redraw()
