const h = (tag, attrs, ...kids)=>{
  const el = document.createElement(tag);
  if (attrs) {
    for (const k in attrs) {
      const v = attrs[k];
      if (k === "onclick") {
        el.onclick = v;
      } else if (k === "style") {
        for (let sk in v) {
          el.style[sk] = v[sk];
        }
      } else {
        el.setAttribute(k, v);
      }
    }
  }
  (kids || []).forEach(kid => {
    if (typeof kid === "string") {
      el.appendChild(document.createTextNode(kid));
    } else {
      el.appendChild(kid);
    }
  });
  return el;
};

const s = document.title + "\n" + location.href + "\n";
const outer = h("div");
const inner = h("div", { style: {
      position: "fixed"
    , top: 0
    , left: 0
    , background: "#aaa"
    , padding: "0.5rem"
    }
  }
, h("button", { onclick: ()=>{ outer.remove(); } }, "close")
, h("textarea", { style: { "font-family": "Courier, monospace" } }, s)
);
outer.appendChild(inner);
document.body.appendChild(outer);
