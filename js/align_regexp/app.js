// TODO 全角文字の幅を考慮

function puts(...args) {
  console.log(...args);
}

function _el(sel) {
  return document.querySelector(sel);
}

function _els(sel) {
  return document.querySelectorAll(sel);
}

function addev(el, ev, fn) {
  el.addEventListener(ev, fn);
}

function refresh() {
  const input = _el("#input").value;
  const pattern = _el("#pattern").value;

  const lines = input.split("\n");

  // 桁の位置を決める
  const llens = lines.map(line => {
    const i = line.indexOf(pattern);
    if (0 <= i) {
      return i;
    } else {
      return 0;
    }
  });
  puts(32, Math.max(...llens));

  // 揃える位置
  const i = Math.max(...llens);
  puts(32, llens, i);

  const lines2 = lines.map(line => {
    const i2 = line.indexOf(pattern);
    const left = line.substring(0, i2);
    const right = line.substring(i2);
    const num_spcs = i - left.length;
    puts(left, right, num_spcs);

    let left2 = left;
    for (let i3 = 0; i3 < num_spcs; i3++) {
      left2 = left2 + " ";
    }

    return left2 + right;
  });

  const output = lines2.join("\n");

  _el("#output").value = output;
}

function init() {
  addev(_el("#pattern"), "input", ()=>{
    refresh();
  });
  addev(_el("#input"), "input", ()=>{
    refresh();
  });
}

window.addEventListener("pageshow", init);
