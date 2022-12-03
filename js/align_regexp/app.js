function puts(...args) {
  // console.log(...args);
}

function getEl(sel) {
  return document.querySelector(sel);
}

function handleEv(el, ev, fn) {
  let els;
  if (typeof el === "string") {
    els = document.querySelectorAll(el);
  } else {
    els = [el];
  }
  els.forEach(el => el.addEventListener(ev, fn));
}

function debounce(fn, msec) {
  const self = debounce;

  if (self.map[fn] != null) {
    clearTimeout(self.map[fn]);
  }

  self.map[fn] = setTimeout(
    () => {
      fn();
      self.map[fn] = null;
    },
    msec
  );
}
debounce.map = {};

// --------------------------------

function getPattern() {
  return getEl("#pattern").value;
}

function getIndex() {
  return Math.trunc(getEl("#index").value);
}

function getInput() {
  return getEl("#input").value;
}

function setInput(text) {
  getEl("#input").value = text;
}

function getPreview() {
  return getEl("#preview").value;
}

function setPreview(text) {
  getEl("#preview").value = text;
}

// --------------------------------

function isHankaku(c) {
  const cc = c.charCodeAt(0);
  return (
    (32 <= cc && cc <= 126) // (SPC) ... '~'
    || (65377 <= cc && cc <= 65439) // '｡' ... 'ﾟ'
  );
}

function charWidth(c) {
  return isHankaku(c) ? 1 : 2;
}

function lengthInHankaku(s) {
  let len = 0;
  for (let i = 0; i < s.length; i++) {
    len += charWidth(s.charAt(i));
  }
  return len;
}

/*
 * left sep right
 *     ^m.index == left length
 *      ^^^m[0]
 */
function matchLine(line, re, index) {
  let rest = line;
  let found = false;
  let pos = 0;

  let i = -1;
  while (true) {
    i++;
    if (1000 < i) {
      throw new Error("too many iteration");
    }

    const m = rest.match(re);
    if (m == null) {
      break;
    }

    pos += m.index; // add left

    if (index <= i) {
      found = true;
      break;
    }

    pos += m[0].length; // add separator
    rest = rest.substring(m.index + m[0].length);
  }

  return found ? pos : null;
}

function getAlignPos(pairs) {
  const headLenList =
    pairs
      .map(pair => pair[0])
      .map(lengthInHankaku);

  return Math.max(...headLenList);
}

function toPair(line, re, index) {
  const pos = matchLine(line, re, index);
  if (pos == null) {
    return [line, ""];
  } else {
    const head = line.substring(0, pos);
    const rest = line.substring(pos);
    return [head, rest];
  }
}

function alignRegexp(input, re, index) {
  const lines = input.split("\n");
  const pairs = lines.map(line => toPair(line, re, index));
  const alignPos = getAlignPos(pairs);

  const newLines =
    pairs.map(pair => {
      const [head, rest] = pair;
      if (rest === "") {
        return head;
      } else {
        const numSpaces = alignPos - lengthInHankaku(head);
        return head + " ".repeat(numSpaces) + " " + rest;
      }
    });

  return newLines.join("\n");
}

function _refreshPreview() {
  const pattern = getPattern();
  if (pattern === "") {
    setPreview("");
    return;
  }
  const re = new RegExp(pattern);

  const input = getInput();
  const index = getIndex();

  const aligned = alignRegexp(input, re, index);

  setPreview(aligned);
}

function refreshPreview() {
  try {
    _refreshPreview();
  } catch(e) {
    setPreview(e.message);
    throw e;
  }
}

function doApply() {
  const text = getPreview();
  setInput(text);
}

function oninput_text() {
  debounce(refreshPreview, 100);
}

function onclick_apply() {
  doApply();
}

function onkeydown(ev) {
  if (ev.ctrlKey && ev.key === "Enter") {
    doApply();
  }
}

function init() {
  refreshPreview();

  ["#pattern", "#input", "#index"]
    .forEach(sel =>
      handleEv(sel, "input", oninput_text)
    );
  handleEv("#btn_apply", "click", onclick_apply)
  handleEv(window, "keydown", (ev) => onkeydown(ev));
}

window.addEventListener("pageshow", init);
