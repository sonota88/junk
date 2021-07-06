function puts(... args){
  console.log.apply(console, args);
}

function _parseInt(str){
  return parseInt(str, 10);
}

const __g = {
  api: function(method, path, data, fnOk, fnNg){
    var _data = {
      _method: method.toUpperCase()
      ,_params: JSON.stringify(data)
    };
    $.post(path, _data, (data)=>{
      if(data.errors.length > 0){
        fnNg(data.errors);
        return;
      }
      fnOk(data.result);
    });
  },

  api_v2: (method, path, data, fnOk, fnNg)=>{
    const req = new Request(path);

    const fd = new FormData();
    fd.append("_method", method.toUpperCase());
    fd.append("_params", JSON.stringify(data));

    fetch(req, {
      method: 'POST',
      body: fd,
      credentials: 'include', // cookie をリクエストに含める
    }).then((res)=>{
      if (res.ok) {
        puts("res.ok == true", res);
      } else {
        puts("res.ok != true", res);
      }
      return res.json();
    }).then((resData)=>{
      if (resData.errors.length > 0) {
        fnNg(resData.errors);
        return;
      }
      fnOk(resData.result);
    }).catch((err)=>{
      puts(err);
    });
  },

  guard: ()=>{
    $("#guard_layer").show();
  },

  unguard: ()=>{
    setTimeout(()=>{
      $("#guard_layer").fadeOut(100);
    }, 100);
  },

  printApiErrors: (es)=>{
    es.forEach((e, i)=>{
      puts(`-------- error ${i} --------`);
      puts(e.trace.split("\n").reverse().join("\n"));
      puts(e.msg);
    });
  },

  updatePageTitle: (title)=>{
    document.title = title + " | jjsiki";
  },

  ready: (page)=>{
    window.__p = page;
    document.addEventListener("DOMContentLoaded", ()=>{
      page.init();
      __g.updatePageTitle(page.getTitle());
    });
  },

  mkToc: (containerEl)=>{
    var $toc = $('<ul>');

    var $hs = $(containerEl)
      .find("h1, h2, h3, h4, h5, h6")
      .not(".notoc");

    $hs.each(function(i, el){
      var $link = $('<a></a>')
          .text(el.childNodes[0].textContent)
          .attr({ href: "#" + el.id })
      ;
      var lv = el.tagName.substring(1, 2);
      var $li = $('<li></li>').append($link).addClass("toc_lv" + lv);
      $toc.append($li);
    });

    return $toc.get(0);
  }
};
