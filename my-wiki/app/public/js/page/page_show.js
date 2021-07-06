class View {
  static render(state){
    return TreeBuilder.build(h =>
      h("div", {}
      , h("h1", {}, state.title)
      , h("a", { href: `/page/${__p.getPageId()}/edit` }, "編集")
      , h("hr")

      , h("div", { id: "toc" }
        , h("div", { id: "toc_heading" }, "目次")
        )

      // , h("pre", {}, __p.toHtml())
      , h("pre"
        , { id: "article_main" }
        , TreeBuilder.buildRawHtml(__p.toHtml())
        )
      , h("h1", {}, "このページを参照しているページ")
      , h("ul", {}
        , state.invertedLinks.map(pid =>
            h("li", {}
            , h("a", { href: `/page/${pid}` }, state.pageIdTitleMap[pid])
            )
          )
        )
      )
    );
  }
}

class Page {
  constructor(){
    this.state = {
      title: '{title}'
      ,src: '{html}'
      ,pageIdTitleMap: {} // TODO
    };
  }

  getTitle(){
    return this.state.title;
  }

  getPageId(){
    location.href.match(/page\/(\d+)/);
    return _parseInt(RegExp.$1);
  }

  init(){
    puts("init");
    __g.api_v2("get", "/api/page/" + this.getPageId(), {
      }, (result)=>{
      __g.unguard();
      puts(result);
      Object.assign(this.state, result);

      this.render();
      __g.updatePageTitle(this.getTitle());

      this.updateLinks();
    }, (errors)=>{
      __g.unguard();
      __g.printApiErrors(errors);
      alert("Check console.");
    });
  }

  render(){
    $("#tree_builder_container")
      .empty()
      .append(View.render(this.state));

    prettyPrint();

    $("#toc")
      .empty()
      .append(__g.mkToc($("#article_main").get(0)));
  }

  toHtml(){
    try{
      const ret = wikiproc.toHTML(
        this.getPageId()
        ,this.state.src
        ,this.state.pageIdTitleMap
        ,{}
      );
      puts(64, ret);
      // return "{html}";
      return ret.mainContent;
    }catch(e){
      return "error";
    }
  }

  updateLinks() {
    const uniq = (xs) => {
      const obj = {};
      xs.forEach(x => { obj[x] = null; });
      return Object.keys(obj);
    };

    const pageId = this.getPageId();

    if (sessionStorage.getItem("changedPageId") == null) {
      return;
    }

    const changedPageId =
          _parseInt(
            sessionStorage.getItem("changedPageId")
          );
    puts("changedPageId", changedPageId);

    // assert
    if (changedPageId !== pageId) {
      return;
    }

    if (changedPageId != null) {
      const ids =
        Array.from($("#article_main").find("a"))
        .map(link => link.href)
        .filter(href => /\/page\//.test(href))
        .filter(href => ! /\/page\/(.+)\/edit/.test(href))
        .map(href => {
          const m = href.match(/\/page\/(\d+)/);
          if (m) {
            return m[1];
          } else {
            return null;
          }
        })
        .filter(id => id != null)
        ;

      __g.api_v2(
        "patch", `/api/page/${pageId}/links`,
        {
          destIds: uniq(ids).map(_parseInt)
        },
        (result)=>{
          __g.unguard();
          puts(137, result);
          sessionStorage.removeItem("changedPageId");
        },
        (errors)=>{
          __g.unguard();
          __g.printApiErrors(errors);
          alert("Check console.");
        }
      );
    }
  }
}

__g.ready(new Page());
