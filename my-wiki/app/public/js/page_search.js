class TitleResultList {
  static render(results) {
    if (results.length === 0) {
      return "(no result)";
    }

    return TreeBuilder.build(h =>
      h("div", {}
      , h("ul", {}
        , results.map((page)=>
            h("li", {}
            , "- "
            , h("a", { href: `/page/${page.id}` }, page.title)
            )
          )
        )
      )
    );
  }
}

class MatchedLine {
  static render(line) {
    return TreeBuilder.build(h =>
      h("li", {
        style: {
          "border-left": "solid 0.3rem #eee"
        , "padding-left": "0.5rem"
        }}, `${line.ln}: ${line.text}`)
    );
  }
}

class BodyResultList {
  static render(results) {
    if (results.length === 0) {
      return "(no result)";
    }

    return TreeBuilder.build(h =>
      h("div", {}
      , h("ul", {}
        , results.map((page)=>
            h("li", {}
            , "- "
            , h("button", {
                  style: { width: "2rem" }
                , onclick: ()=>{ __p.onclick_searchDetail(page.id); }
                }
              , page.n
              )
            , h("a", { href: `/page/${page.id}` }, page.title)

            , page.showLines
              ? h("ul", {}
                  , page.lines.map(MatchedLine.render)
                )
              : null
            )
          )
        )
      )
    );
  }
}

class View {
  static render(state){
    return TreeBuilder.build(h =>
      h("div", {}

      , h("h1", {}, "タイトル")

      , h("input"
        , { name: "q_title"
          , value: state.qTitle
          , oninput: ()=>{ __p.onchange_qTitle(); }
          , style: { width: "20rem" }
          } )
      , h("button", {}, "×") // TODO

      , h("div", { id: "title_result_box" }, "{search by title results}")

      , h("hr")

      , h("h1", {}, "タイトル＋本文")

      , h("input"
        , { name: "q_body"
          , style: { width: "20rem" }
          , onkeydown: (ev)=>{ __p.onkeydown_searchQBody(ev); }
          } )
      , h("button", { onclick: ()=>{ __p.onclick_searchQBody(); } }, "検索") // TODO

      , h("div", { id: "body_result_box" }, "{search by title results}")

      )
    );
  }
}

class Page {
  constructor(){
    this.state = {
      qTitle: "",
      titleResults: [
        // { id: 1, title: "foo"}, { id: 2, title: "foo2"}
      ],
      bodyResults: [
        // { id: 1, title: "foo", n: 1
        //   // , lines: []
        // }, { id: 2, title: "foo2", n: 123
        //   ,lines: [
        //     { ln: 123, text: "fdsa" },
        //     { ln: 1, text: "fdsaFDSA" }
        //   ]
        // }
      ],
      qBody: "",
    };
  }

  getTitle(){
    return "sinatra-skelton";
  }

  init(){
    puts("init");
    __g.api_v2("get", "/api/search", {
        // fooBar: 123, b: { c: 456 }
      }, (result)=>{
      __g.unguard();
      puts(result);
      Object.assign(this.state, result);

      this.render();

    }, (errors)=>{
      __g.unguard();
      __g.printApiErrors(errors);
      alert("Check console.");
    });
  }

  renderTitleResultList(){
    $("#title_result_box")
      .empty()
      .append(TitleResultList.render(this.state.titleResults));
  }

  renderBodyResultList(){
    $("#body_result_box")
      .empty()
      .append(BodyResultList.render(this.state.bodyResults));
  }

  render(){
    $("#tree_builder_container")
      .empty()
      .append(View.render(this.state));

    this.renderTitleResultList();
    this.renderBodyResultList();
  }

  onchange_qTitle() {
    puts("-->> onchange_qTitle");
    this.state.qTitle = $("[name=q_title]").val();
    const results = [];

    if (1 <= this.state.qTitle.length) {
      Object.keys(this.state.idTitleMap).forEach((id)=>{
        const title = this.state.idTitleMap[id];
        if (title.toLowerCase().includes(this.state.qTitle.toLowerCase())) {
          results.push({id, title});
        }
      });
    }

    this.state.titleResults = results;

    this.renderTitleResultList();
  }

  onclick_searchQBody() {
    this.state.qBody = $("[name=q_body]").val();

    const params = {};
    params.q = this.state.qBody;
    if (1 <= params.q.length) {
      __g.api_v2(
        "get", "/api/search_grep"
      , params
      , (result)=>{
          __g.unguard();
          puts(result);
          // Object.assign(this.state, result);

          // this.render();
          this.state.bodyResults = result.pages;
          this.renderBodyResultList();
        }
      , (errors)=>{
          __g.unguard();
          __g.printApiErrors(errors);
          alert("Check console.");
        }
      );
    } else {
      this.state.bodyResults = [];
      this.renderBodyResultList();
    }
  }

  onkeydown_searchQBody(ev) {
    if (ev.code === "Enter") {
      this.onclick_searchQBody();
    }
  }

  onclick_searchDetail(pageId){
    const currentResult = this.state.bodyResults.find((br)=>{
      return br.id === pageId;
    });

    if (currentResult.showLines) {
      currentResult.showLines = false;
      this.renderBodyResultList();
      return;
    }

    if (this.state.qBody.length === 0) {
      puts("q too short");
      return;
    }

    const params = {};
    params.q = this.state.qBody;
    params.pageId = pageId;

    __g.api_v2(
      "get", "/api/search_grep_file"
    , params
    , (result)=>{
        __g.unguard();

        const br = this.state.bodyResults.find(br => br.id === pageId)
        br.lines = result.lines;
        br.showLines = true;

        this.renderBodyResultList();
      }
    , (errors)=>{
        __g.unguard();
        __g.printApiErrors(errors);
        alert("Check console.");
      }
    );
  }
}

__g.ready(new Page());
