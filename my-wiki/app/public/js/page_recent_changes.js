class View {
  static render(state){
    return TreeBuilder.build(h =>
      h("div", {}

      , h("table", {}
        , state.changes.map((change)=>{
            return h("tr", {}
            , h("td", {}, change.id)
            , h("td", {}, change.timestamp)
            , h("td", {}
              , h("a", {
                    href: `/page/${change.id}`
                  }, change.title
                )
              )
            , h("td", {}, change.type)
            );
          })
        )
      )
    );
  }
}

class Page {
  constructor(){
    this.state = {
      changes: []
    };
  }

  getTitle(){
    return "sinatra-skelton";
  }

  init(){
    puts("init");
    __g.api_v2("get", "/api/recent_changes", {
        fooBar: 123, b: { c: 456 }
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

  render(){
    $("#tree_builder_container")
      .empty()
      .append(View.render(this.state));
  }
}

__g.ready(new Page());
