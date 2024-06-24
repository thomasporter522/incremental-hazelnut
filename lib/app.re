// open Core;
open Incr_dom;
// open Js_of_ocaml;

// open Lang;

module State = {
  type t = unit;
};

module Action = {
  type t =
    | Edit(Lang.action)
    | SetFunField(string)
    | SetVarField(string);

  let sexp_of_t = (a: t): Sexplib0.Sexp.t =>
    switch (a) {
    | _ => Sexplib0.Sexp.Atom("idk")
    };
};

module Model = {
  type t = {
    zterm: Lang.zterm,
    funfield: string,
    varfield: string,
  };
  let cutoff = (_: t, _: t) => false; //t1.state == t2.state;
};

let on_startup = (~schedule_action as _, _) => Async_kernel.return();

let (let+) = (x, f) => {
  Incr.map(x, ~f);
};

let ( let* ) = (x, f) => {
  Incr.bind(x, ~f);
};

let apply_action =
    (model: Model.t, action: Action.t, _, ~schedule_action as _): Model.t => {
  switch (action) {
  | Edit(action) => {
      ...model,
      zterm: Lang.apply_action(model.zterm, action),
    }
  | SetFunField(s) => {...model, funfield: s}
  | SetVarField(s) => {...model, varfield: s}
  };
};

let view =
    (model: Incr.t(Model.t), ~inject: Action.t => Ui_effect.t(Base.unit))
    : Ui_incr.t(Vdom.Node.t) => {
  open Vdom;
  let _ = inject;
  let+ model = model;
  // let (a, b) = Lang.strings_of_syn_env(model);
  let button = (text, action) =>
    Node.button(
      ~attr=Attr.many_without_merge([Attr.on_click(_ev => inject(action))]),
      [Node.text(text)],
    );
  Node.div([
    Node.text(Lang.string_of_zterm(model.zterm)),
    Node.br(),
    Node.text(Lang.info_string_of_zterm(model.zterm)),
    Node.br(),
    button("Delete", Edit(Delete)),
    Node.br(),
    button("Insert Num", Edit(InsertNum)),
    Node.br(),
    button("Insert Const", Edit(InsertConst)),
    Node.br(),
    button("Insert Arrow", Edit(InsertArrow)),
    Node.br(),
    Node.input(
      ~attr=
        Attr.many_without_merge([
          Attr.on_input((_ev, text) => inject(SetVarField(text))),
        ]),
      [],
    ),
    button("Insert Var", Edit(InsertVar(model.varfield))),
    Node.br(),
    Node.input(
      ~attr=
        Attr.many_without_merge([
          Attr.on_input((_ev, text) => inject(SetFunField(text))),
        ]),
      [],
    ),
    button("Insert Fun", Edit(InsertFun(model.funfield))),
    Node.br(),
    button("Insert Ap", Edit(InsertAp)),
    Node.br(),
    button("Insert Asc", Edit(InsertAsc)),
    Node.br(),
    button("Move Up", Edit(Move(Up))),
    Node.br(),
    button("Move Down1", Edit(Move(Down1))),
    Node.br(),
    button("Move Down2", Edit(Move(Down2))),
  ]);
};

let create =
    (
      model: Incr.t(Model.t),
      ~old_model as _,
      ~inject: Action.t => Virtual_dom.Vdom.Effect.t(unit),
    )
    : Incr.t(Component.t(Action.t, Model.t, State.t)) => {
  let* view: Vdom.Node.t = view(model, ~inject);
  let+ model: Model.t = model;
  let apply_action = apply_action(model);
  Component.create(~apply_action, model, view);
};

let initial_model: Model.t = {
  zterm: Lang.initial_zterm,
  funfield: "",
  varfield: "",
};
