type var = string;

type typ =
  | Hole
  | Num
  | Arrow(typ, typ);

type ctx_typ =
  | Hole
  | Lookup(var)
  | Just(typ)
  | Arrow(ctx_typ, ctx_typ)
  | ArrowL(ctx_typ)
  | ArrowR(ctx_typ);

type ctx_marks =
  | Empty
  | NotInCtx(var)
  | NotMatchedArrow(ctx_typ)
  | Inconsistent(ctx_typ, ctx_typ)
  // Mark if first arg doesn't match arrow or ArrowL(first arg) inconsist with second arg
  | NotMatchedLeft(ctx_typ, ctx_typ)
  | Union(ctx_marks, ctx_marks);

type ctx_ana_typ =
  | Hole
  | Ana
  | Lookup(var)
  | Just(typ)
  | Arrow(ctx_ana_typ, ctx_ana_typ)
  | ArrowL(ctx_ana_typ)
  | ArrowR(ctx_ana_typ);

type ctx_ana_marks =
  | Empty
  | NotInCtx(var)
  | NotMatchedArrow(ctx_ana_typ)
  | Inconsistent(ctx_ana_typ, ctx_ana_typ)
  // Mark if first arg doesn't match arrow or ArrowL(first arg) inconsist with second arg
  | NotMatchedLeft(ctx_ana_typ, ctx_ana_typ)
  | Union(ctx_ana_marks, ctx_ana_marks);

type syn_term_info = (ctx_typ, ctx_marks);

type ana_term_info = ctx_ana_marks;

type bare_syn_term =
  | Asc(typ, ana_term)
  | SynFun(var, typ, syn_term)
  | Ap(syn_term, ana_term)
  | Var(var)
  | Const
  | Hole

and syn_term = (bare_syn_term, syn_term_info)

and bare_ana_term =
  | Switch(syn_term)
  | AnaFun(var, typ, ana_term)

and ana_term = (bare_ana_term, ana_term_info);

type ctx = list((var, typ));

type syn_typ =
  | Hole
  | Syn
  | Just(typ)
  | Arrow(syn_typ, syn_typ)
  | ArrowL(syn_typ)
  | ArrowR(syn_typ);

type syn_marks =
  | Empty
  | Inconsistent(syn_typ, syn_typ)
  | NotMatchedArrow(syn_typ)
  | Union(syn_marks, syn_marks);

type syn_env_info = (ctx, syn_marks);

type ana_env_info = (ctx, typ);

type bare_syn_env =
  | ZRoot
  | ZSynFun(var, typ, syn_env)
  | ZLAp(ana_term, syn_env)
  | ZSwitch(ana_env)

and syn_env = (bare_syn_env, syn_env_info)

and bare_ana_env =
  | ZAsc(typ, syn_env)
  | ZRAp(syn_term, syn_env)
  | ZAnaFun(var, typ, ana_env)

and ana_env = (bare_ana_env, ana_env_info);

type typ_env =
  | ZLArrow(typ, typ_env)
  | ZRArrow(typ, typ_env)
  | ZTAsc(ana_term, syn_env)
  | ZTSynFun(var, syn_term, syn_env)
  | ZTAnaFun(var, ana_term, ana_env);

type zterm =
  | ZSynTerm(syn_env, syn_term)
  | ZAnaTerm(ana_env, ana_term)
  | ZTyp(typ_env, typ);

let syn_hole: syn_term = (Hole, (Hole, Empty));
let ana_hole: ana_term = (Switch(syn_hole), Empty);

let initial_zterm: zterm = ZSynTerm((ZRoot, ([], Empty)), syn_hole);

type dir =
  | Up
  | Down1
  | Down2;

type action =
  | Delete
  | InsertNum
  | InsertArrow
  | InsertConst
  | InsertVar(string)
  | InsertFun(string)
  | InsertAp
  | InsertAsc
  | Move(dir);

let rec ctx_ana_typ_of_ctx_typ = (t: ctx_typ): ctx_ana_typ =>
  switch (t) {
  | Hole => Hole
  | Lookup(var) => Lookup(var)
  | Just(typ) => Just(typ)
  | Arrow(t1, t2) =>
    Arrow(ctx_ana_typ_of_ctx_typ(t1), ctx_ana_typ_of_ctx_typ(t2))
  | ArrowL(t) => ArrowL(ctx_ana_typ_of_ctx_typ(t))
  | ArrowR(t) => ArrowR(ctx_ana_typ_of_ctx_typ(t))
  };

let rec ctx_ana_marks_of_ctx_marks = (m: ctx_marks): ctx_ana_marks =>
  switch (m) {
  | Empty => Empty
  | NotInCtx(var) => NotInCtx(var)
  | NotMatchedArrow(ctx_typ) =>
    NotMatchedArrow(ctx_ana_typ_of_ctx_typ(ctx_typ))
  | Inconsistent(t1, t2) =>
    Inconsistent(ctx_ana_typ_of_ctx_typ(t1), ctx_ana_typ_of_ctx_typ(t2))
  | NotMatchedLeft(t1, t2) =>
    NotMatchedLeft(ctx_ana_typ_of_ctx_typ(t1), ctx_ana_typ_of_ctx_typ(t2))
  | Union(m1, m2) =>
    Union(ctx_ana_marks_of_ctx_marks(m1), ctx_ana_marks_of_ctx_marks(m2))
  };

let rec sub_syn_typ = (t': syn_typ, t: syn_typ): syn_typ =>
  switch (t') {
  | Hole => Hole
  | Syn => t
  | Just(t) => Just(t)
  | Arrow(t1, t2) => Arrow(sub_syn_typ(t1, t), sub_syn_typ(t2, t))
  | ArrowL(t1) => ArrowL(sub_syn_typ(t1, t))
  | ArrowR(t1) => ArrowR(sub_syn_typ(t1, t))
  };

let rec sub_syn_marks = (m: syn_marks, t: syn_typ): syn_marks =>
  switch (m) {
  | Empty => Empty
  | Inconsistent(t1, t2) =>
    Inconsistent(sub_syn_typ(t1, t), sub_syn_typ(t2, t))
  | NotMatchedArrow(t1) => NotMatchedArrow(sub_syn_typ(t1, t))
  | Union(m1, m2) => Union(sub_syn_marks(m1, t), sub_syn_marks(m2, t))
  };

let rec sub_ana_typ = (t': ctx_ana_typ, t: ctx_typ): ctx_typ =>
  switch (t') {
  | Hole => Hole
  | Ana => t
  | Lookup(var) => Lookup(var)
  | Just(t) => Just(t)
  | Arrow(t1, t2) => Arrow(sub_ana_typ(t1, t), sub_ana_typ(t2, t))
  | ArrowL(t1) => ArrowL(sub_ana_typ(t1, t))
  | ArrowR(t1) => ArrowR(sub_ana_typ(t1, t))
  };

let rec sub_ana_marks = (m: ctx_ana_marks, t: ctx_typ): ctx_marks =>
  switch (m) {
  | Empty => Empty
  | NotInCtx(var) => NotInCtx(var)
  | NotMatchedArrow(t1) => NotMatchedArrow(sub_ana_typ(t1, t))
  | Inconsistent(t1, t2) =>
    Inconsistent(sub_ana_typ(t1, t), sub_ana_typ(t2, t))
  | NotMatchedLeft(t1, t2) =>
    NotMatchedLeft(sub_ana_typ(t1, t), sub_ana_typ(t2, t))
  | Union(m1, m2) => Union(sub_ana_marks(m1, t), sub_ana_marks(m2, t))
  };

let rec sub_ana_typ' = (t': ctx_ana_typ, t: ctx_ana_typ): ctx_ana_typ =>
  switch (t') {
  | Hole => Hole
  | Ana => t
  | Lookup(var) => Lookup(var)
  | Just(t) => Just(t)
  | Arrow(t1, t2) => Arrow(sub_ana_typ'(t1, t), sub_ana_typ'(t2, t))
  | ArrowL(t1) => ArrowL(sub_ana_typ'(t1, t))
  | ArrowR(t1) => ArrowR(sub_ana_typ'(t1, t))
  };

let rec sub_ana_marks' = (m: ctx_ana_marks, t: ctx_ana_typ): ctx_ana_marks =>
  switch (m) {
  | Empty => Empty
  | NotInCtx(var) => NotInCtx(var)
  | NotMatchedArrow(t1) => NotMatchedArrow(sub_ana_typ'(t1, t))
  | Inconsistent(t1, t2) =>
    Inconsistent(sub_ana_typ'(t1, t), sub_ana_typ'(t2, t))
  | NotMatchedLeft(t1, t2) =>
    NotMatchedLeft(sub_ana_typ'(t1, t), sub_ana_typ'(t2, t))
  | Union(m1, m2) => Union(sub_ana_marks'(m1, t), sub_ana_marks'(m2, t))
  };

let arrowL = (t: typ): typ =>
  switch (t) {
  | Arrow(t, _) => t
  | _ => Hole
  };

let arrowR = (t: typ): typ =>
  switch (t) {
  | Arrow(_, t) => t
  | _ => Hole
  };

let rec sub_ctx_typ = (t: ctx_typ, ctx: ctx): typ =>
  switch (t) {
  | Hole => Hole
  | Just(t) => t
  | Lookup(var) =>
    switch (List.assoc_opt(var, ctx)) {
    | None => Hole
    | Some(t') => t'
    }
  | Arrow(t1, t2) => Arrow(sub_ctx_typ(t1, ctx), sub_ctx_typ(t2, ctx))
  | ArrowL(t) => arrowL(sub_ctx_typ(t, ctx))
  | ArrowR(t) => arrowR(sub_ctx_typ(t, ctx))
  };

let invalid_var = (var: var) => String.trim(var) == "";

let apply_action = (z: zterm, a: action): zterm => {
  switch (z, a) {
  //
  // Delete
  //
  | (ZSynTerm(env, _), Delete) => ZSynTerm(env, syn_hole)
  | (ZAnaTerm(env, _), Delete) => ZAnaTerm(env, ana_hole)
  | (ZTyp(env, _), Delete) => ZTyp(env, Hole)
  //
  // Insertions
  //
  | (ZSynTerm(env, _), InsertConst) =>
    ZSynTerm(env, (Const, (Just(Num), Empty)))
  | (ZSynTerm(env, _), InsertVar(var)) =>
    invalid_var(var)
      ? z : ZSynTerm(env, (Var(var), (Lookup(var), NotInCtx(var))))
  | (ZSynTerm(env, _), InsertFun(var)) =>
    invalid_var(var)
      ? z
      : ZSynTerm(
          env,
          (SynFun(var, Hole, syn_hole), (Arrow(Hole, Hole), Empty)),
        )
  | (ZAnaTerm(env, _), InsertFun(var)) =>
    invalid_var(var)
      ? z : ZAnaTerm(env, (AnaFun(var, Hole, ana_hole), Empty))
  | (ZSynTerm(env, _), InsertAp) =>
    ZSynTerm(env, (Ap(syn_hole, ana_hole), (Hole, Empty)))
  | (ZSynTerm(env, _), InsertAsc) =>
    ZSynTerm(env, (Asc(Hole, ana_hole), (Hole, Empty)))
  // | (ZAnaTerm(env, x), _) => todo: auto switch insert
  | (ZTyp(env, _), InsertNum) => ZTyp(env, Num)
  | (ZTyp(env, _), InsertArrow) => ZTyp(env, Arrow(Hole, Hole))
  //
  // Move Up
  //
  // SynFun
  | (ZSynTerm((ZSynFun(var, typ, env), _), e), Move(Up))
  | (ZTyp(ZTSynFun(var, e, env), typ), Move(Up)) =>
    let (_, (syn, marks)) = e;
    ZSynTerm(env, (SynFun(var, typ, e), (Arrow(Just(typ), syn), marks)));
  // AnaFun
  | (ZTyp(ZTAnaFun(var, e, env), typ), Move(Up))
  | (ZAnaTerm((ZAnaFun(var, typ, env), _), e), Move(Up)) =>
    let (_, marks) = e;
    let marks' = sub_ana_marks'(marks, ArrowR(Ana));
    let marks'': ctx_ana_marks =
      Union(NotMatchedLeft(Ana, Just(typ)), marks');
    ZAnaTerm(env, (AnaFun(var, typ, e), marks''));
  // Ap
  | (ZSynTerm((ZLAp(e2, env), _), e1), Move(Up))
  | (ZAnaTerm((ZRAp(e1, env), _), e2), Move(Up)) =>
    let (_, (syn, marks1)) = e1;
    let (_, marks2) = e2;
    let marks2' = sub_ana_marks(marks2, ArrowL(syn));
    let marks: ctx_marks =
      Union(NotMatchedArrow(syn), Union(marks1, marks2'));
    ZSynTerm(env, (Ap(e1, e2), (ArrowR(syn), marks)));
  // Asc
  | (ZAnaTerm((ZAsc(typ, env), _), e), Move(Up))
  | (ZTyp(ZTAsc(e, env), typ), Move(Up)) =>
    let (_, marks) = e;
    let marks' = sub_ana_marks(marks, Just(typ));
    ZSynTerm(env, (Asc(typ, e), (Just(typ), marks')));
  // Switch
  | (ZSynTerm((ZSwitch(env), _), e), Move(Up)) =>
    let (_, (syn, marks)) = e;
    let syn' = ctx_ana_typ_of_ctx_typ(syn);
    let marks' = ctx_ana_marks_of_ctx_marks(marks);
    ZAnaTerm(env, (Switch(e), Union(Inconsistent(syn', Ana), marks')));
  // Arrow
  | (ZTyp(ZLArrow(t2, env), t1), Move(Up))
  | (ZTyp(ZRArrow(t1, env), t2), Move(Up)) => ZTyp(env, Arrow(t1, t2))
  //
  // Move Down
  //
  // SynFun
  | (ZSynTerm(env, (SynFun(var, typ, e), _)), Move(Down1)) =>
    ZTyp(ZTSynFun(var, e, env), typ)
  | (ZSynTerm(env, (SynFun(var, typ, e), _)), Move(Down2)) =>
    let (_, (ctx, marks)) = env;
    let marks' = sub_syn_marks(marks, Arrow(Just(typ), Syn));
    ZSynTerm(
      (ZSynFun(var, typ, env), ([(var, typ), ...ctx], marks')),
      e,
    );
  // AnaFun
  | (ZAnaTerm(env, (AnaFun(var, typ, e), _)), Move(Down1)) =>
    ZTyp(ZTAnaFun(var, e, env), typ)
  | (ZAnaTerm(env, (AnaFun(var, typ, e), _)), Move(Down2)) =>
    let (_, (ctx, ana)) = env;
    let ana' = arrowR(ana);
    ZAnaTerm((ZAnaFun(var, typ, env), ([(var, typ), ...ctx], ana')), e);
  // Ap
  | (ZSynTerm(env, (Ap(e1, e2), _)), Move(Down1)) =>
    let (_, (ctx, marks)) = env;
    let marks' =
      Union(NotMatchedArrow(Syn), sub_syn_marks(marks, ArrowL(Syn)));
    ZSynTerm((ZLAp(e2, env), (ctx, marks')), e1);
  | (ZSynTerm(env, (Ap(e1, e2), _)), Move(Down2)) =>
    let (_, (ctx, _)) = env;
    let (_, (syn, _)) = e1;
    let ana = sub_ctx_typ(ArrowL(syn), ctx);
    ZAnaTerm((ZRAp(e1, env), (ctx, ana)), e2);
  // Asc
  | (ZSynTerm(env, (Asc(t, e), _)), Move(Down1)) =>
    ZTyp(ZTAsc(e, env), t)
  | (ZSynTerm(env, (Asc(t, e), _)), Move(Down2)) =>
    let (_, (ctx, _)) = env;
    ZAnaTerm((ZAsc(t, env), (ctx, t)), e);
  // Switch
  | (ZAnaTerm(env, (Switch(e), _)), Move(Down1)) =>
    let (_, (ctx, ana)) = env;
    let marks = Inconsistent(Syn, Just(ana));
    ZSynTerm((ZSwitch(env), (ctx, marks)), e);
  // Arrow
  | (ZTyp(env, Arrow(t1, t2)), Move(Down1)) => ZTyp(ZLArrow(t2, env), t1)
  | (ZTyp(env, Arrow(t1, t2)), Move(Down2)) => ZTyp(ZRArrow(t1, env), t2)
  // | (ZSynTerm(_), InsertNum) => z
  // | (ZAnaTerm(_), InsertNum) => z
  // | (ZSynTerm(_), InsertArrow) => z
  // | (ZAnaTerm(_), InsertArrow) => z
  // | (ZSynTerm(_, (Var(_),_)), Move(Down1)) => z
  | _ => z
  };
};

// let rec string_of_typ = (t: typ): string =>
//   switch (t) {
//   | Hole => "Hole"
//   | Num => "Num"
//   | Arrow(t1, t2) =>
//     "Arrow(" ++ string_of_typ(t1) ++ "," ++ string_of_typ(t2) ++ ")"
//   };

let rec string_of_typ = (t: typ): string =>
  switch (t) {
  | Hole => "?"
  | Num => "Num"
  | Arrow(t1, t2) =>
    "(" ++ string_of_typ(t1) ++ " → " ++ string_of_typ(t2) ++ ")"
  };

// let rec string_of_syn_term = ((e, _): syn_term): string => {
//   switch (e) {
//   | Asc(typ, ana_term) =>
//     "Asc("
//     ++ string_of_typ(typ)
//     ++ ","
//     ++ string_of_ana_term(ana_term)
//     ++ ")"
//   | SynFun(var, typ, syn_term) =>
//     "SynFun("
//     ++ var
//     ++ ","
//     ++ string_of_typ(typ)
//     ++ ","
//     ++ string_of_syn_term(syn_term)
//     ++ ")"
//   | Ap(syn_term, ana_term) =>
//     "Ap("
//     ++ string_of_syn_term(syn_term)
//     ++ ","
//     ++ string_of_ana_term(ana_term)
//     ++ ")"
//   | Var(var) => "Var(" ++ var ++ ")"
//   | Const => "Const"
//   | Hole => "Hole"
//   };
// }

let rec string_of_syn_term = ((e, _): syn_term): string => {
  switch (e) {
  | Asc(typ, ana_term) =>
    "(" ++ string_of_ana_term(ana_term) ++ " : " ++ string_of_typ(typ) ++ ")"
  | SynFun(var, typ, syn_term) =>
    "("
    ++ var
    ++ " : "
    ++ string_of_typ(typ)
    ++ ") ↦ "
    ++ string_of_syn_term(syn_term)
  // ++ ")"
  | Ap(syn_term, ana_term) =>
    // "(" ++
    string_of_syn_term(syn_term)
    ++ "("
    ++ string_of_ana_term(ana_term)
    ++ ")"
  | Var(var) => var
  | Const => "42"
  | Hole => "?"
  };
}

// and string_of_ana_term = ((e, _): ana_term): string =>
//   switch (e) {
//   | Switch(syn_term) => "Switch(" ++ string_of_syn_term(syn_term) ++ ")"
//   | AnaFun(var, typ, ana_term) =>
//     "AnaFun("
//     ++ var
//     ++ ","
//     ++ string_of_typ(typ)
//     ++ ","
//     ++ string_of_ana_term(ana_term)
//     ++ ")"
//   };

and string_of_ana_term = ((e, _): ana_term): string =>
  switch (e) {
  | Switch(syn_term) => "switch(" ++ string_of_syn_term(syn_term) ++ ")"
  | AnaFun(var, typ, ana_term) =>
    "("
    ++ var
    ++ " : "
    ++ string_of_typ(typ)
    ++ ") ↦ "
    ++ string_of_ana_term(ana_term)
  };

// let rec strings_of_syn_env = ((e, _): syn_env): (string, string) =>
//   switch (e) {
//   | ZRoot => ("", "")
//   | ZSynFun(var, typ, syn_env) =>
//     let (a, b) = strings_of_syn_env(syn_env);
//     (a ++ "SynFun(" ++ var ++ "," ++ string_of_typ(typ) ++ ",", ")" ++ b);
//   | ZLAp(ana_term, syn_env) =>
//     let (a, b) = strings_of_syn_env(syn_env);
//     (a ++ "Ap(", "," ++ string_of_ana_term(ana_term) ++ ")" ++ b);
//   | ZSwitch(ana_env) =>
//     let (a, b) = strings_of_ana_env(ana_env);
//     (a ++ "Switch(", ")" ++ b);
//   }

let rec strings_of_syn_env = ((e, _): syn_env): (string, string) =>
  switch (e) {
  | ZRoot => ("", "")
  | ZSynFun(var, typ, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (a ++ "(" ++ var ++ " : " ++ string_of_typ(typ) ++ ") ↦ ", b);
  | ZLAp(ana_term, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (a, "(" ++ string_of_ana_term(ana_term) ++ ")" ++ b);
  | ZSwitch(ana_env) =>
    let (a, b) = strings_of_ana_env(ana_env);
    (a ++ "switch(", ")" ++ b);
  }

// and strings_of_ana_env = ((e, _): ana_env): (string, string) =>
//   switch (e) {
//   | ZAsc(typ, syn_env) =>
//     let (a, b) = strings_of_syn_env(syn_env);
//     (a ++ "Asc(" ++ string_of_typ(typ) ++ ",", ")" ++ b);
//   | ZRAp(syn_term, syn_env) =>
//     let (a, b) = strings_of_syn_env(syn_env);
//     (a ++ "Ap(" ++ string_of_syn_term(syn_term) ++ ",", ")" ++ b);
//   | ZAnaFun(var, typ, ana_env) =>
//     let (a, b) = strings_of_ana_env(ana_env);
//     (a ++ "AnaFun(" ++ var ++ "," ++ string_of_typ(typ) ++ ",", ")" ++ b);
//   };

and strings_of_ana_env = ((e, _): ana_env): (string, string) =>
  switch (e) {
  | ZAsc(typ, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (a ++ "(", " : " ++ string_of_typ(typ) ++ ")" ++ b);
  | ZRAp(syn_term, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (a ++ string_of_syn_term(syn_term) ++ "(", ")" ++ b);
  | ZAnaFun(var, typ, ana_env) =>
    let (a, b) = strings_of_ana_env(ana_env);
    (a ++ "(" ++ var ++ " : " ++ string_of_typ(typ) ++ ") ↦ ", b);
  };

// let rec strings_of_typ_env = (e: typ_env): (string, string) =>
//   switch (e) {
//   | ZLArrow(typ, typ_env) =>
//     let (a, b) = strings_of_typ_env(typ_env);
//     (a ++ "Arrow(", "," ++ string_of_typ(typ) ++ ")" ++ b);
//   | ZRArrow(typ, typ_env) =>
//     let (a, b) = strings_of_typ_env(typ_env);
//     (a ++ "Arrow(" ++ string_of_typ(typ) ++ ",", ")" ++ b);
//   | ZTAsc(ana_term, syn_env) =>
//     let (a, b) = strings_of_syn_env(syn_env);
//     (a ++ "Asc(", "," ++ string_of_ana_term(ana_term) ++ ")" ++ b);
//   | ZTSynFun(var, syn_term, syn_env) =>
//     let (a, b) = strings_of_syn_env(syn_env);
//     (
//       a ++ "SynFun(" ++ var ++ ",",
//       "," ++ string_of_syn_term(syn_term) ++ ")" ++ b,
//     );
//   | ZTAnaFun(var, ana_term, ana_env) =>
//     let (a, b) = strings_of_ana_env(ana_env);
//     (
//       a ++ "AnaFun(" ++ var ++ ",",
//       "," ++ string_of_ana_term(ana_term) ++ ")" ++ b,
//     );
//   };

let rec strings_of_typ_env = (e: typ_env): (string, string) =>
  switch (e) {
  | ZLArrow(typ, typ_env) =>
    let (a, b) = strings_of_typ_env(typ_env);
    (a ++ "(", " → " ++ string_of_typ(typ) ++ ")" ++ b);
  | ZRArrow(typ, typ_env) =>
    let (a, b) = strings_of_typ_env(typ_env);
    (a ++ "(" ++ string_of_typ(typ) ++ " → ", ")" ++ b);
  | ZTAsc(ana_term, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (a ++ "(" ++ string_of_ana_term(ana_term) ++ " : ", ")" ++ b);
  | ZTSynFun(var, syn_term, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (
      a ++ "(" ++ var ++ " : ",
      ") ↦ " ++ string_of_syn_term(syn_term) ++ b,
    );
  | ZTAnaFun(var, ana_term, ana_env) =>
    let (a, b) = strings_of_ana_env(ana_env);
    (
      a ++ "(" ++ var ++ " : ",
      ") ↦ " ++ string_of_ana_term(ana_term) ++ b,
    );
  };

let string_of_zterm = (z: zterm): string =>
  switch (z) {
  | ZSynTerm(syn_env, syn_term) =>
    let (a, b) = strings_of_syn_env(syn_env);
    a ++ ">" ++ string_of_syn_term(syn_term) ++ "<" ++ b;
  | ZAnaTerm(ana_env, ana_term) =>
    let (a, b) = strings_of_ana_env(ana_env);
    a ++ ">" ++ string_of_ana_term(ana_term) ++ "<" ++ b;
  | ZTyp(typ_env, typ) =>
    let (a, b) = strings_of_typ_env(typ_env);
    a ++ ">" ++ string_of_typ(typ) ++ "<" ++ b;
  };

let string_of_ctx = ctx =>
  String.concat(
    ",",
    List.map(((x, t)) => x ++ " : " ++ string_of_typ(t), ctx),
  );
let string_of_syn_env_info = ((ctx, _): syn_env_info) =>
  "Context: " ++ string_of_ctx(ctx);
let string_of_ana_env_info = ((ctx, ana): ana_env_info) =>
  "Context: " ++ string_of_ctx(ctx) ++ ", Ana: " ++ string_of_typ(ana);

let string_of_syn_term_info = ((ctx_typ, _): syn_term_info, ctx: ctx) =>
  "Syn: " ++ string_of_typ(sub_ctx_typ(ctx_typ, ctx));

let string_of_ana_term_info = (_: ana_term_info) => "";

let info_string_of_zterm = (z: zterm): string =>
  switch (z) {
  | ZSynTerm((_, i), (_, j)) =>
    let (ctx, _) = i;
    string_of_syn_env_info(i) ++ "|" ++ string_of_syn_term_info(j, ctx);
  | ZAnaTerm((_, i), (_, j)) =>
    string_of_ana_env_info(i) ++ "|" ++ string_of_ana_term_info(j)
  | ZTyp(_, _) => "|"
  };
