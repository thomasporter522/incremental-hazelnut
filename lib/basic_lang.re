type var = string;

type typ =
  | Hole
  | Arrow(typ, typ);

type syn_term =
  | Asc(typ, ana_term)
  | SynFun(var, typ, syn_term)
  | Ap(syn_term, ana_term)
  | Var(var)
  | Hole

and ana_term =
  | Switch(syn_term)
  | AnaFun(var, typ, ana_term);

type syn_env =
  | ZRoot
  | ZSynFun(var, typ, syn_env)
  | ZLAp(ana_term, syn_env)
  | ZSwitch(ana_env)

and ana_env =
  | ZAsc(typ, syn_env)
  | ZRAp(syn_term, syn_env)
  | ZAnaFun(var, typ, ana_env);

type typ_env =
  | ZLArrow(typ, typ_env)
  | ZRArrow(typ, typ_env)
  | ZTAsc(ana_term, syn_env)
  | ZTSynFun(var, syn_term, syn_env)
  | ZTAnaFun(var, ana_term, ana_env);

let rec string_of_typ = (t: typ): string =>
  switch (t) {
  | Hole => "Hole"
  | Arrow(t1, t2) =>
    "Arrow(" ++ string_of_typ(t1) ++ "," ++ string_of_typ(t2) ++ ")"
  };

let rec string_of_syn_term = (e: syn_term): string => {
  switch (e) {
  | Asc(typ, ana_term) =>
    "Asc("
    ++ string_of_typ(typ)
    ++ ","
    ++ string_of_ana_term(ana_term)
    ++ ")"
  | SynFun(var, typ, syn_term) =>
    "SynFun("
    ++ var
    ++ ","
    ++ string_of_typ(typ)
    ++ ","
    ++ string_of_syn_term(syn_term)
    ++ ")"
  | Ap(syn_term, ana_term) =>
    "Ap("
    ++ string_of_syn_term(syn_term)
    ++ string_of_ana_term(ana_term)
    ++ ")"
  | Var(var) => "Var(" ++ var ++ ")"
  | Hole => "Hole"
  };
}
and string_of_ana_term = (e: ana_term): string =>
  switch (e) {
  | Switch(syn_term) => "Switch(" ++ string_of_syn_term(syn_term) ++ ")"
  | AnaFun(var, typ, ana_term) =>
    "AnaFun("
    ++ var
    ++ ","
    ++ string_of_typ(typ)
    ++ ","
    ++ string_of_ana_term(ana_term)
    ++ ")"
  };

let rec strings_of_syn_env = (e: syn_env): (string, string) =>
  switch (e) {
  | ZRoot => ("", "")
  | ZSynFun(var, typ, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (a ++ "SynFun(" ++ var ++ "," ++ string_of_typ(typ) ++ ",", ")" ++ b);
  | ZLAp(ana_term, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (a ++ "Ap(", "," ++ string_of_ana_term(ana_term) ++ ")" ++ b);
  | ZSwitch(ana_env) =>
    let (a, b) = strings_of_ana_env(ana_env);
    (a ++ "Switch(", ")" ++ b);
  }
and strings_of_ana_env = (e: ana_env): (string, string) =>
  switch (e) {
  | ZAsc(typ, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (a ++ "Asc(" ++ string_of_typ(typ) ++ ",", ")" ++ b);
  | ZRAp(syn_term, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (a ++ "Ap(" ++ string_of_syn_term(syn_term) ++ ",", ")" ++ b);
  | ZAnaFun(var, typ, ana_env) =>
    let (a, b) = strings_of_ana_env(ana_env);
    (a ++ "AnaFun(" ++ var ++ "," ++ string_of_typ(typ) ++ ",", ")" ++ b);
  };

let rec strings_of_typ_env = (e: typ_env): (string, string) =>
  switch (e) {
  | ZLArrow(typ, typ_env) =>
    let (a, b) = strings_of_typ_env(typ_env);
    (a ++ "Arrow(" ++ string_of_typ(typ) ++ ",", ")" ++ b);
  | ZRArrow(typ, typ_env) =>
    let (a, b) = strings_of_typ_env(typ_env);
    (a ++ "Arrow(", "," ++ string_of_typ(typ) ++ ")" ++ b);
  | ZTAsc(ana_term, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (a ++ "Asc(", "," ++ string_of_ana_term(ana_term) ++ ")" ++ b);
  | ZTSynFun(var, syn_term, syn_env) =>
    let (a, b) = strings_of_syn_env(syn_env);
    (
      a ++ "SynFun(" ++ var ++ ",",
      "," ++ string_of_syn_term(syn_term) ++ ")" ++ b,
    );
  | ZTAnaFun(var, ana_term, ana_env) =>
    let (a, b) = strings_of_ana_env(ana_env);
    (
      a ++ "AnaFun(" ++ var ++ ",",
      "," ++ string_of_ana_term(ana_term) ++ ")" ++ b,
    );
  };
