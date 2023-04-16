use std::{cmp, env, io, collections::HashMap, fs::File, io::Write, time::Duration};
use egg::{rewrite as rw, *};
use symbolic_expressions::Sexp;

define_language! {
    enum ACCLang {
	"list" = List(Box<[Id]>),

	"functionCall" = Call(Box<[Id]>),

	"st"  = St( [Id; 4]),
	"st2" = St2([Id; 5]),
	"st3" = St3([Id; 6]),
	"st4" = St4([Id; 7]),
	"st5" = St5([Id; 8]),
	"st6" = St6([Id; 9]),

	"ld"  = Ld ([Id; 3]),
	"ld2" = Ld2([Id; 4]),
	"ld3" = Ld3([Id; 5]),
	"ld4" = Ld4([Id; 6]),
	"ld5" = Ld5([Id; 7]),
	"ld6" = Ld6([Id; 8]),

	"phi" = Phi([Id; 4]),

	"fma" = Fma([Id; 3]),
	"mfma" = Mfma([Id; 3]),

	"plusExpr" = Plus([Id; 2]),
	"minusExpr" = Minus([Id; 2]),
	"mulExpr" = Mul([Id; 2]),
	"divExpr" = Div([Id; 2]),
	"modExpr" = Mod([Id; 2]),
	"LshiftExpr" = Lshift([Id; 2]),
	"RshiftExpr" = Rshift([Id; 2]),
	"bitAndExpr" = BitAnd([Id; 2]),
	"bitOrExpr" = BitOr([Id; 2]),
	"bitXorExpr" = BitXor([Id; 2]),

	"logEQExpr" = LogEQ([Id; 2]),
	"logNEQExpr" = LogNEQ([Id; 2]),
	"logGEExpr" = LogGE([Id; 2]),
	"logGTExpr" = LogGT([Id; 2]),
	"logLEExpr" = LogLE([Id; 2]),
	"logLTExpr" = LogLT([Id; 2]),
	"logAndExpr" = LogAnd([Id; 2]),
	"logOrExpr" = LogOr([Id; 2]),

	"unaryMinusExpr" = UnaryMinus([Id; 1]),
	"bitNotExpr" = BitNot([Id; 1]),

	"condExpr" = Cond([Id; 3]),

	// This order is important; Num should be ahead of Symbol.
	Num(i32),
	Symbol(Symbol),
    }
}

fn make_rules() -> Vec<Rewrite<ACCLang, ACCAnalysis>> {
    vec![
	// rw!("st-st-reduce";
	//     "(st ?a (st ?a ?s ?i ?e1) ?i ?e2)" =>
	//     "(st ?a ?s ?i ?e2)"),
	// rw!("st2-st2-reduce";
	//     "(st2 ?a (st2 ?a ?s ?i1 ?i2 ?e1) ?i1 ?i2 ?e2)" =>
	//     "(st2 ?a ?s ?i1 ?i2 ?e2)"),
	// rw!("st3-st3-reduce";
	//     "(st3 ?a (st3 ?a ?s ?i1 ?i2 ?i3 ?e1) ?i1 ?i2 ?i3 ?e2)" =>
	//     "(st3 ?a ?s ?i1 ?i2 ?i3 ?e2)"),
	// rw!("st4-st4-reduce";
	//     "(st4 ?a (st4 ?a ?s ?i1 ?i2 ?i3 ?i4 ?e1) ?i1 ?i2 ?i3 ?i4 ?e2)" =>
	//     "(st4 ?a ?s ?i1 ?i2 ?i3 ?i4 ?e2)"),
	// rw!("st5-st5-reduce";
	//     "(st5 ?a (st5 ?a ?s ?i1 ?i2 ?i3 ?i4 ?i5 ?e1) ?i1 ?i2 ?i3 ?i4 ?i5 ?e2)" =>
	//     "(st5 ?a ?s ?i1 ?i2 ?i3 ?i4 ?i5 ?e2)"),
	// rw!("st6-st6-reduce";
	//     "(st6 ?a (st6 ?a ?s ?i1 ?i2 ?i3 ?i4 ?i5 ?i6 ?e1) ?i1 ?i2 ?i3 ?i4 ?i5 ?i6 ?e2)" =>
	//     "(st6 ?a ?s ?i1 ?i2 ?i3 ?i4 ?i5 ?i6 ?e2)"),

	// rw!("ld-st-reduce";
	//     "(ld ?a (st ?a ?s ?i ?e) ?i)" => "?e"),
	// rw!("ld2-st2-reduce";
	//     "(ld2 ?a (st2 ?a ?s ?i1 ?i2 ?e) ?i1 ?i2)" => "?e"),
	// rw!("ld3-st3-reduce";
	//     "(ld3 ?a (st3 ?a ?s ?i1 ?i2 ?i3 ?e) ?i1 ?i2 ?i3)" => "?e"),
	// rw!("ld4-st4-reduce";
	//     "(ld4 ?a (st4 ?a ?s ?i1 ?i2 ?i3 ?i4 ?e) ?i1 ?i2 ?i3 ?i4)" => "?e"),
	// rw!("ld5-st5-reduce";
	//     "(ld5 ?a (st5 ?a ?s ?i1 ?i2 ?i3 ?i4 ?i5 ?e) ?i1 ?i2 ?i3 ?i4 ?i5)" => "?e"),
	// rw!("ld6-st6-reduce";
	//     "(ld6 ?a (st6 ?a ?s ?i1 ?i2 ?i3 ?i4 ?i5 ?i6 ?e) ?i1 ?i2 ?i3 ?i4 ?i5 ?i6)" => "?e"),

	// // st and ld must have the same ?s
	// // ld referring to rewritten st sees ?a or original st -> it's valid
	// rw!("st-ld-reduce";
	//     "(st ?a ?s ?i (ld ?a ?s ?i))" => "?a"),
	// rw!("st2-ld2-reduce";
	//     "(st2 ?a ?s ?i1 ?i2 (ld2 ?a ?s ?i1 ?i2))" => "?a"),
	// rw!("st3-ld3-reduce";
	//     "(st3 ?a ?s ?i1 ?i2 ?i3 (ld3 ?a ?s ?i1 ?i2 ?i3))" => "?a"),
	// rw!("st4-ld4-reduce";
	//     "(st4 ?a ?s ?i1 ?i2 ?i3 ?i4 (ld4 ?a ?s ?i1 ?i2 ?i3 ?i4))" => "?a"),
	// rw!("st5-ld5-reduce";
	//     "(st5 ?a ?s ?i1 ?i2 ?i3 ?i4 ?i5 (ld5 ?a ?s ?i1 ?i2 ?i3 ?i4 ?i5))" => "?a"),
	// rw!("st6-ld6-reduce";
	//     "(st6 ?a ?s ?i1 ?i2 ?i3 ?i4 ?i5 ?i6 (ld6 ?a ?s ?i1 ?i2 ?i3 ?i4 ?i5 ?i6))" => "?a"),

	// rw!("ld-st-reduce2";
	//     "(ld ?a (st ?a ?s ?i1 ?e) ?i2)" => "(ld ?a ?s ?i2)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i1 ?i2) 0)", "1")),
	// rw!("ld2-st2-reduce2/1";
	//     "(ld2 ?a (st2 ?a ?s ?i11 ?i12 ?e) ?i21 ?i22)" => "(ld2 ?a ?s ?i21 ?i22)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i11 ?i21) 0)", "1")),
	// rw!("ld2-st2-reduce2/2";
	//     "(ld2 ?a (st2 ?a ?s ?i11 ?i12 ?e) ?i21 ?i22)" => "(ld2 ?a ?s ?i21 ?i22)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i12 ?i22) 0)", "1")),
	// rw!("ld3-st3-reduce2/1";
	//     "(ld3 ?a (st3 ?a ?s ?i11 ?i12 ?i13 ?e) ?i21 ?i22 ?i23)" => "(ld3 ?a ?s ?i21 ?i22 ?i23)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i11 ?i21) 0)", "1")),
	// rw!("ld3-st3-reduce2/2";
	//     "(ld3 ?a (st3 ?a ?s ?i11 ?i12 ?i13 ?e) ?i21 ?i22 ?i23)" => "(ld3 ?a ?s ?i21 ?i22 ?i23)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i12 ?i22) 0)", "1")),
	// rw!("ld3-st3-reduce2/3";
	//     "(ld3 ?a (st3 ?a ?s ?i11 ?i12 ?i13 ?e) ?i21 ?i22 ?i23)" => "(ld3 ?a ?s ?i21 ?i22 ?i23)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i13 ?i23) 0)", "1")),
	// rw!("ld4-st4-reduce2/1";
	//     "(ld4 ?a (st4 ?a ?s ?i11 ?i12 ?i13 ?i14 ?e) ?i21 ?i22 ?i23 ?i24)" => "(ld4 ?a ?s ?i21 ?i22 ?i23 ?i24)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i11 ?i21) 0)", "1")),
	// rw!("ld4-st4-reduce2/2";
	//     "(ld4 ?a (st4 ?a ?s ?i11 ?i12 ?i13 ?i14 ?e) ?i21 ?i22 ?i23 ?i24)" => "(ld4 ?a ?s ?i21 ?i22 ?i23 ?i24)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i12 ?i22) 0)", "1")),
	// rw!("ld4-st4-reduce2/3";
	//     "(ld4 ?a (st4 ?a ?s ?i11 ?i12 ?i13 ?i14 ?e) ?i21 ?i22 ?i23 ?i24)" => "(ld4 ?a ?s ?i21 ?i22 ?i23 ?i24)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i13 ?i23) 0)", "1")),
	// rw!("ld4-st4-reduce2/4";
	//     "(ld4 ?a (st4 ?a ?s ?i11 ?i12 ?i13 ?i14 ?e) ?i21 ?i22 ?i23 ?i24)" => "(ld4 ?a ?s ?i21 ?i22 ?i23 ?i24)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i14 ?i24) 0)", "1")),
	// rw!("ld5-st5-reduce2/1";
	//     "(ld5 ?a (st5 ?a ?s ?i11 ?i12 ?i13 ?i14 ?i15 ?e) ?i21 ?i22 ?i23 ?i24 ?i25)" => "(ld5 ?a ?s ?i21 ?i22 ?i23 ?i24 ?i25)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i11 ?i21) 0)", "1")),
	// rw!("ld5-st5-reduce2/2";
	//     "(ld5 ?a (st5 ?a ?s ?i11 ?i12 ?i13 ?i14 ?i15 ?e) ?i21 ?i22 ?i23 ?i24 ?i25)" => "(ld5 ?a ?s ?i21 ?i22 ?i23 ?i24 ?i25)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i12 ?i22) 0)", "1")),
	// rw!("ld5-st5-reduce2/3";
	//     "(ld5 ?a (st5 ?a ?s ?i11 ?i12 ?i13 ?i14 ?i15 ?e) ?i21 ?i22 ?i23 ?i24 ?i25)" => "(ld5 ?a ?s ?i21 ?i22 ?i23 ?i24 ?i25)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i13 ?i23) 0)", "1")),
	// rw!("ld5-st5-reduce2/4";
	//     "(ld5 ?a (st5 ?a ?s ?i11 ?i12 ?i13 ?i14 ?i15 ?e) ?i21 ?i22 ?i23 ?i24 ?i25)" => "(ld5 ?a ?s ?i21 ?i22 ?i23 ?i24 ?i25)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i14 ?i24) 0)", "1")),
	// rw!("ld5-st5-reduce2/5";
	//     "(ld5 ?a (st5 ?a ?s ?i11 ?i12 ?i13 ?i14 ?i15 ?e) ?i21 ?i22 ?i23 ?i24 ?i25)" => "(ld5 ?a ?s ?i21 ?i22 ?i23 ?i24 ?i25)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i15 ?i25) 0)", "1")),
	// rw!("ld6-st6-reduce2/1";
	//     "(ld6 ?a (st6 ?a ?s ?i11 ?i12 ?i13 ?i14 ?i15 ?i16 ?e) ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)" => "(ld6 ?a ?s ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i11 ?i21) 0)", "1")),
	// rw!("ld6-st6-reduce2/2";
	//     "(ld6 ?a (st6 ?a ?s ?i11 ?i12 ?i13 ?i14 ?i15 ?i16 ?e) ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)" => "(ld6 ?a ?s ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i12 ?i22) 0)", "1")),
	// rw!("ld6-st6-reduce2/3";
	//     "(ld6 ?a (st6 ?a ?s ?i11 ?i12 ?i13 ?i14 ?i15 ?i16 ?e) ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)" => "(ld6 ?a ?s ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i13 ?i23) 0)", "1")),
	// rw!("ld6-st6-reduce2/4";
	//     "(ld6 ?a (st6 ?a ?s ?i11 ?i12 ?i13 ?i14 ?i15 ?i16 ?e) ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)" => "(ld6 ?a ?s ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i14 ?i24) 0)", "1")),
	// rw!("ld6-st6-reduce2/5";
	//     "(ld6 ?a (st6 ?a ?s ?i11 ?i12 ?i13 ?i14 ?i15 ?i16 ?e) ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)" => "(ld6 ?a ?s ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i15 ?i25) 0)", "1")),
	// rw!("ld6-st6-reduce2/6";
	//     "(ld6 ?a (st6 ?a ?s ?i11 ?i12 ?i13 ?i14 ?i15 ?i16 ?e) ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)" => "(ld6 ?a ?s ?i21 ?i22 ?i23 ?i24 ?i25 ?i26)"
	//     if ConditionEqual::parse("(logNEQExpr (minusExpr ?i16 ?i26) 0)", "1")),

	// // st in ld always shares the same array name with the ld
	// // rw!("ld-st-reduce3";
	// //        "(ld ?v1 (st ?v2 ?s ?i ?e) ?i)"
	// //     => "(ld ?v1 ?s ?i)" if is_not_same(var("?v1"), var("?v2")) ),
	// //
	// // X no reorder among st (cannot keep stores)
	// // (st (st a _ ...) b _ ...) => (st (st b _ ...) a _ ...)
	// // (st (st a _ ...) a _ ...) => (st (st a _ ...) a _ ...)
	// //
	// // X no reduce among st (ld would miss some of them)
	// // (st (st a _ ...) b _ ...) => (st a _ ...)

	// // phi
	// rw!("phi-true";  "(phi ?a ?cond ?b ?c)" => "?b"
	//     if ConditionEqual::parse("(logEQExpr ?cond 0)", "0")),
	// rw!("phi-false"; "(phi ?a 0 ?b ?c)" => "?c"),

	// rw!("cond-true"; "(condExpr ?cond ?a ?b)" => "?a"
	//     if ConditionEqual::parse("(logEQExpr ?cond 0)", "0")),
	// rw!("cond-false";
	//     "(condExpr 0 ?a ?b)" => "?b"),

	// rw!("logEQExpr-same"; "(logEQExpr ?a ?a)" => "1"),
	// rw!("logNEQExpr-same"; "(logNEQExpr ?a ?a)" => "0"),
	// rw!("logGEExpr-same"; "(logGEExpr ?a ?a)" => "1"),
	// rw!("logGTExpr-same"; "(logGTExpr ?a ?a)" => "0"),
	// rw!("logLEExpr-same"; "(logLEExpr ?a ?a)" => "1"),
	// rw!("logLTExpr-same"; "(logLTExpr ?a ?a)" => "0"),

	// rw!("logEQExpr-not";
	//     "(bitNotExpr (logEQExpr ?a ?b))" => "(logNEQExpr ?a ?b)"),
	// rw!("logNEQExpr-not";
	//     "(bitNotExpr (logNEQExpr ?a ?b))" => "(logEQExpr ?a ?b)"),

	// rw!("unaryMinusExpr"; "(unaryMinusExpr ?a)" => "(minusExpr 0 ?a)"),

        rw!("fma"; "(plusExpr ?a (mulExpr ?b ?c))" => "(fma ?a ?b ?c)" if is_not_cycle()),
        rw!("mfma1"; "(minusExpr ?a (mulExpr ?b ?c))" => "(mfma ?a ?b ?c)" if is_not_cycle()),
        rw!("mfma2"; "(minusExpr (mulExpr ?b ?c) ?a)" => "(fma (unaryMinusExpr ?a) ?b ?c)" if is_not_cycle()),

        rw!("comm-add"; "(plusExpr ?a ?b)" => "(plusExpr ?b ?a)" if is_not_cycle()),
        rw!("comm-mul"; "(mulExpr ?a ?b)" => "(mulExpr ?b ?a)" if is_not_cycle()),

	rw!("assoc-add1"; "(plusExpr ?a (plusExpr ?b ?c))"
	    => "(plusExpr (plusExpr ?a ?b) ?c)" if is_not_cycle()),
	rw!("assoc-add2"; "(plusExpr (plusExpr ?a ?b) ?c)"
	    => "(plusExpr ?a (plusExpr ?b ?c))" if is_not_cycle()),
	rw!("assoc-mul1"; "(mulExpr ?a (mulExpr ?b ?c))"
	    => "(mulExpr (mulExpr ?a ?b) ?c)" if is_not_cycle()),
	rw!("assoc-mul2"; "(mulExpr (mulExpr ?a ?b) ?c)"
	    => "(mulExpr ?a (mulExpr ?b ?c))" if is_not_cycle()),

	// rw!("sub"; "(minusExpr ?a ?b)"
	//     => "(plusExpr ?a (unaryMinusExpr ?b))"
	//     if is_not_cycle()),
	// rw!("unaryMinus0"; "(unaryMinusExpr (mulExpr ?a ?b))"
	//     => "(mulExpr (mulExpr -1 ?a) ?b)"
	//     if is_not_cycle()),

        // rw!("add-0"; "(plusExpr ?a 0)" => "?a"),
	// rw!("add-cancel"; "(plusExpr (mulExpr ?a -1) ?a)" => "0"),
        // rw!("mul-0"; "(mulExpr ?a 0)" => "0"),
        // rw!("mul-1"; "(mulExpr ?a 1)" => "?a"),

	// rw!("sub-canon1"; "(minusExpr ?a (plusExpr ?b ?c))"
	//     => "(plusExpr ?a (plusExpr (mulExpr -1 ?b) (mulExpr -1 ?c)))"
	//     if is_not_cycle()),
	// rw!("sub-canon2"; "(minusExpr ?a (mulExpr ?b ?c))"
	//     => "(plusExpr ?a (mulExpr (mulExpr -1 ?b) ?c))"
	//     if is_not_cycle()),
	// rw!("sub-canon3"; "(minusExpr (plusExpr ?a ?b) ?c)"
	//     => "(plusExpr (plusExpr ?a ?b) (mulExpr -1 ?c))"
	//     if is_not_cycle()),
	// rw!("sub-canon4"; "(minusExpr (mulExpr ?a ?b) ?c)"
	//     => "(plusExpr (mulExpr ?a ?b) (mulExpr -1 ?c))"
	//     if is_not_cycle()),

	// // rw!("canon-sub"; "(plusExpr ?a (mulExpr -1 ?b))"
	// //     => "(minusExpr ?a ?b)"),
	// rw!("cancel-sub"; "(minusExpr ?a ?a)" => "0"),
	// rw!("distribute"; "(mulExpr ?a (plusExpr ?b ?c))"
        //     => "(plusExpr (mulExpr ?a ?b) (mulExpr ?a ?c))"
	//     if is_not_cycle()),
	// rw!("factor"    ; "(plusExpr (mulExpr ?a ?b) (mulExpr ?a ?c))"
	//     => "(mulExpr ?a (plusExpr ?b ?c))"
	//     if is_not_cycle()),

	// rw!("div-dist"; "(divExpr (plusExpr ?b ?c) ?a)"
	//     => "(plusExpr (divExpr ?b ?a) (divExpr ?c ?a))"
	//     if is_not_cycle()),
	// rw!("div-fact"; "(plusExpr (divExpr ?b ?a) (divExpr ?c ?a))"
	//     => "(divExpr (plusExpr ?b ?c) ?a)"
	//     if is_not_cycle()),
	// rw!("cancel-div"; "(divExpr ?a ?a)" => "1"),
	// rw!("cancel2-div"; "(mulExpr ?a (divExpr ?b ?a))" => "?b"),

    ]
}

fn is_not_cycle() -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    move |egraph, id, _subst| !egraph[id].data.cycle
}

type EGraph = egg::EGraph<ACCLang, ACCAnalysis>;

#[derive(Default)]
struct ACCAnalysis;

#[derive(Debug)]
struct Data {
    constant: Option<i32>,
    constantf: Option<f64>,
    descendant: Vec<Id>,
    cycle: bool,
    symbol: bool,
}

fn eval(egraph: &EGraph, enode: &ACCLang) -> Option<i32> {
    let x = |i: &Id| egraph[*i].data.constant;

    match enode {
	ACCLang::Num(n) => Some(*n),
	ACCLang::Plus([a, b]) => Some(x(a)? + x(b)?),
	ACCLang::Minus([a, b]) => Some(x(a)? - x(b)?),
	ACCLang::Mul([a, b]) => Some(x(a)? * x(b)?),
	ACCLang::Div([a, b]) => Some(x(a)? / x(b)?),
	ACCLang::Mod([a, b]) => Some(x(a)? % x(b)?),

	ACCLang::Lshift([a, b]) => Some(x(a)? << x(b)?),
	ACCLang::Rshift([a, b]) => Some(x(a)? >> x(b)?),
	ACCLang::BitAnd([a, b]) => Some(x(a)? & x(b)?),
	ACCLang::BitOr([a, b]) => Some(x(a)? | x(b)?),
	ACCLang::BitXor([a, b]) => Some(x(a)? ^ x(b)?),

	ACCLang::LogEQ([a, b]) => Some((x(a)? == x(b)?).into()),
	ACCLang::LogNEQ([a, b]) => Some((x(a)? != x(b)?).into()),
	ACCLang::LogGE([a, b]) => Some((x(a)? >= x(b)?).into()),
	ACCLang::LogGT([a, b]) => Some((x(a)? > x(b)?).into()),
	ACCLang::LogLE([a, b]) => Some((x(a)? <= x(b)?).into()),
	ACCLang::LogLT([a, b]) => Some((x(a)? < x(b)?).into()),
	ACCLang::LogAnd([a, b]) => Some((!(x(a)? == 0) && !(x(b)? == 0)).into()),
	ACCLang::LogOr([a, b]) => Some((!(x(a)? == 0) || !(x(b)? == 0)).into()),
	ACCLang::UnaryMinus([a]) => Some(-x(a)?),
	ACCLang::BitNot([a]) => Some(!x(a)?),

	_ => None,
    }
}

fn evalf(egraph: &EGraph, enode: &ACCLang) -> Option<f64> {
    let x = |i: &Id| egraph[*i].data.constantf;

    match enode {
	ACCLang::Num(n) => Some(*n as f64),
	ACCLang::Symbol(n) => Some((*n).as_str().parse::<f64>().ok()?),
	ACCLang::Plus([a, b]) => Some(x(a)? + x(b)?),
	ACCLang::Minus([a, b]) => Some(x(a)? - x(b)?),
	ACCLang::Mul([a, b]) => Some(x(a)? * x(b)?),
	ACCLang::Div([a, b]) => Some(x(a)? / x(b)?),
	ACCLang::UnaryMinus([a]) => Some(-x(a)?),
	_ => None,
    }
}

impl Analysis<ACCLang> for ACCAnalysis {
    type Data = Data;

    fn merge(&mut self, to: &mut Self::Data, from: Self::Data) -> DidMerge {
	to.constantf = to.constantf.or(from.constantf);
	to.symbol = to.symbol | from.symbol;
	merge_option(&mut to.constant, from.constant, |a, b| {
	    egg::merge_max(a, b)
	})
    }

    fn make(egraph: &EGraph, enode: &ACCLang) -> Self::Data {
	let constant = eval(egraph, enode);
	let constantf = evalf(egraph, enode);
	let descendant = vec![];
	let cycle = false;
	let symbol = match enode { ACCLang::Symbol(_) => true, _ => false };
	Data { constant, constantf, descendant, cycle, symbol }
    }

    fn modify(egraph: &mut EGraph, id: Id) {
	let mut v = vec![];
        for node in egraph[id].iter() {
            for &child in node.children() {
		v.push(child);
		v.append(&mut egraph[child].data.descendant.clone());
            }
	}
	v.sort();
	v.dedup();
	egraph[id].data.descendant.clear();
	egraph[id].data.descendant.extend(v);

	if let Some(_i) = egraph[id].data.descendant.iter().position(|&r| r == id) {
	    egraph[id].data.cycle = true;
	}

        if let Some(i) = egraph[id].data.constant {
            let added = egraph.add(ACCLang::Num(i));
            egraph.union(id, added);
        }
        else if let Some(i) = egraph[id].data.constantf {
            if !egraph[id].data.symbol {
                let added = egraph.add(ACCLang::Symbol(Symbol::from(format!("{:e}", i))));
                egraph.union(id, added);
            }
        }
    }
}

struct CostFn;

impl LpCostFunction<ACCLang, ACCAnalysis> for CostFn {
    fn node_cost(&mut self, _egraph: &EGraph, _eclass: Id,
		 enode: &ACCLang) -> f64
    {
	match enode {
	    ACCLang::List(_) => 1.0,
	    ACCLang::Symbol(_) => 1.0,
	    ACCLang::Phi(..) => 1.0,
	    ACCLang::Num(..) => 0.0,

	    ACCLang::Ld(..) => 100.0,
	    ACCLang::Ld2(..) => 100.0,
	    ACCLang::Ld3(..) => 100.0,
	    ACCLang::Ld4(..) => 100.0,
	    ACCLang::Ld5(..) => 100.0,
	    ACCLang::Ld6(..) => 100.0,

	    ACCLang::St(..) => 100.0,
	    ACCLang::St2(..) => 100.0,
	    ACCLang::St3(..) => 100.0,
	    ACCLang::St4(..) => 100.0,
	    ACCLang::St5(..) => 100.0,
	    ACCLang::St6(..) => 100.0,

	    ACCLang::Plus(..) => 10.0,
	    ACCLang::Minus(..) => 10.0,
	    ACCLang::Mul(..) => 10.0,
	    ACCLang::Fma(..) => 10.0,
	    ACCLang::Mfma(..) => 10.0,

	    ACCLang::Lshift(..) => 10.0,
	    ACCLang::Rshift(..) => 10.0,
	    ACCLang::BitAnd(..) => 10.0,
	    ACCLang::BitOr(..) => 10.0,
	    ACCLang::BitXor(..) => 10.0,
	    ACCLang::LogEQ(..) => 10.0,
	    ACCLang::LogNEQ(..) => 10.0,
	    ACCLang::LogGE(..) => 10.0,
	    ACCLang::LogGT(..) => 10.0,
	    ACCLang::LogLE(..) => 10.0,
	    ACCLang::LogLT(..) => 10.0,
	    ACCLang::LogAnd(..) => 10.0,
	    ACCLang::LogOr(..) => 10.0,
	    ACCLang::BitNot(..) => 10.0,
	    ACCLang::Cond(..) => 10.0,

	    ACCLang::UnaryMinus(..) => 10.0,

	    ACCLang::Div(..) => 100.0,
	    ACCLang::Mod(..) => 100.0,
	    ACCLang::Call(..) => 100.0,
	}
    }
}

trait ReplaceAdd {
    fn replace_add_expr(&mut self, expr: RecExpr<ACCLang>,
			map: &HashMap<String, Id>) -> Id;
    fn replace_add_expr_rec(&mut self, expr: &RecExpr<ACCLang>, i: usize,
			    map: &HashMap<String, Id>) -> Id;
    fn add_expr_rec_memo(&mut self, expr: &RecExpr<ACCLang>, i: usize,
			 memo: &mut HashMap<usize, Id>) -> Id;
}

// Add while replacing vars
impl ReplaceAdd for EGraph {
    fn replace_add_expr(&mut self, expr: RecExpr<ACCLang>,
			map: &HashMap<String, Id>) -> Id {
	let nodes = expr.as_ref();
	let last = nodes.len() - 1;
	return self.replace_add_expr_rec(&expr, last, &map);
    }

    fn replace_add_expr_rec(&mut self, expr: &RecExpr<ACCLang>, i: usize,
			    map: &HashMap<String, Id>) -> Id {
	let nodes = expr.as_ref();
	let node = nodes[i].clone();

	if node.is_leaf() {
	    return match map.get(&node.to_string()) {
		Some(&x) => x,
		None => self.add(node),
	    };
	} else {
	    let new_node = node.map_children(
		|i| self.replace_add_expr_rec(&expr, usize::from(i), &map));
	    return self.add(new_node);
	}
    }

    fn add_expr_rec_memo(&mut self, expr: &RecExpr<ACCLang>, i: usize,
			 memo: &mut HashMap<usize, Id>) -> Id {
	let nodes = expr.as_ref();
	let node = nodes[i].clone();

	if let Some(n) = memo.get(&i) {
	    return *n;
	}
	if node.is_leaf() {
	    let id = self.add(node);
	    memo.insert(i, id);
	    return id;
	}
	else {
	    let new_node = node.map_children(
		|i| self.add_expr_rec_memo(&expr, usize::from(i), memo));
	    let id = self.add(new_node);
	    memo.insert(i, id);
	    return id;
	}
    }
}

static TMPTAG: &str = "_v";

trait SSAConstruct {
    fn construct_ssa(&self, egraph: &mut EGraph,
		     top_vars: Vec<String>,
		     initpos: &HashMap<Id, usize>) -> Sexp;
    fn construct_ssa_ref(&self, pos: usize, hash: i32,
			 ref_hash: &mut HashMap<usize, Vec<i32>>);
    fn construct_ssa_count(&self, pos: usize,
			   ref_count: &mut HashMap<usize, usize>);
    fn construct_ssa_rename(&self, pos: usize,
			    ref_count: &mut HashMap<usize, usize>,
			    decl: &mut HashMap<usize, bool>,
			    naming: &mut HashMap<usize, String>);
    fn get_last_initpos(&self,
			egraph: &mut EGraph,
			pos: usize, initpos: &HashMap<Id, usize>,
			memo: &mut HashMap<usize, Id>) -> usize;
    fn get_shortest_st(&self, egraph: &mut EGraph,
		       ld_pos: usize, memo: &mut HashMap<usize, Id>) -> Id;
    fn make_expr(&self, pos: usize, naming: &HashMap<usize, String>) -> Sexp;
}

impl SSAConstruct for RecExpr<ACCLang> {
    fn construct_ssa(&self, egraph: &mut EGraph, top_vars: Vec<String>,
		     initpos: &HashMap<Id, usize>) -> Sexp {
	let nodes = self.as_ref();
	let n = nodes.len() - 1;

	let mut ref_hash = HashMap::new();  // (i, [hash, ...])
	let mut ref_count = HashMap::new(); // (i, reference count)

	for (i, n) in nodes[n].children().iter().enumerate() {
	    self.construct_ssa_ref(usize::from(*n), cut_hash(&top_vars[i]),
				   &mut ref_hash);
	    self.construct_ssa_count(usize::from(*n), &mut ref_count);
	}

	if !env::var("ACCSAT_NOBULKLOAD").is_ok() {
		let mut upper = HashMap::new(); // (cid, top index after st)
		let mut memo = HashMap::new();

		for (i, id) in nodes[n].children().iter().enumerate() {
		    let pos = usize::from(*id);
		    let node = &nodes[pos];
		    if !node.is_leaf() && node.to_string()[0..2].eq("st") {
			let cid = egraph.add_expr_rec_memo(self, pos, &mut memo);
			if i == nodes[n].children().len() - 1 {
			    upper.insert(cid, i);
			}
			else {
			    upper.insert(cid, i+1);
			}
		    }
		}

		for (i, _) in ref_count.iter() {
		    let pos = *i;
		    let node = &nodes[pos];
		    if !node.is_leaf() && node.to_string()[0..2].eq("ld") {
			let x = cmp::max(
			    *upper.get(&self.get_shortest_st(egraph, pos, &mut memo))
				.unwrap_or(&0),
			    self.get_last_initpos(egraph, pos, initpos, &mut memo));
			self.construct_ssa_ref(pos, cut_hash(&top_vars[x]),
					       &mut ref_hash);
		    }
		}
	}

	let mut naming = HashMap::new(); // (i, varname)

	for (key, hash) in ref_hash.iter() {
	    let h = hash.iter().map(|i| i.to_string())
		.collect::<Vec<String>>().join("_");

	    let name = format!("{}{}__{}", TMPTAG, *key, h);

	    naming.insert(*key, name.clone());
	}

	let mut decl = HashMap::new(); // (i, already declared?)

	for x in nodes[n].children().iter() {
	    let pos = usize::from(*x);

	    // Preserve temporary variables declared before by processing
	    // variables from earlier declared ones. Temporary variables
	    // declared at the same spot (e.g. s = f + g) are sorted by
	    // sat.rkt so either eclass's order (right to left; check
	    // try_build_recexpr) or the opposite is allowed.
	    //
	    // e.g.) s1 = x + 1; s2 = (x + 1) * 2; s3 = s1; s4 = s1 + s2;
	    //       (Prevent s2 from reusing the register of (x + 1))
	    //
	    // e.g.) s1 = x + 1; s2 = (x + 1) * 2; s3 = s1; s4 = s2 * 2;
	    //       (Cannot reuse the register of (x + 1) )
	    self.construct_ssa_rename(pos, &mut ref_count, &mut decl, &mut naming);
	}

	let mut ssa = vec![]; // (varname expression)

	for (pos, varname) in &naming {
	    let e = self.make_expr(*pos, &naming);

	    // (var expr)
	    ssa.push(Sexp::List(vec![Sexp::String(varname.clone()), e]));
	}

	for (i, n) in nodes[n].children().iter().enumerate() {
	    let pos = usize::from(*n);

	    if let Some(x) = naming.get(&pos) {
		// (var tmp_var)
		ssa.push(Sexp::List(vec![Sexp::String(top_vars[i].clone()),
					 Sexp::String(x.clone())]));
	    }
	    else {
		// phi, leaf, or st (including the result of st-ld-reduce)
		let e = self.make_expr(pos, &naming);
		ssa.push(Sexp::List(vec![Sexp::String(top_vars[i].clone()), e]));
	    }
	}

	return Sexp::List(ssa);
    }

    fn get_shortest_st(&self, egraph: &mut EGraph, ld_pos: usize,
    		       memo: &mut HashMap<usize, Id>) -> Id {
	let id = egraph.add_expr_rec_memo(self, ld_pos, memo);
	let extractor = Extractor::new(&egraph, AstDepth);
	return extractor.find_best_node(id).children()[1];
    }

    fn get_last_initpos(&self, egraph: &mut EGraph,
			pos: usize, initpos: &HashMap<Id, usize>,
			memo: &mut HashMap<usize, Id>) -> usize {
	let nodes = self.as_ref();
	let node = nodes[pos].clone();
	let op = node.to_string();

	if node.is_leaf() {
	    return 0;
	}
	if op[0..2].eq("st") {
	    return 0;
	}
	if op.eq("phi") {
	    let cid = egraph.add_expr_rec_memo(self, pos, memo);
	    return initpos.get(&cid).unwrap_or(&0).clone();
	}

	// The second argument of ld is a store chain and could be phi
	node.children().iter()
	    .map(|n| self.get_last_initpos(egraph, usize::from(*n),
					   &initpos, memo) ).max().unwrap()
    }

    fn construct_ssa_ref(&self, pos: usize, hash: i32,
			 ref_hash: &mut HashMap<usize, Vec<i32>>) {
	let nodes = self.as_ref();
	let node = nodes[pos].clone();
	let op = node.to_string();

	// phi is treated as a normal variable (thus no variable assignment to phi)
	// phi works for preventing invalid expression sharing and perhaps for
	// better optimization when the condition is determined statistically.
	// The cost of phi is equal to a variable.
	//
	// The second argument of ld also prevents invalid sharing and it is
	// always the chain of st (which are executed by other statements).
	// So, the cost of ld is independent of the second argument.
	//
	// st needn't a temporary variable but arguments should be processed
	if !node.is_leaf() && op.ne(&"phi") {
	    if op[0..2].ne("st") {
		ref_hash.entry(pos).and_modify(|v| { (*v).push(hash); (*v).dedup() })
		    .or_insert(vec![hash]);
	    }

	    for (i, n) in node.children().iter().enumerate() {
		if i != 1 || (op[0..2].ne("ld") && op[0..2].ne("st")) {
		    self.construct_ssa_ref(usize::from(*n), hash, ref_hash);
		}
	    }
	}
    }

    fn construct_ssa_count(&self, pos: usize,
			   ref_count: &mut HashMap<usize, usize>) {
	let nodes = self.as_ref();
	let node = nodes[pos].clone();
	let op = node.to_string();

	if !node.is_leaf() && op.ne(&"phi") {
	    if let Some(_) = ref_count.get(&pos) {
		ref_count.entry(pos).and_modify(|v| *v += 1);
	    } else {
		ref_count.insert(pos, 1);

		for (i, n) in node.children().iter().enumerate() {
		    if i != 1 || (op[0..2].ne("ld") && op[0..2].ne("st")) {
			self.construct_ssa_count(usize::from(*n), ref_count);
			// Preserve indexes
			if op[0..2].eq("ld") || op[0..2].eq("st") {
			    if i != 1 && !(op[0..2].eq("st") &&
					   i == node.children().len() - 1) {
				self.construct_ssa_count(usize::from(*n),
							 ref_count);
			    }
			}
		    }
		}
	    }
	}
    }

    // SSA Gen
    //
    // Decrement the reference counts so that variable can be reused (e.g. fma)
    //
    // { { a; } b; } c;  =>  tmp_a_b_c=1; tmp_b_c=tmp_a_b_c; { { a; } b; } c;
    //                       // tmp_a_b_c is not reused because still ref>0
    // But,
    // { ... b; } c; d;  =>  { ... b; } tmp_b_c+=...; c; d;
    //                       // tmp_b_c is reused after the statement b
    //
    //
    // phi -> cut out the original variable
    //
    // Construct the expression (e.g. (plusExpr var var) )
    //
    // Support asg*; plusExpr => (v (asgPlusExpr v a)), mul, fma
    // (ignore: asgMinusExpr (b=a-b => b-=a; a bit costly) or asgDivExpr
    //

    fn construct_ssa_rename(&self, pos: usize,
			    ref_count: &mut HashMap<usize, usize>,
			    decl: &mut HashMap<usize, bool>,
    			    naming: &mut HashMap<usize, String>) {
	let nodes = self.as_ref();
	let node = nodes[pos].clone();
	let op = node.to_string();

	if !node.is_leaf() && op.ne(&"phi") {
	    if let None = decl.get(&pos) {
		for (i, n) in node.children().iter().enumerate() {
		    if i != 1 || (op[0..2].ne("ld") && op[0..2].ne("st")) {
			ref_count.entry(usize::from(*n)).and_modify(|v| *v += 1);
			self.construct_ssa_rename(usize::from(*n),
						  ref_count, decl, naming);
		    }
		}
		for (i, n) in node.children().iter().enumerate() {
		    if i != 1 || (op[0..2].ne("ld") && op[0..2].ne("st")) {
			ref_count.entry(usize::from(*n)).and_modify(|v| *v -= 1);
		    }
		}
	    }

	    if op[0..2].eq("st") {
		return;
	    }

	    ref_count.entry(pos).and_modify(|v| *v -= 1);

	    if let None = decl.get(&pos) {
		decl.insert(pos, true);
		// // Reuse varname (v(REUSE) + v(ORIGINAL) + __HASH_..)
		// // todo avoid reuse inside loops without initialization
		// if let Some(x) = node.children().iter()
		//     .find(|&&x| ref_count.get(&usize::from(x)) == Some(&0)) {
		//     let orig = naming.get(&pos).unwrap().clone();
		//     let reuse = cut_var(naming.get(&usize::from(*x)).unwrap());
		//     naming.insert(pos, format!("{}{}", reuse, orig));
		// }
	    }
	}
    }

    fn make_expr(&self, pos: usize, naming: &HashMap<usize, String>) -> Sexp {
	let nodes = self.as_ref();
	let node = nodes[pos].clone();
	let op = node.to_string();

	if node.is_leaf() {
	    return Sexp::String(op.clone());
	}
	if op.eq("phi") {
	    // cut out the original variable
	    let c = node.children()[0];
	    return self.make_expr(usize::from(c), &naming);
	}

	let mut vec = vec![Sexp::String(op.clone())];
	let mut args = node.children().iter()
	    .map(|i| {
		let n = usize::from(*i);
		if let Some(v) = naming.get(&n) {
		    return Sexp::String(v.clone());
		}
		else {
		    // leaf or phi
		    return self.make_expr(n, &naming);
		}
	    }).collect::<Vec<Sexp>>();
	vec.append(&mut args);

	if op[0..2].eq("st") || op[0..2].eq("ld") {
	    vec[2]=vec[1].clone(); // second argument to indicate the original var
	}
	else {
	    // Construct asg*
	    let varname = cut_first_var(naming.get(&pos).unwrap());

	    if op.eq("plusExpr") || op.eq("mulExpr") {
		let c0 = naming.get(&usize::from(node.children()[0]));
		let c1 = naming.get(&usize::from(node.children()[1]));

		let newop = if op.eq("plusExpr") {
		    "asgPlusExpr"
		} else {
		    "asgMulExpr"
		}.to_string();

		if c0.is_some() && cut_first_var(c0.unwrap()) == varname {
		    vec[0] = Sexp::String(newop);
		}
		else if c1.is_some() && cut_first_var(c1.unwrap()) == varname {
		    vec[0] = Sexp::String(newop);
		    let tmp = vec[1].clone();
		    vec[1] = vec[2].clone();
		    vec[2] = tmp;
		}
	    }

	    else if op.eq("fma") {
		let c0 = naming.get(&usize::from(node.children()[0]));

		if c0.is_some() && cut_first_var(c0.unwrap()) == varname {
		    vec[0] = Sexp::String("asgFma".to_string());
		}
	    }

	    else if op.eq("mfma") {
		let c0 = naming.get(&usize::from(node.children()[0]));

		if c0.is_some() && cut_first_var(c0.unwrap()) == varname {
		    vec[0] = Sexp::String("asgMfma".to_string());
		}
	    }

	    else if op.eq("minusExpr") || op.eq("divExpr") {
		let c0 = naming.get(&usize::from(node.children()[0]));

		let newop = if op.eq("minusExpr") {
		    "asgMinusExpr"
		} else {
		    "asgDivExpr"
		}.to_string();

		if c0.is_some() && cut_first_var(c0.unwrap()) == varname {
		    vec[0] = Sexp::String(newop);
		}
	    }
	}

	return Sexp::List(vec);
    }
}

// var_hash -> var
fn _cut_var(var: &String) -> String {
    let var_and_hash: Vec<&str> = var.split("__").collect();
    let var = var_and_hash[0];
    return var.to_string();
}

// {var0 + var + var ..} _ hash -> var0
fn cut_first_var(var: &String) -> String {
    let var_and_hash: Vec<&str> = var.split("__").collect();
    let var = var_and_hash[0].split(TMPTAG).collect::<Vec<&str>>()[1];
    return format!("{}{}", TMPTAG, var.to_string());
}

// var_hash -> hash<i32>
fn cut_hash(var: &String) -> i32 {
    let var_and_hash: Vec<&str> = var.split("__").collect();
    let hash = var_and_hash[1].parse::<i32>().unwrap();
    return hash;
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let path = &args[1];

    let mut input = String::new();

    io::stdin().read_line(&mut input).expect("Error: No input");

    // The order of s must follow that of declaration
    let s = symbolic_expressions::parser::parse_str(&input).unwrap();

    let mut egraph = EGraph::default();

    let mut map = HashMap::new();

    let mut top_args = vec![];
    let mut top_vars = vec![];
    let mut initpos = HashMap::new();
    let mut pending = vec![];

    if let Sexp::List(top) = &s {
	for i in &top[0..] {
	    if let Sexp::List(pair) = i {
		if let [a, b] = &pair[..] {
		    let var  = a.to_string();
		    let expr : RecExpr<ACCLang> = b.to_string().parse().unwrap();

		    let node = egraph.replace_add_expr(expr, &map);

		    map.insert(var.clone(), node);

		    let hash = cut_hash(&var);

		    if hash > 0 {
			while let Some(x) = pending.pop() {
			    initpos.insert(x, top_args.len());
			}
			top_args.push(node);
			top_vars.push(var);
		    }
		    else {
			pending.push(node);
		    }
		}
	    }
	}
    }

    let top = egraph.add(ACCLang::List(top_args.into_boxed_slice()));

    egraph.rebuild();

    // todo change the limit
    let mut runner =
		if env::var("ACCSAT_NOSAT").is_ok() {
			Runner::default().with_egraph(egraph)
		} else {
			Runner::default()
			.with_iter_limit(10)
			.with_node_limit(10_000)
			.with_time_limit(Duration::new(10, 0))
			.with_egraph(egraph)
			.run(&make_rules())
		};

	if !env::var("ACCSAT_NOSAT").is_ok() {
	    eprintln!("Stopped after {} iterations, reason: {:?}",
		      runner.iterations.len(), runner.stop_reason);
	}

    //runner.egraph.dot().to_dot("tmp.dot").unwrap();

    let cost_func = CostFn;

    let mut extractor = LpExtractor::new(&runner.egraph, cost_func);

    extractor.timeout(30.0);

    let lp_best = extractor.solve(top);

    let ssa = lp_best.construct_ssa(&mut runner.egraph, top_vars, &initpos);

    let mut w = File::create(path).unwrap();

    writeln!(&mut w, "{}", ssa).unwrap();
}
