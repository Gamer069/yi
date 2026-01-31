use std::{collections::HashMap};

use cranelift::codegen::settings::Configurable;
use cranelift::codegen::{ir::{types, InstBuilder, Type, Value}, settings};
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift::module::Module;
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::EntityRef;

use crate::{parser::{BinOp, Expr, Statements, Symbol}, sym_table::SymTable};

macro_rules! eeprintln {
    ($($arg:tt)*) => {{
        eprintln!("{}помилка:{} {}", "\x1b[31m", "\x1b[0m", format!($($arg)*));
    }};
}

#[derive(Hash, Copy, Clone)]
pub enum Signedness {
    Signed,
    Unsigned,
}

impl Signedness {

    /// Returns `true` if the signedness is [`Signed`].
    ///
    /// [`Signed`]: Signedness::Signed
    #[must_use]
    pub fn is_signed(&self) -> bool {
        matches!(self, Self::Signed)
    }

    /// Returns `true` if the signedness is [`Unsigned`].
    ///
    /// [`Unsigned`]: Signedness::Unsigned
    #[must_use]
    pub fn is_unsigned(&self) -> bool {
        matches!(self, Self::Unsigned)
    }
}

pub struct IRGenerator {
    pub ast: Vec<Statements>,
    pub gst: SymTable, // global symbol table
    pub cranelift_var_map: HashMap<String, usize>,
    pub cranelift_signedness_map: HashMap<String, Signedness>,
    pub module: ObjectModule,
	pub builder_ctx: FunctionBuilderContext,
}

impl IRGenerator {
    pub fn new(ast: Vec<Statements>, gst: SymTable) -> Self {
        let mut cranelift_var_map = HashMap::new();
        let mut cranelift_signedness_map = HashMap::new();

        for (i, (key, sym)) in gst.iter().enumerate() {
            if let Symbol::Variable(expr) = sym {
                cranelift_var_map.insert(key.clone(), i);

                if let Expr::U64(_) = expr {
                    cranelift_signedness_map.insert(key.clone(), Signedness::Signed);
                } else {
                    cranelift_signedness_map.insert(key.clone(), Signedness::Unsigned);
                }
            }
        }

		let mut flag_buildr = settings::builder();
		flag_buildr.set("use_colocated_libcalls", "false").unwrap();
		flag_buildr.set("is_pic", "false").unwrap();

		let isa_buildr = cranelift::native::builder().unwrap_or_else(|msg| {
			eeprintln!("Хост-машина не підтримується: {}", msg);
			std::process::exit(-1);
		});

		let isa = isa_buildr
			.finish(settings::Flags::new(flag_buildr))
			.unwrap();

		let module = ObjectModule::new(
			ObjectBuilder::new(
				isa,
				"yi_runtime",
				cranelift::module::default_libcall_names()
			).unwrap()
		);

		let builder_ctx = FunctionBuilderContext::new();

        Self { ast, gst, cranelift_var_map, cranelift_signedness_map, module, builder_ctx }
    }

    pub fn generate(&mut self) {
        println!("using cranelift version {}", cranelift::frontend::VERSION);

		let mut sig = self.module.make_signature();

        if !self.gst.contains("старт") {
            eeprintln!("програма має мати функцію старт()");
            std::process::exit(-1);
        }

        let mut func: Option<(String, Vec<Statements>, FunctionBuilder)> = None;

		println!("{:#?}", self.ast.clone());
        // for stmt in self.ast.clone() {
            // match stmt {
                // Statements::Func(id, statements) => {
					// let new_func = Function::new();
					// let func_builder = FunctionBuilder::new(&mut new_func, &mut self.builder_ctx);
//
                    // func = Some((id, statements, func_builder));
                // },
                // Statements::Let(id, expr) => {
                    // self.gen_expr(*expr, builder);
                // },
                // _ => {}
            // }
        // }
    }

    pub fn gen_expr(&self, expr: Expr, builder: &mut FunctionBuilder) -> (Value, Signedness) {
        match expr {
            Expr::U64(val) => {
                (builder.ins().iconst(types::I64, val as i64), Signedness::Unsigned)
			},
            Expr::F64(val) => {
                (builder.ins().f64const(val), Signedness::Signed)
			},
            Expr::I64(val) => {
                (builder.ins().iconst(types::I64, val as i64), Signedness::Signed)
			},
            Expr::Str(val) => {
                unimplemented!();
			},
            Expr::Bool(val) => {
                if val {
                    (builder.ins().iconst(types::I8, 1), Signedness::Signed)
                } else {
                    (builder.ins().iconst(types::I8, 0), Signedness::Signed)
                }
			},
            Expr::Id(id) => {
                let index = self.cranelift_var_map.get(&id);
                if let Some(index) = index {
                    (builder.use_var(Variable::new(*index)), self.cranelift_signedness_map.get(&id).unwrap().clone())
                } else {
                    eeprintln!("{} не визначено", id);
                    std::process::exit(-1);
                }
			},
            Expr::BinOp(left, op, right) => {
                let (left_val, left_signedness) = self.gen_expr(*left, builder);
                let (right_val, right_signedness) = self.gen_expr(*right, builder);

                let left_ty = builder.func.dfg.value_type(left_val);
                let right_ty = builder.func.dfg.value_type(right_val);

                if self.type_rank(left_ty).is_some() && self.type_rank(right_ty).is_some() {
                    self.op(left_val, left_signedness, op, right_val, right_signedness, builder)
                } else {
                    eeprintln!("Не вийшло порівняти, лівий операнд не такого самого типу як правий операнд");
                    std::process::exit(-1);
                }
            },
        }
    }

    pub fn op(
        &self,
        left: Value,
        left_signedness: Signedness,
        op: BinOp,
        right: Value,
        right_signedness: Signedness,
        builder: &mut FunctionBuilder,
    ) -> (Value, Signedness) {
        let left_ty = builder.func.dfg.value_type(left);
        let right_ty = builder.func.dfg.value_type(right);

        let target_ty = self.highest_type(left_ty, right_ty).unwrap();

        let left_cast = self.cast(left, left_ty, left_signedness, target_ty, left_signedness, builder);
        let right_cast = self.cast(right, right_ty, right_signedness, target_ty, right_signedness, builder);

        let val = match op {
            BinOp::Add => {
                if target_ty.is_int() {
                    builder.ins().iadd(left_cast, right_cast)
                } else {
                    builder.ins().fadd(left_cast, right_cast)
                }
            }
            BinOp::Sub => {
                if target_ty.is_int() {
                    builder.ins().isub(left_cast, right_cast)
                } else {
                    builder.ins().fsub(left_cast, right_cast)
                }
            }
            BinOp::Mul => {
                if target_ty.is_int() {
                    builder.ins().imul(left_cast, right_cast)
                } else {
                    builder.ins().fmul(left_cast, right_cast)
                }
            }
            BinOp::Div => {
                if target_ty.is_int() {
                    if left_signedness.is_signed() {
                        builder.ins().sdiv(left_cast, right_cast)
                    } else {
                        builder.ins().udiv(left_cast, right_cast)
                    }
                } else {
                    builder.ins().fdiv(left_cast, right_cast)
                }
            }
            BinOp::EqEq => {
                if target_ty.is_int() {
                    builder.ins().icmp(
                        cranelift::codegen::ir::condcodes::IntCC::Equal,
                        left_cast,
                        right_cast,
                    )
                } else {
                    builder.ins().fcmp(
                        cranelift::codegen::ir::condcodes::FloatCC::Equal,
                        left_cast,
                        right_cast,
                    )
                }
            }
        };

        let result_signedness = match op {
            BinOp::EqEq => Signedness::Unsigned,
            _ => {
                if target_ty.is_int() {
                    left_signedness
                } else {
                    Signedness::Unsigned
                }
            }
        };

        (val, result_signedness)
    }

    pub fn cast(&self, val: Value, from: Type, from_signedness: Signedness, to: Type, to_signedness: Signedness, builder: &mut FunctionBuilder) -> Value {
        if from == to {
            val
        } else if from.is_int() && to.is_int() {
            if from_signedness.is_signed() {
                builder.ins().sextend(to, val)
            } else {
                builder.ins().uextend(to, val)
            }
        } else if from.is_int() && to.is_float() {
            if from_signedness.is_signed() {
                builder.ins().fcvt_from_sint(to, val)
            } else {
                builder.ins().fcvt_from_uint(to, val)
            }
        } else if from.is_float() && to.is_int() {
            if to_signedness.is_signed() {
                builder.ins().fcvt_to_sint(to, val)
            } else {
                builder.ins().fcvt_to_uint(to, val)
            }
        } else if from.is_float() && to.is_float() {
            let from_type_mass = from.lane_bits() * from.lane_count();
            let to_type_mass = to.lane_bits() * to.lane_count();
            if from_type_mass > to_type_mass {
                builder.ins().fpromote(to, val)
            } else {
                builder.ins().fdemote(to, val)
            }
        } else {
            eeprintln!("Не можна закастувати з {:?} до {:?}", from, to);
            std::process::exit(-1);
        }
    }

    pub fn type_rank(&self, ty: Type) -> Option<u8> {
        if ty == types::I8  { Some(1) }
        else if ty == types::I16 { Some(2) }
        else if ty == types::I32 { Some(3) }
        else if ty == types::I64 { Some(4) }
        else if ty == types::F32 { Some(5) }
        else if ty == types::F64 { Some(6) }
        else { None } // not a numeric type
    }

    pub fn highest_type(&self, a: Type, b: Type) -> Option<Type> {
        match (self.type_rank(a), self.type_rank(b)) {
            (Some(ra), Some(rb)) => {
                if ra >= rb { Some(a) } else { Some(b) }
            },
            _ => None,
        }
    }
}
