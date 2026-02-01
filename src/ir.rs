use std::fs::File;
use std::io::Write;
use std::process::Command;
use std::{collections::HashMap};

use cranelift::codegen::Context;
use cranelift::codegen::ir::Function;
use cranelift::codegen::settings::Configurable;
use cranelift::codegen::{ir::{types, InstBuilder, Type, Value}, settings};
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift::module::{FuncId, Linkage, Module};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::{AbiParam, EntityRef};

use crate::{parser::{BinOp, Expr, Statements}, sym_table::SymTable};

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
	// hack to get around the borrow checker - module should not really be Option
	pub module: Option<ObjectModule>,
	pub builder_ctx: FunctionBuilderContext,
	pub functions: HashMap<String, Function>,
	pub functions_to_id: HashMap<String, FuncId>,
}

impl IRGenerator {
	pub fn new(ast: Vec<Statements>, gst: SymTable) -> Self {
		let cranelift_var_map = HashMap::new();
		let cranelift_signedness_map = HashMap::new();

		let mut flag_buildr = settings::builder();
		flag_buildr.set("use_colocated_libcalls", "false").unwrap();
		flag_buildr.set("is_pic", "true").unwrap();

		// TODO: take arguments for flag_buildr flags

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

		Self { ast, gst, cranelift_var_map, cranelift_signedness_map, module: Some(module), builder_ctx, functions: HashMap::new(), functions_to_id: HashMap::new() }
	}

	pub fn translate_std_func_name<'a>(yi_name: &'a str) -> &'a str {
		match yi_name {
			"друклн" => "drukln",
			"старт" => "main",
			_ => yi_name, // fallback to original if not in map
		}
	}


	pub fn add_std_functions(&mut self) {
		let module = self.module.as_mut().expect("Module not initialized");

		let mut sig = module.make_signature();
		// TODO: come back to this when args will be added!
		// sig.params.push(AbiParam::new(types::I64));
		sig.returns.push(AbiParam::new(types::I32)); // or void

		let println_name = Self::translate_std_func_name("друклн");

		let println_id = module
			.declare_function(println_name, Linkage::Import, &sig)
			.expect("Failed to declare друклн");

		self.functions_to_id.insert(println_name.to_string(), println_id);
	}

	pub fn generate(&mut self) {
		println!("using cranelift version {}", cranelift::frontend::VERSION);

		if !self.gst.contains("старт") {
			eeprintln!("програма має мати функцію старт()");
			std::process::exit(-1);
		}

		self.add_std_functions();

		let func_data: Vec<(String, Vec<Statements>)> = self.ast.iter()
			.filter_map(|stmt| {
				if let Statements::Func(id, statements) = stmt {
					self.functions.insert(id.clone(), Function::new());
					Some((id.clone(), statements.clone()))
				} else {
					None
				}
			})
			.collect();

		for (id, stmts) in func_data {
			self.gen_func(id, stmts);
		}

		// PRINTIN' TIME!
		let mut module = self.module.take().expect("Module missing");

		// TODO: error handling
		for (func_id, func) in self.functions.clone() {
			let new_func_id = if func_id == "старт" { "main" } else { &func_id };
			let func_id = module.declare_function(new_func_id, Linkage::Export, &func.signature).expect("Failed to declare function");
			let mut ctx = Context::for_function(func.clone());
			ctx.func = func;  // your Cranelift Function
			module.define_function(func_id, &mut ctx).expect("Failed to define function");

			self.functions_to_id.insert(new_func_id.to_string(), func_id);
		}

		let product = module.finish();
		let product_bytes = product.emit().unwrap();

		let mut file = File::create("output.o").unwrap();
		file.write_all(&product_bytes).unwrap();

		// TODO: dont depend on clang
		let link = Command::new("clang")
			.args(&["output.o", "libyi_std.a", "-o", "output"])
			.output()
			.expect("Failed to run linker");

		if !link.status.success() {
			eeprintln!("Лінкувати не вийшло:");
			eeprintln!("{}", String::from_utf8_lossy(&link.stderr));
			std::process::exit(-1);
		}

		let _ = std::fs::remove_file("output.o");
	}

	pub fn gen_func(&mut self, id: String, stmts: Vec<Statements>) {
		let mut builder_ctx = std::mem::take(&mut self.builder_ctx);

		let mut func = self.functions.remove(&id).unwrap();
		let mut func_builder = FunctionBuilder::new(&mut func, &mut builder_ctx);

		let entry_block = func_builder.create_block();
		func_builder.switch_to_block(entry_block);

		for stmt in stmts {
			if let Statements::Let(var_id, expr) = stmt {
				self.gen_let(var_id, expr, &mut func_builder);
			} else if let Statements::Assign(var_id, expr) = stmt {
				self.gen_assign(var_id, expr, &mut func_builder);
			} else if let Statements::Expr(expr) = stmt {
				self.gen_expr(*expr, &mut func_builder);
			}
		}

		func_builder.ins().return_(&[]);

		func_builder.seal_block(entry_block);

		func_builder.finalize();

		self.functions.insert(id.clone(), func);

		self.builder_ctx = builder_ctx;
	}

	pub fn gen_let(&mut self, var_id: String, expr: Box<Expr>, mut func_builder: &mut FunctionBuilder<'_>) {
		let expr = *expr;
		let (val, signedness) = self.gen_expr(expr.clone(), &mut func_builder);
		let ty = func_builder.func.dfg.value_type(val);
		let new_var = func_builder.declare_var(ty);

		func_builder.def_var(new_var, val);

		self.cranelift_var_map.insert(var_id.clone(), new_var.as_u32() as usize);
		self.cranelift_signedness_map.insert(var_id, signedness);
	}

	pub fn gen_assign(&mut self, var_id: String, expr: Box<Expr>, mut func_builder: &mut FunctionBuilder<'_>) {
		let var = self.cranelift_var_map.get(&var_id).copied();

		let (val, _signedness) = self.gen_expr(*expr.clone(), &mut func_builder);

		if let Some(var) = var {
			let variable = Variable::new(var);
			func_builder.def_var(variable, val);
		} else {
			eeprintln!("Змінна {} не задеклерована", var_id);
			std::process::exit(-1);
		}
	}

	pub fn gen_expr(&mut self, expr: Expr, builder: &mut FunctionBuilder) -> (Value, Signedness) {
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
			Expr::Str(_val) => {
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
			Expr::Call(id) => {
				let actual_name = Self::translate_std_func_name(&id);
				let module = self.module.as_mut();
				let func_id = self.functions_to_id.get(actual_name).unwrap();

				let func_ref = module.unwrap().declare_func_in_func(func_id.clone(), &mut builder.func);

				let call_inst = builder.ins().call(func_ref, &[]);
				let res = builder.inst_results(call_inst);

				if res.len() > 0 {
					(res[0], Signedness::Signed)
				} else {
					// Void function - return dummy value
					(builder.ins().iconst(types::I32, 0), Signedness::Signed)
				}

			}
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
