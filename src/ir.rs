use std::fs::File;
use std::io::Write;
use std::process::Command;
use std::{collections::HashMap};

use cranelift::codegen::Context;
use cranelift::codegen::ir::{BlockArg, Function};
use cranelift::codegen::settings::Configurable;
use cranelift::codegen::{ir::{types, InstBuilder, Type, Value}, settings};
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift::module::{DataDescription, FuncId, Linkage, Module};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::{AbiParam, EntityRef, Signature};

use crate::keywords::{self, TypeKw};
use crate::{parser::{BinOp, Expr, Statements}, sym_table::SymTable};

macro_rules! eeprintln {
	($($arg:tt)*) => {{
		eprintln!("{}помилка:{} {}", "\x1b[31m", "\x1b[0m", format!($($arg)*));
	}};
}

#[derive(Hash, Copy, Clone, Debug)]
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
	// lit -> mem
	pub string_literals: HashMap<String, usize>,
	pub ir: bool,
	pub verbose: bool,
}

impl IRGenerator {
	pub fn new(ast: Vec<Statements>, gst: SymTable, ir: bool, verbose: bool) -> Self {
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

		Self { ast, gst, ir, verbose, cranelift_var_map, cranelift_signedness_map, module: Some(module), builder_ctx, functions: HashMap::new(), functions_to_id: HashMap::new(), string_literals: HashMap::new() }
	}

	pub fn translate_std_func_name<'a>(yi_name: &'a str) -> &'a str {
		match yi_name {
			"друклн" => "drukln",
			"друклн_і64" => "drukln_i64",
			"друклн_стр" => "drukln_str",
			"друклн_бул" => "drukln_bool",

			"друк" => "druk",
			"друк_і64" => "druk_i64",
			"друк_стр" => "druk_str",
			"друк_бул" => "druk_bool",

			"скинути_вивід" => "skynuty_stdout",

			"старт" => "main",
			_ => yi_name, // fallback to original if not in map
		}
	}


	pub fn add_std_functions(&mut self) {
		self.add_std_function("друклн", &mut |sig: &mut Signature| Self::void(sig));
		self.add_std_function("друклн_і64", &mut |sig: &mut Signature| Self::arg_and(sig, types::I64, &mut Self::void));
		self.add_std_function("друклн_стр", &mut |sig: &mut Signature| Self::arg_and(sig, types::I64, &mut Self::void));
		self.add_std_function("друклн_бул", &mut |sig: &mut Signature| Self::arg_and(sig, types::I8, &mut Self::void));
		self.add_std_function("друк", &mut |sig: &mut Signature| Self::void(sig));
		self.add_std_function("друк_і64", &mut |sig: &mut Signature| Self::arg_and(sig, types::I64, &mut Self::void));
		self.add_std_function("друк_стр", &mut |sig: &mut Signature| Self::arg_and(sig, types::I64, &mut Self::void));
		self.add_std_function("друк_бул", &mut |sig: &mut Signature| Self::arg_and(sig, types::I8, &mut Self::void));
		self.add_std_function("скинути_вивід", &mut |sig: &mut Signature| Self::void(sig));
	}

	pub fn void(sig: &mut Signature) {
		sig.returns.push(AbiParam::new(types::I32));
	}

	pub fn arg(sig: &mut Signature, ty: Type) {
		sig.params.push(AbiParam::new(ty));
	}

	pub fn arg_and(sig: &mut Signature, ty: Type, modifier: &mut dyn FnMut(&mut Signature) -> ()) {
		sig.params.push(AbiParam::new(ty));
		modifier(sig);
	}

	pub fn add_std_function(&mut self, name: &str, sig_initializer: &mut dyn FnMut(&mut Signature) -> ()) {
		let module = self.module.as_mut().expect("Модуль не ініціалізовано");

		let mut sig = module.make_signature();
		sig_initializer(&mut sig);

		let name = Self::translate_std_func_name(name);

		let id = module
			.declare_function(name, Linkage::Import, &sig)
			.expect(&format!("Не вийшло задекларувати функцію {}", name));


		self.functions_to_id.insert(name.to_string(), id);
	}

	pub fn generate(&mut self) {
		eprintln!("Користуємо cranelift версію {}", cranelift::frontend::VERSION);

		if self.verbose {
			eprintln!("Перевірка наявності функції старт()...");
		}

		if !self.gst.contains("старт") {
			eeprintln!("програма має мати функцію старт()");
			std::process::exit(-1);
		}

		if self.verbose {
			eprintln!("Додавання стандартних функцій...");
		}

		self.add_std_functions();

		// PRINTIN' TIME!
		for stmt in &self.ast {
			if let Statements::Func(id, args, ret_ty, _) = stmt {
				if self.verbose {
					eprintln!("створення сигнатури для функції {} з аргументами {:#?} та типом повернення {:#?}", id, args, ret_ty);
				}

				let mut sig = self.module.as_mut().unwrap().make_signature();
				for (_arg_id, arg_ty) in args {
					sig.params.push(AbiParam::new(arg_ty.cranelift()));
				}
				if *ret_ty != keywords::TypeKw::Void {
					sig.returns.push(AbiParam::new(ret_ty.cranelift()));
				}

				// Map "старт" to "main" for linking
				let link_name = if id == "старт" { "main" } else { &id };

				let func_id = self.module.as_mut().unwrap()
					.declare_function(link_name, Linkage::Export, &sig)
					.expect("Не вийшло задекларувати функцію");

				self.functions_to_id.insert(id.clone(), func_id); // keep original name for calls
				self.functions.insert(id.clone(), Function::new());
			}
		}

		if self.verbose {
			eprintln!("Генерація IR для {} функцій...", self.ast.len());
		}

		for stmt in self.ast.clone() {
			if let Statements::Func(id, args, ret_ty, stmts) = stmt {
				self.gen_func(id.clone(), args.clone(), ret_ty.clone(), stmts.clone());
			}
		}

		let mut module = self.module.take().expect("Модуль не наявний");

		if self.verbose {
			eprintln!("Оголошення {} функцій користувача", self.functions.len());
		}

		for (func_name, func) in &self.functions {
			let func_id = *self.functions_to_id.get(func_name).unwrap();
			let mut ctx = Context::for_function(func.clone());
			ctx.func = func.clone();
			module.define_function(func_id, &mut ctx)
				.expect(&format!("Не вийшло визначити функцію `{}`", func_name));
		}

		let product = module.finish();
		let product_bytes = product.emit().unwrap();

		let mut file = File::create("output.o").unwrap();
		file.write_all(&product_bytes).unwrap();

		// TODO: dont depend on clang
		let link = Command::new("clang")
			.args(&["output.o", "libyi_std.a", "-o", "output"])
			.output()
			.expect("Не вийшло запустити лінкер");

		if !link.status.success() {
			eeprintln!("Лінкувати не вийшло:");
			eeprintln!("{}", String::from_utf8_lossy(&link.stderr));
			std::process::exit(-1);
		}

		let _ = std::fs::remove_file("output.o");
	}

	pub fn gen_func(&mut self, id: String, args: Vec<(String, keywords::TypeKw)>, ret_ty: keywords::TypeKw, stmts: Vec<Statements>) {
		let mut builder_ctx = std::mem::take(&mut self.builder_ctx);

		let mut func = self.functions.remove(&id).unwrap();

		if ret_ty != keywords::TypeKw::Void {
			func.signature.returns.push(AbiParam::new(ret_ty.cranelift()));
		}

		for (_, ty) in &args {
			let cr_ty = ty.cranelift();
			func.signature.params.push(AbiParam::new(cr_ty));
		}

		let mut func_builder = FunctionBuilder::new(&mut func, &mut builder_ctx);

		let entry_block = func_builder.create_block();
		func_builder.append_block_params_for_function_params(entry_block);
		func_builder.switch_to_block(entry_block);

		for (i, (arg_name, arg_ty)) in args.iter().enumerate() {
			// TODO: debug info
			let cr_ty = arg_ty.cranelift();
			let var = func_builder.declare_var(cr_ty);
			let val = func_builder.block_params(entry_block)[i];
			func_builder.def_var(var, val);
			self.cranelift_var_map.insert(arg_name.clone(), var.as_u32() as usize);
			self.cranelift_signedness_map.insert(arg_name.clone(), arg_ty.signedness());
		}

		let mut terminated = false;

		for stmt in stmts {
			if terminated {
				continue;
			}

			if let Statements::Let(var_id, ty, expr) = stmt {
				self.gen_let(var_id, ty, expr, &mut func_builder);
			} else if let Statements::Assign(var_id, expr) = stmt {
				self.gen_assign(var_id, *expr, &mut func_builder);
			} else if let Statements::Expr(expr) = stmt {
				self.gen_expr(Some(*expr), &mut func_builder);
			} else if let Statements::Return(expr) = stmt {
				match self.gen_expr(Some(*expr), &mut func_builder) {
					Some((val, _signedness)) => {
						func_builder.ins().return_(&[val]);
					},
					None => {
						func_builder.ins().return_(&[]);
					}
				}
				terminated = true;
			}
		}

		if !terminated {
			func_builder.ins().return_(&[]);
		}

		func_builder.seal_block(entry_block);

		if self.ir {
			println!("{}", func_builder.func.display());
			println!();
		}

		func_builder.finalize();

		self.functions.insert(id.clone(), func);

		self.builder_ctx = builder_ctx;
	}

	pub fn gen_let(&mut self, var_id: String, ty: Option<TypeKw>, expr: Option<Box<Expr>>, mut func_builder: &mut FunctionBuilder<'_>) {
		let expr = expr.map(|expr| *expr);
		let expr = self.gen_expr(expr.clone(), &mut func_builder);

		if let Some((val, signedness)) = expr {
			let cr_ty = ty.clone().map(|ty| ty.cranelift()).unwrap_or(func_builder.func.dfg.value_type(val));

			let new_var = func_builder.declare_var(cr_ty);

			func_builder.def_var(new_var, val);

			self.cranelift_var_map.insert(var_id.clone(), new_var.as_u32() as usize);

			let final_signedness = ty.map(|t| t.signedness()).unwrap_or(signedness);

			self.cranelift_signedness_map.insert(var_id, final_signedness);
		}
	}

	pub fn gen_assign(&mut self, var_id: String, expr: Expr, mut func_builder: &mut FunctionBuilder<'_>) {
		let var = self.cranelift_var_map.get(&var_id).copied();

		let (val, _signedness) = self.gen_expr(Some(expr.clone()), &mut func_builder).expect("Немає значення для призначення змінної");

		if let Some(var) = var {
			let variable = Variable::new(var);
			func_builder.def_var(variable, val);
		} else {
			eeprintln!("Змінна {} не задеклерована", var_id);
			std::process::exit(-1);
		}
	}


	pub fn gen_expr(&mut self, expr: Option<Expr>, mut builder: &mut FunctionBuilder) -> Option<(Value, Signedness)> {
		match expr {
			Some(Expr::U64(val)) => {
				Some((builder.ins().iconst(types::I64, val as i64), Signedness::Unsigned))
			},
			Some(Expr::F64(val)) => {
				Some((builder.ins().f64const(val), Signedness::Signed))
			},
			Some(Expr::I64(val)) => {
				Some((builder.ins().iconst(types::I64, val), Signedness::Signed))
			},
			Some(Expr::Str(val)) => {
				// strings are complicated
				let module = self.module.as_mut().unwrap();

				let mut bytes = val.clone().into_bytes();
				bytes.push(0x00);

				let name = format!("str{}", self.string_literals.len());
				let data_id = module
					.declare_data(&name, Linkage::Local, false, false)
					.unwrap();

				let mut data_desc = DataDescription::new();
				data_desc.define(bytes.into_boxed_slice());

				let _ = module.define_data(data_id, &data_desc).inspect_err(|err| {
					eeprintln!("Була помилка при визначанні данних для строки: {}", err);
				});

				self.string_literals.insert(val.clone(), data_id.as_u32() as usize);
				let data_ref = module.declare_data_in_func(data_id, &mut builder.func);
				let ptr_val = builder.ins().symbol_value(types::I64, data_ref);

				Some((ptr_val, Signedness::Signed))
			},
			Some(Expr::Bool(val)) => {
				if val {
					Some((builder.ins().iconst(types::I8, 1), Signedness::Signed))
				} else {
					Some((builder.ins().iconst(types::I8, 0), Signedness::Signed))
				}
			},
			Some(Expr::Id(id)) => {
				let index = self.cranelift_var_map.get(&id);
				if let Some(index) = index {
					Some((builder.use_var(Variable::new(*index)), self.cranelift_signedness_map.get(&id).unwrap().clone()))
				} else {
					eeprintln!("{} не визначено", id);
					std::process::exit(-1);
				}
			},
			Some(Expr::Call(id, args)) => {
				let actual_name = Self::translate_std_func_name(&id);
				let module = self.module.as_mut();

				let func_id = self.functions_to_id.get(actual_name);

				let mut i = 0;

				if let Some(func_id) = func_id {
					let func_ref = module.unwrap().declare_func_in_func(func_id.clone(), &mut builder.func);

					let val_args = args.iter().map(|arg| {
						i += 1;

						self.gen_expr(Some(arg.clone()), &mut builder)
							.expect(&format!("Аргумент {} до функції '{}' є порожнім (мовинен бути значенням)", i+1, id))
					});
					let val_args_pure: Vec<_> = val_args.map(|arg| arg.0).collect();

					let call_inst = builder.ins().call(func_ref, &val_args_pure);
					let res = builder.inst_results(call_inst);

					if res.len() > 0 {
						return Some((res[0], Signedness::Signed))
					} else {
						return None
					}
				}

				eeprintln!("Функція `{}` не-задеклерована", actual_name);
				std::process::exit(-1);
			}
			Some(Expr::BinOp(left, op, right)) => {
				let (left_val, left_signedness) = self.gen_expr(Some(*left), builder).expect(&format!("Операція {:#?} не може бути виконана з порожнім лівим операндом.", op));
				let (right_val, right_signedness) = self.gen_expr(Some(*right), builder).expect(&format!("Операція {:#?} не може бути виконана з порожнім правим операндом.", op));

				let left_ty = builder.func.dfg.value_type(left_val);
				let right_ty = builder.func.dfg.value_type(right_val);

				if self.type_rank(left_ty).is_some() && self.type_rank(right_ty).is_some() {
					Some(self.op(left_val, left_signedness, op, right_val, right_signedness, builder))
				} else {
					eeprintln!("Не вийшло порівняти, лівий операнд не такого самого типу як правий операнд");
					std::process::exit(-1);
				}
			},

			Some(Expr::If(cond, then_block, else_block)) => {
				let (cond_val, _cond_signedness) = self.gen_expr(Some(*cond), builder)
					.expect("Якщо не може бути ніщо");

				let then_block_cr = builder.create_block();
				let else_block_cr = builder.create_block();
				let merge_block_cr = builder.create_block();

				// Branch on condition from current block
				builder.ins().brif(cond_val, then_block_cr, &[], else_block_cr, &[]);

				// === Generate THEN block ===
				builder.switch_to_block(then_block_cr);
				let (then_val, then_terminated) = self.gen_statements_block(then_block, builder)
					.expect("Не вийшло скомпілювати блок інструкцій в `якщо`");

				// Add jump to merge if not terminated
				if !then_terminated {
					if let Some((val, _)) = then_val {
						let ty = builder.func.dfg.value_type(val);
						builder.append_block_param(merge_block_cr, ty);
						builder.ins().jump(merge_block_cr, &[BlockArg::Value(val)]);
					} else {
						builder.ins().jump(merge_block_cr, &[]);
					}
				}
				// Now we can seal then block since we're done with it
				builder.seal_block(then_block_cr);

				// === Generate ELSE block ===
				builder.switch_to_block(else_block_cr);
				let (else_val, else_terminated) = self.gen_statements_block(else_block, builder)
					.expect("Не вийшло скомпілювати блок інструкцій в `інакше`");

				// Add jump to merge if not terminated
				if !else_terminated {
					match (then_val, else_val) {
						(Some((_then_v, _)), Some((else_v, sign))) => {
							// Both produce values - merge already has param from then block
							builder.ins().jump(merge_block_cr, &[BlockArg::Value(else_v)]);

							// Seal blocks and switch to merge
							builder.seal_block(else_block_cr);
							builder.switch_to_block(merge_block_cr);
							builder.seal_block(merge_block_cr);

							return Some((builder.block_params(merge_block_cr)[0], sign));
						},
						(None, None) => {
							// Neither produces values
							builder.ins().jump(merge_block_cr, &[]);

							builder.seal_block(else_block_cr);
							builder.switch_to_block(merge_block_cr);
							builder.seal_block(merge_block_cr);

							return None;
						},
						(Some((then_v, sign)), None) if then_terminated => {
							// Only else is active, then terminated
							builder.ins().jump(merge_block_cr, &[]);

							builder.seal_block(else_block_cr);
							builder.switch_to_block(merge_block_cr);
							builder.seal_block(merge_block_cr);

							return None;
						},
						(None, Some((else_v, sign))) if !then_terminated => {
							// Type mismatch
							eeprintln!("Гілки умови `якщо` повинні повертати однакові типи");
							std::process::exit(-1);
						},
						_ => {
							eeprintln!("Гілки умови `якщо` повинні повертати однакові типи");
							std::process::exit(-1);
						}
					}
				} else if then_terminated {
					// Both terminated - no merge needed
					builder.seal_block(else_block_cr);
					builder.seal_block(merge_block_cr);
					return None;
				} else {
					// else terminated, then didn't - merge already has jump from then
					builder.seal_block(else_block_cr);
					builder.switch_to_block(merge_block_cr);
					builder.seal_block(merge_block_cr);

					if let Some((_, sign)) = then_val {
						return Some((builder.block_params(merge_block_cr)[0], sign));
					} else {
						return None;
					}
				}
			},

			// support both
			Some(Expr::Void) => None,
			None => None,

			Some(Expr::Arg(..)) => {
				eeprintln!("Щось сталось не так - АСД має інвалідний для нього вираз");
				std::process::exit(-1);
			}
		}
	}

	pub fn gen_statements_block(
		&mut self,
		stmts: Vec<Statements>,
		builder: &mut FunctionBuilder
	) -> Result<(Option<(Value, Signedness)>, bool), String> {
		let mut last_value = None;
		let mut terminated = false;

		for stmt in stmts {
			match stmt {
				Statements::Let(var_id, ty, expr) => {
					self.gen_let(var_id, ty, expr, builder);
				},
				Statements::Assign(var_id, expr) => {
					self.gen_assign(var_id, *expr, builder);
				},
				Statements::Expr(expr) => {
					last_value = self.gen_expr(Some(*expr), builder);
				},
				Statements::Return(expr) => {
					match self.gen_expr(Some(*expr), builder) {
						Some((val, _)) => {
							builder.ins().return_(&[val]);
						},
						None => {
							builder.ins().return_(&[]);
						}
					}
					terminated = true;
					break; // Stop processing statements
				},
				Statements::Func(_, _, _, _) => {
					eeprintln!("Вкладені функції ще не підтримуються");
					std::process::exit(-1);
				},
			}
		}

		Ok((last_value, terminated))
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

		let op_signedness = if left_signedness.is_signed() || right_signedness.is_signed() {
			Signedness::Signed
		} else {
			Signedness::Unsigned
		};

		let left_cast = self.cast(left, left_ty, left_signedness, target_ty, op_signedness, builder);
		let right_cast = self.cast(right, right_ty, right_signedness, target_ty, op_signedness, builder);

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
					if op_signedness.is_signed() {
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
			BinOp::NotEq => {
				if target_ty.is_int() {
					builder.ins().icmp(
						cranelift::codegen::ir::condcodes::IntCC::NotEqual,
						left_cast,
						right_cast,
					)
				} else {
					builder.ins().fcmp(
						cranelift::codegen::ir::condcodes::FloatCC::NotEqual,
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
					op_signedness
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
