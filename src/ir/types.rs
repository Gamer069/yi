use std::{collections::HashMap, fs::File, io::Write, path::PathBuf, process::Command};

use cranelift::{codegen::{Context, ir::Function}, module::{FuncId, Linkage, Module as _}, object::{ObjectBuilder, ObjectModule}, prelude::{types, AbiParam, Configurable, FunctionBuilderContext, settings}};
use yi_std::Type as YiType;

use crate::{eeprintln, keywords, parser::Statements, sym_table::SymTable};

pub struct IRGenerator {
	pub ast: Vec<Statements>,
	pub gst: SymTable, // global symbol table
	pub cranelift_var_map: HashMap<String, usize>,
	pub yi_type_map: HashMap<String, YiType>,
	pub cranelift_signedness_map: HashMap<String, Signedness>,
	// hack to get around the borrow checker - module should not really be Option
	pub module: Option<ObjectModule>,
	pub builder_ctx: FunctionBuilderContext,
	pub functions: HashMap<String, Function>,
	pub functions_to_id: HashMap<String, FuncId>,
	pub functions_to_ret_type: HashMap<String, YiType>,
	// lit -> mem
	pub string_literals: HashMap<String, usize>,
	pub string_literal_count: usize,
	pub ir: bool,
	pub verbose: bool,
	pub output: PathBuf,
}

impl IRGenerator {
	pub fn new(ast: Vec<Statements>, gst: SymTable, ir: bool, verbose: bool, output: String) -> Self {
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

		Self { ast, gst, ir, verbose, cranelift_var_map, cranelift_signedness_map, module: Some(module), builder_ctx, functions: HashMap::new(), functions_to_id: HashMap::new(), string_literals: HashMap::new(), yi_type_map: HashMap::new(), functions_to_ret_type: HashMap::new(), string_literal_count: 0, output: output.into() }
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

				if id == "старт" {
					if *ret_ty == keywords::TypeKw::Void {
						sig.returns.push(AbiParam::new(types::I32));
					} else {
						sig.returns.push(AbiParam::new(ret_ty.cranelift()));
					}
				} else if *ret_ty != keywords::TypeKw::Void {
					sig.returns.push(AbiParam::new(ret_ty.cranelift()));
				}

				// Map "старт" to "main" for linking
				let link_name = if id == "старт" { "main" } else { &id };

				let func_id = self.module.as_mut().unwrap()
					.declare_function(link_name, Linkage::Export, &sig)
					.expect("Не вийшло задекларувати функцію");

				let mut func = Function::new();
				func.signature = sig;
				self.functions.insert(id.clone(), func);

				self.functions_to_id.insert(id.clone(), func_id); // keep original name for calls
				self.functions_to_ret_type.insert(id.clone(), ret_ty.std());
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

		let obj_path = format!("{}.o", self.output.display());
		let exec_path = format!("{}", self.output.display());

		let mut file = File::create(obj_path.clone()).unwrap();
		file.write_all(&product_bytes).unwrap();

		// TODO: dont depend on clang
		let link = Command::new("clang")
			.args(&[&obj_path, "libyi_std.a", "-o", &exec_path])
			.output()
			.expect("Не вийшло запустити лінкер");

		if !link.status.success() {
			eeprintln!("Лінкувати не вийшло:");
			eeprintln!("{}", String::from_utf8_lossy(&link.stderr));
			std::process::exit(-1);
		}

		let _ = std::fs::remove_file(obj_path);
	}

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
