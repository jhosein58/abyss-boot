use abyss_parser::ast::{BinaryOp, UnaryOp};
use cranelift::codegen::Context;
use cranelift::codegen::ir::InstBuilder;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use std::collections::HashMap;

use crate::target::{Target, Type as MyType};

pub struct CraneliftTarget {
    module: JITModule,
    ctx: Context,
    builder_ctx: FunctionBuilderContext,
    builder: Option<FunctionBuilder<'static>>,

    var_map: HashMap<String, Variable>,
    var_index: usize,
    functions_map: HashMap<String, FuncId>,
    current_func_name: String,
}

impl CraneliftTarget {
    pub fn new(extern_symbols: &[(&str, *const u8)]) -> Self {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names()).unwrap();

        for (name, ptr) in extern_symbols {
            builder.symbol(*name, *ptr);
        }

        let module = JITModule::new(builder);

        Self {
            module,
            ctx: Context::new(),
            builder_ctx: FunctionBuilderContext::new(),
            builder: None,
            var_map: HashMap::new(),
            var_index: 0,
            functions_map: HashMap::new(),
            current_func_name: String::new(),
        }
    }
    pub fn run_fn(&mut self, name: &str) -> Result<i32, String> {
        self.module
            .finalize_definitions()
            .map_err(|e| format!("{:?}", e))?;

        let code = self.module.get_name(name).ok_or("Function not found")?;

        if let cranelift_module::FuncOrDataId::Func(func_id) = code {
            let ptr = self.module.get_finalized_function(func_id);
            let code_fn = unsafe { std::mem::transmute::<_, extern "C" fn() -> i32>(ptr) };
            Ok(code_fn())
        } else {
            Err("Not a function".to_string())
        }
    }

    fn to_clif_type(&self, t: MyType) -> types::Type {
        match t {
            MyType::Int => types::I64,
            MyType::Float => types::F64,
            MyType::Bool => types::I8,
            MyType::Void => types::INVALID,
        }
    }
}

impl Target for CraneliftTarget {
    type Value = Value;
    type Block = Block;

    fn start_program(&mut self) {
        self.module.clear_context(&mut self.ctx);
    }

    fn declare_extern_function(
        &mut self,
        name: &str,
        params: &[(String, MyType)],
        return_type: MyType,
    ) {
        let mut signature = self.module.make_signature();

        for (_, ty) in params {
            signature.params.push(AbiParam::new(self.to_clif_type(*ty)));
        }

        if return_type != MyType::Void {
            signature
                .returns
                .push(AbiParam::new(self.to_clif_type(return_type)));
        }

        let func_id = self
            .module
            .declare_function(name, Linkage::Import, &signature)
            .expect("Failed to declare extern function");

        self.functions_map.insert(name.to_string(), func_id);
    }

    fn end_program(&mut self) {}

    fn predefine_function(&mut self, name: &str, params: &[(String, MyType)], return_type: MyType) {
        let mut signature = self.module.make_signature();

        for (_, ty) in params {
            signature.params.push(AbiParam::new(self.to_clif_type(*ty)));
        }

        if return_type != MyType::Void {
            signature
                .returns
                .push(AbiParam::new(self.to_clif_type(return_type)));
        }

        let func_id = self
            .module
            .declare_function(name, Linkage::Export, &signature)
            .expect("Failed to declare function signature");

        self.functions_map.insert(name.to_string(), func_id);
    }

    fn start_function(&mut self, name: &str) {
        self.current_func_name = name.to_string();
        self.module.clear_context(&mut self.ctx);

        let func_id = *self
            .functions_map
            .get(name)
            .expect("Function must be predefined");

        let signature = self
            .module
            .declarations()
            .get_function_decl(func_id)
            .signature
            .clone();
        self.ctx.func.signature = signature;

        self.var_map.clear();
        self.var_index = 0;
    }

    fn add_function_param(&mut self, name: &str) {
        let var = Variable::new(self.var_index);
        self.var_map.insert(name.to_string(), var);
        self.var_index += 1;
    }

    fn start_function_body(&mut self) {
        let builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);
        let builder_ptr = unsafe { std::mem::transmute::<_, FunctionBuilder<'static>>(builder) };
        self.builder = Some(builder_ptr);

        let builder = self.builder.as_mut().unwrap();

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let params = builder.block_params(entry_block).to_vec();
        for (i, val) in params.iter().enumerate() {
            if let Some((_, var)) = self.var_map.iter().find(|(_, v)| v.index() == i) {
                let ty = builder.func.dfg.value_type(*val);
                builder.declare_var(*var, ty);
                builder.def_var(*var, *val);
            }
        }
    }

    fn end_function(&mut self) {
        if let Some(builder) = self.builder.take() {
            builder.finalize();
        }

        let func_id = *self
            .functions_map
            .get(&self.current_func_name)
            .expect("Function not found in map");

        self.module
            .define_function(func_id, &mut self.ctx)
            .expect("Failed to define function");

        self.module.clear_context(&mut self.ctx);
    }

    fn seal_block(&mut self, block: Self::Block) {
        if let Some(builder) = self.builder.as_mut() {
            builder.seal_block(block);
        }
    }

    fn declare_variable(&mut self, name: &str, ty: MyType) {
        let clif_ty = self.to_clif_type(ty);
        let builder = self.builder.as_mut().unwrap();

        let var = Variable::new(self.var_index);
        self.var_map.insert(name.to_string(), var);
        self.var_index += 1;

        builder.declare_var(var, clif_ty);
    }

    fn define_variable(&mut self, name: &str, value: Self::Value) {
        let builder = self.builder.as_mut().unwrap();
        let var = *self
            .var_map
            .get(name)
            .expect(&format!("Variable {} not declared", name));
        builder.def_var(var, value);
    }

    fn assign_variable(&mut self, name: &str, value: Self::Value) {
        self.define_variable(name, value);
    }

    fn create_block(&mut self) -> Self::Block {
        self.builder.as_mut().unwrap().create_block()
    }

    fn switch_to_block(&mut self, block: Self::Block) {
        let builder = self.builder.as_mut().unwrap();
        builder.switch_to_block(block);
    }

    fn jump(&mut self, target_block: Self::Block) {
        let builder = self.builder.as_mut().unwrap();
        builder.ins().jump(target_block, &[]);
    }
    fn branch(&mut self, condition: Self::Value, then_block: Self::Block, else_block: Self::Block) {
        let builder = self.builder.as_mut().unwrap();
        let ty = builder.func.dfg.value_type(condition);

        let cond = if ty.is_float() {
            let zero = builder.ins().f64const(0.0);
            builder.ins().fcmp(FloatCC::NotEqual, condition, zero)
        } else {
            condition
        };

        builder.ins().brif(cond, then_block, &[], else_block, &[]);
    }

    fn return_value(&mut self, value: Self::Value) {
        let builder = self.builder.as_mut().unwrap();
        builder.ins().return_(&[value]);
    }

    fn return_void(&mut self) {
        let builder = self.builder.as_mut().unwrap();
        builder.ins().return_(&[]);
    }

    fn translate_lit_int(&mut self, value: i64) -> Self::Value {
        let builder = self.builder.as_mut().unwrap();
        builder.ins().iconst(types::I64, value)
    }

    fn translate_lit_float(&mut self, value: f64) -> Self::Value {
        let builder = self.builder.as_mut().unwrap();
        builder.ins().f64const(value)
    }

    fn translate_lit_bool(&mut self, value: bool) -> Self::Value {
        let builder = self.builder.as_mut().unwrap();
        builder.ins().iconst(types::I8, if value { 1 } else { 0 })
    }

    fn translate_ident(&mut self, name: &str) -> Self::Value {
        let builder = self.builder.as_mut().unwrap();
        let var = *self
            .var_map
            .get(name)
            .expect(&format!("Variable {} not found", name));
        builder.use_var(var)
    }
    fn translate_binary_op(
        &mut self,
        op: BinaryOp,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> Self::Value {
        let builder = self.builder.as_mut().unwrap();
        let lhs_ty = builder.func.dfg.value_type(lhs);

        match op {
            BinaryOp::Add => {
                if lhs_ty.is_float() {
                    builder.ins().fadd(lhs, rhs)
                } else {
                    builder.ins().iadd(lhs, rhs)
                }
            }
            BinaryOp::Sub => {
                if lhs_ty.is_float() {
                    builder.ins().fsub(lhs, rhs)
                } else {
                    builder.ins().isub(lhs, rhs)
                }
            }
            BinaryOp::Mul => {
                if lhs_ty.is_float() {
                    builder.ins().fmul(lhs, rhs)
                } else {
                    builder.ins().imul(lhs, rhs)
                }
            }
            BinaryOp::Div => {
                if lhs_ty.is_float() {
                    builder.ins().fdiv(lhs, rhs)
                } else {
                    builder.ins().sdiv(lhs, rhs)
                }
            }
            BinaryOp::Mod => {
                if lhs_ty.is_float() {
                    panic!("float mod not supported")
                } else {
                    builder.ins().srem(lhs, rhs)
                }
            }
            BinaryOp::Eq => {
                if lhs_ty.is_float() {
                    builder.ins().fcmp(FloatCC::Equal, lhs, rhs)
                } else {
                    builder.ins().icmp(IntCC::Equal, lhs, rhs)
                }
            }
            BinaryOp::Neq => {
                if lhs_ty.is_float() {
                    builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs)
                } else {
                    builder.ins().icmp(IntCC::NotEqual, lhs, rhs)
                }
            }
            BinaryOp::Lt => {
                if lhs_ty.is_float() {
                    builder.ins().fcmp(FloatCC::LessThan, lhs, rhs)
                } else {
                    builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs)
                }
            }
            BinaryOp::Gt => {
                if lhs_ty.is_float() {
                    builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs)
                } else {
                    builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs)
                }
            }
            BinaryOp::Lte => {
                if lhs_ty.is_float() {
                    builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs)
                } else {
                    builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs)
                }
            }
            BinaryOp::Gte => {
                if lhs_ty.is_float() {
                    builder.ins().fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs)
                } else {
                    builder
                        .ins()
                        .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs)
                }
            }
            BinaryOp::And => builder.ins().band(lhs, rhs),
            BinaryOp::Or => builder.ins().bor(lhs, rhs),
        }
    }

    fn translate_unary_op(&mut self, op: UnaryOp, value: Self::Value) -> Self::Value {
        let builder = self.builder.as_mut().unwrap();
        let ty = builder.func.dfg.value_type(value);

        match op {
            UnaryOp::Neg => {
                if ty.is_float() {
                    builder.ins().fneg(value)
                } else {
                    builder.ins().ineg(value)
                }
            }
            UnaryOp::Not => {
                let one = builder.ins().iconst(types::I8, 1);
                builder.ins().bxor(value, one)
            }
        }
    }

    fn call_function(&mut self, name: &str, args: Vec<Self::Value>) -> Self::Value {
        let func_id = if let Some(id) = self.functions_map.get(name) {
            *id
        } else {
            panic!("Function {} must be compiled before calling", name);
        };

        let builder = self.builder.as_mut().unwrap();

        let local_callee = self.module.declare_func_in_func(func_id, &mut builder.func);

        let call_inst = builder.ins().call(local_callee, &args);

        let results = builder.inst_results(call_inst);
        if results.len() > 0 {
            results[0]
        } else {
            builder.ins().iconst(types::I64, 0)
        }
    }
}
