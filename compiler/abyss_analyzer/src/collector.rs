use abyss_parser::ast::Type;

use crate::{hir::FlatProgram, lir::LirType, symbols::Context};

pub struct Collector;

impl Collector {
    pub fn collect(program: &FlatProgram) -> Result<Context, String> {
        let mut ctx = Context::new();

        for struct_def in &program.structs {
            let mut fields = Vec::new();
            for (field_name, field_type) in &struct_def.fields {
                let lir_type = Self::convert_type(field_type)?;
                fields.push((field_name.clone(), lir_type));
            }
            ctx.register_struct(struct_def.name.clone(), fields);
        }

        for static_def in &program.statics {
            let lir_type = Self::convert_type(&static_def.ty)?;
            ctx.declare_global(static_def.name.clone(), lir_type);
        }

        for func_def in &program.functions {
            let mut params = Vec::new();
            for (_, param_type) in &func_def.params {
                params.push(Self::convert_type(param_type)?);
            }

            let ret_ty = Self::convert_type(&func_def.return_type)?;

            ctx.register_function(func_def.name.clone(), params, ret_ty);
        }

        Ok(ctx)
    }

    fn convert_type(ast_type: &Type) -> Result<LirType, String> {
        match ast_type {
            Type::U8 => Ok(LirType::U8),
            Type::U16 => Ok(LirType::U16),
            Type::U32 => Ok(LirType::U32),
            Type::U64 => Ok(LirType::U64),
            Type::Usize => Ok(LirType::Usize),
            Type::I8 => Ok(LirType::I8),
            Type::I16 => Ok(LirType::I16),
            Type::I32 => Ok(LirType::I32),
            Type::I64 => Ok(LirType::I64),
            Type::Isize => Ok(LirType::Isize),
            Type::F32 => Ok(LirType::F32),
            Type::F64 => Ok(LirType::F64),
            Type::Char => Ok(LirType::Char),
            Type::Bool => Ok(LirType::Bool),
            Type::Void => Ok(LirType::Void),

            Type::Pointer(inner) => {
                let inner_lir = Self::convert_type(inner)?;
                Ok(LirType::Pointer(Box::new(inner_lir)))
            }

            Type::Const(inner) => {
                let inner_lir = Self::convert_type(inner)?;
                Ok(LirType::Const(Box::new(inner_lir)))
            }

            Type::Struct(path, generics) => {
                if !generics.is_empty() {
                    return Err(format!(
                        "Generics in struct usage not supported in Collector yet: {:?}",
                        path
                    ));
                }

                let struct_name = path.join("_");
                Ok(LirType::Struct(struct_name))
            }

            _ => Err(format!(
                "Type conversion not implemented for {:?}",
                ast_type
            )),
        }
    }
}
