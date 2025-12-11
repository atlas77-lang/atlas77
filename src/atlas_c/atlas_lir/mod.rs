use crate::atlas_c::{
    atlas_hir::{HirModule, expr::HirExpr, item::HirFunction, stmt::HirStatement},
    atlas_lir::{
        error::{CurrentFunctionDoesntExistError, LIRLoweringError, LIRResult},
        program::{LIRBlock, LIRFunction, LIRInstr, LIROperand, LIRProgram},
    },
};

pub mod error;
pub(crate) mod program;
pub struct HIRLoweringPass<'hir> {
    hir_module: &'hir HirModule<'hir>,
    /// The function currently being lowered
    current_function: Option<LIRFunction>,
    /// A counter to generate unique variable names
    /// (e.g., temp0, temp1, etc.)
    var_counter: usize,
}

// I just want to be able to lower a simple fib function for now
impl<'hir> HIRLoweringPass<'hir> {
    pub fn new(hir_module: &'hir HirModule<'hir>) -> Self {
        Self {
            hir_module,
            current_function: None,
            var_counter: 0,
        }
    }
    pub fn lower(&mut self) -> LIRResult<LIRProgram> {
        let mut functions = Vec::new();
        for (_, func) in &self.hir_module.body.functions {
            // Initialize current function
            let current_function = LIRFunction {
                name: func.name.to_string(),
                //Default first block
                blocks: vec![
                    LIRBlock {
                        label: "entry".to_string(),
                        instructions: Vec::new(),
                    }
                ],
            };
            // Set current function
            self.current_function = Some(current_function);
            // Lower the function
            self.lower_function(func)?;
            // Push the lowered function to the program
            functions.push(self.current_function.clone().unwrap());
            // Reset current function and var counter
            self.current_function = None;
            self.var_counter = 0;
        }
        Ok(LIRProgram { functions })
    }

    fn get_new_var_name(&mut self) -> String {
        let var_name = format!("t{}", self.var_counter);
        self.var_counter += 1;
        var_name
    }

    /// Creates a new block in the current function
    fn new_block(&mut self) -> LIRResult<()> {
        if let Some(current_function) = &mut self.current_function {
            let block = LIRBlock {
                label: format!("block_{}", current_function.blocks.len()),
                instructions: Vec::new(),
            };
            current_function.blocks.push(block);
            Ok(())
        } else {
            Err(Box::new(LIRLoweringError::CurrentFunctionDoesntExist(
                CurrentFunctionDoesntExistError,
            )))
        }
    }

    /// Pushes an instruction to the last block of the current function
    fn push_instr(&mut self, instr: LIRInstr) -> LIRResult<()> {
        if let Some(current_function) = &mut self.current_function {
            if let Some(current_block) = current_function.blocks.last_mut() {
                current_block.instructions.push(instr);
                Ok(())
            } else {
                Err(Box::new(LIRLoweringError::CurrentFunctionDoesntExist(
                    CurrentFunctionDoesntExistError,
                )))
            }
        } else {
            Err(Box::new(LIRLoweringError::CurrentFunctionDoesntExist(
                CurrentFunctionDoesntExistError,
            )))
        }
    }

    /// Pushes multiple instructions to the last block of the current function
    fn push_instrs(&mut self, instrs: Vec<LIRInstr>) -> LIRResult<()> {
        for instr in instrs {
            self.push_instr(instr)?;
        }
        Ok(())
    }

    pub fn lower_function(&mut self, func: &'hir HirFunction<'hir>) -> LIRResult<()> {
        for stmt in func.body.statements.iter() {
            self.lower_stmt(stmt)?;
        }
        Ok(())
    }

    pub fn lower_stmt(&mut self, stmt: &'hir HirStatement<'hir>) -> LIRResult<()> {
        match stmt {
            HirStatement::IfElse(if_else) => {

            }
            _ => unimplemented!("Only expression statements are supported for lowering to LIR"),
        }
        Ok(())
    }

    pub fn lower_expr(&mut self, expr: &'hir HirExpr<'hir>) -> LIRResult<()> {
        match expr {
            
            _ => unimplemented!("Only basic expressions are supported for lowering to LIR"),
        }
    }
}
