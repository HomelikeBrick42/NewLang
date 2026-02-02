use crate::{
    ast,
    idvec::{IdMap, IdVec, new_id_type},
    interning::InternedStr,
    lexing::SourceLocation,
    type_inference_tree as ti,
};
use derive_more::{Debug, Display};
use rustc_hash::FxHashMap;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("{location}: {kind}")]
pub struct ResolvingError {
    pub location: SourceLocation,
    pub kind: ResolvingErrorKind,
}

#[derive(Debug, Display)]
pub enum ResolvingErrorKind {}

new_id_type!(pub struct ModuleId);

#[derive(Debug)]
pub struct Module<'ast> {
    pub location: SourceLocation,
    pub parent_module: Option<ModuleId>,
    pub name: Option<InternedStr>,
    pub items: FxHashMap<InternedStr, ModuleItem<'ast>>,
}

#[derive(Debug)]
pub struct ModuleItem<'ast> {
    pub location: SourceLocation,
    pub module: ModuleId,
    pub name: InternedStr,
    pub kind: ModuleItemKind<'ast>,
}

#[derive(Debug)]
pub enum ModuleItemKind<'ast> {
    #[debug("UnresolvedItem {{ location: {} }}", item.location)]
    UnresolvedItem {
        item: &'ast ast::Item,
    },
    Module(ModuleId),
    Function(ti::FunctionId),
}

#[derive(Debug)]
pub struct ResolvedProgram<'ast> {
    pub types: IdVec<ti::TypeId, ti::Type>,

    pub function_signatures: IdVec<ti::FunctionId, ti::FunctionSignature>,
    pub function_bodies: IdMap<ti::FunctionId, ti::FunctionBody>,

    pub global_module: ModuleId,
    pub modules: IdVec<ModuleId, Module<'ast>>,
}

pub fn resolve_program<'ast>(
    location: SourceLocation,
    items: &'ast [ast::Item],
) -> Result<ResolvedProgram<'ast>, ResolvingError> {
    let types = IdVec::new();
    let function_signatures = IdVec::new();
    let function_bodies = IdMap::new();
    let mut modules = IdVec::new();

    let global_module = create_unresolved_module_items(location, items, None, None, &mut modules);

    Ok(ResolvedProgram {
        types,

        function_signatures,
        function_bodies,

        global_module,
        modules,
    })
}

fn create_unresolved_module_items<'ast>(
    location: SourceLocation,
    items: impl IntoIterator<Item = &'ast ast::Item>,
    parent_module: Option<ModuleId>,
    name: Option<InternedStr>,
    modules: &mut IdVec<ModuleId, Module<'ast>>,
) -> ModuleId {
    modules.push_with(|module| Module {
        location,
        parent_module,
        name,
        items: items
            .into_iter()
            .map(|item| {
                let (ast::ItemKind::Module { name, .. }
                | ast::ItemKind::Fn { name, .. }
                | ast::ItemKind::Struct { name, .. }
                | ast::ItemKind::Enum { name, .. }
                | ast::ItemKind::Type { name, .. }) = item.kind;

                let module_item = ModuleItem {
                    location,
                    module,
                    name,
                    kind: ModuleItemKind::UnresolvedItem { item },
                };

                (name, module_item)
            })
            .collect(),
    })
}
