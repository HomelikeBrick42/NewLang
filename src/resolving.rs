#![expect(clippy::too_many_arguments)]

use crate::{
    ast,
    idvec::{Id, IdMap, IdVec, new_id_type},
    interning::InternedStr,
    lexing::SourceLocation,
    type_inference_tree as ti,
};
use derive_more::Debug;
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub struct ResolvingError {
    pub location: SourceLocation,
    pub kind: ResolvingErrorKind,
}

#[derive(Debug)]
pub enum ResolvingErrorKind {
    CyclicDependency,
    UnableToFindName { name: InternedStr },
    ExpectedModule { got: Name },
    ExpectedType { got: Name },
    TypeAliasMustHaveType,
}

new_id_type!(pub struct ModuleId);
new_id_type!(pub struct ModuleItemId);

#[derive(Debug)]
pub struct Module {
    pub location: SourceLocation,
    pub parent_module: Option<ModuleId>,
    pub transparent: bool,
    pub name: Option<InternedStr>,
    pub items: FxHashMap<InternedStr, ModuleItemId>,
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
    Resolving,
    Module(ModuleId),
    Function(ti::FunctionId),
    Type(ti::TypeId),
}

#[derive(Debug, Clone, Copy)]
pub struct Name {
    pub location: SourceLocation,
    pub kind: NameKind,
}

#[derive(Debug, Clone, Copy)]
pub enum NameKind {
    Module(ModuleId),
    Function(ti::FunctionId),
    Type(ti::TypeId),
    Variable(ti::VariableId),
}

struct Scope {
    parent_module: ModuleId,
    names: FxHashMap<InternedStr, Name>,
}

struct Builtins {
    runtime_type: Option<ti::TypeId>,
    i64_type: Option<ti::TypeId>,
    unit_type: Option<ti::TypeId>,
    bool_type: Option<ti::TypeId>,
}

#[derive(Debug)]
pub struct ResolvedProgram<'ast> {
    pub types: IdVec<ti::TypeId, ti::Type>,

    pub function_signatures: IdVec<ti::FunctionId, ti::FunctionSignature>,
    pub function_bodies: IdMap<ti::FunctionId, ti::FunctionBody>,

    pub global_module: Option<ModuleId>,
    pub modules: IdVec<ModuleId, Module>,
    pub module_items: IdVec<ModuleItemId, ModuleItem<'ast>>,

    pub errors: Vec<ResolvingError>,
}

pub fn resolve_program<'ast>(
    location: SourceLocation,
    items: &'ast [ast::Item],
) -> Result<ResolvedProgram<'ast>, ResolvingError> {
    let mut types = IdVec::new();
    let mut function_signatures = IdVec::new();
    let function_bodies = IdMap::new();
    let mut modules = IdVec::new();
    let mut module_items = IdVec::new();
    let mut builtins = Builtins {
        runtime_type: None,
        i64_type: None,
        unit_type: None,
        bool_type: None,
    };
    let mut errors = Vec::new();

    let global_module = match create_unresolved_module_items(
        location,
        items,
        None,
        None,
        false,
        &mut types,
        &mut function_signatures,
        &mut modules,
        &mut module_items,
        &mut builtins,
    ) {
        Ok(global_module) => Some(global_module),
        Err(error) => {
            errors.push(error);
            None
        }
    };

    let mut module_item_index = 0;
    loop {
        let mut was_unresolved_item = false;

        while module_item_index < module_items.len() {
            was_unresolved_item = true;

            let id = ModuleItemId::from_index(module_item_index.try_into().unwrap());
            match resolve_module_item(
                id,
                &mut types,
                &mut function_signatures,
                &mut modules,
                &mut module_items,
                &mut builtins,
            ) {
                Ok(()) => {}
                Err(error) => errors.push(error),
            }

            module_item_index += 1;
        }

        if !was_unresolved_item {
            break;
        }
    }

    Ok(ResolvedProgram {
        types,

        function_signatures,
        function_bodies,

        global_module,
        modules,
        module_items,

        errors,
    })
}

fn create_unresolved_module_items<'ast>(
    location: SourceLocation,
    items: impl IntoIterator<Item = &'ast ast::Item>,
    parent_module: Option<ModuleId>,
    name: Option<InternedStr>,
    transparent: bool,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    modules: &mut IdVec<ModuleId, Module>,
    module_items: &mut IdVec<ModuleItemId, ModuleItem<'ast>>,
    builtins: &mut Builtins,
) -> Result<ModuleId, ResolvingError> {
    let module = modules.push(Module {
        location,
        parent_module,
        transparent,
        name,
        items: FxHashMap::default(),
    });
    for item in items {
        let (ast::ItemKind::Module { name, .. }
        | ast::ItemKind::Fn { name, .. }
        | ast::ItemKind::Struct { name, .. }
        | ast::ItemKind::Enum { name, .. }
        | ast::ItemKind::Type { name, .. }) = item.kind;

        let module_item = module_items.push(ModuleItem {
            location,
            module,
            name,
            kind: ModuleItemKind::UnresolvedItem { item },
        });

        if matches!(item.kind, ast::ItemKind::Module { .. }) {
            // TODO: maybe there is a way around doing this here?
            // this is currently here so that #[builtin] types like
            // builtins.unit_type is set as long as the core module is declared first
            resolve_module_item(
                module_item,
                types,
                function_signatures,
                modules,
                module_items,
                builtins,
            )?;
        }

        modules[module].items.insert(name, module_item);
    }
    Ok(module)
}

fn resolve_module_item<'ast>(
    id: ModuleItemId,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    modules: &mut IdVec<ModuleId, Module>,
    module_items: &mut IdVec<ModuleItemId, ModuleItem<'ast>>,
    builtins: &mut Builtins,
) -> Result<(), ResolvingError> {
    if let ModuleItem {
        location,
        module,
        name,
        kind: ref mut kind @ ModuleItemKind::UnresolvedItem { item },
    } = module_items[id]
    {
        *kind = ModuleItemKind::Resolving;
        let &ast::Item {
            location: _,
            builtin,
            ref kind,
        } = item;
        let mut scope = Scope {
            parent_module: module,
            names: FxHashMap::default(),
        };
        module_items[id].kind = match *kind {
            ast::ItemKind::Module { name: _, ref items } => {
                ModuleItemKind::Module(create_unresolved_module_items(
                    location,
                    items,
                    Some(module),
                    Some(name),
                    false,
                    types,
                    function_signatures,
                    modules,
                    module_items,
                    builtins,
                )?)
            }

            ast::ItemKind::Fn {
                name: _,
                ref parameters,
                ref return_type,
                ref body,
            } => {
                let mut variables = IdVec::new();
                let mut parameter_variables = vec![None; parameters.len()].into_boxed_slice();

                let parameters = parameters
                    .iter()
                    .enumerate()
                    .map(|(index, &ast::Parameter { location, ref kind })| {
                        Ok(ti::FunctionParameter {
                            location,
                            kind: match *kind {
                                ast::ParameterKind::Value { name, ref typ } => {
                                    let typ = resolve_type(
                                        typ,
                                        &scope,
                                        types,
                                        function_signatures,
                                        modules,
                                        module_items,
                                        builtins,
                                    )?;

                                    let variable = variables.push(ti::Variable {
                                        name: Some(name),
                                        typ,
                                    });
                                    parameter_variables[index] = Some(variable);

                                    scope.names.insert(
                                        name,
                                        Name {
                                            location,
                                            kind: NameKind::Variable(variable),
                                        },
                                    );

                                    ti::FunctionParameterKind::Value { name, typ }
                                }

                                ast::ParameterKind::Type { name } => {
                                    let typ = types.push(ti::Type {
                                        location,
                                        name: Some(name),
                                        kind: ti::TypeKind::Generic,
                                    });

                                    scope.names.insert(
                                        name,
                                        Name {
                                            location,
                                            kind: NameKind::Type(typ),
                                        },
                                    );

                                    todo!()
                                }

                                ast::ParameterKind::Lifetime { name: _ } => todo!(),
                            },
                        })
                    })
                    .collect::<Result<_, ResolvingError>>()?;

                let return_type = resolve_type(
                    return_type,
                    &scope,
                    types,
                    function_signatures,
                    modules,
                    module_items,
                    builtins,
                )?;

                _ = body;

                ModuleItemKind::Function(function_signatures.push_with(|id| {
                    ti::FunctionSignature {
                        name: Some(name),
                        parameters,
                        return_type,
                        typ: types.push(ti::Type {
                            location,
                            name: None,
                            kind: ti::TypeKind::FunctionItem(id),
                        }),
                        variables,
                        parameter_variables,
                    }
                }))
            }

            ast::ItemKind::Struct {
                name: _,
                ref members,
            } => {
                let typ = types.push(ti::Type {
                    location,
                    name: Some(name),
                    kind: ti::TypeKind::Resolving,
                });

                let members = members
                    .iter()
                    .map(
                        |&ast::Member {
                             location,
                             name,
                             ref typ,
                         }| {
                            Ok(ti::TypeMember {
                                location,
                                name,
                                typ: resolve_type(
                                    typ,
                                    &scope,
                                    types,
                                    function_signatures,
                                    modules,
                                    module_items,
                                    builtins,
                                )?,
                            })
                        },
                    )
                    .collect::<Result<_, ResolvingError>>()?;
                types[typ].kind = ti::TypeKind::Struct { members };

                if builtin {
                    match name.as_str() {
                        "Unit" => {
                            assert!(builtins.unit_type.is_none());
                            builtins.unit_type = Some(typ);
                        }
                        _ => panic!(),
                    }
                }

                ModuleItemKind::Type(typ)
            }

            ast::ItemKind::Enum {
                name: _,
                ref members,
            } => {
                let typ = types.push(ti::Type {
                    location,
                    name: Some(name),
                    kind: ti::TypeKind::Resolving,
                });

                let members = members
                    .iter()
                    .map(
                        |&ast::Member {
                             location,
                             name,
                             ref typ,
                         }| {
                            Ok(ti::TypeMember {
                                location,
                                name,
                                typ: resolve_type(
                                    typ,
                                    &scope,
                                    types,
                                    function_signatures,
                                    modules,
                                    module_items,
                                    builtins,
                                )?,
                            })
                        },
                    )
                    .collect::<Result<_, ResolvingError>>()?;
                types[typ].kind = ti::TypeKind::Enum { members };

                if builtin {
                    match name.as_str() {
                        "Bool" => {
                            assert!(builtins.bool_type.is_none());
                            builtins.bool_type = Some(typ);
                        }
                        _ => panic!(),
                    }
                }

                ModuleItemKind::Type(typ)
            }

            ast::ItemKind::Type { name: _, ref typ } => ModuleItemKind::Type(if builtin {
                assert!(typ.is_none());
                match name.as_str() {
                    "Runtime" => {
                        let typ = types.push(ti::Type {
                            location,
                            name: Some(name),
                            kind: ti::TypeKind::Runtime,
                        });
                        assert!(builtins.runtime_type.is_none());
                        builtins.runtime_type = Some(typ);
                        typ
                    }

                    "I64" => {
                        let typ = types.push(ti::Type {
                            location,
                            name: Some(name),
                            kind: ti::TypeKind::Integer(ti::IntegerTypeKind::I64),
                        });
                        assert!(builtins.i64_type.is_none());
                        builtins.i64_type = Some(typ);
                        typ
                    }

                    _ => panic!(),
                }
            } else if let Some(typ) = typ {
                resolve_type(
                    typ,
                    &scope,
                    types,
                    function_signatures,
                    modules,
                    module_items,
                    builtins,
                )?
            } else {
                return Err(ResolvingError {
                    location,
                    kind: ResolvingErrorKind::TypeAliasMustHaveType,
                });
            }),
        };
    }
    Ok(())
}

fn resolve_type<'ast>(
    &ast::Type { location, ref kind }: &ast::Type,
    scope: &Scope,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    modules: &mut IdVec<ModuleId, Module>,
    module_items: &mut IdVec<ModuleItemId, ModuleItem<'ast>>,
    builtins: &mut Builtins,
) -> Result<ti::TypeId, ResolvingError> {
    Ok(match *kind {
        ast::TypeKind::Path(ref path) => {
            let name = resolve_path(
                path,
                scope,
                types,
                function_signatures,
                modules,
                module_items,
                builtins,
            )?;
            match name.kind {
                NameKind::Type(id) => id,

                NameKind::Module(_) | NameKind::Function(_) | NameKind::Variable(_) => {
                    return Err(ResolvingError {
                        location,
                        kind: ResolvingErrorKind::ExpectedType { got: name },
                    });
                }
            }
        }

        ast::TypeKind::Unit => builtins.unit_type.unwrap(),
    })
}

fn resolve_path<'ast>(
    &ast::Path { location, ref kind }: &ast::Path,
    scope: &Scope,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    modules: &mut IdVec<ModuleId, Module>,
    module_items: &mut IdVec<ModuleItemId, ModuleItem<'ast>>,
    builtins: &mut Builtins,
) -> Result<Name, ResolvingError> {
    match *kind {
        ast::PathKind::Name { name } => {
            if let Some(&name) = scope.names.get(&name) {
                Ok(name)
            } else {
                resolve_module_name(
                    location,
                    scope.parent_module,
                    name,
                    types,
                    function_signatures,
                    modules,
                    module_items,
                    builtins,
                )
            }
        }

        ast::PathKind::PathAccess { ref operand, name } => {
            let operand_name = resolve_path(
                operand,
                scope,
                types,
                function_signatures,
                modules,
                module_items,
                builtins,
            )?;
            match operand_name.kind {
                NameKind::Module(id) => resolve_module_name(
                    location,
                    id,
                    name,
                    types,
                    function_signatures,
                    modules,
                    module_items,
                    builtins,
                ),

                NameKind::Function(_) | NameKind::Type(_) | NameKind::Variable(_) => {
                    Err(ResolvingError {
                        location: operand.location,
                        kind: ResolvingErrorKind::ExpectedModule { got: operand_name },
                    })
                }
            }
        }
    }
}

fn resolve_module_name<'ast>(
    location: SourceLocation,
    id: ModuleId,
    name: InternedStr,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    modules: &mut IdVec<ModuleId, Module>,
    module_items: &mut IdVec<ModuleItemId, ModuleItem<'ast>>,
    builtins: &mut Builtins,
) -> Result<Name, ResolvingError> {
    let module = &modules[id];
    if let Some(&item) = module.items.get(&name) {
        resolve_module_item(
            item,
            types,
            function_signatures,
            modules,
            module_items,
            builtins,
        )?;
        Ok(Name {
            location: module_items[item].location,
            kind: match module_items[item].kind {
                ModuleItemKind::UnresolvedItem { item: _ } => unreachable!(),

                ModuleItemKind::Resolving => {
                    return Err(ResolvingError {
                        location,
                        kind: ResolvingErrorKind::CyclicDependency,
                    });
                }

                ModuleItemKind::Module(id) => NameKind::Module(id),
                ModuleItemKind::Function(id) => NameKind::Function(id),
                ModuleItemKind::Type(id) => NameKind::Type(id),
            },
        })
    } else if module.transparent
        && let Some(parent_module) = module.parent_module
    {
        resolve_module_name(
            location,
            parent_module,
            name,
            types,
            function_signatures,
            modules,
            module_items,
            builtins,
        )
    } else {
        Err(ResolvingError {
            location,
            kind: ResolvingErrorKind::UnableToFindName { name },
        })
    }
}
