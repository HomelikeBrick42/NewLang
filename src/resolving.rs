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
use std::collections::VecDeque;

#[derive(Debug)]
pub struct ResolvingError {
    pub location: SourceLocation,
    pub kind: ResolvingErrorKind,
}

#[derive(Debug)]
pub enum ResolvingErrorKind {
    CyclicDependency { location: SourceLocation },
    UnableToFindName { name: InternedStr },
    ExpectedModule { got: Name },
    ExpectedType { got: Name },
    ExpectedValue { got: Name },
    ExpectedValueOrType { got: Name },
    TypeAliasMustHaveType,
    ConstMustHaveValue,
    FunctionWithoutBody,
}

pub fn print_resolving_errors(resolved_program: &ResolvedProgram) {
    for error in &resolved_program.errors {
        eprint!("{}: ", error.location);
        match error.kind {
            ResolvingErrorKind::CyclicDependency { location } => {
                eprintln!("Cyclic dependency detected");
                eprintln!("NOTE: the cycle was started at {location}");
            }

            ResolvingErrorKind::UnableToFindName { name } => {
                eprintln!("Unable to find name '{name}'");
            }

            ResolvingErrorKind::ExpectedModule { got } => {
                let typ = match got.kind {
                    NameKind::Module(_) => "module",
                    NameKind::Function(_) => "function",
                    NameKind::Type(_) => "type",
                    NameKind::Const(_) => "const",
                    NameKind::Variable(_) => "variable",
                };
                eprintln!("Expected a module but got a {typ}");
                eprintln!("NOTE: the {typ} was declared at {}", got.location);
            }

            ResolvingErrorKind::ExpectedType { got } => {
                let typ = match got.kind {
                    NameKind::Module(_) => "module",
                    NameKind::Function(_) => "function",
                    NameKind::Type(_) => "type",
                    NameKind::Const(_) => "const",
                    NameKind::Variable(_) => "variable",
                };
                eprintln!("Expected a type but got a {typ}");
                eprintln!("NOTE: the {typ} was declared at {}", got.location);
            }

            ResolvingErrorKind::ExpectedValue { got } => {
                let typ = match got.kind {
                    NameKind::Module(_) => "module",
                    NameKind::Function(_) => "function",
                    NameKind::Type(_) => "type",
                    NameKind::Const(_) => "const",
                    NameKind::Variable(_) => "variable",
                };
                eprintln!("Expected a value but got a {typ}");
                eprintln!("NOTE: the {typ} was declared at {}", got.location);
            }

            ResolvingErrorKind::ExpectedValueOrType { got } => {
                let typ = match got.kind {
                    NameKind::Module(_) => "module",
                    NameKind::Function(_) => "function",
                    NameKind::Type(_) => "type",
                    NameKind::Const(_) => "const",
                    NameKind::Variable(_) => "variable",
                };
                eprintln!("Expected a value or a type but got a {typ}");
                eprintln!("NOTE: the {typ} was declared at {}", got.location);
            }

            ResolvingErrorKind::TypeAliasMustHaveType => {
                eprintln!("Type aliases must have an assigned type");
            }

            ResolvingErrorKind::ConstMustHaveValue => {
                eprintln!("Consts must have an assigned value");
            }

            ResolvingErrorKind::FunctionWithoutBody => {
                eprintln!("Functions must have a body");
            }
        }
    }
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
    Const(ti::ConstId),
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
    Const(ti::ConstId),
    Variable(ti::VariableId),
}

#[derive(Debug)]
pub struct Builtins {
    pub runtime_type: Option<ti::TypeId>,
    pub i64_type: Option<ti::TypeId>,
    pub unit_type: Option<ti::TypeId>,
    pub bool_type: Option<ti::TypeId>,
}

#[derive(Debug)]
pub struct ResolvedProgram<'ast> {
    pub types: IdVec<ti::TypeId, ti::Type>,

    pub function_signatures: IdVec<ti::FunctionId, ti::FunctionSignature>,
    pub function_bodies: IdMap<ti::FunctionId, ti::FunctionBody>,

    pub consts: IdVec<ti::ConstId, ti::Const>,
    pub const_values: IdMap<ti::ConstId, ti::ConstValue>,

    pub global_module: Option<ModuleId>,
    pub modules: IdVec<ModuleId, Module>,
    pub module_items: IdVec<ModuleItemId, ModuleItem<'ast>>,

    pub builtins: Builtins,

    #[debug(ignore)]
    pub errors: Vec<ResolvingError>,
}

struct Scope {
    parent_module: ModuleId,
    names: FxHashMap<InternedStr, Name>,
}

struct FunctionBodyToCheck<'ast> {
    function: ti::FunctionId,
    variables: IdVec<ti::VariableId, ti::Variable>,
    parameter_variables: Box<[Option<ti::VariableId>]>,
    scope: Scope,
    expression: &'ast ast::Expression,
}

struct ConstValueToCheck<'ast> {
    const_: ti::ConstId,
    scope: Scope,
    value: &'ast ast::Expression,
}

pub fn resolve_program<'ast>(
    location: SourceLocation,
    items: &'ast [ast::Item],
) -> ResolvedProgram<'ast> {
    let mut types = IdVec::new();
    let mut function_signatures = IdVec::new();
    let mut function_bodies_to_check = VecDeque::new();
    let mut function_bodies = IdMap::new();
    let mut consts = IdVec::new();
    let mut const_values_to_check = VecDeque::new();
    let mut const_values = IdMap::new();
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
        &mut function_bodies,
        &mut function_bodies_to_check,
        &mut consts,
        &mut const_values_to_check,
        &mut const_values,
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
                &mut function_bodies,
                &mut function_bodies_to_check,
                &mut consts,
                &mut const_values_to_check,
                &mut const_values,
                &mut modules,
                &mut module_items,
                &mut builtins,
            ) {
                Ok(()) => {}
                Err(error) => errors.push(error),
            }

            module_item_index += 1;
        }

        while let Some(FunctionBodyToCheck {
            function,
            mut variables,
            parameter_variables,
            scope,
            expression,
        }) = function_bodies_to_check.pop_front()
        {
            was_unresolved_item = true;

            let expression = match resolve_expression(
                expression,
                &scope,
                &mut types,
                &mut function_signatures,
                &mut function_bodies,
                &mut function_bodies_to_check,
                &mut consts,
                &mut const_values_to_check,
                &mut const_values,
                &mut modules,
                &mut module_items,
                &mut builtins,
                &mut variables,
            ) {
                Ok(expression) => Box::new(expression),
                Err(error) => {
                    errors.push(error);
                    continue;
                }
            };

            assert!(function_bodies.get(function).is_none());
            function_bodies.insert(
                function,
                ti::FunctionBody::Expression {
                    variables,
                    parameter_variables,
                    expression,
                },
            );
        }

        while let Some(ConstValueToCheck {
            const_,
            scope,
            value,
        }) = const_values_to_check.pop_front()
        {
            was_unresolved_item = true;

            let mut variables = IdVec::new();
            let value = match resolve_expression(
                value,
                &scope,
                &mut types,
                &mut function_signatures,
                &mut function_bodies,
                &mut function_bodies_to_check,
                &mut consts,
                &mut const_values_to_check,
                &mut const_values,
                &mut modules,
                &mut module_items,
                &mut builtins,
                &mut variables,
            ) {
                Ok(expression) => expression,
                Err(error) => {
                    errors.push(error);
                    continue;
                }
            };

            assert!(const_values.get(const_).is_none());
            const_values.insert(const_, ti::ConstValue::Value { variables, value });
        }

        if !was_unresolved_item {
            break;
        }
    }

    ResolvedProgram {
        types,

        function_signatures,
        function_bodies,

        consts,
        const_values,

        global_module,
        modules,
        module_items,

        builtins,

        errors,
    }
}

fn create_unresolved_module_items<'ast>(
    location: SourceLocation,
    items: impl IntoIterator<Item = &'ast ast::Item>,
    parent_module: Option<ModuleId>,
    name: Option<InternedStr>,
    transparent: bool,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, ti::FunctionBody>,
    function_bodies_to_check: &mut VecDeque<FunctionBodyToCheck<'ast>>,
    consts: &mut IdVec<ti::ConstId, ti::Const>,
    const_values_to_check: &mut VecDeque<ConstValueToCheck<'ast>>,
    const_values: &mut IdMap<ti::ConstId, ti::ConstValue>,
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
        | ast::ItemKind::Type { name, .. }
        | ast::ItemKind::Const { name, .. }) = item.kind;

        let module_item = module_items.push(ModuleItem {
            location: item.location,
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
                function_bodies,
                function_bodies_to_check,
                consts,
                const_values_to_check,
                const_values,
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
    function_bodies: &mut IdMap<ti::FunctionId, ti::FunctionBody>,
    function_bodies_to_check: &mut VecDeque<FunctionBodyToCheck<'ast>>,
    consts: &mut IdVec<ti::ConstId, ti::Const>,
    const_values_to_check: &mut VecDeque<ConstValueToCheck<'ast>>,
    const_values: &mut IdMap<ti::ConstId, ti::ConstValue>,
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

        let mut scope = Scope {
            parent_module: module,
            names: FxHashMap::default(),
        };

        let &ast::Item {
            location: _,
            builtin,
            ref kind,
        } = item;
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
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
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
                                        function_bodies,
                                        function_bodies_to_check,
                                        consts,
                                        const_values_to_check,
                                        const_values,
                                        modules,
                                        module_items,
                                        builtins,
                                    )?;

                                    let variable = variables.push(ti::Variable {
                                        location,
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
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                )?;

                let id = function_signatures.push_with(|id| ti::FunctionSignature {
                    location,
                    name: Some(name),
                    parameters,
                    return_type,
                    typ: types.push(ti::Type {
                        location,
                        name: Some(name),
                        kind: ti::TypeKind::FunctionItem(id),
                    }),
                });

                if builtin {
                    assert!(body.is_none());
                    function_bodies.insert(
                        id,
                        ti::FunctionBody::Builtin(match name.as_str() {
                            "print_i64" => ti::BuiltinFunctionBody::PrintI64,
                            _ => panic!(),
                        }),
                    );
                } else if let Some(body) = body {
                    function_bodies_to_check.push_back(FunctionBodyToCheck {
                        function: id,
                        variables,
                        parameter_variables,
                        scope,
                        expression: body,
                    });
                } else {
                    return Err(ResolvingError {
                        location,
                        kind: ResolvingErrorKind::FunctionWithoutBody,
                    });
                }

                ModuleItemKind::Function(id)
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
                                    function_bodies,
                                    function_bodies_to_check,
                                    consts,
                                    const_values_to_check,
                                    const_values,
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
                                    function_bodies,
                                    function_bodies_to_check,
                                    consts,
                                    const_values_to_check,
                                    const_values,
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
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
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

            ast::ItemKind::Const {
                name,
                ref typ,
                ref value,
            } => {
                let typ = resolve_type(
                    typ,
                    &scope,
                    types,
                    function_signatures,
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                )?;

                let const_ = consts.push(ti::Const {
                    location,
                    name: Some(name),
                    typ,
                });

                if builtin {
                    assert!(value.is_none());
                    #[expect(clippy::match_single_binding)]
                    match name.as_str() {
                        _ => panic!(),
                    }
                } else if let Some(value) = value {
                    const_values_to_check.push_back(ConstValueToCheck {
                        const_,
                        scope,
                        value,
                    });
                } else {
                    return Err(ResolvingError {
                        location,
                        kind: ResolvingErrorKind::ConstMustHaveValue,
                    });
                }

                ModuleItemKind::Const(const_)
            }
        };
    }
    Ok(())
}

fn resolve_type<'ast>(
    &ast::Type { location, ref kind }: &'ast ast::Type,
    scope: &Scope,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, ti::FunctionBody>,
    function_bodies_to_check: &mut VecDeque<FunctionBodyToCheck<'ast>>,
    consts: &mut IdVec<ti::ConstId, ti::Const>,
    const_values_to_check: &mut VecDeque<ConstValueToCheck<'ast>>,
    const_values: &mut IdMap<ti::ConstId, ti::ConstValue>,
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
                function_bodies,
                function_bodies_to_check,
                consts,
                const_values_to_check,
                const_values,
                modules,
                module_items,
                builtins,
            )?;
            match name.kind {
                NameKind::Type(id) => id,

                NameKind::Module(_)
                | NameKind::Function(_)
                | NameKind::Const(_)
                | NameKind::Variable(_) => {
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
    &ast::Path { location, ref kind }: &'ast ast::Path,
    scope: &Scope,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, ti::FunctionBody>,
    function_bodies_to_check: &mut VecDeque<FunctionBodyToCheck<'ast>>,
    consts: &mut IdVec<ti::ConstId, ti::Const>,
    const_values_to_check: &mut VecDeque<ConstValueToCheck<'ast>>,
    const_values: &mut IdMap<ti::ConstId, ti::ConstValue>,
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
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
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
                function_bodies,
                function_bodies_to_check,
                consts,
                const_values_to_check,
                const_values,
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
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                ),

                NameKind::Function(_)
                | NameKind::Type(_)
                | NameKind::Const(_)
                | NameKind::Variable(_) => Err(ResolvingError {
                    location: operand.location,
                    kind: ResolvingErrorKind::ExpectedModule { got: operand_name },
                }),
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
    function_bodies: &mut IdMap<ti::FunctionId, ti::FunctionBody>,
    function_bodies_to_check: &mut VecDeque<FunctionBodyToCheck<'ast>>,
    consts: &mut IdVec<ti::ConstId, ti::Const>,
    const_values_to_check: &mut VecDeque<ConstValueToCheck<'ast>>,
    const_values: &mut IdMap<ti::ConstId, ti::ConstValue>,
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
            function_bodies,
            function_bodies_to_check,
            consts,
            const_values_to_check,
            const_values,
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
                        location: module_items[item].location,
                        kind: ResolvingErrorKind::CyclicDependency { location },
                    });
                }

                ModuleItemKind::Module(id) => NameKind::Module(id),
                ModuleItemKind::Function(id) => NameKind::Function(id),
                ModuleItemKind::Type(id) => NameKind::Type(id),
                ModuleItemKind::Const(id) => NameKind::Const(id),
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
            function_bodies,
            function_bodies_to_check,
            consts,
            const_values_to_check,
            const_values,
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

fn resolve_expression<'ast>(
    &ast::Expression { location, ref kind }: &'ast ast::Expression,
    scope: &Scope,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, ti::FunctionBody>,
    function_bodies_to_check: &mut VecDeque<FunctionBodyToCheck<'ast>>,
    consts: &mut IdVec<ti::ConstId, ti::Const>,
    const_values_to_check: &mut VecDeque<ConstValueToCheck<'ast>>,
    const_values: &mut IdMap<ti::ConstId, ti::ConstValue>,
    modules: &mut IdVec<ModuleId, Module>,
    module_items: &mut IdVec<ModuleItemId, ModuleItem<'ast>>,
    builtins: &mut Builtins,
    variables: &mut IdVec<ti::VariableId, ti::Variable>,
) -> Result<ti::Expression, ResolvingError> {
    Ok(match *kind {
        ast::ExpressionKind::Place(ref place) => {
            let place = Box::new(resolve_place(
                place,
                scope,
                types,
                function_signatures,
                function_bodies,
                function_bodies_to_check,
                consts,
                const_values_to_check,
                const_values,
                modules,
                module_items,
                builtins,
                variables,
            )?);
            ti::Expression {
                location,
                typ: place.typ,
                kind: ti::ExpressionKind::Place(place),
            }
        }

        ast::ExpressionKind::Integer(value) => ti::Expression {
            location,
            typ: types.push(ti::Type {
                location,
                name: None,
                kind: ti::TypeKind::Infer(ti::InferTypeKind::Number),
            }),
            kind: ti::ExpressionKind::Integer(value),
        },

        ast::ExpressionKind::Block {
            ref statements,
            ref last_expression,
        } => {
            let module = create_unresolved_module_items(
                location,
                statements
                    .iter()
                    .filter_map(|statement| match statement.kind {
                        ast::StatementKind::Item(ref item) => Some(&**item),
                        _ => None,
                    }),
                Some(scope.parent_module),
                None,
                true,
                types,
                function_signatures,
                function_bodies,
                function_bodies_to_check,
                consts,
                const_values_to_check,
                const_values,
                modules,
                module_items,
                builtins,
            )?;
            let mut scope = Scope {
                parent_module: module,
                names: scope.names.clone(),
            };
            let statements = statements
                .iter()
                .filter_map(|statement| {
                    resolve_statement(
                        statement,
                        &mut scope,
                        types,
                        function_signatures,
                        function_bodies,
                        function_bodies_to_check,
                        consts,
                        const_values_to_check,
                        const_values,
                        modules,
                        module_items,
                        builtins,
                        variables,
                    )
                    .transpose()
                })
                .collect::<Result<_, ResolvingError>>()?;
            let last_expression = Box::new(resolve_expression(
                last_expression,
                &scope,
                types,
                function_signatures,
                function_bodies,
                function_bodies_to_check,
                consts,
                const_values_to_check,
                const_values,
                modules,
                module_items,
                builtins,
                variables,
            )?);
            ti::Expression {
                location,
                typ: last_expression.typ,
                kind: ti::ExpressionKind::Block {
                    statements,
                    last_expression,
                },
            }
        }

        ast::ExpressionKind::Constructor {
            ref typ,
            ref members,
        } => ti::Expression {
            location,
            typ: resolve_type(
                typ,
                scope,
                types,
                function_signatures,
                function_bodies,
                function_bodies_to_check,
                consts,
                const_values_to_check,
                const_values,
                modules,
                module_items,
                builtins,
            )?,
            kind: ti::ExpressionKind::Constructor {
                members: members
                    .iter()
                    .map(
                        |&ast::ConstructorMember {
                             location,
                             name,
                             ref value,
                         }| {
                            Ok(ti::ConstructorMember {
                                location,
                                name,
                                value: resolve_expression(
                                    value,
                                    scope,
                                    types,
                                    function_signatures,
                                    function_bodies,
                                    function_bodies_to_check,
                                    consts,
                                    const_values_to_check,
                                    const_values,
                                    modules,
                                    module_items,
                                    builtins,
                                    variables,
                                )?,
                            })
                        },
                    )
                    .collect::<Result<_, ResolvingError>>()?,
            },
        },

        ast::ExpressionKind::Unary {
            operator,
            ref operand,
        } => ti::Expression {
            location,
            typ: types.push(ti::Type {
                location,
                name: None,
                kind: ti::TypeKind::Infer(ti::InferTypeKind::Anything),
            }),
            kind: ti::ExpressionKind::Unary {
                operator,
                operand: Box::new(resolve_expression(
                    operand,
                    scope,
                    types,
                    function_signatures,
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                    variables,
                )?),
            },
        },

        ast::ExpressionKind::Binary {
            ref left,
            operator,
            ref right,
        } => ti::Expression {
            location,
            typ: types.push(ti::Type {
                location,
                name: None,
                kind: ti::TypeKind::Infer(ti::InferTypeKind::Anything),
            }),
            kind: ti::ExpressionKind::Binary {
                left: Box::new(resolve_expression(
                    left,
                    scope,
                    types,
                    function_signatures,
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                    variables,
                )?),
                operator,
                right: Box::new(resolve_expression(
                    right,
                    scope,
                    types,
                    function_signatures,
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                    variables,
                )?),
            },
        },

        ast::ExpressionKind::Call {
            ref operand,
            ref arguments,
        } => ti::Expression {
            location,
            typ: types.push(ti::Type {
                location,
                name: None,
                kind: ti::TypeKind::Infer(ti::InferTypeKind::Anything),
            }),
            kind: ti::ExpressionKind::Call {
                operand: Box::new(resolve_expression(
                    operand,
                    scope,
                    types,
                    function_signatures,
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                    variables,
                )?),
                arguments: arguments
                    .iter()
                    .map(|argument| {
                        resolve_argument(
                            argument,
                            scope,
                            types,
                            function_signatures,
                            function_bodies,
                            function_bodies_to_check,
                            consts,
                            const_values_to_check,
                            const_values,
                            modules,
                            module_items,
                            builtins,
                            variables,
                        )
                    })
                    .collect::<Result<_, ResolvingError>>()?,
            },
        },
    })
}

fn resolve_argument<'ast>(
    &ast::Argument { location, ref kind }: &'ast ast::Argument,
    scope: &Scope,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, ti::FunctionBody>,
    function_bodies_to_check: &mut VecDeque<FunctionBodyToCheck<'ast>>,
    consts: &mut IdVec<ti::ConstId, ti::Const>,
    const_values_to_check: &mut VecDeque<ConstValueToCheck<'ast>>,
    const_values: &mut IdMap<ti::ConstId, ti::ConstValue>,
    modules: &mut IdVec<ModuleId, Module>,
    module_items: &mut IdVec<ModuleItemId, ModuleItem<'ast>>,
    builtins: &mut Builtins,
    variables: &mut IdVec<ti::VariableId, ti::Variable>,
) -> Result<ti::Argument, ResolvingError> {
    Ok(ti::Argument {
        location,
        kind: match *kind {
            ast::ArgumentKind::ValueOrType(ref expression) => {
                ti::ArgumentKind::Value(Box::new(resolve_expression(
                    expression,
                    scope,
                    types,
                    function_signatures,
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                    variables,
                )?))
            }

            ast::ArgumentKind::Lifetime { name: _ } => todo!(),
        },
    })
}

fn resolve_statement<'ast>(
    &ast::Statement { location, ref kind }: &'ast ast::Statement,
    scope: &mut Scope,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, ti::FunctionBody>,
    function_bodies_to_check: &mut VecDeque<FunctionBodyToCheck<'ast>>,
    consts: &mut IdVec<ti::ConstId, ti::Const>,
    const_values_to_check: &mut VecDeque<ConstValueToCheck<'ast>>,
    const_values: &mut IdMap<ti::ConstId, ti::ConstValue>,
    modules: &mut IdVec<ModuleId, Module>,
    module_items: &mut IdVec<ModuleItemId, ModuleItem<'ast>>,
    builtins: &mut Builtins,
    variables: &mut IdVec<ti::VariableId, ti::Variable>,
) -> Result<Option<ti::Statement>, ResolvingError> {
    Ok(Some(ti::Statement {
        location,
        kind: match *kind {
            ast::StatementKind::Item(_) => return Ok(None),

            ast::StatementKind::Expression(ref expression) => {
                ti::StatementKind::Expression(Box::new(resolve_expression(
                    expression,
                    scope,
                    types,
                    function_signatures,
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                    variables,
                )?))
            }

            ast::StatementKind::Assignment {
                ref pattern,
                ref value,
            } => ti::StatementKind::Assignment {
                value: Box::new(resolve_expression(
                    value,
                    scope,
                    types,
                    function_signatures,
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                    variables,
                )?),
                pattern: Box::new(resolve_pattern(
                    pattern,
                    scope,
                    types,
                    function_signatures,
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                    variables,
                )?),
            },
        },
    }))
}

fn resolve_place<'ast>(
    &ast::Place { location, ref kind }: &'ast ast::Place,
    scope: &Scope,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, ti::FunctionBody>,
    function_bodies_to_check: &mut VecDeque<FunctionBodyToCheck<'ast>>,
    consts: &mut IdVec<ti::ConstId, ti::Const>,
    const_values_to_check: &mut VecDeque<ConstValueToCheck<'ast>>,
    const_values: &mut IdMap<ti::ConstId, ti::ConstValue>,
    modules: &mut IdVec<ModuleId, Module>,
    module_items: &mut IdVec<ModuleItemId, ModuleItem<'ast>>,
    builtins: &mut Builtins,
    variables: &mut IdVec<ti::VariableId, ti::Variable>,
) -> Result<ti::Place, ResolvingError> {
    Ok(match *kind {
        ast::PlaceKind::Path(ref path) => {
            let name = resolve_path(
                path,
                scope,
                types,
                function_signatures,
                function_bodies,
                function_bodies_to_check,
                consts,
                const_values_to_check,
                const_values,
                modules,
                module_items,
                builtins,
            )?;
            match name.kind {
                NameKind::Function(id) => ti::Place {
                    location,
                    typ: function_signatures[id].typ,
                    kind: ti::PlaceKind::Function(id),
                },

                NameKind::Variable(id) => ti::Place {
                    location,
                    typ: variables[id].typ,
                    kind: ti::PlaceKind::Variable(id),
                },

                NameKind::Const(id) => ti::Place {
                    location,
                    typ: consts[id].typ,
                    kind: ti::PlaceKind::Const(id),
                },

                NameKind::Module(_) | NameKind::Type(_) => {
                    return Err(ResolvingError {
                        location,
                        kind: ResolvingErrorKind::ExpectedValue { got: name },
                    });
                }
            }
        }

        ast::PlaceKind::Expression(ref expression) => {
            let expression = Box::new(resolve_expression(
                expression,
                scope,
                types,
                function_signatures,
                function_bodies,
                function_bodies_to_check,
                consts,
                const_values_to_check,
                const_values,
                modules,
                module_items,
                builtins,
                variables,
            )?);
            ti::Place {
                location,
                typ: expression.typ,
                kind: ti::PlaceKind::Expression(expression),
            }
        }

        ast::PlaceKind::MemberAccess { ref operand, name } => ti::Place {
            location,
            typ: types.push(ti::Type {
                location,
                name: None,
                kind: ti::TypeKind::Infer(ti::InferTypeKind::Anything),
            }),
            kind: ti::PlaceKind::MemberAccess {
                operand: Box::new(resolve_place(
                    operand,
                    scope,
                    types,
                    function_signatures,
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                    variables,
                )?),
                name,
            },
        },
    })
}

fn resolve_pattern<'ast>(
    &ast::Pattern { location, ref kind }: &'ast ast::Pattern,
    scope: &mut Scope,
    types: &mut IdVec<ti::TypeId, ti::Type>,
    function_signatures: &mut IdVec<ti::FunctionId, ti::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, ti::FunctionBody>,
    function_bodies_to_check: &mut VecDeque<FunctionBodyToCheck<'ast>>,
    consts: &mut IdVec<ti::ConstId, ti::Const>,
    const_values_to_check: &mut VecDeque<ConstValueToCheck<'ast>>,
    const_values: &mut IdMap<ti::ConstId, ti::ConstValue>,
    modules: &mut IdVec<ModuleId, Module>,
    module_items: &mut IdVec<ModuleItemId, ModuleItem<'ast>>,
    builtins: &mut Builtins,
    variables: &mut IdVec<ti::VariableId, ti::Variable>,
) -> Result<ti::Pattern, ResolvingError> {
    Ok(match *kind {
        ast::PatternKind::Place(ref place) => {
            let place = Box::new(resolve_place(
                place,
                scope,
                types,
                function_signatures,
                function_bodies,
                function_bodies_to_check,
                consts,
                const_values_to_check,
                const_values,
                modules,
                module_items,
                builtins,
                variables,
            )?);
            ti::Pattern {
                location,
                typ: place.typ,
                kind: ti::PatternKind::Place(place),
            }
        }

        ast::PatternKind::Integer(value) => ti::Pattern {
            location,
            typ: types.push(ti::Type {
                location,
                name: None,
                kind: ti::TypeKind::Infer(ti::InferTypeKind::Number),
            }),
            kind: ti::PatternKind::Integer(value),
        },

        ast::PatternKind::Deconstructor {
            ref typ,
            ref members,
        } => ti::Pattern {
            location,
            typ: resolve_type(
                typ,
                scope,
                types,
                function_signatures,
                function_bodies,
                function_bodies_to_check,
                consts,
                const_values_to_check,
                const_values,
                modules,
                module_items,
                builtins,
            )?,
            kind: ti::PatternKind::Deconstructor {
                members: members
                    .iter()
                    .map(
                        |&ast::DeconstructorMember {
                             location,
                             name,
                             ref pattern,
                         }| {
                            Ok(ti::DeconstructorMember {
                                location,
                                name,
                                pattern: resolve_pattern(
                                    pattern,
                                    scope,
                                    types,
                                    function_signatures,
                                    function_bodies,
                                    function_bodies_to_check,
                                    consts,
                                    const_values_to_check,
                                    const_values,
                                    modules,
                                    module_items,
                                    builtins,
                                    variables,
                                )?,
                            })
                        },
                    )
                    .collect::<Result<_, ResolvingError>>()?,
            },
        },

        ast::PatternKind::Let { name, ref typ } => {
            let typ = if let Some(typ) = typ {
                resolve_type(
                    typ,
                    scope,
                    types,
                    function_signatures,
                    function_bodies,
                    function_bodies_to_check,
                    consts,
                    const_values_to_check,
                    const_values,
                    modules,
                    module_items,
                    builtins,
                )?
            } else {
                types.push(ti::Type {
                    location,
                    name: Some(name),
                    kind: ti::TypeKind::Infer(ti::InferTypeKind::Anything),
                })
            };
            let variable = variables.push(ti::Variable {
                location,
                name: Some(name),
                typ,
            });
            scope.names.insert(
                name,
                Name {
                    location,
                    kind: NameKind::Variable(variable),
                },
            );
            ti::Pattern {
                location,
                typ,
                kind: ti::PatternKind::Let(variable),
            }
        }
    })
}
