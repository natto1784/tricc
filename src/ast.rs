/// A very naive AST definition using recursive enums
/// See the parser for implementation
use std::rc::Rc;

pub type Parent = Vec<Entity>;

/// Entities are functions, classes, and modules
#[derive(Debug)]
pub enum Entity {
    Fn(Fn),
    Class(Class),
    Module(Module),
}

#[derive(Debug)]
pub struct Module {
    pub name: Rc<str>,
    pub children: Vec<ModuleChildren>,
}

/// Modules contain functions, classes and statements
#[derive(Debug)]
pub enum ModuleChildren {
    Fn(Fn),
    Class(Class),
    Module(Module),
    Const(Let),
}

#[derive(Debug)]
pub struct Class {
    pub name: Rc<str>,
    pub children: Vec<ClassChildren>,
}

/// Classes contain functions and statements.
///
/// TODO: Maybe change statements to something else
#[derive(Debug)]
pub enum ClassChildren {
    Fn(Fn),
    Statement(Statement),
}

#[derive(Debug)]
pub struct Fn {
    pub name: Rc<str>,
    pub return_ty: Option<Ty>,
    pub params: Vec<(Rc<str>, Ty)>,
    pub children: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Const(Let),
    Let(Let),
    Expr(Expr),
}

#[derive(Debug)]
pub struct Let {
    pub name: Rc<str>,
    pub ty: Ty,
    pub expr: Option<Expr>,
}

type Op = crate::lexer::TokenSymbol;

#[derive(Debug)]
pub enum Expr {
    Int(i32),
    Float(f32),
    Char(char),
    Op(Op, Box<Expr>, Option<Box<Expr>>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Block(Vec<Statement>),
    Loop,
    Break,
    Continue,
}

/// Primitives
///
/// TODO: add arrays and pointers maybe
#[derive(Debug)]
pub enum Ty {
    Int,
    Float,
    Char,
}
