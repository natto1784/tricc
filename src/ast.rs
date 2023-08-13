//! A very naive AST definition using recursive enums
//!
//! See the parser for implementation

use std::rc::Rc;

pub type Parent = Vec<Entity>;

/// Entities are functions, classes, and modules
#[derive(Debug, PartialEq)]
pub enum Entity {
    Fn(Fn),
    Class(Class),
    Module(Module),
    Static(Let)
}

/// A module just provides an additional scope
///
/// TODO: Add exporting and importing modules
#[derive(Debug, PartialEq)]
pub struct Module {
    /// Name of module
    pub name: Rc<str>,
    /// Everything inside the module
    pub children: Vec<ModuleChildren>,
}

/// Modules contain functions, classes and statements
#[derive(Debug, PartialEq)]
pub enum ModuleChildren {
    Fn(Fn),
    Class(Class),
    Module(Module),
    Static(Let),
}

/// Classes encapsulate functions and definitions.
#[derive(Debug, PartialEq)]
pub struct Class {
    /// Name of class
    pub name: Rc<str>,
    /// Everything inside the class
    pub children: Vec<ClassChildren>,
}

#[derive(Debug, PartialEq)]
pub enum ClassChildren {
    Fn(Fn),
    Let(Let),
    Static(Let),
}

/// A Function
#[derive(Debug, PartialEq)]
pub struct Fn {
    /// Name of the function
    pub name: Rc<str>,
    /// Optional return type
    pub return_ty: Option<Ty>,
    /// Parameters
    pub params: Vec<(Rc<str>, Ty)>,
    /// The function block
    pub children: Vec<Statement>,
}

/// Statements encapsulate expressions and definitions
#[derive(Debug, PartialEq)]
pub enum Statement {
    Static(Let),
    Let(Let),
    Expr(Expr),
}

/// A variable definition
#[derive(Debug, PartialEq)]
pub struct Let {
    /// Name of variabe
    pub name: Rc<str>,
    /// Type of variable
    pub ty: Ty,
    /// Value of variable
    pub expr: Option<Expr>,
}

/// Primitives
///
/// TODO: add arrays and pointers maybe
#[derive(Debug, PartialEq)]
pub enum Ty {
    Int,
    Float,
    Char,
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Vec<Statement>,
    pub or: Option<Box<ElseType>>,
}

#[derive(Debug, PartialEq)]
pub enum ElseType {
    If(If),
    Else(Vec<Statement>),
}

type Op = crate::lexer::TokenSymbol;

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f32),
    Char(char),
}

/// Lowest form of expression
///
/// TODO: refine
#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(Rc<str>),
    Op(Op, Box<Expr>, Option<Box<Expr>>),
    If(If),
    Block(Vec<Statement>),
    Loop(Vec<Statement>),
    Break,
    Continue,
    Return(Option<Box<Expr>>),
}
