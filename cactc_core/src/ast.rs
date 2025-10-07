// ---  Top-Level AST Structures ---

/// The root of the AST, representing a complete CACT source file.
/// A program is a sequence of global items. [cite: 50]
#[derive(Debug, Clone, PartialEq)]
pub struct CompilationUnit {
    pub items: Vec<GlobalItem>,
}

/// A top-level item in a program: either a function definition or a global declaration.
#[derive(Debug, Clone, PartialEq)]
pub enum GlobalItem {
    Function(FuncDef),
    Declaration(Decl),
}

/// The four basic types in CACT. [cite: 14]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BType {
    Int,
    Bool,
    Float,
    Double,
}

// --- Declarations ---

/// A declaration, which can be for constants (`const`) or variables. [cite: 50]
#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Const(ConstDecl),
    Var(VarDecl),
}

/// A constant declaration, e.g., `const int c = 1;`. [cite: 50]
#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl {
    pub b_type: BType,
    pub defs: Vec<ConstDef>,
}

/// A single constant definition, containing its name, dimensions (if array), and initializer.
#[derive(Debug, Clone, PartialEq)]
pub struct ConstDef {
    pub name: String,
    /// Dimensions for an array. Empty if not an array. Must be integer constants. [cite: 50]
    pub dimensions: Vec<i32>,
    pub init: ConstInitVal,
}

/// A variable declaration, e.g., `int a = 1, b;`. [cite: 50]
#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub b_type: BType,
    pub defs: Vec<VarDef>,
}

/// A single variable definition, with an optional initializer.
#[derive(Debug, Clone, PartialEq)]
pub struct VarDef {
    pub name: String,
    /// Dimensions for an array. Empty if not an array. Must be integer constants. [cite: 50]
    pub dimensions: Vec<i32>,
    pub init: Option<ConstInitVal>,
}

/// An initializer for a constant or variable. [cite: 50]
#[derive(Debug, Clone, PartialEq)]
pub enum ConstInitVal {
    /// A single constant expression, e.g., `= 5`.
    Single(ConstExp),
    /// An aggregate initializer for arrays, e.g., `= {1, 2, 3}`.
    Aggregate(Vec<ConstInitVal>),
}

/// A constant expression, which in CACT can only be a literal value. [cite: 52]
pub type ConstExp = Literal;

// --- Functions ---

/// A function definition. [cite: 50]
#[derive(Debug, Clone, PartialEq)]
pub struct FuncDef {
    pub return_type: FuncType,
    pub name: String,
    pub params: Vec<FuncFParam>,
    pub body: Block,
}

/// A function's return type, which can be `void` or one of the basic types. [cite: 50]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FuncType {
    Void,
    Type(BType),
}

/// A single formal parameter in a function definition. [cite: 50]
#[derive(Debug, Clone, PartialEq)]
pub struct FuncFParam {
    pub b_type: BType,
    pub name: String,
    /// Represents array dimensions. An empty vector means it's a scalar.
    /// A non-empty vector means it's an array. `None` represents an unsized
    /// first dimension (e.g., `int a[]`). [cite: 104, 106]
    pub dimensions: Vec<Option<i32>>,
}

// --- Statements and Blocks ---

/// A block of code, enclosed in curly braces `{}`. [cite: 52]
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

/// An item within a block: either a local declaration or a statement. [cite: 52]
#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Declaration(Decl),
    Statement(Stmt),
}

/// Represents all possible kinds of statements. [cite: 52]
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Assignment, e.g., `a = 5;`.
    Assign { left: LVal, right: Exp },
    /// An expression used as a statement, e.g., `foo();`. `None` for empty statement `;`.
    Expression(Option<Exp>),
    /// A code block as a statement.
    Block(Block),
    /// An `if` statement with an optional `else` branch.
    If {
        cond: Exp,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    /// A `while` loop.
    While { cond: Exp, body: Box<Stmt> },
    /// A `break` statement.
    Break,
    /// A `continue` statement.
    Continue,
    /// A `return` statement with an optional return value.
    Return(Option<Exp>),
}

// --- Expressions and Operators ---

/// An expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    /// A literal value like `123`, `true`, or `4.5f`.
    Literal(Literal),
    /// A left-value, which can be a variable or an array element access.
    LValue(LVal),
    /// A unary operation, e.g., `-x` or `!flag`.
    Unary { op: UnaryOp, operand: Box<Exp> },
    /// A binary operation, e.g., `a + b`.
    Binary {
        left: Box<Exp>,
        op: BinaryOp,
        right: Box<Exp>,
    },
    /// A function call, e.g., `foo(a, 5)`.
    Call { name: String, args: Vec<Exp> },
}

/// Represents a value that can appear on the left side of an assignment. [cite: 160]
/// This is either a variable name or an array element access.
#[derive(Debug, Clone, PartialEq)]
pub struct LVal {
    pub name: String,
    /// List of index expressions for array access, e.g., `arr[i][j]`. Empty if it's a simple variable.
    pub indices: Vec<Exp>,
}

/// A literal constant value. [cite: 15, 20, 28]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f32),
    Double(f64),
    Bool(bool),
}

/// Unary operators. [cite: 38]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,  // +
    Minus, // -
    Not,   // !
}

/// Binary operators. [cite: 38]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod, // Arithmetic
    Eq, Ne, // Equality
    Lt, Gt, Le, Ge, // Relational
    And, Or, // Logical
}