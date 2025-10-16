use std::marker::PhantomData;
use std::ops::{Index, IndexMut};
use bumpalo::Bump;
use bumpalo::collections::Vec as BumpVec;
use crate::span::Span;
use crate::interner::Symbol;

// ===================================================================
//  ID and Arena (The Core of the New Design)
// ===================================================================

/// A type-safe ID that references a node in the AstArena.
/// It's a lightweight wrapper around a `u32` index, making it cheap to copy.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct NodeId<T> {
    raw: u32,
    _phantom: PhantomData<T>,
}

// Manually implement Copy and Clone because derive would require T: Copy.
impl<T> Copy for NodeId<T> {}
impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self { *self }
}

impl<T> NodeId<T> {
    fn new(index: usize) -> Self {
        Self { raw: index as u32, _phantom: PhantomData }
    }
    pub fn index(self) -> usize {
        self.raw as usize
    }
}

/// The Arena stores all the AST nodes created during parsing.
/// The AST itself will only contain IDs pointing into these vectors.
#[derive(Debug)]
pub struct AstArena<'ast> {
    bump: &'ast Bump,
    // We create separate vectors for major node kinds.
    // This improves data locality and keeps the arena organized.
    pub global_items: BumpVec<'ast, Node<GlobalItem<'ast>>>,
    pub decls: BumpVec<'ast, Node<Decl<'ast>>>,
    pub stmts: BumpVec<'ast, Node<Stmt<'ast>>>,
    pub exprs: BumpVec<'ast, Node<Exp<'ast>>>,
    pub lvals: BumpVec<'ast, Node<LVal<'ast>>>,
    pub blocks: BumpVec<'ast, Node<Block<'ast>>>,
    pub const_init_vals: BumpVec<'ast, Node<ConstInitVal<'ast>>>,
    // Smaller, self-contained items can sometimes be stored directly,
    // but for consistency and flexibility, adding them to the arena is also fine.
    pub const_defs: BumpVec<'ast, Node<ConstDef<'ast>>>,
    pub var_defs: BumpVec<'ast, Node<VarDef<'ast>>>,
    pub func_f_params: BumpVec<'ast, Node<FuncFParam<'ast>>>,
}

/// 为 AstArena 实现 Index 和 IndexMut traits 的宏
macro_rules! impl_arena_index {
    ( $( ($NodeType:ident, $field:ident) ),* ) => {
        $(
            // --- 为 $NodeType 实现 Index ---
            impl<'ast> std::ops::Index<NodeId<Node<$NodeType<'ast>>>> for AstArena<'ast> {
                type Output = Node<$NodeType<'ast>>;

                fn index(&self, id: NodeId<Node<$NodeType<'ast>>>) -> &Self::Output {
                    // 假设 id.index() 返回 usize
                    &self.$field[id.index()]
                }
            }

            // --- 为 $NodeType 实现 IndexMut ---
            impl<'ast> std::ops::IndexMut<NodeId<Node<$NodeType<'ast>>>> for AstArena<'ast> {
                fn index_mut(&mut self, id: NodeId<Node<$NodeType<'ast>>>) -> &mut Self::Output {
                    &mut self.$field[id.index()]
                }
            }
        )*
    };
}

impl<'ast> AstArena<'ast> {
    /// Creates a new, empty AstArena within a given Bump allocator.
    pub fn new_in(bump: &'ast Bump) -> Self {
        Self {
            bump,
            global_items: BumpVec::new_in(bump),
            decls: BumpVec::new_in(bump),
            stmts: BumpVec::new_in(bump),
            exprs: BumpVec::new_in(bump),
            lvals: BumpVec::new_in(bump),
            blocks: BumpVec::new_in(bump),
            const_init_vals: BumpVec::new_in(bump),
            const_defs: BumpVec::new_in(bump),
            var_defs: BumpVec::new_in(bump),
            func_f_params: BumpVec::new_in(bump),
        }
    }

    // Allocation methods for each node type.
    // The parser will call these to create nodes.

    pub fn alloc_global_item(&mut self, item: Node<GlobalItem<'ast>>) -> NodeId<Node<GlobalItem<'ast>>> {
        let id = NodeId::new(self.global_items.len());
        self.global_items.push(item);
        id
    }

    pub fn alloc_decl(&mut self, decl: Node<Decl<'ast>>) -> NodeId<Node<Decl<'ast>>> {
        let id = NodeId::new(self.decls.len());
        self.decls.push(decl);
        id
    }

    pub fn alloc_stmt(&mut self, stmt: Node<Stmt<'ast>>) -> NodeId<Node<Stmt<'ast>>> {
        let id = NodeId::new(self.stmts.len());
        self.stmts.push(stmt);
        id
    }

    pub fn alloc_expr(&mut self, expr: Node<Exp<'ast>>) -> NodeId<Node<Exp<'ast>>> {
        let id = NodeId::new(self.exprs.len());
        self.exprs.push(expr);
        id
    }
    
    pub fn alloc_lval(&mut self, lval: Node<LVal<'ast>>) -> NodeId<Node<LVal<'ast>>> {
        let id = NodeId::new(self.lvals.len());
        self.lvals.push(lval);
        id
    }

    pub fn alloc_block(&mut self, block: Node<Block<'ast>>) -> NodeId<Node<Block<'ast>>> {
        let id = NodeId::new(self.blocks.len());
        self.blocks.push(block);
        id
    }
    
    pub fn alloc_const_init_val(&mut self, val: Node<ConstInitVal<'ast>>) -> NodeId<Node<ConstInitVal<'ast>>> {
        let id = NodeId::new(self.const_init_vals.len());
        self.const_init_vals.push(val);
        id
    }

    pub fn alloc_const_def(&mut self, def: Node<ConstDef<'ast>>) -> NodeId<Node<ConstDef<'ast>>> {
        let id = NodeId::new(self.const_defs.len());
        self.const_defs.push(def);
        id
    }

    pub fn alloc_var_def(&mut self, def: Node<VarDef<'ast>>) -> NodeId<Node<VarDef<'ast>>> {
        let id = NodeId::new(self.var_defs.len());
        self.var_defs.push(def);
        id
    }
    
    pub fn alloc_func_f_param(&mut self, param: Node<FuncFParam<'ast>>) -> NodeId<Node<FuncFParam<'ast>>> {
        let id = NodeId::new(self.func_f_params.len());
        self.func_f_params.push(param);
        id
    }

    pub fn alloc_param_dimensions(
        &self,
        first: Option<i32>,
        others: Vec<i32>,
    ) -> &'ast [Option<i32>] {
        // 将之前在 parser 中的逻辑完全移动到这里
        let mut dims = BumpVec::new_in(self.bump);
        dims.push(first);
        dims.extend(others.into_iter().map(Some));
        dims.into_bump_slice()
    }

    pub fn alloc_options_i32(&self, vec: BumpVec<'ast, Option<i32>>) -> &'ast [Option<i32>] {
        vec.into_bump_slice()
    }

    /// 从一个迭代器中分配一个 NodeId 列表，并返回一个生命周期正确的切片。
    pub fn alloc_node_ids<T, I>(&self, iter: I) -> &'ast [NodeId<T>]
    where
        I: IntoIterator<Item = NodeId<T>>,
        I::IntoIter: ExactSizeIterator, // 迭代器最好能知道自己的长度，性能更佳
    {
        let mut vec = BumpVec::new_in(self.bump); // 直接访问内部的 bump
        vec.extend(iter);
        vec.into_bump_slice()
    }

    /// 从一个迭代器分配一个 i32 列表 (用于数组维度等)
    pub fn alloc_i32s<I>(&self, iter: I) -> &'ast [i32]
    where
        I: IntoIterator<Item = i32>,
        I::IntoIter: ExactSizeIterator,
    {
        let mut vec = BumpVec::new_in(self.bump);
        vec.extend(iter);
        vec.into_bump_slice()
    }

    pub fn alloc_block_items<I>(&self, iter: I) -> &'ast [BlockItem<'ast>]
    where
        I: IntoIterator<Item = BlockItem<'ast>>,
    {
        let mut vec = BumpVec::new_in(self.bump);
        vec.extend(iter);
        vec.into_bump_slice()
    }
}

impl_arena_index! {
    (GlobalItem, global_items),
    (Decl, decls),
    (Stmt, stmts),
    (Exp, exprs),
    (LVal, lvals),
    (Block, blocks),
    (ConstInitVal, const_init_vals),
    (ConstDef, const_defs),
    (VarDef, var_defs),
    (FuncFParam, func_f_params)
}

// ===================================================================
//  Generic AST Structures
// ===================================================================

/// A generic node containing a `kind` and its `Span`.
/// It is now `Copy` because it no longer holds complex types directly.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Node<T> {
    pub kind: T,
    pub span: Span,
}

impl<T> Node<T> {
    pub fn new(kind: T, span: Span) -> Self {
        Self { kind, span }
    }
}


// ===================================================================
//  Refactored AST Definitions
// ===================================================================

// ---  Top-Level AST Structures ---

/// The root of the AST. It's now just a slice of IDs pointing to global items.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CompilationUnit<'ast> {
    pub items: &'ast [NodeId<Node<GlobalItem<'ast>>>],
}

/// A top-level item. Notice the `'ast` lifetime is still here because
/// some variants hold slices (`&'ast [...]`) which are tied to the arena's lifetime.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GlobalItem<'ast> {
    Function(FuncDef<'ast>),
    Declaration(NodeId<Node<Decl<'ast>>>), // Changed to ID
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BType { Int, Bool, Float, Double }


// --- Declarations ---

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Decl<'ast> {
    Const(ConstDecl<'ast>),
    Var(VarDecl<'ast>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ConstDecl<'ast> {
    pub b_type: BType,
    pub defs: &'ast [NodeId<Node<ConstDef<'ast>>>], // Changed to slice of IDs
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ConstDef<'ast> {
    pub name: Symbol, // Changed from String
    pub dimensions: &'ast [i32], // Changed from Vec
    pub init: NodeId<Node<ConstInitVal<'ast>>>, // Changed to ID
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct VarDecl<'ast> {
    pub b_type: BType,
    pub defs: &'ast [NodeId<Node<VarDef<'ast>>>], // Changed to slice of IDs
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct VarDef<'ast> {
    pub name: Symbol, // Changed from String
    pub dimensions: &'ast [i32], // Changed from Vec
    pub init: Option<NodeId<Node<ConstInitVal<'ast>>>>, // Changed to Option<ID>
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ConstInitVal<'ast> {
    Single(ConstExp),
    Aggregate(&'ast [NodeId<Node<ConstInitVal<'ast>>>]), // Changed to slice of IDs
}

pub type ConstExp = Literal;


// --- Functions ---

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FuncDef<'ast> {
    pub return_type: FuncType,
    pub name: Symbol, // Changed from String
    pub params: &'ast [NodeId<Node<FuncFParam<'ast>>>], // Changed to slice of IDs
    pub body: NodeId<Node<Block<'ast>>>, // Changed to ID
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FuncType { Void, Type(BType) }

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FuncFParam<'ast> {
    pub b_type: BType,
    pub name: Symbol, // Changed from String
    pub dimensions: &'ast [Option<i32>], // Changed from Vec
}


// --- Statements and Blocks ---

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Block<'ast> {
    pub items: &'ast [BlockItem<'ast>],
}

// BlockItem can be simplified. A block contains Decls and Stmts.
// The arena can store DeclId and StmtId directly.
// This enum helps distinguish them in a list.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BlockItem<'ast> {
    Declaration(NodeId<Node<Decl<'ast>>>),
    Statement(NodeId<Node<Stmt<'ast>>>),
}


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Stmt<'ast> {
    Assign {
        left: NodeId<Node<LVal<'ast>>>,
        right: NodeId<Node<Exp<'ast>>>,
    },
    Expression(Option<NodeId<Node<Exp<'ast>>>>),
    Block(NodeId<Node<Block<'ast>>>), // Changed to ID
    If {
        cond: NodeId<Node<Exp<'ast>>>,
        then_branch: NodeId<Node<Stmt<'ast>>>,
        else_branch: Option<NodeId<Node<Stmt<'ast>>>>,
    },
    While {
        cond: NodeId<Node<Exp<'ast>>>,
        body: NodeId<Node<Stmt<'ast>>>,
    },
    Break,
    Continue,
    Return(Option<NodeId<Node<Exp<'ast>>>>),
}


// --- Expressions and Operators ---

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Exp<'ast> {
    Literal(Literal),
    LValue(NodeId<Node<LVal<'ast>>>),
    Unary {
        op: UnaryOp,
        operand: NodeId<Node<Exp<'ast>>>,
    },
    Binary {
        left: NodeId<Node<Exp<'ast>>>,
        op: BinaryOp,
        right: NodeId<Node<Exp<'ast>>>,
    },
    Call {
        name: Symbol, // Changed from String
        args: &'ast [NodeId<Node<Exp<'ast>>>],
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LVal<'ast> {
    pub name: Symbol, // Changed from String
    pub indices: &'ast [NodeId<Node<Exp<'ast>>>],
}


// --- Literals and Operators (No changes needed here) ---

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f32),
    Double(f64),
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp { Plus, Minus, Not }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne,
    Lt, Gt, Le, Ge,
    And, Or,
}
