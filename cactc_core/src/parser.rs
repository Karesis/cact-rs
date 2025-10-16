use chumsky::prelude::*;
use chumsky::error::Rich;
use chumsky::input::{IterInput, MapExtra, Stream};
use chumsky::extra::{Full, SimpleState};
use chumsky::pratt::*;
use crate::lexer::{Token, TokenStream};
use crate::interner::Symbol;
use crate::ast::{self, AstArena, Node};
use crate::span::Span;
use crate::errors::{CompilerError, ParsingError, LexicalError};

type ParserError<'tokens> = Rich<'tokens, Token, Span>;
type ParserInput<'tokens> = IterInput<TokenStream<'tokens>, Span>;
type ParserState<'tokens> = SimpleState<&'tokens mut AstArena<'tokens>>;
type ParserUtils<'tokens> = Full<ParserError<'tokens>, ParserState<'tokens>, ()>;

/// in rust, we cannot yet do like this:
/// type Parser<'tokens, O> = impl chumsky::Parser<'tokens, ParserInput<'tokens>, O, ParserError<'tokens>>;
/// it needs nightly rustc and is unstable.
/// so i just use a macro to simplify it.
macro_rules! ParserOutput {
    // take a param like `$output` ，which is `ty` (type)
    ($output:ty) => {
        // expansion full impl Trait type
        impl chumsky::Parser<
            'tokens,
            ParserInput<'tokens>,
            $output,
            ParserUtils<'tokens>
        >
    };
}

/// 一个用于创建前缀（一元）操作符解析器的宏
macro_rules! unary_op {
    ($prec:expr, $token:expr, $op:expr) => {
        // `prefix` 的闭包接收 `rhs` (一个 NodeId) 和 `extra`
        prefix($prec, just($token).to($op), |op, rhs: ast::NodeId<Node<ast::Exp<'_>>>, extra: &mut MapExtra<'_, '_, ParserInput, ParserUtils>| {
            let op_span = extra.span();
            let arena: &mut &mut AstArena<'_> = &mut extra.state().0;
            let rhs_span = arena[rhs].span;
            let span = op_span.to(rhs_span);

            let kind = ast::Exp::Unary { op, operand: rhs };
            let node = ast::Node::new(kind, span);
            arena.alloc_expr(node)
        }).boxed()
    };
}

/// 一个用于创建中缀（二元）操作符解析器的宏
macro_rules! binary_op {
    ($assoc:ident, $prec:expr, $token:expr, $op:expr) => {
        // `infix` 的闭包接收 `lhs`, `op`, `rhs` (都是 NodeId) 和 `extra`
        infix($assoc($prec), just($token).to($op), |lhs, op, rhs: ast::NodeId<Node<ast::Exp<'_>>>, extra: &mut MapExtra<'_, '_, ParserInput, ParserUtils>| {
            let arena = &mut extra.state().0;
            let lhs_span = arena[lhs].span;
            let rhs_span = arena[rhs].span;
            let span = lhs_span.to(rhs_span);

            let kind = ast::Exp::Binary { left: lhs, op, right: rhs };
            let node = ast::Node::new(kind, span);
            arena.alloc_expr(node)
        }).boxed()
    };
}

pub trait RecursiveParser<'tokens, O>:
    chumsky::Parser<'tokens, ParserInput<'tokens>, O, ParserUtils<'tokens>>
    + Clone
    + 'tokens // 注意这里是 `+ 'tokens`，这是在 Trait bound 中添加生命周期约束的正确语法
{}

impl<'tokens, O, P> RecursiveParser<'tokens, O> for P
where
    P: chumsky::Parser<'tokens, ParserInput<'tokens>, O, ParserUtils<'tokens>>
        + Clone
        + 'tokens,
{}

macro_rules! id_node {
    ($node_type:ident) => {
        ast::NodeId<Node<ast::$node_type<'tokens>>>
    };
}

macro_rules! recursive_parser {
    // take a param like `$output` ，which is `ty` (type)
    ($output:ident) => {
        // expansion full impl Trait type
        impl RecursiveParser<
            'tokens,
            id_node!($output),
        >
    };
}

macro_rules! recursive {
    ($node_type:ident) => {
        Recursive<chumsky::recursive::Indirect<'_, '_, IterInput<TokenStream<'_>, Span>, node_id!(node_type), Full<Rich<'_, Token, Span>, SimpleState<&mut AstArena<'_>>, ()>>>
    };
}

/// parser an ident
fn ident<'tokens>() -> ParserOutput!(Symbol) {
    select! {
        Token::Identifier(symbol) => symbol,
    }
    .labelled("identifier")
}

/// parse a basic type
/// BType = "int" | "bool" | "float" | "double" ;
fn b_type<'tokens>() -> ParserOutput!(ast::BType) {
    select! {
        Token::IntType => ast::BType::Int,
        Token::BoolType => ast::BType::Bool,
        Token::FloatType => ast::BType::Float,
        Token::DoubleType => ast::BType::Double,
    }
    .labelled("basic type (int, bool, float, double)")
}

/// 解析一个整数常量 Token，并返回其 i32 值
fn int_const_val<'tokens>() -> ParserOutput!(i32) {
    select! {
        Token::IntConst(i) => i,
    }
    .boxed()
    .labelled("integer constant")
}

/// parse a return type of a function
/// FuncType = "void" | BType ;
fn func_type<'tokens>() -> ParserOutput!(ast::FuncType) {
    choice((
        // return void
        just(Token::Void).to(ast::FuncType::Void),
        // return a type
        b_type().map(ast::FuncType::Type),
    ))
    .boxed()
    .labelled("function return type (void, int, etc.)")
}

/// 解析一个左值 (LVal)，例如 `x` 或 `arr[i][1]`
/// LVal = Identifier, { "[", Exp, "]" } ;
fn lval<'tokens>(
    expr: recursive_parser!(Exp)
) -> ParserOutput!(ast::NodeId<Node<ast::LVal<'tokens>>>) {
    ident()
        // 后面跟着零个或多个 "[ Exp ]"
        .then(
            expr.clone() // 依赖于我们之后会写的 expression parser
                .delimited_by(just(Token::LBracket), just(Token::RBracket))
                .repeated()
                .collect::<Vec<_>>()
        )
        .map_with(|(name, vec_indices), extra| {
            let span = extra.span();
            let arena = &mut extra.state().0;
            let indices = arena.alloc_node_ids(vec_indices);
            let lvalue = ast::LVal {
                name,
                indices, // `indices` 是一个 Vec<Node<ast::Exp>>
            };
            let node = ast::Node::new(lvalue, span);
            arena.alloc_lval(node)
        })
        .boxed()
        .labelled("variable or array access")
}

/// 解析一个常量初始化值，例如 `= 5` 或 `= {1, 2, 3}`
/// ConstInitVal = ConstExp | "{", [ ConstInitVal, { ",", ConstInitVal } ], "}" ;
fn const_init_val<'tokens>() -> ParserOutput!(ast::NodeId<ast::Node<ast::ConstInitVal<'tokens>>>) {
    let mut init_val = Recursive::declare();


    let body = choice((
        // 情况 1: 单个常量表达式 (返回 ConstInitVal)
        const_exp()
            .map(ast::ConstInitVal::Single)
            .map_with(|kind, extra| {
                let span = extra.span();
                let arena = &mut extra.state().0; 
                let node = ast::Node::new(kind, span);
                arena.alloc_const_init_val(node)
            }),

        // 情况 2: 聚合初始化列表 (也返回 ConstInitVal)
        init_val
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|items, extra: &mut chumsky::input::MapExtra<'_, '_, IterInput<TokenStream<'_>, Span>, Full<Rich<'_, Token, Span>, SimpleState<&mut AstArena<'_>>, ()>>| {
                let span = extra.span();
                // 1. 获取一次 arena 的可变借用，现在它是安全的
                let arena = &mut extra.state().0;

                let items_slice = arena.alloc_node_ids(items);

                let kind = ast::ConstInitVal::Aggregate(items_slice);
                
                let node = ast::Node::new(kind, span);
                let node_id = arena.alloc_const_init_val(node);

                // 5. 返回最终的结果
                node_id
            })
    )).boxed();
    init_val.define(body);
    init_val.labelled("initializer")
}

/// 解析单个变量定义 (VarDef), e.g., `a[10] = 5`
/// VarDef = Identifier, { "[", IntConst, "]" }, [ "=", ConstInitVal ] ;
fn var_def<'tokens>() -> ParserOutput!(ast::NodeId<ast::Node<ast::VarDef<'tokens>>>) { // 2. 返回 NodeId
    
    // 我们将解析链分解开来，让逻辑更清晰
    
    // 解析数组维度部分
    let dimensions = int_const_val()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .repeated()
        .collect::<Vec<_>>(); // 先收集到一个临时的 Vec<i32>

    // 解析可选的初始化部分
    let initializer = just(Token::Assign)
        .ignore_then(const_init_val()) 
        .or_not();

    ident()
        .then(dimensions)
        .then(initializer)
        .map_with(|((name, dims_vec), init), extra| {
            let span = extra.span();
            let arena = &mut extra.state().0;

            // 2. 在 arena 中分配列表
            let dimensions = arena.alloc_i32s(dims_vec);
            
            // 3. 构建 VarDef 这个 "kind"
            let kind = ast::VarDef {
                name,
                dimensions,
                init,
            };
            
            // 4. 将 VarDef 节点本身分配到 arena
            let node = ast::Node::new(kind, span);
            
            // 5. 返回最终的 NodeId
            arena.alloc_var_def(node)
        })
}

/// 解析一个完整的变量声明语句 (VarDecl), e.g., `int a, b = 5;`
/// VarDecl = BType, VarDef, { ",", VarDef }, ";" ;
fn var_decl<'tokens>() -> ParserOutput!(ast::Node<ast::VarDecl<'tokens>>) {
    b_type()
        .then(
            var_def()
                .separated_by(just(Token::Comma))
                .at_least(1) // 必须至少有一个定义
                .collect::<Vec<_>>()
        )
        .then_ignore(just(Token::Semicolon))
        .map_with(|(b_type, vec_defs), extra| {
            let arena = &mut extra.state().0;
            let defs = arena.alloc_node_ids(vec_defs);
            let node = ast::VarDecl {
                b_type,
                defs,
            };
            ast::Node::new(node, extra.span())
        })
        .labelled("variable declaration")
}

// parser.rs

/// 解析一个整数 Token，并将其包装成 ast::Literal::Int
fn int_literal<'tokens>() -> ParserOutput!(ast::Literal) {
    select! {
        Token::IntConst(i) => ast::Literal::Int(i),
    }
    .labelled("integer literal")
}

/// 解析一个浮点数 Token，并将其包装成 ast::Literal::Float
fn float_literal<'tokens>() -> ParserOutput!(ast::Literal) {
    select! {
        Token::FloatConst(f) => ast::Literal::Float(f),
    }
    .labelled("float literal")
}

/// 解析一个双精度浮点数 Token，并将其包装成 ast::Literal::Double
fn double_literal<'tokens>() -> ParserOutput!(ast::Literal) {
    select! {
        Token::DoubleConst(d) => ast::Literal::Double(d),
    }
    .labelled("double literal")
}

/// 解析一个布尔值 Token，并将其包装成 ast::Literal::Bool
fn bool_literal<'tokens>() -> ParserOutput!(ast::Literal) {
    select! {
        Token::BoolConst(b) => ast::Literal::Bool(b),
    }
    .labelled("boolean literal")
}

/// `Number = IntConst | FloatConst | DoubleConst`
/// 这个 parser 解析任意数字，返回 ast::Literal
fn number<'tokens>() -> ParserOutput!(ast::Literal) {
    choice((
        int_literal(),
        float_literal(),
        double_literal(),
    ))
    .labelled("number")
}

/// `ConstExp = Number | BoolConst`
/// 这个 parser 解析任意常量表达式，返回 ast::Literal
fn const_exp<'tokens>() -> ParserOutput!(ast::Literal) {
    choice((
        number(),
        bool_literal(),
    ))
    .labelled("constant expression")
}

/// 解析所有类型的表达式
fn expression<'tokens>(
    expr: recursive_parser!(Exp)
) -> ParserOutput!(ast::NodeId<Node<ast::Exp<'tokens>>>) {
    // 1. 声明一个空的、可克隆的递归解析器占位符
    let mut expr = Recursive::declare();
    // 1. 定义一个递归容器，它将代表完整的表达式解析器
        // 2. 定义表达式的“原子”部分
    let atom = choice((
        // 字面量: Number | BoolConst
        const_exp()
            .map(ast::Exp::Literal)
            .map_with(|kind, extra| {
                let node = Node::new(kind, extra.span());
                extra.state().0.alloc_expr(node)
            })
            .boxed(),

        // 左值: 变量或数组访问
        lval(expr)
            .map(ast::Exp::LValue)
            .map_with(|kind, extra| {
                let node = Node::new(kind, extra.span());
                extra.state().0.alloc_expr(node)
            })
            .boxed(),

        // 函数调用: Identifier ( [FuncRParams] )
        ident()
            .then(
                expr.clone() // 递归调用，因为参数本身也是表达式
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen))
            )
            .map_with(|(name, args_vec), extra| {
                let span = extra.span();
                let arena = &mut extra.state().0;
                let args = arena.alloc_node_ids(args_vec);
                let kind = ast::Exp::Call { name, args };
                let node = Node::new(kind, span);
                arena.alloc_expr(node)
            })
            .boxed(),
            
        // 括号内的表达式: ( Exp )
        expr.clone()
            .delimited_by(just(Token::LParen), just(Token::RParen)).boxed(),
    ));

    // 3. 使用 .pratt() 定义操作符和优先级
    let pratt_parser = atom.pratt((
        // 优先级 6: 前缀操作符 +, -, !
        unary_op!(6, Token::Plus, ast::UnaryOp::Plus),
        unary_op!(6, Token::Minus, ast::UnaryOp::Minus),
        unary_op!(6, Token::Not, ast::UnaryOp::Not),

        // 优先级 5: 乘除 *, /, %
        binary_op!(left, 5, Token::Star, ast::BinaryOp::Mul),
        binary_op!(left, 5, Token::Slash, ast::BinaryOp::Div),
        binary_op!(left, 5, Token::Percent, ast::BinaryOp::Mod),

        // 优先级 4: 加减 +, -
        binary_op!(left, 4, Token::Plus, ast::BinaryOp::Add),
        binary_op!(left, 4, Token::Minus, ast::BinaryOp::Sub),

        // 优先级 3: 关系 <, >, <=, >=
        binary_op!(left, 3, Token::Lt, ast::BinaryOp::Lt),
        binary_op!(left, 3, Token::Gt, ast::BinaryOp::Gt),
        binary_op!(left, 3, Token::Le, ast::BinaryOp::Le),
        binary_op!(left, 3, Token::Ge, ast::BinaryOp::Ge),

        // 优先级 2: 相等 ==, !=
        binary_op!(left, 2, Token::Eq, ast::BinaryOp::Eq),
        binary_op!(left, 2, Token::Ne, ast::BinaryOp::Ne),

        // 优先级 1: 逻辑与 &&
        binary_op!(left, 1, Token::And, ast::BinaryOp::And),

        // 优先级 0: 逻辑或 ||
        binary_op!(left, 0, Token::Or, ast::BinaryOp::Or),
    )).boxed();
    expr.define(pratt_parser);
    expr.labelled("expression")
}

/// 解析单个常量定义 (ConstDef), e.g., `c[10] = 5`
fn const_def<'tokens>() -> ParserOutput!(ast::NodeId<ast::Node<ast::ConstDef<'tokens>>>) {
    let dimensions = int_const_val()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .repeated()
        .collect::<Vec<_>>();

    // 与 var_def 的唯一区别：这里没有 .or_not()
    let initializer = just(Token::Assign).ignore_then(const_init_val());

    ident()
        .then(dimensions)
        .then(initializer)
        .map_with(|((name, dims_vec), init), extra| {
            let span = extra.span();
            let arena = &mut extra.state().0;
            let dimensions = arena.alloc_i32s(dims_vec);
            
            let kind = ast::ConstDef {
                name,
                dimensions,
                init,
            };
            
            let node = ast::Node::new(kind, span);
            arena.alloc_const_def(node)
        })
}

/// 内部函数：只解析并返回 `VarDecl` 这个 "kind"
fn var_decl_kind<'tokens>() -> ParserOutput!(ast::VarDecl<'tokens>) {
    b_type()
        .then(
            var_def()
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect::<Vec<_>>()
        )
        .then_ignore(just(Token::Semicolon))
        .map_with(|(b_type, defs_vec), extra| {
            let arena = &mut extra.state().0;
            let defs = arena.alloc_node_ids(defs_vec);
            ast::VarDecl { b_type, defs }
        })
}

/// 内部函数：只解析并返回 `ConstDecl` 这个 "kind"
fn const_decl_kind<'tokens>() -> ParserOutput!(ast::ConstDecl<'tokens>) {
    just(Token::Const)
        .ignore_then(b_type())
        .then(
            const_def()
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect::<Vec<_>>()
        )
        .then_ignore(just(Token::Semicolon))
        .map_with(|(b_type, defs_vec), extra| {
            let arena = &mut extra.state().0;
            let defs = arena.alloc_node_ids(defs_vec);
            ast::ConstDecl { b_type, defs }
        })
}

/// 解析一个完整的声明语句 (变量或常量)，并返回其 NodeId
fn declaration<'tokens>() -> ParserOutput!(ast::NodeId<ast::Node<ast::Decl<'tokens>>>) {
    choice((
        const_decl_kind().map(ast::Decl::Const),
        var_decl_kind().map(ast::Decl::Var),
    ))
    // 在 choice 之后，我们得到了 ast::Decl 这个 kind
    .map_with(|kind, extra| {
        // 在这里进行统一的、最终的分配
        let node = ast::Node::new(kind, extra.span());
        extra.state().0.alloc_decl(node)
    })
    .labelled("declaration")
}



/// 创建语句和代码块相关的、相互递归的解析器
fn statement_kind_parser<'tokens>(
    stmt: recursive_parser!(Stmt),
    block: recursive_parser!(Block),
    expr: recursive_parser!(Exp)
) -> ParserOutput!(ast::Stmt<'tokens>) 
{
    choice((
        // 分支 1: If-Else 语句
        just(Token::If)
            .ignore_then(expression(expr.clone()).delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(stmt.clone())
            .then(just(Token::Else).ignore_then(stmt.clone()).or_not())
            .map(|((cond, then_branch), else_branch)| ast::Stmt::If { cond, then_branch, else_branch }),

        // 分支 2: While 循环
        just(Token::While)
            .ignore_then(expression(expr.clone()).delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(stmt.clone())
            .map(|(cond, body)| ast::Stmt::While { cond, body }),

        // 分支 3: 返回语句
        just(Token::Return)
            .ignore_then(expression(expr.clone()).or_not())
            .then_ignore(just(Token::Semicolon))
            .map(ast::Stmt::Return),

        // 分支 4: 赋值语句
        lval(expr.clone())
            .then_ignore(just(Token::Assign))
            .then(expression(expr.clone()))
            .then_ignore(just(Token::Semicolon))
            .map(|(left, right)| ast::Stmt::Assign { left, right }),

        // 分支 5: 表达式语句
        expression(expr.clone())
            .or_not()
            .then_ignore(just(Token::Semicolon))
            .map(ast::Stmt::Expression),
        
        // 分支 6: Break 和 Continue
        just(Token::Break).to(ast::Stmt::Break).then_ignore(just(Token::Semicolon)),
        just(Token::Continue).to(ast::Stmt::Continue).then_ignore(just(Token::Semicolon)),

        // 分支 7: 代码块作为语句
        block.clone().map(ast::Stmt::Block),
    ))
}

/// 辅助函数：构建完整的 block 解析器
fn block_parser<'tokens>(
    stmt: recursive!(Stmt)
) -> ParserOutput!(id_node!(Block)) {
    
    // BlockItem 的定义被封装在内部
    let block_item = choice((
        declaration().map(ast::BlockItem::Declaration),
        stmt.map(ast::BlockItem::Statement),
    )).boxed();

    block_item
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
        .map_with(|items_vec, extra| {
            let span = extra.span();
            let arena = &mut extra.state().0;
            // alloc_block_items 接收 Vec<BlockItem>，返回 &'ast [BlockItem]
            let items = arena.alloc_block_items(items_vec); 
            // ast::Block 的 `items` 字段需要 &'ast [BlockItem]，完美匹配！
            let kind = ast::Block { items };
            let node = Node::new(kind, span);
            arena.alloc_block(node)
        })
        .boxed() // 在最后装箱以简化类型
}

/// 创建语句和代码块相关的、相互递归的解析器 (重构后)
fn statement_parsers<'tokens>(
) -> (
    recursive_parser!(Stmt),
    recursive_parser!(Block)
) {
    let mut stmt: recursive!(Stmt) = Recursive::declare();
    let mut block: recursive!(Block) = Recursive::declare();
    let mut expr: recursive!(Exp) = Recursive::declare();
    // 2. 使用辅助函数构建 block 解析器的主体，并定义 block
    let block_body = block_parser(stmt.clone());
    block.define(block_body.boxed());

    // 3. 使用辅助函数构建 statement 的 "kind"，然后完成最后的分配
    let stmt_kind_parser = statement_kind_parser(stmt.clone(), block.clone(), expr.clone());
    
    let stmt_body = stmt_kind_parser
        .map_with(|kind, extra| {
            let node = Node::new(kind, extra.span());
            extra.state().0.alloc_stmt(node)
        })
        .boxed();

    // 4. 定义 stmt
    stmt.define(stmt_body);

    // 5. 返回定义好的解析器
    (stmt, block)
}

/// 解析一个函数形参 (FuncFParam)，并返回其 NodeId
fn func_f_param<'tokens>() -> ParserOutput!(ast::NodeId<Node<ast::FuncFParam<'tokens>>>) {
    // 数组第一维的解析器: `[ [IntConst] ]` -> Option<i32>
    let first_dim = int_const_val()
        .or_not() // 处理 `[]` 的情况，返回 None
        .delimited_by(just(Token::LBracket), just(Token::RBracket));

    // 数组后续维度的解析器: `[ IntConst ]` -> i32
    let other_dims = int_const_val()
        .delimited_by(just(Token::LBracket), just(Token::RBracket));

    b_type()
        .then(ident())
        // `then()` first_dim and other_dims repeated, `or_not()` for the whole array part
        .then(
            first_dim
                .then(other_dims.repeated().collect::<Vec<_>>())
                .or_not() // 整个数组部分是可选的
        )
        .map_with(|((b_type, name), dims_option), extra| {
            let span = extra.span();
            let arena = &mut extra.state().0;
            
            // 处理维度列表
            let dimensions = match dims_option {
                None => &[][..], // 非数组参数，得到一个空切片
                Some((first, others)) => arena.alloc_param_dimensions(first, others),
            };

            let kind = ast::FuncFParam {
                b_type,
                name,
                dimensions,
            };

            let node = Node::new(kind, span);
            arena.alloc_func_f_param(node)
        })
}

/// 解析一个函数定义，只返回 `FuncDef` 这个 "kind"
/// 它需要一个 `block` 解析器作为参数
fn function_definition_kind<'tokens>(
    block: impl RecursiveParser<'tokens, ast::NodeId<Node<ast::Block<'tokens>>>>,
) -> ParserOutput!(ast::FuncDef<'tokens>) {
    
    // 参数列表的解析器
    let params_list = func_f_param()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>();

    func_type()
        .then(ident())
        .then(
            // `or_not()` 处理空参数列表 `()` 的情况
            params_list.or_not() 
                .delimited_by(just(Token::LParen), just(Token::RParen))
        )
        .then(block) // 使用传入的 block 解析器
        .map_with(|(((return_type, name), params_opt), body), extra| {
            let arena = &mut extra.state().0;
            
            // 如果 `params_opt` 是 None，就分配一个空列表
            let params = match params_opt {
                Some(params_vec) => arena.alloc_node_ids(params_vec),
                None => &[][..],
            };

            ast::FuncDef {
                return_type,
                name,
                params,
                body,
            }
        })
}

/// 解析一个顶层项 (GlobalItem)，并返回其 NodeId
fn global_item<'tokens>(
    // 需要传入已经定义好的递归解析器
    stmt: impl RecursiveParser<'tokens, ast::NodeId<Node<ast::Stmt<'tokens>>>>,
    block: impl RecursiveParser<'tokens, ast::NodeId<Node<ast::Block<'tokens>>>>,
) -> ParserOutput!(ast::NodeId<ast::Node<ast::GlobalItem<'tokens>>>) {
    choice((
        // 分支 1: 声明
        declaration().map(ast::GlobalItem::Declaration),

        // 分支 2: 函数定义
        // 我们在这里调用 function_definition_kind，并把 block 解析器传给它
        function_definition_kind(block).map(ast::GlobalItem::Function),
    ))
    // 和 declaration() 模式一样，在 choice 之后统一分配
    .map_with(|kind, extra| {
        let node = Node::new(kind, extra.span());
        extra.state().0.alloc_global_item(node)
    })
    .labelled("global item (declaration or function)")
}

/// 解析整个编译单元 (CompilationUnit)
/// 这是最终的、对外的顶层解析器函数。
pub fn compilation_unit<'tokens>() -> ParserOutput!(ast::CompilationUnit<'tokens>) {
    // 1. 在一个作用域内，创建所有需要的递归解析器
    // 我们从 statement_parsers 工厂函数获取它们
        // 1. 声明占位符
    let (stmt, block) = statement_parsers();

    // 2. 使用这些定义好的解析器来构建 global_item 解析器
    global_item(stmt, block)
        .repeated()
        .collect::<Vec<_>>()
        // 确保解析到文件末尾
        .then_ignore(end())
        .map_with(|items_vec, extra| {
            // 将 Vec<NodeId> 转换成 &'tokens [NodeId]
            let items = extra.state().0.alloc_node_ids(items_vec);
            ast::CompilationUnit { items }
        })
}

pub fn parse<'s>(
    token_stream: TokenStream<'s>,
    source: &str,
    arena: &'s mut AstArena<'s>,
) -> (Option<ast::CompilationUnit<'s>>, Vec<CompilerError>) {
    // 创建 chumsky 输入流
    let eoi_span = Span::new(source.len(), source.len());
    let chumsky_input = IterInput::new(token_stream, eoi_span);

    // 获取我们的顶层解析器
    let parser = compilation_unit();

    // 使用 .parse_with_state 启动解析
    let (ast_option, parse_errors) = parser
        .parse_with_state(chumsky_input, &mut SimpleState(arena))
        .into_output_errors();

    // 将 chumsky 的 Rich 错误转换为我们自己的 CompilerError
    let formatted_errors: Vec<CompilerError> = parse_errors
        .into_iter()
        .map(ParsingError::from)
        .map(CompilerError::from)
        .collect();

    (ast_option, formatted_errors)
}