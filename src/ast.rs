#[derive(Debug, Clone)]
pub struct File {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub name: Identifier,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignement {
        lvalue: Expression,
        rvalue: Expression,
    },
    VariableDeclaration {
        identifier: Identifier,
        initialization: Option<Expression>,
    },
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub final_expression: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct Identifier(pub String);

#[derive(Debug, Clone)]
pub struct Type(pub Identifier);

#[derive(Debug, Clone)]
pub enum Expression {
    BinaryOperation {
        left: Box<Expression>,
        operation: BinaryOperator,
        right: Box<Expression>,
    },
    UnaryOperation {
        operation: UnaryOperator,
        operand: Box<Expression>,
    },
    Identifier(Identifier),
    Literal(Literal),
    Block(Box<Block>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(String),
    Float(String),
    String(String),
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Substract,
    Multiply,
    Divivide,

    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,

    Equal,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negate,
    Not,
    Complement,
}