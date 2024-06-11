#[derive(Debug, Clone, Copy)]
pub struct AstNodeId(usize);

#[derive(Debug, Clone)]
pub enum AstNode {
    File(File),
    FunctionDefinition(FunctionDefinition),
    Statement(Statement),
    Block(Block),
    Identifier(Identifier),
    Type(Type),
    Expression(Expression),
    Literal(Literal),
}

#[derive(Debug, Default)]
pub struct AstNodeStorage {
    storage: Vec<AstNode>,
}

impl AstNodeStorage {
    pub fn push(&mut self, node: impl Into<AstNode>) -> AstNodeId {
        self.storage.push(node.into());
        AstNodeId(self.storage.len() - 1)
    }

    pub fn get(&self, id: AstNodeId) -> &AstNode {
        // No bounds check because if we have an ID, it must have been constructed here
        &self.storage[id.0]
    }

    pub fn expect_function_definition(&self, id: AstNodeId) -> &FunctionDefinition {
        match self.get(id) {
            AstNode::FunctionDefinition(f) => f,
            _ => panic!("Expected function definition"),
        }
    }

    pub fn expect_statement(&self, id: AstNodeId) -> &Statement {
        match self.get(id) {
            AstNode::Statement(s) => s,
            _ => panic!("Expected statement"),
        }
    }

    pub fn expect_block(&self, id: AstNodeId) -> &Block {
        match self.get(id) {
            AstNode::Block(b) => b,
            _ => panic!("Expected block"),
        }
    }

    pub fn expect_identifier(&self, id: AstNodeId) -> &Identifier {
        match self.get(id) {
            AstNode::Identifier(i) => i,
            _ => panic!("Expected identifier"),
        }
    }

    pub fn expect_type(&self, id: AstNodeId) -> &Type {
        match self.get(id) {
            AstNode::Type(t) => t,
            _ => panic!("Expected type"),
        }
    }

    pub fn expect_expression(&self, id: AstNodeId) -> &Expression {
        match self.get(id) {
            AstNode::Expression(e) => e,
            _ => panic!("Expected expression"),
        }
    }

    pub fn expect_literal(&self, id: AstNodeId) -> &Literal {
        match self.get(id) {
            AstNode::Literal(l) => l,
            _ => panic!("Expected literal"),
        }
    }
}

pub trait AstVisitor {
    fn visit_file(&mut self, _file: &File) {}
    fn visit_function_definition(&mut self, _function: &FunctionDefinition) {}
    fn visit_statement(&mut self, _statement: &Statement) {}
    fn visit_block(&mut self, _block: &Block) {}
    fn visit_identifier(&mut self, _identifier: &Identifier) {}
    fn visit_type(&mut self, _type: &Type) {}
    fn visit_expression(&mut self, _expression: &Expression) {}
    fn visit_literal(&mut self, _literal: &Literal) {}
}

impl AstNodeStorage {
    pub fn visit_node<V: AstVisitor>(&self, visitor: &mut V, node: &AstNode) {
        match node {
            AstNode::File(f) => visitor.visit_file(f),
            AstNode::FunctionDefinition(f) => visitor.visit_function_definition(f),
            AstNode::Statement(s) => visitor.visit_statement(s),
            AstNode::Block(b) => visitor.visit_block(b),
            AstNode::Identifier(i) => visitor.visit_identifier(i),
            AstNode::Type(t) => visitor.visit_type(t),
            AstNode::Expression(e) => visitor.visit_expression(e),
            AstNode::Literal(l) => visitor.visit_literal(l),
        }
    }

    pub fn visit_file<V: AstVisitor>(&self, visitor: &mut V, file: &File) {
        for &f in &file.functions {
            visitor.visit_function_definition(self.expect_function_definition(f));
        }
    }
    pub fn visit_function_definition<V: AstVisitor>(
        &self,
        visitor: &mut V,
        function: &FunctionDefinition,
    ) {
        visitor.visit_identifier(self.expect_identifier(function.name));
        visitor.visit_block(self.expect_block(function.body));
    }
    pub fn visit_statement<V: AstVisitor>(&self, visitor: &mut V, statement: &Statement) {
        match statement {
            Statement::Assignement { lvalue, rvalue, .. } => {
                visitor.visit_expression(self.expect_expression(*lvalue));
                visitor.visit_expression(self.expect_expression(*rvalue));
            }
            Statement::VariableDeclaration {
                identifier,
                ty,
                initialization,
            } => {
                visitor.visit_identifier(self.expect_identifier(*identifier));
                visitor.visit_type(self.expect_type(*ty));
                if let Some(init) = initialization {
                    visitor.visit_expression(self.expect_expression(*init));
                }
            }
            Statement::Expression(e) => visitor.visit_expression(self.expect_expression(*e)),
        }
    }
    pub fn visit_block<V: AstVisitor>(&self, visitor: &mut V, block: &Block) {
        for &s in &block.statements {
            visitor.visit_statement(self.expect_statement(s));
        }
    }
    pub fn visit_type<V: AstVisitor>(&self, visitor: &mut V, ty: &Type) {
        visitor.visit_identifier(self.expect_identifier(ty.0));
    }
    pub fn visit_expression<V: AstVisitor>(&self, visitor: &mut V, expression: &Expression) {
        match expression {
            Expression::BinaryOperation { left, right, .. } => {
                visitor.visit_expression(self.expect_expression(*left));
                visitor.visit_expression(self.expect_expression(*right));
            }
            Expression::UnaryOperation { operand, .. } => {
                visitor.visit_expression(self.expect_expression(*operand))
            }
            Expression::Identifier(i) => visitor.visit_identifier(self.expect_identifier(*i)),
            Expression::Literal(l) => visitor.visit_literal(self.expect_literal(*l)),
            Expression::Block(b) => visitor.visit_block(self.expect_block(*b)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub functions: Vec<AstNodeId>,
}

impl From<File> for AstNode {
    fn from(f: File) -> Self {
        Self::File(f)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub name: AstNodeId,
    pub return_type: Option<AstNodeId>,
    pub body: AstNodeId,
}

impl From<FunctionDefinition> for AstNode {
    fn from(f: FunctionDefinition) -> Self {
        Self::FunctionDefinition(f)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignement {
        lvalue: AstNodeId,
        operator: AssignOperator,
        rvalue: AstNodeId,
    },
    VariableDeclaration {
        identifier: AstNodeId,
        ty: AstNodeId,
        initialization: Option<AstNodeId>,
    },
    Expression(AstNodeId),
}

impl From<Statement> for AstNode {
    fn from(s: Statement) -> Self {
        Self::Statement(s)
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<AstNodeId>,
    pub final_expression: Option<AstNodeId>,
}

impl From<Block> for AstNode {
    fn from(b: Block) -> Self {
        Self::Block(b)
    }
}

#[derive(Debug, Clone)]
pub struct Identifier(pub String);

impl From<Identifier> for AstNode {
    fn from(i: Identifier) -> Self {
        Self::Identifier(i)
    }
}

#[derive(Debug, Clone)]
pub struct Type(pub AstNodeId);

impl From<Type> for AstNode {
    fn from(t: Type) -> Self {
        Self::Type(t)
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    BinaryOperation {
        left: AstNodeId,
        operation: BinaryOperator,
        right: AstNodeId,
    },
    UnaryOperation {
        operation: UnaryOperator,
        operand: AstNodeId,
    },
    Identifier(AstNodeId),
    Literal(AstNodeId),
    Block(AstNodeId),
}

impl From<Expression> for AstNode {
    fn from(e: Expression) -> Self {
        Self::Expression(e)
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(String),
    Float(String),
    String(String),
}

impl From<Literal> for AstNode {
    fn from(l: Literal) -> Self {
        Self::Literal(l)
    }
}

#[derive(Debug, Clone)]
pub enum AssignOperator {
    Assign,
    AddAssign,
    SubstractAssign,
    MultiplyAssign,
    DivideAssign,
    BitAndAssign,
    BitOrAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
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
    NotEqual,
    LowerThan,
    LowerThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Nothing,
    Negate,
    Not,
    Complement,
}
