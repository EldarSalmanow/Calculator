#include <vector>
#include <map>
#include <any>
#include <iostream>

#include <args.hxx>

#define ASMJIT_STATIC

#include <asmjit/asmjit.h>

#include <GSCrossPlatform/CrossPlatform.h>

[[noreturn]] Void Error(ConstLRef<UString> message) {
    std::cerr << message << std::endl;

    std::exit(1);
}

enum class TokenType {
    LiteralNumber,

    SymbolPlus,
    SymbolMinus,
    SymbolStar,
    SymbolSlash,

    SymbolLParen,
    SymbolRParen,

    EndOfFile,

    Unknown
};

class Token {
public:

    Token(TokenType type,
          UString value)
            : _type(type),
              _value(std::move(value)) {}

public:

    static Token Create(TokenType type,
                        UString value) {
        return Token(type,
                     std::move(value));
    }

    static Token Create(TokenType type) {
        return Token::Create(type,
                             UString());
    }

    static Token Create() {
        return Token::Create(TokenType::Unknown);
    }

public:

    TokenType GetType() const {
        return _type;
    }

    UString GetValue() const {
        return _value;
    }

private:

    TokenType _type;

    UString _value;
};

class Lexer {
public:

    explicit Lexer(UString text)
            : _text(std::move(text)),
              _textIterator(_text.begin()) {}

public:

    static Lexer Create(UString text) {
        return Lexer(std::move(text));
    }

public:

    std::vector<Token> Lex() {
        std::vector<Token> tokens;

        auto token = GetToken();

        while (token.GetType() != TokenType::EndOfFile) {
            tokens.emplace_back(token);

            token = GetToken();
        }

        tokens.emplace_back(token);

        return tokens;
    }

private:

    Token GetToken() {
        while (IsWhitespaceSymbol()) {
            NextSymbol();
        }

        if (IsDigitSymbol()) {
            UString number;

            while (true) {
                auto symbol = CurrentSymbol();

                if (!symbol.IsDigit()) {
                    break;
                }

                number.Append(symbol);

                NextSymbol();
            }

            return Token::Create(TokenType::LiteralNumber, number);
        } else if (IsSpecialSymbol()) {
            auto symbol = CurrentSymbol();

            auto tokenType = TokenType::Unknown;

            if (symbol == '+') {
                tokenType = TokenType::SymbolPlus;
            } else if (symbol == '-') {
                tokenType = TokenType::SymbolMinus;
            } else if (symbol == '*') {
                tokenType = TokenType::SymbolStar;
            } else if (symbol == '/') {
                tokenType = TokenType::SymbolSlash;
            } else if (symbol == '(') {
                tokenType = TokenType::SymbolLParen;
            } else if (symbol == ')') {
                tokenType = TokenType::SymbolRParen;
            }

            NextSymbol();

            return Token::Create(tokenType,
                                 UString({symbol}));
        } else if (IsEnd()) {
            return Token::Create(TokenType::EndOfFile);
        }

        UStringStream stringStream;

        stringStream << "Error: Unknown symbol '"_us
                     << UString({CurrentSymbol()})
                     << "'!"_us;

        Error(stringStream.String());

        NextSymbol();

        return Token::Create();
    }

private:

    Bool IsWhitespaceSymbol() {
        return CurrentSymbol().IsWhitespace();
    }

    Bool IsDigitSymbol() {
        return CurrentSymbol().IsDigit();
    }

    Bool IsSpecialSymbol() {
        auto symbol = CurrentSymbol();

        if (symbol == '+'
         || symbol == '-'
         || symbol == '*'
         || symbol == '/'
         || symbol == '('
         || symbol == ')') {
            return true;
        }

        return false;
    }

private:

    USymbol CurrentSymbol() {
        return *_textIterator;
    }

    Void NextSymbol() {
        ++_textIterator;
    }

    Void PrevSymbol() {
        --_textIterator;
    }

    Bool IsEnd() {
        return _textIterator == _text.end();
    }

private:

    UString _text;

    UString::ConstIterator _textIterator;
};

class Node {
public:

    virtual ~Node() = default;

public:

    virtual Bool IsExpression() const {
        return false;
    }
};

using NodePtr = std::shared_ptr<Node>;

enum class ExpressionType {
    ValueExpression,
    UnaryExpression,
    BinaryExpression
};

class Expression : public Node {
public:

    Bool IsExpression() const override {
        return true;
    }

public:

    virtual ExpressionType GetExpressionType() const = 0;
};

using ExpressionPtr = std::shared_ptr<Expression>;

class ValueExpression : public Expression {
public:

    template<typename T>
    ValueExpression(T value,
                    UString type)
            : _value(std::move(value)),
              _type(std::move(type)) {}

public:

    template<typename T>
    static std::shared_ptr<ValueExpression> Create(T value,
                                                   UString type) {
        return std::make_shared<ValueExpression>(std::move(value),
                                                 std::move(type));
    }

    static std::shared_ptr<ValueExpression> Create(I64 value) {
        return ValueExpression::Create(value,
                                       "I64");
    }

public:

    std::any GetValue() const {
        return _value;
    }

    template<typename T>
    T GetValueWithType() const {
        return std::any_cast<T>(_value);
    }

    UString GetType() const {
        return _type;
    }

public:

    ExpressionType GetExpressionType() const override {
        return ExpressionType::ValueExpression;
    }

private:

    std::any _value;

    UString _type;
};

enum class UnaryOperation {
    Minus
};

class UnaryExpression : public Expression {
public:

    UnaryExpression(UnaryOperation operation,
                    ExpressionPtr expression)
            : _operation(operation),
              _expression(std::move(expression)) {}

public:

    static std::shared_ptr<UnaryExpression> Create(UnaryOperation operation,
                                                   ExpressionPtr expression) {
        return std::make_shared<UnaryExpression>(operation,
                                                 std::move(expression));
    }

public:

    UnaryOperation GetOperation() const {
        return _operation;
    }

    ExpressionPtr GetExpression() const {
        return _expression;
    }

public:

    ExpressionType GetExpressionType() const override {
        return ExpressionType::UnaryExpression;
    }

private:

    UnaryOperation _operation;

    ExpressionPtr _expression;
};

enum class BinaryOperation {
    Plus,
    Minus,
    Star,
    Slash
};

class BinaryExpression : public Expression {
public:

    BinaryExpression(BinaryOperation operation,
                     ExpressionPtr firstExpression,
                     ExpressionPtr secondExpression)
            : _operation(operation),
              _firstExpression(std::move(firstExpression)),
              _secondExpression(std::move(secondExpression)) {}

public:

    static std::shared_ptr<BinaryExpression> Create(BinaryOperation operation,
                                                    ExpressionPtr firstExpression,
                                                    ExpressionPtr secondExpression) {
        return std::make_shared<BinaryExpression>(operation,
                                                  std::move(firstExpression),
                                                  std::move(secondExpression));
    }

public:

    BinaryOperation GetOperation() const {
        return _operation;
    }

    ExpressionPtr GetFirstExpression() const {
        return _firstExpression;
    }

    ExpressionPtr GetSecondExpression() const {
        return _secondExpression;
    }

public:

    ExpressionType GetExpressionType() const override {
        return ExpressionType::BinaryExpression;
    }

private:

    BinaryOperation _operation;

    ExpressionPtr _firstExpression, _secondExpression;
};

std::map<TokenType, I32> OperatorsPrecedence = {
        {TokenType::SymbolStar,  2},
        {TokenType::SymbolSlash, 2},
        {TokenType::SymbolPlus,  1},
        {TokenType::SymbolMinus, 1}
};

class Parser {
public:

    explicit Parser(std::vector<Token> tokens)
            : _tokens(std::move(tokens)),
              _tokensIterator(_tokens.begin()) {}

public:

    static Parser Create(std::vector<Token> tokens) {
        return Parser(std::move(tokens));
    }

public:

    NodePtr Parse() {
        auto expression = ParseExpression();

        if (!IsCurrentTokenType(TokenType::EndOfFile)) {
            Error("Error: Program must be a one expression!");

            return nullptr;
        }

        return expression;
    }

private:

    ExpressionPtr ParseExpression() {
        auto expression = ParseUnaryExpression();

        return ParseBinaryExpression(0, expression);
    }

    ExpressionPtr ParseBinaryExpression(I32 expressionPrecedence, LRef<ExpressionPtr> expression) {
        while (true) {
            auto currentTokenPrecedence = CurrentTokenPrecedence();

            if (currentTokenPrecedence < expressionPrecedence) {
                return expression;
            }

            BinaryOperation binaryOperator;

            switch (CurrentTokenType()) {
                case TokenType::SymbolPlus:
                    binaryOperator = BinaryOperation::Plus;

                    break;
                case TokenType::SymbolMinus:
                    binaryOperator = BinaryOperation::Minus;

                    break;
                case TokenType::SymbolStar:
                    binaryOperator = BinaryOperation::Star;

                    break;
                case TokenType::SymbolSlash:
                    binaryOperator = BinaryOperation::Slash;

                    break;
                default:
                    UStringStream stringStream;

                    stringStream << "Error: Unknown binary operator '"_us
                                 << UString({CurrentTokenValue()})
                                 << "'!"_us;

                    Error(stringStream.String());

                    return nullptr;
            }

            NextToken();

            auto secondExpression = ParseUnaryExpression();

            auto nextTokenPrecedence = CurrentTokenPrecedence();

            if (currentTokenPrecedence < nextTokenPrecedence) {
                secondExpression = ParseBinaryExpression(currentTokenPrecedence + 1,
                                                         secondExpression);
            }

            expression = BinaryExpression::Create(binaryOperator,
                                                  expression,
                                                  secondExpression);
        }
    }

    ExpressionPtr ParseUnaryExpression() {
        if (IsCurrentTokenType(TokenType::SymbolMinus)) {
            NextToken();

            auto expression =  ParseValueExpression();

            return UnaryExpression::Create(UnaryOperation::Minus,
                                           expression);
        }

        return ParseValueExpression();
    }

    ExpressionPtr ParseValueExpression() {
        if (IsCurrentTokenType(TokenType::LiteralNumber)) {
            auto number = std::stoll(CurrentTokenValue().AsUTF8());

            NextToken();

            return ValueExpression::Create(number);
        } else if (IsCurrentTokenType(TokenType::SymbolLParen)) {
            NextToken();

            auto expression = ParseExpression();

            if (!IsCurrentTokenType(TokenType::SymbolRParen)) {
                Error("Error: Missed ')'!");

                return nullptr;
            }

            NextToken();

            return expression;
        }

        return nullptr;
    }

private:

    Token CurrentToken() {
        return *_tokensIterator;
    }

    TokenType CurrentTokenType() {
        return CurrentToken().GetType();
    }

    UString CurrentTokenValue() {
        return CurrentToken().GetValue();
    }

    I32 CurrentTokenPrecedence() {
        auto precedence = OperatorsPrecedence[CurrentTokenType()];

        if (!precedence) {
            return -1;
        }

        return precedence;
    }

    Bool IsCurrentTokenType(TokenType type) {
        return CurrentTokenType() == type;
    }

    Void NextToken() {
        ++_tokensIterator;
    }

    Void PrevToken() {
        --_tokensIterator;
    }

private:

    std::vector<Token> _tokens;

    std::vector<Token>::const_iterator _tokensIterator;
};

class Executor {
public:

    virtual ~Executor() = default;

public:

    virtual I64 Run(ConstLRef<NodePtr> node) = 0;
};

using ExecutorPtr = std::shared_ptr<Executor>;

class Interpreter : public Executor {
public:

    Interpreter() = default;

public:

    static std::shared_ptr<Interpreter> Create() {
        return std::make_shared<Interpreter>();
    }

public:

    I64 Visit(ConstLRef<NodePtr> node) {
        if (node->IsExpression()) {
            return VisitExpression(std::reinterpret_pointer_cast<Expression>(node));
        }

        Error("Error: Unknown node type!");
    }

    I64 VisitExpression(ConstLRef<ExpressionPtr> expression) {
        switch (expression->GetExpressionType()) {
            case ExpressionType::ValueExpression:
                return VisitValueExpression(std::reinterpret_pointer_cast<ValueExpression>(expression));
            case ExpressionType::UnaryExpression:
                return VisitUnaryExpression(std::reinterpret_pointer_cast<UnaryExpression>(expression));
            case ExpressionType::BinaryExpression:
                return VisitBinaryExpression(std::reinterpret_pointer_cast<BinaryExpression>(expression));
        }

        Error("Error: Unknown expression type!");
    }

    I64 VisitValueExpression(ConstLRef<std::shared_ptr<ValueExpression>> valueExpression) {
        if (valueExpression->GetType() == "I64") {
            return valueExpression->GetValueWithType<I64>();
        }

        UStringStream stringStream;

        stringStream << "Error: Unknown '"_us
                     << valueExpression->GetType()
                     << "' value type!"_us;

        Error(stringStream.String());
    }

    I64 VisitUnaryExpression(ConstLRef<std::shared_ptr<UnaryExpression>> unaryExpression) {
        auto expressionResult = VisitExpression(unaryExpression->GetExpression());

        switch (unaryExpression->GetOperation()) {
            case UnaryOperation::Minus:
                return -expressionResult;
        }

        Error("Error: Unknown unary operation type!");
    }

    I64 VisitBinaryExpression(ConstLRef<std::shared_ptr<BinaryExpression>> binaryExpression) {
        auto firstExpressionResult = VisitExpression(binaryExpression->GetFirstExpression());
        auto secondExpressionResult = VisitExpression(binaryExpression->GetSecondExpression());

        switch (binaryExpression->GetOperation()) {
            case BinaryOperation::Plus:
                return firstExpressionResult + secondExpressionResult;
            case BinaryOperation::Minus:
                return firstExpressionResult - secondExpressionResult;
            case BinaryOperation::Star:
                return firstExpressionResult * secondExpressionResult;
            case BinaryOperation::Slash:
                if (secondExpressionResult == 0) {
                    Error("Error: Division by zero!");
                }

                return firstExpressionResult / secondExpressionResult;
        }

        Error("Error: Unknown binary operation type!");
    }

public:

    I64 Run(ConstLRef<NodePtr> node) override {
        return Visit(node);
    }
};

class JitCompilerErrorHandler : public asmjit::ErrorHandler {
public:

    void handleError(asmjit::Error err,
                     const char *message,
                     asmjit::BaseEmitter *origin) override {
        std::cout << "AsmJit error message >> "
                  << message
                  << std::endl;
    }
};

class JitCompiler : public Executor {
public:

    JitCompiler() {
        _runtime = new asmjit::JitRuntime();
        _codeHolder = new asmjit::CodeHolder();

        _codeHolder->init(_runtime->environment());

        _errorHandler = new JitCompilerErrorHandler();

        _codeHolder->setErrorHandler(_errorHandler);

        _assembler = new asmjit::x86::Assembler(_codeHolder);
    }

public:

    ~JitCompiler() {
        delete _errorHandler;
        delete _assembler;
        delete _codeHolder;
        delete _runtime;
    }

public:

    static std::shared_ptr<JitCompiler> Create() {
        return std::make_shared<JitCompiler>();
    }

public:

    Void Visit(ConstLRef<NodePtr> node) {
        if (node->IsExpression()) {
            VisitExpression(std::reinterpret_pointer_cast<Expression>(node));

            return;
        }

        Error("Error: Unknown node type!");
    }

    Void VisitExpression(ConstLRef<ExpressionPtr> expression) {
        switch (expression->GetExpressionType()) {
            case ExpressionType::ValueExpression:
                VisitValueExpression(std::reinterpret_pointer_cast<ValueExpression>(expression));

                return;
            case ExpressionType::UnaryExpression:
                VisitUnaryExpression(std::reinterpret_pointer_cast<UnaryExpression>(expression));

                return;
            case ExpressionType::BinaryExpression:
                VisitBinaryExpression(std::reinterpret_pointer_cast<BinaryExpression>(expression));

                return;
        }

        Error("Error: Unknown expression type!");
    }

    Void VisitValueExpression(ConstLRef<std::shared_ptr<ValueExpression>> valueExpression) {
        if (valueExpression->GetType() == "I64") {
            _assembler->push(valueExpression->GetValueWithType<I64>());

            return;
        }

        UStringStream stringStream;

        stringStream << "Error: Unknown '"_us
                     << valueExpression->GetType()
                     << "' value type!"_us;

        Error(stringStream.String());
    }

    Void VisitUnaryExpression(ConstLRef<std::shared_ptr<UnaryExpression>> unaryExpression) {
        VisitExpression(unaryExpression->GetExpression());

        _assembler->pop(asmjit::x86::rax);

        switch (unaryExpression->GetOperation()) {
            case UnaryOperation::Minus:
                _assembler->neg(asmjit::x86::rax);

                break;
            default:
                Error("Error: Unknown unary operation type!");
        }

        _assembler->push(asmjit::x86::rax);
    }

    Void VisitBinaryExpression(ConstLRef<std::shared_ptr<BinaryExpression>> binaryExpression) {
        VisitExpression(binaryExpression->GetFirstExpression());
        VisitExpression(binaryExpression->GetSecondExpression());

        _assembler->pop(asmjit::x86::rbx);
        _assembler->pop(asmjit::x86::rax);

        switch (binaryExpression->GetOperation()) {
            case BinaryOperation::Plus:
                _assembler->add(asmjit::x86::rax, asmjit::x86::rbx);

                break;
            case BinaryOperation::Minus:
                _assembler->sub(asmjit::x86::rax, asmjit::x86::rbx);

                break;
            case BinaryOperation::Star:
                _assembler->imul(asmjit::x86::rbx);

                break;
            case BinaryOperation::Slash:
                _assembler->cdq();
                _assembler->idiv(asmjit::x86::rbx);

                break;
            default:
                Error("Error: Unknown binary operation type!");
        }

        _assembler->push(asmjit::x86::rax);
    }

public:

    I64 Run(ConstLRef<NodePtr> node) override {
        Visit(node);

        _assembler->pop(asmjit::x86::rax);

        _assembler->ret();

        _assembler->finalize();

        I64 (*function) ();

        auto error = _runtime->add(&function, _codeHolder);

        if (error) {
            Error("Error: Can`t compile code in jit compiler! "_us + asmjit::DebugUtils::errorAsString(error));
        }

        auto result = function();

        _runtime->release(&function);

        return result;
    }

private:

    asmjit::JitRuntime *_runtime;

    asmjit::CodeHolder *_codeHolder;

    asmjit::x86::Assembler *_assembler;

    asmjit::ErrorHandler *_errorHandler;
};

I32 main(I32 argc, Ptr<Ptr<C>> argv) {
    args::ArgumentParser parser("Calculator - simple command line calculator");
    args::HelpFlag helpFlag(parser, "help", "Display help description about Calculator", {'h', "help"});
    args::Flag JITFlag(parser, "JIT", "Is JIT executing strategy", {'j', "jit"});

    try {
        parser.ParseCLI(argc, argv);
    } catch (LRef<args::Help> help) {
        std::cout << parser.Help();

        return 1;
    } catch (LRef<args::Error> error) {
        std::cout << error.what() << std::endl << std::endl << parser.Help();

        return 1;
    }

    while (true) {
        UString code;

        std::cout << "<< ";

        std::cin >> code;

        if (code.Empty()) {
            break;
        }

        auto tokens = Lexer::Create(code).Lex();

        auto node = Parser::Create(tokens).Parse();

        ExecutorPtr executor;

        if (JITFlag.Get()) {
            executor = JitCompiler::Create();
        } else {
            executor = Interpreter::Create();
        }

        auto result = executor->Run(node);

        std::cout << ">> " << result << std::endl;
    }

    return 0;
}
