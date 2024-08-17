#include <iostream>
#include <map>
#include <set>
#include <stack>
#include <string>
#include <vector>

enum TokenType
{
    NUMBER,
    OPERATOR,
    FUNCTION,
    VARIABLE,
    ASSIGNMENT,
    PARENTHESIS,
    END
};

struct Token
{
    TokenType type;
    double value;
    char op;
    std::string func;

    std::string GetPrint()
    {
        switch (type)
        {
        case NUMBER:
            {
                return std::to_string(value);
            }
            
        case OPERATOR:
            {
                return std::string{op};
            }
            
        case FUNCTION:
        case VARIABLE:
        case ASSIGNMENT:
            {
                return func;
            }
            
        case PARENTHESIS:
            {
                return std::string{op};
            }
        }

        return "";
    }
};

std::map<char, int> precedences =
{
    {'+', 1},
    {'-', 1},
    {'*', 2},
    {'/', 2},
    {'^', 3}
};

static const std::set<std::string> functions =
{
    "sin",
    "cos",
    "tan",
    "log",
    "sqrt",
    "exp"
};

Token GetNextToken(const std::string& expression, size_t& index)
{
    while (index < expression.size()
        && isspace(expression[index]))
    {
        index++;
    }

    if (index >= expression.size())
    {
        return {END, 0, 0, ""};
    }

    if (isdigit(expression[index])
        || expression[index] == '.')
    {
        double num = 0;

        int decimals = -1;

        while (index < expression.size()
            && (isdigit(expression[index])
                || expression[index] == '.'))
        {
            if (expression[index] == '.')
            {
                decimals = 0;
            }
            else
            {
                if (decimals == -1)
                {
                    num = num * 10 + (expression[index] - '0');
                }
                else
                {
                    num += (expression[index] - '0') * pow(10, -decimals);

                    decimals++;
                }
            }

            index++;
        }

        return {NUMBER, num, 0, ""};
    }

    if (isalpha(expression[index]))
    {
        std::string name;

        while (index < expression.size()
            && isalnum(expression[index]))
        {
            name += expression[index];

            index++;
        }

        while (index < expression.size()
            && isspace(expression[index]))
        {
            index++;
        }

        if (index < expression.size()
            && expression[index] == '=')
        {
            index++;

            return {ASSIGNMENT, 0, 0, name};
        }

        if (functions.count(name))
        {
            return {FUNCTION, 0, 0, name};
        }

        return {VARIABLE, 0, 0, name};
    }

    if (strchr("+-*/^=()", expression[index]))
    {
        const char op = expression[index];

        index++;

        return {op == '(' || op == ')' ? PARENTHESIS : OPERATOR, 0, op, ""};
    }

    throw std::runtime_error("Unexpected character: " + std::string(1, expression[index]));
}

int GetPrecedence(const char& c)
{
    const std::map<char, int>::iterator it = precedences.find(c);

    if (it != precedences.end())
    {
        return it->second;
    }

    return 0;
}

double GetOperatorResult(const char op, const double first, const double second)
{
    switch (op)
    {
    case '+':
        return first + second;

    case '-':
        return first - second;

    case '*':
        return first * second;

    case '/':
        if (second == 0)
        {
            throw std::runtime_error("Division by zero");
        }

        return first / second;

    case '^':
        return pow(first, second);

    default:
        throw std::runtime_error("Unknown operator");
    }
}

double GetFunctionResult(const std::string& func, double arg)
{
    if (func == "sin")
    {
        return sin(arg);
    }

    if (func == "cos")
    {
        return cos(arg);
    }

    if (func == "tan")
    {
        return tan(arg);
    }

    if (func == "log")
    {
        return log(arg);
    }

    if (func == "sqrt")
    {
        return sqrt(arg);
    }

    if (func == "exp")
    {
        return exp(arg);
    }

    throw std::runtime_error("Unknown function: " + func);
}

double EvaluateExpression(const std::string& expression, std::map<std::string, double>& variables);

std::vector<Token> InfixToPostfix(const std::string& expression, std::map<std::string, double>& variables)
{
    std::stack<Token> stack;

    std::vector<Token> result;

    size_t index = 0;

    while (index < expression.size())
    {
        Token token = GetNextToken(expression, index);

        if (token.type == NUMBER
            || token.type == VARIABLE
            || token.type == FUNCTION)
        {
            result.push_back(token);
        }
        else if (token.type == OPERATOR)
        {
            while (!stack.empty()
                && GetPrecedence(stack.top().op) >= GetPrecedence(token.op))
            {
                result.push_back(stack.top());

                stack.pop();
            }

            stack.push(token);
        }
        else if (token.type == PARENTHESIS)
        {
            if (token.op == '(')
            {
                stack.push(token);
            }
            else if (token.op == ')')
            {
                while (!stack.empty()
                    && stack.top().op != '(')
                {
                    result.push_back(stack.top());

                    stack.pop();
                }

                stack.pop();
            }
        }
        else if (token.type == ASSIGNMENT)
        {
            const std::string varName = token.func;

            const double value = EvaluateExpression(expression.substr(index), variables);

            variables[varName] = value;

            return {Token{NUMBER, value, 0, ""}};
        }
    }

    while (!stack.empty())
    {
        result.push_back(stack.top());

        stack.pop();
    }

    return result;
}

double EvaluatePostfix(std::vector<Token>& postfix, std::map<std::string, double>& variables)
{
    std::stack<double> stack;

    for (Token& token : postfix)
    {
        if (token.type == NUMBER)
        {
            stack.push(token.value);
        }
        else if (token.type == VARIABLE)
        {
            if (variables.find(token.func) != variables.end())
            {
                stack.push(variables[token.func]);
            }
            else
            {
                throw std::runtime_error("Undefined variable: " + token.func);
            }
        }
        else if (token.type == OPERATOR)
        {
            const double top = stack.top();

            stack.pop();

            const double bottom = stack.top();

            stack.pop();

            stack.push(GetOperatorResult(token.op, bottom, top));
        }
        else if (token.type == FUNCTION)
        {
            const double arg = stack.top();

            stack.pop();

            stack.push(GetFunctionResult(token.func, arg));
        }
    }

    return stack.top();
}

double EvaluateExpression(const std::string& expression, std::map<std::string, double>& variables)
{
    std::vector<Token> postfix = InfixToPostfix(expression, variables);

    return EvaluatePostfix(postfix, variables);
}

int main()
{
    std::cout << "Welcome in the scientific calculator.\n" << std::endl;
    std::cout << "Enter an expression to calculate it or 'exit' to quit.\n" << std::endl;

    std::map<std::string, double> variables;
    
    std::string input;

    while (true)
    {
        std::cout << "> ";

        std::getline(std::cin, input);

        if (input == "exit")
        {
            break;
        }

        try
        {
            const double result = EvaluateExpression(input, variables);

            std::cout << "Result : " << result << std::endl;
        }
        catch (const std::exception& ex)
        {
            std::cerr << "Error: " << ex.what() << std::endl;
        }

        std::cin.clear();
        std::cin.sync();
    }

    return 0;
}