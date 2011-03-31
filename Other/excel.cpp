#include <algorithm>
#include <assert.h>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <strstream>
#include <vector>

struct i_value
{
    virtual ~i_value(){}
    virtual std::string as_str() = 0;
    virtual int as_int() = 0;
};

struct i_context
{
    virtual ~i_context(){}
    virtual i_value& ref(int col, int row) = 0;
};

struct i_expression
{
    virtual ~i_expression(){}
    virtual std::auto_ptr<i_value> eval(i_context&) = 0;
};

class parse_exception : public std::runtime_error 
{
public:
    parse_exception() : std::runtime_error("parse error") {}
};

class eval_exception : public std::runtime_error 
{
public:
    eval_exception() : std::runtime_error("eval error") {}
};

class calc_exception : public std::runtime_error 
{
public:
    calc_exception(std::string const& err) : std::runtime_error(err) {}
};

class stream
{
public:
    stream(std::string const& s) : str(s), pos(0) {}
    bool end() const { return this->pos == this->str.size(); }
    char next_char() const 
    { 
        assert( this->pos < this->str.size() );
        return this->str[this->pos];
    }
    char read_char() 
    { 
    char result = this->next_char();
        ++this->pos;
        return result; 
    }
    std::string read_all()
    {
    std::string result = this->str.substr(this->pos);
        this->pos = this->str.size();
        return result;
    }
private:
    std::string str;
    std::string::size_type pos;
};

struct empty_value : i_value
{
    virtual std::string as_str() { return std::string(); }
    virtual int as_int() { return 0; }
};

class string_value : public i_value
{
public:
    string_value(std::string const& s) : str(s) {}
    virtual std::string as_str() { return this->str; }
    virtual int as_int() { throw eval_exception(); }
private:
    std::string str;
};

class number_value : public i_value
{
public:
    number_value(int n) : number(n) {}
    virtual std::string as_str() 
    { 
    std::ostrstream s;
        s << this->number;
        return std::string(s.str(), size_t(s.pcount())); 
    }
    virtual int as_int() { return this->number; }
private:
    int number;
};

class value_proxy : public i_value
{
public:
    value_proxy(i_value& impl) : impl(impl) {}
    virtual std::string as_str() { return this->impl.as_str(); }
    virtual int as_int() { return this->impl.as_int(); }
private:
    i_value& impl;
};

class number_expression : public i_expression
{
public:
    number_expression(int n) : number(n) {}
    virtual std::auto_ptr<i_value> eval(i_context&) { return std::auto_ptr<i_value>( new number_value( this->number )); }
private:
    int number;    
};

class ref_expression : public i_expression
{
public:
    ref_expression(int col, int row) : col(col), row(row) {}
    virtual std::auto_ptr<i_value> eval(i_context& c) 
    { 
        return std::auto_ptr<i_value>(new value_proxy(c.ref(col, row))); 
    }
private:
    int col;
    int row;
};

bool
parse_number(stream& s, int& result)
{
std::string str;
    while (!s.end() && ::isdigit(s.next_char()))
        str.insert(str.end(), s.read_char());
    if (str.empty())            
        return false;
std::istrstream is(str.c_str());
    is >> result;
    if (!is)
        throw parse_exception();
    return true;
}

std::auto_ptr<i_expression>
parse_ref(stream& s)
{
    if (s.end())
        throw parse_exception();
char alpha = s.read_char();
    if (!(alpha >= 'A' && alpha <= 'Z')
     && !(alpha >= 'a' && alpha <= 'z'))
        throw parse_exception();
    if (s.end())
        throw parse_exception();
char digit = s.read_char();
    if (digit < '0' || digit > '9')
        throw parse_exception();

int col = ::tolower(alpha) - 'a';
int row = (digit - '0') - 1; // 1-based index
    return std::auto_ptr<i_expression>(new ref_expression(col, row));
}

std::auto_ptr<i_expression>
parse_term(stream& s)
{
int number = 0;
    if (parse_number(s, number))
        return std::auto_ptr<i_expression>(new number_expression(number));
    else
        return parse_ref(s);
}

class operation_expression : public i_expression
{
public:
    typedef int (*op)(int,int);
    operation_expression(std::auto_ptr<i_expression> left, std::auto_ptr<i_expression> right, op o)
        : left(left), right(right), o(o) 
    {
        assert(this->left.get());
        assert(this->right.get());
        assert(this->o);
    }
    virtual std::auto_ptr<i_value> eval(i_context& c) 
    {
    std::auto_ptr<i_value> l = this->left->eval(c);
    std::auto_ptr<i_value> r = this->right->eval(c);
    return std::auto_ptr<i_value>(new number_value(this->o(l->as_int(), r->as_int())));
    }
private:
    std::auto_ptr<i_expression> left;
    std::auto_ptr<i_expression> right;
    op o;
};

int multiple(int i1, int i2) { return i1 * i2; }

int divide(int i1, int i2) 
{ 
    if (!i2) 
        throw calc_exception("divide by zero"); 
    return i1 / i2;
}

int plus(int i1, int i2) { return i1 + i2; }

int minus(int i1, int i2) { return i1 - i2; }

operation_expression::op
char_to_op(char c)
{
    switch (c)
        {
        case '*':
            return &multiple;
        case '/':
            return &divide;
        case '+':
            return &plus;
        case '-':
            return &minus;
        }
    throw parse_exception();
}

std::auto_ptr<i_expression>
parse_operation(std::auto_ptr<i_expression> left, stream& s)
{
operation_expression::op op = char_to_op(s.read_char());
    return std::auto_ptr<i_expression>(new operation_expression(left, parse_term(s), op));
}

std::auto_ptr<i_expression>
parse_expression(stream& s)
{
std::auto_ptr<i_expression> result = parse_term(s);
    while (!s.end())
        result = parse_operation(result, s);
    return result;
}

std::auto_ptr<i_value>
eval_cell(stream& s, i_context& context)
{
    if (s.end())
        return std::auto_ptr<i_value>(new empty_value());

std::auto_ptr<i_value> result;
    switch (s.next_char())
    {
    case '=':
        s.read_char();
        result = parse_expression(s)->eval(context);
        break;
    case '\'':
        s.read_char();
        result = std::auto_ptr<i_value>(new string_value(s.read_all()));
        break;
    default:
        int number = 0;
        if (!parse_number(s, number))
            throw parse_exception();
        result = std::auto_ptr<i_value>(new number_value(number));
        }

    if (!s.end())
        throw parse_exception();

    return result;
}

template <typename T>
class checked_array
{
public:
    checked_array(size_t cols, size_t rows) : data(cols, std::vector<T>(rows)){}
    size_t cols() const { return this->data.size(); }
    size_t rows() const { return this->data.empty() ? 0 : this->data.front().size(); }
    T &cell(size_t c, size_t r)
    {
        if (c >= this->cols() || r >= this->rows())
            throw std::runtime_error("index out of bounds");
        return this->data[c][r];
    }
private:
    std::vector< std::vector<T> > data;
};

typedef checked_array<std::string> table_data;

class table : public i_context
{
public:
    table(table_data const& data) 
        : src_data(data)
        , calculated(data.cols(), data.rows())
        , cycle_error_value("cycle error")
        {}
    ~table()
    {
        for(size_t c = 0; c < this->calculated.cols(); ++c)
            for(size_t r = 0; r < this->calculated.rows(); ++r)
                this->reset_cell(NULL, c, r);
    }
    virtual i_value& ref(int c, int r) 
    {
    i_value* v = this->calculated.cell(c, r);
        if (!v)
        {
            this->reset_cell(&this->cycle_error_value, c, r);
            v = &this->calc(c, r);
        }
        return *v;
    }
    size_t cols() const { return this->src_data.cols(); }
    size_t rows() const { return this->src_data.rows(); }
private:
    class calc_error : public i_value
    {
    public:
        calc_error(std::string const& err) : err(err) {}
        virtual std::string as_str() {throw calc_exception(this->err); }
        virtual int as_int() {throw calc_exception(this->err); }
    private:
        std::string err;
    };
private:
    i_value& calc(int c, int r)
    {
        try
        {
        std::string str = this->src_data.cell(c, r);
        stream s(str);
        std::auto_ptr<i_value> v = eval_cell(s, *this);
            assert(v.get());
            return *this->reset_cell(v.release(), c, r);
        }
        catch (std::exception const& x)
        {
            return *this->reset_cell(new calc_error(x.what()), c, r);
        }
    }
    i_value* reset_cell(i_value* new_cell, int c, int r)
    {
    i_value*& v = this->calculated.cell(c, r);
        if (v != &this->cycle_error_value)
            delete v;
        v = new_cell;
        return v;
    }
private:
    table_data src_data;
    checked_array<i_value*> calculated;
    calc_error cycle_error_value;
};

std::string
read_cell(std::string const& row, std::string::const_iterator& from)
{
std::string::const_iterator next = std::find(from, row.end(), '\t');
std::string result(from, next);
    from = next;
    if (from != row.end())
        ++from;
    return result;
}

table_data
read_data(std::istream& s)
{
std::string line;
    std::getline(s, line);
std::istrstream cols_rows(line.c_str());
int rows = 0;
int cols = 0;
    cols_rows >> rows >> cols;

table_data result(cols, rows);
    for(size_t r = 0; r < result.rows(); ++r)
    {
        std::getline(s, line);
    std::string::const_iterator from = line.begin();
        for(size_t c = 0; c < result.cols(); ++c)
            result.cell(c, r) = read_cell(line, from);
    }
    return result;
}

void
print_table(table& t)
{
    for(size_t r = 0; r < t.rows(); ++r)
    {
        for(size_t c = 0; c < t.cols(); ++c)
            {
            if (c)
                std::cout << "\t";
            std::string content;
                try 
                {
                    content = t.ref(c, r).as_str();
                }
                catch(std::exception const& x) 
                {
                    content = std::string("<") + x.what() + ">";
                }
            std::cout << content;
            }
        std::cout << std::endl;
    }
}

namespace test {

class test_context : public i_context
{
public:
    test_context(int col, int row, std::auto_ptr<i_value> value) 
        : col(col), row(row), value(value), empty(new empty_value) 
    {
        assert(this->value.get());
    }
    virtual i_value& ref(int c, int r) { return (c == this->col && r == this->row) ? *this->value : *this->empty; }
private:
    int col;
    int row;
    std::auto_ptr<i_value> value;    
    std::auto_ptr<i_value> empty;
};

void
test_expression(std::string const& e, i_context& context, std::string const& expected)
{
stream s(e);
std::string result = parse_expression(s)->eval(context)->as_str();
    if (result != expected)
        throw std::runtime_error( "Expression '" + e + " evaluated to '" + result + "', test failed.");
}

void
test_expression_error(std::string const& e, i_context& context)
{
stream s(e);
    try 
    {
        parse_expression(s)->eval(context);
        throw std::runtime_error( "Exception expected.");
    }
    catch (std::exception const&)
    {
    }
}

void
test_cell(std::string const& content, i_context& context, std::string const& expected)
{
stream s(content);
std::string result = eval_cell(s, context)->as_str();
    if (result != expected)
        throw std::runtime_error( "Cell '" + content + " evaluated to '" + result + "', test failed.");
}

void
test_table(table& t, int c, int r, std::string const& expected)
{
std::string result = t.ref(c, r).as_str();
    if (result != expected)
        throw std::runtime_error( "Table cell evaluated to '" + result + "', test failed.");
}

void
test_table_error(table& t, int c, int r)
{
    try 
    {
        t.ref(c, r).as_str();
        throw std::runtime_error( "Exception expected.");
    }
    catch (std::exception const&)
    {
    }
}

void check(bool b) { if (!b) throw std::runtime_error( "test check failed" ); }

void
test()
{
test_context context(3, 4, std::auto_ptr<i_value>(new number_value(3)));
    test_expression("5", context, "5");
    test_expression("2*3", context, "6");
    test_expression("12/3+1", context, "5");
    test_expression("d5+1*100", context, "400");
    test_expression("a1-3", context, "-3");
    test_expression_error("1/0", context);

    test_cell("'abc", context, "abc");
    test_cell("5", context, "5");
    test_cell("=d5", context, "3");
    
table_data data(3, 3);
    data.cell(0, 0) = "'test";
    data.cell(0, 1) = "=a1";
    data.cell(0, 2) = "=a2";
    data.cell(1, 0) = "=b2";
    data.cell(1, 1) = "=b1";
    data.cell(2, 0) = "=z9";
table t(data);
    test_table(t, 0, 2, "test");
    test_table_error(t, 1, 0);
    test_table_error(t, 2, 0);

std::istrstream is(
    "3\t4\n"
    "12\t=C2\t3\'Sample\n"
    "=A1+B1*C1/5\t=A2*B1\t=B3-C3\t'Spread\n"
    "'Test\t=4-3\t5\t'Sheet"
    );
    data = read_data(is);
    check(data.rows() == 3);
    check(data.cols() == 4);
    check(data.cell(0, 0) == "12");
    check(data.cell(3, 2) == "'Sheet");
}

} // test

int 
main()
{
    try 
    {
        test::test();
        
        table t(read_data(std::cin));
        print_table(t);
    }
    catch (std::exception const& ex)
    {
        std::cerr << ex.what() << std::endl;
        return -1;
    }
    return 0;
}