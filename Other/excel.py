import sys

def parse_exception():
    return Exception("parse error")

def eval_exception():
    return Exception("eval error")

def index_exception():
    return Exception("index out of bounds")

calc_exception = Exception

class stream:
    def __init__(self, str):
        self.__str = str
        self.__pos = 0
    
    def end(self):
        return self.__pos == len(self.__str)

    def next_char(self):
        assert(self.__pos < len(self.__str))
        return self.__str[self.__pos]

    def read_char(self):
        result = self.next_char()
        self.__pos = self.__pos + 1
        return result

    def read_all(self):
        result = self.__str[self.__pos :]
        self.__pos = len(self.__str)
        return result

class empty_value:
    def as_str(self):
        return ''

    def as_int(self):
        return 0

class string_value:
    def __init__(self, s):
        self.__str = s
    
    def as_str(self):
        return self.__str
    
    def as_int(self):
        raise eval_exception()

class number_value:
    def __init__(self, n):
        self.__number = n

    def as_str(self): 
        return str(self.__number)

    def as_int(self):
        return self.__number

def parse_number(stream):
    num_str = ''
    while not stream.end() and stream.next_char().isdigit():
        num_str += stream.read_char()

    if len(num_str) == 0:
        return None

    try:
        return int(num_str)
    except:
        raise parse_exception()

def parse_ref(stream):
    if stream.end():
        raise parse_exception()

    alpha = stream.read_char()
    if    not(alpha >= 'A' and alpha <= 'Z')\
      and not(alpha >= 'a' and alpha <= 'z'):
        raise parse_exception()

    if stream.end():
        raise parse_exception()

    digit = stream.read_char()
    if digit < '0' or digit > '9':
        raise parse_exception()

    col = ord(alpha.lower()) - ord('a')
    row = (ord(digit) - ord('0')) - 1 # 1-based index
    return lambda context: context(col, row)

def parse_term(stream):
    number = parse_number(stream)
    if number != None:
        return lambda context: number_value(number)
    else:
        return parse_ref(stream)

def char_to_op(c):
    ops = { '*': lambda i1, i2: i1 * i2
          , '/': lambda i1, i2: i1 / i2
          , '+': lambda i1, i2: i1 + i2
          , '-': lambda i1, i2: i1 - i2
          }
    try:
        return ops[c]
    except:
        raise parse_exception()

def parse_operation(left, stream):
    op = char_to_op(stream.read_char())
    right = parse_term(stream)
    return lambda context: number_value(op(left(context).as_int(), right(context).as_int()))

def parse_expression(stream):
    result = parse_term(stream)
    while not stream.end():
        result = parse_operation(result, stream)
    return result

def eval_cell(stream, context):
    if stream.end():
        return empty_value()

    result = None
    c = stream.next_char()
    if c == '=':
        stream.read_char()
        result = parse_expression(stream)(context)
    elif c == '\'':
        stream.read_char()
        result = string_value(stream.read_all())
    else:
        number = parse_number(stream)
        if number == None:
            raise parse_exception()
        result = number_value(number)

    if not stream.end():
        raise parse_exception()

    return result

class checked_array:
    def __init__(self, cols, rows):
        self.__cols = cols
        self.__rows = rows
        self.__dict = {}

    def cols(self):
        return self.__cols

    def rows(self):
        return self.__rows

    def get(self, c, r):
        if c >= self.__cols or r >= self.__rows:
            raise index_exception()
        return self.__dict.get(c, {}).get(r)
    def put(self, c, r, v):
        if c >= self.__cols or r >= self.__rows:
            raise index_exception()
        self.__dict.setdefault(c, {})[r] = v

class table:
    class __calc_error_value:
        def __init__(self, err):
            self.__err = err

        def as_str(self):
            raise calc_exception(self.__err)

        def as_int(self):
            raise calc_exception(self.__err)

    def __init__(self, table_data):
        self.__src_data = table_data
        self.__calculated = checked_array(table_data.cols(), table_data.rows())

    def cols(self):
        return self.__src_data.cols()

    def rows(self):
        return self.__src_data.rows()

    def get(self, c, r):
        value = self.__calculated.get(c, r)
        if value == None:
            self.__calculated.put(c, r, table.__calc_error_value("cycle error"))
            value = self.__calc(c, r)
        return value

    def __calc(self, c, r):
        value = None
        try:
            s = self.__src_data.get(c, r)
            value = eval_cell(stream(s), lambda c, r: self.get(c, r))
        except Exception, x:
            value = table.__calc_error_value(x)

        assert(value)
        self.__calculated.put(c, r, value)
        return value

def read_data(lines):
    rows, cols = [int(x) for x in lines[0].rstrip().split('\t')]
    result = checked_array(cols, rows)
    r = 0
    for l in lines[1 :]:
        c = 0
        for s in l.rstrip('\n').split('\t'):
            result.put(c, r, s)
            c = c + 1
        r = r + 1
    return result

def print_table(table):
    for r in range(0, table.rows()):
        for c in range(0, table.cols()):
            if c != 0:
                sys.stdout.write('\t')
            content = None
            try:
                content = table.get(c, r).as_str()
            except Exception, x:
                content = '#' + str(x)
            sys.stdout.write(content)
        sys.stdout.write('\n')

def test():
    def test_context(col, row, value):
        return lambda c, r: value if c == col and r == row else empty_value()

    def test_expression(expression, context, expected):
        result = parse_expression(stream(expression))(context).as_str()
        if result != expected:
            raise Exception("Expression '%s' evaluated to '%s', test failed." % (expression, result))

    def test_expression_error(expression, context):
        try:
            parse_expression(stream(expression))(context)
            raise( "Exception expected.")
        except:
            pass

    def test_cell(content, context, expected):
        result = eval_cell(stream(content), context).as_str()
        if result != expected:
            raise Exception( "Cell '%s' evaluated to '%s', test failed." % (content, result))

    def test_table(table, c, r, expected):
        result = t.get(c, r).as_str()
        if result != expected:
            raise Exception( "Table cell evaluated to '%s', test failed." % result)

    def test_table_error(table, c, r):
        try:
            t.ref(c, r).as_str()
            raise Exception( "Exception expected.")
        except:
            pass

    context = test_context(3, 4, number_value(3))
    test_expression("5", context, "5")
    test_expression("2*3", context, "6")
    test_expression("12/3+1", context, "5")
    test_expression("d5+1*100", context, "400")
    test_expression("a1-3", context, "-3")
    test_expression_error("1/0", context)

    test_cell("'abc", context, "abc")
    test_cell("5", context, "5")
    test_cell("=d5", context, "3")
    
    data = checked_array(3, 3)
    data.put(0, 0, "'test")
    data.put(0, 1, "=a1")
    data.put(0, 2, "=a2")
    data.put(1, 0, "=b2")
    data.put(1, 1, "=b1")
    data.put(2, 0, "=z9")
    t = table(data)
    test_table(t, 0, 2, "test")
    test_table_error(t, 1, 0)
    test_table_error(t, 2, 0)

    lines = [
          "3\t4\n"
        , "12\t=C2\t3\'Sample\n"
        , "=A1+B1*C1/5\t=A2*B1\t=B3-C3\t'Spread\n"
        , "'Test\t=4-3\t5\t'Sheet"
        ]
    data = read_data(lines)
    assert(data.rows() == 3)
    assert(data.cols() == 4)
    assert(data.get(0, 0) == "12")
    assert(data.get(3, 2) == "'Sheet")

if __name__ == "__main__":
    test()
        
    t = table(read_data(sys.stdin.readlines()))
    print_table(t)
