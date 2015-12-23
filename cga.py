#!/usr/bin/env python

from spark import GenericScanner, GenericParser, GenericASTTraversal
import os

class AST(object):
    def __init__(self, token=None, type=None, value=None, children=None):
        if token:
            self.type = token.type
            self.value = token.value
        else:
            self.type = type
            self.value = value
        if children:
            self.children = children
        else:
            self.children = []
    def __getitem__(self, i):
        return self.children[i]
    def print_tree(self, depth):
        ret = '  ' * depth + self.type
        if self.value != None:
            ret += '(' + self.value + ')'
        ret += '\n'
        for child in self.children:
            ret += child.print_tree(depth + 1)
        return ret
    def __str__(self):
        return self.print_tree(0)
    def __repr__(self):
        return self.__str__()

class Token(object):
    def __init__(self, type, value=None):
        self.type = type
        self.value = value

    def __cmp__(self, o):
        return cmp(self.type, o)

    def __str__(self):
        ret = self.type
        if self.value != None:
            ret += '(' + self.value + ')'
        return ret
    def __repr__(self):
        return self.__str__()

# Lexer	
class Scanner(GenericScanner):
    def __init__(self):
        GenericScanner.__init__(self)

    def tokenize(self, input):
        self.rv = []
        GenericScanner.tokenize(self, input)
        self.handle_keywords()
        return self.rv

    def handle_keywords(self):
        keywords = {
            'True'    : ('bool',        'True'),
            'False'   : ('bool',        'False'),
            'attr'    : ('attr_tok',    None),
            'comp'    : ('comp_tok',    None),
            'case'    : ('case_tok',    None),
            'else'    : ('else',        None),
            'split'   : ('split_tok',   None),
            'version' : ('version_tok', None),
            'const'   : ('const_tok',   None)
        }
        for tok in self.rv:
            if tok.type == 'name' and tok.value in keywords:
                tok.type = keywords[tok.value][0]
                tok.value = keywords[tok.value][1]

    def t_whitespace(self, s):
        r'\s+'
        #self.rv.append(Token(type='whitespace', value=s))
        pass

    def t_symbol(self, s):
        r"~|'|\.|:|\|\||\||,|\(|\)|{|}|\[|\]|-->|\*|\/|%|\+|\-|==|<=|>=|<|>|=|!=|!|&&"
        self.rv.append(Token(type=s))

    def t_name(self, s):
        r'[a-zA-Z]+[\_.a-zA-Z0-9]*'
        self.rv.append(Token(type='name', value=s))

    def t_annotation(self, s):
        r'@[a-zA-Z]+[\_a-zA-Z0-9]*'
        self.rv.append(Token(type='anno_tok', value=s))

    # Integers are not valid CGA types, we don't care, handle them separately
    # nontheless.
    def t_int(self, s):
        r'\d+'
        self.rv.append(Token(type='int', value=s))

    def t_string(self, s):
        r'"[^"]*"'
        self.rv.append(Token(type='string', value=s))

class SubScanner(Scanner):
    def __init__(self):
        Scanner.__init__(self)

    def t_float(self, s):
        r'\d+\.\d+'
        self.rv.append(Token(type='float', value=s))

    def t_handle(self, s):  # TODO(whoever): Handle parsing is missing.
        r'@Handle(.+)'
        self.rv.append(Token(type='handle', value=s))

    def t_comment(self, s):
        r'\/\*(.|\r|\n|\r\n|)*?\*\/|\/\/.*|\#.*'
        #self.rv.append(Token(type='comment', value=s))
        pass

class CGAScanner(SubScanner):
    def __init__(self):
        SubScanner.__init__(self)

    def t_probability(self, s):
        r'\d+(\.\d+)?%'
        self.rv.append(Token(type='prob_tok', value=s))

# Parser
class CGAParser(GenericParser):
    def __init__(self, start='cga'):
        GenericParser.__init__(self, start)

    def p_cga_1(self, args):
        ' cga ::= stmt_list '
        return AST(type='cga', children=args[0])
    def p_cga_2(self, args):
        ' cga ::= version_tok string stmt_list '
        return AST(type='cga', value=args[1].value, children=args[2])

    def p_stmt_list_1(self, args):
        ' stmt_list ::= anno_stmt stmt_list '
        return [args[0]] + args[1]
    def p_stmt_list_2(self, args):
        ' stmt_list ::= '
        return []

    def p_anno_stmt(self, args):
        ' anno_stmt ::= anno_list_parent stmt '
        return AST(type='anno_stmt', children=[args[0], args[1]])
    def p_anno_stmt_2(self, args):
        ' anno_stmt ::= stmt '
        return AST(type='anno_stmt', children=[args[0]])

    def p_anno_list_parent(self, args):
        ' anno_list_parent ::= anno_list '
        return AST(type='annotations', children=args[0])
    def p_anno_list_1(self, args):
        ' anno_list ::= anno anno_list '
        return [AST(token=args[0])] + args[1]
    def p_anno_list_2(self, args):
        ' anno_list ::= anno '
        return [AST(token=args[0])]

    def p_anno_1(self, args):
        ' anno ::= anno_tok '
        return AST(type='anno', value=args[0].value)
    def p_anno_2(self, args):
        ' anno ::= anno_tok ( anno_param_list ) '
        return AST(type='anno', value=args[0].value, children=args[2])
    def p_anno_3(self, args):  # TODO(whoever): Handle parsing is missing.
        ' anno ::= handle '
        return AST(type='handle', value=args[0].value[8:-2])

    def p_anno_param_list_1(self, args):
        ' anno_param_list ::= anno_param , anno_param_list '
        return [AST(token=args[0])] + args[2]
    def p_anno_param_list_2(self, args):
        ' anno_param_list ::= anno_param '
        return [AST(token=args[0])]

    def p_anno_param_1(self, args):
        ' anno_param ::= expr '
        return AST(type='anno_param', children=[args[0]])
    def p_anno_param_2(self, args):
        ' anno_param ::= name = expr '
        return AST(type='anno_param', children=[args[0], args[2]])

    def p_stmt(self, args):
        '''
            stmt ::= attr
            stmt ::= glob_var
            stmt ::= const
            stmt ::= func
            stmt ::= rule
        '''
        return args[0]

    def p_attr(self, args):
        ' attr ::= attr_tok name = extended_expr '
        return AST(type='attr', value=args[1].value, children=[args[3]])

    def p_var(self, args):
        ' glob_var ::= name = extended_expr '
        return AST(type='global_variable', value=args[0].value, children=[args[2]])

    def p_const(self, args):
        ' const ::= const_tok name = extended_expr '
        return AST(type='const', value=args[1].value, children=[args[3]])

    def p_func(self, args):
        ' func ::= name ( arguments ) = extended_expr '
        return AST(type='function', value=args[0].value, children=[args[2], args[5]])

    def p_extended_expr(self, args):
        '''
            extended_expr ::= expr
            extended_expr ::= expr_case_list_parent
            extended_expr ::= expr_prob_list_parent
        '''
        return args[0]

    def p_expr_case_list_parent(self, args):
        ' expr_case_list_parent ::= expr_case_list '
        return AST(type='conditional_expr', children=args[0])
    def p_expr_case_list_1(self, args):
        ' expr_case_list ::= case_tok expr_case expr_case_list '
        return [args[1]] + args[2]
    def p_expr_case_list_2(self, args):
        ' expr_case_list ::= else : expr '
        return [AST(type='else', children=[args[2]])]
    def p_expr_case(self, args):
        ' expr_case ::= expr : extended_expr '
        return AST(type='case', children=[args[0], args[2]])

    def p_expr_prob_list_parent(self, args):
        ' expr_prob_list_parent ::= expr_prob_list '
        return AST(type='stochastic_expr', children=args[0])
    def p_expr_prob_list_1(self, args):
        ' expr_prob_list ::= expr_prob expr_prob_list '
        return [args[0]] + args[1]
    def p_expr_prob_list_2(self, args):
        ' expr_prob_list ::= else : expr '
        return [AST(type='else', children=[args[2]])]
    def p_expr_prob(self, args):
        ' expr_prob ::= prob_tok : extended_expr '
        return AST(type='probability', children=[AST(token=args[0]), args[2]])

    def p_rule_1(self, args):
        ' rule ::= name --> successor '
        return AST(type='rule', value=args[0].value, children=[args[2]])
    def p_rule_2(self, args):
        ' rule ::= name ( arguments ) --> successor '
        return AST(type='rule', value=args[0].value, children=[args[2], args[5]])

    def p_arguments(self, args):
        ' arguments ::= arg_list '
        return AST(type='arguments', children=args[0])

    def p_arg_list_1(self, args):
        ' arg_list ::= name , arg_list '
        return [AST(token=args[0])] + args[2]
    def p_arg_list_2(self, args):
        ' arg_list ::= name '
        return [AST(token=args[0])]

    def p_successor_1(self, args):
        ' successor ::= inst_list '
        return AST(type='successor', children=args[0])
    def p_successor_2(self, args):
        ' successor ::= case_list_parent '
        return AST(type='successor', children=[args[0]])
    def p_successor_3(self, args):
        ' successor ::= prob_list_parent '
        return AST(type='successor', children=[args[0]])

    def p_case_list_parent(self, args):
        ' case_list_parent ::= case_list '
        return AST(type='conditional', children=args[0])
    def p_case_list_1(self, args):
        ' case_list ::= case_tok case case_list '
        return [args[1]] + args[2]
    def p_case_list_2(self, args):
        ' case_list ::= else : successor '
        return [AST(type='else', children=[args[2]])]
    def p_case(self, args):
        ' case ::= expr : successor '
        return AST(type='case', children=[args[0], args[2]])

    def p_prob_list_parent(self, args):
        ' prob_list_parent ::= prob_list '
        return AST(type='stochastic', children=args[0])
    def p_prob_list_1(self, args):
        ' prob_list ::= prob prob_list '
        return [args[0]] + args[1]
    def p_prob_list_2(self, args):
        ' prob_list ::= else : successor '
        return [AST(type='else', children=[args[2]])]
    def p_prob(self, args):
        ' prob ::= prob_tok : successor '
        return AST(type='probability', children=[AST(token=args[0]), args[2]])


    def p_inst_list_1(self, args):
        ' inst_list ::= inst inst_list '
        return [args[0]] + args[1]
    def p_inst_list_2(self, args):
        ' inst_list ::= inst '
        return [args[0]]

    def p_inst_1(self, args):
        '''
            inst ::= symbol
            inst ::= symbol ( )
        '''
        return AST(type='symbol', value=args[0])
    def p_inst_2(self, args):
        ' inst ::= symbol ( param_list ) '
        return AST(type='symbol', value=args[0], children=args[2])
    def p_inst_3(self, args):
        ' inst ::= split_tok ( name ) sub_split '
        return AST(type='split', value=args[2].value, children=[args[4]])
    def p_inst_4(self, args):
        ' inst ::= split_tok ( name , param_list ) sub_split '
        return AST(type='split', value=args[2].value, children=[AST(type='paramters', children=args[4]), args[6]])
    def p_inst_5(self, args):
        ' inst ::= comp_tok ( name ) { comp_list } '
        return AST(type='comp_split', value=args[2].value, children=args[5])

    def p_symbol_1(self, args):
        '''
            symbol ::= [
            symbol ::= ]
        '''
        return args[0].type
    def p_symbol_2(self, args):
        ' symbol ::= name '
        return args[0].value

    def p_comp_list_1(self, args):
        ' comp_list ::= comp | comp_list '
        return [args[0]] + args[2]
    def p_comp_list_2(self, args):
        ' comp_list ::= comp '
        return [args[0]]
    def p_comp(self, args):
        '''
            comp ::= expr : successor
            comp ::= expr = successor
        '''
        return AST(type='comp', value=args[1].type, children=[args[0], args[2]])

    def p_sub_split_1(self, args):
        ' sub_split ::= { split_list } '
        return AST(type='split_list', children=args[1])
    def p_sub_split_2(self, args):
        ' sub_split ::= { split_list } * '
        return AST(type='split_list', value=args[3].type, children=args[1])
    def p_split_list_1(self, args):
        ' split_list ::= split_pair | split_list '
        return [args[0]] + args[2]
    def p_split_list_2(self, args):
        ' split_list ::= split_pair '
        return [args[0]]
    def p_split_pair_1(self, args):
        ' split_pair ::= param : successor '
        return AST(type='split_pair', children=[args[0], args[2]])
    def p_split_pair_2(self, args):
        ' split_pair ::= sub_split '
        return args[0]

    def p_var_name_1(self, args):
        '''
            var_name ::= name
            var_name ::= name ( )
        '''
        return AST(type='variable', value=args[0].value)
    def p_var_name_2(self, args):
        ' var_name ::= name ( param_list ) '
        return AST(type='variable', value=args[0].value, children=args[2])

    def p_param_list_1(self, args):
        ' param_list ::= param , param_list '
        return [args[0]] + args[2]
    def p_param_list_2(self, args):
        ' param_list ::= param '
        return [args[0]]

    def p_param_1(self, args):
        ' param ::= expr '
        return AST(type='parameter', children=[args[0]])
    def p_param_2(self, args):
        """
            param ::= ~ expr
            param ::= ' expr
        """
        return AST(type='parameter', value=args[0].type, children=[args[1]])

    def p_expr_1(self, args):
        ' expr ::= join || expr '
        return AST(type='op', value=args[1].type, children=[args[0], args[2]])
    def p_expr_2(self, args):
        ' expr ::= join '
        return args[0]

    def p_join_1(self, args):
        ' join ::= equality && join '
        return AST(type='op', value=args[1].type, children=[args[0], args[2]])
    def p_join_2(self, args):
        ' join ::= equality '
        return args[0]

    def p_equality_1(self, args):
        '''
            equality ::= rel == equality
            equality ::= rel != equality
        '''
        return AST(type='op', value=args[1].type, children=[args[0], args[2]])
    def p_equality_2(self, args):
        ' equality ::= rel '
        return args[0]

    def p_rel_1(self, args):
        '''
            rel ::= arithmetic_expr <  arithmetic_expr
            rel ::= arithmetic_expr <= arithmetic_expr
            rel ::= arithmetic_expr >= arithmetic_expr
            rel ::= arithmetic_expr >  arithmetic_expr
        '''
        return AST(type='op', value=args[1].type, children=[args[0], args[2]])
    def p_rel_2(self, args):
        ' rel ::= arithmetic_expr '
        return args[0]

    def p_arithmetic_expr_1(self, args):
        '''
            arithmetic_expr ::= term + arithmetic_expr
            arithmetic_expr ::= term - arithmetic_expr
        '''
        return AST(type='op', value=args[1].type, children=[args[0], args[2]])
    def p_arithmetic_expr_2(self, args):
        ' arithmetic_expr ::= term '
        return args[0]

    def p_term_1(self, args):
        '''
            term ::= unary * term
            term ::= unary / term
            term ::= unary % term
        '''
        return AST(type='op', value=args[1].type, children=[args[0], args[2]])
    def p_term_2(self, args):
        ' term ::= unary '
        return args[0]

    def p_unary_1(self, args):
        '''
            unary ::= - factor
            unary ::= ! factor
        '''
        return AST(type='op', value=args[0].type, children=[args[1]])
    def p_unary_2(self, args):
        ' unary ::= factor '
        return args[0]

    def p_factor(self, args):
        '''
            factor ::= int
            factor ::= float
            factor ::= string
            factor ::= bool
        '''
        return AST(token=args[0])
    def p_factor_2(self, args):
        ' factor ::= var_name '
        return args[0]
    def p_factor_3(self, args):
        ' factor ::= ( expr ) '
        return AST(type='bracket_expr', children=[args[1]])

# Grammar classes
class Expr(object):  # TODO(whoever): Is this necessary or even useful?
    pass
class ValueExpr(Expr):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        if type(self.value) == float:
            return "%.2f" % self.value
        elif type(self.value) == int or type(self.value) == bool:
            return str(self.value)
        else:
            return self.value
class OpExpr(Expr):
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right
    def __str__(self):
        if self.left:
            return str(self.left) + str(self.op) + str(self.right)
        else:
            return str(self.op) + str(self.right)
class BracketExpr(Expr):
    def __init__(self, expr):
        self.expr = expr
    def __str__(self):
        return '(' + str(self.expr) + ')'
class NameExpr(Expr):
    def __init__(self, name, params=None):
        self.name = name
        self.params = params
    def __str__(self):
        ret = self.name
        if self.params:
            ret += '(' + ', '.join(map(lambda x: str(x), self.params)) + ')'
        return ret
class CaseExpr(Expr):
    def __init__(self, conditions, exprs, default):
        self.conditions = conditions
        self.exprs = exprs
        self.default = default
    def __str__(self):
        ret = ""
        for (cond, expr) in zip(conditions, exprs):
            ret += "case " + str(expr) + " : " + str(succ) + '\n'
        ret += "else : " + str(default)
        return ret
class ProbExpr(Expr):
    def __init__(self, probabilities, exprs, default):
        self.probabilietes = probabilities
        self.exprs = exprs
        self.default = default
    def __str__(self):
        ret = ""
        for (prob, expr) in zip(probabilities, exprs):
            ret += ("%.2f" % prob * 100) + "% : " + str(expr) + '\n'
        ret += "else : " + str(default)
        return ret
class Parameter(Expr):
    def __init__(self, expr, modifier=''):
        self.expr = expr
        self.modifier = modifier
    def __str__(self):
        return self.modifier + str(self.expr)

class Variable(object):
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr
    def __str__(self):
        return self.name + ' = ' + str(self.expr)
class Const(Variable):
    def __init__(self, name, expr):
        Variable.__init__(self, name, expr)
    def __str__(self):
        return 'const ' + Variable.__str__(self)
class Attr(Variable):
    def __init__(self, name, expr):
        Variable.__init__(self, name, expr)
        self.annotations = {}
    def __str__(self):
        return 'attr ' + Variable.__str__(self)
class Function(Variable):
    def __init__(self, name, args, expr):
        Variable.__init__(self, name, expr)
        self.args = args
    def __str__(self):
        ret = self.name
        if self.args:
            ret += str(self.args)
        ret += ' = ' + str(self.expr)
        return ret

class Rule(object):
    def __init__(self, name, args, successor):
        self.name = name
        self.args = args
        self.successor = successor
    def __str__(self):
        ret = str(self.name)
        if self.args:
            ret += '(' + ', '.join(self.args) + ')'
        ret +=  " --> " + str(self.successor)
        return ret
class Symbol(object):
    def __init__(self, name, params):
        self.name = name
        self.params = params
    def __str__(self):
        if self.params:
            return self.name + '(' + ', '.join(map(lambda x: str(x), self.params)) + ')'
        else:
            return self.name
class Instruction(Symbol):
    def __init__(self, name, params):
        Symbol.__init__(self, name, params)
class ConditionalRule(object):
    def __init__(self, conditions, successors, default):
        self.conditions = conditions
        self.successors = successors
        self.default = default
    def __str__(self):
        ret = ""
        for (cond, succ) in zip(self.conditions, self.successors):
            ret += "case " + str(cond) + " : " + str(succ) + ' '
        ret += "else : " + str(self.default)
        return ret
class StochasticRule(object):
    def __init__(self, probabilities, successors, default):
        self.probabilities = probabilities
        self.successors = successors
        self.default = default
    def __str__(self):
        ret = ""
        for (prob, succ) in zip(self.probabilities, self.successors):
            ret += ("%.2f" % prob.value) + "% : " + str(succ) + ' '
        ret += "else : " + str(self.default)
        return ret
class Split(object):
    def __init__(self, expr, assignment, successor):
        self.expr = expr
        self.assignment = assignment
        self.successor = successor
    def __str__(self):
        return str(self.expr) + ' ' + self.assignment + ' ' + str(self.successor)
class NestedSplit(object):
    def __init__(self, splits, repeated):
        self.splits = splits
        self.repeated = repeated
    def __str__(self):
        ret = "{ "
        ret += " | ".join(map(lambda x: str(x), self.splits))
        ret += " }"
        if self.repeated:
            ret += '*'
        return ret
class SubdivSplit(object):
    def __init__(self, direction, params, nested_split):
        self.direction = direction
        self.params = params
        self.nested_split = nested_split
    def __str__(self):
        ret = "split(" + self.direction
        if self.params:
            ret += ", " + ', '.join(map(lambda x: str(x), self.params))
        ret += ") " + str(self.nested_split)
        return ret
class ComponentSplit(object):
    def __init__(self, type, splits):
        self.type = type
        self.splits = splits
    def __str__(self):
        ret = "comp(" + self.type + ") { "
        ret += " | ".join(map(lambda x: str(x), self.splits))
        ret += " }"
        return ret
class String(list):
    def __init__(self, chars):
        list.__init__(self, chars)
    def __str__(self):
        ret = ' '.join(map(lambda x: str(x), self))
        return ret

class CGAGrammar(object):
    def __init__(self):
        self.version = None
        self.attrs = []
        self.vars = []
        self.consts = []
        self.funcs = []
        self.rules = []

    def __str__(self):
        # TODO(whoever): Make the output real CGA.
        ret = '# CGA grammar written by grammar.py\n'
        if self.version:
            ret += 'version ' + self.version + '\n\n'
        if self.attrs:
            ret += '# Attributes\n'
            ret += '\n'.join(map(lambda x: str(x), self.attrs)) + '\n\n'
        if self.vars:
            ret += '# Global variables\n'
            ret += '\n'.join(map(lambda x: str(x), self.vars)) + '\n\n'
        if self.consts:
            ret += '# Constants\n'
            ret += '\n'.join(map(lambda x: str(x), self.consts)) + '\n\n'
        if self.funcs:
            ret += '# Functions\n'
            ret += '\n\n'.join(map(lambda x: str(x), self.funcs)) + '\n\n'
        if self.rules:
            ret += '# Production rules\n'
            ret += '\n'.join(map(lambda x: str(x), self.rules)) + '\n\n'
        return ret


class GrammarBuilder(GenericASTTraversal):
    def __init__(self, ast):
        GenericASTTraversal.__init__(self, ast)
        self.grammar = CGAGrammar()
        self.postorder()

    def n_cga(self, node):
        self.grammar.version = node.value

    def n_anno_stmt(self, node):
        # TODO(whoever): statment object should be created here together with
        # their annotations.
        #print "TODO, anno not implemented yet"
        pass
    def n_annotations(self, node):
        print "TODO, annotations not implemented yet"
    def n_anno(self, node):
        print "TODO, anno not implemented yet"
    def n_handle(self, node):
        print "TODO, handle not implemented yet"
    def n_anno_param(self, node):
        print "TODO, anno_param not implemented yet"

    def n_attr(self, node):
        self.grammar.attrs.append(Attr(node.value, node.children[0].expr))

    def n_global_variable(self, node):
        self.grammar.vars.append(Variable(node.value, node.children[0].expr))

    def n_const(self, node):
        self.grammar.consts.append(Const(node.value, node.children[0].expr))

    def n_function(self, node):
        print "TODO, function not implemented yet"
        #args = Arguments(map(lambda x: x.value, node.children[0].children))
        #self.grammar.funcs.append(Function(node.value, args, node.children[1].expr))

    def n_conditional_expr(self, node):
        print "TODO, cond expr not implemented yet"
    def n_stochastic_expr(self, node):
        print "TODO, prob expr not implemented yet"

    def n_rule(self, node):
        if len(node.children) == 1:
            args = None
            successor = node.children[0].successor
        else:
            args = map(lambda x: x.value, node.children[0])
            successor = node.children[1].successor
        self.grammar.rules.append(Rule(node.value, args, successor))

    def n_successor(self, node):
        node.successor = String(map(lambda x: x.char, node.children))

    def n_conditional(self, node):
        conditions = map(lambda x: x.children[0].expr, node.children[:-1])
        successors = map(lambda x: x.children[1].successor, node.children[:-1])
        default = node.children[-1].children[0].successor
        node.char = ConditionalRule(conditions, successors, default)
    def n_stochastic(self, node):
        probabilities = map(lambda x: x.children[0].expr, node.children[:-1])
        successors = map(lambda x: x.children[1].successor, node.children[:-1])
        default = node.children[-1].children[0].successor
        node.char = StochasticRule(probabilities, successors, default)

    def n_symbol(self, node):
        params = map(lambda x: x.expr, node.children)

        # TODO(whoever): Make this complete!
        # List of everything that's neither Terminal nor Non-Terminal.
        instruction_keywords = [
            'extrude', '[', ']', 't', 'r', 's', 'i', 'setupProjection',
            'texture', 'projectUV', 'translateUV', 'alignScopeToGeometry', 'center', 'color',
            'tileUV', 'set', 'NIL', 'deleteUV'
        ]
        if node.value in instruction_keywords:
            node.char = Instruction(node.value, params)
        else:
            node.char = Symbol(node.value, params)

    def n_split(self, node):
        if len(node.children) == 2:
            params = map(lambda x: x.expr, node.children[0].children)
            node.char = SubdivSplit(node.value, params, node.children[1].split)
        else:
            node.char = SubdivSplit(node.value, None, node.children[0].split)
    def n_comp_split(self, node):
        node.char = ComponentSplit(node.value, map(lambda x: x.split, node.children))
    def n_comp(self, node):
        node.split = Split(node.children[0].expr, node.value, node.children[1].successor)
    def n_split_list(self, node):
        node.split = NestedSplit(map(lambda x: x.split, node.children), True if node.value == '*' else False)
    def n_split_pair(self, node):
        node.split = Split(node.children[0].expr, ':', node.children[1].successor)

    def n_variable(self, node):
        params = map(lambda x: x.expr, node.children)
        node.expr = NameExpr(node.value, params)

    def n_parameter(self, node):
        if node.value:
            node.expr = Parameter(node.children[0].expr, node.value)
        else:
            node.expr = Parameter(node.children[0].expr)

    def n_bracket_expr(self, node):
        node.expr = BracketExpr(node.children[0].expr)

    def n_op(self, node):
        if len(node.children) == 2:
            node.expr = OpExpr(node.children[0].expr, node.value, node.children[1].expr)
        else:
            node.expr = OpExpr(None, node.value, node.children[0].expr)

    def n_int(self, node):
        node.expr = ValueExpr(int(node.value))

    def n_float(self, node):
        node.expr = ValueExpr(float(node.value))

    def n_prob_tok(self, node):
        node.expr = ValueExpr(float(node.value[:-1]))

    def n_string(self, node):
        node.expr = ValueExpr(str(node.value))

    def n_bool(self, node):
        node.expr = ValueExpr(node.value == 'True')

    def default(self, node):
        pass

class StructuralAggregationNode(object):
    def __init__(self,name,predecessor = None, attrs = None, successors = None):
        self.predecessor = predecessor
        self.name = name
        if attrs is None:
            attrs = {}
        self.attrs = attrs
        if successors is None:
            successors = {}
        self.successors = successors

class StructureGraphBuilder(object):
    def __init__(self, grammar):
        self.grammar = grammar
        self.graph = {}
        self.generate_graph()

    def generate_graph(self):
         
        def merge_dicts(old,new,repeat = False):
            if repeat:
                for key in new:
                    old[key] = -1
            else:
                for key in new:
                    if key in old:
                        if old[key]>=0:
                            old[key] += new[key]
                    else:
                        old[key] = new[key]

        def handle_suc_nested_split(lhs, nsplit):
            sucs = {}
            for s in nsplit.splits:
                t = type(s).__name__
                if t == "Split":
                    merge_dicts(sucs,handle_suc(lhs, s.successor))
                elif t == "NestedSplit":
                    print "Dat nested split"
                    repeat_split = s.repeated #Bool is auto-promoted to int for addition
                    merge_dicts(sucs,handle_suc_nested_split(lhs, s),repeat_split)
                else:
                    print "ERROR: Nested split handling failed!"
            return sucs;

        def handle_suc(lhs, string):
            sucs = {}
            for c in string:
                t = type(c).__name__
                if t == "Symbol":
                    self.graph[lhs].add(c.name)
                    sucs[c.name] = 1;
                elif t == "Instruction":
                    pass
                elif t == "ConditionalRule":
                    for succ in c.successors:
                        handle_suc(lhs, succ)
                    merge_dicts(sucs,handle_suc(lhs, c.default))
                elif t == "StochasticRule":
                    for succ in c.successors:
                        handle_suc(lhs, succ)
                    merge_dicts(sucs,handle_suc(lhs, c.default))
                elif t == "SubdivSplit":
                    merge_dicts(sucs,handle_suc_nested_split(lhs, c.nested_split),c.nested_split.repeated)
                elif t == "ComponentSplit":
                    for comp in c.splits:
                        merge_dicts(sucs,handle_suc(lhs, comp.successor))
                else:
                    print "ERROR: Unsupported char type (" + t + ") encountered!"
            return sucs;

        def handle_param_nested_split(lhs, nsplit):
            attrs = {}
            for s in nsplit.splits:
                t = type(s).__name__
                if t == "Split":
                    merge_dicts(attrs,get_params_from_expr(s.expr))
                    merge_dicts(attrs,handle_params(lhs, s.successor))
                elif t == "NestedSplit":
                    print "Dat nested split"
                    repeat_split = s.repeated #Bool is auto-promoted to int for addition
                    merge_dicts(attrs,handle_param_nested_split(lhs, s),repeat_split)
                else:
                    print "ERROR: Nested split handling failed in parameter extraction!" 
            return attrs;

        def get_params_from_expr(expr):
            attrs = {}
            t = type(expr).__name__
            if t == "Parameter":
                merge_dicts(attrs,get_params_from_expr(expr.expr))
            elif t == "NameExpr": #base case
                attrs[expr.name]=1
            elif t == "ValueExpr": 
                return {}
            elif t == "OpExpr": 
                merge_dicts(attrs,get_params_from_expr(expr.left))
                merge_dicts(attrs,get_params_from_expr(expr.right))
            elif t == "BracketExpr": 
                return {}
            elif t == "CaseExpr": 
                return {}
            elif t == "ProbExpr": 
                return {}
            else:
                print "ERROR: Unsupported char type (" + t + ") encountered when parsing expression!"
            return attrs;

        def handle_params(lhs,successors):
            attrs = {}
            for suc in successors:
                t = type(suc).__name__
                if t == "Symbol":
                    pass #TODO           
                elif t == "Instruction":
                    if (not suc.name == "s") and (not suc.name == "t"):
                        for param in suc.params:
                            merge_dicts(attrs,get_params_from_expr(param))
                elif t == "ConditionalRule":
                    for cond in suc.conditions:
                        merge_dicts(attrs,get_params_from_expr(cond))
                    for suc2 in suc.successors:
                        merge_dicts(attrs,handle_params(lhs, suc2))
                    merge_dicts(attrs,handle_params(lhs, suc.default))
                elif t == "StochasticRule":   
                    for prob in suc.probabilities:
                        merge_dicts(attrs,get_params_from_expr(prob))
                    for suc2 in suc.successors:
                        merge_dicts(attrs,handle_params(lhs, suc2))
                    merge_dicts(attrs,handle_params(lhs, suc.default))                   
                elif t == "SubdivSplit":
                    print "Dat split, yo! " + str(suc.direction)
                    merge_dicts(attrs,handle_param_nested_split(lhs, suc.nested_split),suc.nested_split.repeated)
                elif t == "ComponentSplit":
                    for comp in suc.splits:
                        merge_dicts(attrs,handle_params(lhs, comp.successor))
                else:
                    print "ERROR: Parameter extraction encountered unsupported char type (" + t + ") encountered!"  
            return attrs;

        self.params = {}
        self.count = {}
        self.attrs = {}
        for attr in self.grammar.attrs:
            self.attrs[attr.name] = 1
        for rule in self.grammar.rules:            
            lhs = rule.name
            if lhs in self.graph:
                print "ERROR: encountered a duplicate of production rule '" + lhs +"'!"
            self.graph[lhs] = set()
            self.params[lhs] = handle_params(lhs, rule.successor)
            self.count[lhs] = handle_suc(lhs, rule.successor)
            print str(self.count[lhs])

#class StructuralAggregationNode(object):
#    def __init__(self,name,predecessor = None, attrs = {}, successors = {}):
#        self.predecessor = predecessor
#        self.name = name
#        self.attrs = attrs
#        self.successors = successors

    def write_slightly_reduced_dot(self, file_name):
        def node_name(rule):
            # GraphViz dot cannot handle . in the node names.
            return rule.replace('.', '_')
        
        def mark_visited(rule,mapping_from,visited,starting_rules):
            visited.append(rule)
            rule_stack = mapping_from[rule]
            while rule_stack:
                iter_rule = rule_stack.pop()
                if visited.count(iter_rule)==0:
                    visited.append(iter_rule)
                    if iter_rule in mapping_from:
                        rule_stack.extend(mapping_from[iter_rule])
                    else:
                        starting_rules.append(iter_rule)
        
        def find_structural_nodes(s_rule,pred,mapping,params,counts,attrs):
            
            def add_attributes(node,rule,params,attrs):
                for attr in params[rule]:
                    if attr in attrs:
                        if params[rule][attr] > 0:
                            node.attrs[attr] = params[rule][attr]
            
            def select_node(rule,pred,mapping,params,counts,attrs,node_dict):
                if rule in params and params[rule]:#found attr node and not empty
                    node = StructuralAggregationNode(rule)
                    add_attributes(node,rule,params,attrs)
                    if node.attrs:
                        node_dict[node.name] = node
                        return node
                    else:
                        return None
                elif pred is not None and pred in counts and rule in counts[pred] and counts[pred][rule]<0:
                    print "BAZZINGA"
                    node = StructuralAggregationNode(rule)
                    node_dict[node.name] = node
                    return node
                else:
                    return None
            
            def merge_node(child,rule,mapping,params,counts,attrs):
                if rule in params:
                    node =  StructuralAggregationNode(rule)
                    for attr in params[rule]:
                        node.attrs[attr] = params[rule][attr]
                    for attr in child.attrs:
                        if attr in node.attrs:
                            node.attrs[attr] += child.attrs[attr]
                        else:
                            node.attrs[attr] = child.attrs[attr]
                else:
                    return StructuralAggregationNode(rule,child.params)
            
            def locate_successors(s_rule,pred,mapping,params,counts,attrs,node_dict):
                print "locating successors of ... " + s_rule
                children = []
                for rule in mapping[s_rule]:#for each succesor
                    print "testing " + rule
                    if rule == s_rule:
                        continue
                    if rule in mapping:#traverse until bottom
                        if rule in node_dict:
                            child = node_dict[rule]
                        else:
                            child = locate_successors(rule,s_rule,mapping,params,counts,attrs,node_dict)
                    else:#visit leaves
                        child = select_node(rule,s_rule,mapping,params,counts,attrs,node_dict)
                    if child is None:#no child
                        continue
                    else:
                        children.append(child)
                print children
                if children:
                    node = StructuralAggregationNode(s_rule)
                    if s_rule in params and params[s_rule]: 
                        add_attributes(node,s_rule,params,attrs)
                    for c in children:
                        if c.attrs:
                            node.successors[c.name] = c
                        elif counts[node.name][c.name] < 0:
                            node.successors[c.name] = c
                        else: #can be merged away
                            for name,c2 in c.successors.iteritems():
                                node.successors[name] = c2
                            
                    node_dict[node.name] = node
                    return node
                else:
                    node = StructuralAggregationNode(s_rule)
                    if s_rule in params and params[s_rule]:
                        add_attributes(node,s_rule,params,attrs)
                    if node.attrs:
                        node_dict[node.name] = node
                        return node
                    else:
                        return None
                
            #test children for importance
            node_dict = {}
            return locate_successors(s_rule,pred,mapping,params,counts,attrs,node_dict)
        
        #obtain predecessors and successors
        mapping_to = {}
        mapping_from = {}
        for rule, deps in self.graph.iteritems():
            for d in deps:
                if rule in mapping_to:
                    mapping_to[rule].append(d)
                else:
                    mapping_to[rule] = [d]
                if d in mapping_from:
                    mapping_from[d].append(rule)
                else:
                    mapping_from[d] = [rule]
                    
        #find starting rules
        visited = []
        starting_rules = []
        for rule, deps in self.graph.iteritems():
            if rule not in visited:
                if rule in mapping_from:
                    mark_visited(rule,mapping_from,visited,starting_rules);
                else:
                    starting_rules.append(rule)
                    visited.append(rule)
        
        reduced_structure_graphs = {}
        for starting_rule in starting_rules:
            reduced_structure_graphs[starting_rule] = find_structural_nodes(starting_rule,None,mapping_to,self.params,self.count,self.attrs)
        
        visited = {}
        labels = ""
        output = "digraph {\n"
        for starting_rule in starting_rules:
            if starting_rule != "FrontFacade":
                continue
            rule = reduced_structure_graphs[starting_rule]
            if rule is None:
                continue
            rule_stack = [rule]
            while rule_stack:
                rule_node = rule_stack.pop()
                if rule_node.name in visited:
                    continue
                else:
                    visited[rule_node.name]=1
                for node in rule_node.successors.iteritems():
                    rule_stack.append(node[1])
                deps = rule_node.successors
                rule = rule_node.name
                labels += "    " + node_name(rule) + ' [label=<"' + rule + '"'
                if rule in self.params:
                    for attr in self.params[rule]:
                        if attr in self.attrs:
                            if self.params[rule][attr] > 0:
                                labels += '\n<BR /><FONT POINT-SIZE="10">'+attr+':'+str(self.params[rule][attr])+'</FONT>'
                            else:
                                labels += '\n<BR /><FONT POINT-SIZE="10">'+attr+':n</FONT>'
                labels +='>];\n'
                for d in deps:
                    output += '    ' + node_name(rule) + ' -> ' + node_name(d)
                    if d in self.count[rule]:
                        if self.count[rule][d] > 0:
                            output += '[label="'+str(self.count[rule][d])+'"];\n'
                        else:
                            output += '[label="n"];\n'
                    else:
                        output += '[label="1"];\n'
                        
            output += labels
        output += "}"

        with file(file_name[0:3]+"slightly_reduced_" + file_name[3:] + ".gv", 'w') as out:
            out.write(output)

    def write_strictly_reduced_dot(self, file_name):
        def node_name(rule):
            # GraphViz dot cannot handle . in the node names.
            return rule.replace('.', '_')
        
        def mark_visited(rule,mapping_from,visited,starting_rules):
            visited.append(rule)
            rule_stack = mapping_from[rule]
            while rule_stack:
                iter_rule = rule_stack.pop()
                if visited.count(iter_rule)==0:
                    visited.append(iter_rule)
                    if iter_rule in mapping_from:
                        rule_stack.extend(mapping_from[iter_rule])
                    else:
                        starting_rules.append(iter_rule)
        
        def find_structural_nodes(s_rule,pred,mapping,params,counts,attrs):
            
            def add_attributes(node,rule,params,attrs):
                for attr in params[rule]:
                    if attr in attrs:
                        if params[rule][attr] > 0:
                            if attr in node.attrs:
                                node.attrs[attr] += params[rule][attr]
                            else:
                                node.attrs[attr] = params[rule][attr]
            
            def select_node(rule,pred,mapping,params,counts,attrs,node_dict):
                if rule in params and params[rule]:#found attr node and not empty
                    node = StructuralAggregationNode(rule)
                    add_attributes(node,rule,params,attrs)
                    if node.attrs:
                        node_dict[node.name] = node
                        return node
                    else:
                        return None
                elif pred is not None and pred in counts and rule in counts[pred] and counts[pred][rule]<0:
                    print "BAZZINGA"
                    node = StructuralAggregationNode(rule)
                    node_dict[node.name] = node
                    return node
                else:
                    return None
            
            def clean_node(node,mapping,params,counts,attrs):
                rule = node.name
                new_sucs = {}
                for suc_rule, suc_node in node.successors.iteritems():
                    if rule in counts and suc_rule in counts[rule] and counts[rule][suc_rule]==1:
                        add_attributes(node,suc_rule,params,attrs)
                        for suc2_rule, suc2_node in suc_node.successors.iteritems():
                            new_sucs[suc2_rule]=suc2_node
                    else:
                        new_sucs[suc_rule]=suc_node
                node.successors = new_sucs
                        
            
            def locate_successors(s_rule,pred,mapping,params,counts,attrs,node_dict):
                print "locating successors of ... " + s_rule
                children = []
                for rule in mapping[s_rule]:#for each succesor
                    print "testing " + rule
                    if rule == "Window":
                        a=1
                    if rule == s_rule:
                        continue
                    if rule in mapping:#traverse until bottom
                        if rule in node_dict:
                            child = node_dict[rule]
                        else:
                            child = locate_successors(rule,s_rule,mapping,params,counts,attrs,node_dict)
                    else:#visit leaves
                        child = select_node(rule,s_rule,mapping,params,counts,attrs,node_dict)
                    if child is None:#no child
                        continue
                    else:
                        children.append(child)
                print children
                if children:
                    node = StructuralAggregationNode(s_rule)
                    if s_rule in params and params[s_rule]: 
                        add_attributes(node,s_rule,params,attrs)
                    for c in children:
                        if c.attrs:
                            node.successors[c.name] = c
                        elif counts[node.name][c.name] < 0:
                            node.successors[c.name] = c
                        elif counts[node.name][c.name] == 1: #can be merged away
                            for name,c2 in c.successors.iteritems():
                                node.successors[name] = c2
                    
                    clean_node(node,mapping,params,counts,attrs)
                    node_dict[node.name] = node
                    return node
                else:
                    node = StructuralAggregationNode(s_rule)
                    if s_rule in params and params[s_rule]:
                        add_attributes(node,s_rule,params,attrs)
                    if node.attrs:
                        node_dict[node.name] = node
                        return node
                    else:
                        return None
                
            #test children for importance
            node_dict = {}
            return locate_successors(s_rule,pred,mapping,params,counts,attrs,node_dict)
        
        #obtain predecessors and successors
        mapping_to = {}
        mapping_from = {}
        for rule, deps in self.graph.iteritems():
            for d in deps:
                if rule in mapping_to:
                    mapping_to[rule].append(d)
                else:
                    mapping_to[rule] = [d]
                if d in mapping_from:
                    mapping_from[d].append(rule)
                else:
                    mapping_from[d] = [rule]
                    
        #find starting rules
        visited = []
        starting_rules = []
        for rule, deps in self.graph.iteritems():
            if rule not in visited:
                if rule in mapping_from:
                    mark_visited(rule,mapping_from,visited,starting_rules);
                else:
                    starting_rules.append(rule)
                    visited.append(rule)
        
        reduced_structure_graphs = {}
        for starting_rule in starting_rules:
            reduced_structure_graphs[starting_rule] = find_structural_nodes(starting_rule,None,mapping_to,self.params,self.count,self.attrs)
        
        visited = {}
        labels = ""
        output = "digraph {\n"
        for starting_rule in starting_rules:
            if starting_rule != "FrontFacade":
                continue
            rule = reduced_structure_graphs[starting_rule]
            if rule is None:
                continue
            rule_stack = [rule]
            while rule_stack:
                rule_node = rule_stack.pop()
                if rule_node.name in visited:
                    continue
                else:
                    visited[rule_node.name]=1
                for node in rule_node.successors.iteritems():
                    rule_stack.append(node[1])
                deps = rule_node.successors
                rule = rule_node.name
                labels += "    " + node_name(rule) + ' [label=<"' + rule + '"'
                for attr in rule_node.attrs:
                    if rule_node.attrs[attr] > 0:
                        labels += '\n<BR /><FONT POINT-SIZE="10">'+attr+':'+str(rule_node.attrs[attr])+'</FONT>'
                    else:
                        labels += '\n<BR /><FONT POINT-SIZE="10">'+attr+':n</FONT>'
                labels +='>];\n'
                for d in deps:
                    output += '    ' + node_name(rule) + ' -> ' + node_name(d)
                    if d in self.count[rule]:
                        if self.count[rule][d] > 0:
                            output += '[label="'+str(self.count[rule][d])+'"];\n'
                        else:
                            output += '[label="n"];\n'
                    else:
                        output += '[label="1"];\n'
                        
            output += labels
        output += "}"

        with file(file_name[0:3]+"strictly_reduced_" + file_name[3:] + ".gv", 'w') as out:
            out.write(output)

    def write_dot(self, file_name):
        self.write_slightly_reduced_dot(file_name)
        self.write_strictly_reduced_dot(file_name)
        def node_name(rule):
            # GraphViz dot cannot handle . in the node names.
            return rule.replace('.', '_')
        labels = ""
        output = "digraph {\n"
        for rule, deps in self.graph.iteritems():
            labels += "    " + node_name(rule) + ' [label=<"' + rule + '"'
            for attr in self.params[rule]:
                if attr in self.attrs:
                    if self.params[rule][attr] > 0:
                        labels += '\n<BR /><FONT POINT-SIZE="10">'+attr+':'+str(self.params[rule][attr])+'</FONT>'
                    else:
                        labels += '\n<BR /><FONT POINT-SIZE="10">'+attr+':n</FONT>'
            labels +='>];\n'
            for d in deps:
                output += '    ' + node_name(rule) + ' -> ' + node_name(d)
                if d in self.count[rule]:
                    if self.count[rule][d] > 0:
                        output += '[label="'+str(self.count[rule][d])+'"];\n'
                    else:
                        output += '[label="n"];\n'
                else:
                    output += '[label="1"];\n'
                    
        output += labels
        output += "}"

        with file(file_name + ".gv", 'w') as out:
            out.write(output)

    def write_pdf(self, file_name):
        file_name2 = "gv/"+file_name
        self.write_dot(file_name2)
        os.system("dot -Tpdf " + file_name2 + ".gv -o " + file_name2 + ".pdf")
