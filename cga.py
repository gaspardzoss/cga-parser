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
        r"~|'|\.|:|\|\||\||,|\(|\)|{|}|\[|\]|-->|\*|\/|%|\+|\-|==|<=|>=|<|>|=|\^|!=|!|&&"
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
        
    def t_group(self, s):  # TODO(whoever): Handle parsing is missing.
        r'@Group(.+)'
        self.rv.append(Token(type='handle', value=s))
        
    def t_order(self, s):  # TODO(whoever): Handle parsing is missing.
        r'@Order(.+)'
        self.rv.append(Token(type='handle', value=s))
        
    def t_range(self, s):  # TODO(whoever): Handle parsing is missing.
        r'@Range(.+)'
        self.rv.append(Token(type='handle', value=s))
        
    def t_description(self, s):  # TODO(whoever): Handle parsing is missing.
        r'@Description(.+)'
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
    def p_expr_3(self, args):
        ' expr ::= join ^ expr '
        return AST(type='op', value=args[1].type, children=[args[0], args[2]])
#    def p_expr_4(self, args):
#        ' expr ::= expr , expr '
#        return AST(type='list', value=args[1].type, children=[args[0], args[2]])
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
        
#    def p_factor_4(self, args):
#        ' factor ::= { expr } '
#        return AST(type='bracket_expr', children=[args[1]])

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
        for (cond, expr) in zip(self.conditions, self.exprs):
            ret += "case " + str(expr) + " : " + str(self.succ) + '\n'
        ret += "else : " + str(self.default)
        return ret
class ProbExpr(Expr):
    def __init__(self, probabilities, exprs, default):
        self.probabilietes = probabilities
        self.exprs = exprs
        self.default = default
    def __str__(self):
        ret = ""
        for (prob, expr) in zip(self.probabilities, self.exprs):
            ret += ("%.2f" % prob * 100) + "% : " + str(expr) + '\n'
        ret += "else : " + str(self.default)
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
            'tileUV', 'set', 'NIL', 'deleteUV','reverseNormals','rotateScope','offset','envelope'
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
        
class Node(object):
    def __init__(self,name,attrs = None):
        self.name = name
        self.predecessors = {}
        self.successors = {}
        if not attrs:
            attrs = {}
        self.attrs = attrs
        self.rep_trans_suc = False
        
        self.pred_cons = {
            "comp":{},
            "split":{},
            "case":{},
            "prob":{}        
        }
        self.pred_cons["comp"] = {}
        self.pred_cons["split"] = {}
        self.pred_cons["case"] = {}
        self.pred_cons["prob"] = {}
        
        self.suc_cons = {
            "comp":{},
            "split":{},
            "case":{},
            "prob":{}        
        }
        self.suc_cons["comp"] = {}
        self.suc_cons["split"] = {}
        self.suc_cons["case"] = {}
        self.suc_cons["prob"] = {}
        
    def __repr__(self):
        ret = self.name + "\n"
        for attr,count in self.attrs.iteritems():
            ret += "    "+ attr + ": " + str(count) + "\n"
        for suc_name, count in self.successors.iteritems():
            attrs = ""
            for con_name, cons in self.suc_cons.iteritems():
                if suc_name in cons:
                    attrs += str(cons[suc_name]) + ", "
            ret += "  " + str(count) + " -> " + suc_name + "(" + attrs + ")\n"
            
        
        for pred_name, count in self.predecessors.iteritems():
            attrs = ""
            for con_name, cons in self.pred_cons.iteritems():
                if pred_name in cons:
                    attrs += str(cons[pred_name]) + ", "
            ret += "  " +  pred_name + "(" + attrs + ") <- " + str(count) + "\n"
            
        return ret
        
        

class StructureGraphBuilder(object):
    def __init__(self, grammar):
        self.grammar = grammar
        self.graph = {}
        self.generate_graph()

    def generate_graph(self):
        
        def merge_dicts(old,new,repeat = False):
            if repeat:
                for key in new:
                    if key == "comp_type" or key == "split_dir":
                        continue
                    old[key] = -1
            else:
                for key in new:
                    if key in old:    
                        if key == "comp_type" or key == "split_dir":
                            old[key] += "," + new[key]
                        elif old[key]>=0:
                            old[key] += new[key]
                    else:
                        old[key] = new[key]

        def handle_suc_nested_split(lhs, nsplit, cons, pred_info = None):
            sucs = {}
            if not pred_info:
                pred_info = {'split':None, 'comp':None}
            for split in nsplit.splits:
                copied_pred_info = {
                    'source':pred_info['source'],
                    'split':pred_info['split'],
                    'comp':pred_info['comp'],
                    'prob':pred_info['prob'],
                    'case':pred_info['case']
                }
                t = type(split).__name__
                if t == "Split":
                    merge_dicts(sucs,handle_suc(lhs, split.successor, cons, copied_pred_info))
                elif t == "NestedSplit":
                    repeat_split = split.repeated #Bool is auto-promoted to int for addition
                    merge_dicts(sucs,handle_suc_nested_split(lhs, split, cons, copied_pred_info),repeat_split)
                else:
                    print "ERROR: Nested split handling failed!"
            return sucs

        def handle_suc(lhs, successors, cons, pred_info = None):
            sucs = {}
            if not pred_info:
                pred_info = {'source':None,'split':None, 'comp':None,'prob':None,'case':None}
            for suc in successors:
                t = type(suc).__name__
                if t == "Symbol":
                    for name,con_type in cons.iteritems():#update special connections
                        if pred_info[name]:
                            if pred_info['source'] in con_type:
                                if not suc.name in con_type[pred_info['source']]:
                                    con_type[pred_info['source']][suc.name] = pred_info[name]
                            else:
                                con_type[pred_info['source']] = {}
                                con_type[pred_info['source']][suc.name] = pred_info[name]
                    self.graph[lhs].add(suc.name)
                    sucs[suc.name] = 1
            for suc in successors:
                t = type(suc).__name__
                if t == "Instruction":
                    pass
                elif t == "ConditionalRule":
                    index = 1
                    for suc2 in suc.successors:
                        lhs_new = lhs+"_"+str(index)
                        self.graph[lhs_new] = set()
                        self.graph[lhs].add(lhs_new)
                        self.count[lhs_new] = {}
                        sucs[lhs_new] = 1
                        pred_info['source'] = lhs_new
                        if lhs in cons['case']:
                            cons['case'][lhs][lhs_new] = "c"
                        else:
                            cons['case'][lhs] = {}
                            cons['case'][lhs][lhs_new] = "c"
                        merge_dicts(self.count[lhs_new],handle_suc(lhs_new, suc2, cons))
                        index += 1
                        
                    #default case
                    lhs_new = lhs+"_0"
                    self.graph[lhs_new] = set()
                    self.graph[lhs].add(lhs_new)
                    self.count[lhs_new] = {}
                    sucs[lhs_new] = 1
                    pred_info['source'] = lhs_new
                    if lhs in cons['case']:
                        cons['case'][lhs][lhs_new] = "c"
                    else:
                        cons['case'][lhs] = {}
                        cons['case'][lhs][lhs_new] = "c"
                    merge_dicts(self.count[lhs_new],handle_suc(lhs_new, suc.default, cons))
                elif t == "StochasticRule":
                    index = 1
                    for suc2 in suc.successors:
                        lhs_new = lhs+"_"+str(index)
                        self.graph[lhs_new] = set()
                        self.graph[lhs].add(lhs_new)
                        self.count[lhs_new] = {}
                        sucs[lhs_new] = 1
                        pred_info['source'] = lhs_new
                        if lhs in cons['prob']:
                            cons['prob'][lhs][lhs_new] = "p"
                        else:
                            cons['prob'][lhs] = {}
                            cons['prob'][lhs][lhs_new] = "p"
                        merge_dicts(self.count[lhs_new],handle_suc(lhs_new, suc2, cons))
                        index += 1
                        
                    #default case
                    lhs_new = lhs+"_0"
                    self.graph[lhs_new] = set()
                    self.graph[lhs].add(lhs_new)
                    self.count[lhs_new] = {}
                    sucs[lhs_new] = 1
                    pred_info['source'] = lhs_new
                    if lhs in cons['prob']:
                        cons['prob'][lhs][lhs_new] = "p"
                    else:
                        cons['prob'][lhs] = {}
                        cons['prob'][lhs][lhs_new] = "p"
                    merge_dicts(self.count[lhs_new],handle_suc(lhs_new, suc.default, cons))
                elif t == "SubdivSplit":
                    if pred_info['split']:    
                        lhs_new = lhs+str(suc.direction)
                        if pred_info['source'] in cons['split']:
                            cons['split'][pred_info['source']][lhs_new] = pred_info['split']
                        else:
                            cons['split'][pred_info['source']] = {}
                            cons['split'][pred_info['source']][lhs_new] = pred_info['split']
                        pred_info['split'] = str(suc.direction)                        
                    else:
                        pred_info['split'] = str(suc.direction)
                        lhs_new = lhs+"_"+str(suc.direction)
                        
                    pred_info['source'] = str(lhs_new)
                    
                    self.graph[lhs_new] = set()
                    self.graph[lhs].add(lhs_new)
                    self.count[lhs_new] = {}
                    sucs[lhs_new] = 1
                    merge_dicts(self.count[lhs_new],handle_suc_nested_split(lhs_new, suc.nested_split, cons, pred_info),suc.nested_split.repeated)
                elif t == "ComponentSplit":
                    if pred_info['comp']:
                        pred_info['comp'] += str(suc.type)
                    else:
                        pred_info['source'] = str(lhs)
                        pred_info['comp'] = str(suc.type)
                    for comp in suc.splits:
                        merge_dicts(sucs,handle_suc(lhs, comp.successor, cons, pred_info))
                else:
                    if t != "Symbol":
                        print "ERROR: Unsupported char type (" + t + ") encountered!"
            return sucs

        def handle_param_nested_split(lhs, nsplit, split_dirs):
            attrs = {}
            for s in nsplit.splits:
                t = type(s).__name__
                if t == "Split":
                    merge_dicts(attrs,get_params_from_expr(s.expr))
                    merge_dicts(attrs,handle_params(lhs, s.successor, split_dirs))
                elif t == "NestedSplit":
                    repeat_split = s.repeated #Bool is auto-promoted to int for addition
                    merge_dicts(attrs,handle_param_nested_split(lhs, s, split_dirs),repeat_split)
                else:
                    print "ERROR: Nested split handling failed in parameter extraction!" 
            return attrs

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
                if t != "NoneType":
                    print "ERROR: Unsupported char type (" + t + ") encountered when parsing expression!"
            return attrs

        def handle_params(lhs, successors, split_dirs = None):
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
                    index = 1
                    for cond in suc.conditions:
                        merge_dicts(attrs,get_params_from_expr(cond))
                    for suc2 in suc.successors:
                        self.params[lhs+"_"+str(index)] = {}
                        merge_dicts(self.params[lhs+"_"+str(index)],handle_params(lhs+"_"+str(index), suc2))
                        index += 1
                    self.params[lhs+"_"+"0"] = {}
                    merge_dicts(self.params[lhs+"_"+"0"],handle_params(lhs+"_"+"0", suc.default))
                elif t == "StochasticRule":  
                    index = 1 
                    for prob in suc.probabilities:
                        merge_dicts(attrs,get_params_from_expr(prob))
                    for suc2 in suc.successors:
                        self.params[lhs+"_"+str(index)] = {}
                        merge_dicts(self.params[lhs+"_"+str(index)],handle_params(lhs+"_"+str(index), suc2))
                        index += 1
                    self.params[lhs+"_"+"0"] = {}
                    merge_dicts(self.params[lhs+"_"+"0"],handle_params(lhs+"_"+"0", suc.default))                   
                elif t == "SubdivSplit":
                    if split_dirs:
                        split_dirs += str(suc.direction)
                        new_lhs = lhs + str(suc.direction)
                    else:
                        split_dirs = str(suc.direction)
                        new_lhs = lhs+"_"+split_dirs
                    self.params[new_lhs] = {}
                    merge_dicts(self.params[new_lhs],handle_param_nested_split(new_lhs, suc.nested_split, split_dirs),suc.nested_split.repeated)
                elif t == "ComponentSplit":
                    for comp in suc.splits:
                        merge_dicts(attrs,handle_params(lhs, comp.successor))
                else:
                    print "ERROR: Parameter extraction encountered unsupported char type (" + t + ") encountered!"  
            return attrs

        self.params = {}
        self.count = {}
        self.attrs = {}
        self.connections = {
            'case': {},
            'comp': {},
            'split':{},
            'prob': {}
        }
        self.comp_connections = {}
        self.split_connections = {}
        for attr in self.grammar.attrs:
            self.attrs[attr.name] = 1
        for rule in self.grammar.rules:            
            lhs = rule.name
            if lhs in self.graph:
                print "ERROR: encountered a duplicate of production rule '" + lhs +"'!\n"
            self.graph[lhs] = set()
            self.count[lhs] = handle_suc(lhs, rule.successor,self.connections)
            self.params[lhs] = handle_params(lhs, rule.successor)

    def write_slightly_reduced_dot(self, file_name):      
        
        def merge_dicts(old,new,repeat = False):
            if repeat:
                for key in new:
                    old[key] = -1
            else:
                for key in new:
                    if key in old:    
                        if old[key] > 0 and new[key] > 0:
                            old[key] += new[key]
                        elif new[key] < 0:
                            old[key] = new[key]
                    else:
                        old[key] = new[key]
                        
        def node_name(rule):
            # GraphViz dot cannot handle . in the node names.
            return rule.replace('.', '_')
        
        def mark_visited(rule,mapping_from,visited,starting_rules):
            visited.append(rule)
            rule_stack = [x for x in mapping_from[rule]]
            while rule_stack:
                iter_rule = rule_stack.pop()
                if visited.count(iter_rule)==0:
                    visited.append(iter_rule)
                    if iter_rule in mapping_from:
                        rule_stack.extend([x for x in mapping_from[iter_rule]])
                    else:
                        starting_rules.append(iter_rule)
        def remove_nodes_lazily(reduced_node_dict, node_dict,lazy_del_set,save_list = None):
            if not save_list:
                save_list =  []
            visited_from = {}
            disconnecting = {}
            for orig_tuple in lazy_del_set:
                orig = orig_tuple[1]
                if orig in visited_from:
                    continue
                visited_from[orig] = True
                dests = {}
                for dest_tuple in lazy_del_set:
                    if orig == dest_tuple[1]:
                        dests[dest_tuple[0]] = True
                valid_delete = True
                for key,val in node_dict[orig].successors.iteritems():#only allow deleting if all children are deleted
                    if not key in dests:
                        valid_delete = False
                        
                if orig in save_list:
                    disconnecting[orig] = True
                    valid_delete = True
                    
                if valid_delete:
                    for dest_name,visited in dests.iteritems():
                        if orig in reduced_node_dict[dest_name].predecessors:
                            del reduced_node_dict[dest_name].predecessors[orig]   
                        if orig in  reduced_node_dict and dest_name in reduced_node_dict[orig].successors:
                            del reduced_node_dict[orig].successors[dest_name]
            del_set = set()
            for orig_tuple in lazy_del_set:
                if orig_tuple[1] not in disconnecting:
                    del_set.add(orig_tuple[1])    
            for orig in del_set:
                if orig in reduced_node_dict:
                    if not reduced_node_dict[orig].predecessors:
                        del reduced_node_dict[orig]
        
        def get_structure_graph(mapping_suc,mapping_pred,s_rule):
            
            def add_overewrite_attributes(node,rule):
                if rule in self.params:
                    for attr in self.params[rule]:
                        if attr in self.attrs:
                            node.attrs[attr] = self.params[rule][attr]
            
            def get_children(s_rule,pred,mapping_suc,mapping_pred,node_dict):
                children = []
                for rule in mapping_suc[s_rule]:#for each successor
                    if rule == s_rule:
                        continue
                    if rule in mapping_suc:#traverse until bottom
                        if rule in node_dict:
                            child = node_dict[rule]
                        else:
                            child = get_children(rule,s_rule,mapping_suc,mapping_pred,node_dict)
                    else:#visit leaves
                        if rule in node_dict:
                            child = node_dict[rule]
                        else:
                            child = Node(rule)
                            add_overewrite_attributes(child,rule)
                            node_dict[rule] = child
                    if child:
                        children.append(child)
                node = Node(s_rule)
                add_overewrite_attributes(node,s_rule)
                
                cons = self.connections
                for c in children:     
                    #establish transitively who has repeating children
                    if self.count[s_rule][c.name] < 0 or c.rep_trans_suc:
                        node.rep_trans_suc = True
                        
                    #update node
                    node.successors[c.name] = self.count[node.name][c.name]
                    for con_name,n_cons in node.suc_cons.iteritems():
                        if node.name in cons[con_name] and c.name in cons[con_name][node.name] and len(cons[con_name][node.name][c.name]) > 0 :
                            n_cons[c.name] = cons[con_name][node.name][c.name]
                    
                    #update child
                    c.predecessors[node.name] = self.count[node.name][c.name]
                    for con_name,c_cons in c.pred_cons.iteritems(): 
                        if node.name in cons[con_name] and c.name in cons[con_name][node.name] and len(cons[con_name][node.name][c.name]) > 0 :
                            c_cons[node.name] = cons[con_name][node.name][c.name]
                node_dict[s_rule] = node
                return node

            sg_node_dict = {}
            get_children(s_rule,None,mapping_suc,mapping_pred,sg_node_dict)
            return sg_node_dict            
       
        def transfer_childs_successors(receptor, origin,  lazy_del_set, node_dict, red_node_dict, branched = False):
            rn = receptor.name
            on = origin.name
            for sn,count in origin.successors.iteritems():
                
                if sn not in red_node_dict:
                    continue
                
                #establish predecessors
                if rn in red_node_dict[sn].predecessors:
                    if origin.name in node_dict[sn].predecessors:
                        if red_node_dict[sn].predecessors[rn] > 0 and node_dict[sn].predecessors[on] > 0:
                            red_node_dict[sn].predecessors[rn] += node_dict[sn].predecessors[on]
                        elif node_dict[sn].predecessors[on] < 0:
                            red_node_dict[sn].predecessors[rn] = node_dict[sn].predecessors[on]
                    else:
                        if red_node_dict[sn].predecessors[rn] > 0 and red_node_dict[sn].predecessors[on] > 0:
                            red_node_dict[sn].predecessors[rn] += red_node_dict[sn].predecessors[on]
                        elif red_node_dict[sn].predecessors[on] < 0:
                            red_node_dict[sn].predecessors[rn] = red_node_dict[sn].predecessors[on]
                else:
                    if on in node_dict[sn].predecessors:
                        red_node_dict[sn].predecessors[rn] = node_dict[sn].predecessors[on]
                    else:
                        if on in red_node_dict[sn].predecessors:
                            red_node_dict[sn].predecessors[rn] = red_node_dict[sn].predecessors[on]
                        
                
                #establish special relations to child's predecessor
                for con_name,cons in red_node_dict[sn].pred_cons.iteritems():
                    info = ""
                    if on in node_dict[sn].pred_cons[con_name]:
                        info += node_dict[sn].pred_cons[con_name][on]      
                    if rn in node_dict[on].pred_cons[con_name]:
                        info += node_dict[on].pred_cons[con_name][rn]
                    
                    #relation is potentially not in the old_node, but in the new one   
                    if len(info) == 0:
                        if sn in red_node_dict and on in cons:
                            info += cons[on]      
                        if on in red_node_dict and rn in red_node_dict[on].pred_cons[con_name]:
                            info += red_node_dict[on].pred_cons[con_name][rn]
                            
                    if len(info)>0:
                        if rn in cons and not branched:
                            cons[rn] += info
                        else:
                            cons[rn] = info
                    elif rn in cons:
                        del cons[rn]
                
                #establish successors
                if sn in receptor.successors:
                    if receptor.successors[sn] > 0 and origin.successors[sn] > 0:
                        receptor.successors[sn] += origin.successors[sn]
                    elif origin.successors[sn] < 0:
                        receptor.successors[sn] = origin.successors[sn]
                else:
                    receptor.successors[sn] = origin.successors[sn]
                    
                #establish special relations to successor
                for con_name,cons in receptor.suc_cons.iteritems():
                    info = ""
                    if on in node_dict[rn].suc_cons[con_name]:
                        info += node_dict[rn].suc_cons[con_name][on]    
                    if sn in node_dict[on].suc_cons[con_name]:
                        info += node_dict[on].suc_cons[con_name][sn]
                    
                    #relation is potentially not in the old_node, but in the new one
                    if len(info) == 0: 
                        if rn in red_node_dict and on in red_node_dict[rn].suc_cons[con_name]:
                            info += red_node_dict[rn].suc_cons[con_name][on]    
                        if on in red_node_dict and sn in red_node_dict[on].suc_cons[con_name]:
                            info += red_node_dict[on].suc_cons[con_name][sn]
                    
                    if len(info) > 0:
                        if sn in cons and not branched:
                            cons[sn] += info
                        else:
                            cons[sn] = info
                    
                #lazily delete old connection
                lazy_del_set.add((sn,on,))  
        
        def copy_successors(red_node_dict, node_dict, copy, origin, child, merge_comps = False):
            cpn = copy.name
            chn = child.name
            
            #establish child's predecessors
            if cpn in node_dict[chn].predecessors:
                if copy.name in child.predecessors:
                    if child.predecessors[cpn] > 0  and node_dict[chn].predecessors[cpn] > 0:
                        child.predecessors[cpn] += node_dict[chn].predecessors[cpn]
                    elif node_dict[chn].predecessors[cpn] < 0:
                        child.predecessors[cpn] = node_dict[chn].predecessors[cpn]
                else:
                    child.predecessors[cpn] = node_dict[chn].predecessors[cpn]
            elif cpn in red_node_dict[chn].predecessors:
                if copy.name in child.predecessors:
                    if child.predecessors[cpn] > 0  and red_node_dict[chn].predecessors[cpn] > 0:
                        child.predecessors[cpn] += red_node_dict[chn].predecessors[cpn]
                    elif red_node_dict[chn].predecessors[cpn] < 0:
                        child.predecessors[cpn] = red_node_dict[chn].predecessors[cpn]
                else:
                    child.predecessors[cpn] = red_node_dict[chn].predecessors[cpn]
                    
            #establish child's special relations to predecessor
            for con_name,cons in child.pred_cons.iteritems():
                if cpn in node_dict[chn].pred_cons[con_name] and len(node_dict[chn].pred_cons[con_name][cpn]) > 0:
                    if cpn in cons:
                        cons[cpn] += node_dict[chn].pred_cons[con_name][cpn]
                    else:
                        cons[cpn] = node_dict[chn].pred_cons[con_name][cpn]
                elif cpn in red_node_dict[chn].pred_cons[con_name] and len(red_node_dict[chn].pred_cons[con_name][cpn]) > 0:
                    if cpn in cons:
                        cons[cpn] += red_node_dict[chn].pred_cons[con_name][cpn]
                    else:
                        cons[cpn] = red_node_dict[chn].pred_cons[con_name][cpn]
                
            #establish successors
            if chn in node_dict[origin.name].successors:                
                if chn in copy.successors:
                    if copy.successors[chn] > 0 and node_dict[origin.name].successors[chn] > 0:
                        copy.successors[chn] += node_dict[origin.name].successors[chn]
                    elif origin.successors[chn] < 0:
                        copy.successors[chn] = node_dict[origin.name].successors[chn]    
                else:
                    copy.successors[chn] = node_dict[origin.name].successors[chn]
            elif chn in red_node_dict[origin.name].successors:                
                if chn in copy.successors and not merge_comps:
                    if copy.successors[chn] > 0 and red_node_dict[origin.name].successors[chn] > 0:
                        copy.successors[chn] += red_node_dict[origin.name].successors[chn]
                    elif chn in origin.successors and origin.successors[chn] < 0:
                        copy.successors[chn] = red_node_dict[origin.name].successors[chn]   
                else:
                    copy.successors[chn] = red_node_dict[origin.name].successors[chn]
            
            #establish special relations to successors
            for con_name,cons in copy.suc_cons.iteritems():
                if chn in node_dict[origin.name].suc_cons[con_name] and node_dict[origin.name].suc_cons[con_name][chn] > 0:
                    if chn in cons:
                        cons[chn] += node_dict[origin.name].suc_cons[con_name][chn]
                    else:
                        cons[chn] = node_dict[origin.name].suc_cons[con_name][chn]

        def aggregate_redundant_nodes(node_dict,starting_rule):  

            def test_mergeable(node_dict,old_node,new_child):
                cname = new_child.name
                if (    cname in old_node.suc_cons["comp"] or
                        cname in old_node.suc_cons["split"] or
                        cname in old_node.suc_cons["prob"] or
                        cname in old_node.suc_cons["case"]):
                    return False
                else:
                    if len(node_dict[new_child.name].predecessors) == 1:
                        return True
                    else:
                        return False
                
            def gather_finite_attrs(node_dict, attrs, root_name, node_name):
                node = node_dict[node_name]
                new_attrs = {}
                for suc_name in node.successors:
                    gather_finite_attrs(node_dict, new_attrs, node_name, suc_name)
                
                if node.attrs:
                    for attr in node.attrs:
                        if attr in new_attrs:
                            new_attrs[attr] += node.attrs[attr]
                        else:
                            new_attrs[attr] = node.attrs[attr]
                            
                for attr in new_attrs:
                    if attr in attrs:
                        attrs[attr] += new_attrs[attr] * node.predecessors[root_name]
                    else:
                        attrs[attr] = new_attrs[attr] * node.predecessors[root_name]
                
                return attrs
                            
            
            def reduce_successors(reduced_node_dict, node_dict, s_rule,lazy_del_set):
                if s_rule in reduced_node_dict:
                    return reduced_node_dict[s_rule]
                old_node = node_dict[s_rule]
                
                rep_pred = False
                for pred_name, count in old_node.predecessors.iteritems():
                    if count < 0:
                        rep_pred = True
                        break
                
                if not old_node.successors and not old_node.attrs and not rep_pred:#remove empty leaves
                    return None
                elif not old_node.successors and old_node.attrs:#keep leaves with attrs
                    new_leave = Node(old_node.name, attrs = old_node.attrs)
                    reduced_node_dict[new_leave.name] = new_leave
                    return new_leave
                elif not old_node.successors and rep_pred:#structural nodes
                    new_leave = Node(old_node.name, attrs = old_node.attrs)
                    reduced_node_dict[new_leave.name] = new_leave
                    return new_leave
                    
                new_node = Node(old_node.name, old_node.attrs)
                
                #gather children
                children = []
                for suc_name,count in old_node.successors.iteritems():#collect valid children
                    #Gather attributes from finite children
                    if not node_dict[suc_name].rep_trans_suc and node_dict[suc_name].predecessors[old_node.name]>0:
                        old_child = node_dict[suc_name]
                        min_mult = float("inf")
                        for pred_name, count in old_child.predecessors.iteritems():
                            min_mult = min(min_mult,count)
                        if min_mult > 0:
                            gather_finite_attrs(node_dict, new_node.attrs,new_node.name,suc_name)
                            continue
                    suc = reduce_successors(reduced_node_dict, node_dict, suc_name,lazy_del_set)
                    if suc:
                        children.append(suc)
                if not children and not old_node.attrs and not rep_pred:
                    return None
                
                #update multiplicity of node
                new_node.rep_trans_suc = node_dict[old_node.name].rep_trans_suc
                for child in children:
                    new_node.rep_trans_suc = new_node.rep_trans_suc or reduced_node_dict[child.name].rep_trans_suc 
                
                #process children
                for child in children:
                    mergeable = test_mergeable(node_dict,old_node,child)
                    if mergeable:
                        merge_dicts(new_node.attrs,child.attrs)
                        transfer_childs_successors(new_node, child,  lazy_del_set, node_dict, reduced_node_dict)
                    else:
                        copy_successors(reduced_node_dict, node_dict, new_node, old_node, child)
                
                if not new_node.attrs and not new_node.successors and not rep_pred:
                    return None
                reduced_node_dict[new_node.name] = new_node
                return new_node
            
            print "PHASE 1: Aggregate redundant nodes"
            reduced_node_dict = {}
            lazy_del_set = set()
            reduce_successors(reduced_node_dict, node_dict, starting_rule,lazy_del_set)
            remove_nodes_lazily(reduced_node_dict, node_dict,lazy_del_set)
            return reduced_node_dict
            
        def aggregate_redundant_cases(node_dict,starting_rule):  

            def test_mergeable(node_dict,old_node,new_child):
                probs = old_node.suc_cons["prob"]
                cases = old_node.suc_cons["case"]
                if (    (len(probs) == 1 and new_child.name in probs and len (probs[new_child.name]) > 0) or
                        (len(cases) == 1 and new_child.name in cases and len (cases[new_child.name]) > 0)):
                    return True
                elif (  len(probs) > 1 or
                        len(cases) > 1): #check that all branches unit in a common successor (only to depth = 1)
                    branch_sucs = {}
                    branch_len = len(old_node.successors)
                    for suc_name in old_node.successors:
                        suc = node_dict[suc_name]
                        for suc2_name,count in suc.successors.iteritems():
                            if suc2_name in branch_sucs:
                                branch_sucs[suc2_name] += 1
                            else:
                                branch_sucs[suc2_name] = 1
                    mergeable = True
                    for key, val in branch_sucs.iteritems():
                        if val != branch_len:
                            mergeable = True
                    return mergeable
                else:
                    return False                            
            
            def reduce_successors(reduced_node_dict, node_dict, s_rule,lazy_del_set):
                if s_rule in reduced_node_dict:
                    return reduced_node_dict[s_rule]
                old_node = node_dict[s_rule]
                
                new_node = Node(old_node.name, old_node.attrs)
                
                #gather children
                children = []
                for suc_name,count in old_node.successors.iteritems():#collect valid children
                    suc = reduce_successors(reduced_node_dict, node_dict, suc_name,lazy_del_set)
                    if suc:
                        children.append(suc)
                
                #update multiplicity of node
                new_node.rep_trans_suc = node_dict[old_node.name].rep_trans_suc
                for child in children:
                    new_node.rep_trans_suc = new_node.rep_trans_suc or reduced_node_dict[child.name].rep_trans_suc 
                    
                #process children
                for child in children:
                    mergeable = test_mergeable(node_dict,old_node,child)
                    if mergeable:
                        merge_dicts(new_node.attrs,child.attrs)
                        branched = True
                        transfer_childs_successors(new_node, child,  lazy_del_set, node_dict, reduced_node_dict,branched)
                    else:
                        copy_successors(reduced_node_dict, node_dict, new_node, old_node, child)
                
                reduced_node_dict[new_node.name] = new_node
                return new_node
            
            print "PHASE 2: Aggregate redundant cases/probs"
            reduced_node_dict = {}
            lazy_del_set = set()
            reduce_successors(reduced_node_dict, node_dict, starting_rule,lazy_del_set)
            remove_nodes_lazily(reduced_node_dict, node_dict,lazy_del_set)
            return reduced_node_dict
            
        def aggregate_redundant_comps(node_dict,starting_rule):  

            def test_mergeable(node_dict,old_node,new_child):
                probs = old_node.suc_cons["prob"]
                cases = old_node.suc_cons["case"]
                if (    (len(probs) == 1 and new_child.name in probs and len (probs[new_child.name]) > 0) or
                        (len(cases) == 1 and new_child.name in cases and len (cases[new_child.name]) > 0)):
                    return True
                elif (  len(probs) > 1 or
                        len(cases) > 1): #check that all branches unit in a common successor (only to depth = 1)
                    branch_sucs = {}
                    branch_len = len(old_node.successors)
                    for suc_name in old_node.successors:
                        suc = node_dict[suc_name]
                        for suc2_name in suc.successors:
                            if suc2_name in branch_sucs:
                                branch_sucs[suc2_name] += 1
                            else:
                                branch_sucs[suc2_name] = 1
                    for key, val in branch_sucs.iteritems():
                        if val != branch_len:
                            return False
                    return True
                else:
                    return False      

            def find_splits(reduced_node_dict, node_dict, s_rule):
                old_node = node_dict[s_rule]
                if len(old_node.suc_cons["split"]) > 0:
                    return set([old_node.name])
                split_nodes = set()
                for suc_name,count in old_node.successors.iteritems():#collect valid children
                    prev = find_splits(reduced_node_dict, node_dict, suc_name)
                    split_nodes = split_nodes | prev
                return split_nodes
                
            def gather_attrs_until_split(node_dict, split_nodes, attrs, root_name, node_name):
                reduced = False
                for split_node in split_nodes:
                    if split_node == node_name:
                        reduced = True
                        break
                
                node = node_dict[node_name]
                new_attrs = {}
                if not reduced:
                    for suc_name in node.successors:
                        gather_attrs_until_split(node_dict, split_nodes, new_attrs, node_name, suc_name)
                
                    if node.attrs:
                        for attr in node.attrs:
                            if attr in new_attrs:
                                new_attrs[attr] += node.attrs[attr]
                            else:
                                new_attrs[attr] = node.attrs[attr]
                            
                for attr in new_attrs:
                    if attr in attrs:
                        attrs[attr] += new_attrs[attr] * node.predecessors[root_name]
                    else:
                        attrs[attr] = new_attrs[attr] * node.predecessors[root_name]
                
                return attrs
                
            def gather_nodes_until_split(reduced_node_dict, node_dict, s_rule, split_nodes):
                
                old_node = node_dict[s_rule]
                for suc_name in old_node.successors:
                    if suc_name in split_nodes:
                        return None
                        
                new_node = Node(old_node.name, old_node.attrs)
                
                children = []
                for suc_name in old_node.successors:
                    sup = gather_nodes_until_split(reduced_node_dict, node_dict, suc_name, split_nodes)
                    if sup:
                        child = reduced_node_dict[suc_name]
                    else:
                        child = node_dict[suc_name]
                    children.append(child)
                    if new_node.name == "SideFacade" or child.name == "SideFacade":
                        a= 1
                for child in children:
                    transfer_childs_successors(new_node, child,  lazy_del_set, node_dict, reduced_node_dict)
                
                reduced_node_dict[new_node.name] = new_node
                return new_node

            def reduce_until_splits(reduced_node_dict, node_dict, s_rule, split_nodes, lazy_del_set):
                
                old_node = node_dict[s_rule]
                new_node = gather_nodes_until_split(reduced_node_dict, node_dict, s_rule, split_nodes)
                if not new_node:
                    return None
                    
                for suc_name,val in old_node.successors.iteritems():
                    gather_attrs_until_split(node_dict, split_nodes, new_node.attrs, s_rule, suc_name)
                         
                children = []
                for split_node in split_nodes:
                    children.append(node_dict[split_node])
                                    
                new_node.rep_trans_suc = node_dict[old_node.name].rep_trans_suc
                merge_comps = True
                for child in children:
                    new_node.rep_trans_suc = new_node.rep_trans_suc or reduced_node_dict[child.name].rep_trans_suc
                    
                    copy_successors(reduced_node_dict, node_dict, new_node, old_node, child, merge_comps)
                    child.pred_cons["comp"][new_node.name] = "f"
                    new_node.suc_cons["comp"][child.name] = "f"
                    if new_node.name in child.pred_cons["case"]:
                        del child.pred_cons["case"][new_node.name]
                    if child.name in new_node.suc_cons["case"]:
                        del new_node.suc_cons["case"][child.name]
                    if new_node.name in child.pred_cons["prob"]:
                        del child.pred_cons["prob"][new_node.name]
                    if child.name in new_node.suc_cons["prob"]:
                        del new_node.suc_cons["prob"][child.name]
                
                reduced_node_dict[new_node.name] = new_node
                return new_node
      
            
            def reduce_successors(reduced_node_dict, node_dict, s_rule,lazy_del_set):
                if s_rule in reduced_node_dict:
                    return reduced_node_dict[s_rule]
                old_node = node_dict[s_rule]
                
                new_node = Node(old_node.name, old_node.attrs)
                
                
                should_reduce = True
                children = []
                if len(old_node.suc_cons["comp"]) > 0 and old_node.rep_trans_suc:                
                    #gather children until split 
                    split_nodes = find_splits(reduced_node_dict, node_dict, s_rule)
                    for suc_name,node in old_node.successors.iteritems():
                        if suc_name in split_nodes:
                            should_reduce = False
                    for suc_name in split_nodes:#collect valid children
                        suc = reduce_successors(reduced_node_dict, node_dict, suc_name,lazy_del_set)
                        if suc:
                            children.append(suc)
                    if should_reduce:
                        reduce_until_splits(reduced_node_dict, node_dict, s_rule,split_nodes,lazy_del_set)
                else:
                    should_reduce = False
                    #gather children
                    for suc_name,count in old_node.successors.iteritems():#collect valid children
                        suc = reduce_successors(reduced_node_dict, node_dict, suc_name,lazy_del_set)
                        if suc:
                            children.append(suc)
                 
                    
                #update multiplicity of node
                new_node.rep_trans_suc = node_dict[old_node.name].rep_trans_suc
                for child in children:
                    new_node.rep_trans_suc = new_node.rep_trans_suc or reduced_node_dict[child.name].rep_trans_suc  
                    copy_successors(reduced_node_dict, node_dict, new_node, old_node, child)
                if not should_reduce:
                    reduced_node_dict[new_node.name] = new_node
                return new_node
            
            print "PHASE 3: Aggregate redundant comps"
            reduced_node_dict = {}
            lazy_del_set = set()
            reduce_successors(reduced_node_dict, node_dict, starting_rule,lazy_del_set)
            remove_nodes_lazily(reduced_node_dict, node_dict,lazy_del_set)
            return reduced_node_dict
        
        def aggregate_redundant_splits(node_dict,starting_rule):
            def reduce_successors(reduced_node_dict, node_dict, s_rule,lazy_del_set, processed_splits):
                print "Inspecting " + s_rule
                if s_rule in reduced_node_dict:
                    return reduced_node_dict[s_rule]
                old_node = node_dict[s_rule]
                
                new_node = Node(old_node.name, old_node.attrs)
                
                children = []
                children_splits = {}
                for suc_name,node in old_node.successors.iteritems():
                    split_dir = None
                    if suc_name in old_node.suc_cons["split"]:
                        split_dir = old_node.suc_cons["split"][suc_name]
                        children_splits[suc_name] = split_dir
                        print "Successor " + suc_name + " has split " + split_dir
                    
                    suc = reduce_successors(reduced_node_dict, node_dict, suc_name,lazy_del_set,processed_splits)
                    children.append(suc)            
                    
                new_node.rep_trans_suc = node_dict[old_node.name].rep_trans_suc
                multiplicity = {}
                if s_rule == "TileRow":
                    a= 1
                lazy_child_removal = []
                for child in children:
                    copy_directly = False
                    if not child.name in processed_splits:
                        new_node.rep_trans_suc = new_node.rep_trans_suc or reduced_node_dict[child.name].rep_trans_suc 
                        cur_split = None
                        if child.name in old_node.suc_cons["split"]:
                            cur_split =  old_node.suc_cons["split"][child.name]
                        transfer_list = []
                        for child2_name, child2_count in child.successors.iteritems():
                            child2 = reduced_node_dict[child2_name]
                            if child2_name in child.suc_cons["split"] and child.suc_cons["split"][child2_name] == cur_split:
                                if len(child2.successors) > 0:
                                    transfer_list.append((child,child2,))
                                elif child.successors[child2_name]<0:
                                    print "Transfering: \n " + str(child) + "to \n" + str(new_node)
                                    copy_directly = True
                        
                        if len(transfer_list)>0:
                            copy_directly = False
                        for transfer in transfer_list:
                            child = transfer[0]
                            child2 = transfer[1]
                            if child2.name == "Floor_yxxy":
                                a=1
                            print "Transfering: \n " + str(child2) + "to \n" + str(child)
                            transfer_childs_successors(child, child2,  lazy_del_set, node_dict, reduced_node_dict)
                            for suc_name, splits in child.suc_cons["split"].iteritems():
                                slen = len(splits)
                                child.suc_cons["split"][suc_name] = splits[slen-1:slen]
                            
                            old_child = node_dict[child.name]
                            for suc_name, count in old_child.successors.iteritems():
                                if count < 0:
                                    if old_child.suc_cons["split"][suc_name] != child.suc_cons["split"][suc_name]:
                                        child.suc_cons["split"][suc_name] = old_child.suc_cons["split"][suc_name]
                                #TODO does not work if child 2 had the same child with infinite multiplicity but different split
                            multiplicity[child.name] = child.successors[child2.name]
                            #new_node.suc_cons["split"][child.name] = cur_split
                            if new_node.name in child.pred_cons["case"]:
                                del child.pred_cons["case"][new_node.name]
                            if child.name in new_node.suc_cons["case"]:
                                del new_node.suc_cons["case"][child.name]
                            if new_node.name in child2.pred_cons["prob"]:
                                del child.pred_cons["prob"][new_node.name]
                            if child.name in new_node.suc_cons["prob"]:
                                del new_node.suc_cons["prob"][child.name]
                            lazy_del_set.add((child2.name,child.name,))
                            processed_splits[s_rule] = (child.name,child2.name)
                    if copy_directly:
                        if child =="Floor_yxxy":
                            a=1
                        transfer_childs_successors(new_node, child,  lazy_del_set, node_dict, reduced_node_dict)
                        lazy_child_removal.append(child)
                for child in lazy_child_removal:
                    children.remove(child)
                if len(lazy_del_set)>0:
                    remove_nodes_lazily(reduced_node_dict, node_dict,lazy_del_set,[c.name for c in children])
                    keys = list(lazy_del_set)
                    for key in keys:
                        lazy_del_set.discard(key)
                for child in children:
                    copy_successors(reduced_node_dict, node_dict, new_node, old_node, reduced_node_dict[child.name])
                for node_name,value in multiplicity.iteritems():
                    reduced_node_dict[node_name].predecessors[new_node.name] *= value
                    new_node.successors[node_name] *= value
                reduced_node_dict[new_node.name] = new_node
                return reduced_node_dict[new_node.name]
            
            print "PHASE 4: Aggregate redundant splits"
            reduced_node_dict = {}
            lazy_del_set = set()
            processed_splits = {}
            reduce_successors(reduced_node_dict, node_dict, starting_rule,lazy_del_set,processed_splits)
            remove_nodes_lazily(reduced_node_dict, node_dict,lazy_del_set)
            return reduced_node_dict
    
        def aggregate_redundant_split_children(node_dict,starting_rule):  

            def test_mergeable(node_dict,old_node,new_child):
                cname = new_child.name
                if (    cname in old_node.suc_cons["comp"] or
                        (cname in old_node.suc_cons["split"] and old_node.successors[cname] < 0)):
                    return False
                else:
                    if len(node_dict[new_child.name].predecessors) >= 1 and not new_child.rep_trans_suc:
                        return True
                    else:
                        return False
                
            def gather_finite_attrs(node_dict, attrs, root_name, node_name):
                node = node_dict[node_name]
                new_attrs = {}
                for suc_name in node.successors:
                    gather_finite_attrs(node_dict, new_attrs, node_name, suc_name)
                
                if node.attrs:
                    for attr in node.attrs:
                        if attr in new_attrs:
                            new_attrs[attr] += node.attrs[attr]
                        else:
                            new_attrs[attr] = node.attrs[attr]
                            
                for attr in new_attrs:
                    if attr in attrs:
                        attrs[attr] += new_attrs[attr] * node.predecessors[root_name]
                    else:
                        attrs[attr] = new_attrs[attr] * node.predecessors[root_name]
                
                return attrs
                            
            
            def reduce_successors(reduced_node_dict, node_dict, s_rule,lazy_del_set):
                if s_rule in reduced_node_dict:
                    return reduced_node_dict[s_rule]
                old_node = node_dict[s_rule]
                
                rep_pred = False
                for pred_name, count in old_node.predecessors.iteritems():
                    if count < 0:
                        rep_pred = True
                        break
                
                if not old_node.successors and not old_node.attrs and not rep_pred:#remove empty leaves
                    return None
                elif not old_node.successors and old_node.attrs:#keep leaves with attrs
                    new_leave = Node(old_node.name, attrs = old_node.attrs)
                    reduced_node_dict[new_leave.name] = new_leave
                    return new_leave
                elif not old_node.successors and rep_pred:#structural nodes
                    new_leave = Node(old_node.name, attrs = old_node.attrs)
                    reduced_node_dict[new_leave.name] = new_leave
                    return new_leave
                    
                new_node = Node(old_node.name, old_node.attrs)
                
                #gather children
                children = []
                for suc_name,count in old_node.successors.iteritems():#collect valid children
                    #Gather attributes from finite children
                    if not node_dict[suc_name].rep_trans_suc and node_dict[suc_name].predecessors[old_node.name]>0:
                        old_child = node_dict[suc_name]
                        min_mult = float("inf")
                        for pred_name, count in old_child.predecessors.iteritems():
                            min_mult = min(min_mult,count)
                        if min_mult > 0:
                            gather_finite_attrs(node_dict, new_node.attrs,new_node.name,suc_name)
                            continue
                    suc = reduce_successors(reduced_node_dict, node_dict, suc_name,lazy_del_set)
                    if suc:
                        children.append(suc)
                if not children and not old_node.attrs and not rep_pred:
                    return None
                
                #update multiplicity of node
                new_node.rep_trans_suc = node_dict[old_node.name].rep_trans_suc
                for child in children:
                    new_node.rep_trans_suc = new_node.rep_trans_suc or reduced_node_dict[child.name].rep_trans_suc 
                
                #process children
                for child in children:
                    mergeable = test_mergeable(node_dict,old_node,child)
                    if mergeable:
                        merge_dicts(new_node.attrs,child.attrs)
                        transfer_childs_successors(new_node, child,  lazy_del_set, node_dict, reduced_node_dict)
                    else:
                        copy_successors(reduced_node_dict, node_dict, new_node, old_node, child)
                
                if not new_node.attrs and not new_node.successors and not rep_pred:
                    return None
                reduced_node_dict[new_node.name] = new_node
                return new_node
            
            print "PHASE 5: Aggregate redundant split children"
            reduced_node_dict = {}
            lazy_del_set = set()
            reduce_successors(reduced_node_dict, node_dict, starting_rule,lazy_del_set)
            remove_nodes_lazily(reduced_node_dict, node_dict,lazy_del_set)
            return reduced_node_dict
            
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
        visited1 = []
        starting_rules = []
        for rule, deps in self.graph.iteritems():
            if rule not in visited1:
                if rule in mapping_from:
                    mark_visited(rule,mapping_from,visited1,starting_rules)
                else:
                    starting_rules.append(rule)
                    visited1.append(rule)
        
        structure_graphs = {}
        rsg1 = {} 
        rsg2 = {} 
        rsg3 = {} 
        rsg4 = {} 
        rsg5 = {} 
        rsg_list = [rsg1,rsg2,rsg3,rsg4,rsg5]#,rsg6,rsg7]
        for starting_rule in starting_rules:
            structure_graphs[starting_rule] = get_structure_graph(mapping_to,mapping_from,starting_rule)
            rsg1[starting_rule] = aggregate_redundant_nodes(structure_graphs[starting_rule],starting_rule)
            rsg2[starting_rule] = aggregate_redundant_cases(rsg1[starting_rule],starting_rule)
            rsg3[starting_rule] = aggregate_redundant_comps(rsg2[starting_rule],starting_rule)
            rsg4[starting_rule] = aggregate_redundant_splits(rsg3[starting_rule],starting_rule)
            rsg5[starting_rule] = aggregate_redundant_split_children(rsg4[starting_rule],starting_rule)
        
        index = 1
        for rsg in rsg_list:      
            visited = {}
            labels = ""
            output = "digraph {\n"
            for starting_rule in starting_rules:
                node_dict = rsg[starting_rule]
                rule = node_dict[starting_rule]
                if rule is None:
                    continue
                rule_stack = [rule]
                while rule_stack:
                    rule_node = rule_stack.pop()
                    if rule_node.name in visited:
                        continue
                    else:
                        visited[rule_node.name]=1
                    for suc_rule,count in rule_node.successors.iteritems():
                        rule_stack.append(node_dict[suc_rule])
                    deps = rule_node.successors
                    rule = rule_node.name
                    labels += "    " + node_name(rule) + ' [label=<"' + rule + '"'
                    for attr in rule_node.attrs:
                        if attr in self.attrs:
                            if rule_node.attrs[attr] > 0:
                                labels += '\n<BR /><FONT POINT-SIZE="10">'+attr+':'+str(rule_node.attrs[attr])+'</FONT>'
                            else:
                                labels += '\n<BR /><FONT POINT-SIZE="10">'+attr+':n</FONT>'
                    labels +='>]\n'
                    for d in deps:
                        output += '    ' + node_name(rule) + ' -> ' + node_name(d)
                        output += '[label=<'
                        if rule_node.successors[d] > 0:
                            output += str(rule_node.successors[d])+ ''
                        else:
                            output += 'n'
                        for con_name, cons in rule_node.suc_cons.iteritems():
                            if d in cons and len(cons[d]) > 0:
                                output += '; '+cons[d]
                        output += '>]\n'
                                
                output += labels
            output += "}"

            with file(file_name[0:3] + file_name[3:] + "_rsg" + str(index)+ ".gv", 'w') as out:
                out.write(output)
                
            index += 1

    def write_dot(self, file_name):
        self.write_slightly_reduced_dot(file_name)
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
            labels +='>]\n'
            for d in deps:
                output += '    ' + node_name(rule) + ' -> ' + node_name(d)
                output += '[label=<'
                if d in self.count[rule]:
                    if self.count[rule][d] > 0:
                        output += str(self.count[rule][d])+ ''
                    else:
                        output += 'n'
                else:
                    output += '1'
                for con_name, cons in self.connections.iteritems():
                    if rule in cons and d in cons[rule]:
                        output += '; '+cons[rule][d]
                output += '>]\n'
                    
        output += labels
        output += "}"

        with file(file_name + ".gv", 'w') as out:
            out.write(output)

    def write_pdf(self, file_name):
        file_name2 = "gv/"+file_name
        self.write_dot(file_name2)
        os.system("dot -Tpdf " + file_name2 + ".gv -o " + file_name2 + ".pdf")
