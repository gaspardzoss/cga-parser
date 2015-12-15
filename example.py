#!/usr/bin/env python
import sys
from cga import CGAScanner, CGAParser, GrammarBuilder

if len(sys.argv) != 2:
    print "Usage: python example.py example.cga"
    sys.exit(0)

filename = sys.argv[1]
f = open(filename)
input = f.read()
f.close()

scanner = CGAScanner()
tokens = scanner.tokenize(input)
# print tokens

parser = CGAParser()
ast =  parser.parse(tokens)
# print ast

builder = GrammarBuilder(ast)
grammar = builder.grammar
print grammar

#structure_graph_builder = StructureGraphBuilder(ast)
#structure_graph_builder.write_dot(sys.argv[1][:-4] + ".gv")
#print structure_graph_builder.dependencies

