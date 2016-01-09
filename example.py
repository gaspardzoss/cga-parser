#!/usr/bin/env python
import sys
from cga import CGAScanner, CGAParser, GrammarBuilder, StructureGraphBuilder

if len(sys.argv) != 2:
    print "Usage: python example.py example.cga"
    sys.exit(0)

filename = sys.argv[1]
f = open("cga/"+ filename)
input = f.read()
f.close()

scanner = CGAScanner()
tokens = scanner.tokenize(input)
#print tokens

parser = CGAParser()
ast =  parser.parse(tokens)
#print ast

builder = GrammarBuilder(ast)
grammar = builder.grammar
#print grammar

debug = True
graph_builder = StructureGraphBuilder(grammar,debug=debug)
#print graph_builder.graph
#graph_builder.write_dot(sys.argv[1][:-4])
graph_builder.write_pdf(sys.argv[1][:-4])

