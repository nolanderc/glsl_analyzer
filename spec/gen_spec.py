#!/usr/bin/env python3

import sys
import os
from glob import iglob
import json
from bs4 import BeautifulSoup
import progressbar
import tokenize

variables = []
functions = []

def process_file(path):
    is_variable = os.path.basename(path).startswith('gl_')

    with open(path, 'r') as f:
        soup = BeautifulSoup(str(f.read()), 'html.parser')

        desc_node = soup.find(id='description')
        if desc_node is None: return
        paragraphs = desc_node.find_all('p')
        description = [' '.join(p.getText().split()) for p in paragraphs]

        versions_table = soup.find(id='versions')
        versions_header = versions_table.find('thead').find_all('tr')[-1]
        versions = [int(v.getText().replace('.', '')) for v in versions_header.find_all('th')[1:]]

        versions_body = versions_table.find('tbody').find_all('tr')
        version_support = []
        for row in versions_body:
            datas = row.find_all('td')
            supported = [v for v, data in zip(versions, datas[1:]) if data.getText().strip() != '-']
            version_support.append(supported)

        if is_variable:
            for node in soup.find_all(attrs={'class':'fieldsynopsis'}):
                variable = parse_variable(node)
                variable['description'] = description
                variable['versions'] = version_support[0]
                variables.append(variable)
        else:
            for i, node in enumerate(soup.find_all(attrs={'class':'funcprototype-table'})):
                prototype = parse_prototype(node)
                prototype['description'] = description
                prototype['versions'] = version_support[min(i, len(version_support) - 1)]
                functions.append(prototype)

def parse_variable(node):
    tokens = tokenize(node.getText())

    variable = {}
    variable['modifier'] = tokens[0]
    variable['type'] = tokens[1]
    variable['name'] = tokens[2]
    variable['type'] += ''.join(tokens[3:-1])
    assert tokens[-1] == ';'

    return variable

def parse_prototype(node):
    tokens = tokenize(node.getText())

    func_output = tokens[0]
    func_name = tokens[1]
    assert tokens[2] == '('

    parameters = []
    i = 3
    while i < len(tokens) - 2:
        parameter = {}

        if tokens[i] == 'out' or tokens[i] == 'in' or tokens[i] == 'inout':
            parameter['modifier'] = tokens[i]
            i += 1

        param_type = tokens[i]
        if param_type == 'void': break
        parameter['type'] = param_type
        i += 1

        param_name = tokens[i]
        if param_name[0].isalnum():
            parameter['name'] = param_name
            i += 1

        parameters.append(parameter)

        if tokens[i] == ',': i += 1

    assert tokens[-2] == ')'
    assert tokens[-1] == ';'

    return {
        'return':func_output,
        'name':func_name,
        'parameters':parameters,
    }


def tokenize(text):
    i = 0
    N = len(text)
    tokens = []
    while i < N:
        if text[i].isspace(): i += 1; continue

        if text[i].isalnum():
            start = i
            i += 1
            while i < N and (text[i].isalnum() or text[i] == '_'): i += 1
            tokens.append(text[start:i])
            continue

        tokens.append(text[i])
        i += 1;
    return tokens


scriptdir = os.path.dirname(sys.argv[0])
files = [f for f in iglob(f'{scriptdir}/docs.gl/sl4/*.xhtml')]

for i, path in enumerate(files):
    print(f'{1+i:3}/{len(files)}: {path}')
    process_file(path)

with open(f'{scriptdir}/spec.json', 'w') as f:
    f.write(json.dumps({
        'comment': 'generated from docs.gl',
        'variables':variables,
        'functions':functions,
    }, indent=4))
