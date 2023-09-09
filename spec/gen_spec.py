#!/usr/bin/env python3

import sys
import os
from glob import iglob
import json
from bs4 import BeautifulSoup
import progressbar
import tokenize
import re

variables = []
functions = []

def process_docs_gl_file(path):
    is_variable = os.path.basename(path).startswith('gl_')

    with open(path, 'r') as f:
        soup = BeautifulSoup(str(f.read()), 'html.parser')

        desc_node = soup.find(id='description')
        if desc_node is None: return
        paragraphs = desc_node.find_all('p')
        description = [paragraph_to_markdown(p) for p in paragraphs]

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
                variable = parse_variable(node.getText())
                variable['description'] = description
                variable['versions'] = version_support[0]
                variables.append(variable)
        else:
            for i, node in enumerate(soup.find_all(attrs={'class':'funcprototype-table'})):
                prototype = parse_prototype(node.getText())
                prototype['description'] = description
                prototype['versions'] = version_support[min(i, len(version_support) - 1)]
                functions.append(prototype)


def paragraph_to_markdown(paragraph):
    if paragraph.math is not None and paragraph.math.mtable is not None:
        return '```\n' + expand_math(paragraph.math.mtable) + '\n```\n'

    for tag in paragraph.find_all('em'):
        tag.replace_with('_' + tag.getText() + '_')

    for tag in paragraph.find_all('code'):
        tag.replace_with('`' + tag.getText() + '`')

    for tag in paragraph.find_all('math'):
        tag.replace_with('`' + escape_math(tag) + '`')

    return ' '.join(paragraph.getText().split())

def math_children(node):
    children = []
    for child in node.children:
        if child.name is None: continue
        children.append(child)
    return children

def escape_math(node):
    return ' '.join(expand_math(node).split(" \t\r")).replace('δ ', 'δ')

def expand_math(node):
    if node.name is None or node.name in ['mi', 'mn', 'mo']:
        text = node.getText().strip()
        if text == '\u2061': return ''
        return text

    if node.name == 'mrow' or node.name == 'math':
        return ' '.join([expand_math(c) for c in node.children])

    if node.name == 'mfrac':
        parts = math_children(node)
        return f'{expand_math(parts[0])} / {expand_math(parts[1])}'

    if node.name == 'mfenced':
        open = node['open']
        close = node['close']
        return open + ' '.join([expand_math(c) for c in node.children]).strip() + close

    if node.name == 'msup':
        parts = math_children(node)
        return expand_math(parts[0]) + '**' + expand_math(parts[1])

    if node.name == 'msub':
        parts = math_children(node)
        return expand_math(parts[0]) + '_' + expand_math(parts[1])

    if node.name == 'msubsup':
        parts = math_children(node)
        return (expand_math(parts[0])
            + '_' + expand_math(parts[1])
            + '^' + expand_math(parts[2]))

    if node.name == 'mtable':
        rows = node.find_all('mtr')
        res = ''
        for row in rows:
            cols = row.find_all('mtd')
            for col in cols:
                res += ' '.join([expand_math(c) for c in col.children]) + '  '
            res += '\n'
        return res

    if node.name == 'msqrt':
        return f'sqrt({expand_math(node.contents[0])})'

    raise Exception(f'unknown math node {node.name}: {node}')

modifier_keywords = ['out', 'in', 'inout', 'const', 'highp', 'lowp', 'perprimitiveEXT']
modifier_regex = '(' + '|'.join(modifier_keywords) + ')'

def parse_variable(text):
    tokens = tokenize(text)

    variable = {}

    i = 0
    while tokens[i] in modifier_keywords:
        if 'modifiers' in variable: variable['modifiers'] += ' ' + tokens[i]
        else: variable['modifiers'] = tokens[i]
        i += 1

    variable['type'] = tokens[i]; i += 1
    variable['name'] = tokens[i]; i += 1

    if tokens[i] == '[': 
        start = i
        while tokens[i] != ']': i += 1
        i += 1
        variable['type'] += ''.join(tokens[start:i])

    if tokens[i] == '=': 
        i += 1
        start = i
        while tokens[i] != ';': i += 1
        variable['default_value'] = ' '.join(tokens[start:i])

    assert tokens[i] == ';'

    return variable

def parse_prototype(text):
    tokens = tokenize(text)

    func_output = tokens[0]
    func_name = tokens[1]
    i = 2
    while i < len(tokens) and tokens[i] != '(':
        func_output = func_name
        func_name = tokens[i]
        i += 1
    i += 1

    parameters = []
    while i < len(tokens) - 2:
        parameter = {}

        optional = tokens[i] == '['
        if optional: 
            i += 1
            parameter['optional'] = True

        while tokens[i] in modifier_keywords:
            if 'modifiers' in parameter: parameter['modifiers'] += ' ' + tokens[i]
            else: parameter['modifiers'] = tokens[i]
            i += 1

        param_type = tokens[i]
        if param_type == 'void': break
        parameter['type'] = param_type
        i += 1

        param_name = tokens[i]
        if param_name[0].isalnum():
            parameter['name'] = param_name
            i += 1

        if tokens[i] == '[':
            array_start = i
            while i < len(tokens) and tokens[i] != ']':
                i += 1
            i += 1
            parameter['type'] += ''.join(tokens[array_start:i])

        parameters.append(parameter)

        if optional: assert tokens[i] == ']'; i += 1
        if tokens[i] == ',': i += 1

    assert tokens[-1] == ')' or (tokens[-2] == ')' and tokens[-1] == ';')

    return {
        'return_type':func_output,
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

def group_by_indent(text: str, tab_indent=None):
    stack = [[]]
    indents = [0]
    indentation = 4
    for line in text.splitlines():
        i = 0
        for ch in line: 
            if ch == ' ': i += 1; continue
            if ch == '\t': i += tab_indent or indentation; continue
            break

        line = line[i:]

        if i % 4 == 2:
            indentation = 2

        # Round to nearest multiple of indentation to account for minor errors
        i = int(round(i / indentation) * indentation)

        if len(line) == 0: 
            stack[-1].append('')
            continue

        while i < indents[-1]:
            last = stack.pop()
            stack[-1].append(last)
            indents.pop()

        if i == indents[-1] or len(line) == 0: 
            stack[-1].append(line)
            continue

        if i > indents[-1]: 
            stack.append([line])
            indents.append(i)
            continue

    while len(stack) > 1:
        last = stack.pop()
        stack[-1].append(last)

    return stack[0]

def find_matching_groups(groups, pattern, flags=0):
    start = None
    last_end = 0
    text = ''

    for i, item in enumerate(groups):
        if isinstance(item, str):
            text += '\n' + item
            if re.search(pattern, text, flags) is None: continue
            start = last_end
        else:
            text = ''
            last_end = i+1

            if start is not None: 
                yield groups[start:i+1]
                start = None
                continue

            for match in find_matching_groups(item, pattern, flags):
                yield match

    if start is not None: 
        yield groups[start:]

def flatten(xs):
    for x in xs:
        if isinstance(x, list):
            for value in flatten(x): yield value
        else:
            yield x

def find_prototypes(text):
    syntaxes = []
    descriptions = []

    for match in re.finditer(r'(\s*(\+(-*\+)+|\|([^|]*\|)+))+', text, re.MULTILINE):
        table_text = text[match.start():match.end()].strip()

        header = True
        for i, line in enumerate(table_text.splitlines()):
            line = line.strip()
            if line[0] == '+':
                if i != 0:
                    syntaxes.append('')
                    descriptions.append('')
                continue
            
            pieces = line.split('|')
            if len(pieces) != 4: break

            if header:
                if re.match(r'\s*(syntax|function)\s*', pieces[1], re.IGNORECASE) is None: break
                if re.match(r'\s*description\s*', pieces[2], re.IGNORECASE) is None: break
                header = False
                continue

            syntaxes[-1] += pieces[1]
            descriptions[-1] += pieces[2] + '\n'

    for i in range(len(syntaxes)):
        syntaxes[i] = ' '.join(syntaxes[i].strip().split())
        descriptions[i] = descriptions[i].strip().split('\n\n')

    syntaxes.append(text)
    descriptions.append(None)

    found = set()

    for syntax, description in zip(syntaxes, descriptions):
        for match in re.finditer(r'((\w+\s+)+\w+\([^()]*\))', syntax, re.MULTILINE):
            match_text = syntax[match.start(1):match.end(1)]

            if '|' in match_text: continue

            proto = parse_prototype(match_text)

            if proto['name'] in found: continue
            found.add(proto['name'])

            if description is not None:
                proto['description'] = description

            return_type = proto['return_type']
            if re.match(r'.*(void|int|float|double|vec|mat|bool|gen\w*Type).*', return_type) is not None:
                yield proto
            else:
                known_invalid_pattern = r'(functions?|of|enable|the|and|to|call|with|if|as|in)'
                if re.match(known_invalid_pattern, return_type, re.IGNORECASE) is None:
                    print('unknown return type:', match_text)

def process_extension_file(path):
    filename = os.path.basename(path)
    with open(path, 'r') as f:
        text = str(f.read())

        tab_indent = 4
        if filename == 'GL_HUAWEI_cluster_culling_shader': tab_indent = 8 
        groups = group_by_indent(text, tab_indent)

        extension_names = []
        for match in re.finditer(r'\s+#extension (\w+)\s*:\s*<\w+>', text):
            extension_names.append(text[match.start(1):match.end(1)])

        implicit_extensions = ['GL_KHR_vulkan_glsl.txt', 'GL_EXT_vulkan_glsl_relaxed.txt']
        if len(extension_names) == 0:
            if filename not in implicit_extensions:
                print('missing extension names:', name)
                exit(0)

        prototypes = []
        vardecls = []

        if filename not in implicit_extensions:
            for section in find_matching_groups(groups, re.compile(r'chapter 7|section 7\.\d+', re.IGNORECASE)):
                section_text = '\n'.join(flatten(section))
                for match in re.finditer(r'(' + modifier_regex + r'\s+)+\w+\s+gl_\w+(\s*=\s*\w+)?;', section_text, re.MULTILINE):
                    match_text = section_text[match.start():match.end()]
                    vardecls.append(parse_variable(match_text))

            pattern = re.compile('(' + '|'.join([
                    r'variables?\s+<?gl_\w+>?',
                    r'<?gl_\w+(\?\?\w+)?>?variables?\s+',
                ]) + ')', re.IGNORECASE)
            for section in find_matching_groups(groups, pattern):
                while isinstance(section[-1], list) or section[-1] == '':
                    section = section[:-1]

                i = 0
                while i < len(section):
                    if section[i] == '' or section[i].isspace(): 
                        i += 1
                        continue

                    start = i
                    while i < len(section) and section[i] != '': i += 1
                    text = '\n'.join(section[start:i])

                    match = re.search(r'(variables?)?(,?(\s+and)?\s+<?gl_\w+(\?\?\w+)?>?)+(\s+(variables?|is\s+available))?', text)
                    if match is None: continue

                    matches = re.findall('(gl_\w+(\?\?\w+)?)', match.group())
                    if len(matches) == 0: i += 1; continue
                    names = [match[0] for match in matches]

                    if '??' in names[0]:
                        variants = ['Eq', 'Ge', 'Gt', 'Le', 'Lt']
                        names = [names[0].replace('??', v) for v in variants]

                    for name in names:
                        found = False
                        for var in vardecls:
                            if var['name'] == name:
                                found = True
                                if 'description' not in var:
                                    var['description'] = [text]
                        if not found:
                            print('unknown variable:', name)

        for section in find_matching_groups(groups, re.compile(r'chapter 8|section 8\.\d+', re.IGNORECASE)):
            for prototype in find_prototypes('\n'.join(flatten(section))):
                if prototype['name'].startswith('imageAtomic'): continue
                prototypes.append(prototype)

        for section in find_matching_groups(groups, re.compile(r'.*(the\s+function\s+\w+\([^()]*\)).*', re.IGNORECASE)):
            while (isinstance(section[-1], list) 
                   or section[-1] == '' 
                   or re.match(r'^syntax:', section[-1].strip(), re.IGNORECASE) is not None):
                section = section[:-1]
            
            text = '\n'.join(section)
            for match in re.finditer(r'(the\s+function|and)\s+(\w+)\([^()]*\)', text, re.IGNORECASE | re.MULTILINE):
                name = text[match.start(2):match.end(2)]
                found = False
                for proto in prototypes:
                    if proto['name'] == name:
                        found = True
                        if 'description' not in proto:
                            proto['description'] = text.split('\n\n')
                if not found:
                    print('unknown function:', name)



        for proto in prototypes + vardecls:
            required_extensions = extension_names

            if 'description' not in proto:
                print('missing documentation:', proto['name'])
            else:
                desc = proto['description']
                for item in desc:
                    match = re.match(r'Only usable if the extension (\w+) is enabled', item, re.IGNORECASE)
                    if match is not None:
                        required_extensions = [item[match.start(1):match.end(1)]]

                proto['description'] = [escape_code(p) for p in desc]

            if filename not in implicit_extensions:
                if len(required_extensions) == 0:
                    print('missing extension:', proto['name'])
                else:
                    proto['extensions'] = required_extensions

        functions.extend(prototypes)
        variables.extend(vardecls)


# Given a string, attempts to find and escape markdown code snippets
def escape_code(text):
    result = ''
    last = 0
    for match in re.finditer(r'\w+(\([^()]*\))?', text):
        result += text[last:match.start()]
        word = text[match.start():match.end()]
        if '_' in word or word[-1] == ')':
            result += '`' + word + '`'
        else:
            result += word
        last = match.end()

    result += text[last:]
    return result



output = sys.argv[1]

scriptdir = os.path.dirname(sys.argv[0])
extension_files = [f for f in iglob(f'{scriptdir}/GLSL/extensions/*/*.txt')]
docs_files = [f for f in iglob(f'{scriptdir}/docs.gl/sl4/*.xhtml')]

work = 0
total_work = len(extension_files) + len(docs_files)

def progress(info):
    global work
    print(f'{work}/{total_work}: {info}')
    work += 1

for i, path in enumerate(extension_files):
    progress(path)
    process_extension_file(path)

for i, path in enumerate(docs_files):
    progress(path)
    process_docs_gl_file(path)

variables.append({
    'modifiers': 'in',
    'type': 'int',
    'name': 'gl_VertexIndex',
    'description': [
        ' '.join("""The variable `gl_VertexIndex` is a vertex language input variable that
        holds an integer index for the vertex, relative to a base.  While the
        variable `gl_VertexIndex` is always present, its value is not always
        defined.""".split())
    ],
    'versions': [450],
})

variables.append({
    'modifiers': 'in',
    'type': 'int',
    'name': 'gl_InstanceIndex',
    'description': [
        ' '.join("""The variable `gl_InstanceIndex` is a vertex language input variable that
        holds the instance number of the current primitive in an instanced draw
        call, relative to a base. If the current primitive does not come from
        an instanced draw call, the value of `gl_InstanceIndex` is zero.""".split())
    ],
    'versions': [450],
})


with open(output, 'w') as f:
    f.write(json.dumps({
        'comment': 'generated from docs.gl',
        'variables':variables,
        'functions':functions,
    }, indent=4, ensure_ascii=False))

progress('done')

