"""
Copyright 2015 BlazeMeter Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""
import codecs
from abc import abstractmethod

from bzt.six import etree


class PythonGenerator(object):
    IMPORTS = ''
    INDENT_STEP = 4

    def __init__(self, scenario, parent_logger):
        self.root = etree.Element("PythonCode")
        self.tree = etree.ElementTree(self.root)
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.scenario = scenario

    def add_imports(self):
        imports = etree.Element("imports")
        imports.text = self.IMPORTS
        return imports

    @abstractmethod
    def build_source_code(self):
        pass

    @staticmethod
    def gen_class_definition(class_name, inherits_from, indent=0):
        def_tmpl = "class {class_name}({inherits_from}):"
        class_def_element = etree.Element("class_definition", indent=str(indent))
        class_def_element.text = def_tmpl.format(class_name=class_name, inherits_from="".join(inherits_from))
        return class_def_element

    @staticmethod
    def gen_method_definition(method_name, params, indent=None):
        if indent is None:
            indent = PythonGenerator.INDENT_STEP

        def_tmpl = "def {method_name}({params}):"
        method_def_element = etree.Element("method_definition", indent=str(indent))
        method_def_element.text = def_tmpl.format(method_name=method_name, params=",".join(params))
        return method_def_element

    @staticmethod
    def gen_decorator_statement(decorator_name, indent=None):
        if indent is None:
            indent = PythonGenerator.INDENT_STEP

        def_tmpl = "@{decorator_name}"
        decorator_element = etree.Element("decorator_statement", indent=str(indent))
        decorator_element.text = def_tmpl.format(decorator_name=decorator_name)
        return decorator_element

    @staticmethod
    def gen_statement(statement, indent=None):
        if indent is None:
            indent = PythonGenerator.INDENT_STEP * 2

        statement_elem = etree.Element("statement", indent=str(indent))
        statement_elem.text = statement
        return statement_elem

    def gen_comment(self, comment, indent=None):
        return self.gen_statement("# %s" % comment, indent=indent)

    def save(self, filename):
        with codecs.open(filename, 'w', encoding='utf-8') as fds:
            for child in self.root.iter():
                if child.text is not None:
                    indent = int(child.get('indent', "0"))
                    fds.write(" " * indent + child.text + "\n")

    def gen_new_line(self, indent=0):
        return self.gen_statement("", indent=indent)
