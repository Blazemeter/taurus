"""
Copyright 2018 BlazeMeter Inc.

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

import ast
import re
from abc import abstractmethod

from bzt.utils import iteritems

from .ast_helpers import ast_call


class JMeterFunction(object):
    def __init__(self, arg_names, compiler):
        self.arg_names = arg_names
        self.compiler = compiler

    def to_python(self, arguments):
        """arguments -> (expression, stmts)"""
        args = dict(zip(self.arg_names, arguments))
        return self._compile(args)

    @abstractmethod
    def _compile(self, args):
        pass


class RandomFunction(JMeterFunction):
    def __init__(self, compiler):
        super(RandomFunction, self).__init__(["min", "max", "varname"], compiler)

    def _compile(self, args):
        if args.get("min") is None or args.get("max") is None:
            return None

        # TODO: handle `varname` arg

        return ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="apiritif", ctx=ast.Load()),
                attr='random_uniform',
                ctx=ast.Load(),
            ),
            args=[self.compiler.gen_expr(int(args["min"])), self.compiler.gen_expr(int(args["max"]))],
            keywords=[],
            starargs=None,
            kwargs=None
        )


class RandomStringFunction(JMeterFunction):
    def __init__(self, compiler):
        super(RandomStringFunction, self).__init__(["size", "chars", "varname"], compiler)

    def _compile(self, args):
        if args.get("size") is None:
            return None

        # TODO: handle `varname`

        size = int(args.get("size"))
        arguments = [self.compiler.gen_expr(size)]
        if "chars" in args:
            arguments.append(self.compiler.gen_expr(args["chars"]))

        return ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="apiritif", ctx=ast.Load()),
                attr='random_string',
                ctx=ast.Load(),
            ),
            args=arguments,
            keywords=[],
            starargs=None,
            kwargs=None
        )


class Base64DecodeFunction(JMeterFunction):
    def __init__(self, compiler):
        super(Base64DecodeFunction, self).__init__(["text"], compiler)

    def _compile(self, args):
        if args.get("text") is None:
            return None

        return ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="apiritif", ctx=ast.Load()),
                attr='base64_decode',
                ctx=ast.Load(),
            ),
            args=[self.compiler.gen_expr(args["text"])],
            keywords=[],
            starargs=None,
            kwargs=None
        )


class Base64EncodeFunction(JMeterFunction):
    def __init__(self, compiler):
        super(Base64EncodeFunction, self).__init__(["text"], compiler)

    def _compile(self, args):
        if args.get("text") is None:
            return None

        return ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="apiritif", ctx=ast.Load()),
                attr='base64_encode',
                ctx=ast.Load(),
            ),
            args=[self.compiler.gen_expr(args["text"])],
            keywords=[],
            starargs=None,
            kwargs=None
        )


class TimeFunction(JMeterFunction):
    def __init__(self, compiler):
        super(TimeFunction, self).__init__(["format", "varname"], compiler)

    def _compile(self, args):
        # TODO: handle varname
        arguments = []
        if "format" in args:
            arguments.append(self.compiler.gen_expr(args["format"]))
        return ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="apiritif", ctx=ast.Load()),
                attr='format_date',
                ctx=ast.Load(),
            ),
            args=arguments,
            keywords=[],
            starargs=None,
            kwargs=None
        )


class UrlEncodeFunction(JMeterFunction):
    def __init__(self, compiler):
        super(UrlEncodeFunction, self).__init__(["chars"], compiler)

    def _compile(self, args):
        if "chars" not in args:
            return None
        return ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="apiritif", ctx=ast.Load()),
                attr='encode_url',
                ctx=ast.Load(),
            ),
            args=[self.compiler.gen_expr(args["chars"])],
            keywords=[],
            starargs=None,
            kwargs=None
        )


class UuidFunction(JMeterFunction):
    def __init__(self, compiler):
        super(UuidFunction, self).__init__([], compiler)

    def _compile(self, args):
        return ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="apiritif", ctx=ast.Load()),
                attr='uuid',
                ctx=ast.Load(),
            ),
            args=[],
            keywords=[],
            starargs=None,
            kwargs=None
        )



class JMeterExprCompiler(object):
    def __init__(self, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)

    @staticmethod
    def gen_var_accessor(varname, ctx=None):
        if ctx is None:
            ctx = ast.Load()

        return ast.Subscript(
            value=ast.Name(id="self.vars", ctx=ast.Load()),
            slice=ast.Index(value=ast.Str(s=varname, kind="")),
            ctx=ctx
        )

    def gen_expr(self, value):
        if isinstance(value, bool):
            return ast.Name(id="True" if value else "False", ctx=ast.Load())
        elif isinstance(value, (int, float)):
            return ast.Num(n=value, kind="")
        elif isinstance(value, str):
            # if is has interpolation - break it into either a `"".format(args)` form or a Name node
            # otherwise - it's a string literal
            parts = re.split(r'(\$\{.*?\})', value)
            format_args = []
            for item in parts:
                if item:
                    if item.startswith("${") and item.endswith("}"):
                        value = value.replace(item, "{}")
                        compiled = self.translate_jmeter_expr(item[2:-1])
                        format_args.append(compiled)
            if format_args:
                if len(format_args) == 1 and value == "{}":
                    result = format_args[0]
                else:
                    result = ast_call(
                        func=ast.Attribute(
                            value=ast.Str(s=value, kind=""),
                            attr='format',
                            ctx=ast.Load()),
                        args=format_args)
            else:
                result = ast.Str(s=value, kind="")
            return result
        elif isinstance(value, type(None)):
            return ast.Name(id="None", ctx=ast.Load())
        elif isinstance(value, dict):
            items = sorted(list(iteritems(value)))
            return ast.Dict(keys=[self.gen_expr(k) for k, _ in items],
                            values=[self.gen_expr(v) for _, v in items])
        elif isinstance(value, list):
            return ast.List(elts=[self.gen_expr(val) for val in value], ctx=ast.Load())
        elif isinstance(value, tuple):
            return ast.Tuple(elts=[self.gen_expr(val) for val in value], ctx=ast.Load())
        elif isinstance(value, ast.AST):
            return value
        else:
            return value

    def translate_jmeter_expr(self, expr):
        """
        Translates JMeter expression into Apiritif-based Python expression.
        :type expr: str
        :return:
        """
        self.log.debug("Attempting to translate JMeter expression %r", expr)
        functions = {
            '__time': TimeFunction,
            '__Random': RandomFunction,
            '__RandomString': RandomStringFunction,
            '__base64Encode': Base64EncodeFunction,
            '__base64Decode': Base64DecodeFunction,
            '__urlencode': UrlEncodeFunction,
            '__UUID': UuidFunction,
        }
        regexp = r"(\w+)\((.*?)\)"
        args_re = r'(?<!\\),'
        match = re.match(regexp, expr)
        if match is None:  # doesn't look like JMeter func, translate as a var
            return self.gen_var_accessor(expr)

        varname, arguments = match.groups()

        if arguments is None:  # plain variable
            result = self.gen_var_accessor(varname)
        else:  # function call
            if not arguments:
                args = []
            else:
                # parse arguments: split by ',' but not '\,'
                args = [arg.strip() for arg in re.split(args_re, arguments)]

            if varname not in functions:  # unknown function
                return ast.Name(id=varname, ctx=ast.Load())

            self.log.debug("Translating function %s with arguments %s", varname, arguments)
            func = functions[varname](self)
            result = func.to_python(args)
            if result is None:
                result = ast.Name(id=varname, ctx=ast.Load())
        self.log.debug("Compile: %r -> %r", expr, result)
        return result

