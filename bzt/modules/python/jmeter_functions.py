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
from abc import abstractmethod


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
