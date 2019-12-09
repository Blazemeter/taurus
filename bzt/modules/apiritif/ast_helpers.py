import ast

from bzt.six import string_types


def ast_attr(fields):
    """ fields is string of attrs (e.g. 'self.call.me.now') or list of ast args"""
    if isinstance(fields, string_types):
        if "." in fields:
            fields_list = fields.split(".")
            return ast.Attribute(attr=fields_list[-1], value=ast_attr(".".join(fields_list[:-1])))

        return ast.Name(id=fields)
    else:
        if len(fields) == 1:
            if isinstance(fields[0], string_types):
                return ast.Name(id=fields[0])
            else:
                return fields[0]

        return ast.Attribute(attr=fields[-1], value=ast_attr(fields[:-1]))  # join ast expressions


def ast_call(func, args=None, keywords=None):
    args = args or []
    if isinstance(func, string_types):
        func = ast.Name(id=func)
    return ast.Call(func=func, args=args, starargs=None, kwargs=None, keywords=keywords or [])


def gen_empty_line_stmt():
    return ast.Expr(value=ast.Name(id="")) # hacky, but works


def gen_subscript(var_name, index):
    """ Generates code like variable[1]  """
    return ast.Expr(value=ast.Subscript(value=ast.Name(id=var_name, ctx=ast.Load()),
                                        slice=ast.Index(value=ast.Num(n=index)), ctx=ast.Load()))


def gen_store(name, value):
    return ast.Assign(
        targets=[ast.Subscript(
            value=ast_attr("self.vars"),
            slice=ast.Str(name))],
        value=value)
