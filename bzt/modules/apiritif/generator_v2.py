"""
BZT structures

Copyright 2019 BlazeMeter Inc.

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
from enum import Enum

from bzt import TaurusConfigError
from bzt.modules.apiritif.ast_helpers import ast_call, ast_attr, gen_empty_line_stmt, gen_subscript, gen_store
from bzt.modules.apiritif.jmeter_functions import JMeterExprCompiler
from bzt.six import text_type, string_types, PY2
from bzt.utils import dehumanize_time


class ScriptGeneratorV2(object):
    """
    Code Generator for yaml syntax version 2 supporting multiple locators, i.e. alternative locators are used when
    primary fails
    """
    BYS = {
        'xpath': "XPATH",
        'css': "CSS_SELECTOR",
        'name': "NAME",
        'id': "ID",
        'linktext': "LINK_TEXT"
    }

    ACTION_CHAINS = {
        'doubleclick': "double_click",
        'mousedown': "click_and_hold",
        'mouseup': "release",
        'mousemove': "move_to_element",
        'mouseover': "move_to_element",
        'mouseout': "move_to_element_with_offset"
    }

    def __init__(self, scenario, ignore_unknown_actions=False):
        self.ignore_unknown_actions = ignore_unknown_actions
        self.scenario = scenario
        self.log = self.scenario.engine.log.getChild(self.__class__.__name__)
        self.expr_compiler = JMeterExprCompiler(parent_log=self.log)

    def parse_action(self, action_config):
        if isinstance(action_config, dict):
            action_type = action_config["type"]
            param = action_config["param"]
        else:
            raise TaurusConfigError("Unsupported value for action: %s" % action_config)

        if not action_type:
            raise TaurusConfigError("Missing required 'type' of action in: %s" % action_config)

        actions_v1 = "|".join(['mouseMove', 'switchFrame', 'pause', 'clear', 'assert', 'close', 'script', 'switch',
                               'go', 'echo', 'element', 'store', 'open', 'screenshot', 'rawCode', 'resize', 'maximize'])

        actions_v2 = "|".join(['mouseDown', 'mouseUp', 'mouseOut', 'mouseOver', 'assertText', 'assertValue', 'click',
                               'editcontent', 'drag', 'doubleClick', 'select', 'wait', 'keys', 'type', 'submit',
                               'storeText', 'storeValue'])

        all_actions = actions_v1 + "|" + actions_v2

        tag = "For|Cookies|Title|Window|Eval|ByName|ByIdx|String"
        expr = re.compile("^(%s)(%s)?$" % (all_actions, tag), re.IGNORECASE)
        res = expr.match(action_type)
        if not res:
            msg = "Unsupported action: %s" % action_type
            if self.ignore_unknown_actions:
                self.log.warning(msg)
                return
            else:
                raise TaurusConfigError(msg)

        atype = res.group(1).lower()
        tag = res.group(2).lower() if res.group(2) else ""

        is_v2_action = True
        if atype in actions_v1.lower():
            locator = param
            param = "" if atype not in ["store"] else param
            is_v2_action = False
        else:
            if not action_config["target"] or not action_config["target"]["locators"]:
                raise TaurusConfigError("Missing locators for action: %s" % action_config)
            locator = action_config["target"]["locators"]

        source = target = None
        if atype == "drag":
            source = action_config["source"]
            target = action_config["target"]

        return atype, tag, param, locator, is_v2_action, source, target

    def gen_action(self, atype, param, locators, source, target):

        if atype in ("click", "type", "keys", "submit"):
            return self._gen_keys_mngr(atype, param, locators)
        if atype is not None and (atype.startswith("assert") or atype.startswith("store")):
            return self._gen_assert_store_mngr(atype, param, locators)
        if atype == "drag":
            return self._gen_drag_mngr(source, target)
        if atype in self.ACTION_CHAINS:
            return self._gen_chain_mngr(atype, locators)
        if atype == "editcontent":
            return self._gen_edit_mngr(param, locators)
        if atype == "select":
            return [self._gen_try_multiple_locators(locators,
                                                    [ActionWithArgs("Select", param, ActionType.SELECT)])]
        if atype == "wait":
            return self._gen_wait_mngr(param, locators)

    def _gen_wait_mngr(self, param, locators):
        exc = TaurusConfigError("wait action requires timeout in scenario: \n%s" % self.scenario)
        timeout = dehumanize_time(self.scenario.get("timeout", exc))
        mode = "visibility" if param == 'visible' else 'presence'
        actions_w_args = [ActionWithArgs("", [mode, timeout], ActionType.WAIT)]
        return [self._gen_try_multiple_locators(locators, actions_w_args)]

    def _gen_edit_mngr(self, param, locators):
        if not param:
            raise TaurusConfigError("Missing param for editContent action.")
        var_name = "edit_content_loc"

        elements = [self._gen_get_locators(var_name, locators)]
        locator = self._gen_dynamic_locator(var_name)
        tag = gen_subscript(var_name, 0)
        selector = gen_subscript(var_name, 1)

        exc_type = ast_call(
            func="NoSuchElementException",
            args=[
                ast.BinOp(
                    left=ast.Str("The element (%s: %r) is not a contenteditable element"),
                    op=ast.Mod(),
                    right=ast.Tuple(elts=[tag, selector]))
            ]
        )

        if PY2:
            raise_kwargs = {
                "type": exc_type,
                "inst": None,
                "tback": None
            }
        else:
            raise_kwargs = {
                "exc": exc_type,
                "cause": None}

        body = ast.Expr(ast_call(func=ast_attr("self.driver.execute_script"),
                                 args=[
                                     ast.BinOp(
                                         left=ast.Str("arguments[0].innerHTML = '%s';"),
                                         op=ast.Mod(),
                                         right=self._gen_expr(param.strip())),
                                     locator]))

        element = ast.If(
            test=ast_call(
                func=ast_attr(
                    fields=(locator, "get_attribute")),
                args=[ast.Str("contenteditable")]),
            body=[body],
            orelse=[ast.Raise(**raise_kwargs)])

        elements.append(element)
        return elements

    def _gen_chain_mngr(self, atype, locators):
        actions_w_args = [ActionWithArgs(atype, [], ActionType.ACTION_CHAINS)]
        return [self._gen_try_multiple_locators(locators, actions_w_args)]

    def _gen_drag_mngr(self, source, target):
        if not source or not source["locators"]:
            raise TaurusConfigError("Can not generate action for 'drag'. Source is empty.")
        if not target or not target["locators"]:
            raise TaurusConfigError("Can not generate action for 'drag'. Target is empty.")

        elements = [self._gen_get_locators("source", source["locators"]),
                    self._gen_get_locators("target", target["locators"])]

        operator = ast_attr(
            fields=(
                ast_call(
                    func="ActionChains",
                    args=[ast_attr("self.driver")]),
                "drag_and_drop"))
        elements.append(ast_call(
            func=ast_attr(
                fields=(
                    ast_call(
                        func=operator,
                        args=[self._gen_dynamic_locator("source"),
                              self._gen_dynamic_locator("target")]),
                    "perform"))))
        return elements

    @staticmethod
    def _gen_dynamic_locator(var_w_locator):
        return ast_call(
            func=ast_attr("self.driver.find_element"),
            args=[
                gen_subscript(var_w_locator, 0),
                gen_subscript(var_w_locator, 1)
            ])

    @staticmethod
    def _gen_get_locators(var_name, locators):
        args = []
        for loc in locators:
            locator_type = list(loc.keys())[0]
            locator_value = loc[locator_type]
            args.append(ast.Dict([ast.Str(locator_type)], [ast.Str(locator_value)]))

        return ast.Assign(
            targets=[ast.Name(id=var_name, ctx=ast.Store())],
            value=ast_call(func="self.loc_mng.get_locator",
                           args=[ast.List(elts=args)]))

    def _gen_assert_store_mngr(self, atype, param, locators):
        elements = []
        target = None
        if atype.lower() in ["asserttext", "storetext"]:
            target = "innerText"
        elif atype.lower() in ["assertvalue", "storevalue"]:
            target = "value"
        if not param:
            raise TaurusConfigError("Missing param for %s action." % atype)
        if target:
            action_type = ActionType.STORE if "store" in atype.lower() else ActionType.ASSERT
            action = ActionWithArgs("get_attribute", target, action_type, param.strip())
            elements.append(self._gen_try_multiple_locators(locators, [action]))
        return elements

    def _gen_keys_mngr(self, atype, param, locators):
        elements = []
        actions_w_args = []

        if atype in ["click", "submit"]:
            actions_w_args = [ActionWithArgs(atype, [])]
        elif atype in ["keys", "type"]:
            if atype == "type":
                actions_w_args = [ActionWithArgs("clear", [])]
            if isinstance(param, (string_types, text_type)) and param.startswith("KEY_"):
                args = [ast_attr("Keys.%s" % param.split("KEY_")[1])]
            else:
                args = [self._gen_expr(str(param))]
            actions_w_args.append(ActionWithArgs("send_keys", args))

        if actions_w_args:
            elements.append(self._gen_try_multiple_locators(locators, actions_w_args))
        return elements

    def _gen_base_action(self, locator, action_w_args, is_handler):
        """
        Generates a line of code that will go to the try ... except block \n
        The code differs if generating to the first body or to the handlers part \n
        In the first body we are using the implicit wait however for the handlers part we are using explicit wait
        set to 0 so not to wait for each locator by implicit wait

        :param locator: locator to be used
        :param action_w_args: ActionWithArgs class instance
        :param is_handler: True if we are generating code for the except part or false if generating for the body of try
        :return: generates a single line of code, examples:
            self.driver.find_element(By.XPATH, '/doc/abc').send_keys(Keys.ENTER)
            self.vars['var_name'] = self.driver.find_element(By.XPATH, '/html/body/').get_attribute('innerText')
        """
        args = action_w_args.args if action_w_args.action_type is (ActionType.GENERIC or ActionType.ACTION_CHAINS) \
            else [ast.Str(action_w_args.args)]

        gen_loc_method = self._gen_wait_locator if is_handler else self._gen_locator

        locator_type = list(locator.keys())[0]
        locator_value = locator[locator_type]

        call_locator = gen_loc_method(self.BYS[locator_type.lower()], locator_value)

        call_body = ast_call(
            func=ast_attr(
                fields=(
                    call_locator,
                    action_w_args.name)),
            args=args)

        if action_w_args.action_type is ActionType.GENERIC:
            return call_body
        if action_w_args.action_type is ActionType.STORE:
            return gen_store(action_w_args.param, call_body)
        if action_w_args.action_type is ActionType.ASSERT:
            return self._gen_assert(action_w_args.param, call_body)
        if action_w_args.action_type is ActionType.ACTION_CHAINS:
            return self._gen_chain_action(action_w_args.name, call_locator)
        if action_w_args.action_type is ActionType.SELECT:
            return self._gen_select_type(action_w_args.args, call_locator)
        if action_w_args.action_type is ActionType.WAIT:
            exc_msg = "Element %r: %r failed to appear within %ss" % (
                      locator_type, locator_value, action_w_args.args[1])
            return self._gen_wait_locator(self.BYS[locator_type.lower()], locator_value, action_w_args.args[0],
                                          action_w_args.args[1], exc_msg)

    @staticmethod
    def _gen_select_type(args, call_locator):
        return ast_call(
            func=ast_attr(
                fields=(
                    ast_call(func="Select", args=[call_locator]),
                    "select_by_visible_text")),
            args=[ast.Str(args)])

    def _gen_try_multiple_locators(self, locators, actions_w_args):
        body = [
            gen_empty_line_stmt(),
            list(map(lambda a: [self._gen_base_action(locators[0], a, False),
                                gen_empty_line_stmt()], actions_w_args))
        ]

        if len(locators) == 1:
            return body

        if actions_w_args[0].action_type == ActionType.WAIT:
            handler = ast.ExceptHandler(type=ast.Name(id='TimeoutException', ctx=ast.Load()), name=None, body=[
                self._gen_handlers_for_multiple_locators(locators[1:], actions_w_args)
            ])
        else:
            handler = ast.ExceptHandler(type=ast.Name(id='NoSuchElementException', ctx=ast.Load()), name="nse", body=[
                self._gen_handlers_for_multiple_locators(locators[1:], actions_w_args)
            ])

        handlers = [handler]

        return ast.Try(
            body=[body],
            handlers=[handlers],
            orelse=[],
            finalbody=[]
        )

    def _gen_handlers_for_multiple_locators(self, locators, actions_w_args):
        body = [gen_empty_line_stmt(), list(map(lambda a: [
            self._gen_base_action(locators[0], a, True), gen_empty_line_stmt()], actions_w_args))]

        if actions_w_args[0].action_type == ActionType.WAIT and len(locators) == 1:
            return body

        if len(locators) > 1:
            handler_body = self._gen_handlers_for_multiple_locators(locators[1:], actions_w_args)
        else:
            handler_body = ast.Raise(ast.Name(id="nse"), [])

        handlers = [
            ast.ExceptHandler(type=ast.Name(id='TimeoutException', ctx=ast.Load()), name=None, body=[handler_body])
        ]

        return ast.Try(
            body=[body],
            handlers=[handlers],
            orelse=[],
            finalbody=[]
        )

    def _gen_wait_locator(self, tag, locator, mode="presence", timeout=0, err_msg=""):
        """
            Generates output like:
            WebDriverWait(self.driver, 0).until(econd.presence_of_element_located((By.CSS_SELECTOR, 'input.btn')))
        """
        return [ast_call(
            func=ast_attr(
                fields=(
                    ast_call(
                        func=ast_attr("WebDriverWait"),
                        args=[ast_attr("self.driver"), ast.Str(timeout)]),
                    "until")),
            args=[
                ast_call(
                    func=ast_attr("econd.%s_of_element_located" % mode),
                    args=[
                        ast.Tuple(elts=[
                            ast_attr("By.%s" % tag),
                            self._gen_expr(locator)])
                    ]),
                ast.Str(err_msg)
            ]
        )]

    def _gen_locator(self, tag, locator):
        return ast_call(
            func=ast_attr("self.driver.find_element"),
            args=[
                ast_attr("By.%s" % tag),
                self._gen_expr(locator)])

    def _gen_expr(self, value):
        return self.expr_compiler.gen_expr(value)

    def _gen_chain_action(self, atype, value):
        operator = ast_attr(fields=(
            ast_call(func="ActionChains", args=[ast_attr("self.driver")]),
            self.ACTION_CHAINS[atype.lower()]))
        args = [value, ast.Num(-10), ast.Num(-10)] if atype == "mouseout" else [value]
        return ast_call(
            func=ast_attr(
                fields=(
                    ast_call(
                        func=operator,
                        args=args),
                    "perform")))

    def _gen_assert(self, param, value):
        return ast_call(
            func=ast_attr(fields="self.assertEqual"),
            args=[
                value,
                ast_call(
                    func=ast_attr(
                        fields=(
                            self._gen_expr(param),
                            "strip")))])


class ActionType(Enum):
    GENERIC = "GENERIC"
    STORE = "STORE"
    ASSERT = "ASSERT"
    ACTION_CHAINS = "ACTION_CHAINS"
    SELECT = "SELECT"
    WAIT = "WAIT"


class ActionWithArgs(object):

    def __init__(self, name, args, action_type=ActionType.GENERIC, param=""):
        self.name = name
        self.args = args
        self.action_type = action_type
        self.param = param
