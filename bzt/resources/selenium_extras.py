# Utility functions and classes for Taurus Selenium tests
import time

from apiritif import get_transaction_handlers, set_transaction_handlers, get_from_thread_store, get_iteration, external_handler

from selenium.common.exceptions import NoSuchWindowException, NoSuchFrameException, NoSuchElementException, \
    TimeoutException, UnexpectedAlertPresentException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.remote.webelement import WebElement

from apiritif.action_plugins import BaseActionHandler
from bzt import TaurusException
from bzt.resources.shadow_element import ShadowElement

BYS = {
    'xpath': By.XPATH,
    'css': By.CSS_SELECTOR,
    'name': By.NAME,
    'id': By.ID,
    'linktext': By.LINK_TEXT
}


def find_element_by_shadow(shadow_loc):
    """
    Enables finding element using the Shadow Locator
    :param shadow_loc: shadow locator in the string form - css locators divided by commas, e.g. 'c-comp1, c-basic, .std_btn'
    :return: the found element otherwise NoSuchElementException is raised
    """
    el = None
    css_path = [x.strip() for x in shadow_loc.split(',')]
    for p in css_path:
        if not el:
            el = _get_driver().find_element(By.CSS_SELECTOR, p)
        else:
            curr_el = _find_element_in_shadow(el, p, False)
            if curr_el is None:
                # try to search in the shadowRoot of the parent element
                parent_el = el.find_element(By.XPATH, '..')
                curr_el = _find_element_in_shadow(parent_el, p, True)
            el = curr_el
    return ShadowElement(el, _get_driver())


def _find_element_in_shadow(el, css_selector, raise_exception):
    shadow_root = el.get_property("shadowRoot")
    element = None
    if shadow_root:
        element = _find_by_css_selector(shadow_root, css_selector, False)
    if element is None:
        # sometimes the element is not located under the shadowRoot so try to look for it the usual way
        element = _find_by_css_selector(el, css_selector, raise_exception)
    return element


def _find_by_css_selector(root, css_selector, raise_exception):
    element = None
    try:
        # Chrome 96+ returns the element as a dict that includes the id
        if isinstance(root, dict):
            key = list(root.keys())[0]
            shadow_root_el = WebElement(_get_driver(), root[key])
            element = shadow_root_el.find_element(By.CSS_SELECTOR, css_selector)
        else:
            element = root.find_element(By.CSS_SELECTOR, css_selector)
    except NoSuchElementException as nse:
        if raise_exception:
            if root.tag_name == "slot":
                raise TaurusException("Shadow DOM Slots are currently not supported in Taurus execution.")
            raise nse
        pass
    return element


def _is_shadow_locator(locators):
    return len(locators) == 1 and locators[0].get("shadow")


def get_locator(locators, parent_el=None, ignore_implicit_wait=False, raise_exception=False):
    """
    :param locators: List of Dictionaries holding the locators, e.g. [{'id': 'elem_id'},
    {css: 'my_cls'}]
    :param parent_el: reference to the parent element (WebElement instance), optional - if provided the find_elements
    method is called on it instead of global context
    :param ignore_implicit_wait: set it to True to set the implicit wait immediately to 0
    :param raise_exception: set it to True to get the NoSuchElementException in case no elements are matching any
    of the passed locators, if set to False then the first locator is returned in that case
    :return: first valid locator from the passed List, if no locator is valid then returns the
    first one
    """
    driver = _get_driver()
    timeout = _get_timeout()
    first_locator = None
    if ignore_implicit_wait:
        driver.implicitly_wait(0)
    if _is_shadow_locator(locators):
        return locators
    for locator in locators:
        locator_type = list(locator.keys())[0]
        locator_value = locator[locator_type]
        if not first_locator:
            first_locator = (BYS[locator_type.lower()], locator_value)
        else:
            # set implicit wait to 0 get the result instantly for the other locators
            driver.implicitly_wait(0)
        if parent_el:
            elements = parent_el.find_elements(BYS[locator_type.lower()], locator_value)
        else:
            elements = driver.find_elements(BYS[locator_type.lower()], locator_value)
        if len(elements) > 0:
            locator = (BYS[locator_type.lower()], locator_value)
            break
    else:
        if raise_exception:
            driver.implicitly_wait(timeout)
            msg = "Element not found: (%s, %s)" % first_locator
            raise NoSuchElementException(msg)
        else:
            locator = first_locator

    # restore the implicit wait value
    driver.implicitly_wait(timeout)
    return locator


def get_elements(locators):
    """
    :param locators: List of Dictionaries holding the locators, e.g. [{'id': 'elem_id'},
    {css: 'my_cls'}]
    :return: all elements that match the first valid locator out of the passed locators
    """
    elements = []
    first_locator = True
    driver = _get_driver()
    for locator in locators:
        locator_type = list(locator.keys())[0]
        locator_value = locator[locator_type]
        if not first_locator:
            driver.implicitly_wait(0)
        elements = driver.find_elements(BYS[locator_type.lower()], locator_value)
        first_locator = False
        if len(elements) > 0:
            break

    # restore the implicit wait value
    driver.implicitly_wait(_get_timeout())
    return elements


def _get_driver():
    return get_from_thread_store("driver")


def _get_timeout():
    timeout = get_from_thread_store("timeout")
    if not (timeout or timeout == 0):  # timeout in (None, []), default requires
        timeout = 30

    return timeout


def add_flow_markers():
    handlers = get_transaction_handlers()
    handlers["enter"].append(_send_start_flow_marker)
    handlers["exit"].append(_send_exit_flow_marker)
    set_transaction_handlers(handlers)


def _send_marker(stage, params):
    _get_driver().execute_script("/* FLOW_MARKER test-case-%s */" % stage, params)


def _send_start_flow_marker(*args, **kwargs):  # for apiritif. remove when compatibiltiy code in
    stage = "start"  # apiritif removed (http.py) and apiritif released ( > 0.9.2)

    test_case, test_suite, scenario_name, data_sources, action_handlers, driver = get_from_thread_store(
        ['test_case', 'test_suite', 'scenario_name', 'data_sources', 'action_handlers', 'driver']
    )
    params = {
        "testCaseName": test_case,
        "testSuiteName": scenario_name or test_suite}

    if data_sources:
        params["testDataIterationId"] = get_iteration()

    for handler in action_handlers:
        handler.handle(driver.session_id, BaseActionHandler.TEST_CASE_START, params)

    _send_marker(stage, params)


def _send_exit_flow_marker(*args, **kwargs):  # for apiritif. remove when compatibiltiy code in
    stage = "stop"  # apiritif removed (http.py) and apiritif released ( > 0.9.2)
    labels = "status", "message"
    action_handlers, driver = get_from_thread_store(['action_handlers', 'driver'])
    values = get_from_thread_store(labels)
    params = dict(zip(labels, values))

    for handler in action_handlers:
        handler.handle(driver.session_id, BaseActionHandler.TEST_CASE_STOP, params)
    _send_marker(stage, params)


def dialogs_replace():
    """
    Replaces the standard JavaScript methods, i.e. 'window.confirm', 'window.alert' and 'window.prompt' with
    own implementation that stores the messages from the dialogs and also is capable of returning user defined
    values
    """

    _get_driver().execute_script("""
          if (window.__webdriverAlerts) { return; }
          window.__webdriverAlerts = [];
          window.__webdriverOriginalAlert = window.alert;
          window.__webdriverNextAlert = null;
          window.alert = function(msg) {
            if (window.__webdriverNextAlert === null) {
                window.__webdriverOriginalAlert(msg);
            }
            window.__webdriverNextAlert = null; 
            window.__webdriverAlerts.push(msg); 
          };
          window.__webdriverConfirms = [];
          window.__webdriverNextConfirm = null;
          window.__webdriverPrevConfirm = window.confirm;
          window.confirm = function(msg) {
            window.__webdriverConfirms.push(msg);
            var res = window.__webdriverNextConfirm;
            if (res === null) {
                return window.__webdriverPrevConfirm(msg);
            }
            window.__webdriverNextConfirm = null;
            return res;
          };
          window.__webdriverPrompts = [];
          window.__webdriverNextPrompts = true;
          window.prompt = function(msg, def) {
            window.__webdriverPrompts.push(msg || def);
            var res = window.__webdriverNextPrompt;
            window.__webdriverNextPrompt = true;
            return res;
          };
        """)


def dialogs_get_next_confirm():
    """
    :return: the message from the last invocation of 'window.confirm'
    """
    return _get_driver().execute_script("""
                 if (!window.__webdriverConfirms) { return null; }
                 return window.__webdriverConfirms.shift();
               """)


def dialogs_get_next_alert():
    """
    :return: the alert message from the last invocation of 'window.alert'
    """
    return _get_driver().execute_script("""
                if (!window.__webdriverAlerts) { return null } 
                var t = window.__webdriverAlerts.shift(); 
                if (t) { t = t.toString().replace(/\\n/g, ' '); }
                return t;
              """)


def dialogs_get_next_prompt():
    """
    :return: the message from the last invocation of 'window.prompt'
    """
    return _get_driver().execute_script("""
                if (!window.__webdriverPrompts) { return null; }
                return window.__webdriverPrompts.shift();
              """)


def dialogs_answer_on_next_alert(value):
    """
    Simulates click on OK button in the next alert
    """
    dialogs_replace()
    if str(value).lower() == '#ok':
        _get_driver().execute_script("window.__webdriverNextAlert = true")


def dialogs_answer_on_next_prompt(value):
    """
    :param value: The value to be used to answer the next 'window.prompt', if '#cancel' is provided then
    click on cancel button is simulated by returning null
    """
    dialogs_replace()
    if str(value).lower() == '#cancel':
        _get_driver().execute_script("window.__webdriverNextPrompt = null")
    else:
        _get_driver().execute_script("window.__webdriverNextPrompt = '%s';" % value)


def dialogs_answer_on_next_confirm(value):
    """
    :param value: either '#ok' to click on OK button or '#cancel' to simulate click on Cancel button in the
    next 'window.confirm' method
    """
    dialogs_replace()
    if str(value).lower() == '#ok':
        confirm = 'true'
    else:
        confirm = 'false'
    _get_driver().execute_script("window.__webdriverNextConfirm = %s;" % confirm)


def wait_for(condition, locators, wait_timeout=10):
    if condition.lower() in ["present", "visible", "clickable"]:
        if _is_shadow_locator(locators):
            _wait_for_positive_shadow(condition.lower(), locators[0].get("shadow"), wait_timeout)
        else:
            _wait_for_positive(condition.lower(), locators, wait_timeout)
    elif condition.lower() in ["notpresent", "notvisible", "notclickable"]:
        if _is_shadow_locator(locators):
            _wait_for_negative_shadow(condition.lower(), locators[0].get("shadow"), wait_timeout)
        else:
            _wait_for_negative(condition.lower(), locators, wait_timeout)


def presence_of_shadow_element_located(locator):
    """
    Extends expected_conditions.py to support shadow locators
    """
    def _predicate(driver=None):
        return find_element_by_shadow(locator)

    return _predicate


def _wait_for_positive_shadow(condition, shadow_loc, wait_timeout):
    start_time = time.time()
    while True:
        shadow_element = None
        try:
            shadow_element = find_element_by_shadow(shadow_loc)
        except NoSuchElementException:
            pass
        if shadow_element:
            try:
                element = WebDriverWait(_get_driver(), wait_timeout).until(
                    _get_until_cond_shadow(condition, shadow_element.element, shadow_loc))
                if element:
                    return
            except TimeoutException:
                pass
        elapsed_time = time.time() - start_time
        if elapsed_time > wait_timeout:
            raise NoSuchElementException("Timeout occurred while waiting for element (%s) to become '%s' " % (shadow_loc, condition))


def _wait_for_negative_shadow(condition, shadow_loc, wait_timeout):
    shadow_element = None
    try:
        shadow_element = find_element_by_shadow(shadow_loc)
    except NoSuchElementException:
        pass
    if shadow_element:
        WebDriverWait(_get_driver(), wait_timeout).until_not(
            _get_until_cond_shadow(condition, shadow_element, shadow_loc),
            message="Timeout occurred while waiting for element (%s) to become '%s'" %
                    (shadow_loc, condition))


def _wait_for_positive(condition, locators, wait_timeout):
    start_time = time.time()
    while True:
        locator = None
        try:
            locator = get_locator(locators, ignore_implicit_wait=True, raise_exception=True)
        except NoSuchElementException:
            pass
        if locator:
            element = None
            try:
                element = WebDriverWait(_get_driver(), wait_timeout).until(_get_until_cond(condition, locator))
            except TimeoutException:
                pass
            if element:
                return

        elapsed_time = time.time() - start_time
        if elapsed_time > wait_timeout:
            raise NoSuchElementException("Timeout occurred while waiting for '%s' condition" % condition)


def _wait_for_negative(condition, locators, wait_timeout):
    present_locs = []
    for locator in locators:
        try:
            present_locs.append(get_locator([locator], ignore_implicit_wait=True, raise_exception=True))
        except NoSuchElementException:
            pass
    if not present_locs:
        return
    start_time = time.time()
    for locator in present_locs:
        elapsed_time = time.time() - start_time
        timeout = wait_timeout - elapsed_time
        WebDriverWait(_get_driver(), timeout).until_not(
            _get_until_cond(condition, locator),
            message="Timeout occurred while waiting for element (%s=%s) to become '%s'" %
                    (locator[0], locator[1], condition))


def _get_until_cond(condition, locator):
    loc_tuple = (locator[0], locator[1])
    if "clickable" in condition:
        return econd.element_to_be_clickable(loc_tuple)
    if "present" in condition:
        return econd.presence_of_element_located(loc_tuple)
    if "visible" in condition:
        return econd.visibility_of_element_located(loc_tuple)


def _get_until_cond_shadow(condition, element, shadow_locator=None):
    if "clickable" in condition:
        return econd.element_to_be_clickable(element)
    if "present" in condition:
        return presence_of_shadow_element_located(shadow_locator)
    if "visible" in condition:
        return econd.visibility_of(element)


def get_loop_range(start, end, step):
    """
    :return: the range over which the loop will operate
    """
    start = int(start)
    end = int(end)
    step = int(step)
    end = end + 1 if step > 0 else end - 1
    return range(start, end, step)


def switch_frame(frame_name=None):
    driver = _get_driver()
    try:
        if not frame_name or frame_name == "relative=top":
            driver.switch_to_default_content()
        elif isinstance(frame_name, str) and frame_name.startswith(
                "index="):  # Switch using index frame using relative position
            driver.switch_to.frame(int(frame_name.split("=")[1]))
        elif frame_name == "relative=parent":  # Switch to parent frame of the current frame
            driver.switch_to.parent_frame()
        else:  # Use the selenium alternative
            driver.switch_to.frame(frame_name)
    except NoSuchFrameException:
        raise NoSuchFrameException("Invalid Frame ID: %s" % frame_name)


def switch_window(window_name=None):
    driver = _get_driver()
    try:
        if window_name is None:  # Switch to last window created
            driver.switch_to.window(driver.window_handles[-1])
        else:
            if isinstance(window_name, int) or window_name.isdigit():  # Switch to window handler index
                _switch_by_idx(int(window_name))
            else:
                if window_name.startswith("win_ser_"):  # Switch using window sequential mode
                    _switch_by_win_ser(window_name)
                else:  # Switch using window name
                    driver.switch_to.window(window_name)
    except NoSuchWindowException:
        raise NoSuchWindowException("Invalid Window ID: %s" % window_name)


def open_window(url):
    """
    Opens the given url in a new window and also switches to it automatically
    """
    driver = _get_driver()
    driver.execute_script("window.open('%s');" % url)
    driver.switch_to.window(driver.window_handles[-1])


def _switch_by_idx(win_index):
    driver = _get_driver()
    wnd_handlers = driver.window_handles
    if 0 <= win_index < len(wnd_handlers):
        driver.switch_to.window(wnd_handlers[win_index])
    else:
        raise NoSuchWindowException("Invalid Window ID: %s" % str(win_index))


def _switch_by_win_ser(window_name):
    driver = _get_driver()
    if window_name == "win_ser_local":
        wnd_handlers = driver.window_handles
        if len(wnd_handlers) > 0:
            driver.switch_to.window(wnd_handlers[0])
        else:
            raise NoSuchWindowException("Invalid Window ID: %s" % window_name)
    elif window_name.split("win_ser_")[1].isdigit():  # e.g. win_ser_1
        _switch_by_idx(int(window_name.split("win_ser_")[1]))
    else:
        windows = get_from_thread_store("windows")
        if window_name not in windows:
            windows[window_name] = driver.current_window_handle
        driver.switch_to.window(windows[window_name])


def close_window(window_name=None):
    if window_name:
        switch_window(window_name)
    _get_driver().close()


def waiter():
    """
    Allows waiting for page to finish loading before performing other actions on non completely loaded page
    """
    try:
        WebDriverWait(_get_driver(), _get_timeout()) \
            .until(lambda driver: driver.execute_script('return document.readyState') == 'complete',
                   message="Timeout occurred while waiting for page to finish loading.")
    except UnexpectedAlertPresentException:
        pass


def action_start(action):
    driver = _get_driver()
    external_handler(driver.session_id if driver else None, BaseActionHandler.YAML_ACTION_START, action)


def action_end(action):
    driver = _get_driver()
    external_handler(driver.session_id if driver else None, BaseActionHandler.YAML_ACTION_END, action)
