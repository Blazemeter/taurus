# Utility functions and classes for Taurus Selenium tests
import time

from apiritif import get_transaction_handlers, set_transaction_handlers, get_from_thread_store, get_iteration
from selenium.common.exceptions import NoSuchWindowException, NoSuchFrameException, NoSuchElementException, \
    TimeoutException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support import expected_conditions as econd

BYS = {
    'xpath': By.XPATH,
    'css': By.CSS_SELECTOR,
    'name': By.NAME,
    'id': By.ID,
    'linktext': By.LINK_TEXT
}

def get_locator(locators, ignore_implicit_wait=False, raise_exception=False):
    """
    :param locators: List of Dictionaries holding the locators, e.g. [{'id': 'elem_id'},
    {css: 'my_cls'}]
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
    for locator in locators:
        locator_type = list(locator.keys())[0]
        locator_value = locator[locator_type]
        if not first_locator:
            first_locator = (BYS[locator_type.lower()], locator_value)
        else:
            # set implicit wait to 0 get the result instantly for the other locators
            driver.implicitly_wait(0)
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
    if not (timeout or timeout == 0):   # timeout in (None, []), default requires
        timeout = 30

    return timeout


def add_flow_markers():
    handlers = get_transaction_handlers()
    handlers["enter"].append(_send_start_flow_marker)
    handlers["exit"].append(_send_exit_flow_marker)
    set_transaction_handlers(handlers)


def _send_marker(stage, params):
    _get_driver().execute_script("/* FLOW_MARKER test-case-%s */" % stage, params)


def _send_start_flow_marker(*args, **kwargs):   # for apiritif. remove when compatibiltiy code in
    stage = "start"                             # apiritif removed (http.py) and apiritif released ( > 0.9.2)

    test_case, test_suite, scenario_name, data_sources = get_from_thread_store(
        ['test_case', 'test_suite', 'scenario_name', 'data_sources']
    )
    params = {
        "testCaseName": test_case,
        "testSuiteName": scenario_name or test_suite}

    if data_sources:
        params["testDataIterationId"] = get_iteration()

    _send_marker(stage, params)


def _send_exit_flow_marker(*args, **kwargs):   # for apiritif. remove when compatibiltiy code in
    stage = "stop"                             # apiritif removed (http.py) and apiritif released ( > 0.9.2)
    labels = "status", "message"
    values = get_from_thread_store(labels)
    params = dict(zip(labels, values))
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
                if (t) { t = t.replace(/\\n/g, ' '); }
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
    if str(value).lower() == '#ok':
        _get_driver().execute_script("window.__webdriverNextAlert = true")


def dialogs_answer_on_next_prompt(value):
    """
    :param value: The value to be used to answer the next 'window.prompt', if '#cancel' is provided then
    click on cancel button is simulated by returning null
    """
    if str(value).lower() == '#cancel':
        _get_driver().execute_script("window.__webdriverNextPrompt = null")
    else:
        _get_driver().execute_script("window.__webdriverNextPrompt = '%s';" % value)


def dialogs_answer_on_next_confirm(value):
    """
    :param value: either '#ok' to click on OK button or '#cancel' to simulate click on Cancel button in the
    next 'window.confirm' method
    """
    if str(value).lower() == '#ok':
        confirm = 'true'
    else:
        confirm = 'false'
    _get_driver().execute_script("window.__webdriverNextConfirm = %s;" % confirm)


def wait_for(condition, locators, wait_timeout=10):
    if condition.lower() in ["present", "visible", "clickable"]:
        _wait_for_positive(condition.lower(), locators, wait_timeout)
    elif condition.lower() in ["notpresent", "notvisible", "notclickable"]:
        _wait_for_negative(condition.lower(), locators, wait_timeout)


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
        elif frame_name.startswith("index="):  # Switch using index frame using relative position
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
    elif window_name.split("win_ser_")[1].isdigit():    # e.g. win_ser_1
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
