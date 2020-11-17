from selenium.webdriver.remote.webelement import WebElement


class ShadowElement(WebElement):
    """
    This class extends WebElement to allow for overriding methods that do not work properly in the Shadow components
    Currently the issues are mostly on salesforce site and so far only related to click action
    """

    def __init__(self, element, driver):
        self.element = element
        self.driver = driver

    def __getattr__(self, attr):
        return getattr(self.element, attr)

    def click(self):
        self.driver.execute_script("arguments[0].click();", self.element)
