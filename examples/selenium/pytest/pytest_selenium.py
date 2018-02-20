import pytest


@pytest.fixture
def browser(request):
    from selenium import webdriver
    request.instance.browser = webdriver.Firefox()
    request.addfinalizer(lambda: request.instance.browser.quit())


@pytest.mark.usefixtures("browser")
class TestBlazedemo:
    def test_index(self):
        self.browser.get("http://blazedemo.com/")

    def test_reserve(self):
        self.browser.get("http://blazedemo.com/reserve.php")
