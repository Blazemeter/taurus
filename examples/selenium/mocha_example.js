var assert = require('assert'),
    webdriver = require('selenium-webdriver'),
    test = require('selenium-webdriver/testing');

var driver;

test.describe('Blazedemo verification', function() {
    this.timeout(30000);

    test.before(function () {
        driver = new webdriver.Builder().
            withCapabilities(webdriver.Capabilities.chrome()).
            build();
    });

    test.after(function () {
        driver.quit();
    });

    test.it('home page loads', function () {
        driver.get('http://blazedemo.com/');
        driver.getTitle().then(function (title) {
            assert.equal(title, 'Simple Flights App');
        });
    });

});
