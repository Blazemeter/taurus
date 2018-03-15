var assert = require('assert'),
    webdriver = require('selenium-webdriver');

var driver;

describe('Blazedemo verification', function() {
    this.timeout(30000);

    before(function () {
        driver = new webdriver.Builder().
            withCapabilities(webdriver.Capabilities.chrome()).
            build();
    });

    after(function () {
        driver.quit();
    });

    it('selenium-mocha', function () {
        driver.get('http://blazedemo.com/');
        driver.getTitle().then(function (title) {
            assert.equal(title, 'BlazeDemo');
        });
    });

});
