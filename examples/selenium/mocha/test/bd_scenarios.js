var assert = require('assert'),
    test = require('selenium-webdriver/testing'),
    webdriver = require('selenium-webdriver'),
    blazedemo = require('../lib/blazedemo');

var driver;

test.describe('Site verification', function() {
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
        driver.get(blazedemo.homePage());
        driver.getTitle().then(function (title) {
            assert.equal(title, blazedemo.homePageTitle());
        });
    });

    test.it("reserve.php opens", function () {
        driver.get(blazedemo.homePage() + blazedemo.reserve());
    });

});
