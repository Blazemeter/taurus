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

    test.xit("reserve.php opens", function () {
        driver.get(blazedemo.homePage() + blazedemo.reserve());
    });

    test.it("this test is broken", function () {
       assert.equal(2 + 2 * 2, 8);
    });
});
