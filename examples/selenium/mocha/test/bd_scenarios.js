var assert = require('assert'),
    webdriver = require('selenium-webdriver'),
    blazedemo = require('../lib/blazedemo');

var driver;

describe('Site verification', function() {
    this.timeout(30000);

    before(function () {
        driver = new webdriver.Builder().
            withCapabilities(webdriver.Capabilities.chrome()).
            build();
    });

    after(function () {
        driver.quit();
    });

    it('home page loads', function () {
        driver.get(blazedemo.homePage());
        driver.getTitle().then(function (title) {
            assert.equal(title, blazedemo.homePageTitle());
        });
    });

    it("reserve.php opens", function () {
        driver.get(blazedemo.homePage() + blazedemo.reserve());
    });

});
