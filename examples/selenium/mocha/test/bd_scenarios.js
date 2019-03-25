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
        return driver.get(blazedemo.homePage())
            .then(() => driver.getTitle())
            .then(title => {
                assert.equal(title, blazedemo.homePageTitle());
            });
    });

    it("reserve.php opens", function () {
        return driver.get(blazedemo.homePage() + blazedemo.reserve());
    });

});
