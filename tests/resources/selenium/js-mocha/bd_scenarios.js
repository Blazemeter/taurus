var assert = require('assert'),
    Mocha = require('mocha');

describe('Simple test', function() {
    this.timeout(30000);

    it('this test passes', function () {
        assert.equal(eval('1 + 2 + 3 + 4'), 10);
    });

    xit("this test is skipped", function () {
        // do nothing
    });

    it("this test is broken", function () {
       assert.equal(2 + 2 * 2, 8);
    });
});
