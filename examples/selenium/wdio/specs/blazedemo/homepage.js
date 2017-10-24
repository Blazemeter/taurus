describe('blazedemo homepage', () => {
  it('should load', () => {
    browser.url('http://blazedemo.com/');
    var title = browser.title().value;
    title.should.be.equal("BlazeDemo");
  });
});
