require "selenium-webdriver"
require "rspec"

describe "Blazedemo" do

  before(:each) do
    @driver = Selenium::WebDriver.for :firefox
    @base_url = "http://blazedemo.com"
    @accept_next_alert = true
    @driver.manage.timeouts.implicit_wait = 30
  end
 
  after(:each) do
    @driver.quit
  end

  it "blazedemo.com example" do
    @driver.get(@base_url + "/purchase.php")
    @driver.find_element(:id, "inputName").clear
    @driver.find_element(:id, "inputName").send_keys "First Last"
    @driver.find_element(:id, "inputName").send_keys :return
    sleep 1.0
  end

end
