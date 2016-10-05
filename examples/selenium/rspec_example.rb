require "selenium-webdriver"
require "rspec"

describe "Google Search" do

  before(:each) do
    @driver = Selenium::WebDriver.for :firefox
    @base_url = "https://www.google.com/"
    @accept_next_alert = true
    @driver.manage.timeouts.implicit_wait = 30
  end
 
  after(:each) do
    @driver.quit   
  end
 
  it "search Taurus on google" do
    @driver.get(@base_url + "/")
    @driver.find_element(:id, "lst-ib").clear
    @driver.find_element(:id, "lst-ib").send_keys "BlazeMeter Taurus"
    @driver.find_element(:id, "lst-ib").send_keys :return
    sleep 1.0
  end

end
