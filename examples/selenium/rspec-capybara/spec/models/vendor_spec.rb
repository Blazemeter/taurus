require 'rails_helper'

RSpec.describe Vendor, type: :model do
  it "has a name" do
    jeff = Vendor.create(name: "jeff")

    assert jeff.valid?
  end

  it "can have many suyas" do
    jeff = Vendor.create(name: "jeff")
    jeff.suyas << Suya.create(meat: "beef", spicy: false, price: 300)
    jeff.suyas << Suya.create(meat: "beef", spicy: false, price: 320)

    assert_equal 2, jeff.suyas.count
  end
end
