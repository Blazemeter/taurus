require 'rails_helper'

RSpec.describe Suya, type: :model do
  it "exists" do
    suya = Suya.create

    assert suya
  end

  it "is invalid without meat" do
    suya = Suya.create(meat: nil)

    refute suya.valid?
  end

  it "is invalid without spicy which is a boolean" do
    suya1 = Suya.create(meat: "beef", spicy: nil, price: 20)
    suya3 = Suya.create(meat: "beef", spicy: false, price: 40)
    suya4 = Suya.create(meat: "beef", spicy: true, price: 30)

    expect(suya1).to be_invalid
    expect(suya3).to be_valid
    expect(suya3).to be_valid
  end

  it "is invalid without a price which is an integer" do
    suya1 = Suya.create(meat: "beef", spicy: true, price: "a")
    suya2 = Suya.create(meat: "beef", spicy: true, price: 10.24)
    suya3 = Suya.create(meat: "beef", spicy: true, price: 10)
    suya4 = Suya.create(meat: "beef", spicy: true, price: nil)

    assert suya1.invalid?
    assert suya2.invalid?
    assert suya3.valid?
    assert suya4.invalid?
  end
end
