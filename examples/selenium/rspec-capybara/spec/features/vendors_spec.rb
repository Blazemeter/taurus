require 'rails_helper'

RSpec.feature "Vendors", type: :feature do
  scenario "the vendors index page can show all of the vendors" do
    Vendor.create(name: "jeff")
    Vendor.create(name: "ikem")
    Vendor.create(name: "nad")

    visit vendors_path

    expect(page).to have_selector("h1", text: "Vendors And Their Suyas")
    expect(page).to have_content("jeff")
    expect(page).to have_content("ikem")
    expect(page).to have_content("nad")
    expect(page).to have_selector("div.vendor", count: 3)
  end

  scenario "the vendors index page can show the vendors' suyas" do
    jeff = Vendor.create(name: "jeff")
    ikem = Vendor.create(name: "ikem")
    jeff.suyas << Suya.create(meat: "beef", spicy: false, price: 400)
    jeff.suyas << Suya.create(meat: "ram", spicy: true, price: 410)
    ikem.suyas << Suya.create(meat: "beef", spicy: true, price: 200)
    ikem.suyas << Suya.create(meat: "liver", spicy: true, price: 210)
    ikem.suyas << Suya.create(meat: "ram", spicy: false, price: 220)

    visit vendors_path

    within("div.vendor", text: "jeff") do
      expect(page).to have_selector("li.suyas", count:2)
    end
    within("div.vendor", text: "ikem") do
      expect(page).to have_selector("li.suyas", count:3)
    end

    expect(page).to have_selector("li.suyas", count: 5)
  end
end
