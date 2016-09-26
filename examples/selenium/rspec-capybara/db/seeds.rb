# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rake db:seed (or created alongside the db with db:setup).
#
# Examples:
#
#   cities = City.create([{ name: 'Chicago' }, { name: 'Copenhagen' }])
#   Mayor.create(name: 'Emanuel', city: cities.first)

jeff = Vendor.create(name: "jeff")
ikem = Vendor.create(name: "ikem")
nad = Vendor.create(name: "nad")

jeff.suyas << Suya.create(meat: "beef", spicy: false, price: 1000)
jeff.suyas << Suya.create(meat: "beef", spicy: true, price: 1100)
ikem.suyas << Suya.create(meat: "beef", spicy: true, price: 1200)
ikem.suyas << Suya.create(meat: "beef", spicy: true, price: 1300)
ikem.suyas << Suya.create(meat: "beef", spicy: false, price: 1400)
nad.suyas << Suya.create(meat: "beef", spicy: false, price: 1800)
nad.suyas << Suya.create(meat: "beef", spicy: false, price: 1800)
nad.suyas << Suya.create(meat: "beef", spicy: true, price: 1900)
nad.suyas << Suya.create(meat: "beef", spicy: true, price: 2000)
