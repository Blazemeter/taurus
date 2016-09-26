class Suya < ActiveRecord::Base
  belongs_to :vendor

  validates :meat, presence: true
  validates :spicy, inclusion: [true, false]
  validates :price, numericality: { only_integer: true }
end
