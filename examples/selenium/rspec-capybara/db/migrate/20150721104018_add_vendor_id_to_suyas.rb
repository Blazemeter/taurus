class AddVendorIdToSuyas < ActiveRecord::Migration
  def change
    add_reference :suyas, :vendor, index: true, foreign_key: true
  end
end
