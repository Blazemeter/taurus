class AddPriceToSuyas < ActiveRecord::Migration
  def change
    add_column :suyas, :price, :integer
  end
end
