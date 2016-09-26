class AddSpicyToSuyas < ActiveRecord::Migration
  def change
    add_column :suyas, :spicy, :boolean
  end
end
