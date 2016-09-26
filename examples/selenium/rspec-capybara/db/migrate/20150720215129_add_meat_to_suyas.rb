class AddMeatToSuyas < ActiveRecord::Migration
  def change
    add_column :suyas, :meat, :string
  end
end
