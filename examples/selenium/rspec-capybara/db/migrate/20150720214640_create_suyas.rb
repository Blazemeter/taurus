class CreateSuyas < ActiveRecord::Migration
  def change
    create_table :suyas do |t|

      t.timestamps null: false
    end
  end
end
