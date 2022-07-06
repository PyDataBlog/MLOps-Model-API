class AddImageFileNameToErrorMessage < ActiveRecord::Migration
  def change
		add_column :error_messages, :image_file_name, :string
		add_column :error_messages, :image_content_type, :string
		add_column :error_messages, :image_updated_at, :datetime
  end
end
