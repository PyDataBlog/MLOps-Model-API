# == Schema Information
#
# Table name: uploaded_files
#
#  id             :integer          not null, primary key
#  title          :string
#  description    :text
#  archive        :string
#  created_at     :datetime         not null
#  updated_at     :datetime         not null
#  publication_id :integer
#

FactoryGirl.define do
  factory :uploaded_file do
    title "MyString"
    description "MyText"
    archive "MyString"
  end
end
