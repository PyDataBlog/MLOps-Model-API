require 'rails_helper'

RSpec.describe "professores/show", type: :view do
  before(:each) do
    @professor = assign(:professor, Professor.create!(
      :nome => "Nome",
      :bio_curta => "Bio Curta",
      :contato => "Contato"
    ))
  end

  it "renders attributes in <p>" do
    render
    expect(rendered).to match(/Nome/)
    expect(rendered).to match(/Bio Curta/)
    expect(rendered).to match(/Contato/)
  end
end
