# frozen_string_literal: true

require 'rake'

describe 'desif_abrasf_tasks' do
  before do
    Rake.application.rake_require 'tasks/desif_abrasf_tasks'
    Rake::Task.define_task :environment
  end

  describe 'abrasf-desif:db:seed' do
    let(:rake) { 'abrasf-desif:db:seed' }

    before do
      allow(Abrasf::Desif::Engine).to receive :load_seed
      Rake.application.invoke_task rake
    end

    it { expect(Abrasf::Desif::Engine).to have_received :load_seed }
  end

  describe 'abrasf-desif:city_tax_code:seed[city_ibge]' do
    let(:id) { '234567' }
    let(:rake) { "abrasf-desif:city_tax_code:seed[#{id}]" }

    before do
      allow(Abrasf::Desif::TaxCodeToCity).to receive :create
      Rake.application.invoke_task rake
    end

    it do
      expect(Abrasf::Desif::TaxCodeToCity)
        .to have_received(:create).with(city_id: id)
    end
  end
end
