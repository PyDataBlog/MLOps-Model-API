class AddStoreVariantToSpreeAllegroStoreVariants < ActiveRecord::Migration
  def change
    add_reference :spree_allegro_store_variants, :store_variant, index: true
  end
end
