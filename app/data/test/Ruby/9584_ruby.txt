Deface::Override.new(:virtual_path => 'spree/admin/configurations/index',
                     :name => 'add_taxon_splashes_to_admin_configurations_menu',
                     :insert_bottom => "[data-hook='admin_configurations_menu']",
                     :text => %q{
<tr>
  <td><%= link_to 'Author Pages', admin_authors_path %></td>
  <td>Manage Author Pages</td>
</tr>
                        })
