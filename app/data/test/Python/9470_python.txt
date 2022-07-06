# -*- coding: utf-8 -*-
##############################################################################
#
#    OpenERP, Open Source Management Solution
#    Copyright (C) Open Solutions Finland 2013.
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Affero General Public License as
#    published by the Free Software Foundation, either version 3 of the
#    License, or (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Affero General Public License for more details.
#
#    You should have received a copy of the GNU Affero General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
##############################################################################

{
    "name" : "Exporting and Intrastat",
    "version" : "1.0",
    "author" : "Open Solutions Finland",
    "description" : """
    OpenERP module for exporting goods. Intrastat additions to invoices. Adds country of origin and customs code fields to products.
    """,
    "website" : "www.opensolutions.fi",
    "depends" : ["base","product","sale","stock"],
    "category" : "Generic Modules",
    "init_xml" : [],
    "demo_xml" : [],
    "data" : [
              'product_extension_view.xml',
              'invoice_extension_view.xml'
                    ],
    'test': [
             ],
    'installable': True,
    'active': False,
    'certificate': '',
}
