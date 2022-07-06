module Dhole
  class Category < ActiveRecord::Base
    self.table_name = 'category'
    self.primary_key = 'cat_id'
    # cat_files, cat_title
    
    def members
      CategoryLink.where(cl_to: cat_title) || []
    end
    def member_page_titles
      page_ids = members.select{|cl| cl.cl_type == 'page'}.map(&:cl_from)
      Page.find(page_ids).map(&:page_title)
    end
    def member_file_titles
      file_page_ids = members.select{|cl| cl.cl_type == 'file'}.map(&:cl_from)
      Page.find(file_page_ids).map(&:page_title) # img_name == page_title
    end
    # careful! mass instantiation!
    def member_pages
      page_ids = members.select{|cl| cl.cl_type == 'page'}.map(&:cl_from)
      Page.find(page_ids)
    end
    # careful! mass instantiation!
    def member_files
      file_titles = members_file_titles
      Image.find(file_titles)
    end
    # return only members of specified type ('file','page', 'subcat')
    def members_by_type(mtype)
      # TBD
    end
  end
end
