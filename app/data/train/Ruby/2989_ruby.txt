MAILCHIMP_API_KEY = Configuration[:mailchimp_api_key] if ActiveRecord::Base.connection.table_exists? 'configurations'
