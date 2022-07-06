### VERSION 0.0.3 => 0.0.4

  If you use Devise of callback named verify_authenticity_token we need to add a initializers like

  Add `config/initializers/locales.rb`

  ```
  module LocalesControllerCallbacks
    extend ActiveSupport::Concern

    included do
      skip_before_action :verify_authenticity_token
    end
  end

  LocalesController.send(:include, LocalesControllerCallbacks)
  ```

  Otherwise is ok.
