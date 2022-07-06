# rails.plugin.sh
# ver 0.1
# 2014-01-14
# Francis San Juan <francis@ftsanjuan.com>
#
# Various Rails aliases and utilities
#
# @dependencies
#   rails
#   rake

# ------------------------------------
# Rails aliases
# ------------------------------------
alias rc="rails console"
alias rs="rails server"
alias rdbreset="railsdbreset"

# runs a second rails server running port 3001
# in case multiple rails applications
# need to be run locally
alias rs2="rails s --port=3001"

# ------------------------------------
# Rake
# ------------------------------------
alias brake="bundle exec rake"
alias rdm="brake db:migrate"
alias rdc="brake db:create"
alias rds="brake db:seed"

# Migrates a database up to a specified version
function rdmu() {
  bundle exec rake db:migrate:up VERSION=$@
}

# ------------------------------------
# UTILITIES
# ------------------------------------

# Full database reset
# @dependencies
#   brake
#     an alias for 'bundle exec rake' defined in this file
function railsdbreset() {
  echo "Dropping database..."
  brake db:drop
  echo "Creating database..."
  brake db:create
  echo "Performing database migrations..."
  brake db:migrate
  echo "Seeding database..."
  brake db:seed
  echo "Finished full database reset."
}
