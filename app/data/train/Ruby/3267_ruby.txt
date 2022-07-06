# encoding: UTF-8
# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 20160205055959) do

  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"

  create_table "ages", force: :cascade do |t|
    t.string   "age_range"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "coeds", force: :cascade do |t|
    t.string   "mixed"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "daytimes", force: :cascade do |t|
    t.string   "time_of_day"
    t.datetime "created_at",  null: false
    t.datetime "updated_at",  null: false
  end

  create_table "frequencies", force: :cascade do |t|
    t.string   "how_often"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "genders", force: :cascade do |t|
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "industries", force: :cascade do |t|
    t.string   "field"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "interactions", force: :cascade do |t|
    t.string   "conversation_level"
    t.datetime "created_at",         null: false
    t.datetime "updated_at",         null: false
  end

  create_table "kohorts", force: :cascade do |t|
    t.integer  "zip_code"
    t.datetime "created_at",      null: false
    t.datetime "updated_at",      null: false
    t.integer  "industry_id"
    t.integer  "frequency_id"
    t.integer  "coed_id"
    t.integer  "interaction_id"
    t.integer  "conversation_id"
    t.integer  "age_id"
  end

  create_table "users", force: :cascade do |t|
    t.string   "email"
    t.string   "first_name"
    t.string   "password_digest"
    t.datetime "created_at",            null: false
    t.datetime "updated_at",            null: false
    t.string   "password_confirmation"
    t.integer  "gender_id"
    t.integer  "age_id"
  end

  add_index "users", ["email"], name: "index_users_on_email", unique: true, using: :btree

end
