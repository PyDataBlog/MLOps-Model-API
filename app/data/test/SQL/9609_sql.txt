CREATE TABLE IF NOT EXISTS people (
 people_id Int primary key
 people_name 
 people_username
 people_email
 people_password
 people_sex
 people_phone
)

CREATE TABLE IF NOT EXISTS place (
 place_id primary key
 place_name
 place_description
 place_phone
 place_email
 place_type
 place_website

 
)

CREATE TABLE IF NOT EXISTS event (
event_id
event_name
event_description
event_phone
event_email
event_type
event_host_id foreign key
event_cohost_id foreign key

)