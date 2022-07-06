CREATE TABLE accounts.organization_preferences
(
  id serial NOT NULL,
  organization_id int NOT NULL,
  day_of_week_order text NOT NULL DEFAULT 'M-S', -- other option S-S?
  CONSTRAINT pk_organization_preferences PRIMARY KEY (id),
  CONSTRAINT uq_organization UNIQUE (organization_id),
  FOREIGN KEY (organization_id) REFERENCES accounts.organizations (id)
);
ALTER TABLE accounts.organization_preferences
  OWNER TO ape;
