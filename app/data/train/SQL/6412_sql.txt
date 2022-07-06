--DROP SCHEMA IF EXISTS :schemaName;
--CREATE SCHEMA :schemaName;
--USE :schemaName;

DROP SCHEMA IF EXISTS saccharomyces_cerevisiae_module_47_1g;
CREATE SCHEMA saccharomyces_cerevisiae_module_47_1g;
USE saccharomyces_cerevisiae_module_47_1g;

CREATE TABLE biomodule(
	module_id   INT UNSIGNED NOT NULL AUTO_INCREMENT,
	identifier  VARCHAR(50) NOT NULL,
	version     INT ZEROFILL,
	name        VARCHAR(255),
	description TEXT,
	type_id     INT UNSIGNED,
	source_id   INT UNSIGNED,
	PRIMARY KEY (module_id)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

ALTER TABLE biomodule ADD CONSTRAINT fk_biomodule_type_id
FOREIGN KEY (type_id) REFERENCES type(type_id)
ON DELETE CASCADE;

ALTER TABLE biomodule ADD CONSTRAINT fk_biomodule_source_id
FOREIGN KEY (source_id) REFERENCES source(source_id)
ON DELETE CASCADE;

CREATE TABLE biomodule_complex(
	module_complex_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
	module_id   INT UNSIGNED NOT NULL,
	PRIMARY KEY (module_complex_id)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

ALTER TABLE biomodule_complex ADD CONSTRAINT fk_biomodule_complex_module_id
FOREIGN KEY (module_id) REFERENCES biomodule(module_id)
ON DELETE CASCADE;

CREATE TABLE biomodule_coexpression(
	module_coexpression_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
	module_id   INT UNSIGNED NOT NULL,
	PRIMARY KEY (module_coexpression_id)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

ALTER TABLE biomodule_coexpression ADD CONSTRAINT fk_biomodule_coexpression_module_id
FOREIGN KEY (module_id) REFERENCES biomodule(module_id)
ON DELETE CASCADE;

CREATE TABLE biomodule_comodulation(
	module_comodulation_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
	module_id   INT UNSIGNED NOT NULL,
	PRIMARY KEY (module_comodulation_id)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

ALTER TABLE biomodule_comodulation ADD CONSTRAINT fk_biomodule_comodulation_module_id
FOREIGN KEY (module_id) REFERENCES biomodule(module_id)
ON DELETE CASCADE;


CREATE TABLE biomodule_process(
	module_process_id  INT UNSIGNED NOT NULL AUTO_INCREMENT,
	module_id   INT UNSIGNED NOT NULL,
	PRIMARY KEY (module_process_id)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

ALTER TABLE biomodule_process ADD CONSTRAINT fk_biomodule_process_module_id
FOREIGN KEY (module_id) REFERENCES biomodule(module_id)
ON DELETE CASCADE;

CREATE TABLE source(
	source_id        INT UNSIGNED NOT NULL AUTO_INCREMENT,
	name             VARCHAR(30) NOT NULL,
	description      VARCHAR(100),
	module_type_id   INT UNSIGNED NOT NULL,
	PRIMARY KEY (source_id),
	UNIQUE(name)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

ALTER TABLE source ADD CONSTRAINT fk_source_module_type_id
FOREIGN KEY (module_type_id) REFERENCES type(type_id)
ON DELETE CASCADE;

CREATE TABLE type(
    type_id         INT UNSIGNED NOT NULL AUTO_INCREMENT,
	name            VARCHAR(30) NOT NULL,
	description     VARCHAR(100),
	PRIMARY KEY (type_id),
	UNIQUE(name)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

CREATE TABLE biomodule_component(
	module_id      INT UNSIGNED,
	component_id   INT UNSIGNED,
	PRIMARY KEY (module_id, component_id)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

ALTER TABLE biomodule_component ADD CONSTRAINT fk_biomodule_component_module_id
FOREIGN KEY (module_id) REFERENCES biomodule(module_id)
ON DELETE CASCADE;

ALTER TABLE biomodule_component ADD CONSTRAINT fk_biomodule_component_component_id
FOREIGN KEY (component_id) REFERENCES component(component_id)
ON DELETE CASCADE;

CREATE TABLE component(
	component_id   INT UNSIGNED NOT NULL AUTO_INCREMENT,
	PRIMARY KEY (component_id)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

CREATE TABLE component_protein (
	component_protein_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
	component_id   INT UNSIGNED,
	translation_id      INT UNSIGNED,
	PRIMARY KEY (component_protein_id)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

ALTER TABLE component_protein ADD CONSTRAINT fk_component_protein_component_id
FOREIGN KEY (component_id) REFERENCES component(component_id)
ON DELETE CASCADE;

CREATE TABLE component_transcript (
	component_transcript_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
	component_id   INT UNSIGNED,
	transcript_id      INT UNSIGNED,
	PRIMARY KEY (component_transcript_id)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

ALTER TABLE component_transcript ADD CONSTRAINT fk_component_transcript_component_id
FOREIGN KEY (component_id) REFERENCES component(component_id)
ON DELETE CASCADE;

CREATE TABLE component_gene (
	component_gene_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
	component_id   INT UNSIGNED,
	gene_id      INT UNSIGNED,
	PRIMARY KEY (component_gene_id)
)ENGINE=MyISAM DEFAULT CHARSET=latin1;

ALTER TABLE component_gene ADD CONSTRAINT fk_component_gene_component_id
FOREIGN KEY (component_id) REFERENCES component(component_id)
ON DELETE CASCADE;

CREATE TABLE module_stable_id (
    module_id   INT UNSIGNED NOT NULL AUTO_INCREMENT,
    stable_id   VARCHAR(128) NOT NULL,
    version     INT,
    PRIMARY KEY (module_id)
)

ALTER TABLE module_stable_id ADD CONSTRAINT fk_module_stable_id
FOREIGN KEY (module_id) REFERENCES biomodule(module_id)
ON DELETE CASCADE;




--CREATE INDEX idx_peptide_sequence_id ON peptide(sequence_id);
