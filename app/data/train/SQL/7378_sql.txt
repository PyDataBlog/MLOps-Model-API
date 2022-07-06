-- Table: albergue_habilitado_od3

-- DROP TABLE albergue_habilitado_od3;

CREATE TABLE albergue_habilitado_od3
(
  id bigint NOT NULL,
  fecha_hora timestamp with time zone,
  nombre_de_albergue_habilitado character varying,
  estado character varying,
  oficial character varying,
  hay_agua character varying,
  numero_ninos_0_5 integer,
  numero_ninos_5_17 integer,
  numero_adultos integer,
  numero_total integer,
  commentarios character varying,
  fotos character varying,
  geom geometry(Point,4326),
  CONSTRAINT albergue_habilitado_od3_pkey PRIMARY KEY (id )
)
WITH (
  OIDS=FALSE
);
ALTER TABLE albergue_habilitado_od3
  OWNER TO postgres;
