
--
-- TOC entry 170 (class 1259 OID 16521)
-- Name: articulos; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE articulos (
    id_articulo integer NOT NULL,
    id_tipo_mueble integer,
    progresivo bigint,
    caracteristicas character varying(300),
    id_estatus integer
);


ALTER TABLE public.articulos OWNER TO postgres;

--
-- TOC entry 171 (class 1259 OID 16524)
-- Name: articulos_id_articulo_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE articulos_id_articulo_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.articulos_id_articulo_seq OWNER TO postgres;

--
-- TOC entry 2019 (class 0 OID 0)
-- Dependencies: 171
-- Name: articulos_id_articulo_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE articulos_id_articulo_seq OWNED BY articulos.id_articulo;


--
-- TOC entry 172 (class 1259 OID 16526)
-- Name: cat_direccion_ejecutiva; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE cat_direccion_ejecutiva (
    id_direccion_ejecutiva integer NOT NULL,
    nombre character varying(50)
);


ALTER TABLE public.cat_direccion_ejecutiva OWNER TO postgres;

--
-- TOC entry 173 (class 1259 OID 16529)
-- Name: cat_direccion_ejecutiva_id_direccion_ejecutiva_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE cat_direccion_ejecutiva_id_direccion_ejecutiva_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cat_direccion_ejecutiva_id_direccion_ejecutiva_seq OWNER TO postgres;

--
-- TOC entry 2020 (class 0 OID 0)
-- Dependencies: 173
-- Name: cat_direccion_ejecutiva_id_direccion_ejecutiva_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE cat_direccion_ejecutiva_id_direccion_ejecutiva_seq OWNED BY cat_direccion_ejecutiva.id_direccion_ejecutiva;


--
-- TOC entry 174 (class 1259 OID 16531)
-- Name: cat_estatus; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE cat_estatus (
    id_estatus integer NOT NULL,
    descripcion character varying(100)
);


ALTER TABLE public.cat_estatus OWNER TO postgres;

--
-- TOC entry 175 (class 1259 OID 16534)
-- Name: cat_estatus_id_estatus_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE cat_estatus_id_estatus_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cat_estatus_id_estatus_seq OWNER TO postgres;

--
-- TOC entry 2021 (class 0 OID 0)
-- Dependencies: 175
-- Name: cat_estatus_id_estatus_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE cat_estatus_id_estatus_seq OWNED BY cat_estatus.id_estatus;


--
-- TOC entry 176 (class 1259 OID 16536)
-- Name: cat_tipo_mueble; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE cat_tipo_mueble (
    id_tipo_mueble integer NOT NULL,
    descripcion character varying(400),
    estatus character varying(50),
    clave integer
);


ALTER TABLE public.cat_tipo_mueble OWNER TO postgres;

--
-- TOC entry 177 (class 1259 OID 16539)
-- Name: cat_tipo_mueble_id_tipo_mueble_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE cat_tipo_mueble_id_tipo_mueble_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cat_tipo_mueble_id_tipo_mueble_seq OWNER TO postgres;

--
-- TOC entry 2022 (class 0 OID 0)
-- Dependencies: 177
-- Name: cat_tipo_mueble_id_tipo_mueble_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE cat_tipo_mueble_id_tipo_mueble_seq OWNED BY cat_tipo_mueble.id_tipo_mueble;


--
-- TOC entry 178 (class 1259 OID 16541)
-- Name: cat_tipo_usuario; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE cat_tipo_usuario (
    id_tipo_usuario integer NOT NULL,
    descripcion character varying(30)
);


ALTER TABLE public.cat_tipo_usuario OWNER TO postgres;

--
-- TOC entry 179 (class 1259 OID 16544)
-- Name: cat_tipo_usuario_id_tipo_usuario_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE cat_tipo_usuario_id_tipo_usuario_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.cat_tipo_usuario_id_tipo_usuario_seq OWNER TO postgres;

--
-- TOC entry 2023 (class 0 OID 0)
-- Dependencies: 179
-- Name: cat_tipo_usuario_id_tipo_usuario_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE cat_tipo_usuario_id_tipo_usuario_seq OWNED BY cat_tipo_usuario.id_tipo_usuario;


--
-- TOC entry 180 (class 1259 OID 16546)
-- Name: usuario_has_articulo; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE usuario_has_articulo (
    id_usuario_articulo integer NOT NULL,
    id_usuario integer,
    id_articulo integer
);


ALTER TABLE public.usuario_has_articulo OWNER TO postgres;

--
-- TOC entry 181 (class 1259 OID 16549)
-- Name: usuario_has_articulo_id_usuario_articulo_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE usuario_has_articulo_id_usuario_articulo_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.usuario_has_articulo_id_usuario_articulo_seq OWNER TO postgres;

--
-- TOC entry 2024 (class 0 OID 0)
-- Dependencies: 181
-- Name: usuario_has_articulo_id_usuario_articulo_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE usuario_has_articulo_id_usuario_articulo_seq OWNED BY usuario_has_articulo.id_usuario_articulo;


--
-- TOC entry 182 (class 1259 OID 16551)
-- Name: usuarios; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE usuarios (
    id_usuario integer NOT NULL,
    nombre character varying(50),
    ap_paterno character varying(50),
    ap_materno character varying(50),
    cargo character varying(50),
    num_empleado integer,
    id_direccion_ejecutiva integer,
    id_tipo_usuario integer NOT NULL,
    email character varying(50),
    contrasenia character varying(20),
    estatus smallint
);


ALTER TABLE public.usuarios OWNER TO postgres;

--
-- TOC entry 183 (class 1259 OID 16554)
-- Name: usuarios_id_tipo_usuario_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE usuarios_id_tipo_usuario_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.usuarios_id_tipo_usuario_seq OWNER TO postgres;

--
-- TOC entry 2025 (class 0 OID 0)
-- Dependencies: 183
-- Name: usuarios_id_tipo_usuario_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE usuarios_id_tipo_usuario_seq OWNED BY usuarios.id_tipo_usuario;


--
-- TOC entry 184 (class 1259 OID 16556)
-- Name: usuarios_id_usuario_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE usuarios_id_usuario_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.usuarios_id_usuario_seq OWNER TO postgres;

--
-- TOC entry 2026 (class 0 OID 0)
-- Dependencies: 184
-- Name: usuarios_id_usuario_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE usuarios_id_usuario_seq OWNED BY usuarios.id_usuario;


--
-- TOC entry 1861 (class 2604 OID 16558)
-- Name: id_articulo; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY articulos ALTER COLUMN id_articulo SET DEFAULT nextval('articulos_id_articulo_seq'::regclass);


--
-- TOC entry 1862 (class 2604 OID 16559)
-- Name: id_direccion_ejecutiva; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY cat_direccion_ejecutiva ALTER COLUMN id_direccion_ejecutiva SET DEFAULT nextval('cat_direccion_ejecutiva_id_direccion_ejecutiva_seq'::regclass);


--
-- TOC entry 1863 (class 2604 OID 16560)
-- Name: id_estatus; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY cat_estatus ALTER COLUMN id_estatus SET DEFAULT nextval('cat_estatus_id_estatus_seq'::regclass);


--
-- TOC entry 1864 (class 2604 OID 16561)
-- Name: id_tipo_mueble; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY cat_tipo_mueble ALTER COLUMN id_tipo_mueble SET DEFAULT nextval('cat_tipo_mueble_id_tipo_mueble_seq'::regclass);


--
-- TOC entry 1865 (class 2604 OID 16562)
-- Name: id_tipo_usuario; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY cat_tipo_usuario ALTER COLUMN id_tipo_usuario SET DEFAULT nextval('cat_tipo_usuario_id_tipo_usuario_seq'::regclass);


--
-- TOC entry 1866 (class 2604 OID 16563)
-- Name: id_usuario_articulo; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY usuario_has_articulo ALTER COLUMN id_usuario_articulo SET DEFAULT nextval('usuario_has_articulo_id_usuario_articulo_seq'::regclass);


--
-- TOC entry 1867 (class 2604 OID 16564)
-- Name: id_usuario; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY usuarios ALTER COLUMN id_usuario SET DEFAULT nextval('usuarios_id_usuario_seq'::regclass);


--
-- TOC entry 1868 (class 2604 OID 16565)
-- Name: id_tipo_usuario; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY usuarios ALTER COLUMN id_tipo_usuario SET DEFAULT nextval('usuarios_id_tipo_usuario_seq'::regclass);


--
-- TOC entry 1996 (class 0 OID 16521)
-- Dependencies: 170
-- Data for Name: articulos; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (1, 1, 222, 'silla azul', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (2, 1, 223, 'silla verde', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (3, 1, 224, 'silla amarilla', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (5, 1, 0, 'jhgfds', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (10, 1, 0, 'Silla de metal', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (19, 1, 0, 'Silla de metal', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (20, 1, 0, 'Silla de metal', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (4, 1, 0, 'Silla de metal', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (9, 1, 0, 'Silla de metal', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (11, 1, 0, 'Silla de metal', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (6, 2, 76543, 'Esquinero de madera', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (7, 1, 7655, 'Silla de metal azul', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (8, 3, 1234, 'manta de papel', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (12, 4, 1334132, 'manta de plastico', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (13, 6, 45345, 'cajon de madera', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (14, 6, 45345, 'cajon de madera', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (15, 7, 1312, '4vvte', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (16, 4, 123123, 'dfrv', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (17, 4, 1341, 'njrjnjfrk', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (18, 6, 313, 'silla de papel', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (21, 2, 7655689, 'mesa de selofan', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (22, 5, 759474, 'telefono de metal', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (23, 4, 542421444142422442, 'asus', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (24, 4, 987989, 'uihgibhui', 1);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (25, 2, 0, 'uhgutiuie', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (26, 5, 4756758575, 'jusrhuyih', 2);
INSERT INTO articulos (id_articulo, id_tipo_mueble, progresivo, caracteristicas, id_estatus) VALUES (27, 2, 7575869696797, 'jiahiuhuibhsuiybhsiyhi', 2);


--
-- TOC entry 2027 (class 0 OID 0)
-- Dependencies: 171
-- Name: articulos_id_articulo_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('articulos_id_articulo_seq', 27, true);


--
-- TOC entry 1998 (class 0 OID 16526)
-- Dependencies: 172
-- Data for Name: cat_direccion_ejecutiva; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO cat_direccion_ejecutiva (id_direccion_ejecutiva, nombre) VALUES (1, 'CGM');


--
-- TOC entry 2028 (class 0 OID 0)
-- Dependencies: 173
-- Name: cat_direccion_ejecutiva_id_direccion_ejecutiva_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('cat_direccion_ejecutiva_id_direccion_ejecutiva_seq', 1, true);


--
-- TOC entry 2000 (class 0 OID 16531)
-- Dependencies: 174
-- Data for Name: cat_estatus; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO cat_estatus (id_estatus, descripcion) VALUES (1, 'Deteriorado');
INSERT INTO cat_estatus (id_estatus, descripcion) VALUES (2, 'Optimo');
INSERT INTO cat_estatus (id_estatus, descripcion) VALUES (3, 'Inactivo');


--
-- TOC entry 2029 (class 0 OID 0)
-- Dependencies: 175
-- Name: cat_estatus_id_estatus_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('cat_estatus_id_estatus_seq', 3, true);


--
-- TOC entry 2002 (class 0 OID 16536)
-- Dependencies: 176
-- Data for Name: cat_tipo_mueble; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (1, 'silla', '1', 100);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (2, 'escritorio', '1', 101);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (3, 'escritorio', '1', 1001);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (4, 'pc escritorio', '1', 1002);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (5, 'pc escritorio 2', '1', 1003);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (6, 'pc escritorio 2', '1', 1002);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (7, 'pc escritorio 3', '1', 1003);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (8, 'pc escritorio 4', '1', 1004);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (9, 'pc escritorio 5', '1', 1005);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (10, 'pc escritorio 6', '1', 1006);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (11, 'pc escritorio 7', '1', 1007);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (12, 'pc escritorio 8', '1', 1008);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (13, 'pc escritorio 9', '1', 1009);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (14, 'pc escritorio 10', '1', 1010);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (15, 'pc escritorio 11', '1', 1011);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (16, 'pc escritorio 12', '1', 1012);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (17, 'pc escritorio 13', '1', 1013);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (18, 'pc escritorio 14', '1', 1014);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (19, 'pc escritorio 15', '1', 1015);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (20, 'silla', NULL, 123);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (21, 'silla', '1', 123);
INSERT INTO cat_tipo_mueble (id_tipo_mueble, descripcion, estatus, clave) VALUES (22, 'Silla', '1', 12343);


--
-- TOC entry 2030 (class 0 OID 0)
-- Dependencies: 177
-- Name: cat_tipo_mueble_id_tipo_mueble_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('cat_tipo_mueble_id_tipo_mueble_seq', 22, true);


--
-- TOC entry 2004 (class 0 OID 16541)
-- Dependencies: 178
-- Data for Name: cat_tipo_usuario; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO cat_tipo_usuario (id_tipo_usuario, descripcion) VALUES (1, 'Administrador');


--
-- TOC entry 2031 (class 0 OID 0)
-- Dependencies: 179
-- Name: cat_tipo_usuario_id_tipo_usuario_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('cat_tipo_usuario_id_tipo_usuario_seq', 1, true);


--
-- TOC entry 2006 (class 0 OID 16546)
-- Dependencies: 180
-- Data for Name: usuario_has_articulo; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO usuario_has_articulo (id_usuario_articulo, id_usuario, id_articulo) VALUES (4, 3, 1);
INSERT INTO usuario_has_articulo (id_usuario_articulo, id_usuario, id_articulo) VALUES (6, 3, 3);
INSERT INTO usuario_has_articulo (id_usuario_articulo, id_usuario, id_articulo) VALUES (5, 3, 2);


--
-- TOC entry 2032 (class 0 OID 0)
-- Dependencies: 181
-- Name: usuario_has_articulo_id_usuario_articulo_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('usuario_has_articulo_id_usuario_articulo_seq', 6, true);


--
-- TOC entry 2008 (class 0 OID 16551)
-- Dependencies: 182
-- Data for Name: usuarios; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO usuarios (id_usuario, nombre, ap_paterno, ap_materno, cargo, num_empleado, id_direccion_ejecutiva, id_tipo_usuario, email, contrasenia, estatus) VALUES (3, 'Ernesto', 'Andrade', 'Luna', 'Director', 210310108, 1, 1, 'n3to', '123', 1);
INSERT INTO usuarios (id_usuario, nombre, ap_paterno, ap_materno, cargo, num_empleado, id_direccion_ejecutiva, id_tipo_usuario, email, contrasenia, estatus) VALUES (1, 'nombre', 'apP', 'apM', 'cargo', 0, 1, 1, 'email', '0', 1);
INSERT INTO usuarios (id_usuario, nombre, ap_paterno, ap_materno, cargo, num_empleado, id_direccion_ejecutiva, id_tipo_usuario, email, contrasenia, estatus) VALUES (4, 'usuario1', 'usuario', 'usuario', 'El de los chescos', 11111, 1, 1, 'usuario1', '000', 1);
INSERT INTO usuarios (id_usuario, nombre, ap_paterno, ap_materno, cargo, num_empleado, id_direccion_ejecutiva, id_tipo_usuario, email, contrasenia, estatus) VALUES (5, 'Oscar', 'Amaya', 'Ordoñez', 'Administrador', 26424676, 1, 1, 'osskar_the_pimp@hotmail.com', '123', 1);
INSERT INTO usuarios (id_usuario, nombre, ap_paterno, ap_materno, cargo, num_empleado, id_direccion_ejecutiva, id_tipo_usuario, email, contrasenia, estatus) VALUES (6, 'Oscar', 'Amaya', 'Ordoñez', 'Administrador', 26424676, 1, 1, 'osskar_the_pimp@hotmail.com', '123', 1);


--
-- TOC entry 2033 (class 0 OID 0)
-- Dependencies: 183
-- Name: usuarios_id_tipo_usuario_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('usuarios_id_tipo_usuario_seq', 1, false);


--
-- TOC entry 2034 (class 0 OID 0)
-- Dependencies: 184
-- Name: usuarios_id_usuario_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('usuarios_id_usuario_seq', 6, true);


--
-- TOC entry 1870 (class 2606 OID 16567)
-- Name: id_articulo; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY articulos
    ADD CONSTRAINT id_articulo PRIMARY KEY (id_articulo);


--
-- TOC entry 1872 (class 2606 OID 16569)
-- Name: id_direccion_ejecutiva; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY cat_direccion_ejecutiva
    ADD CONSTRAINT id_direccion_ejecutiva PRIMARY KEY (id_direccion_ejecutiva);


--
-- TOC entry 1874 (class 2606 OID 16571)
-- Name: id_estatus; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY cat_estatus
    ADD CONSTRAINT id_estatus PRIMARY KEY (id_estatus);


--
-- TOC entry 1876 (class 2606 OID 16573)
-- Name: id_tipo_mueble; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY cat_tipo_mueble
    ADD CONSTRAINT id_tipo_mueble PRIMARY KEY (id_tipo_mueble);


--
-- TOC entry 1878 (class 2606 OID 16575)
-- Name: id_tipo_usuario; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY cat_tipo_usuario
    ADD CONSTRAINT id_tipo_usuario PRIMARY KEY (id_tipo_usuario);


--
-- TOC entry 1882 (class 2606 OID 16577)
-- Name: id_usuario; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY usuarios
    ADD CONSTRAINT id_usuario PRIMARY KEY (id_usuario);


--
-- TOC entry 1880 (class 2606 OID 16579)
-- Name: id_usuario_articulo; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY usuario_has_articulo
    ADD CONSTRAINT id_usuario_articulo PRIMARY KEY (id_usuario_articulo);


--
-- TOC entry 1885 (class 2606 OID 16580)
-- Name: id_articulo; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY usuario_has_articulo
    ADD CONSTRAINT id_articulo FOREIGN KEY (id_articulo) REFERENCES articulos(id_articulo);


--
-- TOC entry 1887 (class 2606 OID 16585)
-- Name: id_direccion_ejecutiva; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY usuarios
    ADD CONSTRAINT id_direccion_ejecutiva FOREIGN KEY (id_direccion_ejecutiva) REFERENCES cat_direccion_ejecutiva(id_direccion_ejecutiva);


--
-- TOC entry 1883 (class 2606 OID 16590)
-- Name: id_estatus; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY articulos
    ADD CONSTRAINT id_estatus FOREIGN KEY (id_estatus) REFERENCES cat_estatus(id_estatus);


--
-- TOC entry 1884 (class 2606 OID 16595)
-- Name: id_tipo_mueble; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY articulos
    ADD CONSTRAINT id_tipo_mueble FOREIGN KEY (id_tipo_mueble) REFERENCES cat_tipo_mueble(id_tipo_mueble);


--
-- TOC entry 1888 (class 2606 OID 16600)
-- Name: id_tipo_usuario; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY usuarios
    ADD CONSTRAINT id_tipo_usuario FOREIGN KEY (id_tipo_usuario) REFERENCES cat_tipo_usuario(id_tipo_usuario);


--
-- TOC entry 1886 (class 2606 OID 16605)
-- Name: id_usuario; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY usuario_has_articulo
    ADD CONSTRAINT id_usuario FOREIGN KEY (id_usuario) REFERENCES usuarios(id_usuario);


--
-- TOC entry 2017 (class 0 OID 0)
-- Dependencies: 6
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


-- Completed on 2015-05-19 17:21:32

--
-- PostgreSQL database dump complete
--

