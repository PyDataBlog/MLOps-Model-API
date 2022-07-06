delimiter ##
create procedure registrar_processador(
IN registros_descricao VARCHAR(100), 
IN registros_usuario VARCHAR(30), 
IN modelo varchar (30),
IN clock float(11),
IN sockete varchar(10),
IN nucleos int)

BEGIN

DECLARE next_int INTEGER;
SET next_int = (SELECT AUTO_INCREMENT FROM information_schema.tables WHERE table_name = 'registros_de_entrada' AND table_schema = DATABASE());

INSERT INTO registros_de_entrada (id_unico, descricao, data, usuario) VALUES (next_int, registros_descricao, now(), registros_usuario);

INSERT INTO processador (id, modelo, clock, sockete, nucleos) VALUES (next_int, modelo, clock, sockete, nucleos);

END;
##
delimiter ;
