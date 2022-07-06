--MySQL: comandos básicos
-------------------------
--
--Como conectar ao MySQL
--Conecte-se ao servidor MySQL com o seguinte comando:
--    mysql -h NOME-DO-SERVIDOR -u NOME-DO-USUARIO
--    
--Após dar o comando, o sistema vai pedir a sua senha de usuário.
--Dica: Se você instalou o MySQL no seu próprio PC, pode usar localhost no lugar de 
--NOME-DO-SERVIDOR.
--
--No exemplo, a seguir, veja como se conectar como root:
--    mysql -u root -p
--    
--Se não foi você quem fez a instalação, pode precisar contatar o administrador do banco de 
--dados para saber que valores usar para conectar ao servidor MySQL.

-------------------------

--Como criar um banco de dados no MySQL

    CREATE DATABASE nome-do-banco;
    CREATE DATABASE testes;

--Para ver todos os bancos de dados existentes no servidor:

    SHOW DATABASES;

--Usando um banco de dados existente:

    USE nome-do-banco;
    USE testes;
    
-------------------------

--Como criar uma tabela no MySQL
    
    USE testes; # Defini a tabela a ser utilizada
    
    # cria uma tabela clientes
    CREATE TABLE clientes (
      id mediumint(8) unsigned NOT NULL auto_increment,
      nomeEmpresa varchar(255),
      nomeDiretor varchar(255) default NULL,
      numEmpregados mediumint default NULL,
      PRIMARY KEY (id)) AUTO_INCREMENT=1;
      
--Data Types: https://dev.mysql.com/doc/refman/5.0/en/data-types.html

--Ver as tabelas da base de dados
    
    SHOW tables;
    
--Obter informações da tabela

    DESCRIBE clientes;
    
-------------------------

--Inserindo dados na base de dados

INSERT INTO `clientes` (`id`,`nomeEmpresa`,`nomeDiretor`,`numEmpregados`) 
    VALUES (1,"Malesuada Inc.","Johnny Pedd",4847);

INSERT INTO `clientes` (`nomeEmpresa`,`nomeDiretor`,`numEmpregados`) 
    VALUES ("Aliquam Inc.","Al Capino",4135);
    
INSERT INTO `clientes` (`nomeEmpresa`) 
    VALUES ("Geração Tec");
    
INSERT INTO `clientes` (`nomeDiretor`,`numEmpregados`) 
    VALUES ("Al Capino",4135);

-------------------------

--Fazendo consulta em todos os dados

SELECT * FROM clientes;
SELECT nomeEmpresa FROM clientes;
SELECT id, nomeEmpresa FROM clientes;
SELECT id, nomeEmpresa FROM clientes WHERE id=2;
SELECT id, nomeEmpresa FROM clientes WHERE id<>2;
SELECT * FROM clientes WHERE nomeDiretor LIKE "A%";


-------------------------

--Novamente (com extras)

USE testes;

CREATE TABLE livros (
    livro_id INT, 
    titulo   VARCHAR(50),
    autor    VARCHAR(50)
);

ALTER TABLE livros
    CHANGE COLUMN livro_id livro_id INT AUTO_INCREMENT PRIMARY KEY,
    CHANGE COLUMN autor autor_id INT,
    ADD COLUMN descricao TEXT,
    ADD COLUMN tipo ENUM('romance','poesia','drama'), 
    ADD COLUMN editora_id INT,
    ADD COLUMN ano_publicacao VARCHAR(4),
    ADD COLUMN isbn VARCHAR(20);
    
CREATE TABLE autores (
    autor_id   INT AUTO_INCREMENT PRIMARY KEY,
    primeiro_nome  VARCHAR(50),
    ultimo_nome VARCHAR(50),
    pais       VARCHAR(50)
) AUTO_INCREMENT=1;

SHOW TABLES;

# Insira 10 livros;
# Insira 10 autores;

# Escreva 20 consultas diferentes utilizando estes livros e autores


-------------------------

--Crie tabelas que reflitam as classes (do GeracaoTec-java):
--[tá lá no GIT]

-- oo.Humano
-- oo.estatico.exercicios.Conta
-- oo.jogo.Heroi (sem utilizar mais de uma tabela)
--
--// Criem 5 inserts para cada uma dessas tabelas;
--// Crie 3 selects específicos para cada uma dessas tabelas


















