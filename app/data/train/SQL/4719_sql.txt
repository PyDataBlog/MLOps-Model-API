CREATE OR REPLACE VIEW caracterizacao_its.vw_cartoes_ordernados AS (
  SELECT
    co.nome_ferramenta,
    co.nome_funcionalidade,
    co.descricao_funcionalidade,
    co.observacoes,
    co.url_documentacao,
    co.topico_funcionalidade
  FROM caracterizacao_its.temp_cartoes_ordenados co
);