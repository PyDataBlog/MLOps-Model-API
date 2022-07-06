INSERT INTO
  museotest.tecnicas
  (
    id,
    tecnica,
    creacion,
    modifica
  )
  SELECT
    IdTecnica,
    Tecnica,
    now(),
    now()
FROM original.tecnica ori
LEFT OUTER JOIN museotest.tecnicas o
  ON (ori.IdTecnica = o.id)
ORDER BY ori.IdTecnica ASC;
