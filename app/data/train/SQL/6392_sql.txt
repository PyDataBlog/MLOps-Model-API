SELECT i.IgrejaLotacao, SUM(CASE WHEN l.Movimento = "1" THEN l.ValorLancamento ELSE 0 END) AS ValorEntrada,
SUM(CASE WHEN l.Movimento = "2" THEN l.ValorLancamento ELSE 0 END) AS ValorSaida
FROM financ_lancamentos AS l
INNER JOIN sec_igreja AS i ON i.IgrejaID = l.Cod_Lotacao
GROUP BY l.Cod_Lotacao