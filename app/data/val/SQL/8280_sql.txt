
CREATE OR REPLACE FUNCTION dondeestaban(IN text, IN text)
  RETURNS TABLE(nombre text, ageat interval, donde text) AS
$BODY$
BEGIN
   RETURN QUERY
    SELECT a.firstname, age(b.bornday,a.bornday),

CASE WHEN age(uno.bornday, todos.bornday) < 6 * '1 year'::interval THEN 'Preescolar'
        WHEN age(uno.bornday, todos.bornday) BETWEEN 6 * '1 year'::interval AND 12 * '1 year'::interval THEN 'Primaria'
        WHEN age(uno.bornday, todos.bornday) BETWEEN 12 * '1 year'::interval AND 16 * '1 year'::interval THEN 'Secundaria'
        WHEN age(uno.bornday, todos.bornday) BETWEEN 16 * '1 year'::interval AND 18 * '1 year'::interval THEN 'Bachiller'
        WHEN age(uno.bornday, todos.bornday) BETWEEN 18 * '1 year'::interval AND 22 * '1 year'::interval THEN 'Universidad'
	ELSE 'nada'
   END cat


    
    FROM 
    (SELECT firstname, bornday FROM persons WHERE firstname!=$1 AND lastname!=$2) AS a,
    (SELECT bornday FROM persons WHERE firstname=$1 AND lastname=$2) AS b;
   
END
$BODY$
  LANGUAGE plpgsql

