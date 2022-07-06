SELECT kdsup FROM m04supl



SELECT t02_id, tanggal,  
m03.kd_tabung AS kd_tabung, m03.jenis_tabung AS jenis_tabung, 
m03.harga AS harga, m04.kdsup AS kdsup, 
m04.nmsup AS nmsup, jumlah, t02.keterangan 
FROM kkp.t02beli t02
INNER JOIN m03tabu m03
ON m03.kd_tabung = t02.kd_tabung
INNER JOIN m04supl m04
ON m04.kdsup = t02.kdsupl;

INSERT INTO kkp.t02beli (tanggal, kd_tabung, kdsupl, jumlah, keterangan)
VALUES (?,?,?,?,?);

SELECT t02_id, tanggal,  
m03.kd_tabung AS kd_tabung, m03.jenis_tabung AS jenis_tabung, 
m03.harga AS harga, m04.kdsup AS kdsup, 
m04.nmsup AS nmsup, jumlah, t02.keterangan 
FROM kkp.t02beli t02
INNER JOIN m03tabu m03
ON m03.kd_tabung = t02.kd_tabung
INNER JOIN m04supl m04
ON m04.kdsup = t02.kdsupl
WHERE tanggal = ? AND m03.kd_tabung LIKE ? AND m04.kdsup LIKE ?;

DELETE FROM t02beli WHERE t02_id= ?;

UPDATE  t02beli SET 
tanggal = ?, 
kd_tabung= ?, 
kdsupl= ?, 
jumlah = ? 
keterangan = ?
WHERE t02_id = ?;