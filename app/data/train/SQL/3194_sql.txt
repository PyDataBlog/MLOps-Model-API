-- id	effectiveTime	active	moduleId	definitionStatusId
-- P = 900000000000074008
-- D = 900000000000073002

-- Assuming same definition patterns as AMT
-- Qualifiers primitive, and everythingelse defined.
-- classification can reveal issues...
-- Some components are from other modules AMT, SCT.. will need updating
-- first all the primitive classes
select id,20161027,1,"NZMT_ModuleId", 900000000000074008 from ct where is_retired = 0
union
select id,20161027,1,"NZMT_ModuleId", 900000000000074008 from doseform where is_retired = 0
union
select id,20161027,1,"NZMT_ModuleId", 900000000000074008 from uom where is_retired = 0
union
select id,20161027,1,"NZMT_ModuleId", 900000000000074008 from tp where is_retired = 0
union
select id,20161027,1,"NZMT_ModuleId", 900000000000074008 from pf where is_retired = 0
union
select id,20161027,1,"NZMT_ModuleId", 900000000000074008 from substance where is_retired = 0
union
select id,20161027,1,"NZMT_ModuleId", 900000000000073002 from mp where is_retired = 0
union
-- Here's the defined drig stuff
select id,20161027,1,"NZMT_ModuleId", 900000000000073002 from mpuu where is_retired = 0
union
select id,20161027,1,"NZMT_ModuleId", 900000000000073002 from mpp where is_retired = 0
union
select id,20161027,1,"NZMT_ModuleId", 900000000000073002 from tpuu where is_retired = 0
union
select id,20161027,1,"NZMT_ModuleId", 900000000000073002 from tpp where is_retired = 0
union
select id,20161027,1,"NZMT_ModuleId", 900000000000073002 from ctpp where is_retired = 0;
