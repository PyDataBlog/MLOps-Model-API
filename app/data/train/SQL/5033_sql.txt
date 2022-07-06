-- Verify uadt:modifiedTransformsTrigger on pg

BEGIN;

SELECT 1/COUNT(*)
  FROM information_schema.triggers
  WHERE event_object_table = 'transforms'
  AND trigger_schema = '1'
  AND trigger_name = 'update_transforms_modified_time';

COMMIT;
