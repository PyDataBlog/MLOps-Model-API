DELETE FROM `items_to_parts`;

ALTER TABLE `items_to_parts`
	MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=1;

ALTER TABLE `items_to_parts`
MODIFY column `integrated` tinyint(2) NOT NULL;


INSERT into `items_to_parts` (`item_id`,`part_id`,`part_type`,`integrated`)
values  (1, 1,'sensor', 1),
		(1, 2,'sensor', 0),
		(1, 3,'sensor', 0),
		(1, 4,'sensor', 0),
		(1, 5,'sensor', 0),
		(1, 6,'sensor', 0),
		(1, 7,'sensor', 0),
		(1, 8,'sensor', 0),
		(1, 1,'actuator', 1),
		(1, 2,'actuator', 1),
		(1, 3,'actuator', 0),
		(1, 4,'actuator', 0),
		(1, 5,'actuator', 0),
		(1, 6,'actuator', 0),
		(1, 7,'actuator', 0),
		(1, 8,'actuator', 0),
		(1, 9,'actuator', 0),
		(1, 10,'actuator', 0);

INSERT into `items_to_parts` (`item_id`,`part_id`,`part_type`,`integrated`)
values  (2, 1,'sensor', 1),
		(2, 2,'sensor', 1),
		(2, 3,'sensor', 0),
		(2, 4,'sensor', 0),
		(2, 5,'sensor', 0),
		(2, 6,'sensor', 0),
		(2, 7,'sensor', 0),
		(2, 8,'sensor', 0),
		(2, 1,'actuator', 1),
		(2, 2,'actuator', 1),
		(2, 3,'actuator', 1),
		(2, 4,'actuator', 1),
		(2, 5,'actuator', 0),
		(2, 6,'actuator', 0),
		(2, 7,'actuator', 0),
		(2, 8,'actuator', 0),
		(2, 9,'actuator', 0),
		(2, 10,'actuator', 0);

INSERT into `items_to_parts` (`item_id`,`part_id`,`part_type`,`integrated`)
values  (3, 1,'sensor', 0),
		(3, 2,'sensor', 0),
		(3, 3,'sensor', 1),
		(3, 4,'sensor', 0),
		(3, 5,'sensor', 0),
		(3, 6,'sensor', 1),
		(3, 7,'sensor', 0),
		(3, 8,'sensor', 0),
		(3, 9,'sensor', 0),
		(3, 10,'sensor', 1),
		(3, 11,'sensor', 0),
		(3, 12,'sensor', 1),
		(3, 1,'actuator', 0),
		(3, 2,'actuator', 0),
		(3, 3,'actuator', 0),
		(3, 4,'actuator', 0),
		(3, 5,'actuator', 1),
		(3, 6,'actuator', 1),
		(3, 7,'actuator', 1),
		(3, 8,'actuator', 0),
		(3, 9,'actuator', 0),
		(3, 10,'actuator', 0),
		(3, 11,'actuator', 0),
		(3, 12,'actuator', 0),
		(3, 13,'actuator', 1),
		(3, 14,'actuator', 1),
		(3, 15,'actuator', 0);
