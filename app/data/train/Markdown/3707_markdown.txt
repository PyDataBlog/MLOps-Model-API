1. Select all data for all states.

  SELECT * FROM states;

2. Select all data for all regions.

  SELECT * FROM regions;

3. Select the state_name and population for all states.

  SELECT state_name, population FROM states;

4. Select the state_name and population for all states ordered by
population. The state with the highest population should be at the top.

  SELECT state_name, population FROM states
  ...> ORDER BY population DESC;

5. Select the state_name for the states in region 7.

  SELECT state_name FROM states
  ...> WHERE region_id = 7;

6. Select the state_name and population_density for states with a
population density over 50 ordered from least to most dense.

  SELECT state_name, population_density FROM states
  ...> WHERE population_density > 50
  ...> ORDER BY population_density ASC;

7. Select the state_name for states with a population between 1 million
and 1.5 million people.

  SELECT state_name FROM states
  ...> WHERE population > 1000000
  ...> AND population < 1500000;

8. Select the state_name and region_id for states ordered by region in ascending order.

   SELECT state_name, region_id FROM states
   ...> ORDER BY region_id ASC;

9. Select the region_name for the regions with "Central" in the name.

   SELECT region_name FROM regions
   ...> WHERE region_name LIKE '%Central%';

10. Select the region_name and the state_name for all states and
regions in ascending order by region_id. Refer to the region by name.
(This will involve joining the tables).

   SELECT regions.region_name, states.state_name
   ...> FROM regions
   ...> INNER JOIN states
   ...> ON regions.id=states.region_id;

   ![wardrobe schema](https://raw.githubusercontent.com/mrYakamoto/phase-0/master/week-8/database-intro/wardrobe_schema.png)


REFLECTION
What are databases for?
Organizing large amounts of data into an easily accessible structure.

What is a one-to-many relationship?
A not unique category that may apply to many unique instances.
for example, a DOG has a BREED. a BREED has many DOGS.

What is a primary key? What is a foreign key? How can you determine
 which is which?
A primary key is a unique id used for each row in a table. A foreign
key is created to link another table's data using its primary keys.
You can tell which is which by determining the one-to-many
relationship.

How can you select information out of a SQL database? What are some
 general guidelines for that?
 By using SQL query commands, you can select information out of a
 database using all sorts of criteria and qualifiers. Some general
 guidelines would include typing commands in all caps and ending
 statements with a semicolon.
