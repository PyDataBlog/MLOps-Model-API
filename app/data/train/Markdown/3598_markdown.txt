# Make results show in one line

There is a function to concat data to make the data transfer much easily. It is called `group_concat`. The grammar can be illustrated by one example:

```sql
SELECT province, group_concat(city order by city desc separator ';') FROM table GROUP BY province;
```

Mainly pay attention to the group_concat statement. It is to order the city by descending order and use ';' as the separator. We may also use `distinct` to the field city.

**Important**
The group_concat has a maximum size limitation. By default it is 1024. To temporarily make it larger we can use:

```sql
SET GLOBAL group_concat_max_len=102400;
SET SESSION group_concat_max_len=102400;
```
The first statement needs a root privilege.

[source](http://hchmsguo.iteye.com/blog/555543)