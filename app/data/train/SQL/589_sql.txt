SELECT 
  "Students"."StudentName", COUNT("StudentCourses"."CourseID")
FROM
  "Students" LEFT JOIN "StudentCourses"
ON
  "Students"."StudentID" = "StudentCourses"."StudentID"
GROUP BY
  "Students"."StudentID";