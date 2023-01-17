SELECT * FROM vgsales;

-- Example when using case statements
#SELECT distinct(Platform),
#CASE
#	WHEN Platform LIKE 'X%' THEN 'Microsoft'
#    WHEN Platform LIKE 'PS%' THEN 'Sony'
#    WHEN Platform LIKE 'PC%' THEN 'Computer'
#    WHEN Platform Like 'GB%' THEN 'Nintendo'
#    ELSE 'Other'
#END AS Console
#FROM vgsales
#ORDER BY Console;

SELECT Manufacterer,(Global_Sales) FROM vgsales as s
LEFT JOIN manufacterer as m ON s.Platform = m.Platform
GROUP BY Manufacterer;


SELECT distinct(YEAR) FROM vgsales;






