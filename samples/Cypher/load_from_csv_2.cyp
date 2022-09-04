// clear data
MATCH (n)
DETACH DELETE n;
// load Employee nodes
LOAD CSV WITH HEADERS FROM 'file:///people.csv' AS row
MERGE (e:Employee {employeeId: row.employeeId, name: row.Name})
RETURN count(e);
// load Company nodes
LOAD CSV WITH HEADERS FROM 'file:///companies.csv' AS row
WITH row WHERE row.Id IS NOT NULL
WITH row,
(CASE row.BusinessType
 WHEN 'P' THEN 'Public'
 WHEN 'R' THEN 'Private'
 WHEN 'G' THEN 'Government'
 ELSE 'Other' END) AS type
MERGE (c:Company {companyId: row.Id, hqLocation: coalesce(row.Location, "Unknown")})
SET c.emailAddress = CASE trim(row.Email) WHEN "" THEN null ELSE row.Email END
SET c.businessType = type
RETURN count(c);
// create relationships
LOAD CSV WITH HEADERS FROM 'file:///people.csv' AS row
MATCH (e:Employee {employeeId: row.employeeId})
MATCH (c:Company {companyId: row.Company})
MERGE (e)-[:WORKS_FOR]->(c)
RETURN *;