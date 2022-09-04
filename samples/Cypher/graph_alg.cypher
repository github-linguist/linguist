MERGE (mark:Person:DevRel {name: "Mark"})
MERGE (praveena:Person:Engineering {name: "Praveena"})
MERGE (joe:Person:Field {name: "Joe"})
MERGE (lju:Person:DevRel {name: "Lju"})
MERGE (zhen:Person:Engineering {name: "Zhen"})
MERGE (stefan:Person:Field {name: "Stefan"})
MERGE (alicia:Person:Product {name: "Alicia"})
MERGE (martin:Person:Engineering {name: "Martin"})
MERGE (jake:Person:Product {name: "Jake"})

MERGE (zhen)-[:KNOWS]-(stefan)
MERGE (zhen)-[:KNOWS]-(lju)
MERGE (zhen)-[:KNOWS]-(praveena)
MERGE (zhen)-[:KNOWS]-(martin)
MERGE (mark)-[:KNOWS]-(jake)
MERGE (alicia)-[:KNOWS]-(jake)

MERGE (alicia)-[:FOLLOWS]->(joe)
MERGE (joe)-[:FOLLOWS]->(mark)
MERGE (joe)-[:FOLLOWS]->(praveena)
MERGE (joe)-[:FOLLOWS]->(zhen)
MERGE (mark)-[:FOLLOWS]->(stefan)
MERGE (stefan)-[:FOLLOWS]->(joe)
MERGE (praveena)-[:FOLLOWS]->(joe)


MATCH (p:Person {name: "Praveena"})
CALL apoc.path.expand(p, "KNOWS", null, 1, 2)
YIELD path
RETURN path, length(path) AS hops
ORDER BY hops;