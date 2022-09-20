WITH "https://api.stackexchange.com/2.2/questions?pagesize=100&order=desc&sort=creation&tagged=neo4j&site=stackoverflow&filter=!5-i6Zw8Y)4W7vpy91PMYsKM-k9yzEsSC1_Uxlf" AS url
CALL apoc.load.json(url,'$.items[?(@.answer_count>0)].answers[*]') YIELD value
MERGE (a:Answer {id: value.answer_id})
  ON CREATE SET a.accepted = value.is_accepted,
                a.shareLink = value.share_link,
                a.lastActivityDate = value.last_activity_date,
                a.creationDate = value.creation_date,
                a.title = value.title,
                a.score = value.score
MERGE (q:Question {id: value.question_id})
MERGE (a)-[rel:POSTED_TO]->(q)
WITH a as answer, value.owner as value
MERGE (u:User {userId: value.user_id})
  ON CREATE SET u.displayName = value.display_name,
                u.userType = value.user_type,
                u.reputation = value.reputation,
                u.userLink = value.link
MERGE (u)-[rel2:SUBMITTED]->(answer)
RETURN count(answer)