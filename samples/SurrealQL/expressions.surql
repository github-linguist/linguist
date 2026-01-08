-- Select a nested array, and filter based on an attribute
SELECT emails[WHERE active = true] FROM person;

-- Select all 1st, 2nd, and 3rd level people who this specific person record knows, or likes, as separate outputs
SELECT ->knows->(? AS f1)->knows->(? AS f2)->(knows, likes AS e3 WHERE influencer = true)->(? AS f3) FROM person:tobie;

-- Select all person records (and their recipients), who have sent more than 5 emails
SELECT *, ->sent->email->to->person FROM person WHERE count(->sent->email) > 5;

-- Select other products purchased by people who purchased this laptop
SELECT <-purchased<-person->purchased->product FROM product:laptop;

-- Select products purchased by people in the last 3 weeks who have purchased the same products that we purchased
SELECT ->purchased->product<-purchased<-person->(purchased WHERE created_at > time::now() - 3w)->product FROM person:tobie;