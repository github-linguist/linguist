-- Drop all writes to the reading table. We don't need every reading.
DEFINE TABLE reading DROP;

-- Define a table as a view which aggregates data from the reading table
DEFINE TABLE temperatures_by_month AS
	SELECT
		count() AS total,
		time::month(recorded_at) AS month,
		math::mean(temperature) AS average_temp
	FROM reading
	GROUP BY city
;

-- Add a new temperature reading with some basic attributes
CREATE reading SET
	temperature = 27.4,
	recorded_at = time::now(),
	city = 'London',
	location = (-0.118092, 51.509865)
;