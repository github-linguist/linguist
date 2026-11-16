-- Specify a field on the user table
DEFINE FIELD email ON TABLE user TYPE string ASSERT string::is::email($value);

-- Add a unique index on the email field to prevent duplicate values
DEFINE INDEX email ON TABLE user COLUMNS email UNIQUE;

-- Create a new event whenever a user changes their email address
DEFINE EVENT email ON TABLE user WHEN $before.email != $after.email THEN (
	CREATE event SET user = $this, time = time::now(), value = $after.email, action = 'email_changed'
);