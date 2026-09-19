// @xx
-- @xx
/*
    This is the first line of
    of a comment that spans multiple
    lines
*/
CREATE ROLE IF NOT EXISTS coach -- End of line comment
WITH LOGIN = true
AND PASSWORD = 'All4One2day!';

CREATE ROLE IF NOT EXISTS sys_admin // End of line comment
WITH LOGIN = true
AND PASSWORD = 'changeme'
AND SUPERUSER = true;

/* @name some block */
