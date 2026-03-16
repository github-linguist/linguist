ALTER TYPE cycling.fullname
ADD middlename text;

ALTER TYPE cycling.fullname
RENAME middlename TO middle
AND lastname TO last
AND firstname TO first;
