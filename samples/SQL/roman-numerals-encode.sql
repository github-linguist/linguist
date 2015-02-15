--
-- This only works under Oracle and has the limitation of 1 to 3999
--- Higher numbers in the Middle Ages were represented by  "superscores" on top of the numeral to multiply by 1000
--- Vertical bars to the sides multiply by 100. So |M| means 100,000
-- When the query is run, user provides the Arabic numerals for the ar_year
-- A.Kebedjiev
--

SELECT to_char(to_char(to_date(&ar_year,'YYYY'), 'RRRR'), 'RN') AS roman_year FROM DUAL;

-- or you can type in the year directly

SELECT to_char(to_char(to_date(1666,'YYYY'), 'RRRR'), 'RN') AS roman_year FROM DUAL;

ROMAN_YEAR
MDCLXVI
