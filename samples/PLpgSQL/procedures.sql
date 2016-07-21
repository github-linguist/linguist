load 'plpgsql';
load 'plpgsql_lint';

DROP FUNCTION IF EXISTS list_sites();
CREATE OR REPLACE FUNCTION list_sites() RETURNS TABLE (fc json) AS
$func$
BEGIN
RETURN QUERY SELECT row_to_json(feat_col) FROM (
    SELECT 'FeatureCollection' AS type, array_to_json(array_agg(feat)) AS features FROM (
            SELECT DISTINCT ON (new_id) 'Feature' AS type, ST_ASGeoJSON(loc.geom)::json AS geometry, row_to_json(
                (SELECT prop FROM (SELECT new_id) AS prop)) AS properties FROM location loc) AS feat) AS feat_col;
END;
$func$ LANGUAGE plpgsql;


DROP FUNCTION IF EXISTS get_observations(character varying, integer);
CREATE OR REPLACE FUNCTION get_observations(kind varchar, site_id integer) RETURNS TABLE (fc json) AS
$func$
BEGIN
    IF kind = 'o2_abs' THEN
        RETURN QUERY SELECT array_to_json(array_agg(row_to_json(obs))) FROM (
            SELECT observation_date AS date, o2_abs AS value FROM oxygen WHERE new_id = site_id) AS obs;
    ELSIF kind = 'o2_rel' THEN
        RETURN QUERY SELECT array_to_json(array_agg(row_to_json(obs))) FROM (
            SELECT observation_date AS date, o2_rel AS value FROM oxygen WHERE new_id = site_id) AS obs;
    ELSIF kind = 'temp' THEN
        RETURN QUERY SELECT array_to_json(array_agg(row_to_json(obs))) FROM (
            SELECT observation_date AS date, temp AS value FROM oxygen WHERE new_id = site_id) AS obs;
    END IF;
END;
$func$ LANGUAGE plpgsql;
