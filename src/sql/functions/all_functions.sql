-- NAMESPACE: all
-- REQUIRES: clear

CREATE OR REPLACE FUNCTION create_new_schema(_schema_name text)
 RETURNS void AS $$

 DECLARE
 BEGIN
    EXECUTE 'DROP SCHEMA IF EXISTS "' || _schema_name || '" CASCADE';
    EXECUTE 'CREATE SCHEMA IF NOT EXISTS "' || _schema_name || '"';
 END

$$ LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION drop_existing_schema(_schema_name text)
 RETURNS void AS $$

 DECLARE
 BEGIN
    EXECUTE 'DROP SCHEMA IF EXISTS "' || _schema_name || '" CASCADE';
 END

$$ LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION clear_connection(_workspace text)
 RETURNS void AS $$

 DECLARE
 BEGIN
    EXECUTE 'SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE query LIKE
      ''SELECT ST_AsText(ST_Force2D(ST_Envelope(ST_Extent("the_geom"::geometry)))) FROM %' || _workspace || '%''';
 END

$$ LANGUAGE PLPGSQL;
