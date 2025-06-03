-- NAMESPACE: clear

DO $$
 DECLARE
   _sql text;
 BEGIN
    SELECT INTO _sql
        string_agg(format('DROP FUNCTION %s;', p.oid::regprocedure), E'\n')
    FROM pg_proc p
    JOIN pg_roles r ON p.proowner = r.oid
    WHERE r.rolname = :'user'
        AND p.prokind = 'f';

    IF _sql IS NOT NULL THEN
        EXECUTE _sql;
    ELSE
        RAISE NOTICE 'No functions found.';
    END IF;
 END
$$ LANGUAGE plpgsql;
