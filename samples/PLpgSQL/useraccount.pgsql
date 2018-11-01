-- $Id$
--- @file
-- VBox Test Manager Database Stored Procedures - UserAccounts.
--

--
-- Copyright (C) 2012-2014 Oracle Corporation
--
-- This file is part of VirtualBox Open Source Edition (OSE), as
-- available from http://www.virtualbox.org. This file is free software;
-- you can redistribute it and/or modify it under the terms of the GNU
-- General Public License (GPL) as published by the Free Software
-- Foundation, in version 2 as it comes in the "COPYING" file of the
-- VirtualBox OSE distribution. VirtualBox OSE is distributed in the
-- hope that it will be useful, but WITHOUT ANY WARRANTY of any kind.
--
-- The contents of this file may alternatively be used under the terms
-- of the Common Development and Distribution License Version 1.0
-- (CDDL) only, as it comes in the "COPYING.CDDL" file of the
-- VirtualBox OSE distribution, in which case the provisions of the
-- CDDL are applicable instead of those of the GPL.
--
-- You may elect to license modified versions of this file under the
-- terms and conditions of either the GPL or the CDDL or both.
--

\set ON_ERROR_STOP 1
\connect testmanager;

---
-- Checks if the user name and login name are unique, ignoring a_uidIgnore.
-- Raises exception if duplicates are found.
--
-- @internal
--
CREATE OR REPLACE FUNCTION UserAccountLogic_checkUniqueUser(a_sUsername TEXT, a_sLoginName TEXT, a_uidIgnore INTEGER)
    RETURNS VOID AS $$
    DECLARE
        v_cRows INTEGER;
    BEGIN
        -- sUserName
        SELECT  COUNT(*) INTO v_cRows
        FROM    Users
        WHERE   sUsername = a_sUsername
            AND tsExpire  = 'infinity'::TIMESTAMP
            AND uid      <> a_uidIgnore;
        IF v_cRows <> 0 THEN
            RAISE EXCEPTION 'Duplicate user name "%" (% times)', a_sUsername, v_cRows;
        END IF;

        -- sLoginName
        SELECT  COUNT(*) INTO v_cRows
        FROM    Users
        WHERE   sLoginName = a_sLoginName
            AND tsExpire   = 'infinity'::TIMESTAMP
            AND uid       <> a_uidIgnore;
        IF v_cRows <> 0 THEN
            RAISE EXCEPTION 'Duplicate login name "%" (% times)', a_sUsername, v_cRows;
        END IF;
    END;
$$ LANGUAGE plpgsql;
              
---
-- Check that the user account exists.
-- Raises exception if it doesn't.
--
-- @internal
--
CREATE OR REPLACE FUNCTION UserAccountLogic_checkExists(a_uid INTEGER) RETURNS VOID AS $$
    DECLARE
        v_cUpdatedRows INTEGER;
    BEGIN
        IF NOT EXISTS(  SELECT  *
                        FROM    Users
                        WHERE   uid       = a_uid
                            AND tsExpire  = 'infinity'::TIMESTAMP ) THEN
            RAISE EXCEPTION 'User with ID % does not currently exist', a_uid;
        END IF;
    END;
$$ LANGUAGE plpgsql;

---
-- Historize a row.
-- @internal
--
CREATE OR REPLACE FUNCTION UserAccountLogic_historizeEntry(a_uid INTEGER, a_tsExpire TIMESTAMP WITH TIME ZONE) 
    RETURNS VOID AS $$
    DECLARE
        v_cUpdatedRows INTEGER;
    BEGIN
        UPDATE  Users
          SET   tsExpire = a_tsExpire
          WHERE uid      = a_uid
            AND tsExpire = 'infinity'::TIMESTAMP;
        GET DIAGNOSTICS v_cUpdatedRows = ROW_COUNT;
        IF v_cUpdatedRows <> 1 THEN
            IF v_cUpdatedRows = 0 THEN
                RAISE EXCEPTION 'User with ID % does not currently exist', a_uid;
            END IF;
            RAISE EXCEPTION 'Integrity error in UserAccounts: % current rows with uid=%d', v_cUpdatedRows, a_uid;
        END IF;
    END;
$$ LANGUAGE plpgsql;


---
-- Adds a new user.
--
CREATE OR REPLACE FUNCTION UserAccountLogic_addEntry(a_uidAuthor INTEGER, a_sUsername TEXT, a_sEmail TEXT, a_sFullName TEXT, 
                                                     a_sLoginName TEXT) 
    RETURNS VOID AS $$
    DECLARE
        v_cRows INTEGER;
    BEGIN
        PERFORM UserAccountLogic_checkUniqueUser(a_sUsername, a_sLoginName, -1);
        INSERT INTO Users(uidAuthor, sUsername, sEmail, sFullName, sLoginName)
            VALUES (a_uidAuthor, a_sUsername, a_sEmail, a_sFullName, a_sLoginName);
    END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION UserAccountLogic_editEntry(a_uidAuthor INTEGER, a_uid INTEGER, a_sUsername TEXT, a_sEmail TEXT, 
                                                      a_sFullName TEXT, a_sLoginName TEXT)  
    RETURNS VOID AS $$
    BEGIN
        PERFORM UserAccountLogic_checkExists(a_uid);
        PERFORM UserAccountLogic_checkUniqueUser(a_sUsername, a_sLoginName, a_uid);

        PERFORM UserAccountLogic_historizeEntry(a_uid, CURRENT_TIMESTAMP);
        INSERT INTO Users (uid, uidAuthor, sUsername, sEmail, sFullName, sLoginName)
            VALUES (a_uid, a_uidAuthor, a_sUsername, a_sEmail, a_sFullName, a_sLoginName);
    END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION UserAccountLogic_delEntry(a_uidAuthor INTEGER, a_uid INTEGER) RETURNS VOID AS $$
    DECLARE
        v_Row           Users%ROWTYPE;
        v_tsEffective   TIMESTAMP WITH TIME ZONE;
    BEGIN
        --
        -- To preserve the information about who deleted the record, we try to
        -- add a dummy record which expires immediately.  I say try because of 
        -- the primary key, we must let the new record be valid for 1 us. :-(
        --

        SELECT  * INTO STRICT v_Row
        FROM    Users
        WHERE   uid      = a_uid
            AND tsExpire = 'infinity'::TIMESTAMP;

        v_tsEffective := CURRENT_TIMESTAMP - INTERVAL '1 microsecond';
        IF v_Row.tsEffective < v_tsEffective THEN
            PERFORM UserAccountLogic_historizeEntry(a_uid, v_tsEffective);
            v_Row.tsEffective = v_tsEffective;
            v_Row.tsExpire    = CURRENT_TIMESTAMP;
            INSERT INTO Users VALUES (v_Row.*);
        ELSE
            PERFORM UserAccountLogic_historizeEntry(a_uid, CURRENT_TIMESTAMP);
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            RAISE EXCEPTION 'User with ID % does not currently exist', a_uid;
        WHEN TOO_MANY_ROWS THEN
            RAISE EXCEPTION 'Integrity error in UserAccounts: Too many current rows for %', a_uid;
    END;
$$ LANGUAGE plpgsql;

