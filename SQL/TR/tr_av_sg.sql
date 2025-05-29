------------------------------------------------------------------------------ 
--------------------CREATE SURGERY FLAG TABLES - ALL SITES------------- 
------------------------------------------------------------------------------ 
--1)---------------- ALL SITES - SURGERY FROM AT_TREATMENT_ENGLAND ------------------ 
-- Create a surgery flag for the tumour if: 
-- there is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with surgery (event is '01a', '01b', '01z', or '01c') 
-- and the opcs4_code is in the tumour resection list 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 

CREATE TABLE tr_av_sg AS 
    WITH TEMP1 AS (
        SELECT 
            tc.tumourid, 
            (avtreat.eventdate-tc.diagnosisdatebest) AS datediff, 
            RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, 
            avtreat.eventdate, 
            avtreat.trust_code AS avsg_trust_code
        FROM tr_tumour_cohort tc 
        INNER JOIN timeframe_lookup@casref02 tim 
            ON tim.tumouricdsite3code = tc.tumour_code 
        INNER JOIN avyyyy.at_treatment_england@casref02 avtreat 
            ON avtreat.tumourid = tc.tumourid 
            AND eventcode IN ('01a', '01b', '01z', '01c') 
            AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time) 
        INNER JOIN opcs4resection_lookup@casref02 opcs 
            ON opcs.tumouricdsite3code = tc.tumour_code 
            AND TRIM(opcs.opcsresectioncode) = avtreat.opcs4_code),
            
    TEMP2 AS (
        SELECT 
            tumourid, 
            datediff, 
            rk, 
            eventdate, 
            avsg_trust_code
        FROM TEMP1
        WHERE 
            rk = 1)
    
    SELECT 
        DISTINCT 
        tumourid, 
        CASE 
            WHEN datediff IS NULL THEN 0 
            ELSE 1 
            END AS avsg_flag, 
        eventdate AS avsg_date, 
        avsg_trust_code
    FROM TEMP2;

