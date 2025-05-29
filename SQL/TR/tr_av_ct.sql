------------------------------------------------------------------------------ 
------------------------ CREATE CHEMO FLAG TABLES ------------------------- 
------------------------------------------------------------------------------ 
--19)---------------- ALL SITES - AVCT TABLE ------------------------------- 
-- Create a chemo flag for the tumour if: 
-- There is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with chemotherapy (event is either 'Cytotoxic Chemotherapy' (code = 02) or 'CT - Other' (code = CTX) or 'chemoradiotherapy' (code = 04) or 'Immunotherapy' (code = 15)) 
-- AND the event date (eventdate) occurred in the relevant timeframe (see SOP) 

CREATE TABLE tr_av_ct AS 
    WITH TEMP1 AS (
        SELECT 
            tc.tumourid, 
            avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
            RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, 
            avtreat.eventdate, 
            avtreat.trust_code AS avct_trust_code
        FROM tr_tumour_cohort tc 
        INNER JOIN timeframe_lookup@casref02 tim 
            ON tim.tumouricdsite3code = tc.tumour_code 
        INNER JOIN avyyyy.at_treatment_england@casref02 avtreat 
            ON avtreat.tumourid = tc.tumourid 
            AND (eventcode IN ('04', '03_15', '03_21', '14', '21', 'CTX') 
                  OR substr(eventcode, 1, 2) IN ('02', '15'))
            AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.CHEMO_TIME)),
        
    TEMP2 AS (
        SELECT 
            tumourid, 
            datediff,
            rk,
            eventdate,
            avct_trust_code 
        FROM TEMP1 
        WHERE   
            rk = 1)

    SELECT
        DISTINCT 
        tumourid, 
        CASE 
            WHEN datediff IS NULL THEN 0 
            ELSE 1 
        END AS avct_flag, 
        eventdate AS avct_date, 
        avct_trust_code
    FROM TEMP2;