------------------------------------------------------------------------------ 
---------------- CREATE RADIOTHERAPY FLAG TABLES ------------------ 
------------------------------------------------------------------------------ 
--23)---------------- ALL SITES - AT_TREATMENT_ENGLAND ------------------ 
-- Create a radiotherapy flag for the tumour if: 
-- There is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with radiotherapy 
--(event is either 'RT - Teletherapy' (code = 05) or 'chemoradiotherapy' (code = 04) or 'brachytherapy' (code = 06) or 'radiosurgery' (code = 22) or 'RT - Other/ NK' (code = RTX) or 'radioisotope therapy (including radioiodine)' (code = 19)) 
-- AND the event date (eventdate) occurred in the relevant timeframe (see SOP) 

CREATE TABLE tr_av_rt AS 
    WITH TEMP1 AS (
        SELECT 
            tc.tumourid, 
            avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
            RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, 
            avtreat.eventdate, 
            avtreat.trust_code AS avrt_trust_code
        FROM tr_tumour_cohort tc 
        INNER JOIN timeframe_lookup@casref02 tim 
            ON tim.tumouricdsite3code = tc.tumour_code 
        INNER JOIN avyyyy.at_treatment_england@casref02 avtreat 
            ON avtreat.tumourid = tc.tumourid 
            AND eventcode IN ('04', '05', '06', '22', 'RTX', '19', '13') 
            AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.RADIO_TIME)),
            
    TEMP2 AS (
        SELECT 
            tumourid, 
            datediff, 
            rk, 
            eventdate, 
            avrt_trust_code 
        FROM TEMP1
        WHERE rk = 1)
        
    SELECT 
        DISTINCT 
        tumourid, 
        CASE 
            WHEN datediff IS NULL THEN 0 
            ELSE 1 
        END AS avrt_flag, 
        eventdate AS avrt_date, 
        avrt_trust_code
    FROM TEMP2;
