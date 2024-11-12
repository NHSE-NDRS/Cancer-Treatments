
DROP TABLE tr_tumour_cohort PURGE;
DROP TABLE tr_av_sg PURGE;
DROP TABLE tr_hes_sg PURGE;
DROP TABLE tr_av_liver PURGE;
DROP TABLE tr_hes_liver PURGE;
DROP TABLE tr_av_oesoph PURGE;
DROP TABLE tr_hes_oesoph PURGE;
DROP TABLE tr_av_stomach PURGE;
DROP TABLE tr_hes_stomach PURGE;
DROP TABLE tr_av_bladder PURGE;
DROP TABLE tr_hes_bladder PURGE;
DROP TABLE tr_av_conebiops PURGE;
DROP TABLE tr_hes_conebiops PURGE;
DROP TABLE tr_av_lymph PURGE;
DROP TABLE tr_hes_lymph PURGE;
DROP TABLE tr_av_colorec PURGE;
DROP TABLE tr_hes_colorec PURGE;
DROP TABLE tr_av_coloappen PURGE;
DROP TABLE tr_hes_coloappen PURGE;
DROP TABLE tr_av_ct PURGE;
DROP TABLE tr_sact PURGE;
DROP TABLE tr_sact_2 PURGE;
DROP TABLE tr_hes_ct PURGE;
DROP TABLE tr_av_rt PURGE;
DROP TABLE tr_rtds PURGE;
DROP TABLE tr_rtds_2 PURGE;
DROP TABLE treatment_table_1321_4p9 PURGE;

-- This is the SQL to generate the treatment flags (resection, chemo, radio) publication, which includes demographic & geographic breakdowns
-- This SQL code creates a table for cancer diagnoses between 2013 and 2021 with flags if a treatment has occurred wihtin the defined timeframes, and surgical procedure list.

--It uses the opcs4resection and timeframe lookup tables.

--1. Set your connection to to the data server.
--2. Create each table in turn in the SQL, starting with the cohort of interest. 
--If limiting the cohort, this should be done in the first table (tr_tumour_cohort_d) 
--3. The last table brings all the previous tables together into the final export. 
--4. After each new table is created, indexing it to create database statistics optimises performance. 
	--This is included throughout using the create index and execute commands 
	--Without manual indexing, database stats are automatically generated overnight
	--The username of the analyst running the pipeline will need to be inserted at 'username' 
	--'__' preceeding a table name, e.g. __.at_tumour_skin, denotes where the table is saved in an analyst's individual table space.
	--If, after creating and indexing a table, it needs to be rerun, it may be more efficient to truncate the table than drop and create it again, e.g.: 
		--Truncate table tr_tumour_cohort; 
		--insert into tr_tumour_cohort_d 
--5. For use of the final table, refer to treatment_table_YYYY_4pX e.g. treatment_table_1321_4p9
--6. If analysing in stata, the code below can be used to collapse the data down so it is not identifiable (example below groups by stage, cancer type & diagnosis year) 
	--collapse (count) tumourid, by (cancergroup stage_group rt_flag ct_flag SG_flag diagnosisyear) 

------------------------------------------------------------------------------ 
------------------------------------------------------------------------------
 -------- CREATE TUMOUR COHORT TABLE -------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
drop table tr_tumour_cohort purge;
CREATE TABLE tr_tumour_cohort AS

--Skin cancer have been defined in the at_tumour_skin table and so the skin cohort needs to be selected separately to the cohort for other tumours and joined together 
WITH skin_cohort AS
--Create cohort of non-keratinocyte skin cancers 
(SELECT ats.patientid, ats.tumourid, ats.diagnosisdatebest, ats.diagnosisyear, avt.nhsnumber, avt.figo, avt.gender, avt.ethnicity, avt.morph_icd10_o2, avt.fiveyearageband, avt.age, avt.dedup_flag, avt.site_icd10r4_o2_from2013, avt.site_icd10r4_o2_3char_from2013, avt.ctry_code, avt.statusofregistration   
,CASE WHEN tumour_type_2 = 'Melanoma' THEN 'NON-KC_MELANOMA' 
    WHEN tumour_type_2 = 'Rare' THEN 'NON-KC_RARE'    
	WHEN tumour_type_1 = 'Extramammary paget disease' THEN 'NON-KC_EMPD'  
    WHEN tumour_type_1 = 'Melanoma in situ' THEN 'NON-KC_MELANOMA_INSITU'  
END AS tumour_code
FROM av2021.at_tumour_skin_england@casref01 ats
LEFT JOIN av2021.at_tumour_england@casref01 avt ON ats.tumourid=avt.tumourid
WHERE ats.diagnosisyear between 2013 and 2021
AND (ats.tumour_type_2 IN ('Melanoma', 'Rare') OR ats.tumour_type_1 in ('Melanoma in situ','Extramammary paget disease'))
AND avt.ctry_code = 'E'
AND avt.statusofregistration = 'F'
AND avt.dedup_flag = '1'
AND avt.age BETWEEN 0 AND 200 
AND avt.gender IN (1,2)

UNION

--Create cohort of keratinoctye skin cancers following the first ever registration of BCC and first ever registration of cSCC tumours in addition to all genital BCC tumours and all genital cSCC tumours 
SELECT ats.patientid, ats.tumourid, ats.diagnosisdatebest, ats.diagnosisyear, avt.nhsnumber, avt.figo, avt.gender, avt.ethnicity, avt.morph_icd10_o2, avt.fiveyearageband, avt.age, avt.dedup_flag, avt.site_icd10r4_o2_from2013, avt.site_icd10r4_o2_3char_from2013, avt.ctry_code, avt.statusofregistration   
, CASE WHEN tumour_type_3 = 'BCC' THEN 'KC_BCC'
       WHEN tumour_type_3 = 'cSCC' THEN 'KC_CSCC'
 END AS tumour_code
FROM av2021.at_tumour_skin_england@casref01 ats
LEFT JOIN av2021.at_tumour_england@casref01 avt ON ats.tumourid=avt.tumourid
WHERE ats.diagnosisyear between 2013 and 2021
AND (ats.tumour_type_4 IN ('Genital BCC', 'Genital cSCC')
OR ats.tumour_type_5 IN ('First BCC', 'First cSCC'))
AND avt.ctry_code = 'E'
AND avt.statusofregistration = 'F'
AND avt.dedup_flag = '1'
AND avt.age BETWEEN 0 AND 200 
AND avt.gender IN (1,2)),

-- Create tumour cohort for all other (non skin) tumours 
non_skin AS
(SELECT tumourid, patientid, nhsnumber, diagnosisdatebest, site_icd10r4_o2_from2013, figo, gender, ethnicity, morph_icd10_o2, fiveyearageband, age

--Create amended tumour_code variable to differentiate between different types of cancers within ICD10-defined sites.  
,CASE 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C48') 
AND (avt.morph_icd10_o2 NOT IN (8693, 8800, 8801, 8802, 8803, 8804, 8805, 8806, 8963, 8990, 8991, 9040, 9041, 9042, 9043, 9044, 8810, 9490, 9500) 
AND (avt.morph_icd10_o2 NOT BETWEEN 8811 AND 8921) 
AND (avt.morph_icd10_o2 NOT BETWEEN 9120 AND 9373) 
AND (avt.morph_icd10_o2 NOT BETWEEN 9530 AND 9582) 
AND avt.gender=2)
THEN 'C48OVARY' 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C48') THEN 'C48OTHER' 
WHEN avt.site_icd10r4_o2_from2013 IN ('D391') THEN 'D39OVARY'
WHEN avt.site_icd10r4_o2_3char_from2013 = 'D39' AND avt.site_icd10r4_o2_from2013 NOT IN ('D391') THEN 'D39OTHER'
WHEN avt.site_icd10r4_o2_from2013 IN ('D292') THEN 'D29TESTES'
WHEN avt.site_icd10r4_o2_3char_from2013 = 'D29' AND avt.site_icd10r4_o2_from2013 NOT IN ('D292') THEN 'D29OTHER'
WHEN avt.site_icd10r4_o2_from2013 IN ('C751','C752','C753') THEN  'C75BRAIN'
WHEN avt.site_icd10r4_o2_3char_from2013 = 'C75' AND avt.site_icd10r4_o2_from2013 NOT IN ('C751','C752','C753') THEN 'C75OTHER'
WHEN avt.site_icd10r4_o2_from2013 IN ('D320','D321','D329') THEN 'D32BRAIN'                                                     
WHEN avt.site_icd10r4_o2_from2013 IN ('D330','D331','D332','D333','D334','D337','D339') THEN 'D33BRAIN'                                                             
WHEN avt.site_icd10r4_o2_from2013 IN ('D352','D353','D354') THEN 'D35BRAIN'                                                     
WHEN avt.site_icd10r4_o2_from2013 IN ('D420','D421','D429') THEN 'D42BRAIN'                                                     
WHEN avt.site_icd10r4_o2_from2013 IN ('D430','D431','D432','D433','D434','D437','D439') THEN 'D43BRAIN'                                                             
WHEN avt.site_icd10r4_o2_from2013 IN ('D443','D444','D445') THEN 'D44BRAIN' 
WHEN avt.site_icd10r4_o2_from2013 IN ('D414') THEN 'D41BLADDER'                                                             
WHEN avt.site_icd10r4_o2_from2013 IN ('D090') THEN 'D09BLADDER' 
WHEN avt.site_icd10r4_o2_from2013 IN ('C220','C222','C223','C224','C227','C229') THEN 'LIVER' --Liver excluding intrahepatic cholangiocarcinoma
WHEN avt.site_icd10r4_o2_from2013 IN ('C221','C240','C248','C249') THEN 'CHOLANGIOCARCINOMA' --Cholangiocarcinoma
WHEN avt.site_icd10r4_o2_from2013 IN ('C241') THEN 'AOV' --Ampulla of Vater
--haem subsites
WHEN haem.split_2 = 'Acute lymphoblastic leukaemia (ALL)' then 'ALL'
WHEN haem.split_2 = 'Acute myeloid leukaemia (AML)' then 'AML'
WHEN haem.split_2 = 'Hodgkin lymphoma' then 'HODGKIN'
WHEN haem.split_3 = 'Chronic lymphocytic leukaemia (CLL) or small lymphocytic lymphoma (SLL)' then 'CLLSLL'
WHEN haem.split_3 = 'Diffuse large B-cell lymphoma (DLBCL) and other high grade mature B-cell neoplasms' then 'DLBCL'
WHEN haem.split_3 = 'Follicular lymphoma' then 'FOLLICULAR'
WHEN haem.split_3 = 'Mantle cell lymphoma (MCL)' then 'MCL'
WHEN haem.split_3 = 'Chronic myeloid leukaemia (CML)' then 'CML'
WHEN haem.split_3 = 'Myeloma' then 'MYELOMA'
WHEN haem.split_1 is not null then 'OTHER_HAEM'

ELSE avt.site_icd10r4_o2_3char_from2013

END AS tumour_code

FROM av2021.at_tumour_england@casref01 AVT 
left join gdo.morph_haem haem
--- join the lookup on morph/behaviour in ICD-O-3 Rev 2011
--- These fields only complete for 2013 onwards
on haem.morph_icdo3rev2011 = AVT.morph_icdo3rev2011
and haem.behaviour = AVT.behaviour_icdo3rev2011


--Define cohort of interest here
WHERE avt.diagnosisyear between 2013 and 2021 
AND (substr(avt.site_icd10r4_o2_3char_from2013,1,1) IN ('C','D') OR haem.split_1 is not null)--Restrict to C and D codes except for haem (E85, L99)
AND avt.site_icd10r4_o2_3char_from2013 NOT IN ('D01','D03','D04','D06','D07','D11','D13','D15','D16','D18','D25','D27','D36','D40','D48','C44') 
AND avt.ctry_code = 'E'
AND avt.statusofregistration = 'F'
AND avt.dedup_flag = '1'
AND avt.age BETWEEN 0 AND 200 
AND avt.gender IN (1,2)

AND ((gender = '2' and site_icd10r4_o2_3char_from2013 not in ('C60','C61','C62','C63')) 
OR (gender = '1' and  site_icd10r4_o2_3char_from2013 not in ('C51','C52','C53','C54','C55','C56','C57','C58'))) -- gender doesn't agree with tumour site 

--- Remove haem transformations
and not exists (
select 
ath.transformed_tumourid
from analysispollyjeffrey.at_transformation_haem@cas2210 ath
where transformation_year between 2013 and 2021
and AVT.tumourid=ath.transformed_tumourid
)
),

--Remove any tumours from the all tumours cohort that also appear in the skin cohort to avoid duplication
non_skin_cohort AS
(SELECT nsk.tumourid, nsk.patientid, nsk.nhsnumber, nsk.diagnosisdatebest, nsk.site_icd10r4_o2_from2013, nsk.figo, nsk.gender, nsk.ethnicity, nsk.morph_icd10_o2, nsk.fiveyearageband, nsk.age, nsk.tumour_code
FROM non_skin nsk
LEFT JOIN skin_cohort skn ON nsk.tumourid=skn.tumourid
WHERE skn.tumourid IS NULL),

--Now union together the skin and non-skin cancer cohorts to create the full cohort 
tumour_cohort AS
(SELECT tumourid, patientid, nhsnumber, diagnosisdatebest, site_icd10r4_o2_from2013, figo, gender, ethnicity, morph_icd10_o2, fiveyearageband, age, tumour_code
FROM skin_cohort

UNION

SELECT tumourid, patientid, nhsnumber, diagnosisdatebest, site_icd10r4_o2_from2013, figo, gender, ethnicity, morph_icd10_o2, fiveyearageband, age, tumour_code
FROM non_skin_cohort)

--Identify patients with multiple tumours within an 18-month period with tumour_flag
SELECT tumourid, patientid, nhsnumber, diagnosisdatebest, site_icd10r4_o2_from2013, figo, gender, ethnicity, morph_icd10_o2, fiveyearageband, age, tumour_code, tumour_flag
FROM 
(SELECT avt.tumourid, avt.patientid, avt.nhsnumber, avt.diagnosisdatebest, avt.site_icd10r4_o2_from2013, avt.figo, avt.gender, avt.ethnicity, avt.morph_icd10_o2, avt.fiveyearageband, avt.age, avt.tumour_code

-- This join flags any tumours diagnosed in 2013-21 that belong to a patient who had another tumour in the 18 months before or after that diagnosis 
--(so that later, patient level datasets (hes, sact, rtds) are only used for patients with 1 tumour) 
-- Tumour_flag = 1; the tumour belonged to a patient who had another tumour within 18 months 

,CASE WHEN ABS(avt.diagnosisdatebest-avt2.diagnosisdatebest)<548 THEN 1 ELSE 0 END AS tumour_flag 

-- In the process of joining AVT2 to AVT to identify multiple tumours, duplicate rows are generated 
-- The difference between diagnosis date for tumours in AVT AND AVT2 ranks multiple tumours where more than one exists AND drops all but the closest tumour to the original tumour. 
-- Where rk = 1; this is the tumour record to keep 

,RANK() OVER (PARTITION BY avt.tumourid ORDER BY ABS(avt.diagnosisdatebest-avt2.diagnosisdatebest) ASC, avt2.tumourid) AS rk 
FROM tumour_cohort AVT

-- Multiple tumours join: 
-- For tumours diagnosed from 2013-2021, identify any other tumour IDs that occurred between 2011-2023 
-- A second copy of the tumour cohort (AVT2) is joined to the original tumour cohort of 2013-21 diagnoses (TC) 
-- Records from AVT2 are only joined if the patient ID is the same but the tumour ID is different 

LEFT JOIN av2021.at_tumour_england@casref01 AVT2 ON avt.patientid=avt2.patientid 
AND NOT(avt.tumourid=avt2.tumourid) 
--AND avt2.cascade_inci_flag = 1 
AND ((avt2.site_icd10r4_o2_3char_from2013 NOT IN ('D01','D03','D04','D06','D07','D11','D13','D15','D16','D18','D25','D27','D36','D40','D48','C44')    
AND avt2.diagnosisyear BETWEEN 2013 AND 2023)
or (avt2.site_icd10_o2_3char_pre2013 NOT IN ('D01','D03','D04','D06','D07','D11','D13','D15','D16','D18','D25','D27','D36','D40','D48','C44')    
AND avt2.diagnosisyear between 2011 AND 2012))

--Removes duplicate tumour rows that had been added to identify patients with multiple tumours
)WHERE rk=1;

--Create table indexes for tumour cohort table

CREATE UNIQUE INDEX analysisnataliapetersen.tr_tumcohort_tumourid_uq ON analysisnataliapetersen.tr_tumour_cohort ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE INDEX analysisnataliapetersen.tr_tumcohort_patientid_ix ON analysisnataliapetersen.tr_tumour_cohort ( patientid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE INDEX analysisnataliapetersen.tr_tumcohort_nhsnumber_ix ON analysisnataliapetersen.tr_tumour_cohort ( nhsnumber ) NOLOGGING TABLESPACE analysisdata_IX; 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_tumour_cohort') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_tumcohort_tumourid_uq') 



------------------------------------------------------------------------------ 
--------------------CREATE SURGERY FLAG TABLES - ALL SITES------------- 
------------------------------------------------------------------------------ 
--1)---------------- ALL SITES - SURGERY FROM AT_TREATMENT_ENGLAND ------------------ 
-- Create a surgery flag for the tumour if: 
-- there is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with surgery (event is '01a', '01b', '01z', or '01c') 
-- and the opcs4_code is in the tumour resection list 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 

CREATE TABLE tr_av_sg AS( 
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS avsg_flag 
, eventdate AS avsg_date
, avsg_trust_code
FROM ( 
SELECT tumourid, datediff, rk , eventdate, avsg_trust_code
FROM ( 
SELECT tc.tumourid, 
(avtreat.eventdate-tc.diagnosisdatebest) AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk 
, avtreat.eventdate
, avtreat.trust_code AS avsg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN av2021.at_treatment_england@casref01 avtreat ON avtreat.tumourid=tc.tumourid 
AND eventcode IN ('01a','01b','01z', '01c') AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time) 
INNER JOIN analysisnataliapetersen.opcs4resection_lookup_13_21@casref01 opcs ON opcs.tumouricdsite3code = tc.tumour_code AND TRIM(opcs.opcsresectioncode) = avtreat.opcs4_code 
)
WHERE rk=1
));

--2)--------------- ALL SITES - SURGERY FROM HES ------------------ 
-- Create a surgery flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a tumour resection opcs-4 code in one of the operation fields 
-- and the operation date (opertn) occurred in the relevant timeframe create table 
-- and the patient only had one tumour in the time period of interest (this is also incorporated in the final table)
CREATE TABLE tr_hes_sg AS( 
SELECT DISTINCT tumourid, hessg_flag, hessg_date, hessg_trust_code
FROM (
select tumourid, hessg_flag, hessg_date, hessg_trust_code, RANK() OVER (PARTITION BY tumourid ORDER BY hessg_date, source) as rk
FROM (
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS hessg_flag 
, opdate AS hessg_date 
, hessg_trust_code 
, 'HESAPC' as source
FROM ( 
SELECT tumourid, datediff, rk , opdate, hessg_trust_code 
FROM ( 
SELECT tc.tumourid, 
ho.opdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear,hl.epikeyanon,POS) AS rk 
, ho.opdate 
, procode3 AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_apc@casref01 hl ON tc.patientid = hl.patientid 
INNER JOIN heslive.hesapc_opertn@casref01 ho ON ho.datayear = hl.datayear AND ho.epikeyanon = hl.epikeyanon 
AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
INNER JOIN heslive.hesapc@casref01 ha ON ha.datayear = hl.datayear AND ha.epikeyanon = hl.epikeyanon 
INNER JOIN analysisnataliapetersen.opcs4resection_lookup_13_21@casref01 opcs ON opcs.tumouricdsite3code = tc.tumour_code AND TRIM(opcs.opcsresectioncode) = ho.opertn 
) 
WHERE rk=1)

UNION ALL

SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS hessg_flag 
, apptdate AS hessg_date 
, hessg_trust_code 
, 'HESOP' as source
FROM ( 
SELECT tumourid, datediff, rk , apptdate, hessg_trust_code 
FROM ( 
SELECT tc.tumourid, 
op.apptdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear,h2.attendkeyanon,POS) AS rk 
, op.apptdate 
, procodet AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_op@casref01 h2 ON tc.patientid = h2.patientid
INNER JOIN heslive.hesop_opertn@casref01 ho2 ON ho2.datayear = h2.datayear AND ho2.attendkeyanon = h2.attendkeyanon
INNER JOIN heslive.hesop@casref01 op ON op.datayear = h2.datayear and op.attendkeyanon = h2.attendkeyanon 
INNER JOIN analysisnataliapetersen.opcs4resection_lookup_13_21@casref01 opcs ON opcs.tumouricdsite3code = tc.tumour_code
where op.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND TRIM(opcs.opcsresectioncode) = ho2.opertn
) 
WHERE rk=1)))
WHERE rk=1)
; 

------------------------------------------------------------------------------ 
-----------CREATE SURGERY FLAG TABLES - STAGE SPECIFIC RESECTIONS------------ 
------------------------------------------------------------------------------ 
--3)---------------- LIVER excluding intrahepatic cholangiocarcinoma - AT_TREATMENT_ENGLAND ------------------ 
-- Create a surgery flag for the tumour if: 
-- there is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with surgery (event is '01a', '01b', '01z', or '01c') 
-- and the opcs4_code is a percutaneous radiofrequency, thermal, chemical AND microwave ablation of lesion of liver (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is TNM stage 1 (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 

CREATE TABLE tr_av_liver as ( 
SELECT DISTINCT 
tumourid,
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS liver_avtreat 
, eventdate AS avsg_date 
, avsg_trust_code
FROM ( 
SELECT tumourid, datediff, rk, eventdate, avsg_trust_code 
FROM ( 
SELECT tc.tumourid, 
avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, avtreat.eventdate 
, avtreat.trust_code AS avsg_trust_code 
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN av2021.at_treatment_england@casref01 avtreat ON avtreat.tumourid=tc.tumourid 
AND eventcode IN ('01a','01b','01z','01c') AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time) 
AND avtreat.opcs4_code IN ('J124','J125','J126','J127') AND tc.tumour_code IN ('LIVER')) 
WHERE rk=1));


--4)---------------- LIVER excluding intrahepatic cholangiocarcinoma - HES------------------ 
-- Create a surgery flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a tumour resection opcs-4 code in one of the operation fields 
-- and the opcs4_code is a percutaneous radiofrequency, thermal, chemical AND microwave ablation of lesion of liver (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is TNM stage 1 (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 
-- and the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 

CREATE TABLE tr_hes_liver AS( 
SELECT DISTINCT tumourid, liver_hes, hessg_date, hessg_trust_code
FROM (
select tumourid, liver_hes, hessg_date, hessg_trust_code, RANK() OVER (PARTITION BY tumourid ORDER BY hessg_date, source) as rk
FROM (
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS liver_hes 
, opdate AS hessg_date 
, hessg_trust_code
, 'HESAPC' as source
FROM ( 
SELECT tumourid, datediff, rk, opdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
ho.opdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear,hl.epikeyanon,pos) AS rk 
, ho.opdate 
, procode3 AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_apc@casref01 hl ON tc.patientid = hl.patientid
INNER JOIN heslive.hesapc@casref01 ha ON ha.datayear = hl.datayear AND ha.epikeyanon = hl.epikeyanon 
INNER JOIN heslive.hesapc_opertn@casref01 ho ON ho.datayear = hl.datayear AND ho.epikeyanon = hl.epikeyanon 
AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho.opertn IN ('J124','J125','J126','J127') AND tc.tumour_code in ('LIVER')) 
WHERE rk=1)

UNION ALL

SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS liver_hes 
, apptdate AS hessg_date 
, hessg_trust_code
, 'HESOP' as source
FROM ( 
SELECT tumourid, datediff, rk, apptdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
op.apptdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear,h2.attendkeyanon,pos) AS rk 
, op.apptdate 
, procodet AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_op@casref01 h2 ON tc.patientid = h2.patientid
INNER JOIN heslive.hesop@casref01 op ON op.datayear = h2.datayear and op.attendkeyanon = h2.attendkeyanon 
INNER JOIN heslive.hesop_opertn@casref01 ho2 ON ho2.datayear = h2.datayear AND ho2.attendkeyanon = h2.attendkeyanon
where op.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho2.opertn IN ('J124','J125','J126','J127') AND tc.tumour_code in ('LIVER')) 
WHERE rk=1)))
WHERE rk=1); 

-------------------------------------------------------------------------------- 
--5)---------------- OESOPHAGUS C15 - AT_TREATMENT_ENGLAND ------------------ 
-- Create a surgery flag for the tumour if:
-- there is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with surgery (event is '01a', '01b', '01z', or '01c') 
-- and the opcs4_code is a fibreoptic endoscopic resection of lesions of upper gastrointestinal tract AND oesophagus (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is TNM stage 1a (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 

CREATE TABLE tr_av_oesoph AS( 
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS oesoph_avtreat 
, eventdate AS avsg_date 
, avsg_trust_code
FROM ( 
SELECT tumourid, datediff, rk, eventdate, avsg_trust_code 
FROM ( 
SELECT tc.tumourid, 
avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, avtreat.eventdate 
, avtreat.trust_code AS avsg_trust_code 
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN av2021.at_treatment_england@casref01 avtreat ON avtreat.tumourid=tc.tumourid 
AND eventcode IN ('01a','01b','01z','01c') AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time) 
AND avtreat.opcs4_code IN ('G421','G431','G146','G171','G438') AND tc.tumour_code IN ('C15')) 
WHERE rk=1));


--6)---------------- OESOPHAGUS C15 - HES ------------------ 
-- Create a surgery flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a tumour resection opcs-4 code in one of the operation fields 
-- and the opcs4_code is a fibreoptic endoscopic resection of lesions of upper gastrointestinal tract AND oesophagus (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is TNM stage 1a (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 
-- and the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 

CREATE TABLE tr_hes_oesoph AS( 
SELECT DISTINCT tumourid, oesoph_hes, hessg_date, hessg_trust_code
FROM (
select tumourid, oesoph_hes, hessg_date, hessg_trust_code, RANK() OVER (PARTITION BY tumourid ORDER BY hessg_date, source) as rk
FROM (
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS oesoph_hes 
, opdate AS hessg_date 
, hessg_trust_code
, 'HESAPC' as source
FROM ( 
SELECT tumourid, datediff, rk, opdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
ho.opdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear,hl.epikeyanon,POS) AS rk 
, ho.opdate 
, procode3 AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_apc@casref01 hl ON tc.patientid = hl.patientid 
INNER JOIN heslive.hesapc@casref01 ha ON ha.datayear = hl.datayear AND ha.epikeyanon = hl.epikeyanon 
INNER JOIN heslive.hesapc_opertn@casref01 ho ON ho.datayear = hl.datayear AND ho.epikeyanon = hl.epikeyanon 
AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho.opertn IN ('G421','G431','G146','G171','G438') AND tc.tumour_code IN ('C15')) 
WHERE rk=1)

UNION ALL

SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS oesoph_hes 
, apptdate AS hessg_date 
, hessg_trust_code
, 'HESOP' as source
FROM ( 
SELECT tumourid, datediff, rk, apptdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
op.apptdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear,h2.attendkeyanon,pos) AS rk 
, op.apptdate 
, procodet AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_op@casref01 h2 ON tc.patientid = h2.patientid
INNER JOIN heslive.hesop@casref01 op ON op.datayear = h2.datayear and op.attendkeyanon = h2.attendkeyanon 
INNER JOIN heslive.hesop_opertn@casref01 ho2 ON ho2.datayear = h2.datayear AND ho2.attendkeyanon = h2.attendkeyanon
where op.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho2.opertn IN ('G421','G431','G146','G171','G438') AND tc.tumour_code IN ('C15')) 
WHERE rk=1)))
WHERE rk=1);


-------------------------------------------------------------------------------- 
--7)---------------- STOMACH C16 - AT_TREATMENT_ENGLAND ------------------ 
-- Create a surgery flag for the tumour if: 
-- there is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with surgery (event is '01a', '01b', '01z', or '01c') 
-- and the opcs4_code is a fibreoptic endoscopic resection of lesions of upper gastrointestinal tract AND oesophagus (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is TNM stage 1a (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 

CREATE TABLE tr_av_stomach AS( 
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS stomach_avtreat 
, eventdate AS avsg_date 
, avsg_trust_code
FROM ( 
SELECT tumourid, datediff, rk, eventdate, avsg_trust_code 
FROM ( 
SELECT tc.tumourid, 
avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, avtreat.eventdate 
, avtreat.trust_code AS avsg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN av2021.at_treatment_england@casref01 avtreat ON avtreat.tumourid=tc.tumourid 
AND eventcode IN ('01a','01b','01z','01c') AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time) 
AND avtreat.opcs4_code IN ('G421','G146','G449') AND tc.tumour_code IN ('C16')) 
WHERE rk=1)); 


--8)---------------- STOMACH C16 - HES ------------------ 
-- Create a surgery flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a tumour resection opcs-4 code in one of the operation fields 
-- and the opcs4_code is a fibreoptic endoscopic resection of lesions of upper gastrointestinal tract AND oesophagus (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is TNM stage 1a (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 
-- and the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 

CREATE TABLE tr_hes_stomach AS( 
SELECT DISTINCT tumourid, stomach_hes, hessg_date, hessg_trust_code
FROM (
select tumourid, stomach_hes, hessg_date, hessg_trust_code, RANK() OVER (PARTITION BY tumourid ORDER BY hessg_date, source) as rk
FROM (
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS stomach_hes 
, opdate AS hessg_date 
, hessg_trust_code
, 'HESAPC' as source
FROM ( 
SELECT tumourid, datediff, rk, opdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
ho.opdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear,hl.epikeyanon,POS) AS rk 
, ho.opdate 
, procode3 AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_apc@casref01 hl ON tc.patientid = hl.patientid 
INNER JOIN heslive.hesapc@casref01 ha ON ha.datayear = hl.datayear AND ha.epikeyanon = hl.epikeyanon 
INNER JOIN heslive.hesapc_opertn@casref01 ho ON ho.datayear = hl.datayear AND ho.epikeyanon = hl.epikeyanon 
AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho.opertn IN ('G421','G146','G449') AND tc.tumour_code IN ('C16')) 
WHERE rk=1)

UNION ALL

SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS stomach_hes 
, apptdate AS hessg_date 
, hessg_trust_code
, 'HESOP' as source
FROM ( 
SELECT tumourid, datediff, rk, apptdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
op.apptdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear,h2.attendkeyanon,pos) AS rk 
, op.apptdate 
, procodet AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_op@casref01 h2 ON tc.patientid = h2.patientid
INNER JOIN heslive.hesop@casref01 op ON op.datayear = h2.datayear and op.attendkeyanon = h2.attendkeyanon 
INNER JOIN heslive.hesop_opertn@casref01 ho2 ON ho2.datayear = h2.datayear AND ho2.attendkeyanon = h2.attendkeyanon
where op.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho2.opertn IN ('G421','G146','G449') AND tc.tumour_code IN ('C16')) 
WHERE rk=1)))
WHERE rk=1);

-------------------------------------------------------------------------------- 
--9)---------------- BLADDER CANCERS (C67) - AT_TREATMENT_ENGLAND-------------- 
-- Create a surgery flag for the tumour if: 
-- there is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with surgery (event is '01a', '01b', '01z', or '01c') 
-- and the opcs4_code is a endoscopic resections of lesion of bladder (TURBT) (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is T1 (non-muscle invasive) (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 

CREATE TABLE tr_av_bladder AS( 
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS bladder_avtreat 
, eventdate AS avsg_date 
, avsg_trust_code
FROM ( 
SELECT tumourid, datediff, rk, eventdate, avsg_trust_code
FROM ( 
SELECT tc.tumourid, 
avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, avtreat.eventdate 
, avtreat.trust_code AS avsg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN av2021.at_treatment_england@casref01 avtreat ON avtreat.tumourid=tc.tumourid 
AND eventcode IN ('01a','01b','01z','01c') AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time)  
AND avtreat.opcs4_code IN ('M421', 'M422', 'M423', 'M428', 'M429') AND tc.tumour_code IN ('C67','D09BLADDER')) 
WHERE rk=1)); 

--10)---------------- BLADDER CANCERS (C67) - HES ------------------------
-- Create a surgery flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a tumour resection opcs-4 code in one of the operation fields 
-- and the opcs4_code is an endoscopic resections of lesion of bladder (TURBT) (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is T1 (non-muscle invasive) (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 
-- and the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 

CREATE TABLE tr_hes_bladder AS( 
SELECT DISTINCT tumourid, bladder_hes, hessg_date, hessg_trust_code
FROM (
select tumourid, bladder_hes, hessg_date, hessg_trust_code, RANK() OVER (PARTITION BY tumourid ORDER BY hessg_date, source) as rk
FROM (
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS bladder_hes 
, opdate AS hessg_date 
, hessg_trust_code
, 'HESAPC' as source
FROM ( 
SELECT tumourid, datediff, rk, opdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
ho.opdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear,hl.epikeyanon,POS) AS rk 
, ho.opdate 
, procode3 AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_apc@casref01 hl ON tc.patientid = hl.patientid 
INNER JOIN heslive.hesapc@casref01 ha ON ha.datayear = hl.datayear AND ha.epikeyanon = hl.epikeyanon 
INNER JOIN heslive.hesapc_opertn@casref01 ho ON ho.datayear = hl.datayear AND ho.epikeyanon = hl.epikeyanon 
AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho.opertn IN ('M421', 'M422', 'M423', 'M428', 'M429') AND tc.tumour_code IN ('C67','D09BLADDER')) 
WHERE rk=1)

UNION ALL

SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS bladder_hes 
, apptdate AS hessg_date 
, hessg_trust_code
, 'HESOP' as source
FROM ( 
SELECT tumourid, datediff, rk, apptdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
op.apptdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear,h2.attendkeyanon,pos) AS rk 
, op.apptdate 
, procodet AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_op@casref01 h2 ON tc.patientid = h2.patientid
INNER JOIN heslive.hesop@casref01 op ON op.datayear = h2.datayear and op.attendkeyanon = h2.attendkeyanon 
INNER JOIN heslive.hesop_opertn@casref01 ho2 ON ho2.datayear = h2.datayear AND ho2.attendkeyanon = h2.attendkeyanon
where op.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho2.opertn IN ('M421', 'M422', 'M423', 'M428', 'M429') AND tc.tumour_code IN ('C67','D09BLADDER')) 
WHERE rk=1)))
WHERE rk=1);


-------------------------------------------------------------------------------- 
--11)---------------- CERVICAL CANCERS; CONE BIOPSIES - AT_TREATMENT_ENGLAND ------------------ 
--The final treatment table will create a surgery flag for the tumour if: 
--The tumour received a cone biopsy and was FIGO stage 1a (see SOP Appendices for list of opcs4 codes) 
--Or the tumour received a cone biopsy and was FIGO stage 1b & 1b1 disease, if the tumour also received a lymphadenectomy 
--Tables 11-14 flag the cone biopsies and lymphadenectomies, AND a cervical tumour resection flag will bring this together in the final table 
-- Create a cone biopsy flag for the tumour if: 
-- there is a record in at_treatment_england which states that the tumour was treated with surgery (event is '01a', '01b', '01z', or '01c') 
-- and the opcs4_code is a cone biopsy 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 

CREATE TABLE tr_av_conebiops AS( 
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS conebiops_avtreat
, eventdate AS avsg_date 
, avsg_trust_code
FROM ( 
SELECT tumourid, datediff, rk, eventdate, avsg_trust_code
FROM ( 
SELECT tc.tumourid, 
avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, avtreat.eventdate 
, avtreat.trust_code AS avsg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN av2021.at_treatment_england@casref01 avtreat ON avtreat.tumourid=tc.tumourid 
AND eventcode IN ('01a','01b','01z','01c') AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time) 
AND avtreat.opcs4_code IN ('Q014','Q033','Q031','Q032') AND tc.tumour_code='C53') 
WHERE rk=1));


--12)---------------- CERVICAL CANCERS; CONE BIOPSIES - HES ------------------ 
-- Create a cone biopsy flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a tumour resection opcs-4 code in one of the operation fields 
-- and the opcs4_code is a cone biopsy (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the patient only had one tumour in the time period of interest (this is incorporated in the final table) 

CREATE TABLE tr_hes_conebiops AS( 
SELECT DISTINCT tumourid, conebiops_hes, hessg_date, hessg_trust_code
FROM (
select tumourid, conebiops_hes, hessg_date, hessg_trust_code, RANK() OVER (PARTITION BY tumourid ORDER BY hessg_date, source) as rk
FROM (
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS conebiops_hes 
, opdate AS hessg_date 
, hessg_trust_code
, 'HESAPC' as source
FROM ( 
SELECT tumourid, datediff, rk, opdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
ho.opdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear,hl.epikeyanon,POS) AS rk 
, ho.opdate 
, procode3 AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_apc@casref01 hl ON tc.patientid = hl.patientid 
INNER JOIN heslive.hesapc@casref01 ha ON ha.datayear = hl.datayear AND ha.epikeyanon = hl.epikeyanon 
INNER JOIN heslive.hesapc_opertn@casref01 ho ON ho.datayear = hl.datayear AND ho.epikeyanon = hl.epikeyanon 
AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho.opertn IN ('Q014','Q033','Q031','Q032') AND tc.tumour_code='C53') 
WHERE rk=1)

UNION ALL

SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS conebiops_hes 
, apptdate AS hessg_date 
, hessg_trust_code
, 'HESOP' as source
FROM ( 
SELECT tumourid, datediff, rk, apptdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
op.apptdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear,h2.attendkeyanon,pos) AS rk 
, op.apptdate 
, procodet AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_op@casref01 h2 ON tc.patientid = h2.patientid
INNER JOIN heslive.hesop@casref01 op ON op.datayear = h2.datayear and op.attendkeyanon = h2.attendkeyanon 
INNER JOIN heslive.hesop_opertn@casref01 ho2 ON ho2.datayear = h2.datayear AND ho2.attendkeyanon = h2.attendkeyanon
where op.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho2.opertn IN ('Q014','Q033','Q031','Q032') AND tc.tumour_code='C53')  
WHERE rk=1)))
WHERE rk=1);


--13)---------------- CERVICAL CANCERS; LYMPHADENECTOMIES - AT_TREATMENT_ENGLAND ------------------ 
-- Create a lymphadenectomy flag for the tumour if: 
-- there is a record in at_treatment_england which states that the tumour was treated with surgery (event is '01a', '01b', '01z', or '01c') 
-- and the opcs4_code is a lymphadenectomy (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 

CREATE TABLE tr_av_lymph AS( 
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS lymph_avtreat 
, eventdate AS avsg_date 
, avsg_trust_code
FROM ( 
SELECT tumourid, datediff, rk, eventdate, avsg_trust_code
FROM ( 
SELECT tc.tumourid, 
avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, avtreat.eventdate 
, avtreat.trust_code AS avsg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN av2021.at_treatment_england@casref01 avtreat ON avtreat.tumourid=tc.tumourid 
AND eventcode IN ('01a','01b','01z','01c') AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time) 
AND avtreat.opcs4_code IN ('T856','T859','T865') AND tc.tumour_code='C53') 
WHERE rk=1));


--14)---------------- CERVICAL CANCERS; LYMPHADENECTOMIES - HES ------------------ 
-- Create a lymphadenectomy flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a tumour resection opcs-4 code in one of the operation fields 
-- And the opcs4_code is a lymphadenectomy (see SOP Appendices for list of opcs4 codes) 
-- And the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- And the patient only had one tumour in the time period of interest (this is incorporated in the final table) 
CREATE TABLE tr_hes_lymph AS( 
SELECT DISTINCT tumourid, lymph_hes, hessg_date, hessg_trust_code
FROM (
select tumourid, lymph_hes, hessg_date, hessg_trust_code, RANK() OVER (PARTITION BY tumourid ORDER BY hessg_date, source) as rk
FROM (
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS lymph_hes 
, opdate AS hessg_date 
, hessg_trust_code
, 'HESAPC' as source
FROM ( 
SELECT tumourid, datediff, rk, opdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
ho.opdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear,hl.epikeyanon,pos) AS rk 
, ho.opdate 
, procode3 AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_apc@casref01 hl ON tc.patientid = hl.patientid 
INNER JOIN heslive.hesapc@casref01 ha ON ha.datayear = hl.datayear AND ha.epikeyanon = hl.epikeyanon 
INNER JOIN heslive.hesapc_opertn@casref01 ho ON ho.datayear = hl.datayear AND ho.epikeyanon = hl.epikeyanon 
AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho.opertn IN ('T856','T859','T865') AND tc.tumour_code='C53') 
WHERE rk=1)

UNION ALL

SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS lymph_hes 
, apptdate AS hessg_date 
, hessg_trust_code
, 'HESOP' as source
FROM ( 
SELECT tumourid, datediff, rk, apptdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
op.apptdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear,h2.attendkeyanon,pos) AS rk 
, op.apptdate 
, procodet AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_op@casref01 h2 ON tc.patientid = h2.patientid
INNER JOIN heslive.hesop@casref01 op ON op.datayear = h2.datayear and op.attendkeyanon = h2.attendkeyanon 
INNER JOIN heslive.hesop_opertn@casref01 ho2 ON ho2.datayear = h2.datayear AND ho2.attendkeyanon = h2.attendkeyanon
where op.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho2.opertn IN ('T856','T859','T865') AND tc.tumour_code='C53') 
WHERE rk=1)))
WHERE rk=1);


-------------------------------------------------------------------------------- 
--15)---------------- COLORECTAL CANCERS; ENDOSCOPIES - AT_TREATMENT_ENGLAND--------- 
-- Create a surgery flag for the tumour if: 
-- there is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with surgery (event is '01a', '01b', '01z', or '01c') 
-- and the opcs4_code is an endoscopic resection or endoscopic biopsy procedure (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is TNM stage 1 (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 


CREATE TABLE tr_av_colorec AS( 
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS colorec_avtreat 
, eventdate AS avsg_date 
, avsg_trust_code
FROM ( 
SELECT tumourid, datediff, rk, eventdate, avsg_trust_code 
FROM ( 
SELECT tc.tumourid,
avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, avtreat.eventdate 
, avtreat.trust_code AS avsg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN av2021.at_treatment_england@casref01 avtreat ON avtreat.tumourid=tc.tumourid 
AND eventcode IN ('01a','01b','01z','01c') AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time) 
AND avtreat.opcs4_code IN ('H201','H412','H206','H231','H236','H205','H202','H122','H235','H239','H402','H232','H261','H208','H341','H418', 
'H209','H248','H238','H204','H419','H221','H251','H259','H229','H181','H281','H191','H561') 
AND tc.tumour_code in ('C18', 'C19', 'C20')) 
WHERE rk=1)); 


--16)---------------- COLORECTAL CANCERS; ENDOSCOPIES - HES ------------------ 
-- Create a surgery flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a tumour resection opcs-4 code in one of the operation fields 
-- and the opcs4_code is an endoscopic resection or endoscopic biopsy procedure (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is TNM stage 1 (a stage-specific tumour resection flag will incorporate this stage criteria in the final table) 
-- and the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 

CREATE TABLE tr_hes_colorec AS( 
SELECT DISTINCT tumourid, colorec_hes, hessg_date, hessg_trust_code
FROM (
select tumourid, colorec_hes, hessg_date, hessg_trust_code, RANK() OVER (PARTITION BY tumourid ORDER BY hessg_date, source) as rk
FROM (
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS colorec_hes 
, opdate AS hessg_date 
, hessg_trust_code
, 'HESAPC' as source
FROM ( 
SELECT tumourid, datediff, rk, opdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
ho.opdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear,hl.epikeyanon,POS) AS rk 
, ho.opdate 
, procode3 AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_apc@casref01 hl ON tc.patientid = hl.patientid 
INNER JOIN heslive.hesapc@casref01 ha ON ha.datayear = hl.datayear AND ha.epikeyanon = hl.epikeyanon 
INNER JOIN heslive.hesapc_opertn@casref01 ho ON ho.datayear = hl.datayear AND ho.epikeyanon = hl.epikeyanon 
AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho.opertn IN ('H201','H412','H206','H231','H236','H205','H202','H122','H235','H239','H402','H232', 'H261','H208','H341', 
'H418','H209','H248','H238','H204','H419','H221','H251','H259','H229','H181','H281','H191','H561') 
AND tc.tumour_code in ('C18', 'C19', 'C20')) 
WHERE rk=1)

UNION ALL

SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS colorec_hes 
, apptdate AS hessg_date 
, hessg_trust_code
, 'HESOP' as source
FROM ( 
SELECT tumourid, datediff, rk, apptdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
op.apptdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear,h2.attendkeyanon,pos) AS rk 
, op.apptdate 
, procodet AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_op@casref01 h2 ON tc.patientid = h2.patientid
INNER JOIN heslive.hesop@casref01 op ON op.datayear = h2.datayear and op.attendkeyanon = h2.attendkeyanon 
INNER JOIN heslive.hesop_opertn@casref01 ho2 ON ho2.datayear = h2.datayear AND ho2.attendkeyanon = h2.attendkeyanon
where op.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho2.opertn IN ('H201','H412','H206','H231','H236','H205','H202','H122','H235','H239','H402','H232', 'H261','H208','H341', 
'H418','H209','H248','H238','H204','H419','H221','H251','H259','H229','H181','H281','H191','H561') 
AND tc.tumour_code in ('C18', 'C19', 'C20')) 
WHERE rk=1)))
WHERE rk=1);


--17)---------------- COLORECTAL CANCERS; APPENDECTOMIES FOR APPENDIX TUMOURS ONLY C18.1 - AT_TREATMENT_ENGLAND ------------------ 
-- Create a surgery flag for the tumour if: 
-- there is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with surgery (event is '01a', '01b', '01z', or '01c') 
-- And the opcs4_code is an appendectomy procedure (see SOP Appendices for list of opcs4 codes) 
-- And the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- And the tumour is an appendix tumour (C18.1) 

CREATE TABLE tr_av_coloappen AS 
(SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS colorec_avtreat_appen 
, eventdate AS avsg_date 
, avsg_trust_code
FROM ( 
SELECT tumourid, datediff, rk, eventdate, avsg_trust_code
FROM ( 
SELECT tc.tumourid, 
avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk, avtreat.eventdate 
, avtreat.trust_code AS avsg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN av2021.at_treatment_england@casref01 avtreat ON avtreat.tumourid=tc.tumourid 
AND eventcode IN ('01a','01b','01z','01c') AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time) 
AND avtreat.opcs4_code IN ('H024','H019','H011') AND tc.site_icd10r4_o2_from2013 in ('C181')) 
WHERE rk=1)); 


--18)---------------- COLORECTAL CANCERS; APPENDECTOMIES FOR APPENDIX TUMOURS ONLY C18.1 - HES ------------------ 
-- Create a surgery flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a tumour resection opcs-4 code in one of the operation fields 
-- and the opcs4_code is an appendectomy procedure (see SOP Appendices for list of opcs4 codes) 
-- and the operation date (opertn) occurred in the relevant timeframe (see SOP) 
-- and the tumour is an appendix tumour (C18.1) 
-- and the patient only had one tumour in the time period of interest (this is also incorporated in the final table)

CREATE TABLE 
tr_hes_coloappen AS ( 
SELECT DISTINCT tumourid, colorec_hes_appen, hessg_date, hessg_trust_code
FROM (
select tumourid, colorec_hes_appen, hessg_date, hessg_trust_code, RANK() OVER (PARTITION BY tumourid ORDER BY hessg_date, source) as rk
FROM (
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS colorec_hes_appen 
, opdate AS hessg_date 
, hessg_trust_code
, 'HESAPC' as source
FROM ( 
SELECT tumourid, datediff, rk, opdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
ho.opdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear,hl.epikeyanon,POS) AS rk 
, ho.opdate 
, procode3 AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_apc@casref01 hl ON tc.patientid = hl.patientid 
INNER JOIN heslive.hesapc@casref01 ha ON ha.datayear = hl.datayear AND ha.epikeyanon = hl.epikeyanon 
INNER JOIN heslive.hesapc_opertn@casref01 ho ON ho.datayear = hl.datayear AND ho.epikeyanon = hl.epikeyanon 
AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho.opertn IN ('H024','H019','H011') AND tc.site_icd10r4_o2_from2013 in ('C181')) 
WHERE rk=1)

UNION ALL

SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS colorec_hes_appen 
, apptdate AS hessg_date 
, hessg_trust_code
, 'HESOP' as source
FROM ( 
SELECT tumourid, datediff, rk, apptdate, hessg_trust_code FROM ( 
SELECT tc.tumourid, 
op.apptdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear,h2.attendkeyanon,pos) AS rk 
, op.apptdate 
, procodet AS hessg_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_op@casref01 h2 ON tc.patientid = h2.patientid
INNER JOIN heslive.hesop@casref01 op ON op.datayear = h2.datayear and op.attendkeyanon = h2.attendkeyanon 
INNER JOIN heslive.hesop_opertn@casref01 ho2 ON ho2.datayear = h2.datayear AND ho2.attendkeyanon = h2.attendkeyanon
where op.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.resect_time 
AND ho2.opertn IN ('H024','H019','H011') AND tc.site_icd10r4_o2_from2013 in ('C181')) 
WHERE rk=1)))
WHERE rk=1);

------------------------------------------------------------------------------ 
------------------------ CREATE CHEMO FLAG TABLES ------------------------- 
------------------------------------------------------------------------------ 
--19)---------------- ALL SITES - AVCT TABLE ------------------------------- 
-- Create a chemo flag for the tumour if: 
-- There is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with chemotherapy (event is either 'Cytotoxic Chemotherapy' (code = 02) or 'CT - Other' (code = CTX) or 'chemoradiotherapy' (code = 04) or 'Immunotherapy' (code = 15)) 
-- AND the event date (eventdate) occurred in the relevant timeframe (see SOP) 
CREATE TABLE tr_av_ct AS( 
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS avct_flag 
, eventdate AS avct_date 
, avct_trust_code
FROM ( 
SELECT tumourid, datediff, rk ,eventdate, avct_trust_code FROM ( 
SELECT tc.tumourid, 
avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk 
, avtreat.eventdate
, avtreat.trust_code AS avct_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN av2021.at_treatment_england@casref01 avtreat ON avtreat.tumourid=tc.tumourid 
AND eventcode IN ('02','04','15','CTX') AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.CHEMO_TIME) 
) 
WHERE rk=1));


--20)----------------ALL SITES - SACT LEGACY -- UP TO 30th JUNE 2017 ------------------------------------- 
-- Create a chemo flag for the tumour if: 
-- there is a record in SACT LEGACY (excluding those null or classified as 'hormones' or 'Not chemo' or 'Zoledronic acid' or 'Pamidronate' or 'Denosumab' or 'RADIUM 223' or 'LUTETIUM-177' or 'YTTRIUM-90') 
-- AND the start date of the regimen (start_date_of_regimen) occurred in the relevant timeframe 
-- AND the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 
-- AND the start date of the regimen is up to 30th June 2017
CREATE TABLE tr_sact AS ( 
SELECT DISTINCT tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS sact_flag 
, start_date_of_regimen AS sact_date 
, sact_trust_code
FROM ( SELECT tumourid,datediff,rk , start_date_of_regimen, sact_trust_code 
FROM ( SELECT tc.tumourid, sr.start_date_of_regimen-tc.diagnosisdatebest AS datediff, RANK() OVER (PARTITION BY tc.tumourid ORDER BY sr.start_date_of_regimen, sr.merged_regimen_id, st.merged_tumour_id) AS rk 
, sr.start_date_of_regimen
, SUBSTR(st.organisation_code_of_provider,1,3) AS sact_trust_code 
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN sact_legacy.patient@casref01 sp ON tc.nhsnumber=sp.nhs_number 
INNER JOIN sact_legacy.tumour@casref01 st ON sp.merged_patient_id=st.merged_patient_id 
INNER JOIN sact_legacy.regimen@casref01 SR on st.merged_tumour_id=sr.merged_tumour_id 
AND (NOT (benchmark_group IN ('NOT CHEMO','HORMONES','ZOLEDRONIC ACID','PAMIDRONATE','DENOSUMAB', 'RADIUM 223', 'LUTETIUM-177', 'YTTRIUM-90') OR benchmark_group IS NULL)) 
AND sr.start_date_of_regimen-tc.diagnosisdatebest BETWEEN -31 AND tim.chemo_time 
AND sr.start_date_of_regimen<=TO_DATE('2017-06-30','YYYY-MM-DD')
) WHERE rk=1 
)); 


--21)-----------ALL SITES - SACT ENCORE -- FROM 1 JULY 2017 ----------------------------
-- Create a chemo flag for the tumour if: 
-- there is a record in SACT ENCORE (excluding those null or classified as 'hormones' or 'Not chemo' or 'Zoledronic acid' or 'Pamidronate' or 'Denosumab' or 'RADIUM 223' or 'LUTETIUM-177' or 'YTTRIUM-90') 
-- AND the start date of the regimen (start_date_of_regimen) occurred in the relevant timeframe 
-- AND the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 
-- AND the start date of the regimen is from 1 July 2017 onwards 
CREATE TABLE tr_sact_2 AS
(SELECT 
DISTINCT tumourid,
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS sact2_flag,
start_date_of_regimen AS sact2_date
, sact2_trust_code
FROM (SELECT /*+ USE_HASH(tc tim) USE_HASH(tim sp) USE_HASH(sp st) USE_HASH(st sr)*/
tumourid, datediff ,rk, start_date_of_regimen, sact2_trust_code 
	FROM (SELECT tc.tumourid, 
		sr.start_date_of_regimen-tc.diagnosisdatebest AS datediff, RANK() OVER (PARTITION BY tc.tumourid ORDER BY sr.start_date_of_regimen, sr.merged_regimen_id, st.sact_tumour_id) AS rk,
		sr.start_date_of_regimen, SUBSTR(st.organisation_code_of_provider,1,3) AS sact2_trust_code 
		FROM tr_tumour_cohort tc 
		INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01 TIM ON TIM.tumouricdsite3code = tc.tumour_code 
		INNER JOIN sact.at_patient_england@cas2408 sp ON tc.nhsnumber=sp.nhs_number 
		INNER JOIN sact.at_tumour_england@cas2408 st ON sp.encore_patient_id = st.encore_patient_id
		INNER JOIN sact.at_regimen_england@cas2408 sr ON st.sact_tumour_id=sr.sact_tumour_id 
		AND (NOT (benchmark_group IN ('NOT CHEMO','HORMONES','ZOLEDRONIC ACID','PAMIDRONATE','DENOSUMAB', 'RADIUM 223', 'LUTETIUM-177', 'YTTRIUM-90') OR benchmark_group IS NULL)) 
		AND sr.start_date_of_regimen-tc.diagnosisdatebest BETWEEN -31 AND TIM.chemo_time 
		AND sr.start_date_of_regimen>=TO_DATE('2017-07-01','YYYY-MM-DD')
		)
	WHERE rk=1 
	));


--22)--------------- ALL SITES - CHEMO FROM HES ------------------ 
-- Create a chemo flag for the tumour if: 
-- There is an inpatient or outpatient hes episode with a chemo opcs-4 code in one of the operation fields 
-- and the operation date (opertn) occurred in the relevant timeframe create table 
-- and the patient only had one tumour in the time period of interest (this is also incorporated in the final table)

CREATE TABLE tr_hes_ct AS( 
SELECT DISTINCT tumourid, hesct_flag, hesct_date, hesct_trust_code
FROM (
select tumourid, hesct_flag, hesct_date, hesct_trust_code, RANK() OVER (PARTITION BY tumourid ORDER BY hesct_date, source) as rk
FROM (
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS hesct_flag 
, opdate AS hesct_date 
, hesct_trust_code 
, 'HESAPC' as source
FROM ( 
SELECT tumourid, datediff, rk , opdate, hesct_trust_code 
FROM ( 
SELECT tc.tumourid, 
ho.opdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY ho.opdate, hl.datayear,hl.epikeyanon,POS) AS rk 
, ho.opdate 
, procode3 AS hesct_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_apc@casref01 hl ON tc.patientid = hl.patientid 
INNER JOIN heslive.hesapc@casref01 ha ON ha.datayear = hl.datayear AND ha.epikeyanon = hl.epikeyanon 
INNER JOIN heslive.hesapc_opertn@casref01 ho ON ho.datayear = hl.datayear AND ho.epikeyanon = hl.epikeyanon 
AND ho.opdate-tc.diagnosisdatebest BETWEEN -31 AND tim.chemo_time 
AND ho.opertn IN ('Y123','X962','X749','X748','X739','X738','X731','X729','X728','X724','X723','X722','X721','X719','X718',
'X715','X714','X713','X712','X711','X709','X708','X705','X704','X703','X702','X701','X385','X384','X374','X373','X353','X352',
'T482','T133','A106')
) 
WHERE rk=1)

UNION ALL

SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS hesct_flag 
, apptdate AS hesct_date 
, hesct_trust_code 
, 'HESOP' as source
FROM ( 
SELECT tumourid, datediff, rk , apptdate, hesct_trust_code 
FROM ( 
SELECT tc.tumourid, 
op.apptdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY op.apptdate, h2.datayear,h2.attendkeyanon,POS) AS rk 
, op.apptdate 
, procodet AS hesct_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN heslive.hes_linkage_av_op@casref01 h2 ON tc.patientid = h2.patientid
INNER JOIN heslive.hesop@casref01 op ON op.datayear = h2.datayear and op.attendkeyanon = h2.attendkeyanon 
INNER JOIN heslive.hesop_opertn@casref01 ho2 ON ho2.datayear = h2.datayear AND ho2.attendkeyanon = h2.attendkeyanon
where op.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.chemo_time 
AND ho2.opertn IN ('Y123','X962','X749','X748','X739','X738','X731','X729','X728','X724','X723','X722','X721','X719','X718',
'X715','X714','X713','X712','X711','X709','X708','X705','X704','X703','X702','X701','X385','X384','X374','X373','X353','X352',
'T482','T133','A106')
) 
WHERE rk=1)))
WHERE rk=1)
; 


------------------------------------------------------------------------------ 
---------------- CREATE RADIOTHERAPY FLAG TABLES ------------------ 
------------------------------------------------------------------------------ 
--23)---------------- ALL SITES - AT_TREATMENT_ENGLAND ------------------ 
-- Create a radiotherapy flag for the tumour if: 
-- There is a record in AT_TREATMENT_ENGLAND which states that the tumour was treated with radiotherapy 
--(event is either 'RT - Teletherapy' (code = 05) or 'chemoradiotherapy' (code = 04) or 'brachytherapy' (code = 06) or 'radiosurgery' (code = 22) or 'RT - Other/ NK' (code = RTX) or 'radioisotope therapy (including radioiodine)' (code = 19)) 
-- AND the event date (eventdate) occurred in the relevant timeframe (see SOP) 

CREATE TABLE tr_av_rt AS( 
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS avrt_flag 
, eventdate AS avrt_date 
, avrt_trust_code
FROM ( 
SELECT tumourid, datediff, rk, eventdate, avrt_trust_code FROM ( 
SELECT tc.tumourid, 
avtreat.eventdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY avtreat.eventdate, avtreat.eventid) AS rk 
, avtreat.eventdate 
, avtreat.trust_code AS avrt_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN av2021.at_treatment_england@casref01 avtreat ON avtreat.tumourid=tc.tumourid 
AND eventcode IN ('04', '05', '06', '22', 'RTX', '19') AND (avtreat.eventdate-tc.diagnosisdatebest BETWEEN -31 AND tim.RADIO_TIME) 
) 
WHERE rk=1 
));


--24)---------------ALL SITES - RTDS PRE APRIL 2016 (COLLECTED BY NATCANSAT)----------- 
-- Create a radiotherapy flag for the tumour if: 
-- There is a record in rtds (excluding those classed as Brachytherapy, i.e., with RTTREATMENTMODALITY='06') -- removed this restriction due to now counting radio-isotope treatments as radiotherapy rather than chemotherapy
-- AND the appointment date (APPTDATE) occurred in the relevant timeframe 
-- AND the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 
CREATE TABLE 
tr_rtds 
AS( 
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS rtds_flag 
, apptdate AS rtds_date 
, rtds_trust_code
FROM ( 
SELECT tumourid,datediff,rk , apptdate, rtds_trust_code FROM ( 
SELECT tc.tumourid, rl.apptdate-tc.diagnosisdatebest AS datediff, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY rl.apptdate,rl.attendid,rl.orgcodeprovider,pr.radiotherapyepisodeid,pr.prescriptionid) AS rk 
, rl.apptdate
, CAST(SUBSTR(pr.orgcodeprovider,1,3) AS VARCHAR(3)) AS rtds_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN rtds2016.opcds_cas1712_linkage rl ON tc.patientid=rl.patientid AND rl.apptdate-tc.diagnosisdatebest BETWEEN -31 AND tim.radio_time 
INNER JOIN rtds2016.rtds_prescriptions pr ON pr.orgcodeprovider = rl.orgcodeprovider AND pr.attendid = rl.attendid 
AND pr.apptdate = rl.apptdate
) 
WHERE rk=1 
) 
);

--25)--------------- ALL SITES - RTDS POST APRIL 2016 (COLLECTED BY NCRAS; PROCESSED BY ENCORE) ----------------- 
-- Create a radiotherapy flag for the tumour if: 
-- There is a record in rtds (excluding those classed as Brachytherapy, i.e., with RTTREATMENTMODALITY='06') -- removed this restriction due to now counting radio-isotope treatments as radiotherapy rather than chemotherapy
-- AND the appointment date (APPTDATE) occurred in the relevant timeframe 
-- AND the patient only had one tumour in the time period of interest (this is also incorporated in the final table) 
-- Do not flag the patient as receiving radiotherapy if the appointment date was before 1st April 2016 

CREATE TABLE 
tr_rtds_2 AS ( 
SELECT DISTINCT 
tumourid, 
CASE WHEN datediff IS NULL THEN 0 ELSE 1 END AS rtds2_flag 
, TO_DATE(apptdate) AS rtds2_date 
, rtds2_trust_code
FROM ( 
SELECT tumourid,datediff,rk, apptdate, rtds2_trust_code FROM ( 
SELECT tc.tumourid, TO_DATE(pr.apptdate)-tc.diagnosisdatebest AS datediff 
, TO_DATE(pr.apptdate) AS apptdate, 
RANK() OVER (PARTITION BY tc.tumourid ORDER BY TO_DATE(pr.apptdate),pr.attendid,pr.orgcodeprovider,pr.radiotherapyepisodeid,pr.prescriptionid) AS rk 
, pr.orgcodeprovider AS rtds2_trust_code
FROM tr_tumour_cohort tc 
INNER JOIN analysisnataliapetersen.timeframe_lookup_13_21@casref01  tim ON tim.tumouricdsite3code = tc.tumour_code 
INNER JOIN rtds.at_prescriptions_england@cas2408 pr ON pr.patientid=tc.patientid
AND pr.orgcodeprovider <>'7A3' 
AND TO_DATE(pr.apptdate)-tc.diagnosisdatebest BETWEEN -31 AND tim.radio_time
AND TO_DATE(pr.apptdate) >= TO_DATE('01-APR-16', 'dd-mon-yy')
) 
WHERE rk=1 
) 
); 
------------------------------------------------------------------------------ 
------------------ Index the tables from above--------------------------------- 
------------------------------------------------------------------------------- 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_AVCT_tumourid_uq ON analysisnataliapetersen.tr_av_CT ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_AVRT_tumourid_uq ON analysisnataliapetersen.tr_av_RT ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_AVSG_tumourid_uq ON analysisnataliapetersen.tr_av_sg ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_av_bladder_tumourid_uq ON analysisnataliapetersen.tr_av_bladder ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_av_coloappen_tumourid_uq ON analysisnataliapetersen.tr_av_coloappen ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_av_colorec_tumourid_uq ON analysisnataliapetersen.tr_av_colorec ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_av_conebiops_tumourid_uq ON analysisnataliapetersen.tr_av_conebiops ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_av_liver_tumourid_uq ON analysisnataliapetersen.tr_av_liver ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_av_lymph_tumourid_uq ON analysisnataliapetersen.tr_av_lymph ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_av_oesoph_tumourid_uq ON analysisnataliapetersen.tr_av_oesoph ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_av_stomach_tumourid_uq ON analysisnataliapetersen.tr_av_stomach ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX;

EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_av_CT') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_AVCT_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_av_RT') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_AVRT_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_av_sg') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_AVSG_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_av_bladder') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_av_bladder_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_av_coloappen') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_av_coloappen_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_av_colorec') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_av_colorec_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_av_conebiops') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_av_conebiops_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_av_liver') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_av_liver_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_av_lymph') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_av_lymph_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_av_oesoph') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_av_oesoph_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_av_stomach') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_av_stomach_tumourid_uq')

CREATE UNIQUE INDEX analysisnataliapetersen.tr_hes_sg_tumourid_uq ON analysisnataliapetersen.tr_hes_sg ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_hes_bladder_tumid_uq ON analysisnataliapetersen.tr_hes_bladder ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_hes_coloappen_tumid_uq ON analysisnataliapetersen.tr_hes_coloappen ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_hes_colorec_tumourid_uq ON analysisnataliapetersen.tr_hes_colorec ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_hes_conebiops_tumid_uq ON analysisnataliapetersen.tr_hes_conebiops ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_hes_liver_tumourid_uq ON analysisnataliapetersen.tr_hes_liver ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_hes_lymph_tumourid_uq ON analysisnataliapetersen.tr_hes_lymph ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_hes_oesoph_tumourid_uq ON analysisnataliapetersen.tr_hes_oesoph ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_hes_stomach_tumourid_uq ON analysisnataliapetersen.tr_hes_stomach ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_rtds_tumourid_uq ON analysisnataliapetersen.tr_rtds ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_rtds_2_tumourid_uq ON analysisnataliapetersen.tr_rtds_2 ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_sact_tumourid_uq ON analysisnataliapetersen.tr_sact ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_sact_2_tumourid_uq ON analysisnataliapetersen.tr_sact_2 ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 
CREATE UNIQUE INDEX analysisnataliapetersen.tr_hes_ct_tumourid_uq ON analysisnataliapetersen.tr_hes_ct ( tumourid ) NOLOGGING TABLESPACE analysisdata_IX; 

EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_hes_sg') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_hes_sg_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_hes_bladder') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_hes_bladder_tumid_uq')
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_hes_coloappen') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_hes_coloappen_tumid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_hes_colorec') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_hes_colorec_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_hes_conebiops') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_hes_conebiops_tumid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_hes_liver') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_hes_liver_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_hes_lymph') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_hes_lymph_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_hes_oesoph') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_hes_oesoph_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_hes_stomach') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_hes_stomach_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_rtds') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_rtds_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_rtds_2') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_rtds_2_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_sact') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_sact_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_sact_2') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_sact_2_tumourid_uq') 
EXECUTE dbms_stats.gather_table_stats('analysisnataliapetersen', 'tr_hes_ct') 
EXECUTE dbms_stats.gather_index_stats('analysisnataliapetersen', 'tr_hes_ct_tumourid_uq') 

 
------------------------------------------------------------------------------ 
----------- Create final table drawing on all previous tables------------------ 
------------------------------------------------------------------------------ 
CREATE TABLE treatment_table_1321_4p9 COMPRESS
AS 

WITH pre_treat_table AS
(
SELECT 

--Create radiotherapy (RT) flag for the tumour 
--Only use the patient level datasets (rtds, rtds2) if the patient had no other tumours recorded in the 18 months before or after this tumour diagnosis 

CASE 
WHEN avrt_flag=1 THEN 1 
WHEN rtds_flag=1 AND tc.tumour_flag=0 THEN 1 
WHEN rtds2_flag=1 AND tc.tumour_flag=0 THEN 1 
ELSE 0 
END AS rt_flag

-------------------------------------------------------------------------------- 
--Create chemo (CT) flag for the tumour 
--Only use the patient level datasets (sact, sact2) if the patient had no other tumours recorded in the 18 months before or after this tumour diagnosis 

,CASE 
WHEN avct_flag=1 THEN 1 
WHEN sact_flag=1 AND tc.tumour_flag=0 THEN 1 
WHEN sact2_flag=1 AND tc.tumour_flag=0 THEN 1
WHEN hesct_flag=1 AND tc.tumour_flag=0 THEN 1
ELSE 0 
END AS ct_flag 
-------------------------------------------------------------------------------- 
--Create resection flag for the tumour 
--Only use the patient level datasets (hes) if the patient had no other tumours recorded in the 18 months before or after this tumour diagnosis 

,CASE 
-- Firstly, incorporate non-stage specific resection flag using opcs4 resection lookup table 

WHEN AVSG_flag=1 THEN 1 
WHEN hessg_flag=1 AND tc.tumour_flag=0 THEN 1 

-- Secondly, incorporate stage specific rules for particular cancer sites 
--Cervical 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(SUBSTR(tc.figo,1,2))) IN ('1A','IA') AND conebiops_avtreat=1 THEN 1 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(SUBSTR(tc.figo,1,2))) IN ('1A','IA') AND conebiops_hes=1 AND tc.tumour_flag=0 THEN 1 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_avtreat=1) AND (lymph_avtreat=1) THEN 1 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_avtreat=1) AND (lymph_hes=1 AND tc.tumour_flag=0) THEN 1 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_hes=1 AND tc.tumour_flag=0) AND (lymph_avtreat=1) THEN 1 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_hes=1 AND tc.tumour_flag=0) AND (lymph_hes=1 AND tc.tumour_flag=0) THEN 1 

--colorectal: 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C18','C19','C20') AND SUBSTR(avt.stage_best,1,1)='1' AND colorec_avtreat=1 THEN 1 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C18','C19','C20') AND SUBSTR(avt.stage_best,1,1)='1' AND colorec_hes=1 AND tc.tumour_flag=0 THEN 1 
--Sub rule for appendectomies for colorectal: 
WHEN avt.site_icd10r4_o2_from2013 IN ('C181') AND colorec_avtreat_appen=1 THEN 1 
WHEN avt.site_icd10r4_o2_from2013 IN ('C181') AND colorec_hes_appen=1 AND tc.tumour_flag=0 THEN 1 

--bladder 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C67') AND SUBSTR(avt.t_best, 1,1) = '1' AND bladder_avtreat=1 THEN 1 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C67') AND SUBSTR(avt.t_best, 1,1) = '1' AND bladder_hes=1 AND tc.tumour_flag=0 THEN 1

WHEN avt.site_icd10r4_o2_from2013 IN ('D090') AND avt.morph_icd10_o2 = '8130' AND bladder_avtreat=1 THEN 1 
WHEN avt.site_icd10r4_o2_from2013 IN ('D090') AND avt.morph_icd10_o2 = '8130' AND bladder_hes=1 AND tc.tumour_flag=0 THEN 1

-- liver 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('LIVER') AND SUBSTR(avt.stage_best,1,1)='1' AND liver_avtreat=1 THEN 1 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('LIVER') AND SUBSTR(avt.stage_best,1,1)='1' AND liver_hes=1 AND tc.tumour_flag=0 THEN 1 

-- oesophagus 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C15') AND SUBSTR(avt.stage_best, 1,2)='1A' AND oesoph_avtreat=1 THEN 1 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C15') AND SUBSTR(avt.stage_best,1,2)='1A' AND oesoph_hes=1 AND tc.tumour_flag=0 THEN 1 

-- stomach 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C16') AND SUBSTR(avt.stage_best,1,2)='1A' AND stomach_avtreat=1 THEN 1 
WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C16') AND SUBSTR(avt.stage_best,1,2)='1A' AND stomach_hes=1 AND tc.tumour_flag=0 THEN 1 
ELSE 0 

END AS sg_flag 

-------------------------------------------------------------------------------- 
--Create cancer site names 
,CASE WHEN tumour_code IN ('C67') THEN 'MALIGNANT BLADDER' 
WHEN tumour_code IN ('D09BLADDER','D41BLADDER') THEN 'NON-MALIGNANT BLADDER' 
WHEN tumour_code IN ('C50') THEN 'BREAST' 
WHEN tumour_code IN ('C53') THEN 'CERVICAL' 
WHEN tumour_code IN ('C18','C19') THEN 'COLON' 
WHEN tumour_code IN ('C20') THEN 'RECTUM' 
WHEN tumour_code IN ('C21') THEN 'ANAL'
WHEN tumour_code IN ('C01', 'C09', 'C10') THEN 'OROPHARYNX' 
WHEN tumour_code IN ('C02', 'C03', 'C04', 'C06') THEN 'ORAL_CAVITY' 
WHEN tumour_code IN ('C07', 'C08') THEN 'SALIVARY_GLANDS' 
WHEN tumour_code IN ('C12', 'C13') THEN 'HYPOPHARYNX' 
WHEN tumour_code IN ('C32') THEN 'LARYNX'
WHEN tumour_code IN ('C05', 'C11', 'C14', 'C30', 'C31') THEN 'OTHER_HEAD_AND_NECK' 
WHEN tumour_code IN ('C64', 'C65', 'C66', 'C68') THEN 'KIDNEY' 
WHEN tumour_code IN ('C33', 'C34') AND tc.morph_icd10_o2 IN ('8002', '8041','8042','8043','8044','8045') THEN 'SCLC' 
WHEN tumour_code IN ('C33', 'C34') AND tc.morph_icd10_o2 NOT IN ('8002','8041','8042','8043','8044','8045') THEN 'NSCLC' 
WHEN tumour_code IN ('C25') THEN 'PANCREAS' 
WHEN tumour_code IN ('C61') THEN 'PROSTATE' 
WHEN tumour_code IN ('C15') THEN 'OESOPHAGUS' 
WHEN tumour_code IN ('C56', 'C57','C48OVARY', 'D39OVARY') THEN 'OVARY' 
WHEN tumour_code IN ('C16') THEN 'STOMACH' 
WHEN tumour_code IN ('C54', 'C55') THEN 'UTERINE' 
WHEN tumour_code IN ('C51') THEN 'VULVA' 
WHEN tumour_code IN ('C70', 'C71', 'C72') THEN 'MALIGNANT BRAIN'
WHEN tumour_code IN ('D32BRAIN', 'D33BRAIN', 'D42BRAIN', 'D43BRAIN') THEN 'NON-MALIGNANT BRAIN'
WHEN tumour_code IN ('D35BRAIN') THEN 'BENIGN ENDOCRINE'
WHEN tumour_code IN ('C75BRAIN', 'D44BRAIN') THEN 'NON-BENIGN ENDOCRINE'
WHEN tumour_code IN ('C62', 'D29TESTES') THEN 'TESTES' 
WHEN tumour_code IN ('NON-KC_MELANOMA') THEN 'SKIN:NON-KERATINOCYTE, MELANOMA'
WHEN tumour_code IN ('NON-KC_MELANOMA_INSITU') THEN 'SKIN:NON-KERATINOCYTE, MELANOMA IN SITU'
WHEN tumour_code IN ('NON-KC_RARE','NON-KC_EMPD') THEN 'SKIN:NON-KERATINOCYTE, RARE' 
WHEN tumour_code IN ('KC_BCC') THEN 'SKIN:KERATINOCYTE SKIN, BCC' 
WHEN tumour_code IN ('KC_CSCC') THEN 'SKIN:KERATINOCYTE, CSCC' 
WHEN tumour_code IN ('LIVER') THEN 'LIVER' --Liver excluding intrahepatic cholangiocarcinoma
WHEN tumour_code IN ('CHOLANGIOCARCINOMA') THEN 'CHOLANGIOCARCINOMA' --Cholangiocarcinoma
WHEN tumour_code IN ('AOV') THEN 'AMPULLA_OF_VATER' --Ampulla of Vater
WHEN tumour_code IN ('C23') THEN 'GALLBLADDER' --Gallbladder
WHEN tumour_code IN ('ALL') THEN UPPER('HAEM:Acute lymphoblastic leukaemia (ALL)')
WHEN tumour_code IN ('AML') THEN UPPER('HAEM:Acute myeloid leukaemia (AML)')
WHEN tumour_code IN ('HODGKIN') THEN UPPER('HAEM:Hodgkin lymphoma')
WHEN tumour_code IN ('CLLSLL') THEN UPPER('HAEM:Chronic lymphocytic leukaemia (CLL) or small lymphocytic lymphoma (SLL)')
WHEN tumour_code IN ('DLBCL') THEN UPPER('HAEM:Diffuse large B-cell lymphoma (DLBCL) and other high grade mature B-cell neoplasms')
WHEN tumour_code IN ('FOLLICULAR') THEN UPPER('HAEM:Follicular lymphoma')
WHEN tumour_code IN ('MCL') THEN UPPER('HAEM:Mantle cell lymphoma (MCL)')
WHEN tumour_code IN ('CML') THEN UPPER('HAEM:Chronic myeloid leukaemia (CML)')
WHEN tumour_code IN ('MYELOMA') THEN UPPER('HAEM:Myeloma')
WHEN tumour_code IN ('OTHER_HAEM') THEN UPPER('HAEM:Other haematological malignancies')

WHEN SUBSTR(tumour_code,1,1)='D' AND tumour_code NOT IN ('D01','D04','D03','D06','D07','D11','D13','D15','D16','D18','D25','D27','D36','D40','D48','D29TESTES', 'D32BRAIN', 'D33BRAIN', 'D35BRAIN', 'D39OVARY', 'D39OVARY', 'D42BRAIN', 'D43BRAIN', 'D44BRAIN') THEN 'OTHER NON-MALIGNANT' 
ELSE 'OTHER MALIGNANT' 
END AS cancergroup 

-- Select all other variables 
,avt.tumourid 
,avt.diagnosisyear 
,avt.age 
,avt.gender as gender
,avt.dco 
,avt.basisofdiagnosis 
,avt.fiveyearageband 
,avt.ethnicity 
,chrl.chrl_tot_27_03
,case
    when (diagnosisyear = 2013) then IMD15_quintile_lsoas
    when (diagnosisyear IN ('2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021')) then IMD19_quintile_lsoas
    end as imd_quintile_lsoas
,icb23nm as icb_2023_name
,icb23cd as icb_2023_code
,cal23nm as canalliance_2023_name
,cal23cd as canalliance_2023_code
--For checking 
,avt.morph_icd10_o2 
,tc.figo 
,avt.t_best 
,avt.stage_best 
,tc.site_icd10r4_o2_from2013 
,site_icd10r4_o2_3char_from2013 
,tc.tumour_flag

------------------------------------------------------------------------------------------------------------ 
--Select dates of treatment from at_treatment_england 
,avt.diagnosisdatebest 
,avt.deathdatebest 
,avct.avct_date 
,avrt.avrt_date 
,avsg.avsg_date 

--Select dates of treatment from patient-level datasets where only 1 tumour was diagnosed in 18 months before or after that tumour 
,CASE WHEN tc.tumour_flag=0 THEN sact.sact_date END AS sact_date 
,CASE WHEN tc.tumour_flag=0 THEN sact2.sact2_date END AS sact2_date
,CASE WHEN tc.tumour_flag=0 THEN hesct.hesct_date END AS hesct_date
,CASE WHEN tc.tumour_flag=0 THEN rtds.rtds_date END AS rtds_date
,CASE WHEN tc.tumour_flag=0 THEN hessg.hessg_date END AS hessg_date 
,CASE WHEN tc.tumour_flag=0 THEN rtds2.rtds2_date END AS rtds2_date


------------------------------------------------------------------------------------------------------------ 
--Select date of surgery where there were additional site-specific resections flagged: 
------------------CERVICAL------------------ 
-- Take date of cone biopsy in at_treatment_england if: 
-- The tumour received a cone biopsy and was FIGO stage 1a 
-- Or the tumour received a cone biopsy and was FIGO stage 1b & 1b1 disease, if the tumour also received a lymphadenectomy
 
, CASE 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(SUBSTR(tc.figo,1,2)) IN ('1A','IA')) AND conebiops_avtreat=1 THEN cbavt.avsg_date 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_avtreat=1) AND (lymph_avtreat=1) THEN cbavt.avsg_date 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_avtreat=1) AND (lymph_hes=1 AND tc.tumour_flag=0) THEN cbavt.avsg_date 
END AS cbavsg_date 

--Take date of cone biopsy in hes if: 
--The tumour received a cone biopsy and was FIGO stage 1a 
--Or the tumour received a cone biopsy and was FIGO stage 1b & 1b1 disease, if the tumour also received a lymphadenectomy 
--and only 1 tumour was diagnosed in 18 months before or after that tumour 

, CASE 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(SUBSTR(tc.figo,1,2)) IN ('1A','IA')) AND conebiops_hes=1 AND tc.tumour_flag=0 THEN cbhes.hessg_date 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_hes=1 AND tc.tumour_flag=0) AND (lymph_avtreat=1) THEN cbhes.hessg_date 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_hes=1 AND tc.tumour_flag=0) AND (lymph_hes=1 AND tc.tumour_flag=0) THEN cbhes.hessg_date 
END AS cbhessg_date 

---------------colorectal--------------------------------- 
-- As with cervical, select the date of the stage-specific resection for each tumour, according to the rules specified earlier for generating the stage-specific resection flag for that tumour site 
,CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C18','C19','C20') AND SUBSTR(avt.stage_best,1,1)='1' AND colorec_avtreat=1 THEN coloavt.avsg_date 
END AS coloavsg_date 
,CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C18','C19','C20') AND SUBSTR(avt.stage_best,1,1)='1' AND colorec_hes=1 AND tc.tumour_flag=0 THEN colohes.hessg_date 
END AS colohessg_date 
,CASE WHEN avt.site_icd10r4_o2_from2013 IN ('C181') AND colorec_avtreat_appen=1 THEN coloavt_appen.avsg_date 
END AS appenavsg_date 
, CASE WHEN avt.site_icd10r4_o2_from2013 IN ('C181') AND colorec_hes_appen=1 AND tc.tumour_flag=0 THEN colohes_appen.hessg_date 
END AS appenhessg_date

---------------bladder--------------------------------- 
,CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C67') AND SUBSTR(avt.t_best, 1,1) = '1' AND bladder_avtreat=1 THEN blad1_avt.avsg_date 
END AS bladavsg_date 
, CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C67') AND SUBSTR(avt.t_best, 1,1) = '1' AND bladder_hes=1 AND tc.tumour_flag=0 THEN blad1_hes.hessg_date 
END AS bladhessg_date 

,CASE WHEN avt.site_icd10r4_o2_from2013 IN ('D090') AND avt.morph_icd10_o2 = '8130' AND bladder_avtreat=1 THEN blad1_avt.avsg_date 
END AS blad_insitu_avsg_date 
, CASE WHEN avt.site_icd10r4_o2_from2013 IN ('D090') AND avt.morph_icd10_o2 = '8130' AND bladder_hes=1 AND tc.tumour_flag=0 THEN blad1_hes.hessg_date 
END AS blad_insitu_hessg_date 

---------------liver--------------------------------- 
,CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('LIVER') AND SUBSTR(avt.stage_best,1,1)='1' AND liver_avtreat=1 THEN livavt.avsg_date 
END AS livavsg_date 
, CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('LIVER') AND SUBSTR(avt.stage_best,1,1)='1' AND liver_hes=1 AND tc.tumour_flag=0 THEN livhes.hessg_date 
END AS livhessg_date 

---------------oesophageal--------------------------------- 
,CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C15') AND SUBSTR(avt.stage_best,1,2)='1A' AND oesoph_avtreat=1 THEN oesoavt.avsg_date 
END AS oesoavsg_date 
, CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C15') AND SUBSTR(avt.stage_best,1,2)='1A' AND oesoph_hes=1 AND tc.tumour_flag=0 THEN oesohes.hessg_date 
END AS oesohessg_date 

---------------stomach--------------------------------- 
, CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C16') AND SUBSTR(avt.stage_best,1,2)='1A' AND stomach_avtreat=1 THEN stomavt.avsg_date 
END AS stomavsg_date 
, CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C16') AND SUBSTR(avt.stage_best,1,2)='1A' AND stomach_hes=1 AND tc.tumour_flag=0 THEN stomhes.hessg_date 
END AS stomhessg_date 


------------------------------------------------------------------------------------------------------------ 

--Select trust codes from at_treatment_england
, avsg.avsg_trust_code
, avct_trust_code
, avrt_trust_code

--Select trust codes of treatment from patient-level datasets where only 1 tumour was diagnosed in 18 months before or after that tumour 
,CASE WHEN tc.tumour_flag=0 THEN hessg.hessg_trust_code END AS hessg_trust_code
,CASE WHEN tc.tumour_flag=0 THEN sact.sact_trust_code END AS sact_trust_code 
,CASE WHEN tc.tumour_flag=0 THEN sact2.sact2_trust_code END AS sact2_trust_code
,CASE WHEN tc.tumour_flag=0 THEN hesct.hesct_trust_code END AS hesct_trust_code
,CASE WHEN tc.tumour_flag=0 THEN rtds.rtds_trust_code END AS rtds_trust_code
,CASE WHEN tc.tumour_flag=0 THEN rtds2.rtds2_trust_code END AS rtds2_trust_code

------------------------------------------------------------------------------------------------------------ 
--Select trust codes of surgery where there were additional site-specific resections flagged: 
------------------CERVICAL------------------ 
-- Take trust code of cone biopsy in at_treatment_england if: 
-- The tumour received a cone biopsy and was FIGO stage 1a 
-- Or the tumour received a cone biopsy and was FIGO stage 1b & 1b1 disease, if the tumour also received a lymphadenectomy
 
, CASE 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(SUBSTR(tc.figo,1,2)) IN ('1A','IA')) AND conebiops_avtreat=1 THEN cbavt.avsg_trust_code 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_avtreat=1) AND (lymph_avtreat=1) THEN cbavt.avsg_trust_code 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_avtreat=1) AND (lymph_hes=1 AND tc.tumour_flag=0) THEN cbavt.avsg_trust_code 
END AS cbavsg_trust_code 

--Take date of cone biopsy in hes if: 
--The tumour received a cone biopsy AND was FIGO stage 1a 
--Or the tumour received a cone biopsy AND was FIGO stage 1b & 1b1 disease, if the tumour also received a lymphadenectomy 
--AND only 1 tumour was diagnosed in 18 months before or after that tumour 

, CASE 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(SUBSTR(tc.figo,1,2)) IN ('1A','IA')) AND conebiops_hes=1 AND tc.tumour_flag=0 THEN cbhes.hessg_trust_code 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_hes=1 AND tc.tumour_flag=0) AND (lymph_avtreat=1) THEN cbhes.hessg_trust_code 
WHEN avt.site_icd10r4_o2_3char_from2013='C53' AND (upper(tc.figo) IN ('1B','IB') or upper(SUBSTR(tc.figo,1,3)) IN ('1B1','IB1')) AND (conebiops_hes=1 AND tc.tumour_flag=0) AND (lymph_hes=1 AND tc.tumour_flag=0) THEN cbhes.hessg_trust_code 
END AS cbhessg_trust_code 

---------------colorectal--------------------------------- 
-- As with cervical, select the date of the stage-specific resection for each tumour, according to the rules specified earlier for generating the stage-specific resection flag for that tumour site 
,CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C18','C19','C20') AND SUBSTR(avt.stage_best,1,1)='1' AND colorec_avtreat=1 THEN coloavt.avsg_trust_code 
END AS coloavsg_trust_code 
,CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C18','C19','C20') AND SUBSTR(avt.stage_best,1,1)='1' AND colorec_hes=1 AND tc.tumour_flag=0 THEN colohes.hessg_trust_code 
END AS colohessg_trust_code 
,CASE WHEN avt.site_icd10r4_o2_from2013 IN ('C181') AND colorec_avtreat_appen=1 THEN coloavt_appen.avsg_trust_code 
END AS appenavsg_trust_code 
, CASE WHEN avt.site_icd10r4_o2_from2013 IN ('C181') AND colorec_hes_appen=1 AND tc.tumour_flag=0 THEN colohes_appen.hessg_trust_code 
END AS appenhessg_trust_code

---------------bladder--------------------------------- 
,CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C67') AND SUBSTR(avt.t_best, 1,1) = '1' AND bladder_avtreat=1 THEN blad1_avt.avsg_trust_code 
END AS bladavsg_trust_code 
, CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C67') AND SUBSTR(avt.t_best, 1,1) = '1' AND bladder_hes=1 AND tc.tumour_flag=0 THEN blad1_hes.hessg_trust_code 
END AS bladhessg_trust_code 

,CASE WHEN avt.site_icd10r4_o2_from2013 IN ('D090') AND avt.morph_icd10_o2 = '8130' AND bladder_avtreat=1 THEN blad1_avt.avsg_trust_code 
END AS blad_insitu_avsg_trust_code 
, CASE WHEN avt.site_icd10r4_o2_from2013 IN ('D090') AND avt.morph_icd10_o2 = '8130' AND bladder_hes=1 AND tc.tumour_flag=0 THEN blad1_hes.hessg_trust_code 
END AS blad_insitu_hessg_trust_code 

---------------liver--------------------------------- 
,CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('LIVER') AND SUBSTR(avt.stage_best,1,1)='1' AND liver_avtreat=1 THEN livavt.avsg_trust_code 
END AS livavsg_trust_code 
, CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('LIVER') AND SUBSTR(avt.stage_best,1,1)='1' AND liver_hes=1 AND tc.tumour_flag=0 THEN livhes.hessg_trust_code 
END AS livhessg_trust_code 

---------------oesophageal--------------------------------- 
,CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C15') AND SUBSTR(avt.stage_best,1,2)='1A' AND oesoph_avtreat=1 THEN oesoavt.avsg_trust_code 
END AS oesoavsg_trust_code 
, CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C15') AND SUBSTR(avt.stage_best,1,2)='1A' AND oesoph_hes=1 AND tc.tumour_flag=0 THEN oesohes.hessg_trust_code 
END AS oesohessg_trust_code 

---------------stomach--------------------------------- 
, CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C16') AND SUBSTR(avt.stage_best,1,2)='1A' AND stomach_avtreat=1 THEN stomavt.avsg_trust_code 
END AS stomavsg_trust_code 
, CASE WHEN avt.site_icd10r4_o2_3char_from2013 IN ('C16') AND SUBSTR(avt.stage_best,1,2)='1A' AND stomach_hes=1 AND tc.tumour_flag=0 THEN stomhes.hessg_trust_code 
END AS stomhessg_trust_code 


------------------------------------------------------------------------------- 

-- final join of tables with flags 
-- Treatment flag tables 
-- Do not flag surgery for non-ovarian C48 tumour morphologies (these are classified as "other" tumours) 

FROM av2021.at_tumour_england@casref01 AVT 

INNER JOIN analysisnataliapetersen.tr_tumour_cohort@casref01 tc ON avt. tumourid =tc. tumourid
LEFT JOIN analysisnataliapetersen.tr_av_ct@casref01 avct ON avt.tumourid=avct.tumourid 
LEFT JOIN analysisnataliapetersen.tr_sact@casref01 sact ON avt.tumourid=sact.tumourid 
LEFT JOIN analysisnataliapetersen.tr_sact_2@casref01 sact2 ON avt.tumourid=sact2.tumourid
LEFT JOIN analysisnataliapetersen.tr_hes_ct@casref01 hesct ON avt.tumourid=hesct.tumourid
LEFT JOIN analysisnataliapetersen.tr_av_rt@casref01 avrt ON avt.tumourid=avrt.tumourid 
LEFT JOIN analysisnataliapetersen.tr_av_sg@casref01 avsg ON avt.tumourid=avsg.tumourid AND (tc.tumour_code NOT IN ('C48OTHER')) 
LEFT JOIN analysisnataliapetersen.tr_rtds@casref01 rtds ON avt.tumourid=rtds.tumourid 
LEFT JOIN analysisnataliapetersen.tr_hes_sg@casref01 hessg ON avt.tumourid=hessg.tumourid AND (tc.tumour_code NOT IN ('C48OTHER')) 
LEFT JOIN analysisnataliapetersen.tr_rtds_2@casref01 rtds2 ON avt.tumourid=rtds2.tumourid 

-- Add further joins for stage-specific resections:

---------------CERVICAL------------------
LEFT JOIN analysisnataliapetersen.tr_av_conebiops@casref01 CBAVT ON avt.tumourid=cbavt.tumourid 
LEFT JOIN analysisnataliapetersen.tr_hes_conebiops@casref01 CBhes ON avt.tumourid=cbhes.tumourid 
LEFT JOIN analysisnataliapetersen.tr_av_lymph@casref01 lyavt ON avt.tumourid=lyavt.tumourid 
LEFT JOIN analysisnataliapetersen.tr_hes_lymph@casref01 lyhes ON avt.tumourid=lyhes.tumourid 

---------------Colorectal---------------------------------
LEFT JOIN analysisnataliapetersen.tr_av_colorec@casref01 coloavt ON avt.tumourid=coloavt.tumourid 
LEFT JOIN analysisnataliapetersen.tr_hes_colorec@casref01 colohes ON avt.tumourid=colohes.tumourid 
LEFT JOIN analysisnataliapetersen.tr_av_coloappen@casref01 coloavt_appen ON avt.tumourid=coloavt_appen.tumourid 
LEFT JOIN analysisnataliapetersen.tr_hes_coloappen@casref01 colohes_appen ON avt.tumourid=colohes_appen.tumourid 

---------------Bladder---------------------------------
LEFT JOIN analysisnataliapetersen.tr_av_bladder@casref01 blad1_avt ON avt.tumourid=blad1_avt.tumourid 
LEFT JOIN analysisnataliapetersen.tr_hes_bladder@casref01 blad1_hes ON avt.tumourid=blad1_hes.tumourid 

---------------Liver---------------------------------
LEFT JOIN analysisnataliapetersen.tr_av_liver@casref01 livavt ON avt.tumourid=livavt.tumourid 
LEFT JOIN analysisnataliapetersen.tr_hes_liver@casref01 livhes ON avt.tumourid=livhes.tumourid 

---------------Oesophageal---------------------------------
LEFT JOIN analysisnataliapetersen.tr_av_oesoph@casref01 oesoavt ON avt.tumourid=oesoavt.tumourid 
LEFT JOIN analysisnataliapetersen.tr_hes_oesoph@casref01 oesohes ON avt.tumourid=oesohes.tumourid 

---------------Stomach---------------------------------
LEFT JOIN analysisnataliapetersen.tr_av_stomach@casref01 stomavt ON avt.tumourid=stomavt.tumourid 
LEFT JOIN analysisnataliapetersen.tr_hes_stomach@casref01 stomhes ON avt.tumourid=stomhes.tumourid 

-- Additional demographics 
LEFT JOIN av2021.at_geography_england atg ON avt.tumourid=atg.tumourid
LEFT JOIN NSPLC21_202305 nspl on replace(nspl.PCD,' ','')=replace(atg.postcode,' ','')
LEFT JOIN ANALYSISNCR.LSOA21_SICBL_ICB_CA_LA lsoa on lsoa.LSOA21CD=nspl.LSOA21

LEFT JOIN imd.imd2015_equal_lsoas imd15 ON atg.lsoa11_code = imd15.lsoa11_code 
LEFT JOIN imd.imd2019_equal_lsoas imd19 ON atg.lsoa11_code = imd19.lsoa11_code
LEFT JOIN av2021.charlson_2006to2021@casref01 chrl ON chrl.tumourid=avt.tumourid


), summary_fields as(

SELECT tumourid

-- Take first non-null date from avct, sact, sact2, hesct
,CASE WHEN CT_FLAG=1 THEN upper(COALESCE(avct_date, sact_date, sact2_date, hesct_date)) end as summary_ct_date

-- Take first non-null date from avrt, rtds, rtds2
,CASE WHEN RT_FLAG=1 THEN upper(COALESCE(avrt_date, rtds_date, rtds2_date)) end as summary_rt_date

-- Take first non-null date from Avsg, hessg, other site / stage specific stage fields
,CASE WHEN SG_FLAG=1 THEN upper(COALESCE(cbavsg_date, cbhessg_date, coloavsg_date, colohessg_date, appenavsg_date, appenhessg_date, bladavsg_date, bladhessg_date, 
blad_insitu_avsg_date, blad_insitu_hessg_date, livavsg_date, livhessg_date, oesoavsg_date, oesohessg_date, stomavsg_date, stomhessg_date, avsg_date, hessg_date)) end as summary_sg_date


-- Take name corresponding to first non-null date from avct, sact, sact2, hesct
,CASE 
WHEN (CT_FLAG=1 and instr(COALESCE(avct_trust_code, sact_trust_code, sact2_trust_code, hesct_trust_code),'-X') > 0) 

THEN substr(upper(COALESCE(avct_trust_code, sact_trust_code, sact2_trust_code, hesct_trust_code)),1,3)

WHEN CT_FLAG=1 THEN upper(COALESCE(avct_trust_code, sact_trust_code, sact2_trust_code, hesct_trust_code)) end as summary_ct_trustcode

-- Take name corresponding to first non-null date from avrt, rtds, rtds2
,CASE 
WHEN (RT_FLAG=1 and instr(COALESCE(avrt_trust_code, rtds_trust_code, rtds2_trust_code),'-X') > 0) 

THEN substr(upper(COALESCE(avrt_trust_code, rtds_trust_code, rtds2_trust_code)),1,3)

WHEN RT_FLAG=1 THEN upper(COALESCE(avrt_trust_code, rtds_trust_code, rtds2_trust_code)) end as summary_rt_trustcode


-- Take name corresponding to first non-null date from Avsg, hessg, other site / stage specific stage fields
, CASE 
WHEN (SG_FLAG=1 and instr(COALESCE(cbavsg_trust_code, cbhessg_trust_code, coloavsg_trust_code, colohessg_trust_code, appenavsg_trust_code, appenhessg_trust_code, bladavsg_trust_code, bladhessg_trust_code, 
blad_insitu_avsg_trust_code, blad_insitu_hessg_trust_code, livavsg_trust_code, livhessg_trust_code, oesoavsg_trust_code, oesohessg_trust_code, stomavsg_trust_code, stomhessg_trust_code, avsg_trust_code, 
hessg_trust_code),'-X') > 0) 

THEN substr(upper(COALESCE(cbavsg_trust_code, cbhessg_trust_code, coloavsg_trust_code, colohessg_trust_code, appenavsg_trust_code, appenhessg_trust_code, bladavsg_trust_code, bladhessg_trust_code, 
blad_insitu_avsg_trust_code, blad_insitu_hessg_trust_code, livavsg_trust_code, livhessg_trust_code, oesoavsg_trust_code, oesohessg_trust_code, stomavsg_trust_code, stomhessg_trust_code, avsg_trust_code, 
hessg_trust_code)),1,3) 

WHEN SG_FLAG=1 THEN upper(COALESCE(cbavsg_trust_code, cbhessg_trust_code, coloavsg_trust_code, colohessg_trust_code, appenavsg_trust_code, appenhessg_trust_code, bladavsg_trust_code, bladhessg_trust_code, 
blad_insitu_avsg_trust_code, blad_insitu_hessg_trust_code, livavsg_trust_code, livhessg_trust_code, oesoavsg_trust_code, oesohessg_trust_code, stomavsg_trust_code, stomhessg_trust_code, avsg_trust_code, 
hessg_trust_code)) end as summary_sg_trustcode

FROM pre_treat_table 

)

SELECT 
ptr.TUMOURID
,ptr.RT_FLAG
,ptr.CT_FLAG
,ptr.SG_FLAG
,ptr.TUMOUR_FLAG
,ptr.CANCERGROUP
,ptr.DIAGNOSISYEAR
,ptr.AGE
,ptr.FIVEYEARAGEBAND
,ptr.GENDER
,ptr.DIAGNOSISDATEBEST
,ptr.BASISOFDIAGNOSIS
,ptr.site_icd10r4_o2_from2013
,ptr.site_icd10r4_o2_3char_from2013
,ptr.MORPH_ICD10_O2
,ptr.STAGE_BEST
,ptr.T_BEST
,ptr.FIGO
,ptr.DEATHDATEBEST
,ptr.DCO
,ptr.ETHNICITY
,ptr.CHRL_TOT_27_03
,ptr.IMD_QUINTILE_LSOAS
,ptr.icb_2023_name
,ptr.icb_2023_code
,ptr.canalliance_2023_name
,ptr.canalliance_2023_code

,ptr.AVCT_DATE
,ptr.SACT_DATE
,ptr.SACT2_DATE
,ptr.HESCT_DATE
,to_date(sf.summary_ct_date,'dd-mon-yy') as summary_ct_date

,ptr.AVRT_DATE
,ptr.RTDS_DATE
,ptr.RTDS2_DATE
,to_date(sf.summary_rt_date,'dd-mon-yy') as summary_rt_date

,ptr.AVSG_DATE
,ptr.HESSG_DATE
,ptr.CBAVSG_DATE
,ptr.CBHESSG_DATE
,ptr.COLOAVSG_DATE
,ptr.COLOHESSG_DATE
,ptr.APPENAVSG_DATE
,ptr.APPENHESSG_DATE
,ptr.BLADAVSG_DATE
,ptr.BLADHESSG_DATE
,ptr.BLAD_INSITU_AVSG_DATE
,ptr.BLAD_INSITU_HESSG_DATE
,ptr.LIVAVSG_DATE
,ptr.LIVHESSG_DATE
,ptr.OESOAVSG_DATE
,ptr.OESOHESSG_DATE
,ptr.STOMAVSG_DATE
,ptr.STOMHESSG_DATE
,to_date(sf.summary_sg_date,'dd-mon-yy') as summary_sg_date

,ptr.AVCT_TRUST_CODE
,ptr.SACT_TRUST_CODE
,ptr.SACT2_TRUST_CODE
,ptr.hesct_trust_code
,sf.summary_ct_trustcode
,atr_ct.trustname as summary_ct_trustname

,ptr.AVRT_TRUST_CODE
,ptr.RTDS_TRUST_CODE
,ptr.RTDS2_TRUST_CODE
,sf.summary_rt_trustcode
,atr_rt.trustname as summary_rt_trustname

,ptr.AVSG_TRUST_CODE
,ptr.HESSG_TRUST_CODE
,ptr.CBAVSG_TRUST_CODE
,ptr.CBHESSG_TRUST_CODE
,ptr.COLOAVSG_TRUST_CODE
,ptr.COLOHESSG_TRUST_CODE
,ptr.APPENAVSG_TRUST_CODE
,ptr.APPENHESSG_TRUST_CODE
,ptr.BLADAVSG_TRUST_CODE
,ptr.BLADHESSG_TRUST_CODE
,ptr.BLAD_INSITU_AVSG_TRUST_CODE
,ptr.BLAD_INSITU_HESSG_TRUST_CODE
,ptr.LIVAVSG_TRUST_CODE
,ptr.LIVHESSG_TRUST_CODE
,ptr.OESOAVSG_TRUST_CODE
,ptr.OESOHESSG_TRUST_CODE
,ptr.STOMAVSG_TRUST_CODE
,ptr.STOMHESSG_TRUST_CODE
,sf.summary_sg_trustcode
,atr_sg.trustname as summary_sg_trustname


FROM pre_treat_table ptr
LEFT JOIN summary_fields sf on ptr.tumourid = sf.tumourid 
-- Trust name
LEFT JOIN analysisncr.trustsics@cas2408 atr_ct on sf.summary_ct_trustcode = atr_ct.code
LEFT JOIN analysisncr.trustsics@cas2408 atr_rt on sf.summary_rt_trustcode = atr_rt.code
LEFT JOIN analysisncr.trustsics@cas2408 atr_sg on sf.summary_sg_trustcode = atr_sg.code

;


grant select on treatment_table_1321_4p9 to analysischarlotteeversfield;
grant select on opcs4resection_lookup_13_21 to analysischarlotteeversfield;
grant select on timeframe_lookup_13_21 to analysischarlotteeversfield;
grant select on tr_tumour_cohort to analysischarlotteeversfield;

grant select on treatment_table_1321_4p9 to analysisseanmcphail;
grant select on opcs4resection_lookup_13_21 to analysisseanmcphail;
grant select on timeframe_lookup_13_21 to analysisseanmcphail;
grant select on tr_tumour_cohort to analysisseanmcphail;
