# airPollution_ADRD
air pollution and mortality/readmission in Medicare ADRD

## generate dataset
### extract all 2000-2016 hospitalization records with ADRD billing codes
[01extractADRDhospital.R](https://github.com/ShuxinD/airPollution_ADRD/blob/main/codes/00generate_data/01extractADRDhospital.R): 
- screened 25 billing codes from 2000 to 2016
	- though FFS data only billed 10 codes before a certain year (cannot remember) and change to 25 codes afterwards so as to select ADRD people as much as possible from Medicare data
- searched with both ICD-9 and ICD-10 ADRD codes,
	- though ICD codes changed to the 10th version after 2015, in our dataset there are still some mix-ups of the 9th and 10th versions. ADRD diagnosis codes were based on [**Using Medicare claims in identifying Alzheimer’s disease and related dementias**](https://alz-journals.onlinelibrary.wiley.com/doi/10.1002/alz.12199) and [**Identifying Medicare beneficiaries with dementia**](https://agsjournals.onlinelibrary.wiley.com/doi/10.1111/jgs.17183); codes are modified from [a previous one](https://github.com/NSAPH/data_requests/blob/master/request_projects/jan2021_whanhee_fisrt_hosps/code/2_id_hospitalizations.R).
- export several `.fst` files named as `ADRD'type'_'year'.fst`, e.g. `ADRDprimary_2000.fst`
	- `primary` fst file contains hospitalization records with first diagnosis code (`DIAG1`) as ADRD, and `secondary` fst file contains those with any of codes (1-25) as ADRD. (`secondary` file contains `primary`); 

### generate ADRD cohort enrollment info based on hospilization record
[02getEnrolledInfo.R](https://github.com/ShuxinD/airPollution_ADRD/blob/main/codes/00generate_data/02getEnrolledInfo.R) 
- qid format changed after a certain time, but there is a crosswalk file linking qids. According to Ben, QIDs in raw dataset have been converted to one single formmat
- still, some qids weren't matched. We excluded those problemetic qids based on the .csv file Ben provided.
- export `EnrolledInfo.csv`, only containing `QID` and `firstADRDyr`(first hospitalization admission year with ADRD), one-row-per-person

### extract denominator info for ADRD people in Medicare
[03extractDenominatorFile.R](https://github.com/ShuxinD/airPollution_ADRD/blob/main/codes/00generate_data/03extractDenominatorFile.R) 
- donominator files were prepared by research group before, should contain all Medicare enrollee's demographic info
- subset the denominator files to those with `QID` which appeared in `EnrolledInfo.csv`
- subset the person-year data to those starting from `firstADRDyr ` (`year>=firstADRDyr`) based on `EnrolledInfo.csv` for each subject
- remove duplicates, export `ADRDpeople_denom.fst`

### merge air pollution by ZIP code and calendar year
[04mergeExposure.R](https://github.com/ShuxinD/airPollution_ADRD/blob/main/codes/00generate_data/04mergeExposure.R) 
- PM2.5, NO2, ozone, summer ozone
- merge by calendar year and zip code
- calculate Ox
- export `ADRDcohort.fst` (contains all the information starting from firstADRDyr, though we don't need `year==firstADRDyr` rows in survival analysis itself, we do need it for correcting index event bias, should contains all the info we need in the future analysis)

### check the completeness of follow-up
[05checkFollowupCompleteness.md](https://github.com/ShuxinD/airPollution_ADRD/blob/main/codes/00generate_data/05checkFollowupCompleteness.md)
- follow-up data should start from the next year of first hospitalization with ADRD code, ends at the event/loss-of-follow-up/end of study period
- had considered to fill in all the missing person years, but without denominator information it will be very hard. After checking the death risk, we decided to omit those without complete follow-up
- exported `omitInfo.csv` with all qids that should be dropped

### clean data
[06cleanData.R](https://github.com/ShuxinD/airPollution_ADRD/blob/main/code/00generate_data/06cleanData.R)
- when correcting the index event bias, we need the exposure info when people entering the cohort.
	- the earlist exposure info starts from 2000, then the earlist year of `firstADRDyr` should be 2000, and the earlist year of starting follow-up should be 2001
- clean `ADRDcohort.fst`: remove NAs; remove those without complete follow-up based on `omitInfo.csv`
- add necessary variables into the dataset: `entry_age_break`, `race_collapsed`, `region`, convert variables to the right class
- export `ADRDcohort_clean.fst`

### prepare datasets for event analysis (death and readmission)
[07generateReAd.R](https://github.com/ShuxinD/airPollution_ADRD/blob/main/codes/00generate_data/07generateReAd.R)
- `ADRDcohort_clean.fst` is ready to be used for time-to-death analysis
- we defined "readmission" as the second admission of ADRD people after their first hospitalization with ADRD, and the second hospitalization doesn't have to contain ADRD billing code. This is for a better capture of "how sick" ADRD people are, also once people diagnosed with ADRD, they will always have that disease, no matter it was billed or not. When people is too sick, the doctor may not bother billing ADRD instead of other fatal diseases (such as organ failure)
- instead of extracting all hospitalization records with ADRD code as in [01extractADRDhospital.R](https://github.com/ShuxinD/airPollution_ADRD/blob/main/codes/00generate_data/01extractADRDhospital.R), we extracted all hospitalization records for ADRD people based on qid in `EnrolledInfo.csv`.
- subset the hospitalization records for each ADRD patient to the records after their first ADRD hospitalizatoin, and the minimum of admission year would be the `firstRdADyr`
- export `ReAdissionInfo.csv`, only containing `QID` and `firstReAdyr`(second hospitalization admission year), one-row-per-person
- subset `ADRDcohort_clean.fst` based on `ReAdmissionInfo.csv` for time-to-readmission analysis
