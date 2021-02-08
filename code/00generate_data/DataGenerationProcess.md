# Process to generate ADRD study population

## Extract ADRD admission info from hospitalization files

[`01extractADRDhospital.R`](https://github.com/ShuxinD/airPollution_ADRD/blob/main/code/00generate_data/01extractADRDhospital.R)

- Select ADRD diagnosis based on ICD-9 and ICD-10 codes using in the paper [**Using Medicare claims in identifying Alzheimer’s disease and related dementias**](https://alz-journals.onlinelibrary.wiley.com/doi/10.1002/alz.12199); codes are modified from [a previous one](https://github.com/NSAPH/data_requests/blob/master/request_projects/jan2021_whanhee_fisrt_hosps/code/2_id_hospitalizations.R).
- Export several `.fst` files named as `ADRD'type'_'year'.fst`, e.g. `ADRDprimary_2000.fst`

## Generate ADRD study population ID and admission info

[`02getEnrolledInfo.R`](https://github.com/ShuxinD/airPollution_ADRD/blob/main/code/00generate_data/02getEnrolledInfo.R)

- Combine `.fst` files exported from the previous step, check duplicates
- Export `EnrolledInfo.csv`, only containing `QID` and `firstADRDyr`

## Extract the denominator files for ADRD population 

[`03extractDenominatorFile.R`](https://github.com/ShuxinD/airPollution_ADRD/blob/main/code/00generate_data/03extractDenominatorFile.R)

- Subset the denominator files to those with `QID` which appeared in `EnrolledInfo.csv`

- Subset the person-year row data to those after `firstADRDyr ` (`year>=firstADRDyr`), based on `EnrolledInfo.csv`, for each subject
- Remove duplicates, export `ADRDpeople_denom.csv`

## Merge exposure (NO2 and ozone)

[`04mergeExposure.R`](https://github.com/ShuxinD/airPollution_ADRD/blob/main/code/00generate_data/04mergeExposure.R)

- Merge NO2 and ozone into `ADRDpeople_denom.csv` by zipcode and year (admission year)
- Export `ADRDpeople.csv`

## Check the completeness of follow-up

[`05checkFollowupCompleteness.Rmd`](https://github.com/ShuxinD/airPollution_ADRD/blob/main/code/00generate_data/05checkFollowupCompleteness.Rmd)

[`05checkFollowupCompleteness.md`](https://github.com/ShuxinD/airPollution_ADRD/blob/main/code/00generate_data/05checkFollowupCompleteness.md)

## Clean data for next step analyses

[`06cleanData.R`]()

- Clean `ADRDpeople.csv`: remove NAs, remove those with discontinuous follow-up record.
- Export `ADRD_mortality.csv`

