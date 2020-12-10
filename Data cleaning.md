# Clean the 2000-2016 Medicare dataset

for the project of mortality and air pollution in Medicare ADRD population

## Availabel datasets overview and general idea of cleaning

Two datasets available to construct the final dataset:

- "hospital_total.rds" contains info of who should be enrolled in our cohort (who were hospitalized with ADRD diagnosis)

- "national_exp.fst" contains info of all the Medicare enrollees, including their basic socioeconomics status, air pollution merged by year of their admission, census data by zip code merged by zip code provided in the hospitalization record. *Note*: this dataset is a expanded one, the records for each subjects are **expected** to last till 2016 or their death

Our general idea is to:

- get the **QID** and **first ADRD year (firstADRDyr)** of those who should be enrolled in our cohort from "hospital_total.rds"
- subset the "national_exp.fst" based on **QID** we get from the previous step, merge the **firstADRDyr** by QID and drop the year before firstADRDyr.

Our final dataset should be:

- one row per person year
- for those who are alive till the end of study period (2016)