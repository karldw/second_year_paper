

# Health Data

## Medical Expenditure Panel Survey (MEPS)

The data could be useful, but the ones I want are restricted.

### Component event files
|||
|------|-----|
|Restricted? | Yes.
|Unit of observation| Medical event
|Time specificity| Date
|Location specificity| County

### Medical provider component files
|||
|------|-----|
|Restricted? | Yes.
|Unit of observation| Medical payment by paying source
|Time specificity| Date
|Location specificity|

## National Ambulatory Medical Care Survey (NAMCS and NHAMCS)
Not usable. The data are physician-patient encounters (which seems great), but don't have the geographic coverage I need.
Pre-2012, sampling was only valid in four sub-national regions.
Post-2012, the 34 most populous states can be identified. ()

##  National Hospital Discharge Survey (NHDS)
No good, just combining NHAMCS with data I don't need.

## National Hospital Care Survey (NHCS)
No good, just a new name for NHDS.

## National Health Interview Survey (NHIS)
No expenditure data.

## Medicare / Medicaid (CMS)
Only annual data are available before 2015. The data are expensive and limited-use.
https://www.cms.gov/Research-Statistics-Data-and-Systems/Files-for-Order/LimitedDataSets/DenominatorLDS.html
- Part B Summary Data
- Basic Stand Alone (BSA) Medicare Claims Public Use Files
- Health Care Information System (HCIS) Data File
- Provider of Services Current Files
- Ambulatory Surgical Center (ASC) Payment System
- Denominator File


# State and County Data
See `Code/download_state_data.r`
## County-by-year population
- 2000-2010 data:
http://www.census.gov/popest/data/intercensal/county/county2010.html
- 2010-2015 data (2015 vintage)
http://www.census.gov/popest/data/counties/totals/2015/index.html

## State GDP
- http://www.bea.gov/regional/zip/gsp/gsp_naics_all_PC.zip (1997 and onward)
- http://www.bea.gov/regional/zip/gsp/gsp_sic_all_PC.zip (before 1997)
