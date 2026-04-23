Virginia 2023 Select Poverty Data from the American Community Survey (2019-2023) at the County level

##Redistricting Data Hub (RDH) Retrieval Date
03/18/25

##Sources
ACS data retrieved using the Census API: https://api.census.gov/data/2023/acs/acs5

##Fields
Field Name Description                                                                                                                                                                 
GEOID      Unique Geographic Identifier                                                                                                                                                
STATEFP    State FIPS                                                                                                                                                                  
STATE      State Name                                                                                                                                                                  
COUNTYFP   County FIPS                                                                                                                                                                 
COUNTY     County Name                                                                                                                                                                 
TOT_HOUS23 Total households (B17010_001E)                                                                                                                                              
TOT_CHI23  Total households with related children under the age of 18 (sum of B17010_004E, B17010_011E, B17010_017E, B17010_024E, B17010_031E, and B17010_037E)                        
TOT_MAR23  Total married-couple households with related children under the age of 18 (sum of B17010_004E and B17010_024E)                                                              
TOT_MAL23  Total households with a male householder (no spouse present) with related children under the age of 18 (sum of B17010_011E and B17010_031E)                                 
TOT_FEM23  Total households with a female householder (no spouse present) with related children under the age of 18 (sum of B17010_017E and B17010_037E)                               
TOT_BPOV23 Total households with income in the past 12 months below poverty level (B17010_002E)                                                                                        
CHI_BPOV23 Total households with related children under the age of 18 with income in the past 12 months below poverty level (sum of B17010_004E, B17010_011E, and B17010_017E)         
MAR_BPOV23 Total married-couple households with related children under the age of 18 with income in the past 12 months below poverty level (B17010_004E)                               
MAL_BPOV23 Total households with a male householder (no spouse present) with related children under the age of 18 with income in the past 12 months below poverty level (B17010_011E)  
FEM_BPOV23 Total households with a female householder (no spouse present) with related children under the age of 18 with income in the past 12 months below poverty level (B17010_017E)
TOT_APOV23 Total households with income in the past 12 months above poverty level (B17010_022E)                                                                                        
CHI_APOV23 Total households with related children under the age of 18 with income in the past 12 months above poverty level (sum of B17010_024E, B17010_031E, and B17010_037E)         
MAR_APOV23 Total married-couple households with related children under the age of 18 with income in the past 12 months above poverty level l (B17010_024E)                             
MAL_APOV23 Total households with a male householder (no spouse present) with related children under the age of 18 with income in the past 12 months above poverty level (B17010_031E)  
FEM_APOV23 Total households with a female householder (no spouse present) with related children under the age of 18 with income in the past 12 months above poverty level(B17010_037E) 
TOT_CVAP23 Total Citizen Voting Age Population (CVAP) (B29003_001E)                                                                                                                    
BPV_CVAP23 Total CVAP with income in the past 12 months below poverty level (B29003_002E)                                                                                              
APV_CVAP23 Total CVAP with income in the past 12 months above poverty level (B29003_003E)                                                                                              

##Processing
ACS data for Virginia was retrieved with a Python script from the Census API.
The county data is available for all counties in Virginia. The script extracted the data for all counties in Virginia. 
Each field represents an estimate from the Census for a particular variable or sum of variables, as noted in the Fields section above.

##Additional Notes
For any questions about this dataset or if you would like additional related ACS data, please email info@redistrictingdatahub.org.