Virginia 2023 Select Race Data from the American Community Survey (2019-2023) at the County level

##Redistricting Data Hub (RDH) Retrieval Date
03/17/25

##Sources
ACS data retrieved using the Census API: https://api.census.gov/data/2023/acs/acs5

##Fields
Field Name Description                                                                         
GEOID      Unique Geographic Identifier                                                        
STATEFP    State FIPS                                                                          
STATE      State Name                                                                          
COUNTYFP   County FIPS                                                                         
COUNTY     County Name                                                                         
TOT_POP23  Total Population (sum of B03002_002E and B03002_012E)                               
NHSP_POP23 Total Population (Not Hispanic/Latino) (B03002_002E)                                
HSP_POP23  Total Population (Hispanic/Latino) (B03002_012E)                                    
WHT_NHSP23 White Alone (Not Hispanic/Latino) (B03002_003E)                                     
BLK_NHSP23 Black or African-American Alone (Not Hispanic/Latino) (B03002_004E)                 
AIA_NHSP23 American Indian and Alaska Native Alone (Not Hispanic/Latino) (B03002_005E)         
ASN_NHSP23 Asian Alone (Not Hispanic/Latino) (B03002_006E)                                     
HPI_NHSP23 Native Hawaiian and Other Pacific Islander Alone (Not Hispanic/Latino) (B03002_007E)
OTH_NHSP23 Some Other Race Alone (Not Hispanic/Latino) (B03002_008E)                           
2OM_NHSP23 Two or More Races (Not Hispanic/Latino) (B03002_009E)                               
BLK_ALL23  Black or African-American Alone or In Combination (B02009_001E)                     
AIA_ALL23  American Indian and Alaska Native Alone or In Combination (B02010_001E)             
ASN_ALL23  Asian Alone or In Combination (B02011_001E)                                         
HPI_ALL23  Native Hawaiian and Other Pacific Islander Alone or In Combination (B02012_001E)    
OTH_ALL23  Some Other Race Alone or In Combination (B02013_001E)                               

##Processing
ACS data for Virginia was retrieved with a Python script from the Census API.
The county data is available for all counties in Virginia. The script extracted the data for all counties in Virginia. 
Each field represents an estimate from the Census for a particular variable or sum of variables, as noted in the Fields section above.

##Additional Notes
For any questions about this dataset or if you would like additional related ACS data, please email info@redistrictingdatahub.org.