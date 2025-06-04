library(tidycensus)
library(tidyverse)
library(tmap)
library(psych)
library(sf)


# Basic decennial Census data retrieval -----------------------------------

# loads variable table for 2010 decennial census and assigns it to object named
# "decennial_variables_2010"



# prints "decennial_variables_2010"



# brings up "decennial_variables_2010" in data Viewer
View(decennial_variables_2010)

# Extracts a state-level population dataset, based on the 2010 decennial census


# prints "state_population_2010"


# views "state_population_2010" in data viewer

# extracts 2020 Decennial variables


# prints "decennial_variables_2020"



# Extracts a state-level population dataset, based on the 2020 decennial census

# prints "state_population_2020"


# views "state_population_2020" in data viewer

# Extracts a dataset with information on population by county in Colorado, and 
# assigns it to an object named "CO_county_population_2010"



# View "CO_county_population_2010" in viewer

# Extract dataset with information on 2010 population by tract in Boulder County, CO


# prints "boulder_tracts_2010"



# opens "boulder_tracts_2010" in Viewer

# Use tidycensus to extract county level data from Colorado
# population (P001001) and rural population (P002005) 
# based on 2000 decennial census; assigns data to object 
# named "population_rural_CO_county"



# prints "population_rural_CO_county"



# Views "population_rural_CO_county" in data viewer

# Practice Exercise 1: Generate a dataset from the 2010 decennial census that contains 
# information on the total number of mixed-race individuals (i.e. individuals identifying with two or more
# racial categories), and the total population, within census tracts in Cook County, 
# Illinois 




# Preparing and processing decennial Census datasets -----------------------

# Takes "population_rural_CO_county", deletes the "variable" column, 
# renames the "value" column as "population", and assigns this modified dataset 
# to a new object named "population_rural_CO_county_processed"



# Views "CO_county_population_2010_processed" in viewer

# sorts "population_rural_CO_county_processed" in ascending order with respect to
# total_population



# views "population_rural_CO_county_processed_popAscending" in data Viewer

# sorts "population_rural_CO_county_processed" in descending order with respect to
# total_population


# changes order of columns with "relocate()" function


# subsetting data with "filter()" function; extracts rows with total_population>10000
# and assigns subsetted dataset to new object named "population_rural_CO_county_large"


# views "population_rural_CO_county_large" in data viewer

# subsetting "population_rural_CO_county_large" based on strings; extracts
# Denver and Boulder County observations



# uses "slice()" function to extract observation with highest value for "total_population"
# in "CO_county_population_2010_processed" 

# uses "slice()" function to extract observation with 5 highest values for "total_population"
# in "CO_county_population_2010_processed" 

# uses "slice()" function to extract observation with 8 lowest values for "rural_population"
# in "CO_county_population_2010_processed" 

# uses "mutate()" function to create and populate new column in "population_rural_CO_county_processed" 
# named "rural_pct"


# Take the "population_rural_CO_county_processed" dataset, create a new 
# variable named "County" that takes the information in the "NAME" column 
# and removes the part of the string that contains "County, Colorado", 
# and then removes the now superfluous "NAME" column


# relocates "County" column to the front of the "population_rural_CO_county_processed" dataset, 
#just after GEOID


# Practice Exercise 2: Take the dataset you extracted in Exercise 1, and 
# implement the following:
# A) rename the "P001001" column to "total_population" and "P008009" to "mixed_race_population"
# B) Create a new column labeled "mixed_race_pct" that contains the percentage of the total tract population
#    that is mixed race
# C) Subset the data to include the observations with the 15 highest values of "mixed_race_pct"
# D) Assign the modified dataset to a new object named "cook_county_mixed_race_pct_15" and view it in the 
# data viewer



# Summary Statistics ------------------------------------------------------

# extracts a county-level dataset of total and urban population for the entire
# USA, based on the 2010 decennial Census, and assigns it to a new object
# named "usa_counties_total_urban_2010"


# views "usa_counties_total_urban_2010" in viewer

# generates table of county-level summary statistics for "total_population" and
# "urban_population" across the USA



# views "usa_counties_summary" in data viewer

# separates "NAME" column in "usa_counties_total_urban_2010"
# into "County" and "State" columns 


# Generates county-level summary statistics for "total_population" and "urban_population" 
# within each state, using "usa_counties_total_urban_2010"


# Practice Exercise 3: Generate a dataset with the tract-level mean and median Hispanic-origin 
# population for each county in Delaware, based on the 2010 Census.

# extracts data on tract-level Hispanic-origin population in the state of DE


# separates NAMR field into separate columns


# generates table of summary statistics



# Decennial Census data visualization -------------------------------------

# extracts county-level data on total population, Hispanic origin population, and median age from 
# 2010 Decennial census for the state of Connecticut



# creates separate "County" field with county names



# creates bar chart of Hispanic share of the population in CT counties



# inverts coordinate axis of "ct_hispanicl_viz" and assigns the result to a new object
# named "ct_hispanicl_viz_coord_flip"



# creates a visualization of the median age, by county, in CT


# creates map of county-level variation in share of population with Hispanic 
# origins using tmap package


# changes tmap mode to interactive view mode

# creates interactive map

# switches back to "plot" mode

# reprints static map

# # Creates a state-level map showing the percentage of the population identifying as mixed race relative 
# to the state population; first, extracts data with "get_decennial()"


# fixes geometries

# creates USA state-level map of mixed-race population


# Practice Exercise 4: Create an interactive county-level map of the United States showing the 
# percentage of the population identifiying as mixed race in each county, based on the 2010 decennial


# extracts county-level data on the share of the mixed race population based on the 2010 decennial


# creates map of "usa_2010_mixed_race_county" data


# shifts to interactive mode

# makes interactive map


# Practice Exercise 5: Using data from the 2010 decennial Census, create a dataset that calculates the 
# percentage of each U.S. county’s population made up of individuals aged 18 to 22 (the traditional college-age range).
# Then, identify the 10 counties with the highest percentage of 18–22 year-olds relative to their total population.
# Finally, create a bar chart showing those top 10 counties and their corresponding percentages.

# extracts data on college age population, and total population, by county



# adds up individual age demographics from 18-22 for males and females, and uses this 
# sum to make new "college_age" variable



# Creates a new variable, "college_age_pct", in "usa_2010_counties_college_age" that 
# represents the percentage of the total county population that is college age


# extracts observations with ten highest values of "college_age_pct"


# creates bar chart based on "usa_2010_counties_college_age_top10"


# American Community Survey data ------------------------------------------

# loads variables for 5-year ACS ending in 2018

# Views "ACS_5_2019"

# loads variables for 5-year ACS ending in 2015

# loads variables for the 1-year acs ending in 2017

# views "ACS_1_2017"

# loads variables for 3-year ACS ending in 2012

# Views "ACS_3_2012"

# Extracts county-level dataset of median income from the 5-year ACS ending in 2018,
# for state of Colorado


# Views "median_income_2018_CO"

# creates field with County name in "median_income_2018_CO"



# creates visualization, with eror bars, of "median_income_2018_CO"


# extracts highest income county from each state based on the 2019 5-year ACS; first, extracts
# county-level median income data for the US as a whole


# subsets "median_income_USA_counties" to extract highest income county from each state


# Views "highest_income_counties"

# makes visualization to display highest income county in each state



# prints "highest_income_counties_viz"

# makes simply county-level map of median income based on "median_income_USA_counties"


# sets tmap to interactive mode

# creates interactive map

# shifts tmap back to static plot mode

# Practice Exercise 6: Extract county-level median income data based on the 2019 ACS for a state other than Colorado,
# and generate a visualization that displays this information. 


# Exporting R Objects -----------------------------------------------------

# exports "median_income_2018_CO" as CSV file to working directory

# exports "highest_income_counties_viz" visualization as PNG file to working directory

# exports "national_mixed_race_map" as png file to working directory


# Exercise 7: Select one dataset, one ggplot visualization, and one map that you've created in this
# tutorial to export to disk







