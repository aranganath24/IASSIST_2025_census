library(tidycensus)
library(tidyverse)
library(tmap)
library(psych)
library(sf)


# Basic decennial Census data retrieval -----------------------------------

# loads variable table for 2010 decennial census and assigns it to object named
# "decennial_variables_2010"
decennial_variables_2010<-load_variables(year=2010, 
                                         dataset = "sf1")

# prints "decennial_variables_2010"
decennial_variables_2010

# brings up "decennial_variables_2010" in data Viewer
View(decennial_variables_2010)

# Extracts a state-level population dataset, based on the 2010 decennial census
state_population_2010<-get_decennial(geography = "state", 
                                     variables = "P001001", 
                                     year = 2010)


# prints "state_population_2010"
state_population_2010

# views "state_population_2010" in data viewer
View(state_population_2010)

# extracts 2020 Decennial variables
decennial_variables_2020<-load_variables(year = 2020, 
                                         dataset = "pl")

# prints "decennial_variables_2020"
decennial_variables_2020

# Extracts a state-level population dataset, based on the 2020 decennial census
state_population_2020<-get_decennial(geography = "state", 
                                     variables = "P1_001N",
                                     year = 2020)

# prints "state_population_2020"
state_population_2020

# views "state_population_2020" in data viewer
View(state_population_2020)

# Extracts a dataset with information on population by county in Colorado, and 
# assigns it to an object named "CO_county_population_2010"
CO_county_population_2010<-get_decennial(geography = "county", 
                                         state="CO",
                                         variables = "P001001", 
                                         year = 2010)

# View "CO_county_population_2010" in viewer
View(CO_county_population_2010)

# Extract dataset with information on 2010 population by tract in Boulder County, CO
boulder_tracts_2010<-get_decennial(geography = "tract", 
                                   state="CO",
                                   county="Boulder",  
                                   variables = "P001001", 
                                   year = 2010)


# prints "boulder_tracts_2010"
boulder_tracts_2010

# opens "boulder_tracts_2010" in Viewer
View(boulder_tracts_2010)

# Use tidycensus to extract tract-level data from Colorado
# population (P001001) and rural population (P002005) 
# based on 2000 decennial census; assigns data to object 
# named "population_rural_CO_county"
population_rural_CO_county<-get_decennial(geography = "county",
                                          state="CO",
                                          variables = c("P001001", "P002005"),
                                          output="wide",
                                          year = 2000) 

# prints "population_rural_CO_county"
population_rural_CO_county

# Views "population_rural_CO_county" in data viewer
View(population_rural_CO_county)

# Practice Exercise 1: Generate a dataset from the 2010 decennial census that contains 
# information on the total number of mixed-race individuals (i.e. individuals identifying with two or more
# racial categories), and the total population, within census tracts in Cook County, 
# Illinois 

mixed_race_cook_county<-get_decennial(geography = "tract", 
                                      state="IL",
                                      county="Cook",  
                                      variables = c("P001001", "P0030008"),
                                      output="wide",
                                      year = 2010)



# Preparing and processing decennial Census datasets -----------------------

# Takes "population_rural_CO_county", deletes the "variable" column, 
# renames the "value" column as "population", and assigns this modified dataset 
# to a new object named "population_rural_CO_county_processed"
population_rural_CO_county_processed<-population_rural_CO_county %>% 
                                    rename(total_population=P001001,
                                           rural_population=P002005)


# Views "population_rural_CO_county_processed" in viewer
View(population_rural_CO_county_processed) 

# sorts "population_rural_CO_county_processed" in ascending order with respect to
# total_population
population_rural_CO_county_processed_popAscending<-
  population_rural_CO_county_processed %>% 
  arrange(total_population)

# views "population_rural_CO_county_processed_popAscending" in data Viewer
View(population_rural_CO_county_processed_popAscending)

# sorts "population_rural_CO_county_processed" in descending order with respect to
# total_population
population_rural_CO_county_processed %>% 
arrange(desc(total_population))
  
# changes order of columns with "relocate()" function
population_rural_CO_county_processed %>% 
  relocate(total_population,.after=rural_population)

# subsetting data with "filter()" function; extracts rows with total_population>10000
# and assigns subsetted dataset to new object named "population_rural_CO_county_large"
population_rural_CO_county_large<-population_rural_CO_county_processed %>% 
                                    filter(total_population>10000)

# views "population_rural_CO_county_large" in data viewer
View(population_rural_CO_county_large)

# subsetting "population_rural_CO_county_large" based on strings; extracts
# Denver and Boulder County observations 
population_rural_CO_county_large %>% 
  filter(NAME=="Denver County, Colorado"|NAME=="Boulder County, Colorado")

# uses "slice()" function to extract observation with highest value for "total_population"
# in "CO_county_population_2010_processed" 
population_rural_CO_county_processed %>% slice_max(total_population)

# uses "slice()" function to extract observation with 5 highest values for "total_population"
# in "CO_county_population_2010_processed" 
population_rural_CO_county_processed %>% slice_max(n=5, order_by = total_population)

# uses "slice()" function to extract observation with 8 lowest values for "rural_population"
# in "CO_county_population_2010_processed" 
population_rural_CO_county_processed %>% slice_min(n=8, order_by = rural_population)

# uses "mutate()" function to create and populate new column in "population_rural_CO_county_processed" 
# named "rural_pct"
population_rural_CO_county_processed<-
  population_rural_CO_county_processed %>% 
    mutate(rural_pct=(rural_population/total_population)*100)

# Take the "population_rural_CO_county_processed" dataset, create a new 
# variable named "County" that takes the information in the "NAME" column 
# and removes the part of the string that contains "County, Colorado", 
# and then removes the now superfluous "NAME" column
population_rural_CO_county_processed<-
  population_rural_CO_county_processed %>% 
  mutate(County=str_remove(NAME, " County, Colorado")) %>% 
  select(-NAME)

# relocates "County" column to the front of the "population_rural_CO_county_processed" dataset, 
#just after GEOID
population_rural_CO_county_processed<-
  population_rural_CO_county_processed %>% 
  relocate(County, .after = GEOID)

# Practice Exercise 2: Take the dataset you extracted in Exercise 1, and 
# implement the following:
# A) rename the "P001001" column to "total_population" and "P008009" to "mixed_race_population"
# B) Create a new column labeled "mixed_race_pct" that contains the percentage of the total tract population
#    that is mixed race
# C) Subset the data to include the observations with the 15 highest values of "mixed_race_pct"
# D) Assign the modified dataset to a new object named "cook_county_mixed_race_pct_15" and view it in the 
# data viewer

# suggested solution to Exercise 2:
cook_county_mixed_race_pct_15<-
  mixed_race_cook_county %>% 
  rename(total_population=P001001,
         mixed_race_population=P008009) %>% 
  mutate(mixed_race_pct=(mixed_race_population/total_population)*100) %>% 
  slice_max(n=15, order_by=mixed_race_pct)

# views "cook_county_mixed_race_pct_15" in Viewer
View(cook_county_mixed_race_pct_15)


# Summary Statistics ------------------------------------------------------

# extracts a county-level dataset of total and urban population for the entire
# USA, based on the 2010 decennial Census, and assigns it to a new object
# named "usa_counties_total_urban_2010"
usa_counties_total_urban_2010<-get_decennial(geography = "county", 
                                            variables = c("P001001", "P002002"),
                                            output="wide",
                                            year = 2010) %>% 
                              rename(total_population=P001001, urban_population=P002002)

# views "usa_counties_total_urban_2010" in viewer
View(usa_counties_total_urban_2010)

# generates table of county-level summary statistics for "total_population" and
# "urban_population" across the USA
usa_counties_summary<-usa_counties_total_urban_2010 %>% 
                        select(total_population, urban_population) %>% 
                        describe() %>% 
                        as_tibble(rownames="variable")


# views "usa_counties_summary" in data viewer
View(usa_counties_summary)

# separates "NAME" column in "usa_counties_total_urban_2010"
# into "County" and "State" columns 
usa_counties_total_urban_2010<-
  usa_counties_total_urban_2010 %>% 
  separate(NAME, into=c("County", "State"), sep=", ")

# Generates county-level summary statistics for "total_population" and "urban_population" 
# within each state, using "usa_counties_total_urban_2010"
group_summary<-
  usa_counties_total_urban_2010 %>% 
    group_by(State) %>% 
      summarise(number_counties=n(),
                county_mean_pop=mean(total_population),
                county_median_pop=median(total_population),
                county_mean_urban_pop=mean(urban_population),
                county_median_urban_pop=median(urban_population))

# Practice Exercise 3: Generate a dataset with the tract-level mean and median Hispanic-origin 
# population for each county in Delaware, based on the 2010 Census.

# extracts data on tract-level Hispanic-origin population in the state of DE
delaware_hispanic_tract<-get_decennial(geography = "tract", 
                                       variables = "P004003",
                                       state="DE",
                                       output = "wide",
                                       year = 2010) %>% 
                        rename(hispanic_origin=P004003)

# separates NAMR field into separate columns
delaware_hispanic_tract<-delaware_hispanic_tract %>% 
                            separate(NAME, into=c("Tract", "County", "State"), sep=",")

# generates table of summary statistics
group_summary_delaware<-delaware_hispanic_tract %>% 
  group_by(County) %>% 
  summarise(number_tracts=n(),
            tract_mean_pop=mean(hispanic_origin),
            tract_median_pop=median(hispanic_origin))


# Decennial Census data visualization -------------------------------------

# extracts county-level data on total population, Hispanic origin population, and median age from 
# 2010 Decennial census for the state of Connecticut
connecticut_2010<-get_decennial(geography = "county", 
                                state="CT",
                                geometry = TRUE,
                                variables = c("P001001", "P004003", "P013001"),
                                output="wide",
                                year = 2010) %>% 
                 rename(total_population=P001001,
                        total_hispanic=P004003,
                        median_age=P013001) %>% 
              mutate(hispanic_pct=(total_hispanic/total_population)*100) %>% 
              st_transform(crs = 4326)


# creates separate "County" field with county names
connecticut_2010<-connecticut_2010 %>% 
                    mutate(County=str_remove(NAME, " County, Connecticut")) %>% 
                    select(-NAME)

# creates bar chart of Hispanic share of the population in CT counties
ct_hispanic_viz<-
  ggplot(connecticut_2010)+
  geom_col(aes(x=reorder(County, hispanic_pct), y=hispanic_pct))+
  labs(
    title="Hispanic Origin Share of the Population in Connecticut Counties",
    x="County Name", 
    y="Hispanic Percentage")+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90),
        axis.title.x=element_text(margin=margin(t=20)))

# inverts coordinate axis of "ct_hispanicl_viz" and assigns the result to a new object
# named "ct_hispanicl_viz_coord_flip"
ct_hispanic_viz_coord_flip<-ct_hispanic_viz+
                              coord_flip()


# creates a visualization of the median age, by county, in CT
median_age_CT__visualization<-
  ggplot(connecticut_2010)+
    geom_point(aes(x=median_age, y=reorder(County, median_age)))+
      labs(title="Median Age by County, CT", x="Median Age", y="County", caption="Source: United States census accessed via tidycensus")+
      theme(plot.title=element_text(hjust=0.5),
            plot.caption=element_text(size=5))
  
# creates map of county-level variation in share of population with Hispanic 
# origins using tmap package
hispanic_pct_CT_map<-
  tm_shape(connecticut_2010)+
  tm_polygons(col="hispanic_pct",
              style="jenks",
              palette="YlGnBu",
              title="Percent Hispanic")+
  tm_layout(frame=FALSE, 
            main.title="Median Age by County,\nConnecticut",  
            main.title.position="left", 
            main.title.size=1,
            legend.outside=TRUE,
            attr.outside=TRUE)+
  tm_credits("Source: US Census via tidycensus", position=c("right", "bottom"))

# changes tmap mode to interactive view mode
tmap_mode("view")

# creates interactive map
hispanic_pct_CT_map

# switches back to "plot" mode
tmap_mode("plot")

# reprints static map
hispanic_pct_CT_map

# # Creates a state-level map showing the percentage of the population identifying as mixed race relative 
# to the state population; first, extracts data with "get_decennial()"
usa_2010_mixed_race<-get_decennial(geography = "state",
                                    geometry = TRUE,
                                    shift_geo = TRUE,
                                    variables = c("P001001", "P008009"),
                                    output="wide",
                                    year = 2010) %>% 
                    rename(total_population=P001001,
                          mixed_race=P008009) %>% 
                    mutate(mixed_race_pct=(mixed_race/total_population)*100) %>% 
                    st_transform(crs = 4326)

# fixes geometries
usa_2010_mixed_race <- st_make_valid(usa_2010_mixed_race)

# creates USA state-level map of mixed-race population
national_mixed_race_map<-
  tm_shape(usa_2010_mixed_race)+
  tm_polygons(col="mixed_race_pct", 
              style="jenks",
              palette="YlOrBr",
              title="Percentage of Mixed-Race Individuals")+
  tm_layout(frame=FALSE, 
            main.title="Mixed Race Population as a Share of the Overall State Population, 2010",
            main.title.size=0.9,
            main.title.position="center", 
            legend.outside=TRUE, 
            legend.title.size=1,
            legend.outside.position = c("bottom"),
            attr.outside=T)+
  tm_credits("Source: US Census via tidycensus", position=c("right", "bottom"))

# Practice Exercise 4: Create an interactive county-level map of the United States showing the 
# percentage of the population identifying as mixed race in each county, based on the 2010 decennial


# extracts county-level data on the share of the mixed race population based on the 2010 decennial
usa_2010_mixed_race_county<-get_decennial(geography = "county",
                                   geometry = TRUE,
                                   shift_geo = FALSE,
                                   variables = c("P001001", "P008009"),
                                   output="wide",
                                   year = 2010) %>% 
                        rename(total_population=P001001,
                               mixed_race=P008009) %>% 
                        mutate(mixed_race_pct=(mixed_race/total_population)*100) %>% 
                        st_transform(crs = 4326) %>% 
                        st_make_valid()

# creates map of "usa_2010_mixed_race_county" data
national_mixed_race_county_map<-
  tm_shape(usa_2010_mixed_race_county)+
  tm_polygons(col="mixed_race_pct", 
              style="fisher",
              palette="YlOrBr",
              title="Percentage of Mixed-Race Individuals")+
  tm_layout(frame=FALSE, 
            main.title="Mixed Race Population as a Share of the Overall County Population, 2010",
            main.title.size=0.9,
            main.title.position="center", 
            legend.outside=TRUE, 
            legend.title.size=1,
            legend.outside.position = c("bottom"),
            attr.outside=T)+
  tm_credits("Source: US Census via tidycensus", position=c("right", "bottom"))

# shifts to interactive mode
tmap_mode("view")

# makes interactive map
national_mixed_race_county_map

tmap_mode("plot")

# Practice Exercise 5: Using data from the 2010 decennial Census, create a dataset that calculates the 
# percentage of each U.S. county’s population made up of individuals aged 18 to 22 (the traditional college-age range).
# Then, identify the 10 counties with the highest percentage of 18–22 year-olds relative to their total population.
# Finally, create a bar chart showing those top 10 counties and their corresponding percentages.

# extracts data on college age population, and total population, by county
usa_2010_counties_college_age<-get_decennial(geography = "county",
                                          geometry = FALSE,
                                          variables = c("PCT012001", 
                                                        "PCT012021", 
                                                        "PCT012022", 
                                                        "PCT012023", 
                                                        "PCT012024", 
                                                        "PCT012025",
                                                        "PCT012125", 
                                                        "PCT012126", 
                                                        "PCT012127",
                                                        "PCT012128", 
                                                        "PCT012129"),
                                          output="wide",
                                          year = 2010)


# adds up individual age demographics from 18-22 for males and females, and uses this 
# sum to make new "college_age" variable
usa_2010_counties_college_age <- 
  usa_2010_counties_college_age %>%
    rowwise() %>%
     mutate(college_age= sum(c_across(PCT012021:PCT012129), na.rm = TRUE)) %>%
     ungroup()


# Creates a new variable, "college_age_pct", in "usa_2010_counties_college_age" that 
# represents the percentage of the total county population that is college age
usa_2010_counties_college_age<-usa_2010_counties_college_age %>% 
                                mutate(college_age_pct=(college_age/PCT012001)*100)


# extracts observations with ten highest values of "college_age_pct"
usa_2010_counties_college_age_top10<-usa_2010_counties_college_age %>% 
                                      slice_max(n=10, order_by=college_age_pct)

# creates bar chart based on "usa_2010_counties_college_age_top10"
usa_2010_counties_college_age_top10_plot<-
  ggplot(usa_2010_counties_college_age_top10)+
    geom_col(aes(x=reorder(NAME, college_age_pct), y=college_age_pct))+
    coord_flip()+
    labs(
    title="US Counties with Largest Concentrations of College-Age Students (18-22)",
    x="County Name", 
    y="College Age Percentage")+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(angle = 0),
        axis.title.x=element_text(margin=margin(t=20)))


# American Community Survey data ------------------------------------------

# loads variables for 5-year ACS ending in 2018
ACS_5_2018<-load_variables(2018,"acs5")

# Views "ACS_5_2019"
View(ACS_5_2019)

# loads variables for 5-year ACS ending in 2015
ACS_5_2015<-load_variables(2015,"acs5")

# loads variables for the 1-year acs ending in 2017
ACS_1_2017<-load_variables(2017, "acs1")

# views "ACS_1_2017"
View(ACS_1_2017)

# loads variables for 3-year ACS ending in 2012
ACS_3_2012<-load_variables(2012, "acs3")

# Views "ACS_3_2012"
ACS_3_2012

# Extracts county-level dataset of median income from the 5-year ACS ending in 2018,
# for state of Colorado
median_income_2018_CO<-get_acs(geography="county",
                            variables="B19013_001",
                            state="CO",
                            year=2018) %>% 
                    rename(median_income=estimate) %>% 
                    arrange(desc(median_income))

# Views "median_income_2018_CO"
View(median_income_2018)

# creates field with County name in "median_income_2018_CO"
median_income_2018_CO<-
  median_income_2018_CO %>% 
  mutate(County=str_remove_all(NAME, "County, Colorado"))


# creates visualization, with eror bars, of "median_income_2018_CO"
CO_median_income_visualization<-
  ggplot(median_income_2018_CO, aes(x=median_income, y=reorder(County, median_income)))+
    geom_point(color="blue",size=3)+
    geom_errorbarh(aes(xmin=median_income-moe, 
                       xmax=median_income+moe))+
    labs(title="Median Income in Colorado, by County (2018)",
          y="", 
          x="Median Income Estimate from 5 year ACS\n(Bars indicate margin of error)")+
    theme(plot.title=element_text(hjust=0.5))

# extracts highest income county from each state based on the 2019 5-year ACS; first, extracts
# county-level median income data for the US as a whole
median_income_USA_counties<-get_acs(geography="county",
                                    variables="B19013_001",
                                    year=2019,
                                    geometry = TRUE) %>% 
                           rename(median_income=estimate) %>% 
                           arrange(desc(median_income))

# subsets "median_income_USA_counties" to extract highest income county from each state
highest_income_counties<-median_income_USA_counties %>% 
                          separate(NAME,c("County","State"),sep=",") %>% 
                          group_by(State) %>% 
                          arrange(desc(median_income)) %>% 
                          slice_max(n=1, order_by=median_income) %>% 
                          unite(NAME, c("County","State"), remove=FALSE, sep=",")

# Views "highest_income_counties"
View(highest_income_counties) 

# makes visualization to display highest income county in each state
highest_income_counties_viz<-
  ggplot(highest_income_counties, aes(x=median_income,y=reorder(NAME, median_income)))+
    geom_errorbarh(aes(xmin = median_income - moe, xmax = median_income + moe)) +
    geom_point(color = "red", 
             size = 3)+
    labs(title="County with Highest Median Income, by State",
       y="",
       x="Median Income Estimate from 5-year ACS\n(bars indicate margin of error)")+ 
  theme(plot.title=element_text(hjust=0.5))


# prints "highest_income_counties_viz"
highest_income_counties_viz

# makes simply county-level map of median income based on "median_income_USA_counties"
median_income_map<-tm_shape(median_income_USA_counties)+
                    tm_polygons(col="median_income", n=6, style="fisher", palette="YlOrBr")

# sets tmap to interactive mode
tmap_mode("view")

# creates interactive map
median_income_map

# shifts tmap back to static plot mode
tmap_mode("plot")

# Practice Exercise 6: Extract county-level median income data based on the 2019 ACS for a state other than Colorado,
# and generate a visualization that displays this information. 


# Exporting R Objects -----------------------------------------------------

# exports "median_income_2018_CO" as CSV file to working directory
write_csv(median_income_2018_CO, "median_income_2018_CO.csv")

# exports "highest_income_counties_viz" visualization as PNG file to working directory
ggsave("highest_income_counties_viz.png", highest_income_counties_viz, width=10, height=10)

# exports "national_mixed_race_map" as png file to working directory
tmap_save(national_mixed_race_map, "mixed_race_map.png", width=7, height=7)


# Exercise 7: Select one dataset, one ggplot visualization, and one map that you've created in this
# tutorial to export to disk







