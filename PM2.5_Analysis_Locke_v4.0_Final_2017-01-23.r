# Download Packages
#install.packages("rJava")
#install.packages("xlsx")
#install.packages("xlsxjars")
#install.packages("choroplethr")
#install.packages("choroplethrMaps")
#install.packages("sqldf")
#install.packages("tidyverse")

# Install Libraries
library(xlsx)
library(choroplethr)
library(choroplethrMaps)
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(hexbin)
library(sqldf)


############################## Critical Functions ###################################################
# turn state abbreviations to long form
# Source: https://favorableoutcomes.wordpress.com/2012/10/19/create-an-r-function-to-convert-state-codes-to-full-state-name/
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TR",
                      "TX", "UT", "VA","VI", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","tribal","texas",
                     "utah","virginia","virgin islands","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
}


## Function for making State Chrorpleths without Abbreviations 
# Source: http://www.arilamstein.com/blog/2015/09/30/combining-choropleth-maps-and-reference-maps-in-r/

state_choropleth_no_abbvreviations <- function(df,title,legend_title,num_colors){
  c = StateChoropleth$new(df)
  c$title = title
  c$legend = legend_title
  c$set_num_colors(num_colors)
  c$set_zoom(NULL)
  c$show_labels = FALSE
  c$render()
}



######## Maximum Sector in each state ###############################

largest_sector_in_state <- function(df){
  result <- NULL
  max_emissions = 0
  max_sectors = NULL
  current_state = NULL
  for(i in 1:nrow(df)){
    row <- df[i,]
    if (is.null(current_state)){
      #    print("DID i GET to Branch A?")
      current_state <- row$state_abbr
      current_sector <- row$EI_sector 
      current_emissions <- row$emissions
    }
    if (current_emissions > max_emissions){
      max_emissions <- current_emissions
      max_sectors <- current_sector
    }
    else if (row$state_abbr != current_state){
      #    print("DID i GET to Branch B?")
      # Dump the current max out
      result = rbind(result, data.frame(current_state,max_sectors,max_emissions))
      # reset for the next state
      max_emissions = 0
      max_sectors = NULL
      # store the data
      current_state <- row$state_abbr
      current_sector <- row$EI_sector 
      current_emissions <- row$emissions
      if (current_emissions > max_emissions){
        max_emissions <- current_emissions
        max_sectors <- current_sector
      }
    }
    else{
      #    print("DID i GET to Branch C?")
      current_state <- row$state_abbr
      current_sector <- row$EI_sector 
      current_emissions <- row$emissions
      if (current_emissions > max_emissions){
        max_emissions <- current_emissions
        max_sectors <- current_sector
      }
    }
  }
  # This is needed to capture the last row 
  result = rbind(result, data.frame(current_state,max_sectors,max_emissions))
  return(result)
} 

largest_sector_in_county <- function(df){
  result <- NULL
  max_emissions = 0
  max_sectors = NULL
  current_fips = NULL
  for(i in 1:nrow(df)){
    row <- df[i,]
    if (is.null(current_fips)){
      #    print("DID i GET to Branch A?")
      current_fips <- row$fips
      current_sector <- row$EI_sector 
      current_emissions <- row$emissions
    }
    if (current_emissions > max_emissions){
      max_emissions <- current_emissions
      max_sectors <- current_sector
    }
    else if (row$fips != current_fips){
      #    print("DID i GET to Branch B?")
      # Dump the current max out
      result = rbind(result, data.frame(current_fips,max_sectors,max_emissions))
      # reset for the next state
      max_emissions = 0
      max_sectors = NULL
      # store the data
      current_fips <- row$fips
      current_sector <- row$EI_sector 
      current_emissions <- row$emissions
      if (current_emissions > max_emissions){
        max_emissions <- current_emissions
        max_sectors <- current_sector
      }
    }
    else{
      #    print("DID i GET to Branch C?")
      current_fips <- row$fips
      current_sector <- row$EI_sector 
      current_emissions <- row$emissions
      if (current_emissions > max_emissions){
        max_emissions <- current_emissions
        max_sectors <- current_sector
      }
    }
  }
  # This is needed to capture the last row 
  result = rbind(result, data.frame(current_fips,max_sectors,max_emissions))
  return(result)
} 

# Function Removes any infinities that might be present (due to use of Log())
inf2NA <- function(x) { x[is.infinite(x)] <- NA; x }

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
################################# Beg - Primary  Data Cleaning and Manipulations ###################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

###### US Emissions Detail ###### download data
us_emissions_detail_2014 <- read.csv("C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\data\\pm2.5_2014_sectors.csv")
us_emissions_detail_2011 <- read.csv("C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\data\\pm2.5_2011_sectors.csv")
us_emissions_detail_2008 <- read.csv("C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\data\\pm2.5_2008_sectors.csv")
# Bind the dataframes together
us_emissions_detail_all_years <- rbind(us_emissions_detail_2014,us_emissions_detail_2011,us_emissions_detail_2008)


############ Aggregate by County  Emissions ########
us_county_emissions_2008 <- aggregate(emissions ~ fips,data=us_emissions_detail_2008, FUN=sum)
us_county_emissions_2011 <- aggregate(emissions ~ fips,data=us_emissions_detail_2011, FUN=sum)
us_county_emissions_2014 <- aggregate(emissions ~ fips,data=us_emissions_detail_2014, FUN=sum)
# Merge County emissions into a dataframe and export
us_county_emissions_all_years <- merge(us_county_emissions_2008,us_county_emissions_2011, by= "fips")
us_county_emissions_all_years <- merge(us_county_emissions_all_years,us_county_emissions_2014, by= "fips")
names(us_county_emissions_all_years) <- c("fips","Yr_2008","Yr_2011","Yr_2014")
us_county_emissions_all_years$Total <- (us_county_emissions_all_years$Yr_2008 + us_county_emissions_all_years$Yr_2011 + us_county_emissions_all_years$Yr_2014)
us_county_emissions_all_years <- us_county_emissions_all_years[order(-us_county_emissions_all_years$Total),]  # Reorders dataframe on Total
### Calculate CAGR 
us_county_emissions_all_years$CAGR_2008_2014 <- round((((us_county_emissions_all_years$Yr_2014/us_county_emissions_all_years$Yr_2008)^(1/6)-1)*100),0)
us_county_emissions_all_years$CAGR_2008_2011 <- round((((us_county_emissions_all_years$Yr_2011/us_county_emissions_all_years$Yr_2008)^(1/3)-1)*100),0)
us_county_emissions_all_years$CAGR_20011_2014 <- round((((us_county_emissions_all_years$Yr_2014/us_county_emissions_all_years$Yr_2011)^(1/3)-1)*100),0)

########################## US Counties Sectors  #################################################################
# Aggregates the emmisions by Sectors  for each year
us_county_emissions_by_sector_2008 <- aggregate(emissions ~ EI_sector,data=us_emissions_detail_2008, FUN=sum)
us_county_emissions_by_sector_2011 <- aggregate(emissions ~ EI_sector,data=us_emissions_detail_2011, FUN=sum)
us_county_emissions_by_sector_2014 <- aggregate(emissions ~ EI_sector,data=us_emissions_detail_2014, FUN=sum)

# merges 3 dataframes and sorts based on largest total emissions  and writes to output file
us_county_emissions_by_sector_all_years <- merge(us_county_emissions_by_sector_2008,us_county_emissions_by_sector_2011, by="EI_sector") 
us_county_emissions_by_sector_all_years <- merge(us_county_emissions_by_sector_all_years,us_county_emissions_by_sector_2014)
colnames(us_county_emissions_by_sector_all_years)[2] <- "Year_2008" # renames the column names
colnames(us_county_emissions_by_sector_all_years)[3] <- "Year_2011"
colnames(us_county_emissions_by_sector_all_years)[4] <- "Year_2014"
us_county_emissions_by_sector_all_years$Total <- (us_county_emissions_by_sector_all_years$Year_2008 +us_county_emissions_by_sector_all_years$Year_2011+us_county_emissions_by_sector_all_years$Year_2014)  # sums 2008,2011,2014 to get total
us_county_emissions_by_sector_all_years <- us_county_emissions_by_sector_all_years[order(-us_county_emissions_by_sector_all_years$Total),]  # Reorders dataframe on Total

########################## US States Emissions #################################################################
us_states_emissions_2008 <- aggregate(emissions ~ state_abbr,data=us_emissions_detail_2008, FUN=sum)
us_states_emissions_2011 <- aggregate(emissions ~ state_abbr,data=us_emissions_detail_2011, FUN=sum)
us_states_emissions_2014 <- aggregate(emissions ~ state_abbr,data=us_emissions_detail_2014, FUN=sum)

# Merge State emissions into a dataframe and export
us_states_emissions_all_years <- merge(us_states_emissions_2008,us_states_emissions_2011, by= "state_abbr")
us_states_emissions_all_years <- merge(us_states_emissions_all_years,us_states_emissions_2014, by= "state_abbr")

names(us_states_emissions_all_years) <- c("State","Yr_2008","Yr_2011","Yr_2014")
us_states_emissions_all_years$Total <- (us_states_emissions_all_years$Yr_2008 + us_states_emissions_all_years$Yr_2011 + us_states_emissions_all_years$Yr_2014)
# Create data for CAGR 2008 to 2014 and chart - OPtions 1 - Has problems though
us_states_emissions_all_years$CAGR_2008_2014 <- round((((us_states_emissions_all_years$Yr_2014/us_states_emissions_all_years$Yr_2008)^(1/6)-1)*100),0)
us_states_emissions_all_years$CAGR_2008_2011 <- round((((us_states_emissions_all_years$Yr_2011/us_states_emissions_all_years$Yr_2008)^(1/3)-1)*100),0)
us_states_emissions_all_years$CAGR_20011_2014 <- round((((us_states_emissions_all_years$Yr_2014/us_states_emissions_all_years$Yr_2011)^(1/3)-1)*100),0)


######################################## US  - States and Sectors ##############################################################
######  Slice the dataframe By State (first) then Sector (Second) give detail on each sector within each state
# Referenced this source on how to implement: http://stackoverflow.com/questions/8212699/group-by-multiple-columns-and-sum-other-multiple-columns
group_columns_states = c("state_abbr","EI_sector")
data_columns_states = c("emissions")
rpt_states_and_sectors_2008 = ddply(us_emissions_detail_2008, group_columns_states, function(x) colSums(x[data_columns_states]))
rpt_states_and_sectors_2011 = ddply(us_emissions_detail_2011, group_columns_states, function(x) colSums(x[data_columns_states]))
rpt_states_and_sectors_2014 = ddply(us_emissions_detail_2014, group_columns_states, function(x) colSums(x[data_columns_states]))
# Apply function that give top sectors in each state 
top_sectors_in_states_2008 <-largest_sector_in_state(rpt_states_and_sectors_2008)
top_sectors_in_states_2011 <-largest_sector_in_state(rpt_states_and_sectors_2011)
top_sectors_in_states_2014 <-largest_sector_in_state(rpt_states_and_sectors_2014)

############### End - read the maximum Sector in each state ###################



########################## Illinois Emissions ############################
### Create a dataframe filtering for IL 
il_emissions_detail_2008 <- subset(us_emissions_detail_2008, state_abbr =="IL")
il_emissions_detail_2011 <- subset(us_emissions_detail_2011, state_abbr =="IL")
il_emissions_detail_2014 <- subset(us_emissions_detail_2014, state_abbr =="IL")
il_emissions_detail_all_years <- rbind(il_emissions_detail_2014,il_emissions_detail_2011,il_emissions_detail_2008)


# Calc  total IL emissions for each year 
rpt_il_total_emission_by_year <- aggregate(emissions ~ inventory_year,data=il_emissions_detail_all_years, FUN=sum, na.rm=TRUE)
il_total_emissions_2008 <- rpt_il_total_emission_by_year[,2][[1]]
il_total_emissions_2011 <- rpt_il_total_emission_by_year[,2][[2]]
il_total_emissions_2014 <- rpt_il_total_emission_by_year[,2][[3]]


# IL total emmisions  by Sectors  
il_emissions_by_sectors_2008 <- aggregate(emissions ~ EI_sector,data=il_emissions_detail_2008, FUN=sum)
il_emissions_by_sectors_2011 <- aggregate(emissions ~ EI_sector,data=il_emissions_detail_2011, FUN=sum)
il_emissions_by_sectors_2014 <- aggregate(emissions ~ EI_sector,data=il_emissions_detail_2014, FUN=sum)
# merge IL emissions by sector into single df 
il_emissions_by_sectors_all_years <- merge(il_emissions_by_sectors_2008,il_emissions_by_sectors_2011, by="EI_sector") 
il_emissions_by_sectors_all_years <- merge(il_emissions_by_sectors_all_years,il_emissions_by_sectors_2014)
# renames the column names
colnames(il_emissions_by_sectors_all_years)[2] <- "Year_2008" 
colnames(il_emissions_by_sectors_all_years)[3] <- "Year_2011"
colnames(il_emissions_by_sectors_all_years)[4] <- "Year_2014"
# Calculate Contirubtion to totals
il_emissions_by_sectors_all_years$Total <- (il_emissions_by_sectors_all_years$Year_2008 +il_emissions_by_sectors_all_years$Year_2011+il_emissions_by_sectors_all_years$Year_2014)
il_emissions_by_sectors_all_years$Contribution_2008 <- (il_emissions_by_sectors_all_years$Year_2008)/il_total_emissions_2008
il_emissions_by_sectors_all_years$Contribution_2011 <- (il_emissions_by_sectors_all_years$Year_2011)/il_total_emissions_2011
il_emissions_by_sectors_all_years$Contribution_2014 <- (il_emissions_by_sectors_all_years$Year_2014)/il_total_emissions_2014
il_emissions_by_sectors_all_years$Contribution_Total <- (il_emissions_by_sectors_all_years$Total)/(il_total_emissions_2008+il_total_emissions_2011+il_total_emissions_2014)
# Calculate CAGRS
il_emissions_by_sectors_all_years$cagr_2008_to_2011 <- round(((((il_emissions_by_sectors_all_years$Year_2011/il_emissions_by_sectors_all_years$Year_2008)^(1/3))-1)*100),0)
il_emissions_by_sectors_all_years$cagr_2011_to_2014 <- round(((((il_emissions_by_sectors_all_years$Year_2014/il_emissions_by_sectors_all_years$Year_2011)^(1/3))-1)*100),0)
il_emissions_by_sectors_all_years$cagr_2008_to_2014 <- round(((((il_emissions_by_sectors_all_years$Year_2014/il_emissions_by_sectors_all_years$Year_2008)^(1/6))-1)*100),0)
# Reorders dataframe on Total
il_emissions_by_sectors_all_years <- il_emissions_by_sectors_all_years[order(-il_emissions_by_sectors_all_years$Total),]
# IL emissions by Counties  
il_emissions_by_county_2008 <- aggregate(emissions ~ fips,data=il_emissions_detail_2008, FUN=sum, na.rm=TRUE)
il_emissions_by_county_2011 <- aggregate(emissions ~ fips,data=il_emissions_detail_2011, FUN=sum, na.rm=TRUE)
il_emissions_by_county_2014 <- aggregate(emissions ~ fips,data=il_emissions_detail_2014, FUN=sum, na.rm=TRUE)

###### Slice by County and Sector a combined ####   ####
### IL Counties
group_columns_il_counties = c("fips","EI_sector")
data_columns_il_counties = c("emissions")
il_counties_and_sectors_2008 = ddply(il_emissions_detail_2008, group_columns_il_counties, function(x) colSums(x[data_columns_il_counties]))
il_counties_and_sectors_2011 = ddply(il_emissions_detail_2011, group_columns_il_counties, function(x) colSums(x[data_columns_il_counties]))
il_counties_and_sectors_2014 = ddply(il_emissions_detail_2014, group_columns_il_counties, function(x) colSums(x[data_columns_il_counties]))




####################################################################################################################
############################################Adding Additional Data from other Sources#########################
######################################################################################################################

############## Data Manipulation and Cleaning ###################
# read in the data fropm file  
us_census_file_location = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\data\\us_census\\DEC_10_SF1_GCTPH1.US05PR_with_ann.csv"
us_census_csv_read = readLines(us_census_file_location)
# eliminate 2nd row...(description)
skip_second = us_census_csv_read[-2]
us_census_data1 = read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE)
# Drop the excessive Columns
us_census_data1[1:4] <- list(NULL)  
# rename columns for ease
setnames(us_census_data1, old = c('GCT_STUB.target.geo.id2','GCT_STUB.display.label','GCT_STUB.display.label.1',
                                  'HD01','HD02','SUBHD0301','SUBHD0302','SUBHD0303','SUBHD0401','SUBHD0402'), 
         new = c('fips','geo.label.long','geo.label.trim',
                 'population','housing.units','area.total','area.water','area.land','pop.density','housing.density'
         ))
# Clean out the Parenthesis (and contents)...only 2 columns 
us_census_data1$population <- gsub("\\s*\\([^\\)]+\\)","",as.character(us_census_data1$population))
us_census_data1$housing.units <- gsub("\\s*\\([^\\)]+\\)","",as.character(us_census_data1$housing.units))
# force to number 
us_census_data1$population <- as.numeric(us_census_data1$population)
us_census_data1$housing.units <- as.numeric(us_census_data1$housing.units)
# add a 1 to eliminate zeros (assists with log transforms)
us_census_data1$population <- us_census_data1$population + 1
us_census_data1$housing.units <- us_census_data1$housing.units +1
us_census_data1$pop.density <- us_census_data1$pop.density +.01
us_census_data1$housing.density <- us_census_data1$pop.density +.01

###### Join with US County Master #####
## Join with Do a "Left Join" bring together (emissions data and Us_census)....
# All emissions data are returned regardless of whether matched to US_census
us_county_master_data  <- merge(x =us_county_emissions_all_years , y = us_census_data1, by = "fips", all.x = TRUE)
# Create new variable Densities and "Log +1" Transforms
us_county_master_data$emissions_density_2014 <- us_county_master_data$Yr_2014 / us_county_master_data$area.land
us_county_master_data$log.pop <- log(us_county_master_data$population +1 )
us_county_master_data$log.emissions.2014 <- log(us_county_master_data$Yr_2014 +.0000001)
## Create a copy of a cleaned dataframe
# removes rows with NAs (in any column) 
us_county_master_data_cleaned <- us_county_master_data[complete.cases(us_county_master_data),]

#### Join with US Detail Master #####
us_emissions_detail_all_years  <- merge(x =us_emissions_detail_all_years , y = us_census_data1, by = "fips")
#Create new variable Densities and "Log +1" Transforms
us_emissions_detail_all_years$emissions_density <- us_emissions_detail_all_years$emissions / us_emissions_detail_all_years$area.land
us_emissions_detail_all_years$log.pop <- log(us_emissions_detail_all_years$population +1 )
us_emissions_detail_all_years$log.emissions <- log(us_emissions_detail_all_years$emissions +.0000001)
## Create a copy of a cleaned dataframe
# removes rows with NAs (in any column) 

#### IL County_joined with Census 
il_county_master_data_cleaned <- subset(us_county_master_data_cleaned, fips >= 17000 & fips <= 18000)

#### IL emissions Detail joined with Census 
il_emissions_detail_all_years <- subset(il_emissions_detail_all_years, fips >= 17000 & fips <= 18000)



##############################################################################################################
##############################################################################################################
########################################### Sector ############################################################
##############################################################################################################
##############################################################################################################

######################### Agricultural Crop & LIvestock Dust 
####### US 
us_agdust_emissions_detail_2014 <- subset(us_emissions_detail_all_years, 
                            EI_sector == "Agriculture - Crops & Livestock Dust" & inventory_year==2014)
us_agdust_emissions_detail_2011 <- subset(us_emissions_detail_all_years, 
                                          EI_sector == "Agriculture - Crops & Livestock Dust" & inventory_year==2011)
us_agdust_emissions_detail_2008 <- subset(us_emissions_detail_all_years, 
                                          EI_sector == "Agriculture - Crops & Livestock Dust" & inventory_year==2008)
us_agdust_by_county_2014 <- aggregate(emissions ~ fips,data=us_agdust_emissions_detail_2014, FUN=sum, na.rm=TRUE)
us_agdust_by_county_2011 <- aggregate(emissions ~ fips,data=us_agdust_emissions_detail_2011, FUN=sum, na.rm=TRUE)
us_agdust_by_county_2008 <- aggregate(emissions ~ fips,data=us_agdust_emissions_detail_2008, FUN=sum, na.rm=TRUE)
colnames(us_agdust_by_county_2014) <- c("fips", "agdust_emissions_2014")
colnames(us_agdust_by_county_2011) <- c("fips", "agdust_emissions_2011")
colnames(us_agdust_by_county_2008) <- c("fips", "agdust_emissions_2008")
us_agdust_by_county_2014$agdust_emissions_2014 <- us_agdust_by_county_2014$agdust_emissions_2014 +.0000001  # This is to remove 0s (so that when you run log )
us_agdust_by_county_2011$agdust_emissions_2011 <- us_agdust_by_county_2011$agdust_emissions_2011 +.0000001  # This is to remove 0s (so that when you run log )
us_agdust_by_county_2008$agdust_emissions_2008 <- us_agdust_by_county_2008$agdust_emissions_2008 +.0000001  # This is to remove 0s (so that when you run log )
us_county_master_data_cleaned <- merge(x =us_county_master_data_cleaned, y=us_agdust_by_county_2014, by="fips",x.all=TRUE )
us_county_master_data_cleaned <- merge(x =us_county_master_data_cleaned, y=us_agdust_by_county_2011, by="fips",x.all=TRUE )
us_county_master_data_cleaned <- merge(x =us_county_master_data_cleaned, y=us_agdust_by_county_2008, by="fips",x.all=TRUE )


######################### Unpaved Road Dust  ##############################
## US Unpaved Road Dust 
us_unpaved_emissions_detail_2008 <- subset(us_emissions_detail_all_years, 
              EI_sector == "Dust - Unpaved Road Dust" & inventory_year == 2008)
us_unpaved_emissions_detail_2011 <- subset(us_emissions_detail_all_years, 
              EI_sector == "Dust - Unpaved Road Dust" & inventory_year == 2011)
us_unpaved_emissions_detail_2014 <- subset(us_emissions_detail_all_years, 
              EI_sector == "Dust - Unpaved Road Dust" & inventory_year == 2014)
us_unpaved_emissions_by_county_2008 <- aggregate(emissions ~ fips,data=us_unpaved_emissions_detail_2008, FUN=sum, na.rm=TRUE)
us_unpaved_emissions_by_county_2011 <- aggregate(emissions ~ fips,data=us_unpaved_emissions_detail_2011, FUN=sum, na.rm=TRUE)
us_unpaved_emissions_by_county_2014 <- aggregate(emissions ~ fips,data=us_unpaved_emissions_detail_2014, FUN=sum, na.rm=TRUE)
colnames(us_unpaved_emissions_by_county_2008) <- c("fips", "unpaved_road_emissions_2008")
colnames(us_unpaved_emissions_by_county_2011) <- c("fips", "unpaved_road_emissions_2011")
colnames(us_unpaved_emissions_by_county_2014) <- c("fips", "unpaved_road_emissions_2014")
us_county_master_data_cleaned <- merge(x =us_county_master_data_cleaned, y=us_unpaved_emissions_by_county_2008, by="fips",x.all=TRUE )
us_county_master_data_cleaned <- merge(x =us_county_master_data_cleaned, y=us_unpaved_emissions_by_county_2011, by="fips",x.all=TRUE )
us_county_master_data_cleaned <- merge(x =us_county_master_data_cleaned, y=us_unpaved_emissions_by_county_2014, by="fips",x.all=TRUE )

### IL Unpaved Road Dust
il_unpaved_emissions_detail_2008 <- subset(us_emissions_detail_all_years, 
                               EI_sector == "Dust - Unpaved Road Dust" &
                                 fips > 17000 & fips < 18000 & inventory_year == 2008)
il_unpaved_emissions_detail_2011 <- subset(us_emissions_detail_all_years, 
                                           EI_sector == "Dust - Unpaved Road Dust" &
                                             fips > 17000 & fips < 18000 & inventory_year == 2011)
il_unpaved_emissions_detail_2014 <- subset(us_emissions_detail_all_years, 
                                           EI_sector == "Dust - Unpaved Road Dust" &
                                             fips > 17000 & fips < 18000 & inventory_year == 2014)
il_unpaved_emissions_by_county_2008 <- aggregate(emissions ~ fips,data=il_unpaved_emissions_detail_2008, FUN=sum, na.rm=TRUE)
il_unpaved_emissions_by_county_2011 <- aggregate(emissions ~ fips,data=il_unpaved_emissions_detail_2011, FUN=sum, na.rm=TRUE)
il_unpaved_emissions_by_county_2014 <- aggregate(emissions ~ fips,data=il_unpaved_emissions_detail_2014, FUN=sum, na.rm=TRUE)
colnames(il_unpaved_emissions_by_county_2008) <- c("fips", "unpaved_road_emissions_2008")
colnames(il_unpaved_emissions_by_county_2011) <- c("fips", "unpaved_road_emissions_2011")
colnames(il_unpaved_emissions_by_county_2014) <- c("fips", "unpaved_road_emissions_2014")
il_county_master_data_cleaned <- merge(x =il_county_master_data_cleaned, y=il_unpaved_emissions_by_county_2008, by="fips",x.all=TRUE )
il_county_master_data_cleaned <- merge(x =il_county_master_data_cleaned, y=il_unpaved_emissions_by_county_2011, by="fips",x.all=TRUE )
il_county_master_data_cleaned <- merge(x =il_county_master_data_cleaned, y=il_unpaved_emissions_by_county_2014, by="fips",x.all=TRUE )

######################### Paved Road Dust  ##############################
## US Paved Road Dust
us_paved_emissions_detail_2008 <- subset(us_emissions_detail_all_years, 
                                           EI_sector == "Dust - Paved Road Dust" & inventory_year == 2008)
us_paved_emissions_detail_2011 <- subset(us_emissions_detail_all_years, 
                                           EI_sector == "Dust - Paved Road Dust" & inventory_year == 2011)
us_paved_emissions_detail_2014 <- subset(us_emissions_detail_all_years, 
                                           EI_sector == "Dust - Paved Road Dust" & inventory_year == 2014)
us_paved_emissions_by_county_2008 <- aggregate(emissions ~ fips,data=us_paved_emissions_detail_2008, FUN=sum, na.rm=TRUE)
us_paved_emissions_by_county_2011 <- aggregate(emissions ~ fips,data=us_paved_emissions_detail_2011, FUN=sum, na.rm=TRUE)
us_paved_emissions_by_county_2014 <- aggregate(emissions ~ fips,data=us_paved_emissions_detail_2014, FUN=sum, na.rm=TRUE)
colnames(us_paved_emissions_by_county_2008) <- c("fips", "paved_road_emissions_2008")
colnames(us_paved_emissions_by_county_2011) <- c("fips", "paved_road_emissions_2011")
colnames(us_paved_emissions_by_county_2014) <- c("fips", "paved_road_emissions_2014")
us_county_master_data_cleaned <- merge(x =us_county_master_data_cleaned, y=us_paved_emissions_by_county_2008, by="fips",x.all=TRUE )
us_county_master_data_cleaned <- merge(x =us_county_master_data_cleaned, y=us_paved_emissions_by_county_2011, by="fips",x.all=TRUE )
us_county_master_data_cleaned <- merge(x =us_county_master_data_cleaned, y=us_paved_emissions_by_county_2014, by="fips",x.all=TRUE )
### IL Unpaved Road Dust
il_paved_emissions_detail_2008 <- subset(us_emissions_detail_all_years, 
                                           EI_sector == "Dust - Paved Road Dust" &
                                             fips > 17000 & fips < 18000 & inventory_year == 2008)
il_paved_emissions_detail_2011 <- subset(us_emissions_detail_all_years, 
                                           EI_sector == "Dust - Paved Road Dust" &
                                             fips > 17000 & fips < 18000 & inventory_year == 2011)
il_paved_emissions_detail_2014 <- subset(us_emissions_detail_all_years, 
                                           EI_sector == "Dust - Paved Road Dust" &
                                             fips > 17000 & fips < 18000 & inventory_year == 2014)
il_paved_emissions_by_county_2008 <- aggregate(emissions ~ fips,data=il_paved_emissions_detail_2008, FUN=sum, na.rm=TRUE)
il_paved_emissions_by_county_2011 <- aggregate(emissions ~ fips,data=il_paved_emissions_detail_2011, FUN=sum, na.rm=TRUE)
il_paved_emissions_by_county_2014 <- aggregate(emissions ~ fips,data=il_paved_emissions_detail_2014, FUN=sum, na.rm=TRUE)
colnames(il_paved_emissions_by_county_2008) <- c("fips", "paved_road_emissions_2008")
colnames(il_paved_emissions_by_county_2011) <- c("fips", "paved_road_emissions_2011")
colnames(il_paved_emissions_by_county_2014) <- c("fips", "paved_road_emissions_2014")
il_county_master_data_cleaned <- merge(x =il_county_master_data_cleaned, y=il_paved_emissions_by_county_2008, by="fips",x.all=TRUE )
il_county_master_data_cleaned <- merge(x =il_county_master_data_cleaned, y=il_paved_emissions_by_county_2011, by="fips",x.all=TRUE )
il_county_master_data_cleaned <- merge(x =il_county_master_data_cleaned, y=il_paved_emissions_by_county_2014, by="fips",x.all=TRUE )



############################################################################################
############################## ADDing IL ACRES DATA IN ######################################## 
############################################################################################

# Get CSV from Source:
#Source of Data: https://quickstats.nass.usda.gov/?source_desc=CENSUS#35495AA0-40F5-3A04-B6AD-3EF6C1497359
# Read the data from CSV
il_acres_file_location <- "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\data\\usda\\quick_stats_2.0\\counties\\EBC09F01-53CB-3D01-844E-1353A225C7C7.csv"
il_counties_acres_2012 <- read.csv(il_acres_file_location)
# Delete Excess Columns # Keep columns, 10,11,16,17,20 (ends at 21)
il_counties_acres_2012[c(1:9,12:15,18:19,21)] <- list(NULL)  
#  Create fips code  - (State value is 17,...but since treating as string..i.e 17039..we use 17,000 to keep numeric but equivalent to string concatenation)
il_state_fips_start_code <- 17000
il_counties_acres_2012$fips <- il_state_fips_start_code+il_counties_acres_2012$County.ANSI
il_counties_acres_2012[c(2)] <- list(NULL)  # then drops the county.ansi column which is no longer needed
# Clean out parenthesis from up column "Value"
il_counties_acres_2012$Value <- gsub("\\s*\\([^\\)]+\\)","",as.character(il_counties_acres_2012$Value))
# force back into number
il_counties_acres_2012$Value <- as.numeric(il_counties_acres_2012$Value)
# Aggregate counties - this sums all acres harvested
il_counties_acres_agg_2012 <- aggregate(Value ~ fips,data=il_counties_acres_2012, FUN=sum, na.rm=TRUE)
# merge - il aggregate acres harvests data to il county data 
il_county_master_data_cleaned <- merge(il_counties_acres_agg_2012,il_county_master_data_cleaned, by="fips")
colnames(il_county_master_data_cleaned)[2] <- "acres_harvest_2012"
######### merge  emission from Ag-Crop dUST Emissions  to il_counties Dataframe


#####################################################################################################
############################ Adding in IL Annual Vechicle Miles Traveled ###########################
#####################################################################################################


il_avmt_file_locatin <- "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\data\\idot\\vechicle_miles_traveled\\il_counties_vehicle_miles_traveled_08to15.csv"
il_avmt <- read.csv(il_avmt_file_locatin)
head(il_avmt)
us_county_master_data  <- merge(x =us_county_master_data_cleaned , y = il_avmt, by = "fips", all.x = TRUE)
il_county_master_data_cleaned  <- merge(x =il_county_master_data_cleaned , y = il_avmt, by = "fips", all.x = TRUE)



#####################################################################################################
############################ Adding in IL Road Miles Traveled ###########################
#####################################################################################################
####### Work in Progress@@@@@#############################
# Ran out of time on this one
#Example on how toi read a PDF 
#Source: http://stackoverflow.com/questions/3852354/extracting-text-data-from-pdf-files

install.packages("tm")  
library('tm')
road_miles_file <- "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\data\\idot\\road_miles\\illinois_highway_and_street_mileage_statistics_2013.pdf"
road_miles_pdf <- readPDF(control = list(text = "-layout"))
road_miles_corpus <- VCorpus(URISource(road_miles_file), 
                  readerControl = list(reader = road_miles_pdf))
road_miles_corpus.array <- content(content(road_miles_corpus)[[1]])
road_miles_outfile <- "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\road_miles\\road_miles_outfile.csv" 
write.csv(road_miles_corpus.array, file = "road_miles_outfile")
str(road_miles_corpus.array)



#####################################################################################################
#### Create Deciles based on Emissions Density 
#####################################################################################################
# Us ntile to split counties into 10 group (store in US_county master_data)
us_county_master_data_cleaned$em_dens_decil_2014 <-  ntile(us_county_master_data_cleaned$emissions_density_2014, 10)
#Stores Deciles in temp Df (that has been trimmed up) in prep for joining
temp_join_df <- us_county_master_data_cleaned[c("fips","em_dens_decil_2014")]
#Join up with all years 
us_emissions_detail_all_years <- join(us_emissions_detail_all_years, temp_join_df, by = "fips")
#Slice by Emissions Dennsity Decils AND Sector 
group_columns_deciles <- c("em_dens_decil_2014","EI_sector")
data_columns_deciles <-  c("emissions_density")
us_emissions_by_decile_AND_sectors_2014 = ddply(us_emissions_detail_all_years, group_columns_deciles, 
                                                function(x) colSums(x[data_columns_deciles]))

# Us ntile to split counties into 100 group (store in US_county master_data)
us_county_master_data_cleaned$em_dens_centile_2014 <-  ntile(us_county_master_data_cleaned$emissions_density_2014, 100)
#Stores Deciles in temp Df (that has been trimmed up) in prep for joining
temp_join_df <- us_county_master_data_cleaned[c("fips","em_dens_centile_2014")]
#Join up with all years 
us_emissions_detail_all_years <- join(us_emissions_detail_all_years, temp_join_df, by = "fips")
#Slice by Emissions Dennsity Decils AND Sector 
group_columns_centiles <- c("em_dens_centile_2014","EI_sector")
data_columns_centiles <-  c("emissions_density")
us_emissions_by_centile_AND_sectors_2014 = ddply(us_emissions_detail_all_years, group_columns_centiles, 
                                                function(x) colSums(x[data_columns_centiles]))



#################  Investigating Worst Areas - And Peoria  ##################################


#### Investigate individual Counties (the worst 1%) 
# Cook County
worst_1per_cook_county <- subset(us_emissions_detail_all_years, fips== 17031 & inventory_year ==2014)
keeps <- c("fips","state_abbr","emissions", "EI_sector","population")
worst_1per_cook_county <- worst_1per_cook_county[,keeps,drop=FALSE]
worst_1per_cook_county$contribution <- round(((worst_1per_cook_county$emissions)/(sum(worst_1per_cook_county$emissions))*100),0)


## Store up and export the largest populations with conerning PM2.5 levels
worst_counties_large_pop_fips <-c(36047,36081,36061,42101,36005,36059,18097,6075,25025,27123,51013,17031)
worst_counties_large_pop <- subset(us_emissions_detail_all_years, 
                fips %in% worst_counties_large_pop_fips & inventory_year ==2014)
keeps <- c("fips","state_abbr","county_name","emissions", "EI_sector","population")
worst_counties_large_pop <- worst_counties_large_pop[,keeps,drop=FALSE]

# Repeat Same process for worst counties Small Populations 
worst_counties_small_pop_fips <-c( 51580,18125,51670,51610,22023,6093)
worst_counties_small_pop <- subset(us_emissions_detail_all_years, 
                                   fips %in% worst_counties_small_pop_fips & inventory_year ==2014)
keeps <- c("fips","state_abbr","county_name","emissions", "EI_sector","population")
worst_counties_small_pop <- worst_counties_small_pop[,keeps,drop=FALSE]


# Repeat Same process for worst counties Small Populations 
peoria_county <- subset(us_emissions_detail_all_years, fips== 17031)
tazwell_county <- subset(us_emissions_detail_all_years, fips== 17179)
peoria_metro_counties_fips <-c(17143,17179)
peoria_metro_counties <- subset(us_emissions_detail_all_years, 
                                   fips %in% peoria_metro_counties_fips & inventory_year ==2014)
keeps <- c("fips","state_abbr","county_name","emissions", "EI_sector","population")
peoria_metro_counties <- peoria_metro_counties[,keeps,drop=FALSE]




#######  Writing out files  for investigation
# County All Years 
write.csv(us_county_master_data,file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\US_by_county.csv")
write.csv(us_county_emissions_by_sector_all_years,file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\sum_by_us_emissions_detail_all_years.csv")
write.csv(us_states_emissions_all_years,file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\by_state.csv")
write.csv(il_emissions_by_sectors_all_years,file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\il_sum_by_sectors_all_years.csv")
write.csv(il_emissions_by_county_2014,file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\il_emissions_by_county_2014.csv")
write.csv(top_sectors_in_states_2008,file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\top_sectors_in_states_2008.csv")
write.csv(top_sectors_in_states_2011,file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\top_sectors_in_states_2011.csv")
write.csv(top_sectors_in_states_2014,file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\top_sectors_in_states_2014.csv")
write.csv(il_counties_and_sectors_2014, file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\il_by_county_AND_sector_2014.csv")
write.csv(rpt_il_counties_larges_sectors_2014, file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\il_counties_largest_sectors_2014.csv")
write.csv(us_emissions_by_decile_AND_sectors_2014, file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\il_counties_em.den.dec_AND_sector.csv")
write.csv(us_emissions_by_centile_AND_sectors_2014, file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\il_counties_em.den.cent_AND_sector.csv")
write.csv(worst_1per_cook_county, file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\worst_1per_cook_county.csv")
write.csv(worst_counties_large_pop, file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\worst_1per_large_pop.csv")
write.csv(worst_counties_small_pop, file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\worst_1per_small_pop.csv")
write.csv(peoria_metro_counties, file ="C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\peoria_metro.csv")


##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
################################# End - Primary  Data Cleaning and Manipulations ###################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################



##############################################################################################################
##############################################################################################################
######################################## Beg Exploration data anlaysis ###################################################
##############################################################################################################
##############################################################################################################



####### Basic Statistics ###################
# summarize Totals by Year
rpt_us_emissions_sum_all_years <- aggregate(emissions ~ inventory_year,data=us_emissions_detail_all_years, FUN=sum, na.rm=TRUE)

# Create Summary of sector emissions (entire US)
summary(us_emissions_detail_2008$emissions)
summary(us_emissions_detail_2011$emissions)
summary(us_emissions_detail_2014$emissions)
summary(us_emissions_detail_all_years$emissions)

# Create A Boxplot of log(emissions)
boxplot(log(emissions)~inventory_year, data=us_emissions_detail_all_years,
        main=toupper("US"), 
        font.main=3, cex.main=1.2, 
        xlab="Year",
        ylab="Log(Tons) of Emissions",
        font.lab=3,
        col="darkgreen")

##############################################################################################################
##############################################################################################################
###################################### Choropleths #################################################################
##############################################################################################################
##############################################################################################################

## Source 1 : Source: http://stackoverflow.com/questions/38938565/alaska-and-hawaii-not-formatting-correctly-for-county-choropleth-map-in-r

##################### Choropleth  US States: Emissions  ##############################
# copy df
choro_us_states_emissions_2008 <- us_states_emissions_2008
choro_us_states_emissions_2011 <- us_states_emissions_2011
choro_us_states_emissions_2014 <- us_states_emissions_2014
# Rename 
colnames(choro_us_states_emissions_2008) <- c("region", "value")
colnames(choro_us_states_emissions_2011) <- c("region", "value")
colnames(choro_us_states_emissions_2014) <- c("region", "value")
# Apply the abbreviation function
choro_us_states_emissions_2008$region <- stateFromLower(choro_us_states_emissions_2008$region)
choro_us_states_emissions_2011$region <- stateFromLower(choro_us_states_emissions_2011$region)
choro_us_states_emissions_2014$region <- stateFromLower(choro_us_states_emissions_2014$region)
# Run the choropleth Function
state_choropleth_no_abbvreviations(choro_us_states_emissions_2008, "2008 US Particulate Matter 2.5 Microns Emission by State","PM2.5 Emmisions",1)
state_choropleth_no_abbvreviations(choro_us_states_emissions_2011, "2011 US Particulate Matter 2.5 Microns Emission by State","PM2.5 Emmisions",1)
state_choropleth_no_abbvreviations(choro_us_states_emissions_2014, "2014 US Particulate Matter 2.5 Microns Emission by State","PM2.5 Emmisions",1)


################### Choropleth US States - CAGR ###############
# copy df 
choro_states_cagr_2014v2008 <- us_states_emissions_all_years
choro_states_cagr_2011v2008 <- us_states_emissions_all_years
choro_states_cagr_2014v2011 <- us_states_emissions_all_years
# trim df 
choro_states_cagr_2014v2008 <- choro_states_cagr_2014v2008[c(1,6)]
choro_states_cagr_2011v2008 <- choro_states_cagr_2011v2008[c(1,7)]
choro_states_cagr_2014v2011 <- choro_states_cagr_2014v2011[c(1,8)]
# renaming df 
colnames(choro_states_cagr_2014v2008) <- c("region", "value")
colnames(choro_states_cagr_2011v2008) <- c("region", "value")
colnames(choro_states_cagr_2014v2011) <- c("region", "value")
# apply appreviations function
choro_states_cagr_2014v2008$region <- stateFromLower(choro_states_cagr_2014v2008$region)
choro_states_cagr_2011v2008$region <- stateFromLower(choro_states_cagr_2011v2008$region)
choro_states_cagr_2014v2011$region <- stateFromLower(choro_states_cagr_2014v2011$region)
# apply  choropleth function
state_choropleth_no_abbvreviations(choro_states_cagr_2014v2008, "US Particulate Matter 2.5 Percentage change in Emissions 2008-2014 CAGR","Percentage Change",7)
state_choropleth_no_abbvreviations(choro_states_cagr_2011v2008, "US Particulate Matter 2.5 Percentage change in Emissions 2008-2011 CAGR","Percentage Change",7)
state_choropleth_no_abbvreviations(choro_states_cagr_2014v2011, "US Particulate Matter 2.5 Percentage change in Emissions 2011-2014 CAGR","Percentage Change",7)


######################### Choropleth - US Counties: emission  ##################################
# copy df 
choro_us_county_emissions_2008 <- us_county_emissions_2008
choro_us_county_emissions_2011 <- us_county_emissions_2011
choro_us_county_emissions_2014 <- us_county_emissions_2014
#rename
colnames(choro_us_county_emissions_2008) <- c("region", "value")
colnames(choro_us_county_emissions_2011) <- c("region", "value")
colnames(choro_us_county_emissions_2014) <- c("region", "value")

################ US County Choropleths - Total Emissions ###############
county_choropleth(choro_us_county_emissions_2008,
                  title      = "",
                  num_colors = 7,
                  legend = "emissions")
county_choropleth(choro_us_county_emissions_2011,
                  title      = "",
                  num_colors = 7,
                  legend = "emissions")
county_choropleth(choro_us_county_emissions_2014,
                  title      = "",
                  num_colors = 7,
                  legend = "emissions")


#####################################################################################
############# Choropleths - US County EMissions Density    #######################
# Create a dataframe that is usable with ChoroplethR
choro_us_county_emission_density_2014 <- us_county_master_data_cleaned
# creates a copy from clean version
choro_us_county_emission_density_2014[c(2:17,19:20)] <- list(NULL)   # deletes out excess columns 
colnames(choro_us_county_emission_density_2014)[1] <- "region" # renames the column names
colnames(choro_us_county_emission_density_2014)[2] <- "value" # renames the column names
# Note NYC is big outlier at 500 (commercial cooking)...

###### US Counties Emissions Density - 7 Categories

choro_us_county_em_den = CountyChoropleth$new(choro_us_county_emission_density_2014)
choro_us_county_em_den$title        = ""
choro_us_county_em_den$ggplot_scale = scale_fill_brewer(name="Population", palette=2, drop=FALSE)
choro_us_county_em_den$render()

print(my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\maps\\counties\\US.counties.emissions.density.continuous.pdf"
dev.copy(jpeg, filename = new_file_name)
dev.off()

##### US County _ Worst 1% 
### Store a chorpleth dataframe that stores just the top 1 percent counties 
choro_worst_1_percent_em_dens <- us_county_master_data_cleaned[,c("fips","em_dens_centile_2014")]
colnames(choro_worst_1_percent_em_dens)[1] <- "region"
colnames(choro_worst_1_percent_em_dens)[2] <- "value"
choro_worst_1_percent_em_dens <- subset(choro_worst_1_percent_em_dens, value==100)

choro <- CountyChoropleth$new(choro_worst_1_percent_em_dens)
choro$title <- "PM2.5 Emission Growth Rate 2014 vs 2011"
choro$ggplot_scale = scale_fill_brewer(name="growth rate (%)",
                                       palette = "Set1", 
                                       drop=FALSE,
                                       na.value = "grey90")
choro$render()





##############   Choro IL Counties - Total Emission ######
# Reference Source: http://stackoverflow.com/questions/33112514/counties-within-one-state-choropleth-map
choro_il_emissions_by_county_2014 <- il_emissions_by_county_2014
colnames(choro_il_emissions_by_county_2014)[1] <- "region"
colnames(choro_il_emissions_by_county_2014)[2] <- "value"

county_choropleth(choro_il_emissions_by_county_2014, 
                  state_zoom = "illinois",
                  num_colors = 1,
                  legend = "emissions") 


############# Choropleths - IL County EMissions Density    #######################
choro_il_county_emission_density <- subset(choro_us_county_emission_density_2014, region>17000 & region<18000)
choro_il_county_emission_density
my_plot <- county_choropleth(choro_il_county_emission_density, 
                             state_zoom = "illinois",
                             title      = "",
                             num_colors = 7,
                             legend = "emissions")
print(my_plot)
new_file_name = "C:/Users/Justin/Desktop/OSF_Inperson_Interview_2017-01-25/output/maps/IL.counties.emissions.density.pdf"
dev.copy(pdf, filename = new_file_name)
dev.off()


########### IL  Largest Sectors in County ###############################################
rpt_il_counties_larges_sectors_2014 <- largest_sector_in_county(il_counties_and_sectors_2014)

# Map the value of Max_sector to either -1 or 0 or +1 ()
rpt_il_counties_larges_sectors_2014$value <- 1
rpt_il_counties_larges_sectors_2014$value[rpt_il_counties_larges_sectors_2014$max_sectors=="Dust - Unpaved Road Dust"] <- 2
rpt_il_counties_larges_sectors_2014$value[rpt_il_counties_larges_sectors_2014$max_sectors=="Agriculture - Crops & Livestock Dust"] <- 3
choro_rpt_il_counties_larges_sectors_2014 <- rpt_il_counties_larges_sectors_2014[,-(2:3),drop=FALSE]
colnames(choro_rpt_il_counties_larges_sectors_2014)[1] <- "region"

county_choropleth(rpt_il_counties_larges_sectors_2014_for_choro, 
                  state_zoom = "illinois",
                  num_colors = 1,
                  legend = "sectors") 
+
  scale_fill_gradient2(high = "red", 
                       mid = "blue",
                       low = "white", 
                       na.value = "grey90", 
                       breaks = pretty(il_emissions_by_county_2014$value, n = 2))



##Creat a choropleth of IL Acres Harvested 
choro_il_acres_harvested <- il_county_master_data_cleaned
choro_il_acres_harvested[c(3:10)] <- NULL
colnames(choro_il_acres_harvested)[1] <-  "region"
colnames(choro_il_acres_harvested)[2] <-  "value"

#### IL
my_plot <- county_choropleth(choro_il_acres_harvested, 
                             state_zoom = "illinois",
                             title      = "",
                             num_colors = 7,
                             legend = "acres")
print(my_plot)
new_file_name = "C:/Users/Justin/Desktop/OSF_Inperson_Interview_2017-01-25/output/maps/IL.counties.acres>harvest.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()




############# Choropleths - Emissions CAGRS US & IL    #######################
# Data Prep  US Cagr
choro_us_county_cagr_14v08 <- us_county_master_data_cleaned[c(1,6)]
choro_us_county_cagr_11v08 <- us_county_master_data_cleaned[c(1,7)]
choro_us_county_cagr_14v11 <- us_county_master_data_cleaned[c(1,8)]
# renaming into usable format
colnames(choro_us_county_cagr_14v08) <- c("region", "value")
colnames(choro_us_county_cagr_11v08) <- c("region", "value")
colnames(choro_us_county_cagr_14v11) <- c("region", "value")
# Data Prep  IL Cagr
choro_il_county_cagr_14v08 <- il_county_master_data_cleaned[c(1,6)]
choro_il_county_cagr_11v08 <- il_county_master_data_cleaned[c(1,7)]
choro_il_county_cagr_14v11 <- il_county_master_data_cleaned[c(1,8)]
# renaming into usable format
colnames(choro_il_county_cagr_14v08) <- c("region", "value")
colnames(choro_il_county_cagr_11v08) <- c("region", "value")
colnames(choro_il_county_cagr_14v11) <- c("region", "value")

#### US 2014 vws 2008 
choro <- CountyChoropleth$new(choro_us_county_cagr_14v08)
choro$title <- "PM2.5 Emission Growth Rate 2014 vs 2008"
choro$ggplot_scale = scale_fill_brewer(name="growth rate (%)",
                                       palette = "RdBu", 
                                       drop=FALSE,
                                       na.value = "grey90")
choro$render()

#### IL 2014 vws 2008 
my_plot <- CountyZoomChoropleth(choro_il_county_cagr_14v08, 
                                state_zoom = "illinois",
                                title      = "",
                                num_colors = 7,
                                legend = "emissions")

print(my_plot)
new_file_name = "C:/Users/Justin/Desktop/OSF_Inperson_Interview_2017-01-25/output/maps/IL.counties.emissions.growth.14v08.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()



#### US CAGR - 2011 vs 2008
choro <- CountyChoropleth$new(choro_us_county_cagr_11v08)
choro$title <- "PM2.5 Emission Growth Rate 2011 vs 2008"
choro$ggplot_scale = scale_fill_brewer(name="growth rate (%)",
                                       palette = "RdBu", 
                                       drop=FALSE,
                                       na.value = "grey90")
choro$render()

#### IL - 2011 vs 2008  
###### Legend doesn't lkook right....the negatives need to be red
my_plot <- county_choropleth(choro_il_county_cagr_11v08, 
                             state_zoom = "illinois",
                             title      = "",
                             num_colors = 7,
                             legend = "emissions")
print(my_plot)
new_file_name = "C:/Users/Justin/Desktop/OSF_Inperson_Interview_2017-01-25/output/maps/IL.counties.emissions.growth.11v08.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()

#### US -  2014 vs 2011 ####################################
choro <- CountyChoropleth$new(choro_us_county_cagr_14v11)
choro$title <- "PM2.5 Emission Growth Rate 2014 vs 2011"
choro$ggplot_scale = scale_fill_brewer(name="growth rate (%)",
                                       palette = "RdBu", 
                                       drop=FALSE,
                                       na.value = "grey90")
choro$render()

#### IL - 2014 vs 2011  
###### Legend doesn't lkook right....the negatives need to be red
my_plot <- county_choropleth(choro_il_county_cagr_14v11, 
                             state_zoom = "illinois",
                             title      = "",
                             num_colors = 7,
                             legend = "emissions")
print(my_plot)
new_file_name = "C:/Users/Justin/Desktop/OSF_Inperson_Interview_2017-01-25/output/maps/IL.counties.emissions.growth.11v08.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()

#### IL - 2014 vs 2011  
###### Legend doesn't lkook right....the negatives need to be red
my_plot <- county_choropleth(choro_il_county_cagr_14v11, 
                             state_zoom = "illinois",
                             title      = "",
                             num_colors = 7,
                             legend = "emissions")
print(my_plot)
new_file_name = "C:/Users/Justin/Desktop/OSF_Inperson_Interview_2017-01-25/output/maps/IL.counties.emissions.growth.11v08.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()

#### US - Deciles  ####################################
temp_df  <- us_county_master_data_cleaned[,c("fips","em_dens_decil_2014")]
colnames(temp_df)[1] <- "region"
colnames(temp_df)[2] <- "value"
choro <- CountyChoropleth$new(temp_df)
choro$title <- ""
choro$ggplot_scale = scale_fill_brewer(name="growth rate (%)",
                                       palette = 7, 
                                       drop=FALSE,
                                       na.value = "grey90")
choro$render()


########## IL County - Deciles - Attempt 1 -...can't get ride of blue
temp_df  <- us_county_master_data_cleaned[,c("fips","em_dens_decil_2014")]
colnames(temp_df)[1] <- "region"
colnames(temp_df)[2] <- "value"
temp_df  <- subset(temp_df, region>17000 & region<18000)

my_plot <- county_choropleth(temp_df, 
                             state_zoom = "illinois",
                             title      = "",
                             palette = 7,
                             legend = "emissions")
print(my_plot)



##############################################################################################################
##############################################################################################################
################################ Beg - Illinois Analysis #########################################
##############################################################################################################
##############################################################################################################


# Summary States on IL emissions detail 
summary(il_emissions_detail_2008$emissions)
summary(il_emissions_detail_2011$emissions)
summary(il_emissions_detail_2014$emissions)
summary(il_emissions_detail_all_years$emissions)


# Create A Boxplot of log(emissions) - IL Only 
boxplot(log(emissions)~inventory_year, data=il_emissions_detail_all_years,
        main=toupper(""), 
        font.main=3, cex.main=1.2, 
        xlab="Year",
        ylab="Log(Tons) of Emissions",
        font.lab=3,
        col="darkgreen")

#Histogram for IL Counties Total emission
hist(il_emissions_by_county_2014$emissions, 
     main="", 
     xlab="emissions", 
     border="white", 
     col="navy blue",
     ylim = c(0,40),
     breaks=20)



#################################################################################################
#################################################################################################
#################################################################################################
################################# Scatterplots #################################################
##################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################

#Primary Source: http://www.statmethods.net/graphs/scatterplot.html

################################## Land Area ##########################################
##### Take log oftranform using log function #####   - ARCHIVING 
y <- log(us_county_master_data$Yr_2014+.000001)
x <- log(us_county_master_data$area.land+.000001)
#### Scatterplot 1a -  Using Hexaxonal bin
bin<-hexbin(x, y, xbins=50) 
my_plot <- plot(bin, 
     main="", 
     xlab="Log Land Area (Log(Square Miles)) ",
     ylab="Log PM2.5 Emissions (Log (tons))")
print(my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\Log_Land_area.Log_emissions.hexagons.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()

#### Scatterplot 1b - Transsparent Green points LIke this one the best 
y <- log(us_county_master_data$Yr_2014+.000001)
x <- log(us_county_master_data$area.land+.000001)
my_plot <- plot(x,y, 
     main="", 
     xlab="Log Land Area (Log(Square Miles)) ",
     ylab="Log PM2.5 Emissions (Log (tons))",
     col=rgb(0,100,0,50,maxColorValue=255),
     pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
print(my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\Log_Land_area.Log_emissions.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))

##### Print out Pearson Correlations Coefficient
# Run Correlation 
#cor(inf2NA(x), inf2NA(y), use="pairwise.complete.obs",method = "pearson")
cor.test(inf2NA(x), inf2NA(y))

#Example: from Source:http://adv-r.had.co.nz/Subsetting.html 
# Scatter Plot 1C  IL - Land Area
y <- log(il_county_master_data_cleaned$Yr_2014+.000001)
x <- log(il_county_master_data_cleaned$area.land+.000001)
my_plot <- plot(x,y, 
                main="", 
                xlab="Log Land Area (Log(sq mi))",
                ylab="Log Pm2.5 Emissions (log(tons)",
                col="dark green",
                pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
#lines(lowess(x,y), col="blue") # lowess line (x,y)
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\IL_counties_land.area_v_emissin.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))


################################## Population  ##########################################
##### Scatterplot 2a -  US Log Population  vs log emission 2014 #####
y <- us_county_master_data_cleaned$log.emissions.2014 
x <- us_county_master_data_cleaned$log.pop
my_plot <- plot(x,y, 
     main="", 
     xlab="Log(Population)",
     ylab="Log(PM2.5 Emissions) log(tons)",
     col=rgb(0,100,0,50,maxColorValue=255),
     pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
print(my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\logPop_v_LogEmissions.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))

#Print Pearson Correlation
cor(inf2NA(x), inf2NA(y), use="pairwise.complete.obs",method = "pearson")
cor.test(inf2NA(x), inf2NA(y))


# Scatter Plot 2b IL - Log Populations
y <- log(il_county_master_data_cleaned$Yr_2014)
x <- log(il_county_master_data_cleaned$population)

my_plot <- plot(x,y, 
                main="", 
                xlab="Log Population (Log # Person)",
                ylab="Log Emissions (Log tons)",
                col="dark green",
                pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
#lines(lowess(x,y), col="blue") # lowess line (x,y)
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\IL_counties_log.pop_v_log.emissin.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))

################################## Emissions Density (emissions / square mile)   ##########################################

# Scatterplot 3a - US Emissions Density  vs Pop 
y <- log(us_county_master_data_cleaned$emissions_density_2014)
x <- log(us_county_master_data_cleaned$population)
my_plot <- plot(x,y, 
     main="", 
     xlab="Log Population [log(# persons)]",
     ylab="Log Emissions Density [log(tons/sq mi)]",
     col=rgb(0,100,0,50,maxColorValue=255),
     pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
#lines(lowess(x,y), col="blue") # lowess line (x,y)
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\log.pop.density_v_log.emissions.density.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()

#Print Pearson Correlation
cor(inf2NA(x), inf2NA(y), use="pairwise.complete.obs",method = "pearson")
cor.test(inf2NA(x), inf2NA(y))


# ScatterPlot 3b -   IL - Population Density
y <- log(il_county_master_data_cleaned$emissions_density_2014)
x <-  log(il_county_master_data_cleaned$pop.density)
my_plot <- plot(x,y, 
                main="", 
                xlab="Log(Population Density), log(pop/sq mi)",
                ylab="Log(PM2.5 Emissions Density) log(tons/sq mi)",
                col="dark green",
                pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
#lines(lowess(x,y), col="blue") # lowess line (x,y)
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\IL_counties_log.emission.den_log.pop.den.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))

# Scatterplot 8a - US -  Housing Units 
y <- log(us_county_master_data_cleaned$emissions_density_2014)
x <- log(us_county_master_data_cleaned$housing.units)
my_plot <- plot(x,y, 
                main="", 
                xlab="Log Housing Units [log(# units)]",
                ylab="Log Emissions Density [log(tons/sq mi)]",
                col=rgb(0,100,0,50,maxColorValue=255),
                pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
#lines(lowess(x,y), col="blue") # lowess line (x,y)
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\US_log.housing.units_v_log.emissions.density.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))

# Scatterplot 8b - IL -  Housing Units 
y <- log(il_county_master_data_cleaned$emissions_density_2014)
x <- log(il_county_master_data_cleaned$housing.units)
my_plot <- plot(x,y, 
                main="", 
                xlab="Log Housing Units [log(# units)]",
                ylab="Log Emissions Density [log(tons/sq mi)]",
                col="dark green",
                pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
#lines(lowess(x,y), col="blue") # lowess line (x,y)
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\IL_log.housing.units_v_log.emissions.density.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))

# Scatterplot 9a - US -  Housing Density  
y <- log(us_county_master_data_cleaned$emissions_density_2014)
x <- log(us_county_master_data_cleaned$housing.density)
my_plot <- plot(x,y, 
                main="", 
                xlab="Log Housing Denisty [log(# units/sq mi)]",
                ylab="Log Emissions Density [log(tons/sq mi)]",
                col=rgb(0,100,0,50,maxColorValue=255),
                pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
#lines(lowess(x,y), col="blue") # lowess line (x,y)
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\US_log.housing.dens_v_log.emissions.density.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))

# Scatterplot 9b - IL -  Housing Density  
y <- log(il_county_master_data_cleaned$emissions_density_2014)
x <- log(il_county_master_data_cleaned$housing.density)
my_plot <- plot(x,y, 
                main="", 
                xlab="Log Housing Units [log(# units)]",
                ylab="Log Emissions Density [log(tons/sq mi)]",
                col="dark green",
                pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
#lines(lowess(x,y), col="blue") # lowess line (x,y)
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\IL_log.housing.dens_v_log.emissions.density.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))


################## Sector Specific #########################################
################ Ag Dust ################
####  Scatterplot 4a - US Pop Density on Ag Dust 
y <- log(us_county_master_data_cleaned$agdust_emissions_2014)
x <- log(us_county_master_data_cleaned$pop.density)
my_plot <- plot(x,y, 
               main="", 
               xlab="Population Density (persons/sq mi)",
               ylab="Emissions from Ag Crop & Livestock (tons)",
               col=rgb(0,100,0,50,maxColorValue=255),
               pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\US_agdust_log.pop.density_V_log.agdust.emissions.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))
# Conclusion: -.23, Population density produces LOWER Emission density (in Ag Crop) 

####  Scatterplot 4b - Pop Density on Agdust 
temp_df <- subset(us_county_master_data_cleaned, fips>17000 & fips<18000)
y <- log(temp_df$agdust_emissions_2014)
x <- log(temp_df$pop.density)
my_plot <- plot(x,y, 
                main="", 
                xlab="Log Population Density(persons/sq mi)",
                ylab="Log Emissions from Ag Crop & Livestock (log(tons)",
                col=rgb(0,100,0,50,maxColorValue=255),
                pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\IL_agdust_log.pop.density_V_log.agdust.emissions.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))

# Conclusion: -.50, Population density produces LOWER Emission density (in Ag Crop) 

####  Scatterplot 5a  - US Ag Crop - Land Area  
y <- log(us_county_master_data_cleaned$agdust_emissions_2014)
x <- log(us_county_master_data_cleaned$area.land)
myplot <- plot(x,y, 
               main="", 
               xlab="Log Land Area (sq mi)",
               ylab="Log Agdust Emissions (tons)",
               col=rgb(0,100,0,50,maxColorValue=255),
               pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\US.ag.crop.dust_2014.emissions_v_land.area.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))


####  Scatterplot 5b  - IL Ag Crop - Land Area  
temp_df <- subset(us_county_master_data_cleaned, fips>17000 & fips<18000)
y <- temp_df$agdust_emissions_2014
x <- temp_df$area.land
myplot <- plot(x,y, 
     main="", 
     xlab="Log Land Area (Log (sq mi))",
     ylab="Log Agdust Emissions (Log(tons))",
     col="dark green",
     pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
print (my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\il.ag.crop.dust_2014.emissions_v_land.area.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))

### Scatterplot 6a US Harvested Acres vs IL Emissions FROM AG 
#No Data for US so dont have 

### Scatterplot 6b IL Harvested Acres vs IL Emissions FROM AG 
temp_df <- subset(us_county_master_data_cleaned, fips>17000 & fips<18000)
y <- temp_df$agdust_emissions_2014
x <- temp_df$acres_harvest_2012
my_plot <- plot(x,y, 
                main="", 
                xlab="Acres Harvested (acres)",
                ylab="Agdust Emissions (tons)",
                col="dark green",
                pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
print(my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\il.agr.crop.dust.emission_vs_il.acres.harvested.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))


# Scatterplot 7 - Unpaved Road Dusts subsett 

y <- il_county_master_data_cleaned$unpaved_road_emissions_2014
x <- log(il_county_master_data_cleaned$Annual_VMT_2014)
my_plot <- plot(x,y, 
                main="", 
                xlab="Log Annual Vehicle Miles Traveled (miles)",
                ylab="PM2.5 Emissions log(tons)",
                col= "dark green",
                pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
print(my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\il.unpaved.road.dust.emission_vs_annual.vehicle.road.miles.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))

# Scatterplot 8 - Paved Road Dusts subsett 
y <- log(il_county_master_data_cleaned$paved_road_emissions_2014)
x <- log(il_county_master_data_cleaned$Annual_VMT_2014)
my_plot <- plot(x,y, 
                main="", 
                xlab="Log Annual Vehicle Miles Traveled (miles)",
                ylab="PM2.5 Emissions log(tons)",
                col= "dark green",
                pch=16)
abline(lm(y~x), col="red") # regression line (y~x) 
print(my_plot)
new_file_name = "C:\\Users\\Justin\\Desktop\\OSF_Inperson_Interview_2017-01-25\\output\\scatterplot\\il.paved.road.dust.emission_vs_annual.vehicle.road.miles.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()
cor.test(inf2NA(x), inf2NA(y))



############# Compare Emission Density IL vs US ##########
# Give basic summary stats
summary(choro_il_county_emission_density$value)
summary(choro_us_county_emission_density_2014$value)

# Bind together 2 dataframe for use in boxplot
choro_il_county_emission_density$geo <- "IL"
choro_us_county_emission_density_2014$geo <- "US"
boxplot_a <- rbind(choro_il_county_emission_density,choro_us_county_emission_density_2014)

# Create A Boxplot comparing 2 geos  (eliminate 6 large outliers in US  ) 
my_plot <- boxplot(value ~ geo,
        data=boxplot_a,
        main=toupper(""), 
        font.main=3, cex.main=1.2, 
        xlab="Geographic Region",
        ylab="Emissions Density (tons/sq mi)",
        font.lab=3,
        ylim = c(0,25),
        col="darkgreen")
print(my_plot)
new_file_name = "C:/Users/Justin/Desktop/OSF_Inperson_Interview_2017-01-25/output/histogram/US_v_IL_em.den.jpeg"
dev.copy(jpeg, filename = new_file_name)
dev.off()


#Histogram for IL Counties emissions density
my_plot <- hist(choro_il_county_emission_density$value, 
     main="", 
     xlab="emissions density (tons/sq mi)", 
     border="white", 
     col="navy blue",
     las=1, 
     xlim = c(0,25),
     ylim = c(0,40),
     breaks=20)
print(my_plot)
new_file_name = "C:/Users/Justin/Desktop/OSF_Inperson_Interview_2017-01-25/output/histogram/il_emission_density.jpg"
dev.copy(jpeg, filename = new_file_name)
dev.off()




# Boxplot 

my_plot <- boxplot(emissions_density_2014 ~ em_dens_decil_2014,
                   data=us_county_master_data_cleaned,
                   main=toupper(""), 
                   font.main=3, cex.main=1.2, 
                   xlab="Deciles",
                   ylab="Emissions Density (tons/sq mi)",
                   font.lab=3,
                   ylim = c(0,140),
                   col="darkgreen")
print(my_plot)
new_file_name = "C:/Users/Justin/Desktop/OSF_Inperson_Interview_2017-01-25/output/boxplot/US_em.den_by_decil.jpeg"
dev.copy(jpeg, filename = new_file_name)
dev.off()

my_plot <- boxplot(emissions_density_2014 ~ em_dens_decil_2014,
                   data=us_county_master_data_cleaned,
                   main=toupper(""), 
                   font.main=3, cex.main=1.2, 
                   xlab="Deciles",
                   ylab="Emissions Density (tons/sq mi)",
                   font.lab=3,
                   ylim = c(0,80),
                   col="darkgreen")
print(my_plot)
new_file_name = "C:/Users/Justin/Desktop/OSF_Inperson_Interview_2017-01-25/output/boxplot/US_em.den_by_decil_truncx.jpeg"
dev.copy(jpeg, filename = new_file_name)
dev.off()

summary(us_county_master_data_cleaned$emissions_density_2014)
