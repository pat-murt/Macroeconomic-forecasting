
library(readxl) # Read/write data files
library(tidyverse) # Collection of packages
library(ggplot2) # Visualisation
library(dplyr) # Manipulating data
library(caret) # Cross Validation models
library(corrplot) # Correlation Plot
library(ggrepel) # GGPLOT label lines
library(psych) # Functions including describeBy
library(gridExtra)
library(RColorBrewer) # GGPLOT Colours
library(cluster) # For cluster analysis
library(car) # Includes VIF function
library(rpart) # Decision Trees
library(rpart.plot) # Plots for Decision Trees
library(ggfortify)
library(flextable)
library(knitr)
library(writexl)
library(grid)
library(moments)
library(ggcorrplot)
library(officer)

#Set the WD and read the data in
setwd('E:/Dissertation/New datasets WB/P_Data_Extract_From_World_Development_Indicators')
options(scipen = 999)
data <- read_csv('WB data Fri.csv')

summary(data)
colSums(is.na(data))

data <- data %>%
  select(-c("Time Code", "Country Code", "Employers, total (% of total employment) (modeled ILO estimate) [SL.EMP.MPYR.ZS]"
  ))

names(data)[names(data) == "Time"] <- "year"
names(data)[names(data) == "Country Name"] <- "country"
names(data)[names(data) == "Access to electricity (% of population) [EG.ELC.ACCS.ZS]"] <- "access_electricity"
names(data)[names(data) == "Adjusted net national income (current US$) [NY.ADJ.NNTY.CD]"] <- "net_income"
names(data)[names(data) == "Agricultural land (% of land area) [AG.LND.AGRI.ZS]"] <- "agricultural_land"
names(data)[names(data) == "Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]"] <- "agricultural_forestry_fishing"
names(data)[names(data) == "Armed forces personnel (% of total labor force) [MS.MIL.TOTL.TF.ZS]"] <- "armed_forces"
names(data)[names(data) == "Birth rate, crude (per 1,000 people) [SP.DYN.CBRT.IN]"] <- "births"
names(data)[names(data) == "Births attended by skilled health staff (% of total) [SH.STA.BRTC.ZS]"] <- "births_attended"
names(data)[names(data) == "Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e) [EN.GHG.CO2.MT.CE.AR5]"] <- "co2"
names(data)[names(data) == "Central government debt, total (% of GDP) [GC.DOD.TOTL.GD.ZS]"] <- "gov_dept"
names(data)[names(data) == "Children out of school (% of primary school age) [SE.PRM.UNER.ZS]"] <- "children_out_of_school"
names(data)[names(data) == "Combustible renewables and waste (% of total energy) [EG.USE.CRNW.ZS]"] <- "combustible_renewables"
names(data)[names(data) == "Commercial service exports (current US$) [TX.VAL.SERV.CD.WT]"] <- "exports"
names(data)[names(data) == "Commercial service imports (current US$) [TM.VAL.SERV.CD.WT]"] <- "imports"
names(data)[names(data) == "Compulsory education, duration (years) [SE.COM.DURS]"] <- "compulsory_education"
names(data)[names(data) == "Control of Corruption: Estimate [CC.EST]"] <- "corruption"
names(data)[names(data) == "Current health expenditure (% of GDP) [SH.XPD.CHEX.GD.ZS]"] <- "health_expenditure"
names(data)[names(data) == "Current health expenditure per capita (current US$) [SH.XPD.CHEX.PC.CD]"] <- "health_expen_pcapita"
names(data)[names(data) == "Death rate, crude (per 1,000 people) [SP.DYN.CDRT.IN]"] <- "deaths"
names(data)[names(data) == "Electric power consumption (kWh per capita) [EG.USE.ELEC.KH.PC]"] <- "electric_consumption"
names(data)[names(data) == "Electricity production from coal sources (% of total) [EG.ELC.COAL.ZS]"] <- "electric_coal"
names(data)[names(data) == "Expenditure on primary education (% of government expenditure on education) [SE.XPD.PRIM.ZS]"] <- "primary_expenditure"
names(data)[names(data) == "Forest area (% of land area) [AG.LND.FRST.ZS]"] <- "forest"
names(data)[names(data) == "GDP (current US$) [NY.GDP.MKTP.CD]"] <- "gdp"
names(data)[names(data) == "International tourism, expenditures (current US$) [ST.INT.XPND.CD]"] <- "tourism"
names(data)[names(data) == "Labor force participation rate, total (% of total population ages 15+) (national estimate) [SL.TLF.CACT.NE.ZS]"] <- "labour_participation"
names(data)[names(data) == "Labor force with basic education (% of total working-age population with basic education) [SL.TLF.BASC.ZS]"] <- "labour_education"
names(data)[names(data) == "Labor force with intermediate education (% of total working-age population with intermediate education) [SL.TLF.INTM.ZS]"] <- "labour_intermediate"
names(data)[names(data) == "Life expectancy at birth, total (years) [SP.DYN.LE00.IN]"] <- "life_expectancy"
names(data)[names(data) == "Literacy rate, adult total (% of people ages 15 and above) [SE.ADT.LITR.ZS]"] <- "adult_literacy"
names(data)[names(data) == "Military expenditure (% of GDP) [MS.MIL.XPND.GD.ZS]"] <- "military_expenditure"
names(data)[names(data) == "Net migration [SM.POP.NETM]"] <- "net_migration"
names(data)[names(data) == "Physicians (per 1,000 people) [SH.MED.PHYS.ZS]"] <- "physicians"
names(data)[names(data) == "Population, total [SP.POP.TOTL]"] <- "population"
names(data)[names(data) == "Power outages in firms in a typical month (number) [IC.ELC.OUTG]"] <- "power_outages"
names(data)[names(data) == "Rural population (% of total population) [SP.RUR.TOTL.ZS]"] <- "rural_population"
names(data)[names(data) == "Self-employed, total (% of total employment) (modeled ILO estimate) [SL.EMP.SELF.ZS]"] <- "self_employed"
names(data)[names(data) == "Social contributions (% of revenue) [GC.REV.SOCL.ZS]"] <- "social_contributions"
names(data)[names(data) == "Suicide mortality rate (per 100,000 population) [SH.STA.SUIC.P5]"] <- "suicide"
names(data)[names(data) == "Total greenhouse gas emissions including LULUCF (Mt CO2e) [EN.GHG.ALL.LU.MT.CE.AR5]"] <- "gg_emissions"
names(data)[names(data) == "Unemployment, total (% of total labor force) (national estimate) [SL.UEM.TOTL.NE.ZS]"] <- "unemployment"
names(data)[names(data) == "Urban population (% of total population) [SP.URB.TOTL.IN.ZS]"] <- "urban_population"


#Changing Datatypes
data$year <- as.numeric(data$year)
data$access_electricity <- as.numeric(data$access_electricity)
data$net_income <- as.numeric(data$net_income)
data$agricultural_land <- as.numeric(data$agricultural_land)
data$agricultural_forestry_fishing <- as.numeric(data$agricultural_forestry_fishing)
data$armed_forces <- as.numeric(data$armed_forces)
data$births <- as.numeric(data$births)
data$births_attended <- as.numeric(data$births_attended)
data$co2 <- as.numeric(data$co2)
data$gov_dept <- as.numeric(data$gov_dept)
data$children_out_of_school <- as.numeric(data$children_out_of_school)
data$combustible_renewables <- as.numeric(data$combustible_renewables)
data$exports <- as.numeric(data$exports)
data$imports <- as.numeric(data$imports)
data$compulsory_education <- as.numeric(data$compulsory_education)
data$corruption <- as.numeric(data$corruption)
data$health_expenditure <- as.numeric(data$health_expenditure)
data$health_expen_pcapita <- as.numeric(data$health_expen_pcapita)
data$deaths <- as.numeric(data$deaths)
data$electric_consumption <- as.numeric(data$electric_consumption)
data$electric_coal <- as.numeric(data$electric_coal)
data$primary_expenditure <- as.numeric(data$primary_expenditure)
data$forest <- as.numeric(data$forest)
data$gdp <- as.numeric(data$gdp)
data$tourism <- as.numeric(data$tourism)
data$labour_participation <- as.numeric(data$labour_participation)
data$labour_education <- as.numeric(data$labour_education)
data$labour_intermediate <- as.numeric(data$labour_intermediate)
data$life_expectancy <- as.numeric(data$life_expectancy)
data$adult_literacy <- as.numeric(data$adult_literacy)
data$military_expenditure <- as.numeric(data$military_expenditure)
data$net_migration <- as.numeric(data$net_migration)
data$physicians <- as.numeric(data$physicians)
data$population <- as.numeric(data$population)
data$power_outages <- as.numeric(data$power_outages)
data$rural_population <- as.numeric(data$rural_population)
data$self_employed <- as.numeric(data$self_employed)
data$social_contributions <- as.numeric(data$social_contributions)
data$suicide <- as.numeric(data$suicide)
data$gg_emissions <- as.numeric(data$gg_emissions)
data$unemployment <- as.numeric(data$unemployment)
data$urban_population <- as.numeric(data$urban_population)

#Identifying all countries
unique_countries <- unique(data$country)

#Adding a 'Region' Column
data$region <- NA

#Assigning Asian regions
data$region[data$country=='Afghanistan']<-'Asia'
data$region[data$country=='Bahrain']<-'Asia'
data$region[data$country=='Bangladesh']<-'Asia'
data$region[data$country=='Bhutan']<-'Asia'
data$region[data$country=='Brunei Darussalam']<-'Asia'
data$region[data$country=='Cambodia']<-'Asia'
data$region[data$country=='China']<-'Asia'
data$region[data$country=='Hong Kong SAR, China']<-'Asia'
data$region[data$country=='India']<-'Asia'
data$region[data$country=='Indonesia']<-'Asia'
data$region[data$country=='Iran, Islamic Rep.']<-'Asia'
data$region[data$country=='Iraq']<-'Asia'
data$region[data$country=='Japan']<-'Asia'
data$region[data$country=='Jordan']<-'Asia'
data$region[data$country=='Kazakhstan']<-'Asia'
data$region[data$country=="Korea, Dem. People's Rep."]<-'Asia'
data$region[data$country=='Korea, Rep.']<-'Asia'
data$region[data$country=='Kuwait']<-'Asia'
data$region[data$country=='Kyrgyz Republic']<-'Asia'
data$region[data$country=='Lao PDR']<-'Asia'
data$region[data$country=='Lebanon']<-'Asia'
data$region[data$country=='Macao SAR, China']<-'Asia'
data$region[data$country=='Malaysia']<-'Asia'
data$region[data$country=='Maldives']<-'Asia'
data$region[data$country=='Mongolia']<-'Asia'
data$region[data$country=='Myanmar']<-'Asia'
data$region[data$country=='Nepal']<-'Asia'
data$region[data$country=='Oman']<-'Asia'
data$region[data$country=='Pakistan']<-'Asia'
data$region[data$country=='Philippines']<-'Asia'
data$region[data$country=='Qatar']<-'Asia'
data$region[data$country=='Saudi Arabia']<-'Asia'
data$region[data$country=='Singapore']<-'Asia'
data$region[data$country=='Sri Lanka']<-'Asia'
data$region[data$country=='Syrian Arab Republic']<-'Asia'
data$region[data$country=='Tajikistan']<-'Asia'
data$region[data$country=='Thailand']<-'Asia'
data$region[data$country=='Timor-Leste']<-'Asia'
data$region[data$country=='Turkmenistan']<-'Asia'
data$region[data$country=='United Arab Emirates']<-'Asia'
data$region[data$country=='Uzbekistan']<-'Asia'
data$region[data$country=='Viet Nam']<-'Asia'
data$region[data$country=='West Bank and Gaza']<-'Asia'
data$region[data$country=='Yemen, Rep.']<-'Asia'

#Assigning European regions
data$region[data$country=='Albania']<-'Europe'
data$region[data$country=='Andorra']<-'Europe'
data$region[data$country=='Austria']<-'Europe'
data$region[data$country=='Belarus']<-'Europe'
data$region[data$country=='Belgium']<-'Europe'
data$region[data$country=='Bosnia and Herzegovina']<-'Europe'
data$region[data$country=='Bulgaria']<-'Europe'
data$region[data$country=='Croatia']<-'Europe'
data$region[data$country=='Czechia']<-'Europe'
data$region[data$country=='Denmark']<-'Europe'
data$region[data$country=='Estonia']<-'Europe'
data$region[data$country=='Finland']<-'Europe'
data$region[data$country=='France']<-'Europe'
data$region[data$country=='Germany']<-'Europe'
data$region[data$country=='Greece']<-'Europe'
data$region[data$country=='Greenland']<-'Europe'
data$region[data$country=='Hungary']<-'Europe'
data$region[data$country=='Iceland']<-'Europe'
data$region[data$country=='Ireland']<-'Europe'
data$region[data$country=='Isle of Man']<-'Europe'
data$region[data$country=='Italy']<-'Europe'
data$region[data$country=='Kosovo']<-'Europe'
data$region[data$country=='Latvia']<-'Europe'
data$region[data$country=='Liechtenstein']<-'Europe'
data$region[data$country=='Lithuania']<-'Europe'
data$region[data$country=='Luxembourg']<-'Europe'
data$region[data$country=='Malta']<-'Europe'
data$region[data$country=='Moldova']<-'Europe'
data$region[data$country=='Monaco']<-'Europe'
data$region[data$country=='Montenegro']<-'Europe'
data$region[data$country=='Netherlands']<-'Europe'
data$region[data$country=='North Macedonia']<-'Europe'
data$region[data$country=='Norway']<-'Europe'
data$region[data$country=='Poland']<-'Europe'
data$region[data$country=='Portugal']<-'Europe'
data$region[data$country=='Romania']<-'Europe'
data$region[data$country=='San Marino']<-'Europe'
data$region[data$country=='Serbia']<-'Europe'
data$region[data$country=='Slovak Republic']<-'Europe'
data$region[data$country=='Slovenia']<-'Europe'
data$region[data$country=='Spain']<-'Europe'
data$region[data$country=='Sweden']<-'Europe'
data$region[data$country=='Switzerland']<-'Europe'
data$region[data$country=='Ukraine']<-'Europe'
data$region[data$country=='United Kingdom']<-'Europe'
data$region[data$country=='Albania']<-'Europe'

#Assigning African Regions
data$region[data$country=='Algeria']<-'Africa'
data$region[data$country=='Angola']<-'Africa'
data$region[data$country=='Benin']<-'Africa'
data$region[data$country=='Botswana']<-'Africa'
data$region[data$country=='Burkina Faso']<-'Africa'
data$region[data$country=='Burundi']<-'Africa'
data$region[data$country=='Cabo Verde']<-'Africa'
data$region[data$country=='Cameroon']<-'Africa'
data$region[data$country=='Central African Republic']<-'Africa'
data$region[data$country=='Chad']<-'Africa'
data$region[data$country=='Comoros']<-'Africa'
data$region[data$country=='Congo, Dem. Rep.']<-'Africa'
data$region[data$country=='Congo, Rep.']<-'Africa'
data$region[data$country=="Cote d'Ivoire"]<-'Africa'
data$region[data$country=='Djibouti']<-'Africa'
data$region[data$country=='Egypt, Arab Rep.']<-'Africa'
data$region[data$country=='Equatorial Guinea']<-'Africa'
data$region[data$country=='Eritrea']<-'Africa'
data$region[data$country=='Eswatini']<-'Africa'
data$region[data$country=='Ethiopia']<-'Africa'
data$region[data$country=='Gabon']<-'Africa'
data$region[data$country=='Gambia, The']<-'Africa'
data$region[data$country=='Ghana']<-'Africa'
data$region[data$country=='Guinea']<-'Africa'
data$region[data$country=='Guinea-Bissau']<-'Africa'
data$region[data$country=='Kenya']<-'Africa'
data$region[data$country=='Lesotho']<-'Africa'
data$region[data$country=='Liberia']<-'Africa'
data$region[data$country=='Libya']<-'Africa'
data$region[data$country=='Madagascar']<-'Africa'
data$region[data$country=='Malawi']<-'Africa'
data$region[data$country=='Mali']<-'Africa'
data$region[data$country=='Mauritania']<-'Africa'
data$region[data$country=='Mauritius']<-'Africa'
data$region[data$country=='Morocco']<-'Africa'
data$region[data$country=='Mozambique']<-'Africa'
data$region[data$country=='Namibia']<-'Africa'
data$region[data$country=='Niger']<-'Africa'
data$region[data$country=='Nigeria']<-'Africa'
data$region[data$country=='Rwanda']<-'Africa'
data$region[data$country=='Sao Tome and Principe']<-'Africa'
data$region[data$country=='Senegal']<-'Africa'
data$region[data$country=='Seychelles']<-'Africa'
data$region[data$country=='Sierra Leone']<-'Africa'
data$region[data$country=='Somalia']<-'Africa'
data$region[data$country=='South Africa']<-'Africa'
data$region[data$country=='South Sudan']<-'Africa'
data$region[data$country=='Sudan']<-'Africa'
data$region[data$country=='Tanzania']<-'Africa'
data$region[data$country=='Togo']<-'Africa'
data$region[data$country=='Tunisia']<-'Africa'
data$region[data$country=='Uganda']<-'Africa'
data$region[data$country=='Zambia']<-'Africa'
data$region[data$country=='Zimbabwe']<-'Africa'

#Assigning the 'Americas' regions
data$region[data$country=='Antigua and Barbuda']<-'Americas'
data$region[data$country=='Argentina']<-'Americas'
data$region[data$country=='Aruba']<-'Americas'
data$region[data$country=='Bahamas, The']<-'Americas'
data$region[data$country=='Barbados']<-'Americas'
data$region[data$country=='Belize']<-'Americas'
data$region[data$country=='Bermuda']<-'Americas'
data$region[data$country=='Bolivia']<-'Americas'
data$region[data$country=='Brazil']<-'Americas'
data$region[data$country=='Canada']<-'Americas'
data$region[data$country=='Cayman Islands']<-'Americas'
data$region[data$country=='Chile']<-'Americas'
data$region[data$country=='Colombia']<-'Americas'
data$region[data$country=='Costa Rica']<-'Americas'
data$region[data$country=='Cuba']<-'Americas'
data$region[data$country=='Dominica']<-'Americas'
data$region[data$country=='Dominican Republic']<-'Americas'
data$region[data$country=='Ecuador']<-'Americas'
data$region[data$country=='El Salvador']<-'Americas'
data$region[data$country=='Grenada']<-'Americas'
data$region[data$country=='Guatemala']<-'Americas'
data$region[data$country=='Guyana']<-'Americas'
data$region[data$country=='Haiti']<-'Americas'
data$region[data$country=='Honduras']<-'Americas'
data$region[data$country=='Jamaica']<-'Americas'
data$region[data$country=='Mexico']<-'Americas'
data$region[data$country=='Nicaragua']<-'Americas'
data$region[data$country=='Panama']<-'Americas'
data$region[data$country=='Paraguay']<-'Americas'
data$region[data$country=='Peru']<-'Americas'
data$region[data$country=='Puerto Rico']<-'Americas'
data$region[data$country=='Sint Maarten (Dutch part)']<-'Americas'
data$region[data$country=='St. Kitts and Nevis']<-'Americas'
data$region[data$country=='St. Lucia']<-'Americas'
data$region[data$country=='St. Martin (French part)']<-'Americas'
data$region[data$country=='St. Vincent and the Grenadines']<-'Americas'
data$region[data$country=='Suriname']<-'Americas'
data$region[data$country=='Trinidad and Tobago']<-'Americas'
data$region[data$country=='Turks and Caicos Islands']<-'Americas'
data$region[data$country=='United States']<-'Americas'
data$region[data$country=='Uruguay']<-'Americas'
data$region[data$country=='Venezuela RB']<-'Americas'
data$region[data$country=='Virgin Islands (U.S.)']<-'Americas'

#Assigning Asia/Europe regions
data$region[data$country=='Armenia']<-'Asia/Europe'
data$region[data$country=='Azerbaijan']<-'Asia/Europe'
data$region[data$country=='Cyprus']<-'Asia/Europe'
data$region[data$country=='Georgia']<-'Asia/Europe'
data$region[data$country=='Israel']<-'Asia/Europe'
data$region[data$country=='Russian Federation']<-'Asia/Europe'
data$region[data$country=='Turkey']<-'Asia/Europe'

#Assinging Oceania
data$region[data$country=='Australia']<-'Oceania'
data$region[data$country=='Fiji']<-'Oceania'
data$region[data$country=='Guam']<-'Oceania'
data$region[data$country=='Kiribati']<-'Oceania'
data$region[data$country=='Marshall Islands']<-'Oceania'
data$region[data$country=='Micronesia, Fed. Sts.']<-'Oceania'
data$region[data$country=='Nauru']<-'Oceania'
data$region[data$country=='New Caledonia']<-'Oceania'
data$region[data$country=='New Zealand']<-'Oceania'
data$region[data$country=='Northern Mariana Islands']<-'Oceania'
data$region[data$country=='Palau']<-'Oceania'
data$region[data$country=='Papua New Guinea']<-'Oceania'
data$region[data$country=='Samoa']<-'Oceania'
data$region[data$country=='Solomon Islands']<-'Oceania'
data$region[data$country=='Tonga']<-'Oceania'
data$region[data$country=='Tuvalu']<-'Oceania'
data$region[data$country=='Vanuatu']<-'Oceania'

data$region <- as.factor(data$region)
data$country <- as.factor(data$country)

data <- data %>%
  filter(country != 'Data from database: World Development Indicators')
data <- data %>%
  filter(country != 'Last Updated: 06/05/2025')

###################################################

### 

###################################################

#Checking NA values by year
for (yr in 1960:2025) {
  assign(paste0("year", yr), filter(data, year == yr))
}

#defining the empty NA count list
na_counts_list <- list()

# Calculate NA counts for each column
for (yr in 1960:2025) {
  dataset <- get(paste0("year", yr))
  na_counts_list[[as.character(yr)]] <- colSums(is.na(dataset))
}

# Convert list to data frame
na_counts_df <- do.call(rbind, lapply(names(na_counts_list), function(y) {
  data.frame(Year = y, t(na_counts_list[[y]]))
}))

#Calculating the total number of NA values per year
na_counts_df$Total_NA <- rowSums(na_counts_df[ , -which(names(na_counts_df)=="Year")])

#% of the values missing per year
na_counts_df$NA_Percentage <- (na_counts_df$Total_NA / 9548) * 100

# Calculate column totals
column_totals <- colSums(na_counts_df[, -1], na.rm = TRUE)

# Create a new row for totals
total_row <- c(Year = "Total", column_totals)

# Bind the total row to the existing dataframe
na_counts_df <- rbind(na_counts_df, total_row)

cols_to_check <- setdiff(names(data), c("country", "year"))
na_summary <- data %>%
  group_by(country, year) %>%
  summarize(total_na = sum(across(all_of(cols_to_check), ~ sum(is.na(.)), .names = "NA_{col}")),
            .groups = "drop") %>%
  ungroup()

#Creatign table for easier viewing
na_wide <- na_summary %>%
  pivot_wider(names_from = year, values_from = total_na)



# Get column names
variable_names <- colnames(na_counts_df)

# Get the 'total' row (which is row 67)
total_row <- na_counts_df[67, ]

# Combine variable names with the total row values into a data frame
subset_df <- data.frame(
  Variable = variable_names,
  Total = as.numeric(total_row)
)

subset_df$percent_na <- subset_df$Total/14322

#write.csv(subset_df, file = "var_na.csv", row.names = FALSE)
###################################################

# Removing values
data <- data[data$year >= 2001 ,]
data <- data[data$year <= 2021 ,]

#checking the NA values by variable after limiting the years
na_counts_01_21 <- data.frame(
  Variable = names(data),
  NA_Count = sapply(data, function(x) sum(is.na(x)))
)

#calculating a % of NA values for this dataframe
na_counts_01_21$percent_na <- na_counts_01_21$NA_Count/4557
#write.csv(na_counts_01_21, file = "var_na_2001-2021.csv", row.names = FALSE)


data <- data %>% 
  select(-c(births_attended, gov_dept, armed_forces, children_out_of_school,
            combustible_renewables, exports, imports, compulsory_education,
            corruption, health_expenditure, health_expen_pcapita,
            electric_consumption, electric_coal, primary_expenditure,
            tourism, labour_education, labour_intermediate,
            adult_literacy, physicians, power_outages, self_employed,
            social_contributions, suicide, gg_emissions, unemployment,
            net_income, labour_participation, military_expenditure,
            #Time, 
            #`Time Code`, `Country Code`, `Employers, total (% of total employment) (modeled ILO estimate) [SL.EMP.MPYR.ZS]`)
            ))

data <- na.omit(data)
colSums(is.na(data))

###################################################

#Exporting na_counts_df
na_year <- na_counts_df %>%
  select(Year, NA_Percentage)
#write.csv(na_year, file = "na_year.csv", row.names = FALSE)

na_var <- na_counts_df %>%
  select(Year, NA_Percentage)
#write.csv(na_year, file = "na_year.csv", row.names = FALSE)
# DO THE SAME FOR VARIABLES AND TOTAL MISSING NA

#Writing the csv file
#write.csv(data, file = "WB_Inidicators_clean_V4.csv", row.names = FALSE)



##########################
###                    ###
###   Visualisations   ###
###                    ###
##########################


options(scipen = 999)
options(digits = 15)


data$country <- as.factor(data$country)
data$region <- as.factor(data$region)
data$year <- as.numeric(data$year)
data$net_migration <- as.numeric(data$net_migration)

data$gdp_capita <- data$gdp / data$population



numeric_cols <- c("access_electricity", "agricultural_land",
                  "agricultural_forestry_fishing", "births", "co2", "deaths", "forest",
                  "gdp", "life_expectancy",
                  "net_migration", "population",
                  "rural_population", "urban_population", "year",
                  "gdp_capita"
)

# Calculate the correlation matrix, handling missing data
corr_matrix <- cor(data[, numeric_cols], use = "pairwise.complete.obs")
#Display the correlation plot
ggcorrplot(corr_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
           title = "Correlation Matrix")

#Subsetting variables 
vars <- c("access_electricity", "agricultural_land",
          "agricultural_forestry_fishing", "births", "co2", "deaths", "forest",
          "gdp", "life_expectancy",
          "net_migration", "population",
          "rural_population", "urban_population", "year", "gdp_capita")

plots <- lapply(vars, function(v) {
  ggplot(data, aes_string(x = v)) +
    geom_histogram(fill = "skyblue", color = "black", bins = 30) +
    ggtitle(v) +
    theme_minimal() +
    theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, hjust=1))
})

# Arrange all plots in a grid
grid.arrange(grobs=plots, ncol=5)

#Subsetting most recent year and earliest year
year2001 <- subset(data, data$year == 2001)
year2021 <- subset(data, data$year == 2021)



### GDP per Capita
##################

summary(data$gdp_capita)
plot(data$gdp_capita)

#ordering top 10 gdp per capita for year 2001
top10_2001 <- year2001 %>%
  top_n(10, gdp_capita) %>%
  arrange(desc(gdp_capita)) %>%
  mutate(Year = "2001")

# getting top 10 countries by GDP per capita for 2021
top10_2021 <- year2021 %>%
  top_n(10, gdp_capita) %>%
  arrange(desc(gdp_capita)) %>%
  mutate(Year = "2021")

# Combine datasets to show top 10
top10_combined <- bind_rows(top10_2001, top10_2021)
ggplot(top10_combined, aes(x = reorder(country, gdp_capita), y = gdp_capita, fill = Year)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Top 10 Countries by GDP per Capita",
       x = "Country", y = "GDP per Capita",
       fill = "Year") +
  theme_minimal()




### GDP
#######

summary(data$gdp)
hist(data$gdp, breaks = 50, main = "Histogram of GDP (US$)", xlab = "GDP (US$)", ylab = "Count")
rug(data$gdp)
boxplot(data$gdp)

#Ordering countries by GDP in 2021 
topgdp21 <- data %>%
  filter(year == 2021) %>%
  arrange(desc(gdp))
topgdp21 <- head(topgdp21, 20)

#Plotting the top 20 countries by GDP in 2021
ggplot(topgdp21, aes(x = reorder(country, gdp), y = gdp / 1e9, fill = region)) +
  geom_bar(stat = "identity", color = 'black') +
  coord_flip() +
  labs(title = "Top 20 Countries by GDP in 2021",
       x = "Country", y = "GDP (Billions)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


### Country
###########
years <- sort(unique(data$year))
start_year <- years[1]
end_year <- years[length(years)]

gdp_change <- data %>%
  filter(year %in% c(start_year, end_year)) %>%
  group_by(country, year) %>%
  summarize(gdp = first(gdp)) %>%
  pivot_wider(names_from = year, values_from = gdp) %>%
  ungroup()

gdp_change <- gdp_change %>%
  mutate(change = `end_year` - `start_year`)

#Setting the min and max year
start_year_value <- min(data$year)
end_year_value <- max(data$year)

# Calculating the change in GDP over the timeframe
gdp_change <- data %>%
  filter(year %in% c(start_year_value, end_year_value)) %>%
  group_by(country, year) %>%
  summarize(gdp = first(gdp)) %>%
  pivot_wider(names_from = year, values_from = gdp) %>%
  mutate(change = !!sym(paste0(end_year_value)) - !!sym(paste0(start_year_value))) %>%
  ungroup()

# Showing top increases
top_increase <- gdp_change %>%
  arrange(desc(change)) %>%
  dplyr::slice(1:10)

# Showing top declines
top_decline <- gdp_change %>%
  arrange(change) %>%
  dplyr::slice(1:10)

#Plotting the top increases
ggplot(top_increase, aes(x = reorder(country, change), y = change)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Countries with Greatest GDP Increase (2001 - 2021)",
       x = "Country", y = "GDP Increase") +
  theme_minimal()

#Plotting the top decreases
ggplot(top_decline, aes(x = reorder(country, change), y = change)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Top 10 Countries with Lowest GDP Increase (2001 - 2021)",
       x = "Country", y = "GDP Decline") +
  theme_minimal()

# Combining top countries for increase and decline
top_countries <- c(top_increase$country, top_decline$country)

#subsetting countries with top change
trend_data <- data %>%
  filter(country %in% top_countries)

#Organising the top changing countries
top_countries <- gdp_change %>%
  arrange(desc(change)) %>% 
  dplyr::slice(1:10) %>%
  pull(country)

# Selecting top countries
filtered_trend_data <- trend_data %>%
  filter(country %in% top_countries)

#Setting end points
end_points <- filtered_trend_data %>%
  group_by(country) %>%
  filter(year == max(year))

# Plotting the change over time as a line graph
ggplot(filtered_trend_data, aes(x = year, y = gdp / 1e9, color = country)) +
  geom_line() +
  geom_text_repel(
    data = end_points,
    aes(label = country),
    nudge_x = 0,
    nudge_y = 0,
    force = 1,
    direction = "both",
    segment.color = NA,
    max.overlaps = 10,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.curvature = 0.2
  ) +
  labs(
    title = "Top 10 Countries for GDP Growth",
    subtitle = "From 2001 - 2021",
    x = "Year", y = "GDP (Billions)"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12)
  )



### Year
########

table(data$year)

# Calculating the average GDP per year
average_gdp_per_year <- data %>%
  group_by(year) %>%
  summarize(average_gdp = mean(gdp, na.rm = TRUE))

# Plotting the average GDP per year
ggplot(data = average_gdp_per_year, aes(average_gdp_per_year$year, average_gdp_per_year$average_gdp  / 1e9)) + 
  geom_line(linewidth = 1.5) +
  labs(title = "Global Average GDP per year", x = "Year", y = "GDP (US$) (Billions)")+
  geom_point(aes(x = 2008.5, y = 335), shape = 1, size = 20, colour = 'red', stroke = 2) +
  annotate("text", x = 2005, y = 385, label = "2008 Financial Crisis", colour = 'red', size = 4) +
  geom_point(aes(x = 2014.5, y = 400), shape = 1, size = 30, colour = 'red', stroke = 2) +
  annotate("text", x = 2015, y = 450, label = "Global decline", colour = 'red', size = 4) +
  geom_point(aes(x = 2019.5, y = 465), shape = 1, size = 20, colour = 'red', stroke = 2) +
  annotate("text", x = 2015, y = 510, label = "COVID-19 Pandemic", colour = 'red', size = 4)


### Region
##########

summary(data$region)

#Calculating the average GDP for each region
avg_gdp_region <- data %>%
  group_by(region) %>%
  summarize(avg_gdp = mean(gdp, na.rm = TRUE))

#Plotting the average GDP for each region
ggplot(avg_gdp_region, aes(x = region, y = avg_gdp  / 1e9, fill = region)) +
  geom_col(colour = 'black') +
  coord_flip() +
  labs(title = "Average GDP by Region",
       x = "Region",
       y = "Average GDP (Billions)") +
  theme_minimal() +
  theme(legend.position = "none")


## Access to Electricity
#########################

plot(data$access_electricity, data$gdp)
hist(data$access_electricity, breaks = 50)

#Plotting electricity access in 2021
ggplot(year2021, aes(x = access_electricity)) +
  geom_histogram(
    bins = 50,
    fill = "steelblue",
    color = "black"
  ) +
  labs(title = "Distribution of Access to Electricity", subtitle = "In the year 2021",
       x = "Access to Electricity (%)",
       y = "Count") +
  theme_minimal()



### Agricultural Land
#####################

summary(data$agricultural_land)
describe(data$agricultural_land)

#Histogram for Agricultural land in 2021
plot_2021 <- ggplot(year2021, aes(x = agricultural_land)) +
  geom_histogram(
    bins = 50,
    fill = "seagreen",
    color = "black"
  ) +
  labs(title = "Distribution of Agricultural Land", subtitle = "In the year 2021",
       x = "Agricultural Land (%)",
       y = "Count") +
  theme_minimal() +
  xlim(0, 100)

year2001 <- subset(data, data$year==2001)

# Create the 2001 histogram plot
plot_2001 <- ggplot(year2001, aes(x = agricultural_land)) +
  geom_histogram(
    bins = 50,
    fill = "seagreen",
    color = "black"
  ) +
  labs(title = "Distribution of Agricultural Land", subtitle = "In the year 2001",
       x = "Agricultural Land (%)",
       y = "Count") +
  theme_minimal() +
  xlim(0, 100)

#Plotting the two graphs in one plot
grid.arrange(plot_2001, plot_2021, nrow = 2)



### Agricultural, Forestry, Fishing
###################################

summary(data$agricultural_forestry_fishing)

#Creating histogram for AFF for each region in 2001
aff2001bar <- ggplot(year2001, aes(x = agricultural_forestry_fishing, fill = region)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "Dark2", name = "Region") +
  labs(title = "Agricultural, Forestry and Fishing Contribution by Region",
       x = "",
       y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  ) +
  xlim(0, 100)

#Creating histogram for AFF for each region in 2021
aff2021bar <- ggplot(year2021, aes(x = agricultural_forestry_fishing, fill = region)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "Dark2", name = "Region") +
  labs(
    x = "Agricultural, Forestry and Fishing Contribution (% of GDP)",
    y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  ) +
  xlim(0, 100)

#Plotting both histograms on one plot
grid.arrange(aff2001bar, aff2021bar, nrow = 2)
par(mfrow = c(1,1))


### Births
##########

summary(data$births)
hist(data$births)

births2001 <- ggplot(year2001, aes(x = births, fill = region)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "Dark2", name = "Region") +
  labs(title = "Distribution of Births by Region",
       x = "",
       y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  ) +
  xlim(0, 60)

births2021 <- ggplot(year2021, aes(x = births, fill = region)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "Dark2", name = "Region") +
  labs(#title = "Distribution of Births by Region",
    x = "Number of Births per 1,000",
    y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  ) +
  xlim(0, 60)

grid.arrange(births2001, births2021, nrow = 2)

par(mfrow = c(1,1))

### Scatter Plots for Births in 2001 and 2021

births_scatter_2001 <- ggplot(year2001, aes(x = births, y = gdp / 1e9)) +
  geom_point(shape=21,
             color="black", 
             fill="deepskyblue", 
             size=3, 
             alpha=0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "deeppink3", size = 1) +
  labs(
    title = "Relationship Between Births and GDP in 2001",
    subtitle = "Scatter plot with linear trend line",
    x = "Number of Births (per 1,000)",
    y = "GDP (Billions)",
    caption = "Source: World Bank Data"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size=14),
    axis.text = element_text(size=12)
  ) +
  xlim(0, 60)

births_scatter_2021 <- ggplot(year2021, aes(x = births, y = gdp / 1e9)) +
  geom_point(shape=21,
             color="black", 
             fill="deepskyblue", 
             size=3, 
             alpha=0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "deeppink3", size = 1) +
  labs(
    title = "Relationship Between Births and GDP in 2021",
    subtitle = "Scatter plot with linear trend line",
    x = "Number of Births (per 1,000)",
    y = "GDP (Billions)",
    caption = "Source: World Bank Data"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size=14),
    axis.text = element_text(size=12)
  )+
  xlim(0, 60)

grid.arrange(births_scatter_2001, births_scatter_2021, nrow = 2)
par(mfrow = c(1,1))


### CO2
#######

summary(data$co2)
hist(data$co2)

# Aggregate total CO2 per year
co2_region_year <- data %>%
  group_by(year, region) %>%
  summarize(total_co2 = sum(co2, na.rm = TRUE))

#Plotting the CO2 by year and region
ggplot(co2_region_year, aes(x = factor(year), y = total_co2, fill = region)) +
  geom_bar(stat = "identity", alpha = 0.9, color = "black") +
  scale_fill_brewer(palette = "Set3") + 
  labs(
    title = "CO2 Emissions by Year and Region",
    subtitle = "Aggregated data segmented by region",
    x = "Year",
    y = "Total CO2 Emissions",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size=12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12)
  )


#Creating Scatter plots for 2001 and 2021
co2_2001 <- ggplot(year2001, aes(x = co2, y = gdp / 1e9)) +
  geom_point(shape=21,
             color="black", 
             fill="deepskyblue", 
             size=3, 
             alpha=0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "deeppink3", size = 1) +
  labs(
    title = "Relationship Between CO2 and GDP in 2001",
    subtitle = "Scatter plot with linear trend line",
    x = "CO2 (Mt CO2e)",
    y = "GDP (Billions)",
    caption = "Source: World Bank Data"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size=14),
    axis.text = element_text(size=12)
  )

co2_2021 <- ggplot(year2021, aes(x = co2, y = gdp / 1e9)) +
  geom_point(shape=21,
             color="black", 
             fill="deepskyblue", 
             size=3, 
             alpha=0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "deeppink3", size = 1) +
  labs(
    title = "Relationship Between CO2 and GDP in 2021",
    subtitle = "Scatter plot with linear trend line",
    x = "CO2 (Mt CO2e)",
    y = "GDP (Billions)",
    caption = "Source: World Bank Data"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size=14),
    axis.text = element_text(size=12)
  )

grid.arrange(co2_2001, co2_2021, ncol = 2)

par(mfrow = c(1,1))


### Deaths
##########

summary(data$deaths)
hist(data$deaths)

#Creating histogram for deaths in 2001
deaths2001 <- ggplot(year2001, aes(x = deaths, fill = region)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "Dark2", name = "Region") +
  labs(title = "Distribution of Deaths by Region",
       subtitle = "Deaths by 1,000 people in 2001:",
       x = "",
       y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle =  element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12)
  ) +
  xlim(0, 30)

#Creating histogram for deaths in 2021
deaths2021 <- ggplot(year2021, aes(x = deaths, fill = region)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "Dark2", name = "Region") +
  labs(
    subtitle = "Deaths by 1,000 people in 2021:",
    x = "Number of Deaths per 1,000",
    y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12)
  ) +
  xlim(0, 30)

#Plotting both charts on one plot
grid.arrange(deaths2001, deaths2021, nrow = 2)
par(mfrow = c(1,1))


### Forest
##########

ggplot(data, aes(x = forest, y = gdp / 1e9)) +
  geom_point(color = "black", fill = "dodgerblue", size = 3, alpha = 0.7, shape = 21) +
  labs(
    title = "Relationship Between Forest Area and GDP",
    x = "Forest Area",
    y = "GDP (Billions)"
  ) +
  geom_smooth(method = 'lm', se = T, color = 'red') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


### Life Expectancy
###################

summary(data$life_expectancy)
#scatter plot for life expectancy in 2001
lifeex2001 <- ggplot(year2001, aes(x = life_expectancy, y = gdp / 1e9, color = region)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "GDP vs Life Expectancy by Region",
    subtitle = "In the year 2001",
    x = "Life Expectancy",
    y = "GDP (Billions)"
  ) +
  geom_smooth(method = 'lm', se = T, color = 'black') +
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position="bottom"
  ) +
  ylim(0, 25000)+
  xlim(40,85)

#scatter plot for life expectancy in 2021
lifeex2021 <- ggplot(year2021, aes(x = life_expectancy, y = gdp / 1e9, color = region)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "GDP vs Life Expectancy by Region",
    subtitle = "In the year 2021",
    x = "Life Expectancy",
    y = "GDP (Billions)"
  ) +
  geom_smooth(method = 'lm', se = T, color = 'black') +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = 'bottom'
  )+
  ylim(0, 25000)+
  xlim(40,85)
#Plotting both on one plot
grid.arrange(lifeex2001, lifeex2021, ncol = 2)
par(mfrow = c(1,1))




### Net Migration
#################

summary(data$net_migration)
hist(data$net_migration)

#Sorting migration in 2001
migration_2001 <- year2001 %>%
  group_by(region) %>%
  summarize(net_migration = sum(net_migration, na.rm=TRUE))
migration_2001 <- migration_2001 %>%
  mutate(sign = ifelse(net_migration >= 0, "positive", "negative"))

#Plotting net-migration by region in 2001
ggplot(migration_2001, aes(x = region, y = net_migration, fill = sign)) +
  geom_bar(stat = "identity", position = "identity", color = "black") +
  scale_fill_manual(values = c("positive" = "steelblue", "negative" = "tomato")) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Net Migration by Region",
       subtitle = "In the year 2001",
       y = "Net Migration",
       x = "Region") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Sorting migration in 2021
migration_2021 <- year2021 %>%
  group_by(region) %>%
  summarize(net_migration = sum(net_migration, na.rm=TRUE))
migration_2021 <- migration_2021 %>%
  mutate(sign = ifelse(net_migration >= 0, "positive", "negative"))

#Plotting net-migration by region in 2021
ggplot(migration_2021, aes(x = region, y = net_migration, fill = sign)) +
  geom_bar(stat = "identity", position = "identity", color = "black") +
  scale_fill_manual(values = c("positive" = "steelblue", "negative" = "tomato")) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Net Migration by Region",
       subtitle = "In the year 2021",
       y = "Net Migration",
       x = "Region") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



### Population
##############

summary(data$population)
#Plotting Population Scatter Plot by GDP
ggplot(year2021, aes(x=gdp/1e9, y=population))+
  geom_point(shape = 21, fill = 'dodgerblue', color='black')+
  geom_smooth(method = 'lm', se = T, color='red') +
  labs(title = "GDP and Population",
       subtitle = "In the year 2021",
       x = "GDP (Billions)",
       y = "Population") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )



### Rural vs. Urban
###################

#Subsetting US urban vs rural population over the years
states <- subset(data, data$country=="United States")
states_long <- states %>%
  select(year, rural_population, urban_population) %>%
  pivot_longer(
    cols = c(rural_population, urban_population),
    names_to = "Population_Type",
    values_to = "Count"
  )
#Plotting this stacked bar chart
ggplot(states_long, aes(x = factor(year), y = Count, fill = Population_Type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Rural vs Urban Population by Year",
    x = "Year",
    y = "Population",
    fill = "Population Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(hjust=0.5, size=16, face="bold")
  )

#Subsetting China urban vs rural population over the years
china <- subset(data, data$country=="China")
china_long <- china %>%
  select(year, rural_population, urban_population) %>%
  pivot_longer(
    cols = c(rural_population, urban_population),
    names_to = "Population_Type",
    values_to = "Count"
  )
#Plotting this stacked bar chart
ggplot(china_long, aes(x = factor(year), y = Count, fill = Population_Type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Rural vs Urban Population by Year",
    subtitle = "In China",
    x = "Year",
    y = "Population",
    fill = "Population Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(hjust=0.5, size=16, face="bold")
  )



#############################
###                       ###
###   Summary Statistics  ###
###                       ###
#############################


desc <- describe(data)
doc <- read_docx()
desc_table <- kable(desc, format = "pandoc")
ft <- flextable(desc)
doc <- body_add_flextable(doc, ft)
print(doc, target = "descriptive_stats.docx")

summary_stats <- data %>%
  summarise(across(where(is.numeric), list(
    mean = ~round(mean(., na.rm=TRUE), 2),
    sd = ~round(sd(., na.rm=TRUE), 2),
    min = ~round(min(., na.rm=TRUE), 2),
    max = ~round(max(., na.rm=TRUE), 2),
    median = ~round(median(., na.rm=TRUE), 2),
    skew = ~round(skewness(., na.rm=TRUE), 2),
    kurtosis = ~round(kurtosis(., na.rm=TRUE), 2)
  )))

# Reshape so each variable is a row and measures are columns
summary_long <- summary_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "measure"),
    names_pattern = "(.+)_([a-z]+)"
  ) %>%
  pivot_wider(names_from = measure, values_from = value)

write_xlsx(summary_long, "summary_table.xlsx")

