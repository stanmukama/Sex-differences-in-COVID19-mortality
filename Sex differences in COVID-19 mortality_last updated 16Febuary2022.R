#This code reads and prepares data for sex-differences in mortality by sex and
#age-adjusted all-cause mortality
#last edited by Trasias on 11 November 2020



#install.packages("osfr")
#install.packages("xlsx")
library(osfr)
library(tidyverse)
library(readr)
library(lubridate)
library(readxl)
library(MortalityLaws)
library(dsr)
library(ggpubr)
library(ggrepel)
library(xlsx)
library(dplyr)

#set working directory
setwd("C:/Users/stanm/OneDrive/Documents/Research/HIGH/Sex and Covid-19")

#read data from the OSF - using data as of 09 February 2020

#osf_retrieve_file("43ucn") %>%
   #osf_download(conflicts = "overwrite", path = "./Data/raw data/")
# This reads it in
Output_10 <- read_csv("./Data/raw data/Output_10.zip",
                      skip = 3,
                      col_types = "ccccciiddd")

#renaming vars
osf_10yr <- Output_10 %>% rename(country=Country, age = Age, region = Region,
                                 code = Code, ageint = AgeInt, sex = Sex,
                                 date=Date, deaths = Deaths, cases=Cases, tests = Tests)
#drop unneeded vars
osf_10yr <- filter(osf_10yr, region == "All")

##' remove redundancies for Italy and France
##' for Italy, ITbol is preferred over ITinfo because it has sex disaggregated data
osf_10yr <- filter(osf_10yr, substr(osf_10yr$code, 1, 5) != "FRheb")
osf_10yr <- filter(osf_10yr, substr(osf_10yr$code, 1, 6) != "ITinfo")

osf_10yr <- osf_10yr %>% select(-c(region, code, ageint, tests, cases))

#remove missing data
osf_10yr <- na.omit(osf_10yr)

##' identify data for the latest data
osf_10yr$today <- as.Date(today())
osf_10yr$days <- as.Date(as.character(osf_10yr$date), format = "%d.%m.%Y") - as.Date(as.character(osf_10yr$today), format ="%Y-%m-%d")
##' keep latest data by country 
osf_10yr <-  osf_10yr %>% group_by(country, age, sex) %>% slice(which.max(days)) %>% ungroup()

##' remove the unnecessary vars
osf_10yr <- select(osf_10yr, -days, -today)

##' keep only sex disaggregated data for each country
##' delete if sex is equal to both
osf_10yr <- filter(osf_10yr, osf_10yr$sex != "b")

##' merge the 90 and older groups
#osf_10yr$age <- if_else(osf_10yr$age == "100", "90", osf_10yr$age)

##' count number of deaths by country
#osf_10yr$cases <- as.numeric(osf_10yr$cases)
osf_10yr$deaths <- as.numeric(osf_10yr$deaths)
osf_10yr_sum <- osf_10yr %>% group_by(country) %>% summarise(deaths = sum(deaths))

osf_10yr_sum <- ungroup(osf_10yr_sum)
osf_10yr_sum <- filter(osf_10yr_sum, deaths >= 50)

osf_countries <- osf_10yr_sum$country
#keep countries with at least 50 deaths in total
osf_10yr <- filter(osf_10yr, country %in% osf_countries)
osf_10yr$sex <- if_else(osf_10yr$sex == "f", "F","M")
osf_10yr <- osf_10yr %>% group_by(country, sex, age,date) %>% summarise_all(sum) %>% ungroup()

#osf_10yr <- osf_10yr %>% mutate(age = case_when(age>=0 & age<= 39~0,age>=40 & age<=49~40,age>=50 & age<=59~50,
#age>=60 &age<=69~60,age>=70 & age<=79~70,age>=80~80))

osf_10yr <- osf_10yr %>% mutate(age = case_when(age==100~90,TRUE~as.numeric(age)))
osf_10yr <- osf_10yr %>% group_by(country,sex,age,date) %>% summarise_all(sum) %>% ungroup()

covid_countries <- osf_10yr %>% select(country) %>% group_by(country) %>% filter(row_number()==1) %>% ungroup()
covid_countries <- covid_countries$country

#save data for table 1;
#table1_covid <- osf_10yr %>% group_by(country,sex,date) %>% summarise(deaths = sum(deaths)) %>% ungroup()
#table1_covid <- table1_covid %>% pivot_wider(names_from = sex, values_from = c(deaths))

osf <- osf_10yr %>% rename(cvd_deaths = deaths) %>% pivot_wider(names_from = sex, values_from = c(cvd_deaths))
osf <- osf %>% rename(cvd_deaths_M = M, cvd_deaths_F = F)

#downloading all-cause data from HMD and WPP

#get pop data from HMD and WPP
# downloading HMD data
username <- "stanmukama@gmail.com"
password <- "01071989"

#download data for all HMD countries ---- 

#get deaths data 
deaths_hmd1 <- ReadHMD("Dx", countries = NULL, interval = "1x1", username, password,
                      save = FALSE, show = TRUE)        
deaths_hmd <- deaths_hmd1$data  

#remove years with pop but not death data
deaths_hmd <- na.omit(deaths_hmd)

#name vars appropriately for merging with death data                    
deaths_hmd <- deaths_hmd %>% rename(year = Year,age = Age,deaths_M = Male,deaths_F = Female,deaths_t = Total)

#replace alpha-3 codes with country names
deaths_hmd <- deaths_hmd %>% mutate(country = case_when(country=="AUS"~"Australia",country=="AUT"~"Austria",country =="BEL"~"Belgium",country=="BGR"~"Bulgaria",
                                                        country=="CAN"~"Canada",country=="CHE"~"Switzerland",country=="CHL"~"Chile",country=="CZE"~"Czechia",
                                                        country=="DEUTNP"~"Germany",country=="DNK"~"Denmark",country=="ESP"~"Spain",country=="EST"~"Estonia",
                                                        country=="FIN"~"Finland",country=="FRATNP"~"France",country=="GBR_NP"~"United Kingdom",country=="GRC"~"Greece",
                                                        country=="HRV"~"Croatia",country=="HUN"~"Hungary",country=="IRL"~"Ireland",country=="ISL"~"Iceland",
                                                        country=="ISR"~"Israel",country=="ITA"~"Italy",country=="JPN"~"Japan",country=="KOR"~"South Korea",
                                                        country=="LTU"~"Lithuania",country=="LUX"~"Luxembourg",country=="LVA"~"Latvia",country=="NLD"~"Netherlands",
                                                        country=="NOR"~"Norway",country=="NZL_NP"~"New Zealand",country=="POL"~"Poland",country=="PRT"~"Portugal",
                                                        country=="RUS"~"Russia",country=="SVK"~"Slovakia",country=="SVN"~"Slovenia",country=="SWE"~"Sweden",
                                                        country=="TWN"~"Taiwan",country=="UKR"~"Ukraine",country=="USA"~"USA",country=="BLR"~"Belarus"))
#keep countries with covid-data
deaths_hmd <- filter(deaths_hmd, country %in% covid_countries)

deaths_hmd$deaths_F <- as.numeric(deaths_hmd$deaths_F)
deaths_hmd$deaths_M <- as.numeric(deaths_hmd$deaths_M)
deaths_hmd$deaths_t <- as.numeric(deaths_hmd$deaths_t)

#remove years with pop but not death data
deaths_hmd <- na.omit(deaths_hmd)

#create 10-year age groups
deaths_hmd$age <- if_else(deaths_hmd$age >= 0 & deaths_hmd$age <10, 0,
                          if_else(deaths_hmd$age >=10 & deaths_hmd$age <20, 10,
                                  if_else(deaths_hmd$age >=20 & deaths_hmd$age <30, 20,
                                          if_else(deaths_hmd$age >= 30 & deaths_hmd$age <40, 30,
                                                  if_else(deaths_hmd$age >= 40 & deaths_hmd$age <50, 40,
                                                          if_else(deaths_hmd$age >=50 & deaths_hmd$age <60, 50,
                                                                  if_else(deaths_hmd$age >=60 & deaths_hmd$age <70, 60,
                                                                          if_else(deaths_hmd$age >= 70 & deaths_hmd$age <80, 70,
                                                                                  if_else(deaths_hmd$age >= 80 & deaths_hmd$age <90, 80, 90)))))))))
#add population and deaths into 10-yr age groups
deaths_hmd <- deaths_hmd %>% group_by(country,year, age) %>% summarise_all(sum) %>% ungroup()



#keep latest data for each country 
deaths_hmd <-  deaths_hmd %>% group_by(country, age) %>% slice(which.max(year)) %>% ungroup()

#hmd_deaths <- deaths_hmd %>%
 # select(-c(deaths_t)) %>% 
  #Reshape
 # pivot_longer(-c(country,year,age),names_to = "reshape") %>% 
 # separate(reshape, into = c("variable","sex"), sep = "_") %>% 
 # pivot_wider(names_from = "variable", values_from = "value") 


#get population data
pop_hmd1 <- ReadHMD("population", countries = NULL, interval = "1x1", username, password,
                   save = FALSE, show = TRUE)        
pop_hmd <- pop_hmd1$data        
#name vars appropriately for merging with death data                    
pop_hmd <- pop_hmd %>% rename(year = Year,age = Age,pop_M = Male,pop_F = Female,pop_t = Total)

#replace alpha-3 codes with country names
pop_hmd <- pop_hmd %>% mutate(country = case_when(country=="AUS"~"Australia",country=="AUT"~"Austria",country =="BEL"~"Belgium",country=="BGR"~"Bulgaria",
                                                  country=="CAN"~"Canada",country=="CHE"~"Switzerland",country=="CHL"~"Chile",country=="CZE"~"Czechia",
                                                  country=="DEUTNP"~"Germany",country=="DNK"~"Denmark",country=="ESP"~"Spain",country=="EST"~"Estonia",
                                                  country=="FIN"~"Finland",country=="FRATNP"~"France",country=="GBR_NP"~"United Kingdom",country=="GRC"~"Greece",
                                                  country=="HRV"~"Croatia",country=="HUN"~"Hungary",country=="IRL"~"Ireland",country=="ISL"~"Iceland",
                                                  country=="ISR"~"Israel",country=="ITA"~"Italy",country=="JPN"~"Japan",country=="KOR"~"South Korea",
                                                  country=="LTU"~"Lithuania",country=="LUX"~"Luxembourg",country=="LVA"~"Latvia",country=="NLD"~"Netherlands",
                                                  country=="NOR"~"Norway",country=="NZL_NP"~"New Zealand",country=="POL"~"Poland",country=="PRT"~"Portugal",
                                                  country=="RUS"~"Russia",country=="SVK"~"Slovakia",country=="SVN"~"Slovenia",country=="SWE"~"Sweden",
                                                  country=="TWN"~"Taiwan",country=="UKR"~"Ukraine",country=="USA"~"USA",country=="BLR"~"Belarus"))
#keep countries with covid-data
pop_hmd <- filter(pop_hmd, country %in% covid_countries)

#create 10-year age groups
pop_hmd$age <- if_else(pop_hmd$age >= 0 & pop_hmd$age <10, 0,
                       if_else(pop_hmd$age >=10 & pop_hmd$age <20, 10,
                               if_else(pop_hmd$age >=20 & pop_hmd$age <30, 20,
                                       if_else(pop_hmd$age >= 30 & pop_hmd$age <40, 30,
                                               if_else(pop_hmd$age >= 40 & pop_hmd$age <50, 40,
                                                       if_else(pop_hmd$age >=50 & pop_hmd$age <60, 50,
                                                               if_else(pop_hmd$age >=60 & pop_hmd$age <70, 60,
                                                                       if_else(pop_hmd$age >= 70 & pop_hmd$age <80, 70,
                                                                               if_else(pop_hmd$age >= 80 & pop_hmd$age <90, 80, 90)))))))))
#add population and deaths into 10-yr age groups
pop_hmd <- pop_hmd %>% group_by(country,year, age) %>% summarise_all(sum) %>% ungroup()

#remove years with pop but not death data
pop_hmd <- na.omit(pop_hmd)

#keep latest data for each country 
#pop_hmd <-  pop_hmd %>% group_by(country, age) %>% slice(which.max(year)) %>% ungroup()

hmd_countries <- pop_hmd %>% select(country) %>% group_by(country) %>% filter(row_number()==1) %>% ungroup()
hmd_countries <- hmd_countries$country

#hmd_pop <- pop_hmd %>%
  #select(-c(pop_t)) %>% 
  #Reshape
  #pivot_longer(-c(country,year,age),names_to = "reshape") %>% 
  #separate(reshape, into = c("variable","sex"), sep = "_") %>% 
  #pivot_wider(names_from = "variable", values_from = "value") %>% 
  #Rename pop to not confuse the standardization function
  #rename(pop_counts = "pop")

hmd <- merge(deaths_hmd, pop_hmd, by = c("country","year","age"), all.x = TRUE)
#hmd <- select(hmd, -year)

hmd_countries <- pop_hmd %>% select(country) %>% group_by(country) %>% filter(row_number()==1) %>% ungroup()
hmd_countries <- hmd_countries$country

#get population data from WPP for countries on HMD but with COVID data
wpp_countries <- setdiff(covid_countries,hmd_countries)

## get population data
wpp_pop_f <- read_excel("./Data/raw data/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx", col_names = T, sheet = 1)
wpp_pop_f$sex <- "F"
wpp_pop_m <- read_excel("./Data/raw data/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx", col_names = T, sheet = 1)
wpp_pop_m$sex <- "M"

wpp_pop <- rbind(wpp_pop_f, wpp_pop_m)
## keep needed variables
wpp_pop <- select(wpp_pop, -c(index, variant, code, notes, type, code2))
wpp_pop <- filter(wpp_pop, period == "2020")
wpp_pop$year <- 2020


## keep data for with covid-pop more than 50
wpp_pop <- filter(wpp_pop, country %in% wpp_countries)

wpp_pop <- gather(wpp_pop, age, pop, age0_4:age100, factor_key=TRUE)

## categorize into 10-year age groups
wpp_pop$age <- if_else(wpp_pop$age %in% c("age0_4", "age5_9"), 0,
                       if_else(wpp_pop$age %in% c("age10_14","age15_19"),10,
                               if_else(wpp_pop$age %in% c("age20_24","age25_29"),20,
                                       if_else(wpp_pop$age %in% c("age30_34", "age35_39"),30,
                                               if_else(wpp_pop$age %in% c("age40_44","age45_49"),40,
                                                       if_else(wpp_pop$age %in% c("age50_54","age55_59"),50,
                                                               if_else(wpp_pop$age %in% c("age60_64","age65_69"),60,
                                                                       if_else(wpp_pop$age %in% c("age70_74","age75_79"),70,
                                                                               if_else(wpp_pop$age %in% c("age80_84","age85_89"), 80, 90)))))))))
## sum pop by age-group
wpp_pop <- select(wpp_pop, -period)
wpp_pop$pop <- as.numeric(wpp_pop$pop)*1000

wpp_pop <- wpp_pop %>% group_by(country, sex, age,year) %>% summarise(pop_counts = sum(pop)) %>% ungroup()
pop_wpp <- wpp_pop %>% select(-year) %>% pivot_wider(names_from = sex, values_from = pop_counts) %>% rename(pop_F = F, pop_M = M) %>%
  mutate(pop_t = pop_F + pop_M)


#death data
wpp_deaths_f <- read_excel("./Data/raw data/WPP2019_MORT_F04_3_DEATHS_BY_AGE_FEMALE.xlsx", col_names = T, sheet = 1)
wpp_deaths_f$sex <- "F"
wpp_deaths_m <- read_excel("./Data/raw data/WPP2019_MORT_F04_2_DEATHS_BY_AGE_MALE.xlsx", col_names = T, sheet = 1)
wpp_deaths_m$sex <- "M"

wpp_deaths <- rbind(wpp_deaths_f, wpp_deaths_m)
## keep needed variables
wpp_deaths <- select(wpp_deaths, -c(index, variant, code, notes, type, code2))
wpp_deaths <- filter(wpp_deaths, period == "2015-2020")

## keep data for with covid-deaths 
wpp_deaths <- filter(wpp_deaths, country %in% wpp_countries)

wpp_deaths <- gather(wpp_deaths, age, deaths, age0_4:age95, factor_key=TRUE)

## categorise into 10-year age groups
wpp_deaths$age <- if_else(wpp_deaths$age %in% c("age0_4", "age5_9"), 0,
                          if_else(wpp_deaths$age %in% c("age10_14","age15_19"),10,
                                  if_else(wpp_deaths$age %in% c("age20_24","age25_29"),20,
                                          if_else(wpp_deaths$age %in% c("age30_34", "age35_39"),30,
                                                  if_else(wpp_deaths$age %in% c("age40_44","age45_49"),40,
                                                          if_else(wpp_deaths$age %in% c("age50_54","age55_59"),50,
                                                                  if_else(wpp_deaths$age %in% c("age60_64","age65_69"),60,
                                                                          if_else(wpp_deaths$age %in% c("age70_74","age75_79"),70,
                                                                                  if_else(wpp_deaths$age %in% c("age80_84","age85_89"), 80, 90)))))))))
## sum deaths by age-group
wpp_deaths <- select(wpp_deaths, -period)
wpp_deaths$deaths <- as.numeric(wpp_deaths$deaths)*1000

wpp_deaths <- wpp_deaths %>% group_by(country, sex, age) %>% summarise(deaths = sum(deaths)) %>% ungroup()
wpp_deaths <- wpp_deaths %>% pivot_wider(names_from = sex, names_prefix = "deaths_", values_from = deaths) %>%
  mutate(deaths_t = deaths_F + deaths_M, year = 2020)

wpp <- merge(pop_wpp, wpp_deaths, by = c("country", "age"), all.x = TRUE)

#merge data for all-cause deaths and population by sex
all_cause <- rbind(hmd,wpp)

rr_allcause <- merge(all_cause,osf, by = c("country","age"), all.x = TRUE)

#table 1 data
#save data for table 1;
table1_allcause <- rr_allcause %>% select(-age) %>% group_by(country,year,date) %>% summarise_all(sum) %>% ungroup()

#calculate weights
 rr_allcause <- rr_allcause %>% group_by(country) %>% mutate(weights = pop_t/sum(pop_t))
 
 rr_allcause <- rr_allcause %>%
   #Rates
   mutate(mx_m = deaths_M/pop_M, mx_f = deaths_F/pop_F,
          cfr_m = cvd_deaths_M/pop_M, cfr_f = cvd_deaths_F/pop_F) %>%
   #Age-specific ratio
   mutate(ratio = mx_m/mx_f, cfr_ratio = cfr_m/cfr_f) %>%
   #Age-specific diff
   mutate(rdiff = mx_m - mx_f, cfr_diff = cfr_m - cfr_f)

#Estimate age-standardised rates
 age_std_allcause <- rr_allcause %>%
   #multiply age-specific rate by weight
   mutate(component_m = mx_m*weights,
          component_f = mx_f*weights,
          component_cfr_m = cfr_m*weights,
          component_cfr_f = cfr_f*weights) %>% 
   
   #Sum over age
   summarize(allcause_m = sum(component_m),
             allcause_f = sum(component_f),
             cvd_m = sum(component_cfr_m),
             cvd_f = sum(component_cfr_f)) %>% 
   #Take dsrr
   mutate(rr_allcause = allcause_m/allcause_f, rr_covid = cvd_m/cvd_f) %>%
    mutate(rd_allcause = allcause_m - allcause_f, rd_covid = cvd_m - cvd_f) %>%
   #Excess ratio
   mutate(em = rr_covid / rr_allcause)
 
 #calculate the 95% CI using the Delta Method
 #Estimate CI
 ci_allcause <- rr_allcause%>% 
   
   #Estimate the age-specific components before summing
   mutate(num_allcause_m = ((weights^2)*deaths_M) / pop_M^2,
          num_allcause_f = ((weights^2)*deaths_F) / pop_F^2,
          num_covid_m = ((weights^2)*cvd_deaths_M) / pop_M^2,
          num_covid_f = ((weights^2)*cvd_deaths_F) / pop_F^2,
          den_allcause_m = weights*(deaths_M/pop_M),
          den_allcause_f = weights*(deaths_F/pop_F),
          den_covid_m = weights*(cvd_deaths_M/pop_M),
          den_covid_f = weights*(cvd_deaths_F/pop_F)) %>% 
   
   #Sum these things
   summarize(num_allcause_m = sum(num_allcause_m),
             num_allcause_f = sum(num_allcause_f),
             num_covid_m = sum(num_covid_m),
             num_covid_f = sum(num_covid_f),
             den_allcause_m = sum(den_allcause_m),
             den_allcause_f = sum(den_allcause_f),
             den_covid_m = sum(den_covid_m),
             den_covid_f = sum(den_covid_f)) %>% 
   
   #Estimate the logratio of ratios
   mutate(ln_ratio_of_ratios = log((den_covid_m*den_allcause_f) / (den_covid_f*den_allcause_m)),
          ln_allcause_ratio = log(den_allcause_m/den_allcause_f),
          ln_cvd_ratio = log(den_covid_m/den_covid_f)) %>% 
   
   #Estimate the standard error of the log of ratio of ratios
   mutate(se_ln_ratio_of_ratios = sqrt((num_allcause_m / den_allcause_m^2) + (num_allcause_f / den_allcause_f^2) + (num_covid_m / den_covid_m^2) + (num_covid_f / den_covid_f^2)),
          se_ln_allcause_ratio = sqrt((num_allcause_m / den_allcause_m^2) + (num_allcause_f / den_allcause_f^2)),
          se_ln_cvd_ratio = sqrt((num_covid_m / den_covid_m^2) + (num_covid_f / den_covid_f^2))) %>% 
   
   #Estimate ln CI bounds
   mutate(ln_em_lcl = ln_ratio_of_ratios - 1.96*se_ln_ratio_of_ratios,
          ln_em_ucl = ln_ratio_of_ratios + 1.96*se_ln_ratio_of_ratios,
          ln_rr_allcause_lcl = ln_allcause_ratio - 1.96*se_ln_allcause_ratio,
          ln_rr_allcause_ucl = ln_allcause_ratio + 1.96*se_ln_allcause_ratio,
          ln_rr_cvd_lcl = ln_cvd_ratio - 1.96*se_ln_cvd_ratio,
          ln_rr_cvd_ucl = ln_cvd_ratio + 1.96*se_ln_cvd_ratio) %>% 
   
   #Estimate the ratio of ratio with its bounds
   mutate(em = exp(ln_ratio_of_ratios),
          em_lcl = exp(ln_em_lcl),
          em_ucl = exp(ln_em_ucl),
          rr_allcause = exp(ln_allcause_ratio),
          rr_allcause_lcl = exp(ln_rr_allcause_lcl),
          rr_allcause_ucl = exp(ln_rr_allcause_ucl),
          rr_cvd = exp(ln_cvd_ratio),
          rr_cvd_lcl = exp(ln_rr_cvd_lcl),
          rr_cvd_ucl = exp(ln_rr_cvd_ucl)) %>%
   #keep vars needed for plotting the graphs
   select(c(country,rr_cvd,rr_cvd_lcl,rr_cvd_ucl,rr_allcause,rr_allcause_lcl,rr_allcause_ucl,em,em_lcl,em_ucl))

  #remove norway and dominican republic
  ci_allcause <- ci_allcause %>% filter(country != "Dominican Republic") %>% na.omit() %>% filter(country != "Norway")
 
 
#plot the graph by rr and excess mortality 
 #plot the excess mortality graph
 #plot excess mortality graphs
 plot_em_allcause <- ci_allcause %>% na.omit() %>% filter(em <10) %>%
   mutate(country = fct_reorder(country, -em)) %>%
   ggplot(aes(y=country, x=em)) +
   geom_point(size=2, colour = "deepskyblue3", alpha=1,aes(em))+
   geom_errorbarh(aes(xmin=em_lcl, xmax=em_ucl), colour = "deepskyblue3", height=0.2, size = 0.5, 
                  show.legend = F, alpha = 1)+
   geom_vline(aes(xintercept = 1), colour = 'black', lty = 2, size = 1) + 
   #geom_point(size=2, alpha=1, colour = "red") +
   labs(title = "", x = "Relative difference in rate ratio", y = "") + 
   scale_x_continuous(breaks = c(0.25,0.5,1,2,4,8), limits=c(0.01, 8)) +
   theme(panel.background = element_blank(), axis.line.x = element_line(), axis.line.y = element_line(),
         axis.text.y = element_text(size = 12, colour = "black"), axis.text.x = element_text(colour = "black", size = 11),
         plot.title = element_text(margin = margin(0,2,0,2,"cm"), hjust = 0.5),
         axis.title=element_text(size=11), panel.border = element_rect(fill = NA),
         legend.position = "bottom", legend.text = element_text(size = 11))+
   coord_trans(x="log10", y = "identity", xlim = c(0.4, 8))
 plot_em_allcause
 
 
 #plot the graph of rate ratios 
 plot_rr_allcause <- ci_allcause %>% na.omit() %>% filter(country != "Dominican Republic" |country != "Norway") %>%
   mutate(country = fct_reorder(country, -rr_cvd)) %>%
   ggplot(aes(y=country, x=rr_cvd)) + 
   geom_vline(aes(xintercept = 1), colour = 'black', lty = 2, size = 1) + 
   geom_point(size=2, alpha=1,aes(colour = "COVID-19")) +
   geom_errorbarh(aes(xmin=rr_cvd_lcl, xmax=rr_cvd_ucl, colour = "COVID-19"), height=0.2, size = 0.5, 
                  show.legend = F, alpha = 1) + 
   geom_point(aes(y=country, x=rr_allcause, colour = "All-causes"), size = 2, show.legend = TRUE)+
   geom_errorbarh(aes(xmin=rr_allcause_lcl, xmax=rr_allcause_ucl, colour = "All-causes"), height=0.2, size = 0.5,
                  show.legend = F, alpha = 1) + 
   scale_colour_manual(name = "", breaks = c("COVID-19", "All-causes"),
                       values = c("COVID-19" = "red", "All-causes" = "blue"))+
   labs(x = "Rate ratio", y = "") + #ggtitle("Male:Female age-adjusted risk-ratio of mortality")+
   scale_x_continuous(breaks = c(0.5,1,2,4,8), limits=c(0.01, 9)) +
   theme(panel.background = element_blank(), axis.line.x = element_line(), axis.line.y = element_line(),
         axis.text.y = element_text(size = 12, colour = "black"), axis.text.x = element_text(colour = "black", size = 12),
         plot.title = element_text(margin = margin(0.5,0,0,0,"cm"), hjust = 0.6, face = "bold"),
         axis.title=element_text(size=12, face = "bold"), panel.border = element_rect(fill = NA),
         legend.position = "bottom", legend.text = element_text(size = 11))+
   coord_trans(x="log10", y = "identity", xlim = c(0.8, 9))
 plot_rr_allcause
 
 #save the plots 
 tiff(file="./outputs/final graphs/em_allcause_12Feb.tiff",
      width=700, height=1400, units="px", res=100)
 plot_em_allcause
 dev.off()
 
 tiff(file="./outputs/final graphs/rr_allcause_12Feb.tiff",
      width=700, height=1400, units="px", res=100)
 plot_rr_allcause
 dev.off()
 
 
#plot suggested by TIM
 
scatter_allcase <-  ci_allcause %>% na.omit() %>% filter(rr_cvd <10) %>%
    mutate(country = fct_reorder(country, -rr_cvd)) %>%
    ggplot(aes(y=rr_allcause, x=rr_cvd)) +
    geom_abline(lty = 1, size = 0.5, colour = "dimgray")+
     #geom_vline(aes(xintercept = 1), colour = 'black', lty = 2, size = 1) + 
    geom_point(size=2, alpha=1, colour = 'red')+
    geom_text_repel(aes(label = country))+
    #geom_text(aes(label=country),hjust=-0.15, vjust=0.2, size = 3.5) +
    labs(x = "COVID-19 case-fatality ratio", y = "Rate ratio of all-cause mortality")+
    scale_x_continuous(breaks = c(1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0))+
    coord_fixed(ratio = 3, xlim = c(1.0,5), ylim = c(1.0,2.2))+
    theme(panel.background = element_rect(colour = "black", fill = NA),
          panel.grid = element_blank())
scatter_allcase

 tiff(file="./outputs/final graphs/scatter_allcase.tiff",
      width=800, height=800, units="px", res=100)
 scatter_allcase
 dev.off()

#save the table 
 write.csv(table1_allcause, file = "./outputs/tables/Table_1_09Feb.csv")
 
 
#plot rates for men on y and for women on x axis;
  plot_covid <- age_std_allcause %>% na.omit() %>% 
     ggplot(aes(y = cvd_m, x = cvd_f))+
     geom_point(size=2, alpha=1, colour = 'red')+
     coord_fixed(ratio = 1, xlim = c(0.0,0.003), ylim = c(0.0,0.003))+
     geom_text_repel(data = filter(age_std_allcause, cvd_m > 3.002536e-04 & country != "Norway"), aes(label = country))+
     geom_abline(lty = 1, size = 0.5, colour = "dimgray")+
     labs(x = "COVID-19 mortality rate in women", y = "COVID-19 mortality rate in men")+
     theme(panel.background = element_rect(colour = "black", fill = NA),
           panel.grid = element_blank())
     
     
  plot_allcause <- age_std_allcause %>% na.omit() %>% 
     ggplot(aes(y = allcause_m, x = allcause_f))+
     geom_point(size=2, alpha=1, colour = 'red')+
     coord_fixed(ratio = 1, xlim = c(0.0,0.09), ylim = c(0.0,0.09))+
     geom_text_repel(data = filter(age_std_allcause, allcause_m > 0.013846159 & country != "Norway"), aes(label = country))+
     geom_abline(lty = 1, size = 0.5, colour = "dimgray")+
     labs(x = "All-cause mortality rate in women", y = "All-cause mortality rate in men")+
     theme(panel.background = element_rect(colour = "black", fill = NA),
           panel.grid = element_blank())

  plot_rd <- ggarrange(plot_covid, plot_allcause, nrow = 1, ncol = 2)
  
  
  #plot rate differences in COVID19 and allcause mortality
  
 #prepare the data
  rd_covid <- age_std_allcause %>% select(c(country, rd_covid)) %>% mutate(cause = "COVID-19") %>% rename(rd = rd_covid)
  rd_allcause <- age_std_allcause %>% select(c(country, rd_allcause)) %>% mutate(cause = "All-cause") %>% rename(rd = rd_allcause)
  rd_dat <- rbind(rd_allcause,rd_covid)
#make a bar plot of difference in mortality
  #install.packages("lattice")
  #library(lattice)
  barchart(rd~country,data=rd_dat,groups=cause, 
           scales=list(x=list(rot=90,cex=0.8)), ylab = "Men to women rate difference in mortality")
  

  
  rd_dat %>% na.omit() %>% filter(country != c("Austria")) %>% filter(country != c("Norway")) %>%
     ggplot(aes(y = rd, x = reorder(country, -rd), fill = cause))+
     geom_bar(position = "dodge", stat = "identity", colour = "black")+
     scale_color_manual(values = c("red","blue"))+
     labs(y = "Men to women rate difference in mortality", x = "")+
     #make country labels vertical
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),panel.background = element_rect(colour = "black", fill = NA),
           panel.grid = element_blank(), legend.position = c(0.8,0.8))
  
  
  ggplot(rd_dat, aes(fill=cause, y=rd, x=country)) + 
     geom_bar(position="dodge", stat="identity")+
     labs(y = "Men to women rate difference in mortality")
     
  
  
  
  
  
  