# install and load packages
require(tidyverse)
require(foreign)
require(haven)
require(dplyr)
require(data.table)
# load data

filepath <- "C:/Users/moctar.aboubacar/Desktop/Data2019_1.csv"
dat <- read.csv(filepath)

# count missing values function
na.count <- function(a){
  require(data.table)
  sum.missing <- a %>% 
    #select_if(is.numeric) %>% 
    map_dbl(function(x) sum(is.na(x)))
  percent.missing <- a %>%
    #select_if(is.numeric) %>% 
    map_dbl(function(x) round(sum(is.na(x))/nrow(a)*100,1))
  sum.missing <- as.data.frame(sum.missing)
  percent.missing <- as.data.frame(percent.missing)
  na.table <<- setDT(cbind(sum.missing, percent.missing), keep.rownames = "variable")[] # <<- assigns df outside the function call
  na.table.ggplot <<- na.table %>%
    arrange(percent.missing) %>%
    ggplot(aes(x = reorder(variable, -percent.missing), y = percent.missing, color = percent.missing))+
    scale_color_gradientn(colours = rainbow(4))+
    xlab("Variables")+
    geom_bar(stat = 'identity')+
    ggtitle("Percent missing")
  return(na.table)
  return(na.table.ggplot)
} 
na.count(dat)
na.table.ggplot

# trim and remove NAs
dat <- as.data.frame(apply(dat, 2, function(x) gsub("^\\s+|\\s+$", "", x)))

# get rid of pesky NAs (assuming that all NAs can carry a value of 0)
dat[is.na(dat)] <- 0

# arrange the psus into a single column (excel will do this with a simple concat...)
dat.2 <- dat %>%
  mutate(a.5.respondent_is_hh_head = 
           factor(
             case_when(a.5.respondent_is_hh_head == "No" ~ 0,
                       a.5.respondent_is_hh_head == "Yes" ~ 1),
             levels = c(0, 1),
             labels = c("No", "Yes")),
         a.6.female_hh_head = 
           factor(
             case_when(a.6.female_hh_head == "Male" ~ 0,
                       a.6.female_hh_head == "Female" ~ 1),
             levels = c(0, 1),
             labels = c("Male", "Female")),
         b.3.caste_ethnicity = 
           factor(
             case_when(b.3.caste_ethnicity == "Brahmin" ~ 1,
                       b.3.caste_ethnicity == "Chhetri" ~ 2,
                       b.3.caste_ethnicity == "Dalit" ~ 3,
                       b.3.caste_ethnicity == "Janajti" ~ 4,
                       b.3.caste_ethnicity == "Other" ~ 5),
             levels = c(1, 2, 3, 4, 5),
             labels = c("Brahmin", "Chhetri", "Dalit", "Janjati", "Other")),
         b.4.education_level_hh_head =
           factor(
             case_when(b.4.education_level_hh_head == "Pre school/Kindergarden" ~ 1,
                       b.4.education_level_hh_head == "Class 1" ~ 2,
                       b.4.education_level_hh_head == "Class 2" ~ 3,
                       b.4.education_level_hh_head == "Class 3" ~ 4,
                       b.4.education_level_hh_head == "Class 4" ~ 5,
                       b.4.education_level_hh_head == "Class 5" ~ 6,
                       b.4.education_level_hh_head == "Class 6" ~ 7,
                       b.4.education_level_hh_head == "Class 7" ~ 8,
                       b.4.education_level_hh_head == "Class 8" ~ 9,
                       b.4.education_level_hh_head == "Class 9" ~ 10,
                       b.4.education_level_hh_head == "Class 10" ~ 11,
                       b.4.education_level_hh_head == "SLC/SEE" ~ 12,
                       b.4.education_level_hh_head == "Class 12/ Intermediate level" ~ 13,
                       b.4.education_level_hh_head == "Bechelor" ~ 14,
                       b.4.education_level_hh_head == "Mater level" ~ 15,
                       b.4.education_level_hh_head == "Ph.D" ~ 16,
                       b.4.education_level_hh_head == "Literate(Non formal education)" ~ 17,
                       b.4.education_level_hh_head == "Illiterate" ~ 18),
             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
             labels = c("Preschool", "Class 1", "Class 2", "Class 3", "Class 4", "Class 5", "Class 6", "Class 7", "Class 8", "Class 9", "Class 10", "SLC/SEE", "Class 12", "Bachelor", "Master", "PhD", "No formal edu-literate", "No formal edu-illiterate")),
         b.6.own_land = 
           factor(
             case_when(b.6.own_land == "Yes" ~ 1,
                       b.6.own_land == "No" ~ 0),
             levels = c(0, 1),
             labels = c("No", "Yes")),       
         b.7.own_livestock = 
           factor(
             case_when(b.7.own_livestock == "No" ~ 0,
                       b.7.own_livestock == "Yes" ~ 1),
             levels = c(0, 1),
             labels = c("No", "Yes")),           
         b.8.toilet_type = 
           factor(
             case_when(b.8.toilet_type == "Flush - connected to municipal sewer" ~ 1,
                       b.8.toilet_type == "Flush - connected to septic tank" ~ 2,
                       b.8.toilet_type == "Household non flush" ~ 3,
                       b.8.toilet_type == "Communal latrin" ~ 4,
                       b.8.toilet_type == "No toilet" ~ 5),
             levels = c(1, 2, 3, 4, 5),
             labels = c("Flush-Municipal-sewer", "Flush-septic-tank", "Non-flush", "Communal-latrin", "No-toilet")),
         b.9.drinking_water = 
           factor(
             case_when(b.9.drinking_water == "Piped to water supply" ~ 1,
                       b.9.drinking_water == "Covered well" ~ 2,
                       b.9.drinking_water == "Uncovered Well" ~ 3,
                       b.9.drinking_water == "Hand pump/tubewell" ~ 4,
                       b.9.drinking_water == "Spring water" ~ 5,
                       b.9.drinking_water == "River/pond" ~ 6,
                       b.9.drinking_water == "Public piped water supply" ~ 7,
                       b.9.drinking_water == "Other" ~ 8),
             levels = c(1, 2, 3, 4, 5, 6, 7, 8),
             labels = c("Piped-water", "Covered-well", "Uncovered-well", "Handpump", "Spring-water", "River-pond", "Public-piped-water", "Other")),
         b.10.lighting = 
           factor(
             case_when(b.10.lighting == "Electricity" ~ 1,
                       b.10.lighting == "Gas/oil/kerosine" ~ 2,
                       b.10.lighting == "Generator" ~ 3,
                       b.10.lighting == "Bio gas" ~ 4,
                       b.10.lighting == "Solar" ~ 5,
                       b.10.lighting == "Other" ~ 6),
             levels = c(1, 2, 3, 4, 5, 6),
             labels = c("Electrcity", "Kerosene", "Generator", "Bio-gas", "Solar", "Other")),
         b.11.cooking_fuel = 
           factor(
             case_when(b.11.cooking_fuel == "Firewood" ~ 1,
                       b.11.cooking_fuel == "LPG Gas" ~ 2,
                       b.11.cooking_fuel == "Electricity" ~ 3,
                       b.11.cooking_fuel == "Bio gas" ~ 4,
                       b.11.cooking_fuel == "Animal Dung" ~ 5,
                       b.11.cooking_fuel == "Kerosene" ~ 6,
                       b.11.cooking_fuel == "Leaves/rubbish/straw/thatch" ~ 7,
                       b.11.cooking_fuel == "Others" ~ 8),
             levels = c(1, 2, 3, 4, 5, 6, 7, 8),
             labels = c("Firewood", "LPG-gas", "Electricity", "Biogas", "Animal-dung", "Kerosene", "Leaves-etc", "Other")),
         c.1.income_source_1 = 
           factor(
             case_when(c.1.income_source_1 == "Agriculture (mostly cereal production)" ~ 1,
                       c.1.income_source_1 == "Agriculture (mostly cash/high value crops)" ~ 2,
                       c.1.income_source_1 == "Livestock" ~ 3,
                       c.1.income_source_1 == "Agriculture wage labour (Unskilled)" ~ 4,
                       c.1.income_source_1 == "Other unskilled labour (porter,stone quarry worker,etc)" ~ 5,
                       c.1.income_source_1 == "Skilled labour (masonry,carpentry,etc)" ~ 6,
                       c.1.income_source_1 == "Remittance" ~ 7,
                       c.1.income_source_1 == "Salaried Employment(Govt/private companies/NGO/Ingo)" ~ 8,
                       c.1.income_source_1 == "Trade/Shop keeping" ~ 9,
                       c.1.income_source_1 == "Sale of NTFP" ~ 10,
                       c.1.income_source_1 == "Social benefit schemes" ~ 11,
                       c.1.income_source_1 == "Humanitarian/Development assistance" ~ 12,
                       c.1.income_source_1 == "Other" ~ 13,
                       c.1.income_source_1 == NA ~ 14),
             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
             labels = c("Agri-cereal", "Agri-cash", "Livestock", "Unskilled-agri", "Unskilled-other", "Skilled", "Remittances", "Salaried", "Trade-shop", "NTFP", "Social-transfers", "Humanitarian-assistance", "Other", "NA")),
         c.1.income_source_2 = 
           factor(
             case_when(c.1.income_source_2 == "Agriculture (mostly cereal production)" ~ 1,
                       c.1.income_source_2 == "Agriculture (mostly cash/high value crops)" ~ 2,
                       c.1.income_source_2 == "Livestock" ~ 3,
                       c.1.income_source_2 == "Agriculture wage labour (Unskilled)" ~ 4,
                       c.1.income_source_2 == "Other unskilled labour (porter,stone quarry worker,etc)" ~ 5,
                       c.1.income_source_2 == "Skilled labour (masonry,carpentry,etc)" ~ 6,
                       c.1.income_source_2 == "Remittance" ~ 7,
                       c.1.income_source_2 == "Salaried Employment(Govt/private companies/NGO/Ingo)" ~ 8,
                       c.1.income_source_2 == "Trade/Shop keeping" ~ 9,
                       c.1.income_source_2 == "Sale of NTFP" ~ 10,
                       c.1.income_source_2 == "Social benefit schemes" ~ 11,
                       c.1.income_source_2 == "Humanitarian/Development assistance" ~ 12,
                       c.1.income_source_2 == "Other" ~ 13,
                       c.1.income_source_1 == NA ~ 14),
             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
             labels = c("Agri-cereal", "Agri-cash", "Livestock", "Unskilled-agri", "Unskilled-other", "Skilled", "Remittances", "Salaried", "Trade-shop", "NTFP", "Social-transfers", "Humanitarian-assistance", "Other", "NA")),
         c.1.income_source_3 = 
           factor(
             case_when(c.1.income_source_3 == "Agriculture (mostly cereal production)" ~ 1,
                       c.1.income_source_3 == "Agriculture (mostly cash/high value crops)" ~ 2,
                       c.1.income_source_3 == "Livestock" ~ 3,
                       c.1.income_source_3 == "Agriculture wage labour (Unskilled)" ~ 4,
                       c.1.income_source_3 == "Other unskilled labour (porter,stone quarry worker,etc)" ~ 5,
                       c.1.income_source_3 == "Skilled labour (masonry,carpentry,etc)" ~ 6,
                       c.1.income_source_3 == "Remittance" ~ 7,
                       c.1.income_source_3 == "Salaried Employment(Govt/private companies/NGO/Ingo)" ~ 8,
                       c.1.income_source_3 == "Trade/Shop keeping" ~ 9,
                       c.1.income_source_3 == "Sale of NTFP" ~ 10,
                       c.1.income_source_3 == "Social benefit schemes" ~ 11,
                       c.1.income_source_3 == "Humanitarian/Development assistance" ~ 12,
                       c.1.income_source_3 == "Other" ~ 13,
                       c.1.income_source_1 == NA ~ 14),
             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
             labels = c("Agri-cereal", "Agri-cash", "Livestock", "Unskilled-agri", "Unskilled-other", "Skilled", "Remittances", "Salaried", "Trade-shop", "NTFP", "Social-transfers", "Humanitarian-assistance", "Other", "NA")),
         c.1.income_source_4 = 
           factor(
             case_when(c.1.income_source_4 == "Agriculture (mostly cereal production)" ~ 1,
                       c.1.income_source_4 == "Agriculture (mostly cash/high value crops)" ~ 2,
                       c.1.income_source_4 == "Livestock" ~ 3,
                       c.1.income_source_4 == "Agriculture wage labour (Unskilled)" ~ 4,
                       c.1.income_source_4 == "Other unskilled labour (porter,stone quarry worker,etc)" ~ 5,
                       c.1.income_source_4 == "Skilled labour (masonry,carpentry,etc)" ~ 6,
                       c.1.income_source_4 == "Remittance" ~ 7,
                       c.1.income_source_4 == "Salaried Employment(Govt/private companies/NGO/Ingo)" ~ 8,
                       c.1.income_source_4 == "Trade/Shop keeping" ~ 9,
                       c.1.income_source_4 == "Sale of NTFP" ~ 10,
                       c.1.income_source_4 == "Social benefit schemes" ~ 11,
                       c.1.income_source_4 == "Humanitarian/Development assistance" ~ 12,
                       c.1.income_source_4 == "Other" ~ 13,
                       c.1.income_source_1 == NA ~ 14),
             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
             labels = c("Agri-cereal", "Agri-cash", "Livestock", "Unskilled-agri", "Unskilled-other", "Skilled", "Remittances", "Salaried", "Trade-shop", "NTFP", "Social-transfers", "Humanitarian-assistance", "Other", "NA")),
         c.1.income_source_5 = 
           factor(
             case_when(c.1.income_source_5 == "Agriculture (mostly cereal production)" ~ 1,
                       c.1.income_source_5 == "Agriculture (mostly cash/high value crops)" ~ 2,
                       c.1.income_source_5 == "Livestock" ~ 3,
                       c.1.income_source_5 == "Agriculture wage labour (Unskilled)" ~ 4,
                       c.1.income_source_5 == "Other unskilled labour (porter,stone quarry worker,etc)" ~ 5,
                       c.1.income_source_5 == "Skilled labour (masonry,carpentry,etc)" ~ 6,
                       c.1.income_source_5 == "Remittance" ~ 7,
                       c.1.income_source_5 == "Salaried Employment(Govt/private companies/NGO/Ingo)" ~ 8,
                       c.1.income_source_5 == "Trade/Shop keeping" ~ 9,
                       c.1.income_source_5 == "Sale of NTFP" ~ 10,
                       c.1.income_source_5 == "Social benefit schemes" ~ 11,
                       c.1.income_source_5 == "Humanitarian/Development assistance" ~ 12,
                       c.1.income_source_5 == "Other" ~ 13,
                       c.1.income_source_1 == NA ~ 14),
             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
             labels = c("Agri-cereal", "Agri-cash", "Livestock", "Unskilled-agri", "Unskilled-other", "Skilled", "Remittances", "Salaried", "Trade-shop", "NTFP", "Social-transfers", "Humanitarian-assistance", "Other", "NA")),
         c.2.female_working = 
           factor(
             case_when(c.2.female_working == "No" ~ 0,
                       c.2.female_working == "Yes" ~ 1),
             levels = c(0, 1),
             labels = c("No", "Yes")),
         d.5.sell_own_product_at_mkt =
           factor(
             case_when(d.5.sell_own_product_at_mkt == "No" ~ 0,
                       d.5.sell_own_product_at_mkt == "Yes" ~ 1),
             levels = c(0, 1),
             labels = c("No", "Yes")),
         e.1.absentee_migrant = 
           factor(
             case_when(e.1.absentee_migrant == "No" ~ 0,
                       e.1.absentee_migrant == "Yes" ~ 1),
             levels = c(0, 1),
             labels = c("No", "Yes")),
         f.6.food_stock_last_year_compare = 
           factor(
             case_when(f.6.food_stock_last_year_compare == "Increased" ~ 1,
                       f.6.food_stock_last_year_compare == "Remained more or less the same" ~ 2,
                       f.6.food_stock_last_year_compare == "Decreased" ~ 3),
             levels = c(1, 2, 3),
             labels = c("Increased", "Same", "Decreased")),
         f.7.reason_for_food_stock_decrease = 
           factor(
             case_when(f.7.reason_for_food_stock_decrease == "Drought resulting in crop loss" ~ 1,
                       f.7.reason_for_food_stock_decrease == "Lack of or less agriculture inputs (credits,seeds,fertilizers)" ~ 2,
                       f.7.reason_for_food_stock_decrease == "Less land cultivated than last year" ~ 3,
                       f.7.reason_for_food_stock_decrease == "Pests and diseases of crops" ~ 4,
                       f.7.reason_for_food_stock_decrease == "Other" ~ 5),
             levels = c(1, 2, 3, 4, 5),
             labels = c("Drought", "Fewer-inputs", "Less-land", "Pests-disease", "Other")),
         h.1.1.source_cereals = 
           factor(
             case_when(h.1.1.source_cereals == "Own production" ~ 1,
                       h.1.1.source_cereals == "Market purchase" ~ 2,
                       h.1.1.source_cereals == "Gathering wild foods/hunting" ~ 3,
                       h.1.1.source_cereals == "Aid/gift/donation" ~ 4,
                       h.1.1.source_cereals == NA ~ 5),
             levels = c(1, 2, 3, 4, 5),
             labels = c("Own-product", "Market", "Gathering", "Gift", "NA")),
         h.2.1.source_pulses = 
           factor(
             case_when(h.2.1.source_pulses == "Own production" ~ 1,
                       h.2.1.source_pulses == "Market purchase" ~ 2,
                       h.2.1.source_pulses == "Gathering wild foods/hunting" ~ 3,
                       h.2.1.source_pulses == "Aid/gift/donation" ~ 4,
                       h.2.1.source_pulses == NA ~ 5),
             levels = c(1, 2, 3, 4, 5),
             labels = c("Own-product", "Market", "Gathering", "Gift", "NA")),
         h.3.1.source_dairy = 
           factor(
             case_when(h.3.1.source_dairy == "Own production" ~ 1,
                       h.3.1.source_dairy == "Market purchase" ~ 2,
                       h.3.1.source_dairy == "Gathering wild foods/hunting" ~ 3,
                       h.3.1.source_dairy == "Aid/gift/donation" ~ 4,
                       h.3.1.source_dairy == NA ~ 5),
             levels = c(1, 2, 3, 4, 5),
             labels = c("Own-product", "Market", "Gathering", "Gift", "NA")),
         h.4.1.source_meatfish = 
           factor(
             case_when(h.4.1.source_meatfish == "Own production" ~ 1,
                       h.4.1.source_meatfish == "Market purchase" ~ 2,
                       h.4.1.source_meatfish == "Gathering wild foods/hunting" ~ 3,
                       h.4.1.source_meatfish == "Aid/gift/donation" ~ 4,
                       h.4.1.source_meatfish == NA ~ 5),
             levels = c(1, 2, 3, 4, 5),
             labels = c("Own-product", "Market", "Gathering", "Gift", "NA")),
         h.5.1.source_vegetables = 
           factor(
             case_when(h.5.1.source_vegetables == "Own production" ~ 1,
                       h.5.1.source_vegetables == "Market purchase" ~ 2,
                       h.5.1.source_vegetables == "Gathering wild foods/hunting" ~ 3,
                       h.5.1.source_vegetables == "Aid/gift/donation" ~ 4,
                       h.5.1.source_vegetables == NA ~ 5),
             levels = c(1, 2, 3, 4, 5),
             labels = c("Own-product", "Market", "Gathering", "Gift", "NA")),
         h.6.1.source_fruit = 
           factor(
             case_when(h.6.1.source_fruit == "Own production" ~ 1,
                       h.6.1.source_fruit == "Market purchase" ~ 2,
                       h.6.1.source_fruit == "Gathering wild foods/hunting" ~ 3,
                       h.6.1.source_fruit == "Aid/gift/donation" ~ 4,
                       h.6.1.source_fruit == NA ~ 5),
             levels = c(1, 2, 3, 4, 5),
             labels = c("Own-product", "Market", "Gathering", "Gift", "NA")),
         h.7.1.source_oilfat = 
           factor(
             case_when(h.7.1.source_oilfat == "Own production" ~ 1,
                       h.7.1.source_oilfat == "Market purchase" ~ 2,
                       h.7.1.source_oilfat == "Gathering wild foods/hunting" ~ 3,
                       h.7.1.source_oilfat == "Aid/gift/donation" ~ 4,
                       h.7.1.source_oilfat == NA ~ 5),
             levels = c(1, 2, 3, 4, 5),
             labels = c("Own-product", "Market", "Gathering", "Gift", "NA")),
         h.8.1.source_sugar = 
           factor(
             case_when(h.8.1.source_sugar == "Own production" ~ 1,
                       h.8.1.source_sugar == "Market purchase" ~ 2,
                       h.8.1.source_sugar == "Gathering wild foods/hunting" ~ 3,
                       h.8.1.source_sugar == "Aid/gift/donation" ~ 4,
                       h.8.1.source_sugar == NA ~ 5),
             levels = c(1, 2, 3, 4, 5),
             labels = c("Own-product", "Market", "Gathering", "Gift", "NA")),
         i.1.shocks_6months = 
           factor(
             case_when(i.1.shocks_6months == "No" ~ 0,
                       i.1.shocks_6months == "Yes" ~ 1),
             levels = c(0, 1),
             labels = c("No", "Yes")),
         i.2.most_imp_shock_faced = 
           factor(
             case_when(i.2.most_imp_shock_faced == "Loss of crops due to drought" ~ 1,
                       i.2.most_imp_shock_faced == "Loss of crops due to floods/landslide" ~ 2,
                       i.2.most_imp_shock_faced == "Loss of crops due to pests/diseases" ~ 3,
                       i.2.most_imp_shock_faced == "Loss of crops due to hail stone" ~ 4,
                       i.2.most_imp_shock_faced == "Livestock loss" ~ 5,
                       i.2.most_imp_shock_faced == "Lack or loss of employment" ~ 6,
                       i.2.most_imp_shock_faced == "Human disease/illness or accident of HH members" ~ 7,
                       i.2.most_imp_shock_faced == "Death of working HH member" ~ 8,
                       i.2.most_imp_shock_faced == "High food price increase" ~ 9,
                       i.2.most_imp_shock_faced == "No food supply in relavent market" ~ 10,
                       i.2.most_imp_shock_faced == "Theft/kidnapping/fraud" ~ 11,
                       i.2.most_imp_shock_faced == "Fire" ~ 12,
                       i.2.most_imp_shock_faced == "Failure of household business" ~ 13,
                       i.2.most_imp_shock_faced == "Low price of HH business product" ~ 14,
                       i.2.most_imp_shock_faced == "Others" ~ 15,
                       i.2.most_imp_shock_faced == NA ~ 16),
             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
             labels = c("Crops-drought", "Crops-flood-landslide", "Crops-pests-disease", "Crops-hail", "Loss-livestock", "Loss-employment", "Human-disease", "Human-death-working", "Food-price-increase", "Food-supply-low", "Theft-kidnapping-fraud", "Fire", "HH-business-failure", "HH-business-lowprice", "Other", "NA")),
         i.3.shocks_recovered = 
           factor(
             case_when(i.3.shocks_recovered == "Not recovered at all" ~ 1,
                       i.3.shocks_recovered == "Partially recovered" ~ 2,
                       i.3.shocks_recovered == "Completely recovered" ~ 3),
             levels = c(1, 2, 3),
             labels = c("Not recovered", "Partially recovered", "Completely recovered")),
         j.1.enough_food_or_food_money = 
           factor(
             case_when(j.1.enough_food_or_food_money == "No" ~ 0,
                       j.1.enough_food_or_food_money == "Yes" ~ 1),
             levels = c(0, 1),
             labels = c("No", "Yes")),
         j.2.1.borrow_food_money_from_lender = 
           factor(
             case_when(j.2.1.borrow_food_money_from_lender == "Yes" ~ 1,
                       j.2.1.borrow_food_money_from_lender == "No, it was not necessary" ~ 2,
                       j.2.1.borrow_food_money_from_lender == "No, because I already did it before and cannot continue to do it" ~ 3,
                       j.2.1.borrow_food_money_from_lender == "Not applicable" ~ 4),
             levels = c(1, 2, 3, 4),
             labels = c("Yes", "No not necessary", "No already did", "NA")),
         j.2.2.sell_nonproductive_animals = 
           factor(
             case_when(j.2.2.sell_nonproductive_animals == "Yes" ~ 1,
                       j.2.2.sell_nonproductive_animals == "No, it was not necessary" ~ 2,
                       j.2.2.sell_nonproductive_animals == "No, because I already did it before and cannot continue to do it" ~ 3,
                       j.2.2.sell_nonproductive_animals == "Not applicable" ~ 4),
             levels = c(1, 2, 3, 4),
             labels = c("Yes", "No not necessary", "No already did", "NA")),
         j.2.3.sell_hh_assets_furniture_radio_jewelry = 
           factor(
             case_when(j.2.3.sell_hh_assets_furniture_radio_jewelry == "Yes" ~ 1,
                       j.2.3.sell_hh_assets_furniture_radio_jewelry == "No, it was not necessary" ~ 2,
                       j.2.3.sell_hh_assets_furniture_radio_jewelry == "No, because I already did it before and cannot continue to do it" ~ 3,
                       j.2.3.sell_hh_assets_furniture_radio_jewelry == "Not applicable" ~ 4),
             levels = c(1, 2, 3, 4),
             labels = c("Yes", "No not necessary", "No already did", "NA")),
         j.2.4.sell_productive_assets = 
           factor(
             case_when(j.2.4.sell_productive_assets == "Yes" ~ 1,
                       j.2.4.sell_productive_assets == "No, it was not necessary" ~ 2,
                       j.2.4.sell_productive_assets == "No, because I already did it before and cannot continue to do it" ~ 3,
                       j.2.4.sell_productive_assets == "Not applicable" ~ 4),
             levels = c(1, 2, 3, 4),
             labels = c("Yes", "No not necessary", "No already did", "NA")),
         j.2.5.withdraw_children_school = 
           factor(
             case_when(j.2.5.withdraw_children_school == "Yes" ~ 1,
                       j.2.5.withdraw_children_school == "No, it was not necessary" ~ 2,
                       j.2.5.withdraw_children_school == "No, because I already did it before and cannot continue to do it" ~ 3,
                       j.2.5.withdraw_children_school == "Not applicable" ~ 4),
             levels = c(1, 2, 3, 4),
             labels = c("Yes", "No not necessary", "No already did", "NA")),
         j.2.6.harvest_immature_crops = 
           factor(
             case_when(j.2.6.harvest_immature_crops == "Yes" ~ 1,
                       j.2.6.harvest_immature_crops == "No, it was not necessary" ~ 2,
                       j.2.6.harvest_immature_crops == "No, because I already did it before and cannot continue to do it" ~ 3,
                       j.2.6.harvest_immature_crops == "Not applicable" ~ 4),
             levels = c(1, 2, 3, 4),
             labels = c("Yes", "No not necessary", "No already did", "NA")),
         j.2.6.harvest_immature_crops = 
           factor(
             case_when(j.2.6.harvest_immature_crops == "Yes" ~ 1,
                       j.2.6.harvest_immature_crops == "No, it was not necessary" ~ 2,
                       j.2.6.harvest_immature_crops == "No, because I already did it before and cannot continue to do it" ~ 3,
                       j.2.6.harvest_immature_crops == "Not applicable" ~ 4),
             levels = c(1, 2, 3, 4),
             labels = c("Yes", "No not necessary", "No already did", "NA")),
         j.2.7.sell_last_female_animal = 
           factor(
             case_when(j.2.7.sell_last_female_animal == "Yes" ~ 1,
                       j.2.7.sell_last_female_animal == "No, it was not necessary" ~ 2,
                       j.2.7.sell_last_female_animal == "No, because I already did it before and cannot continue to do it" ~ 3,
                       j.2.7.sell_last_female_animal == "Not applicable" ~ 4),
             levels = c(1, 2, 3, 4),
             labels = c("Yes", "No not necessary", "No already did", "NA")),
         j.2.8.sell_house_land = 
           factor(
             case_when(j.2.8.sell_house_land == "Yes" ~ 1,
                       j.2.8.sell_house_land == "No, it was not necessary" ~ 2,
                       j.2.8.sell_house_land == "No, because I already did it before and cannot continue to do it" ~ 3,
                       j.2.8.sell_house_land == "Not applicable" ~ 4),
             levels = c(1, 2, 3, 4),
             labels = c("Yes", "No not necessary", "No already did", "NA")),
         k.1.fies_worried = 
           factor(
             case_when(k.1.fies_worried == "Yes" ~ 1,
                       k.1.fies_worried == "No" ~ 2,
                       k.1.fies_worried == "Do not know" ~ 3),
             levels = c(1, 2, 3),
             labels = c("Yes", "No", "Do not know")),
         k.2.fies_healthy = 
           factor(
             case_when(k.2.fies_healthy == "Yes" ~ 1,
                       k.2.fies_healthy == "No" ~ 2,
                       k.2.fies_healthy == "Do not know" ~ 3),
             levels = c(1, 2, 3),
             labels = c("Yes", "No", "Do not know")),
         k.3.fies_fewfoods = 
           factor(
             case_when(k.3.fies_fewfoods == "Yes" ~ 1,
                       k.3.fies_fewfoods == "No" ~ 2,
                       k.3.fies_fewfoods == "Do not know" ~ 3),
             levels = c(1, 2, 3),
             labels = c("Yes", "No", "Do not know")),
         k.4.fies_skipped = 
           factor(
             case_when(k.4.fies_skipped == "Yes" ~ 1,
                       k.4.fies_skipped == "No" ~ 2,
                       k.4.fies_skipped == "Do not know" ~ 3),
             levels = c(1, 2, 3),
             labels = c("Yes", "No", "Do not know")),
         k.5.fies_ateless = 
           factor(
             case_when(k.5.fies_ateless == "Yes" ~ 1,
                       k.5.fies_ateless == "No" ~ 2,
                       k.5.fies_ateless == "Do not know" ~ 3),
             levels = c(1, 2, 3),
             labels = c("Yes", "No", "Do not know")),
         k.6.fies_ranout = 
           factor(
             case_when(k.6.fies_ranout == "Yes" ~ 1,
                       k.6.fies_ranout == "No" ~ 2,
                       k.6.fies_ranout == "Do not know" ~ 3),
             levels = c(1, 2, 3),
             labels = c("Yes", "No", "Do not know")),
         k.7.fies_hungry = 
           factor(
             case_when(k.7.fies_hungry == "Yes" ~ 1,
                       k.7.fies_hungry == "No" ~ 2,
                       k.7.fies_hungry == "Do not know" ~ 3),
             levels = c(1, 2, 3),
             labels = c("Yes", "No", "Do not know")),
         k.8.fies_whlday = 
           factor(
             case_when(k.8.fies_whlday == "Yes" ~ 1,
                       k.8.fies_whlday == "No" ~ 2,
                       k.8.fies_whlday == "Do not know" ~ 3),
             levels = c(1, 2, 3),
             labels = c("Yes", "No", "Do not know")),
         l.1.children_under_5 = 
           factor(
             case_when(l.1.children_under_5 == "No" ~ 0,
                       l.1.children_under_5 == "Yes" ~ 1),
             levels = c(0, 1),
             labels = c("Yes", "No")),
         m.1.assistance_NGO = 
           factor(
             case_when(m.1.assistance_NGO == "No" ~ 0,
                       m.1.assistance_NGO == "Yes" ~ 1),
             levels = c(0, 1),
             labels = c("Yes", "No")),
         m.2.assistance_social = 
           factor(
             case_when(m.2.assistance_social == "No" ~ 0,
                       m.2.assistance_social == "Yes" ~ 1),
             levels = c(0, 1),
             labels = c("Yes", "No"))
  )

# Consistent data
dat.3 <- dat.2 %>% 
  mutate(l.1.children_under_5 = ifelse(b.1.hh_members == 1, 0, l.1.children_under_5), # if it is a 1 person household, there should be no children under 5
         # total livestock ownership should reflect the individual livestock ownership recorded
         b.7.own_livestock = ifelse(sum(b.7.2.yak_cow_buffalo, b.7.3.goat_sheep, b.7.4.pig, b.7.5.poultry, b.7.6.milking_animal) > 0, 1, 0),
         # migrants recorded in the household should reflect the question of migrants' gender
         e.1.absentee_migrant = ifelse(sum(e.2.num_migrant_male, e.3.num_migrant_female) > 0, 1, 0),
         # households without livestock or land cannot sell their own production at the market *I have reserversation about this... to discuss*
        # d.5.sell_own_product_at_mkt = ifelse(b.6.own_land == 0 & b.7.own_livestock == 0, 0, d.5.sell_own_product_at_mkt),#
         # if there are no absentee migrants for past 6 months, there cannot have been remittances within the past 4 months
         e.6.migrant_total_remittance = ifelse(e.1.absentee_migrant == 0, 0, e.6.migrant_total_remittance),
         # if there are no absentee migrants, there can be no remittances (see above)
         c.1.income_remittance = ifelse(e.1.absentee_migrant == 0, 0, c.1.income_remittance),
         # if the household has no dairy producing animals it cannot sell dairy products as its own production 
         d.5.1.sell_at_mkt_dairy = ifelse(b.7.2.yak_cow_buffalo == 0 & b.7.6.milking_animal == 0, 0, d.5.1.sell_at_mkt_dairy))

# further investigation

# display number of households sampled per PSU
dat.hh.per.psu <- dat.2 %>% 
  group_by(psu) %>% 
  summarize(district = unique(a.2.district),
            Households = n())
  

# cv = coefficient of variation, allowing us to compare the variability in the answer within each PSU, across PSUs ('as what proportion of the mean does the data vary on average?'). Anything that is too different from the others *in the same column* should be investigated
# note: answers of "Inf" mean 'infinity', and 'NaN' means 'Not a Number, which means that we divided by 0 at some point--generally indicates that the PSU has only 1 household sampled from it.
dat.by.psu <- dat.2 %>% 
  group_by(psu) %>% 
  summarize(households_surveyed = n(),
            mkt_time_cv = round(mean(d.1.time_to_mkt_mins)/sd(d.1.time_to_mkt_mins), 3),
            num_traders_cv = round(mean(d.2.num_traders_mkt) / sd(d.2.num_traders_mkt),3))

# visualize how many coefficients of variation are very high
ggplot(dat.by.psu, aes(x = psu, y = mkt_time_cv))+
  geom_point()+
  geom_hline(yintercept = 3)
ggplot(dat.by.psu, aes(x = psu, y = num_traders_cv))+
  geom_point()+
  geom_hline(yintercept = 3)

# Return the PSUs for scores above 3 standard deviations in the coefficient of variation
dat.by.psu <- dat.by.psu[!is.infinite(dat.by.psu$mkt_time_cv),]
dat.by.psu <- dat.by.psu[!is.infinite(dat.by.psu$num_traders_cv),]
dat.by.psu <- dat.by.psu[!is.nan(dat.by.psu$mkt_time_cv),]
dat.by.psu <- dat.by.psu[!is.nan(dat.by.psu$num_traders_cv),]
dat.by.psu.2 <- dat.by.psu %>% mutate(deviation_mkt = ifelse(mkt_time_cv > (3*sd(mkt_time_cv, na.rm = T)), "INVESTIGATE", ""),
                                      deviation_traders = ifelse(num_traders_cv > (3*sd(num_traders_cv, na.rm = T)), "INVESTIGATE", "")
                                      )
  
# write to .csv
write.csv(dat.2, "C:/Users/ravi.maharjan/Desktop/R/cleaned_data.csv")

# write to .sav
write_sav(dat.2, "C:/Users/ravi.maharjan/Desktop/R/cleaned_data.sav")

# the data order of columns needs to be the _exact same_ as the raw data being input. So things like the FCS where we seem to be grouping the 2 questions
# labels are taken automatically from the questionnaire, so data entered in raw uncleaned sheet needs to match those
# put all the stringed numbers as double (in a loop/sapply or in map or something, it's shorter!)         
# all dichotomized variables should be 1,0, that's it.
