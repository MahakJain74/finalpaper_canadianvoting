#### Preamble ####
# Purpose: Clean the survey data downloaded from General Social Survey: Social Identity: 2013
# Author: Mahak Jain
# Data: 3 January 2021
# Contact: mahak.jain@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the GSS data and saved it to inputs/data
# - Don't forget to gitignore it!



#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)
library(janitor)



raw_data <- read.csv("inputs/data/raw_data.csv")
dict <- read_lines("inputs/data/codebook.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("inputs/data/Stata.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))

# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


# Adding selected variables 

gss <- raw_data %>% 
  select(recid, 
         agegr10,
         sex,
         marstat,
         prcode,
         ium_10, 
         iuy_01, 
         icr_10,
         socnet,
         icr_30,
         scp_115,
         ism_10,
         ism_30,
         cwr_30, 
         vbr_10,
         vbr_15,
         vbr_20,
         vbr_25,
         vbr_30,
         vbr_35,
         vbr_40,
         vbr_45,
         rep_05,
         srh_110,
         srh_115,
         incm) %>% 
  mutate_at(vars(agegr10:incm), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(agegr10:incm),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Fixing the names
gss <- gss %>% 
  clean_names() %>% 
  rename(id = recid,
         age_grp = agegr10,
         sex = sex,
         mar_stat = marstat,
         prov = prcode,
         int_use_pastm = ium_10, 
         int_use_pasty = iuy_01, 
         int_use_s_pasty = icr_10,
         soc_net = socnet,
         soc_use_freq = icr_30,
         met_new_int = scp_115, 
         elecbank_intuse = ism_10,
         buygs_int = ism_30,
         contact_relatives_int = cwr_30, 
         voted = vbr_10,
         vote_elig = vbr_15,
         no_vote_reason = vbr_20,
         vote_next = vbr_25,
         prov_vote = vbr_30,
         prov_vote_elig = vbr_35,
         mun_vote = vbr_40,
         mun_vote_elig = vbr_45,
         pol_interest = rep_05,
         self_gen_health = srh_110,
         self_mental_health = srh_115,
         annual_inc = incm)


#### Clean up ####

#Cleaning Age Group
gss <- gss %>% 
  mutate(age_grp = case_when(
    age_grp=="15 to 24 years" ~ "15-24",
    age_grp=="25 to 34 years" ~ "25-34",
    age_grp=="35 to 44 years" ~ "35-44",
    age_grp=="45 to 54 years" ~ "45-54",
    age_grp=="55 to 64 years" ~ "55-64",
    age_grp=="65 to 74 years" ~ "65-74",
    age_grp=="75 years and over" ~ "75-100"
  )) 


## Making a regression dataset

#loading variables in the dataset
reg_data <- gss[, c("voted", "int_use_pastm", "soc_net", "soc_use_freq", "int_use_s_pasty", "elecbank_intuse","buygs_int" )]
#Making it numeric binary
reg_data<- within(reg_data, voted[voted == 'Yes'] <- 1)
reg_data<- within(reg_data, voted[voted == 'No'] <- 0)
reg_data<- within(reg_data, soc_net[soc_net == 'Yes'] <- 1)
reg_data<- within(reg_data, soc_net[soc_net == 'No'] <- 0)
reg_data<- within(reg_data, int_use_pastm[int_use_pastm == 'Yes'] <- 1)
reg_data<- within(reg_data, int_use_pastm[int_use_pastm == 'No'] <- 0)
reg_data<- within(reg_data, int_use_s_pasty[int_use_s_pasty == 'Yes'] <- 1)
reg_data<- within(reg_data, int_use_s_pasty[int_use_s_pasty == 'No'] <- 0)
reg_data<- within(reg_data, buygs_int[buygs_int == 'At least once a day'] <- 1)
reg_data<- within(reg_data, buygs_int[buygs_int == 'At least once a month (but not every week)'] <- 1)
reg_data<- within(reg_data, buygs_int[buygs_int == 'At least once a week (but not every day)'] <- 1)
reg_data<- within(reg_data, buygs_int[buygs_int == 'Never'] <- 0)
reg_data<- within(reg_data, buygs_int[buygs_int == 'Not in the past month'] <- 0)
reg_data<- within(reg_data, elecbank_intuse[elecbank_intuse == 'At least once a day'] <- 1)
reg_data<- within(reg_data, elecbank_intuse[elecbank_intuse == 'At least once a month (but not every week)'] <- 1)
reg_data<- within(reg_data, elecbank_intuse[elecbank_intuse == 'At least once a week (but not every day)'] <- 1)
reg_data<- within(reg_data, elecbank_intuse[elecbank_intuse == 'Never'] <- 0)
reg_data<- within(reg_data, elecbank_intuse[elecbank_intuse == 'Not in the past month'] <- 0)

#Removing values != 0 or 1
reg_data = subset(reg_data, voted!= "Not stated" & voted!= "Don't know" & voted!= "Valid skip" & voted!= "Refusal" & int_use_pastm!= "Valid skip" & int_use_pastm!= "Refusal" & int_use_pastm!= "Don't know" & soc_net!= "Refusal" &  soc_net!= "Valid skip" & soc_net!= "Don't know" & soc_net!= "Not stated" & int_use_s_pasty!= "Valid skip" & int_use_s_pasty!= "Refusal" & int_use_s_pasty!= "Don't know" & buygs_int!= "Valid skip" & buygs_int!= "Refusal" & buygs_int!= "Don't know" & elecbank_intuse!= "Valid skip" & elecbank_intuse!= "Refusal" & elecbank_intuse!= "Don't know")



#In the end, converting gss into csv

view(reg_data)
view(gss)
write_csv(gss, "inputs/data/cleaned_gss.csv")

write_csv(reg_data, "inputs/data/regression_data_gss.csv")
    