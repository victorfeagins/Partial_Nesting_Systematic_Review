library(dplyr) #For general data manipulation
library(stringr) #For string manipulation
library(tidyr) #For pivoting
library(readr) #For read_csv which automatic column renaming is useful for output from meta reviewer

raw_dat <- read_csv("Extract_Codes/Partial_Nesting_Review - ES Level - 01-22-2026 10_42 AM.csv", col_types = cols(.default = col_character())) #Reading in all columns as character

## Sorting Columns -----
raw_pub  <- raw_dat |> #Data at the publication level
  select(citation_id,study_id, response_id,starts_with("pub_"))

raw_exp <- raw_dat |>  #Experiment Data at the experimental level for each experiment
  select(citation_id,study_id, response_id, starts_with("exp"))

raw_cond <- raw_dat |> #treatment condition at the treatment level for each experiment
  select(citation_id,study_id, response_id, starts_with("cond"))

raw_dep <- raw_dat |> #Dependency Data at the experimental level for each experiment
  select(citation_id,study_id, response_id, starts_with("dep"))

raw_partial <- raw_dat |> #Partial Nested Data at the experimental level for each experiment
  select(citation_id,study_id, response_id, starts_with("partial"))

raw_analytical <- raw_dat |> #Analysis Data at the experimental level for each experiment
  select(citation_id,study_id, response_id, starts_with("analytical"))


## Pivoting -----
### Data at the Experiment Level -----
#### Experimental Data -----
long_exp <- raw_exp |> 
  pivot_longer(cols = starts_with("exp"), names_to = c("experiment", ".value"), names_pattern = "(\\d+)_(.*)") |> 
  rename_with(~ paste0("exp_", .x), .cols = !c(citation_id,study_id, response_id, experiment)) |> #relabel columns to unique id columns
  filter(!is.na(exp_pop)) #Keeps rows with reported experiments (not every study had multiple experiments)

#### Dependency Data -----

long_dep <- raw_dep |> 
  pivot_longer(cols = starts_with("dep"), names_to = c("experiment", ".value"), names_pattern = "(\\d+)_(.*)") |> 
  rename_with(~ paste0("dep_", .x), .cols = !c(citation_id,study_id, response_id, experiment)) |> #relabel columns to unique id columns
  filter(!is.na(dep_present))#Keeps rows with reported experiments (not every study had multiple experiments)

#### Analytical Data -----
long_analytical <- raw_analytical |> 
  pivot_longer(cols = starts_with("analytical"), names_to = c("experiment", ".value"), names_pattern = "(\\d+)_(.*)") |> 
  rename_with(~ paste0("analytical_", .x), .cols = !c(citation_id,study_id, response_id, experiment)) |> #relabel columns to unique id columns
  filter(!is.na(analytical_outcome))#Keeps rows with reported experiments (not every study had multiple experiments)

#### Partial Nesting Data -----

long_partial <- raw_partial |> 
  pivot_longer(cols = starts_with("partial"), names_to = c("experiment", ".value"), names_pattern = "(\\d+)_(.*)") |> 
  rename_with(~ paste0("partial_", .x), .cols = !c(citation_id,study_id, response_id, experiment)) |> #relabel columns to unique id columns
  filter(!is.na(partial_type))#Keeps rows with reported experiments (not every study had multiple experiments)

#### Joining Experimental Data -----

# Record Check 
stopifnot(nrow(long_exp) == nrow(long_dep) , #Checking to see if row numbers line up. If not there may be a data entry error on the form and should be investigated manually
sum(long_dep$dep_present == "Yes") == nrow(long_analytical),
long_dep |> 
  filter(dep_present == "Yes") |> 
  with(sum(str_detect(dep_types, "Partially Nested Clusters"))) == nrow(long_partial))

# Joining Data 
full_experiment_dat <- left_join(long_exp, long_dep, by = join_by(citation_id, study_id, response_id, experiment)) |>
  left_join(long_partial,  by = join_by(citation_id, study_id, response_id, experiment)) |>
  left_join(long_analytical,  by = join_by(citation_id, study_id, response_id, experiment)) |> 
  left_join(raw_pub, join_by(citation_id, study_id, response_id)) #Adding in publication data 

full_experiment_dat <- full_experiment_dat |> 
  select(citation_id, study_id, response_id, starts_with("pub"), everything()) #Reorder to pub variables are first

### Treatment condition data  -----

long_cond <- raw_cond |> 
  pivot_longer(cols = starts_with("cond"),  names_to = c("experiment", ".value"), names_pattern = "(\\d+)_(.*)") |> #Each row is experiment 
  pivot_longer(cols = !citation_id:experiment, names_to = c('.value'), names_pattern = "(.*)\\.\\.\\.") |> #each row is treatment condition
  filter(!is.na(name))


## Analysis -----

### Partial Nesting Data ----
# 6 Types of partial nested data:
#Individual Randomized experiments with nesting due to group settings (IRGT Trial)
#Individual Randomized experiments with nesting due to treatment provider and not group setting (Facilitator effect trials)

#Cluster randomized experiments with nesting of experimental units due to group setting 
#Cluster randomized experiments with nesting of experimental units due to treatment provider and not group setting

#Cluster randomized experiments with nesting of observational units due to group settings
#Cluster randomized experiments with nesting of observational units due to treatment provider and not group setting


pn_dat <- full_experiment_dat |> 
  filter(str_detect(dep_types, "Partially Nested Clusters")) |> 
  left_join(long_cond, by = join_by(citation_id, study_id, response_id, experiment)) #adding treatment condition data which has number of group settings and treatment providers

pn_dat <- pn_dat |> 
  group_by(response_id, experiment) |> 
  mutate(treatment_groups_exp= ifelse(is.na(treatment_groups_exp), "No", treatment_groups_exp)) |> ####################TEMP LINE OF CODE DUE TO META REVIEWER DATA NOT UPDATING############################################
  mutate(exp_treatment_provider_percent = mean(treatment_provider_exp == "Yes"),
         exp_treatment_group_percent = mean(treatment_groups_exp == "Yes"),
         obs_treatment_provider_percent = mean(treatment_provider_obs == "Yes"), #Should be NA for individually randomized experiments 
         obs_treatment_group_percent = mean(treatment_groups_obs == "Yes")) |> #Should be NA for individually randomized experiments
  ungroup()


# Record Check 
stopifnot(all(!is.na(pn_dat$exp_treatment_provider_percent)),  all(!is.na(pn_dat$exp_treatment_group_percent))) 
#There should not be any missing for these values, If there is then check the record manually to investigate. 

pn_dat <- pn_dat |> 
  mutate(pn_type = case_when(
    exp_level_random == "Individual" & exp_treatment_group_percent < 1 & exp_treatment_group_percent > 0  ~ "Grouped IRD", #No grouping in at least one condition 
    exp_level_random == "Individual" & exp_treatment_provider_percent < 1 & exp_treatment_provider_percent > 0 & exp_treatment_group_percent %in% c(0,1) ~ "Treatment Provider IRD", #no grouping at all but have treatment providers in at least one condition
    exp_level_random == "Individual" & str_detect(partial_type, "Group Setting") & exp_treatment_group_percent  == 1 ~ "MultiGroup IRD", # Different types of grouping or unobserved grouping
    exp_level_random == "Cluster" & exp_treatment_group_percent < 1 & exp_treatment_group_percent > 0 ~ "Grouped CRD", # Experimental units partially grouped in CRD
    exp_level_random == "Cluster" & exp_treatment_provider_percent < 1 & exp_treatment_provider_percent > 0 & exp_treatment_group_percent %in% c(0,1) ~ "Treatment Provider CRD", #Experimental units partially with Treatment provider and no grouping 
    exp_level_random == "Cluster" & obs_treatment_group_percent < 1 & obs_treatment_group_percent > 0 & exp_treatment_group_percent %in% c(0,1) & exp_treatment_provider_percent %in% c(0,1) ~ "Observational Group CRD",  #Observational units partially grouped with no partially grouping or treatment providers in experimental untis  
    exp_level_random == "Cluster" & obs_treatment_provider_percent < 1 & obs_treatment_provider_percent > 0 & obs_treatment_group_percent %in% c(0,1) & exp_treatment_group_percent %in% c(0,1) & exp_treatment_provider_percent %in% c(0,1) ~ "Observational Treatment Provider CRD",
    .default = "Misc")) #Misc are those that are involve a mixed randomization procedure 
n_distinct(pn_dat$response_id)
