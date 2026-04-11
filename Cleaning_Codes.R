library(dplyr) #For general data manipulation
library(stringr) #For string manipulation
library(tidyr) #For pivoting
library(readr) #For read_csv which automatic column renaming is useful for output from meta reviewer
library(flextable) # For Creating Tables


raw_dat_everything <- read_csv("Extract_Codes/Partial_Nesting_Review - ES Level - 04-10-2026 04_17 PM.csv", col_types = cols(.default = col_character())) #Reading in all columns as character


raw_dat <- raw_dat_everything |> 
  filter(created_by == "feagins@wisc.edu", screener_1 == "Victor Feagins") #filters out training set & nick's work

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
  relocate(citation_id, study_id, response_id, starts_with("pub"), everything()) #Reorder to pub variables are first

full_experiment_dat <- full_experiment_dat |> 
  mutate(pub_journal = case_when(study_id == 111568526 ~ "Health Psychology", .default = pub_journal)) |> #Data entry error (Meta reviewer does not update)
  mutate(analytical_partial_cluster= case_when(study_id == 111578810 ~ "Ignored", .default = analytical_partial_cluster)) |>  #Data entry error (Meta reviewer does not update)
  mutate(pub_primary_secondary= case_when(study_id == 111570011 ~ "Based on data that have been previously published", .default = pub_primary_secondary)) |> #Data entry error (Meta reviewer does not update)
  mutate(journal_type = ifelse(toupper(pub_journal) %in% c("JOURNAL OF EDUCATIONAL RESEARCH", 
                               "JOURNAL OF EDUCATIONAL PSYCHOLOGY",
                               "JOURNAL OF RESEARCH ON EDUCATIONAL EFFECTIVENESS"), "Education", "Health")) |> 
  mutate(partial_nest = ifelse(str_detect(dep_types, "Partially Nested Clusters"), "PN", "Non-PN")) |> 
  mutate(partial_nest = ifelse(is.na(dep_types), "Non-PN", partial_nest)) #Experiments with dep_types missing means no dependency present


### Treatment condition data  -----

long_cond <- raw_cond |> 
  pivot_longer(cols = starts_with("cond"),  names_to = c("experiment", ".value"), names_pattern = "(\\d+)_(.*)") |> #Each row is experiment 
  pivot_longer(cols = !citation_id:experiment, names_to = c('.value'), names_pattern = "(.*)\\.\\.\\.", names_prefix = "cond_") |> #each row is treatment condition
  filter(!is.na(name)) |> #removes extra columns added by meta reviewer
  rename_with(~paste0("cond_", .x), .cols = -c(citation_id, study_id, response_id, experiment)) |> #labeling columns 
  mutate(cond_exp_units_count = case_when(study_id == 111569030 ~ "5", .default = cond_exp_units_count))

### Cleaning Partial nesting -------
pn_dat <- full_experiment_dat |> 
  filter(partial_nest == "PN") |> 
  left_join(long_cond, by = join_by(citation_id, study_id, response_id, experiment)) #adding treatment condition data which has number of group settings and treatment providers

pn_dat <- pn_dat |> 
  group_by(response_id, experiment) |> 
  mutate(cond_treatment_groups_exp= ifelse(is.na(cond_treatment_groups_exp), "No", cond_treatment_groups_exp)) |> ####################TEMP LINE OF CODE DUE TO META REVIEWER DATA NOT UPDATING############################################
  mutate(exp_treatment_provider_percent = mean(cond_treatment_provider_exp == "Yes"),
         exp_treatment_group_percent = mean(cond_treatment_groups_exp == "Yes"),
         obs_treatment_provider_percent = mean(cond_treatment_provider_obs == "Yes"), #Should be NA for individually randomized experiments 
         obs_treatment_group_percent = mean(cond_treatment_groups_obs == "Yes")) |> #Should be NA for individually randomized experiments
  ungroup()


# Record Check 
stopifnot(all(!is.na(pn_dat$exp_treatment_provider_percent)),  all(!is.na(pn_dat$exp_treatment_group_percent))) 
#There should not be any missing for these values, If there is then check the record manually to investigate. 

pn_dat <- pn_dat |> 
  mutate(partial_class = case_when(
    exp_level_random == "Individual" & exp_treatment_group_percent < 1 & exp_treatment_group_percent > 0  ~ "Grouped IRD", #No grouping in at least one condition 
    exp_level_random == "Individual" & exp_treatment_provider_percent < 1 & exp_treatment_provider_percent > 0 & exp_treatment_group_percent %in% c(0,1) ~ "Treatment Provider IRD", #no grouping at all but have treatment providers in at least one condition
    exp_level_random == "Individual" & str_detect(partial_type, "Group Setting") & exp_treatment_group_percent  == 1 ~ "MultiGroup IRD", # Different types of grouping or unobserved grouping
    exp_level_random == "Cluster" & exp_treatment_group_percent < 1 & exp_treatment_group_percent > 0 ~ "Grouped CRD", # Experimental units partially grouped in CRD
    exp_level_random == "Cluster" & exp_treatment_provider_percent < 1 & exp_treatment_provider_percent > 0 & exp_treatment_group_percent %in% c(0,1) ~ "Treatment Provider CRD", #Experimental units partially with Treatment provider and no grouping 
    exp_level_random == "Cluster" & obs_treatment_group_percent < 1 & obs_treatment_group_percent > 0 & exp_treatment_group_percent %in% c(0,1) & exp_treatment_provider_percent %in% c(0,1) ~ "Observational Group CRD",  #Observational units partially grouped with no partially grouping or treatment providers in experimental untis  
    exp_level_random == "Cluster" & obs_treatment_provider_percent < 1 & obs_treatment_provider_percent > 0 & obs_treatment_group_percent %in% c(0,1) & exp_treatment_group_percent %in% c(0,1) & exp_treatment_provider_percent %in% c(0,1) ~ "Observational Treatment Provider CRD",
    .default = "Complex-PNRD")) #That that are those that are involve a mixed randomization procedure  or Different types of grouping or unobserved grouping


pn_exp <- pn_dat |> 
  select(study_id, experiment, partial_class, ends_with("percent")) |> 
  distinct()

full_experiment_dat <- full_experiment_dat |> 
  left_join(pn_exp) #adding in pn_type

## Adding Automation Score
screenable <- read.csv("to_be_screened.csv")
experimental_score <- screenable |> 
  select(ID, Score) |> 
  mutate(ID = as.character(ID))

full_experiment_dat <- full_experiment_dat |> 
  left_join(experimental_score, by = join_by(citation_id == ID))

## Saving Data -----

write.csv(full_experiment_dat, file = "Clean_Data/experimental_data.csv", row.names = FALSE)
write.csv(long_cond, file = "Clean_Data/treatment_condition_data.csv",  row.names = FALSE)

## Cleaning Screening Data -----
abstract_screen_raw <- read.csv("Abstract_Screened/Partial_Nesting_Review - Abstract Screening - 04-11-2026 04_30 PM.csv")
full_text_screen_raw <- read.csv("Full_Text_Screen/Partial_Nesting_Review - ES Level - 04-10-2026 04_25 PM.csv")
citation_data_raw <- read.csv("Citation_Data/Partial_Nesting_Review - Citations - 04-11-2026 05_55 PM.csv")
abstract_screen <-abstract_screen_raw |> 
  mutate(Citation.ID = as.character(Citation.ID))

full_text_screen <- full_text_screen_raw |> 
  mutate(study_id = as.character(study_id)) |> 
  filter(created_by == "feagins@wisc.edu") #removes extra rows

citation_data <- citation_data_raw |> 
  mutate(citation_id = as.character(citation_id))


screening_data <- abstract_screen |> 
  mutate(Citation.ID = as.character(Citation.ID)) |> 
  left_join(experimental_score, by = join_by(Citation.ID == ID)) |> 
  left_join(full_text_screen, by = join_by(Citation.ID == study_id)) |> 
  select(Citation.ID, Decision, eligible_study, Score) |> 
  left_join(citation_data, by = join_by(Citation.ID == citation_id)) |> 
  select(Citation.ID, Decision, eligible_study, journal_book_title, Score) |> 
  rename(abstract_screen = Decision, full_text_screen = eligible_study) |> 
  filter(!(is.na(full_text_screen) & abstract_screen == 1)) #removes training papers for Nick


## Saving Data -----
write.csv(screening_data, "Clean_Data/screening_data.csv", row.names = FALSE)