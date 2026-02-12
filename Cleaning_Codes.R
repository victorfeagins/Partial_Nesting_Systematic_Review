library(dplyr) #For general data manipulation
library(stringr) #For string manipulation
library(tidyr) #For pivoting
library(readr) #For read_csv which automatic column renaming is useful for output from meta reviewer
library(flextable)


raw_dat_everything <- read_csv("Extract_Codes/Finish_Extract_Codes_2_11_26.csv", col_types = cols(.default = col_character())) #Reading in all columns as character

raw_dat <- raw_dat_everything |> 
  filter(coder_1 == "Victor Feagins", screener_1 == "Victor Feagins") #filters out training set 

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
  pivot_longer(cols = !citation_id:experiment, names_to = c('.value'), names_pattern = "(.*)\\.\\.\\.", names_prefix = "cond_") |> #each row is treatment condition
  filter(!is.na(name)) |> #removes extra columns added by meta reviewer
  rename_with(~paste0("cond_", .x), .cols = -c(citation_id, study_id, response_id, experiment)) #labeling columns 


## Analysis -----
### Basic Descriptive -----
# # of IRDs, # of CRDs, #number of Mix, # number of experiments with PN
# # of articles 
# # number of experiments with number of experimental condtions
# Sample size per treatment condition 

full_experiment_dat <- full_experiment_dat |> 
  mutate(pub_journal = case_when(study_id == 111568526 ~ "Health Psychology", .default = pub_journal)) |> #Data entry error (Meta reviewer does not update)
  mutate(analytical_partial_cluster= case_when(study_id == 111578810 ~ "Ignored", .default = analytical_partial_cluster)) |>  #Data entry error (Meta reviewer does not update)
  mutate(journal_type = ifelse(toupper(pub_journal) %in% c("JOURNAL OF EDUCATIONAL RESEARCH", 
                               "JOURNAL OF EDUCATIONAL PSYCHOLOGY",
                               "JOURNAL OF RESEARCH ON EDUCATIONAL EFFECTIVENESS"), "Education", "Health")) |> 
  mutate(partial_nest = ifelse(str_detect(dep_types, "Partially Nested Clusters"), "PN", "Non-PN")) |> 
  mutate(partial_nest = ifelse(is.na(dep_types), "Non-PN", partial_nest)) #Experiments with dep_types missing means no dependency present

# Number of Articles and randomization type -----



article_journal <- full_experiment_dat |> 
  group_by(journal_type, pub_journal, exp_level_random, partial_nest) |> 
  summarise(count= n()) |> 
  mutate(experiment_type = paste(exp_level_random, partial_nest, sep = "_")) |> 
  pivot_wider(id_cols = c(journal_type, pub_journal), names_from = experiment_type, values_from = count) |> 
  mutate(across(everything(), ~ifelse(is.na(.x), 0, .x)))

marginal_total_journal <- full_experiment_dat |> 
  group_by(journal_type, exp_level_random, partial_nest) |> 
  summarise(count= n()) |> 
  mutate(experiment_type = paste(exp_level_random, partial_nest, sep = "_")) |> 
  pivot_wider(id_cols = c(journal_type), names_from = experiment_type, values_from = count) |> 
  mutate(across(everything(), ~ifelse(is.na(.x), 0, .x))) |> 
  mutate(pub_journal = journal_type)

total_journal <- full_experiment_dat |> 
  group_by(exp_level_random, partial_nest) |> 
  summarise(count= n()) |> 
   mutate(pub_journal = "Total", journal_type = "Total") |> 
  mutate(experiment_type = paste(exp_level_random, partial_nest, sep = "_")) |> 
  pivot_wider(id_cols = c(journal_type, pub_journal),names_from = experiment_type, values_from = count) |> 
  mutate(across(everything(), ~ifelse(is.na(.x), 0, .x)))
 

articles_table_dat <- bind_rows(article_journal, marginal_total_journal, total_journal) |> 
  ungroup() |> 
  mutate(`Mix_Non-PN` = 0) |> 
  rowwise() |> 
  mutate(Cluster_Total = sum(c_across(starts_with("Cluster"))),
         Individual_Total = sum(c_across(starts_with("Individual"))),
         Mix_Total = sum(c_across(starts_with("Mix")))) |> 
  ungroup()

Number_experiments_table <- articles_table_dat |> 
  arrange(journal_type, desc(pub_journal)) |> 
  select(-journal_type, pub_journal, -contains("Non-PN")) |> 
  relocate(pub_journal, starts_with("Cluster"), starts_with("Individual"), starts_with("Mix")) |> 
  rename(Publication = pub_journal) |> 
  flextable() |> 
  separate_header()

Number_experiments_table |> 
  save_as_docx(path = "number_of_experiments.docx")

### Partial Nesting Data ----
# 6 Types of partial nested data:

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
    #exp_level_random == "Individual" & str_detect(partial_type, "Group Setting") & exp_treatment_group_percent  == 1 ~ "MultiGroup IRD", # Different types of grouping or unobserved grouping
    exp_level_random == "Cluster" & exp_treatment_group_percent < 1 & exp_treatment_group_percent > 0 ~ "Grouped CRD", # Experimental units partially grouped in CRD
    exp_level_random == "Cluster" & exp_treatment_provider_percent < 1 & exp_treatment_provider_percent > 0 & exp_treatment_group_percent %in% c(0,1) ~ "Treatment Provider CRD", #Experimental units partially with Treatment provider and no grouping 
    exp_level_random == "Cluster" & obs_treatment_group_percent < 1 & obs_treatment_group_percent > 0 & exp_treatment_group_percent %in% c(0,1) & exp_treatment_provider_percent %in% c(0,1) ~ "Observational Group CRD",  #Observational units partially grouped with no partially grouping or treatment providers in experimental untis  
    exp_level_random == "Cluster" & obs_treatment_provider_percent < 1 & obs_treatment_provider_percent > 0 & obs_treatment_group_percent %in% c(0,1) & exp_treatment_group_percent %in% c(0,1) & exp_treatment_provider_percent %in% c(0,1) ~ "Observational Treatment Provider CRD",
    .default = "Complex-PNRD")) #That that are those that are involve a mixed randomization procedure  or Different types of grouping or unobserved grouping


pn_exp <- pn_dat |> 
  select(study_id, experiment, partial_class) 

full_experiment_dat <- full_experiment_dat |> 
  left_join(pn_exp) #adding in pn_type

## Research question 2 what kind of grouping structures are there

types_grouping <- full_experiment_dat |> 
  filter(partial_nest == "PN") |> 
  group_by(journal_type, partial_class) |> 
  summarise(Total = n(),
            `Not Ignored` = sum(analytical_partial_cluster != "Ignored"))

PN_types_ignore_table <- types_grouping |> 
  pivot_wider(id_cols = c(partial_class),names_from = journal_type, values_from = c(Total, `Not Ignored`),
names_glue = "{journal_type}_{.value}") |> 
  relocate(partial_class, starts_with("Education"), starts_with("Health")) |> 
  rename(`PN Classification` = partial_class) |> 
  flextable() |> 
  separate_header()


## Research question 3 what kinds of cluster assignment

 full_experiment_dat |> 
  filter(partial_nest == "PN") |> 
  group_by(journal_type, partial_class) |> 
  count(partial_assign_type) |> 
  View()



## Resaerch Question 4

## Research Questions 5

ignore_PN_standard  <- full_experiment_dat |> 
  filter(partial_nest == "PN", analytical_partial_cluster == "Ignored") |> 
  group_by(journal_type, partial_class) |> 
  summarise(`Total Ignore` = n(),
            `Mention Standardization` = sum(partial_standardization == "Yes", na.rm = TRUE), #Why is there NA need to fix
            `Mention Diversity` = sum(partial_diversity == "Yes")) |> 
  pivot_wider(id_cols = c(partial_class),names_from = journal_type, values_from = c(`Total Ignore`, `Mention Standardization`, `Mention Diversity`),
names_glue = "{journal_type}_{.value}") |> 
  relocate(partial_class, starts_with("Education"), starts_with("Health")) |> 
  rename(`PN Classification` = partial_class) |> 
  flextable() |> 
  separate_header()

