library(dplyr) #For general data manipulation
library(stringr) #For string manipulation
library(tidyr) #For pivoting
library(readr) #For read_csv which automatic column renaming is useful for output from meta reviewer

raw_dat <- read_csv("Extract_Codes/Prelim_Data_set_Jan_20_2026.csv", col_types = cols(.default = col_character())) #Reading in all columns as character

## Sorting Columns -----
raw_pub  <- raw_dat |> #Data at the publication level
  select(citation_id,study_id, response_id,starts_with("pub"))

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

full_experiment_dat <- left_join(long_exp, long_dep, by = join_by(citation_id, study_id, response_id, experiment)) |>
  left_join(long_partial,  by = join_by(citation_id, study_id, response_id, experiment)) |>
  left_join(long_analytical,  by = join_by(citation_id, study_id, response_id, experiment))

### Treatment condition data  -----

long_cond <- raw_cond |> 
  pivot_longer(cols = starts_with("cond"),  names_to = c("experiment", ".value"), names_pattern = "(\\d+)_(.*)") |> #Each row is experiment 
  pivot_longer(cols = !citation_id:experiment, names_to = c('.value'), names_pattern = "(.*)\\.\\.\\.") |> #each row 
  filter(!is.na(name))


