library(PRISMA2020)
library(dplyr)
library(stringr)

screening_dat <- read.csv("Clean_Data/screening_data.csv") #Abstract and Full text screening data
all_records_dat <- read.csv("Clean_Data/all_records.csv") #No filtered all records 

all_records_dat <- all_records_dat |> 
  mutate(journal_type = ifelse(toupper(journal_simple) %in% c("JOURNAL OF EDUCATIONAL RESEARCH", 
                               "JOURNAL OF EDUCATIONAL PSYCHOLOGY",
                               "JOURNAL OF RESEARCH ON EDUCATIONAL EFFECTIVENESS"), "Education", "Health"))

screening_dat <- screening_dat |> 
  mutate(journal_type = ifelse(toupper(journal_book_title) %in% c("JOURNAL OF EDUCATIONAL RESEARCH", 
                               "JOURNAL OF EDUCATIONAL PSYCHOLOGY",
                               "JOURNAL OF RESEARCH ON EDUCATIONAL EFFECTIVENESS"), "Education", "Health"))

  
### Stats ------

all_records_dat |>  #Starting Values
  with(table(journal_type))


dup_removed <- all_records_dat |>  #Removing Duplicates Values
  group_by(DOI) |> 
  arrange(Score, .by_group = TRUE) |> 
  slice_head(n= 1) 


research_articles <- dup_removed |>  #Removing non research articles
  filter(!is.na(Authors)) |> 
  filter(!str_starts(Title, "Correction")) |> #not interested in corrections
  filter(!str_starts(Title, "Comment")) |> #not intererested in comments or commentary
  filter(!str_starts(Title, "Corrigendum")) #not interested in Corrigendum


eligible_sampling <- research_articles |> 
  filter(Score > 0)

start_value <- all_records_dat |>  #Starting Values
  with(table(journal_type))
remove_dupe <- dup_removed|> 
  with(table(journal_type))

remove_nonresearch <- research_articles |> 
  with(table(journal_type))

remove_nonRCTs <- eligible_sampling |> 
  with(table(journal_type))

sampled <- screening_dat |> 
  with(table(journal_type))

abstract_eligible_dat  <- screening_dat |> 
  filter(abstract_screen == 1)
abstract_eligible <- abstract_eligible_dat |> 
  with(table(journal_type))

full_text_eligible_dat <- abstract_eligible_dat |> 
  filter(full_text_screen == "Eligible")

full_text_eligible <- full_text_eligible_dat |> 
  with(table(journal_type))

start_value - remove_dupe #Number removed dupe

remove_dupe - remove_nonresearch #Number removed non-research

remove_nonresearch- remove_nonRCTs #number removed non RCT

remove_nonRCTs - sampled #number not sampled 

sampled - abstract_eligible #not eligible abstract wise

abstract_eligible - full_text_eligible #not eligible fill text screen

full_text_eligible #included in the review