library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
file_names <- list.files("System_Review",full.names = TRUE)
scores_files <- str_subset(file_names, "score")
detail_files <- str_subset(file_names, "detail")

scores <- map(scores_files, .f = ~readxl::read_excel(.x)) |> 
  list_rbind()

detail <- map(detail_files, .f = ~readxl::read_excel(.x)) |> 
  list_rbind()

all_results <- scores |> 
  left_join(detail, by = names(detail)[names(detail) %in% names(scores)])

### Cleaning Raw Data -----

articles <- all_results |> 
  filter(!is.na(Authors)) |> 
  group_by(DOI) |> 
  arrange(Score, .by_group = TRUE) |> 
  slice_head(n = 1) |>  #If two articles have the same DOI take the one that has a higher score.
  filter(!str_starts(Title, "Correction")) |> #not interested in corrections
  filter(!str_starts(Title, "Comment")) |> #not intererested in comments or commentary
  filter(!str_starts(Title, "Corrigendum")) |> #not interested in Corrigendum
  ungroup()

all_results |> 
  filter(is.na(Authors)) |> 
  pull(ShortTitle) |> 
  unique()


my_journals <- c("OBESITY (SILVER SPRING, MD.)","OBESITY RESEARCH",
                 "PREVENTIVE MEDICINE",
                 "HEALTH PSYCHOLOGY: OFFICIAL JOURNAL OF THE DIVISION OF HEALTH PSYCHOLOGY, AMERICAN PSYCHOLOGICAL ASSOCIATION",
                 "HEALTH PSYCHOLOGY : OFFICIAL JOURNAL OF THE DIVISION OF HEALTH PSYCHOLOGY, AMERICAN PSYCHOLOGICAL ASSOCIATION",
                 "HEALTH PSYCHOLOGY",
                 "ADDICTIVE BEHAVIORS",
                 "AMERICAN JOURNAL OF PUBLIC HEALTH",
                 "AIDS AND BEHAVIOR",
                 "EVALUATION REVIEW",
                 "JOURNAL OF CONSULTING AND CLINICAL PSYCHOLOGY",
                 "JOURNAL OF EDUCATIONAL RESEARCH","THE JOURNAL OF EDUCATIONAL RESEARCH",
                 "PREVENTION SCIENCE : THE OFFICIAL JOURNAL OF THE SOCIETY FOR PREVENTION RESEARCH", "PREVENTION SCIENCE",
                 "JOURNAL OF EDUCATIONAL PSYCHOLOGY",
                 "JOURNAL OF RESEARCH ON EDUCATIONAL EFFECTIVENESS")

special_issues <- c(
  "ADVANCING ADOLESCENT HEALTH THROUGH THE ADOLESCENT BRAIN COGNITIVE DEVELOPMENT (ABCD) STUDY", #Health Psychology
  "TRANSDIAGNOSTIC APPROACHES TO MENTAL HEALTH", #Journal of Consulting and Clinical Psychology
  "FROM IDEAS TO EFFICACY IN HEALTH PSYCHOLOGY", #Health Psychology
  "DEVELOPING RESILIENCE IN RESPONSE TO STRESS AND TRAUMA", #Health Psychology
  "'BEST PRACTICES' IN PREVENTION AND TREATMENT FOR RACIAL AND ETHNIC MINORITY PEOPLE",#Journal of Consulting and Clinical Psychology
  "VACCINE HESITANCY AND REFUSAL", #Health Psychology
  "THE SCIENCE OF BEHAVIOR CHANGE: IMPLEMENTING THE EXPERIMENTAL MEDICINE APPROACH", #Health Psychology
  "EVIDENCE-BASED TAILORING OF TREATMENT TO PATIENTS, PROVIDERS, AND PROCESSES", #Journal of Consulting and Clinical Psychology
  "THE ROLE OF EMOTIONS AS A MECHANISM OF CHANGE IN MENTAL HEALTH INTERVENTIONS: INTEGRATING APPLIED AND BASIC SCIENCE. PART 2: DAILY LIFE ASSESSMENT OF AFFECT", #Journal of Consulting and Clinical Psychology
  "THE ROLE OF EMOTIONS AS A MECHANISM OF CHANGE IN MENTAL HEALTH INTERVENTIONS: INTEGRATING APPLIED AND BASIC SCIENCE. PART 1: DAILY LIFE ASSESSMENT OF AFFECT", #Journal of Consulting and Clinical Psychology
  "QUALITATIVE STUDIES OF REASONING AND PARTICIPATION", #Journal of Educational Psychology
  "CARDIOVASCULAR BEHAVIORAL MEDICINE", #Health Psychology
  "SPECIAL SERIES: REVERSE TRANSLATION") #Health Psychology)


articles <- articles |> 
  mutate(journal_simple = case_match(
    toupper(Journal),
    c("OBESITY (SILVER SPRING, MD.)","OBESITY RESEARCH") ~ "OBESITY",
    c("HEALTH PSYCHOLOGY: OFFICIAL JOURNAL OF THE DIVISION OF HEALTH PSYCHOLOGY, AMERICAN PSYCHOLOGICAL ASSOCIATION",
      "HEALTH PSYCHOLOGY : OFFICIAL JOURNAL OF THE DIVISION OF HEALTH PSYCHOLOGY, AMERICAN PSYCHOLOGICAL ASSOCIATION",
      "HEALTH PSYCHOLOGY",
      "ADVANCING ADOLESCENT HEALTH THROUGH THE ADOLESCENT BRAIN COGNITIVE DEVELOPMENT (ABCD) STUDY",
      "FROM IDEAS TO EFFICACY IN HEALTH PSYCHOLOGY",
      "DEVELOPING RESILIENCE IN RESPONSE TO STRESS AND TRAUMA",
      "VACCINE HESITANCY AND REFUSAL",
      "THE SCIENCE OF BEHAVIOR CHANGE: IMPLEMENTING THE EXPERIMENTAL MEDICINE APPROACH",
      "CARDIOVASCULAR BEHAVIORAL MEDICINE",
      "SPECIAL SERIES: REVERSE TRANSLATION") ~ "HEALTH PSYCHOLOGY",
    c("TRANSDIAGNOSTIC APPROACHES TO MENTAL HEALTH",
      "'BEST PRACTICES' IN PREVENTION AND TREATMENT FOR RACIAL AND ETHNIC MINORITY PEOPLE",
      "EVIDENCE-BASED TAILORING OF TREATMENT TO PATIENTS, PROVIDERS, AND PROCESSES",
      "THE ROLE OF EMOTIONS AS A MECHANISM OF CHANGE IN MENTAL HEALTH INTERVENTIONS: INTEGRATING APPLIED AND BASIC SCIENCE. PART 2: DAILY LIFE ASSESSMENT OF AFFECT", 
      "THE ROLE OF EMOTIONS AS A MECHANISM OF CHANGE IN MENTAL HEALTH INTERVENTIONS: INTEGRATING APPLIED AND BASIC SCIENCE. PART 1: DAILY LIFE ASSESSMENT OF AFFECT") ~ "JOURNAL OF CONSULTING AND CLINICAL PSYCHOLOGY",
    c("JOURNAL OF EDUCATIONAL RESEARCH","THE JOURNAL OF EDUCATIONAL RESEARCH") ~ "JOURNAL OF EDUCATIONAL RESEARCH",
    c("PREVENTION SCIENCE : THE OFFICIAL JOURNAL OF THE SOCIETY FOR PREVENTION RESEARCH", "PREVENTION SCIENCE") ~  "PREVENTION SCIENCE",
    c("QUALITATIVE STUDIES OF REASONING AND PARTICIPATION") ~ "JOURNAL OF EDUCATIONAL PSYCHOLOGY",
    .default = toupper(Journal)
  ))




articles <- articles |> 
  mutate(journal_simple =case_match(
    ShortTitle,
    c("Anastopoulos (2021)") ~ "JOURNAL OF CONSULTING AND CLINICAL PSYCHOLOGY",
    c("Arnold (2020)")~ "PREVENTION SCIENCE",
    c("Barnes (2020)","Bridgid (2024)", "Lisa (2024)",
      "Paulina (2024)", "Seohyeon (2024)","Young-Suk (2024)",
      "Braithwaite (2021)", "Kim (2021)", "Lillie (2020)") ~ "JOURNAL OF EDUCATIONAL PSYCHOLOGY",
    c("Robert (2020)") ~ "JOURNAL OF RESEARCH ON EDUCATIONAL EFFECTIVENESS",
    c("Morris (2020)") ~ "JOURNAL OF EDUCATIONAL RESEARCH",
    .default = journal_simple
    
  ))

unique(articles$journal_simple) #Looks good



### Separating Pals and My Search ------

articles <- articles |> 
  mutate(Year = as.numeric(Year)) |> 
  mutate(pals = ifelse(Year < 2020, 1, 0))

my_search <- articles |> 
  filter(pals == 0)

pals_papers <- articles |> 
  filter(pals == 1)

to_be_screened <- my_search |> 
  filter(Score > 0)

write.csv(to_be_screened, "to_be_screened.csv", row.names = FALSE)

