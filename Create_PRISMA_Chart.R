library(PRISMA2020)
library(dplyr)
library(stringr)

screening_dat <- read.csv("Clean_Data/screening_data.csv") #Abstract and Full text screening data
all_records_dat <- read.csv("Clean_Data/all_records.csv") #No filtered all records

all_records_dat <- all_records_dat |>
  mutate(
    journal_type = ifelse(
      toupper(journal_simple) %in%
        c(
          "JOURNAL OF EDUCATIONAL RESEARCH",
          "JOURNAL OF EDUCATIONAL PSYCHOLOGY",
          "JOURNAL OF RESEARCH ON EDUCATIONAL EFFECTIVENESS"
        ),
      "Education",
      "Health"
    )
  )

screening_dat <- screening_dat |>
  mutate(
    journal_type = ifelse(
      toupper(journal_book_title) %in%
        c(
          "JOURNAL OF EDUCATIONAL RESEARCH",
          "JOURNAL OF EDUCATIONAL PSYCHOLOGY",
          "JOURNAL OF RESEARCH ON EDUCATIONAL EFFECTIVENESS"
        ),
      "Education",
      "Health"
    )
  )


### Stats ------

all_records_dat |> #Starting Values
  with(table(journal_type))


dup_removed <- all_records_dat |> #Removing Duplicates Values
  group_by(DOI) |>
  arrange(Score, .by_group = TRUE) |>
  slice_head(n = 1)


research_articles <- dup_removed |> #Removing non research articles
  filter(!is.na(Authors)) |>
  filter(!str_starts(Title, "Correction")) |> #not interested in corrections
  filter(!str_starts(Title, "Comment")) |> #not intererested in comments or commentary
  filter(!str_starts(Title, "Corrigendum")) #not interested in Corrigendum


eligible_sampling <- research_articles |>
  filter(Score > 0)

start_value <- all_records_dat |> #Starting Values
  with(table(journal_type))
remove_dupe <- dup_removed |>
  with(table(journal_type))

remove_nonresearch <- research_articles |>
  with(table(journal_type))

remove_nonRCTs <- eligible_sampling |>
  with(table(journal_type))

sampled <- screening_dat |>
  with(table(journal_type))

abstract_eligible_dat <- screening_dat |>
  filter(abstract_screen == 1)
abstract_eligible <- abstract_eligible_dat |>
  with(table(journal_type))

full_text_experimental_dat <- abstract_eligible_dat |>
  filter(eligible_experimental == "Yes")

full_text_experimental <- full_text_experimental_dat |>
  with(table(journal_type))

full_text_experimental_quan_dat <- full_text_experimental_dat |>
  filter(eligible_quantitative == "Yes")

full_text_experimental_quan <- full_text_experimental_quan_dat |>
  with(table(journal_type))

full_text_eligible_dat <- abstract_eligible_dat |>
  filter(full_text_screen == "Eligible")

full_text_eligible <- full_text_eligible_dat |>
  with(table(journal_type))

# start_value - remove_dupe #Number removed dupe

# remove_dupe - remove_nonresearch #Number removed non-research

# remove_nonresearch- remove_nonRCTs #number removed non RCT

# remove_nonRCTs - sampled #number not sampled

# sampled - abstract_eligible #not eligible abstract wise

# abstract_eligible - full_text_experimental #Not Randomized design

# full_text_experimental - full_text_experimental_quan #Not Causal Comparison estimation

# full_text_experimental_quan - full_text_eligible #Other reasons eligible

# full_text_eligible #included in the review

## Loading In PRISMA Data ------

csv_file <- system.file("extdata", "PRISMA.csv", package = "PRISMA2020")
prisma_template <- read.csv(csv_file)

PRISMA_maker <- function(journal_type = "Education", prisma_template) {
  non_academic <- (remove_dupe - remove_nonresearch)[journal_type]
  not_random <- (abstract_eligible - full_text_experimental)[journal_type]
  not_quant <- (full_text_experimental - full_text_experimental_quan)[
    journal_type
  ]
  other <- (full_text_experimental_quan - full_text_eligible)[journal_type]

  full_text_reject <- str_glue(
    "Not randomized study, {not_random}; Not causal comparison, {not_quant}; Other, {other}"
  )

  removed_text <- str_glue("Not research articles (n = {non_academic})")
  my_prisma <- prisma_template |>
    mutate(
      n = case_when(
        description ==
          "Records identified from: Databases" ~ as.character(start_value[
          journal_type
        ]),
        description == "Records identified from: Registers" ~ "",
        description == "Duplicate records" ~ as.character((start_value -
          remove_dupe)[journal_type]),
        description ==
          "Records marked as ineligible by automation tools" ~ as.character((remove_nonresearch -
          remove_nonRCTs)[journal_type]),
        description ==
          "Records removed for other reasons" ~ as.character((remove_nonRCTs -
          sampled)[journal_type]),
        description ==
          "Records screened (databases and registers)" ~ as.character((sampled)[
          journal_type
        ]),
        description ==
          "Records excluded (databases and registers)" ~ as.character((sampled -
          abstract_eligible)[journal_type]),
        description ==
          "Reports sought for retrieval (databases and registers)" ~ as.character((abstract_eligible)[
          journal_type
        ]),
        description ==
          "Reports assessed for eligibility (databases and registers)" ~ as.character((abstract_eligible)[
          journal_type
        ]),
        description ==
          "New studies included in review" ~ as.character(full_text_eligible[
          journal_type
        ]),
        description == "Reports of new included studies" ~ "",
        description ==
          "Reports excluded (databases and registers): [separate reasons and numbers using ; e.g. Reason1, xxx; Reason2, xxx; Reason3, xxx]" ~ full_text_reject,
        .default = n
      ),
      boxtext = case_when(
        description ==
          "Records marked as ineligible by automation tools" ~ str_glue(
          "{removed_text} \\n Ineligible via automation"
        ),
        description ==
          "Records removed for other reasons" ~ "Records not sampled",
        description ==
          "New studies included in review" ~ "Studies included in review",
        description ==
          "Yellow title box; Identification of new studies via databases and registers" ~ "",
        .default = boxtext
      )
    )
}

ed_PRISMA <- PRISMA_maker("Education", prisma_template)
health_PRISMA <- PRISMA_maker("Health", prisma_template)

PRISMA_graph_health <- PRISMA_data(health_PRISMA)

PRISMA_graph_ed <- PRISMA_data(ed_PRISMA)

health_plot <- PRISMA_flowdiagram(
  PRISMA_graph_health,
  interactive = FALSE,
  previous = FALSE,
  other = FALSE,
  title_colour = "white",
  font = "Times",
  fontsize = 12
)

ed_plot <- PRISMA_flowdiagram(
  PRISMA_graph_ed,
  interactive = FALSE,
  previous = FALSE,
  other = FALSE,
  title_colour = "white",
  font = "Times",
  fontsize = 12
)

# PRISMA_save(health_plot, filename = "health_prisma.tiff", overwrite = TRUE)
# PRISMA_save(ed_plot, filename = "ed_prisma.tiff", overwrite = TRUE)
