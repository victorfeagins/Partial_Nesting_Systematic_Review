library(dplyr)


## Reading in Data -----
screenable <- read.csv("Analysis/Clean_Data/to_be_screened.csv")

screenable |>
  group_by(journal_simple) |>
  filter(Score > 0) |>
  summarise(med = quantile(Score, probs = .5), mean = mean(Score), size = n())

## Making sample batches -----
set.seed(123)

random_education <- screenable |>
  select(journal_simple, ID) |>
  filter(
    journal_simple %in%
      c(
        "JOURNAL OF EDUCATIONAL RESEARCH",
        "JOURNAL OF EDUCATIONAL PSYCHOLOGY",
        "JOURNAL OF RESEARCH ON EDUCATIONAL EFFECTIVENESS"
      )
  ) |>
  nest_by(journal_simple) |>
  rowwise() |>
  mutate(sample = list(sample(data$ID))) |>
  mutate(
    batch_1 = list(sample[1:20]), #20 reports per journal
    batch_2 = list(sample[21:40]),
    batch_3 = list(sample[41:60]),
    batch_4 = list(sample[61:80])
  )

random_health <- screenable |>
  select(journal_simple, ID) |>
  filter(
    !(journal_simple %in%
      c(
        "JOURNAL OF EDUCATIONAL RESEARCH",
        "JOURNAL OF EDUCATIONAL PSYCHOLOGY",
        "JOURNAL OF RESEARCH ON EDUCATIONAL EFFECTIVENESS"
      ))
  ) |>
  nest_by(journal_simple) |>
  rowwise() |>
  mutate(sample = list(sample(data$ID))) |>
  mutate(
    batch_1 = list(sample[1:20]), #20 reports per journal
    batch_2 = list(sample[21:40]),
    batch_3 = list(sample[41:60])
  )

batch_1_h <- do.call(c, random_health$batch_1) |>
  na.omit() #na happens when journal does not have enough reports

batch_2_h <- do.call(c, random_health$batch_2) |>
  na.omit()

batch_3_h <- do.call(c, random_health$batch_3) |>
  na.omit()

batch_1_e <- do.call(c, random_education$batch_1) |>
  na.omit() #na happens when journal does not have enough reports

batch_2_e <- do.call(c, random_education$batch_2) |>
  na.omit()

batch_3_e <- do.call(c, random_education$batch_3) |>
  na.omit()

batch_4_e <- do.call(c, random_education$batch_4) |> #Extra batch for training
  na.omit()

## Getting data formatted -----

template <- read.csv("Citation_Template_File.csv")
exportable <- screenable |>
  rename(
    CIT_ID = ID,
    Item_Type = Ref..Type,
    Publication_Year = Year,
    Author_List = Authors,
    Title = Title,
    Publication_Title = journal_simple,
    DOI = DOI,
    Url = Url,
    Abstract = Abstract,
    Pages = Page.s.,
    Issue = Issue,
    Volume = Volume
  )

columns_need_to_add <- names(template)[
  !(names(template) %in% names(exportable))
]

exportable[, columns_need_to_add] <- NA

exportable <- exportable |>
  select(names(template)) #Getting columns in right order


## Getting data exported  -----

batch_1_h_indices <- which(exportable$CIT_ID %in% batch_1_h)
batch_2_h_indices <- which(exportable$CIT_ID %in% batch_2_h) #|> #Random order of indices
#sample()
batch_3_h_indices <- which(exportable$CIT_ID %in% batch_3_h) #|> #Random order of indices
#sample()

batch_1_e_indices <- which(exportable$CIT_ID %in% batch_1_e) #|> #Random order of indices
#sample()
batch_2_e_indices <- which(exportable$CIT_ID %in% batch_2_e) #|> #Random order of indices
#sample()
batch_3_e_indices <- which(exportable$CIT_ID %in% batch_3_e) #|> #Random order of indices
#sample()
batch_4_e_indices <- which(exportable_train$CIT_ID %in% batch_4_e) # Extra batch for training


batch_1_h_dat <- exportable[batch_1_h_indices, ]
batch_2_h_dat <- exportable[batch_2_h_indices, ]
batch_3_h_dat <- exportable[batch_3_h_indices, ]


batch_1_e_dat <- exportable[batch_1_e_indices, ]
batch_2_e_dat <- exportable[batch_2_e_indices, ]
batch_3_e_dat <- exportable[batch_3_e_indices, ]


# write.csv(batch_1_h_dat, file = "batch_1_h_citations_title_abstract_screening.csv", row.names = FALSE)
# write.csv(batch_2_h_dat, file = "batch_2_h_citations_title_abstract_screening.csv", row.names = FALSE)
# write.csv(batch_3_h_dat, file = "batch_3_h_citations_title_abstract_screening.csv", row.names = FALSE)

# write.csv(batch_1_e_dat, file = "batch_1_e_citations_title_abstract_screening.csv", row.names = FALSE)
# write.csv(batch_2_e_dat, file = "batch_2_e_citations_title_abstract_screening.csv", row.names = FALSE)
# write.csv(batch_3_e_dat, file = "batch_3_e_citations_title_abstract_screening.csv", row.names = FALSE)

# Training Sample -----
exportable_train <- screenable |>
  rename(
    CIT_ID = ID,
    Item_Type = Ref..Type,
    Publication_Year = Year,
    Author_List = Authors,
    Title = Title,
    Publication_Title = journal_simple,
    DOI = DOI,
    Url = Url,
    Abstract = Abstract,
    Pages = Page.s.,
    Issue = Issue,
    Volume = Volume
  )

columns_need_to_add <- names(template)[
  !(names(template) %in% names(exportable_train))
]

exportable_train[, columns_need_to_add] <- NA


exportable_train <- exportable_train |>
  select(names(template), Score) #Getting columns in right order

batch_4_e_dat <- exportable_train[batch_4_e_indices, ]

batch_3_h_dat <- exportable_train[batch_3_h_indices, ]


batch_3_h_train_export <- batch_3_h_dat |>
  group_by(Publication_Title) |>
  arrange(desc(Score), .by_group = TRUE) |>
  slice_head(n = 1) |>
  select(-Score)

batch_4_e_train_export <- batch_4_e_dat |>
  group_by(Publication_Title) |>
  arrange(desc(Score), .by_group = TRUE) |>
  slice_head(n = 1) |>
  select(-Score)

write.csv(
  batch_3_h_train_export,
  file = "Batches/training_batch_3_h.csv",
  row.names = FALSE
)

write.csv(
  batch_4_e_train_export,
  file = "Batches/training_batch_4_e.csv",
  row.names = FALSE
)
