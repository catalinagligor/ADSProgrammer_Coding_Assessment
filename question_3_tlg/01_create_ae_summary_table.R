library(pharmaverseadam)
library(gt)

adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

library(dplyr)
library(gtsummary)

adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

# Pre-processing
adae <- adae |>
  filter(
    # safety population
    SAFFL == "Y",
    # treatment emergent adverse events
    TRTEMFL == "Y"
  )

adsl <- adsl |>
  filter(
    # safety population
    SAFFL == "Y",
  )

te_table <- adae |>
  tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = ACTARM,
    id = USUBJID,
    denominator = adsl,
    overall_row = TRUE,

    label = "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs"
  )|>
  add_overall(last = TRUE, col_label = "Overall") |> # add Overall column
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n}")|> # Big Ns to column headers
  sort_hierarchical(method = "descending") # sort by descending frequency

te_table

gtsave(
  as_gt(te_table),
  "output/question_3_tlg/teae_table.pdf",
  expand = 5
)