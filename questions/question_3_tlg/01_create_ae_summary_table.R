#Setting the paths where to save the final output and log file.

out_dir <- file.path("output", "question_3_tlg")
log_dir <- file.path(out_dir, "log")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(log_dir, "TEAE_table.log")
log_con <- file(log_file, open = "wt")

sink(log_con, split = TRUE)
sink(log_con, type = "message")

cat("=====================================\n")
cat("TEAE Table PROGRAM LOG\n")
cat("Start time:", Sys.time(), "\n")
cat("=====================================\n\n")




library(pharmaverseadam)
library(gt)
library(dplyr)
library(gtsummary)
#Libraries used to save table as DOCX
library(flextable)
library(officer)

#Put all the code into a TRY-CATCH structure so we can see if it ran with no errors or otherwise

program_status <- "HAVE ERRORS"

tryCatch(
  {

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
  modify_header(all_stat_cols() ~ "**{level}**\nN = {n}")|> # Big Ns to column headers
  sort_hierarchical(method = "descending") # sort by descending frequency

te_table

#Saving the table to DOCX

ft <- ft |>
  fontsize(size = 8, part = "all") |>
  padding(padding = 2, part = "all") |>
  autofit() |>
  set_table_properties(layout = "autofit", width = 1) # width=1 => 100% of available width

# Landscape + narrower margins so it doesn't crop
sec <- prop_section(
  page_size = page_size(orient = "landscape"),
  page_margins = page_mar(left = 0.5, right = 0.5, top = 0.5, bottom = 0.5)
)


#Add title to table

doc <- read_docx() |>
  body_add_par(
    "Table: Treatment-Emergent Adverse Events by Treatment Group",
    style = "heading 1"
  ) |>
  body_add_flextable(ft) 

print(
  doc,
  target = "output/question_3_tlg/teae_table.docx"
)

program_status <- "NO ERRORS"
  },
error = function(e) {
  cat("\n*** ERROR DETECTED ***\n")
  cat(conditionMessage(e), "\n")
  cat("*** END ERROR ***\n")
}
)

cat("\n=====================================\n")
cat("PROGRAM STATUS:", program_status, "\n")
cat("End time:", Sys.time(), "\n")
cat("=====================================\n")

sink(type = "message")
sink()
close(log_con)
