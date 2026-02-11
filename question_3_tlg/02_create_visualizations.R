#Setting the paths where to save the final output and log file.

out_dir <- file.path("output", "question_3_tlg")
log_dir <- file.path(out_dir, "log")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(log_dir, "visualizations.log")
log_con <- file(log_file, open = "wt")

sink(log_con, split = TRUE)
sink(log_con, type = "message")

cat("=====================================\n")
cat("VISUALIZATIONS PROGRAM LOG\n")
cat("Start time:", Sys.time(), "\n")
cat("=====================================\n\n")



library(pharmaverseadam)
library(dplyr)
library(ggplot2)
library(scales)
library(binom)



#Put all the code into a TRY-CATCH structure so we can see if it ran with no errors or otherwise

program_status <- "HAVE ERRORS"

tryCatch(
  {
#Prepare Datasets
adsl <- pharmaverseadam::adsl |>
  filter(SAFFL == "Y") 
 
adae <- pharmaverseadam::adae |>
  filter(SAFFL == "Y")

#Plot 1 : AE Severity Distribution by Treatment

#Severity levels grouped by TRT
sev_by_trt <- adae |>
  count(ACTARM, AESEV, name = "n") |>
  group_by(ACTARM) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

plot1 <- ggplot(sev_by_trt, aes(x = ACTARM, y = pct, fill = AESEV)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels =percent_format()) +
  labs(
    title = "AE Severity Distribution by Treatment",
    x = "TreatmentArm",
    y = "Count of AEs",
    fill = "Severity/Intensity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(
  filename = "output/question_3_tlg/plot1_ae_severity_by_treatment.png",
  plot= plot1,
  width = 8,
  height = 6,
  dpi = 300
)


#Plot 2: Top 10 Most Frequent AEs

ae_subjs <- adae |>
  distinct(USUBJID, AETERM)

#Incidence and 95% CI
ae_incidence <- ae_subjs |>
  count(AETERM, name = "n") |>
  rowwise() |>
  mutate(
    ci = list(binom.confint(n, N, method = "exact")),
    pct = 100 * n / N,
    lcl = 100 * ci$lower,
    ucl = 100 * ci$upper
  ) |>
  ungroup() |>
  select(-ci)

#Get the first top 10
top10_ae <- ae_incidence |>
  arrange(desc(n)) |> #descenting n so we get the top 10
  slice_head(n = 10) |>
  mutate(AETERM = factor(AETERM, levels = rev(AETERM)))

#Create GG plot

plot2 <- ggplot(top10_ae, aes(x = pct, y = AETERM)) +
  geom_point(size = 2) +
  geom_errorbarh(
    aes(xmin = lcl, xmax = ucl),
    height = 0.2
  ) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", N, " subjects 95% Clopper–Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme_minimal()

#Save plot
ggsave(
  filename = "output/question_3_tlg/plot2_top10_ae.png",
  plot = plot2,
  width = 8,
  height = 5,
  dpi = 300
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
