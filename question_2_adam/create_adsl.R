#Setting the paths where to save the final output and log file.

out_dir <- file.path("output", "question_2_adam")
log_dir <- file.path(out_dir, "log")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(log_dir, "adsl_program.log")
log_con <- file(log_file, open = "wt")

sink(log_con, split = TRUE)
sink(log_con, type = "message")

cat("=====================================\n")
cat("ADSL PROGRAM LOG\n")
cat("Start time:", Sys.time(), "\n")
cat("=====================================\n\n")




library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)

dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)


#Put all the code into a TRY-CATCH structure so we can see if it ran with no errors or otherwise

program_status <- "HAVE ERRORS"

tryCatch(
  {

adsl <- dm %>%
  select(-DOMAIN)%>%
  #Derive Treatment
  mutate(TRT01P = ARM, TRT01A = ACTARM) %>%
  mutate (
    #Derive Age groups
   AGEGR9N = case_when(
     AGE <18 ~ 1,
     AGE >= 18 & AGE <= 50 ~ 2,
     AGE >50 ~ 3
   ),
   AGEGR9 = case_when(
     AGEGR9N == 1 ~ "<18",
     AGEGR9N == 2 ~ "18-50",
     AGEGR9N == 3 ~ ">50",
     
   )
 )
#TRTSDTM and TRTSTMF 
#Get the first exposure record from EX 
#Get the last exposure from EX - will be used when deriving LSTAVLDT

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    time_imputation = "00:00:00",
    highest_imputation = "h",
    flag_imputation = "time",
    ignore_seconds_flag = TRUE #If only seconds are missing then do not populate the imputation flag TRTSTMF
    
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

#Merge into ADSL  
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),  
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
    
  )%>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |(EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )
 
adsl <-adsl %>%
  derive_vars_dtm_to_dt(
    source_vars = exprs(TRTSDTM, TRTEDTM)
    )%>%
  #Derive ITTFL 
  mutate(
    ITTFL = if_else(!is.na(ARM), "Y", "N")
  )

# LSTAVLDT : Last known alive date using any vital signs visit date, any adverse event start date, any disposition record and any exposure record.
adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      # Vitals: last complete VSDTC with a valid test result
      event(
        dataset_name = "vs",
        order = exprs(VSDTC, VSSEQ),
        condition =
          !(is.na(VSSTRESN) & is.na(VSSTRESC)) &
          !is.na(convert_dtc_to_dt(VSDTC, highest_imputation = "n")),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(VSDTC, highest_imputation = "n"),
          seq = VSSEQ
        )
      ),
      
      #  AE: last complete onset date
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        condition = !is.na(convert_dtc_to_dt(AESTDTC, highest_imputation = "n")),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "n"),
          seq = AESEQ
        )
      ),
      
      # DS: last complete disposition date
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(convert_dtc_to_dt(DSSTDTC, highest_imputation = "n")),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "n"),
          seq = DSSEQ
        )
      ),
      
      # Treatment: datepart of ADSL.TRTEDTM
      event(
        dataset_name = "adsl",
        condition = !is.na(TRTEDTM),
        set_values_to = exprs(
          LSTAVLDT = as.Date(TRTEDTM),
          seq = 0
        )
      )
    ),
    source_datasets = list(vs = vs, ae = ae, ds = ds, adsl = adsl),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTAVLDT, seq, event_nr),
    mode = "last",
    new_vars = exprs(LSTAVLDT)
  )

#Write the output file to a csv 
write.csv(
  adsl,
  file = file.path("output", "question_2_adam", "ADSL.csv"),
  row.names = FALSE,
  na = ""
)

cat("Records in ADSL:", nrow(adsl), "\n")

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
