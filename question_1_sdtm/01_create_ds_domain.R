
#Setting the paths where to save the final output and log file.

out_dir <- file.path("output", "question_1_sdtm")
log_dir <- file.path(out_dir, "log")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(log_dir, "ds_program.log")
log_con <- file(log_file, open = "wt")

sink(log_con, split = TRUE)
sink(log_con, type = "message")

cat("=====================================\n")
cat("DS DOMAIN PROGRAM LOG\n")
cat("Start time:", Sys.time(), "\n")
cat("=====================================\n\n")

#Importing libraries needed
library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)

#get familiar with SDTM.OAK package
help(package = "sdtm.oak")

#Put all the code into a TRY-CATCH structure so we can see if it ran with no errors or otherwise

program_status <- "HAVE ERRORS"

tryCatch(
  {
   
    #import the controlled terminology
    study_ct <- read.csv("data/metadata/sdtm_ct.csv")
    
    #Extend the Controlled terminology for all unscheduled visits that exists in ds_raw
    
    ct_ext <- tibble::tribble(
      ~codelist_code, ~term_code, ~term_value, ~collected_value,
      ~term_preferred_term, ~term_synonyms,
      
      "VISITNUM", "VISITNUM", "1.1", "Unscheduled 1.1", "", "",
      "VISITNUM", "VISITNUM", "4.1", "Unscheduled 4.1", "", "",
      "VISITNUM", "VISITNUM", "5.1", "Unscheduled 5.1", "", "",
      "VISITNUM", "VISITNUM", "6.1", "Unscheduled 6.1", "", "",
      "VISITNUM", "VISITNUM", "8.2", "Unscheduled 8.2", "", "",
      "VISITNUM", "VISITNUM", "13.1", "Unscheduled 13.1", "", "",
      
      "VISIT", "VISIT", "UNSCHEDULED 1.1",  "Unscheduled 1.1",  "", "",
      "VISIT", "VISIT", "UNSCHEDULED 4.1",  "Unscheduled 4.1",  "", "",
      "VISIT", "VISIT", "UNSCHEDULED 5.1",  "Unscheduled 5.1",  "", "",
      "VISIT", "VISIT", "UNSCHEDULED 6.1",  "Unscheduled 6.1",  "", "",
      "VISIT", "VISIT", "UNSCHEDULED 8.2",  "Unscheduled 8.2",  "", "",
      "VISIT", "VISIT", "UNSCHEDULED 13.1", "Unscheduled 13.1", "", ""
    )
    
    #Merge the new records into the initial code list
    study_ct_ext <- bind_rows(study_ct, ct_ext)
    
    ds_raw <- pharmaverseraw::ds_raw
    dm <- pharmaversesdtm::dm
    
    ds_raw <- ds_raw %>%
      generate_oak_id_vars(pat_var = "PATNUM", raw_src = "ds_raw") %>%
      mutate(
        #Corrected value from raw dataset to match CT
        INSTANCE = if_else(INSTANCE == "Ambul Ecg Removal", "Ambul ECG Removal", INSTANCE),
        #Derived DSDECOD, DSTERM, DSCAT
        DSDECOD  = if_else(!is.na(OTHERSP), OTHERSP, IT.DSDECOD), 
        DSTERM   = if_else(!is.na(OTHERSP), OTHERSP, IT.DSTERM),
        DSCAT    = if_else(DSDECOD == "Randomized", "PROTOCOL MILESTONE", "DISPOSITION EVENT"),
      ) 
    
    ds <-  
      #Assign DSDECOD
      assign_no_ct(
        raw_dat = ds_raw,
        raw_var = "DSDECOD",
        tgt_var = "DSDECOD",
        id_vars = oak_id_vars()
      ) %>%
      
      #Assign DSTERM 
      assign_no_ct(
        raw_dat = ds_raw,
        raw_var = "DSTERM",
        tgt_var = "DSTERM",
        id_vars = oak_id_vars()
      ) %>%
      
      #Assign DSCAT
      assign_no_ct(
        raw_dat = ds_raw,
        raw_var = "DSCAT",
        tgt_var = "DSCAT",
        id_vars = oak_id_vars()
      ) %>%
      
      #Assign VISIT
      assign_ct(
        raw_dat = ds_raw,
        raw_var = "INSTANCE",
        tgt_var = "VISIT",
        ct_spec = study_ct_ext,
        ct_clst = "VISIT",
        id_vars = oak_id_vars()
      ) %>%
      
      #Assign VISITNUM
      assign_ct(
        raw_dat = ds_raw,
        raw_var = "INSTANCE",
        tgt_var = "VISITNUM",
        ct_spec = study_ct_ext,
        ct_clst = "VISITNUM",
        id_vars = oak_id_vars()
      ) %>%
      
      #Assign DSDTC
      assign_datetime(
        raw_dat = ds_raw,
        raw_var = c("DSDTCOL", "DSTMCOL"),
        tgt_var = "DSDTC",
        raw_fmt = c("m-d-y", "H:M"),
        raw_unk = c("UN", "UNK"),
        id_vars = oak_id_vars()
      ) %>%
      
      #Assign DSSTDTC
      assign_datetime(
        raw_dat = ds_raw,
        raw_var = "IT.DSSTDAT",
        tgt_var = "DSSTDTC",
        raw_fmt = "m-d-y",
        raw_unk = "UN",
        id_vars = oak_id_vars()
      ) 
    
    final_DS <- ds %>%
      dplyr::mutate(
        STUDYID = ds_raw$STUDY,
        DOMAIN = "DS",
        USUBJID = paste0("01-", ds_raw$PATNUM),
        DSTERM = ds_raw$DSTERM,
        DSDECOD = ds$DSDECOD,
        DSCAT = ds$DSCAT,
        VISITNUM = as.numeric(ds$VISITNUM),
        VISIT = ds$VISIT,
        DSDTC = ds$DSDTC,
        DSSTDTC = ds$DSSTDTC,
      ) %>%
      derive_seq(
        tgt_var = "DSSEQ",
        rec_vars = c("USUBJID","VISITNUM", "DSTERM")
      ) %>%
      derive_study_day(
        sdtm_in = .,
        dm_domain = dm,
        tgdt = "DSSTDTC",
        refdt = "RFSTDTC",
        study_day_var = "DSSTDY"
      ) %>%
      select(
        "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM",
        "DSDECOD", "DSCAT", "VISITNUM", "VISIT",
        "DSDTC","DSSTDTC", "DSSTDY"
      )
    
    #Write the output file to a csv 
    write.csv(
      final_DS,
      file = file.path("output", "question_1_sdtm", "DS.csv"),
      row.names = FALSE,
      na = ""
    )
    
    cat("Records in DS:", nrow(ds), "\n")
    
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
