# Gorilla Mouse Tracking Utils
# 2022, by the Sage (elise.r.d.lesage@gmail.com)

# utils to process mouse tracking data from Gorilla.sc

# libraries
library(mousetrap)
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)

# 1.reading in all the mouse traces
# code adapted from https://support.gorilla.sc/support/walkthrough/rstudio#mousetrackingdatatoplotsinr
# IN: d: the overarching response data (downloaded from Gorilla website)
#     MTpath: the path where the mouse traces live on your machine
#     cols: the columns in the dataframe you need, MUST include Response
#         (e.g. c("Participant.ID", "Trial.Number", "Spreadsheet.Row", "Response"))
#     RawTraces: you have to choose to use the raw or the normalised traces. Default: raw
# OUT: the mousetrap object with information about conditions etc
GorillaMouse.ReadInTraces <- function(d, MTpath, cols, RawTraces=TRUE){
  oldpath <- getwd()
  # get the filenames in the right order from the response file
  d <- d %>% select(all_of(cols)) 
  d$Participant.Private.ID <- as.character(d$Participant.Private.ID)
  d$Spreadsheet.Row <- as.character(d$Spreadsheet.Row)
  d <- d[(d$Response %like% ".xlsx")&(d$Response %like% "task"), ]
  setwd(MTpath)
  cat(sprintf("Reading in mouse traces from %s\n", MTpath))
  cat(sprintf("Starting to import %d traces.",nrow(d)))
  xlsx.df.list<-lapply(as.list(d$Response),read_excel)
  #Combine all these imported excel files into a single data frame. 
  gorilla.traj<-rbindlist(xlsx.df.list, fill=TRUE)
  #limit to only the mousetracking data
  gorilla.traj<-gorilla.traj[gorilla.traj$type=="mouse",]
  
  if (RawTraces==TRUE){
    # to work with the raw traces, drop the normalised ones
    gorilla.traj<-within(gorilla.traj,rm(x_normalised,y_normalised))
  } else if (RawTraces==FALSE){
    # to work with the normalised traces, drop the raw ones
    gorilla.traj<-within(gorilla.traj,rm(x,y))
  } else { 
    warning("RawTraces is not specified.")
    }
  
  # import into a mousetrap object
  MT<-mt_import_long(gorilla.traj, xpos_label="x",ypos_label="y",timestamps_label="time_stamp", add_labels= c("participant_id", "spreadsheet_row"),
                     mt_id_label=c("participant_id", "spreadsheet_row"))
  # add the information from the response file, so that conditions etc are integrated with traces
  MT[['data']]<-MT[['data']] %>% tidyr::separate(col="mt_id", into=c("Participant.Private.ID", "Spreadsheet.Row"), sep="_") %>%
    inner_join(d, by=c("Participant.Private.ID", "Spreadsheet.Row"))
  
  setwd(oldpath)
  return(MT)
}

# 2. Putting trace file names in order of participant and trial (default = alphabetical) 
# extract the trial number from the list of files, and order the list according to
# ID and trial number
## ! Redundant for this purpose if you use the list based on the response file!
# Ordering traces based on ID and Trial. Deriving these from the trace file (.xlsx) name, 
# so edit code if ID and Trial aren't 3rd and 8th item in the mouse trace file name.
# Returns a three column dataframe with the names of the trace files are the first,
# participant ID as the second, and Trial number as third column.
# To use the sorted list, just convert the first row of the dataframe to a list, 
# e.g. as.list(var_name$NameList)
GorillaMouse.FixOrder <- function(NameList, explain=TRUE){
  if (explain) {
    message("INFO: ! GorillaMouse.FixOrder() function redundant for this purpose if you use the list based on the response file!
    ")
  }
  Order <- NameList %>% tibble() %>% tidyr::separate(col=".", sep="-", 
              into=c("TaskID", "Version", "ID", "i1", "i2", "i3", "TaskType", "TrialNum", "ScreenNum")) %>%
              select(ID, TrialNum)
  Unsorted <- tibble(NameList) %>% cbind(Order)
  Sorted_ <- Unsorted[order(Unsorted$TrialNum), ]
  Sorted <- Sorted_[order(Sorted_$ID), ]
  return(Sorted)
}

