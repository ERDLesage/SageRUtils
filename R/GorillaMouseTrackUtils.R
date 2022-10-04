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
# OUT: a list that contains (1) the raw read-in trajectories, and (2) the response data
GorillaMouse.ReadInTraces <- function(d, MTpath, cols){
  oldpath <- getwd()
  # get the filenames in the right order from the response file
  d <- d %>% select(all_of(cols)) 
  d$Participant.Private.ID <- as.character(d$Participant.Private.ID)
  d$Spreadsheet.Row <- as.character(d$Spreadsheet.Row)
  d <- d[(d$Response %like% ".xlsx")&(d$Response %like% "task"), ]
  setwd(MTpath)
  cat(sprintf("Reading in mouse traces from %s\n", MTpath))
  cat(sprintf("Starting to import %d traces.\n",nrow(d)))
  xlsx.df.list<-lapply(as.list(d$Response),read_excel)
  #Combine all these imported excel files into a single data frame. 
  gorilla.traj<-rbindlist(xlsx.df.list, fill=TRUE)
  r<- list("dataset" = d, "trajectories" = gorilla.traj)
  setwd(oldpath)
  return(r)
}

# This function takes the raw trace data and imports it into a mousetrap object.
# code adapted from https://support.gorilla.sc/support/walkthrough/rstudio#mousetrackingdatatoplotsinr
# IN: L: output from GorillaMouse.ReadInTraces():
#       a list that contains (1) the raw read-in trajectories, and (2) the response data
#     RawTraces: you have to choose to use the raw or the normalised traces. Default: raw
# OUT: the mousetrap object with information about conditions, performance etc.
GorillaMouse.Import <- function(L, RawTraces=TRUE){
  d <- L[[1]]
  gorilla.traj <- L[[2]]
  #limit to only the mousetracking data
  gorilla.traj<-gorilla.traj[gorilla.traj$type=="mouse",]
  if (RawTraces==TRUE){
    # to work with the raw traces, drop the normalised ones
    cat("Working with raw traces.\n")
    gorilla.traj<-within(gorilla.traj,rm(x_normalised,y_normalised))
    # import into a mousetrap object
    MT<-mt_import_long(gorilla.traj, xpos_label="x",ypos_label="y",timestamps_label="time_stamp", add_labels= c("participant_id", "spreadsheet_row"),
                       mt_id_label=c("participant_id", "spreadsheet_row"))
  } else if (RawTraces==FALSE){
    # to work with the normalised traces, drop the raw ones
    cat("Working with normalised traces.\n")
    gorilla.traj<-within(gorilla.traj,rm(x,y))
    # import into a mousetrap object
    MT<-mt_import_long(gorilla.traj, xpos_label="x_normalised",ypos_label="y_normalised",timestamps_label="time_stamp", add_labels= c("participant_id", "spreadsheet_row"),
                       mt_id_label=c("participant_id", "spreadsheet_row"))
  } else { 
    warning("RawTraces is not specified.\nSet FALSE for normalised traces, TRUE for raw traces.")
  }

  
  # add the information from the response file, so that conditions etc are integrated with traces
  MT[['data']]<-MT[['data']] %>% tidyr::separate(col="mt_id", into=c("Participant.Private.ID", "Spreadsheet.Row"), sep="_", remove = FALSE) %>%
    inner_join(d, by=c("Participant.Private.ID", "Spreadsheet.Row"))
  # make sure the rownames correspond to mt_id; otherwise plotting won't work!
  rownames(MT[['data']]) <- MT[['data']]$mt_id
  return(MT)
}

# making rectangles to use in plots
# code adapted from https://support.gorilla.sc/support/walkthrough/rstudio#mousetrackingdatatoplotsinr
# IN: traj: the raw read-in trajectories (output from GorillaMouse.ReadInTraces(), 2nd element in the list)
#     ZoneNames: the names of the zones (start, destination) from the traj file
# OUT: rectangles (the zones) that can be used for plotting later.
GorillaMouse.MakeRectangles <- function(traj, ZoneNames, RawTraces=TRUE){
  matrix.data<-filter(traj, zone_name %in% ZoneNames)
  if (RawTraces==TRUE){
    matrix.data<-matrix.data[1:length(ZoneNames),c("zone_x","zone_y","zone_width","zone_height")]
  } else if (RawTraces==FALSE){
    matrix.data<-matrix.data[1:length(ZoneNames),c("zone_x_normalised","zone_y_normalised","zone_width_normalised","zone_height_normalised")]
  } else { 
    warning("RawTraces is not specified.\nSet FALSE for normalised traces, TRUE for raw traces.")
  }
  rectangles<-as.matrix(sapply(matrix.data, as.numeric)) 
  rectangles<-unname(rectangles)
  return(rectangles)
}

# Putting trace file names in order of participant and trial (default = alphabetical) 
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

# GorillaMouse.Wrap(MTraw){
#   # import into mousetrap object (RawTraces to set raw (default) or normalised traces)
#   MT_ <- GorillaMouse.Import(MTraw, RawTraces=FALSE)
#   # map so that 
#   #MT <- mt_remap_symmetric(MT_, dimensions = c("xpos", "ypos"), remap_xpos="left", remap_ypos="no")
#   MT <- mt_align(MT_, use="trajectories", dimensions = c("xpos", "ypos"),coordinates = "norm")
#   
#   # calculate some measures and add to the mousetrap data object
#   MT<-mt_measures(MT, use = "trajectories",save_as="measures",
#                   dimensions = c("xpos", "ypos"), timestamps = "timestamps",
#                   verbose = FALSE)
#   
# }
# 
