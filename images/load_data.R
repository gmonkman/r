#library(dplyr)
library(XLConnectJars)
library(XLConnect)

dataRoot <- "C:/Users/Graham Monkman/OneDrive/Documents/PHD/Phase 03 - Novel Applications/Machine Vision"
s <- paste(dataRoot, "/fish_image_data_sheet.xlsm", sep = "")
loadMyData <- function() {
    wbData = XLConnect::loadWorkbook(s)
    dfAll <<- XLConnect::readWorksheet(wbData, sheet = "sample_pivot_cleaned", header = TRUE)[1:8]
    #dfPAMFocal<<-dplyr::filter(dfAll, spatial_method=="focal", survey=="pam", y>0|venue>0)
    attach(dfAll) #make available as vectors
    names(dfAll)
    createDifferences()
}

createDifferences <- function() {
    dfAll$diff_bg_sans_correction <<- dfAll$bg_sans_correction - dfAll$actual_mm
    dfAll$diff_bg_with_correction <<- dfAll$bg_with_correction - dfAll$actual_mm
    dfAll$diff_fg_sans_correction <<- dfAll$fg_sans_correction - dfAll$actual_mm
    dfAll$diff_fg_with_correction <<- dfAll$fg_with_correction - dfAll$actual_mm
    dfAll$diff_laser_sans_correction <<- dfAll$laser_sans_correction - dfAll$actual_mm
    dfAll$diff_laser_with_correction <<- dfAll$laser_with_correction - dfAll$actual_mm
}
