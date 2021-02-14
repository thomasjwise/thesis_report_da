################################################################################
##### Package Loading #######
################################################################################

## Package Loading 
  library(tidyverse)
  library(readxl)
  library(xlsx)
  library(haven)
  library(lubridate)
  library(gdata)
  library(labelled)
  library(sjlabelled)

################################################################################
##### Data Loading #######   
################################################################################

  # Load Combined Dataset
  dat.comb <- read.csv() ## CSV File Removed
  # Load Self Report Dataset 
  dat.self <- read_sav() ## CSV File Removed
  
################################################################################
##### Convert Datums to Date format  #######
################################################################################
  
## Convert Datums to Dates 
  list.dat.comb <- lapply(dat.comb[c("Datumin", "DatumT2.y", "DatumT3", 
                                     "DatumT3b","DatumT4", "DatumT5")], 
                          as.Date)
  dat.comb$Datumin <- list.dat.comb[["Datumin"]]
  dat.comb$DatumT2.x <- list.dat.comb[["DatumT2.y"]]
  dat.comb$DatumT3 <- list.dat.comb[["DatumT3"]]
  dat.comb$DatumT3b <- list.dat.comb[["DatumT3b"]]
  dat.comb$DatumT4 <- list.dat.comb[["DatumT4"]]
  dat.comb$DatumT5 <- list.dat.comb[["DatumT5"]]
  
## Join Together combined data and self reported data
  dat <- inner_join(x = dat.comb, 
                    y = dat.self,
                    by = c("Patnr", "DatumT3", "TIJD.IN.T3", "DatumT3b", 
                           "TIJD.IN.T3b", "DatumT4", "TIJD.IN.T4b", 
                           "DatumT5", "TIJD.IN.T5"))
  
################################################################################
##### Relabel and Clean Variable Notes #######
################################################################################

## Remove Blank Columns 
  dat <- dat[3:2326]
  
## Extract column names 
  dat.colnames <- as.data.frame(colnames(dat))
  colnames(dat.colnames) <- "core_colnames"
  
## Export Column Names
  write.xlsx(dat.colnames, "datcolnames.xlsx")

  ## Rename Columns in XLSX 
## Import New Column names 
  datcolnames.new <- read_excel("datcolnames.new.xlsx")

  new.colnames <- datcolnames.new$english_colnames
  new.descriptions <- datcolnames.new$`Descriptive Information`
  
  ## Indicate new column names 
  colnames(dat) <- new.colnames
  ## Allocate descriptions 
  attr(dat, 'labels') <- new.descriptions

################################################################################
##### Correct Times  #######
################################################################################
  
## Split off/Copy specific time variables 
  colnames.time <- tidyselect::vars_select(
    names(dat), contains(match = c("Patnr", "dat.in.hos", "T2.dat.2", "T2.TIJD.IN",
                                   "T3.dat", "T3.TIJD.IN", "T3b.dat", "T3b.TIJD.IN",
                                   "T4.dat", "T4.TIJD.IN", "T5.dat", "T5.TIJD.IN")))
  dat.time <- dat[colnames.time]
  
  
## Manual Changing of Incorrect dates 
  dat.time[1,"dat.in.hos"] <- as.Date("2005-09-20")
  dat.time[113,"dat.in.hos"] <- as.Date("2006-05-27")
  dat.time[734,"dat.in.hos"] <- as.Date("2008-05-06")
  dat.time[743,"dat.in.hos"] <- as.Date("2008-05-28")
  
  ## Del T3 
  dat.time[c(415, 455, 544),c("T3.dat", "T3.TIJD.IN")] <- NA
  dat.time[c(489, 504, 536, 698, 721, 794, 831),c("T3.dat", "T3.TIJD.IN")] <- NA
  dat.time[c(1, 14, 16, 57, 74, 168, 212, 226, 227, 230, 241, 243, 295, 314, 315, 322,
             331, 334, 339, 355, 358, 367, 376, 379, 389, 390, 398, 409, 414, 428,
             430, 451, 474, 477, 487, 516, 550, 577, 589, 650, 691, 697, 699,
             725, 753, 811, 817, 826),c("T3.dat", "T3.TIJD.IN")] <- NA
  dat.time[c(47, 369),c("T3.dat", "T3.TIJD.IN")] <- NA
  ## Change T3 
  dat.time[211, "T3.dat"] <- as.Date("2007-1-08")
  dat.time[345, "T3.dat"] <- as.Date("2007-08-22")
  dat.time[429, "T3.dat"] <- as.Date("2007-12-04")
  dat.time[438, "T3.dat"] <- as.Date("2008-02-07")
  dat.time[429, "T3.dat"] <- as.Date("2007-12-04")
  dat.time[439, "T3.dat"] <- as.Date("2008-02-06")
  dat.time[459, "T3.dat"] <- as.Date("2008-04-08")
  dat.time[525, "T3.dat"] <- as.Date("2008-07-17")
  dat.time[758, "T3.dat"] <- as.Date("2008-07-20")
  
  ## Del T3b
  dat.time[c(331, 376, 398, 431, 479, 480, 584, 589, 650, 664, 691, 699, 
             703, 771, 817, 832),c("T3b.dat", "T3b.TIJD.IN")] <- NA
  dat.time[c(392,393),c("T3b.dat", "T3b.TIJD.IN")] <- NA 
  
  ## Del T4
  dat.time[c(16, 168, 242, 263, 272, 309, 315, 355, 398, 474, 479, 480, 545, 589,
             650, 691, 699, 817), c("T4.dat", "T4.TIJD.IN")] <- NA
  
  ## Del T5
  dat.time[c(16, 311, 339, 355, 376, 379, 398, 404, 409, 431, 464, 474, 480, 516,
             521, 650, 681, 691, 731, 753, 771), c("T5.dat", "T5.TIJD.IN")] <- NA
  
  ## Change T5
  dat.time[42, "T5.dat"] <- as.Date("2007-05-14")
  dat.time[70, "T5.dat"] <- as.Date("2007-05-14")
  dat.time[402, "T5.dat"] <- as.Date("2008-12-08")
  dat.time[751, "T5.dat"] <- as.Date("2009-06-05")
  dat.time[758, "T5.dat"] <- as.Date("2009-08-07")
  dat.time[237, "T5.dat"] <- as.Date("2008-01-22")
  dat.time[583, "T5.dat"] <- as.Date("2009-12-02")
  
  ## Remove T3
  capst3.nodate <- c(429, 438, 439, 459, 525, 758, 227, 241, 243, 295, 487)
  ## Remove T3b 
  capst3b.nodate <- c(412, 414)
  ## Remove T4 
  capst4.nodate <- c(17, 182, 354, 403, 518, 680)
  ## Remove T5 
  capst5.nodate <- c(673, 56, 5204, 1049, 1398)
  
  ## Find difference between time points 
    ## Trauma Interval 
  dat.time$`T2trauma.interval` <- difftime(dat.time$T2.dat.2,
                                           dat.time$dat.in.hos,
                                           units = "days")
  
  dat.time$`T3trauma.interval` <- difftime(dat.time$T3.dat,
                                           dat.time$dat.in.hos,
                                           units = "days")
  
  dat.time$`T3btrauma.interval` <- difftime(dat.time$T3b.dat,
                                           dat.time$dat.in.hos,
                                           units = "days")
  
  dat.time$`T4trauma.interval` <- difftime(dat.time$T4.dat,
                                           dat.time$dat.in.hos,
                                           units = "days")
  
  dat.time$`T5trauma.interval` <- difftime(dat.time$T5.dat,
                                           dat.time$dat.in.hos,
                                           units = "days")
  
    ## T2 Interval 
  dat.time$`T2T3.interval` <- difftime(dat.time$T3.dat,
                                       dat.time$T2.dat,
                                       units = "days")
  
  dat.time$`T2T3b.interval` <- difftime(dat.time$T3b.dat,
                                       dat.time$T2.dat,
                                       units = "days")
  
  dat.time$`T2T4.interval` <- difftime(dat.time$T4.dat,
                                        dat.time$T2.dat,
                                        units = "days")
  
  dat.time$`T2T5.interval` <- difftime(dat.time$T5.dat,
                                        dat.time$T2.dat,
                                        units = "days")
  
    ## T3 Interval 
  dat.time$`T3T3b.interval` <- difftime(dat.time$T3b.dat,
                                       dat.time$T3.dat,
                                       units = "days")
  
  dat.time$`T3T4.interval` <- difftime(dat.time$T4.dat,
                                       dat.time$T3.dat,
                                       units = "days")
  
  dat.time$`T3T5.interval` <- difftime(dat.time$T5.dat,
                                      dat.time$T3.dat,
                                      units = "days")
  
    ## T3b Interval 
  dat.time$`T3bT4.interval` <- difftime(dat.time$T4.dat,
                                       dat.time$T3b.dat,
                                       units = "days")
  
  dat.time$`T3bT5.interval` <- difftime(dat.time$T5.dat,
                                       dat.time$T3b.dat,
                                       units = "days")
  
    ## T4 Interval 
  dat.time$`T4T5.interval` <- difftime(dat.time$T5.dat,
                                        dat.time$T4.dat,
                                        units = "days")
  
  ## Replace TIJD.IN with Trauma.interval 
  dat.time$T2.TIJD.IN <- dat.time$T2trauma.interval
  dat.time$T3.TIJD.IN <- dat.time$T3trauma.interval
  dat.time$T3b.TIJD.IN <- dat.time$T3btrauma.interval
  dat.time$T4.TIJD.IN <- dat.time$T4trauma.interval
  dat.time$T5.TIJD.IN <- dat.time$T5trauma.interval
  
  ## Place new dates and intervals into the base dataset 
  dat$T2.dat.2 <- dat.time$T2.dat.2
  dat$T3.dat <- dat.time$T3.dat
  dat$T3b.dat <- dat.time$T3b.dat
  dat$T4.dat <- dat.time$T4.dat
  dat$T5.dat <- dat.time$T5.dat
  
  dat$T2.TIJD.IN <- dat.time$T2.TIJD.IN
  dat$T3.TIJD.IN <- dat.time$T3.TIJD.IN
  dat$T3b.TIJD.IN <- dat.time$T3b.TIJD.IN
  dat$T4.TIJD.IN <- dat.time$T4.TIJD.IN
  dat$T5.TIJD.IN <- dat.time$T5.TIJD.IN
  
################################################################################
##### Generate Dummy Components  #######
################################################################################
    
    
  ## Create Empty Dataframe 
    dat.empty <- as.data.frame(dat$Patnr)
    colnames(dat.empty) <- "Patnr"
    
  ## Copy dat 
    dat.copy <- dat
    ## Add variable columns to make groups consistent 
    new.var <- c("T3.ALCAFH", "T3b.ALCAFH", "T4.ALCAFH", 
                 "T3.ALCMIS", "T3b.ALCMIS", "T4.ALCMIS",
                 "T3.MIDSPEC", "T3b.MIDSPEC", "T4.MIDSPEC",
                 "T3.MIDAFH.LT", "T3b.MIDAFH.LT", "T4.MIDAFH.LT",
                 "T3.MIDAFH.H", "T3b.MIDAFH.H", "T4.MIDAFH.H",
                 "T3.MIDMIS.H", "T3b.MIDMIS.H", "T4.MIDMIS.H",
                 "T3.Hypochondrie", "T3b.Hypochondrie",
                 "T3.Gen.Angstst", "T3b.Gen.Angstst",
                 "T3.Gen.Angstst.medisch", "T3b.Gen.Angstst.medisch",
                 "T3.Gen.Angstst.middelen", "T3b.Gen.Angstst.middelen",
                 "T3.Gen.Angstst.leeftij", "T3b.Gen.Angstst.leeftij",
                 "T3.Gen.Angstst.duur", "T3b.Gen.Angstst.duur",
                 "T3b.Acute.Stress.Stoornis.Mini.Plus",
                 "T4.Acute.Stress.Stoornis.Mini.Plus",
                 "T5.Acute.Stress.Stoornis.Mini.Plus",
                 "T3b.CAPS.C4.alternatief", "T4.CAPS.C4.alternatief", "T5.CAPS.C4.alternatief",
                 "T4.CAPS.Crit.A", "T5.CAPS.Crit.A", 
                 "T4.CAPS.Crit.B", "T5.CAPS.Crit.B",
                 "T4.CAPS.Crit.C", "T5.CAPS.Crit.C",
                 "T4.CAPS.Crit.D", "T5.CAPS.Crit.D", 
                 "T4.CAPS.PTSD.DSM", "T5.CAPS.PTSD.DSM", 
                 "T3b.CAPS.no.B", "T4.CAPS.no.B", "T5.CAPS.no.B",
                 "T3b.CAPS.no.C", "T4.CAPS.no.C", "T5.CAPS.no.C", 
                 "T3b.CAPS.no.D", "T4.CAPS.no.D", "T5.CAPS.no.D",
                 "T3b.CAPS.DSM", "T4.CAPS.DSM", "T5.CAPS.DSM",
                 "T3.substance.current", "T3b.substance.current", "T4.substance.current",
            ## Self Report Measures
                 "T5.TSQ1", "T5.TSQ2", "T5.TSQ3", "T5.TSQ4", "T5.TSQ5", "T5.TSQ6",
                 "T5.TSQ7", "T5.TSQ8", "T5.TSQ9", "T5.TSQ10",
                  "T3b.AUDIT1", "T4.AUDIT1",
                  "T3b.AUDIT2", "T4.AUDIT2",
                  "T3b.AUDIT3", "T4.AUDIT3",
                  "T3b.AUDIT4", "T4.AUDIT4",
                  "T3b.AUDIT5", "T4.AUDIT5",
                  "T3b.AUDIT6", "T4.AUDIT6",
                  "T3b.AUDIT7", "T4.AUDIT7",
                  "T3b.AUDIT8", "T4.AUDIT8",
                  "T3b.AUDIT9", "T4.AUDIT9",
                  "T3b.AUDIT10", "T4.AUDIT10",
                  "T3.SSLD.1", "T3b.SSLD.1", "T4.SSLD.1",
                  "T3.SSLD.2", "T3b.SSLD.2", "T4.SSLD.2",
                  "T3.SSLD.3", "T3b.SSLD.3", "T4.SSLD.3",
                  "T3.SSLD.4", "T3b.SSLD.4", "T4.SSLD.4",
                  "T3.SSLD.5", "T3b.SSLD.5", "T4.SSLD.5",
                  "T3.SSLD.6", "T3b.SSLD.6", "T4.SSLD.6",
                  "T3.SSLD.7", "T3b.SSLD.7", "T4.SSLD.7",
                  "T3.SSLD.8", "T3b.SSLD.8", "T4.SSLD.8",
                  "T3.SSLD.9", "T3b.SSLD.9", "T4.SSLD.9",
                  "T3.SSLD.10", "T3b.SSLD.10", "T4.SSLD.10",
                  "T3.SSLD.11", "T3b.SSLD.11", "T4.SSLD.11",
                  "T3.SSLD.12", "T3b.SSLD.12", "T4.SSLD.12",
                  "T3.SSLD.13", "T3b.SSLD.13", "T4.SSLD.13",
                  "T3.SSLD.14", "T3b.SSLD.14", "T4.SSLD.14",
                  "T3.SSLD.15", "T3b.SSLD.15", "T4.SSLD.15",
                  "T3.SSLD.16", "T3b.SSLD.16", "T4.SSLD.16",
                  "T3.SSLD.17", "T3b.SSLD.17", "T4.SSLD.17",
                  "T3.SSLD.18", "T3b.SSLD.18", "T4.SSLD.18",
                  "T3.SSLD.19", "T3b.SSLD.19", "T4.SSLD.19",
                  "T3.SSLD.20", "T3b.SSLD.20", "T4.SSLD.20",
                  "T3.SSLD.21", "T3b.SSLD.21", "T4.SSLD.21",
                  "T3.SSLD.22", "T3b.SSLD.22", "T4.SSLD.22",
                  "T3.SSLD.23", "T3b.SSLD.23", "T4.SSLD.23",
                  "T3.SSLD.24", "T3b.SSLD.24", "T4.SSLD.24",
                  "T3.SSLD.25", "T3b.SSLD.25", "T4.SSLD.25",
                  "T3.SSLD.26", "T3b.SSLD.26", "T4.SSLD.26",
                  "T3.SSLD.27", "T3b.SSLD.27", "T4.SSLD.27",
                  "T3.SSLD.28", "T3b.SSLD.28", "T4.SSLD.28",
                  "T3.SSLD.29", "T3b.SSLD.29", "T4.SSLD.29",
                  "T3.SSLD.30", "T3b.SSLD.30", "T4.SSLD.30",
                  "T3.SSLD.31", "T3b.SSLD.31", "T4.SSLD.31",
                  "T3.SSLD.32", "T3b.SSLD.32", "T4.SSLD.32",
                  "T3.SSLD.33", "T3b.SSLD.33", "T4.SSLD.33",
                  "T3.SSLD.34", "T3b.SSLD.34", "T4.SSLD.34",
                  "T3.WHO.1", "T3b.WHO.1",
                  "T3.WHO.2", "T3b.WHO.2",
                  "T3.WHO.3", "T3b.WHO.3",
                  "T3.WHO.4", "T3b.WHO.4",
                  "T3.WHO.5", "T3b.WHO.5",
                  "T3.WHO.6", "T3b.WHO.6",
                  "T3.WHO.7", "T3b.WHO.7",
                  "T3.WHO.8", "T3b.WHO.8",
                  "T3.WHO.9", "T3b.WHO.9",
                  "T3.WHO.10", "T3b.WHO.10",
                  "T3.WHO.11", "T3b.WHO.11",
                  "T3.WHO.12", "T3b.WHO.12",
                  "T3.WHO.13", "T3b.WHO.13",
                  "T3.WHO.14", "T3b.WHO.14",
                  "T3.WHO.15", "T3b.WHO.15",
                  "T3.WHO.16", "T3b.WHO.16",
                  "T3.WHO.17", "T3b.WHO.17",
                  "T3.WHO.18", "T3b.WHO.18",
                  "T3.WHO.19", "T3b.WHO.19",
                  "T3.WHO.20", "T3b.WHO.20",
                  "T3.WHO.21", "T3b.WHO.21",
                  "T3.WHO.22", "T3b.WHO.22",
                  "T3.WHO.23", "T3b.WHO.23",
                  "T3.WHO.24", "T3b.WHO.24",
                  "T3.WHO.25", "T3b.WHO.25",
                  "T3.WHO.26", "T3b.WHO.26",
                  "T3.HUI12.1", "T3b.HUI12.1",
                  "T3.HUI12.2", "T3b.HUI12.2",
                  "T3.HUI12.3", "T3b.HUI12.3",
                  "T3.HUI12.4", "T3b.HUI12.4",
                  "T3.HUI12.5", "T3b.HUI12.5",
                  "T3.HUI12.6", "T3b.HUI12.6",
                  "T3.HUI12.7", "T3b.HUI12.7",
                  "T3.HUI12.8", "T3b.HUI12.8",
                  "T3.HUI12.9", "T3b.HUI12.9",
                  "T3.HUI12.10", "T3b.HUI12.10",
                  "T3.HUI12.11", "T3b.HUI12.11",
                  "T3.HUI12.12", "T3b.HUI12.12",
            
                  "T3.HUI15.1", "T3b.HUI15.1",
                  "T3.HUI15.2", "T3b.HUI15.2",
                  "T3.HUI15.3", "T3b.HUI15.3",
                  "T3.HUI15.4", "T3b.HUI15.4",
                  "T3.HUI15.5", "T3b.HUI15.5",
                  "T3.HUI15.6", "T3b.HUI15.6",
                  "T3.HUI15.7", "T3b.HUI15.7",
                  "T3.HUI15.8", "T3b.HUI15.8",
                  "T3.HUI15.9", "T3b.HUI15.9",
                  "T3.HUI15.10", "T3b.HUI15.10",
                  "T3.HUI15.11", "T3b.HUI15.11",
                  "T3.HUI15.12", "T3b.HUI15.12",
                  "T3.HUI15.13", "T3b.HUI15.13",
                  "T3.HUI15.14", "T3b.HUI15.14",
                  "T3.HUI15.15", "T3b.HUI15.15",
            
                  "T3.ha", "T3b.ha",
                  "T3.opgen", "T3b.opgen", 
                  "T3.dagen", "T3b.dagen", "T5.dagen", 
                  "T3.ger.trauma", "T3b.ger.trauma", "T5.ger.trauma", 
                  "T3.opgen.reval.", "T3b.opgen.reval.", "T5.opgen.reval.",
                  "T3.rev.dagen", "T3b.rev.dagen", "T5.rev.dagen",
                  "T3.rev.ger.trauma", "T3b.rev.ger.trauma", "T5.rev.ger.trauma",
                  "T3.schadeverg.", "T3b.schadeverg.",
                  "T3.type.schadeverg", "T3b.type.schadeverg",
                  "T3.reden.nee", "T3b.reden.nee",
                  "T3.uitleg.ja", "T3b.uitleg.ja",
                  "T3.aanvraag", "T3b.aanvraag",
                  "T3.nee.afgerond", "T3b.nee.afgerond",
                  "T3.schadeverg.ontv.", "T3b.schadeverg.ontv.", 
                  "T3.tevr.uitkomst", "T3b.tevr.uitkomst",
                  "T3.rechtv.uitkomst", "T3b.rechtv.uitkomst",
                  "T3.EQ6D.1", "T3b.EQ6D.1", "T5.EQ6D.1", 
                  "T3.EQ6D.2", "T3b.EQ6D.2", "T5.EQ6D.2", 
                  "T3.EQ6D.3", "T3b.EQ6D.3", "T5.EQ6D.3", 
                  "T3.EQ6D.4", "T3b.EQ6D.4", "T5.EQ6D.4", 
                  "T3.EQ6D.5", "T3b.EQ6D.5", "T5.EQ6D.5", 
                  "T3.EQ6D.6", "T3b.EQ6D.6", 
                  "T3.EQ6D.7", "T3b.EQ6D.7",  
                  "T3.EQ6D.8", "T3b.EQ6D.8", 
                  "T3.EQ6D.6.B", "T3b.EQ6D.6.B", "T5.EQ6D.6.B", 
                  "T3.EQ6D.7.B", "T3b.EQ6D.7.B", "T5.EQ6D.7.B", 
                  "T3.EQ6D.8.B", "T3b.EQ6D.8.B", "T5.EQ6D.8.B", 
                  "T3.mo", "T3b.mo", 
                  "T3.sc", "T3b.sc",
                  "T3.ua", "T3b.ua",
                  "T3.pd", "T3b.pd",
                  "T3.ad", "T3b.ad",
                  "T3.AVvervolg2", "T3b.AVvervolg2", "T5.AVvervolg2",
                  "T3.nee.afgerond2", "T3b.nee.afgerond2", "T5.nee.afgerond2",
                  "T3.totMissing", "T3b.totMissing", "T5.totMissing",
                  "T3.HUI_Q1", "T3b.HUI_Q1",
                  "T3.HUI_Q2", "T3b.HUI_Q2",
                  "T3.HUI_Q3", "T3b.HUI_Q3", 
                  "T3.HUI_Q4", "T3b.HUI_Q4", 
                  "T3.HUI_Q5", "T3b.HUI_Q5", 
                  "T3.HUI_Q6", "T3b.HUI_Q6", 
                  "T3.HUI_Q7", "T3b.HUI_Q7", 
                  "T3.HUI_Q8", "T3b.HUI_Q8", 
                  "T3.HUI_Q9", "T3b.HUI_Q9", 
                  "T3.HUI_Q10", "T3b.HUI_Q10", 
                  "T3.HUI_Q11", "T3b.HUI_Q11", 
                  "T3.HUI_Q12", "T3b.HUI_Q12", 
                  "T3.HUI_Q13", "T3b.HUI_Q13", 
                  "T3.HUI_Q14", "T3b.HUI_Q14", 
                  "T3.HUI_Q15", "T3b.HUI_Q15", 
                  "T3.HUI3VL", "T3b.HUI3VL", 
                  "T3.HUI3HL", "T3b.HUI3HL", 
                  "T3.HUI3SL", "T3b.HUI3SL", 
                  "T3.HUI3EL", "T3b.HUI3EL", 
                  "T3.HUI3PL", "T3b.HUI3PL",
                  "T3.HUI3AL", "T3b.HUI3AL", 
                  "T3.HUI3DL", "T3b.HUI3DL", 
                  "T3.HUI3CL", "T3b.HUI3CL", 
                  "T3.a1", "T3b.a1", 
                  "T3.a2", "T3b.a2", 
                  "T3.a3", "T3b.a3", 
                  "T3.a4", "T3b.a4", 
                  "T3.a5", "T3b.a5", 
                  "T3.a6", "T3b.a6", 
                  "T3.a7", "T3b.a7", 
                  "T3.a8", "T3b.a8", 
                  "T3.HUI2SL", "T3b.HUI2SL", 
                  "T3.HUI2ML", "T3b.HUI2ML", 
                  "T3.HUI2CL", "T3b.HUI2CL", 
                  "T3.HUI2SCL", "T3b.HUI2SCL", 
                  "T3.HUI2EL", "T3b.HUI2EL", 
                  "T3.HUI2PL", "T3b.HUI2PL", 
                  "T3.b1", "T3b.b1", 
                  "T3.b2", "T3b.b2", 
                  "T3.b3", "T3b.b3", 
                  "T3.b4", "T3b.b4", 
                  "T3.b5", "T3b.b5", 
                  "T3.b6", "T3b.b6",
            
            "T4.totTSQ", "T5.totTSQ",
            "T3b.totAUDIT", "T4.totAUDIT", 
            "T3.WHOQOL.BREF.DOM1", "T3b.WHOQOL.BREF.DOM1",
            "T3.WHOQOL.BREF.DOM2", "T3b.WHOQOL.BREF.DOM2",
            "T3.WHOQOL.BREF.DOM3", "T3b.WHOQOL.BREF.DOM3",
            "T3.WHOQOL.BREF.DOM4", "T3b.WHOQOL.BREF.DOM4",
            "T3.MVH_A1", "T3b.MVH_A1",
            "T3.HUI3.UTI", "T3b.HUI3.UTI",
            "T3.HUI2.UTI", "T3b.HUI2.UTI",
            
            "T3.SSLD.1.tk", "T3b.SSLD.1.tk", "T4.SSLD.1.tk",
            "T3.SSLD.2.tk", "T3b.SSLD.2.tk", "T4.SSLD.2.tk",
            "T3.SSLD.3.tk", "T3b.SSLD.3.tk", "T4.SSLD.3.tk",
            "T3.SSLD.4.tk", "T3b.SSLD.4.tk", "T4.SSLD.4.tk",
            "T3.SSLD.5.tk", "T3b.SSLD.5.tk", "T4.SSLD.5.tk",
            "T3.SSLD.6.tk", "T3b.SSLD.6.tk", "T4.SSLD.6.tk",
            "T3.SSLD.7.tk", "T3b.SSLD.7.tk", "T4.SSLD.7.tk",
            "T3.SSLD.8.tk", "T3b.SSLD.8.tk", "T4.SSLD.8.tk",
            "T3.SSLD.9.tk", "T3b.SSLD.9.tk", "T4.SSLD.9.tk",
            "T3.SSLD.10.tk", "T3b.SSLD.10.tk", "T4.SSLD.10.tk",
            "T3.SSLD.11.tk", "T3b.SSLD.11.tk", "T4.SSLD.11.tk",
            "T3.SSLD.12.tk", "T3b.SSLD.12.tk", "T4.SSLD.12.tk",
            "T3.SSLD.13.tk", "T3b.SSLD.13.tk", "T4.SSLD.13.tk",
            "T3.SSLD.14.tk", "T3b.SSLD.14.tk", "T4.SSLD.14.tk",
            "T3.SSLD.15.tk", "T3b.SSLD.15.tk", "T4.SSLD.15.tk",
            "T3.SSLD.16.tk", "T3b.SSLD.16.tk", "T4.SSLD.16.tk",
            "T3.SSLD.17.tk", "T3b.SSLD.17.tk", "T4.SSLD.17.tk",
            "T3.SSLD.18.tk", "T3b.SSLD.18.tk", "T4.SSLD.18.tk",
            "T3.SSLD.19.tk", "T3b.SSLD.19.tk", "T4.SSLD.19.tk",
            "T3.SSLD.20.tk", "T3b.SSLD.20.tk", "T4.SSLD.20.tk",
            "T3.SSLD.21.tk", "T3b.SSLD.21.tk", "T4.SSLD.21.tk",
            "T3.SSLD.22.tk", "T3b.SSLD.22.tk", "T4.SSLD.22.tk",
            "T3.SSLD.23.tk", "T3b.SSLD.23.tk", "T4.SSLD.23.tk",
            "T3.SSLD.24.tk", "T3b.SSLD.24.tk", "T4.SSLD.24.tk",
            "T3.SSLD.25.tk", "T3b.SSLD.25.tk", "T4.SSLD.25.tk",
            "T3.SSLD.26.tk", "T3b.SSLD.26.tk", "T4.SSLD.26.tk",
            "T3.SSLD.27.tk", "T3b.SSLD.27.tk", "T4.SSLD.27.tk",
            "T3.SSLD.28.tk", "T3b.SSLD.28.tk", "T4.SSLD.28.tk",
            "T3.SSLD.29.tk", "T3b.SSLD.29.tk", "T4.SSLD.29.tk",
            "T3.SSLD.30.tk", "T3b.SSLD.30.tk", "T4.SSLD.30.tk",
            "T3.SSLD.31.tk", "T3b.SSLD.31.tk", "T4.SSLD.31.tk",
            "T3.SSLD.32.tk", "T3b.SSLD.32.tk", "T4.SSLD.32.tk",
            "T3.SSLD.33.tk", "T3b.SSLD.33.tk", "T4.SSLD.33.tk",
            "T3.SSLD.34.tk", "T3b.SSLD.34.tk", "T4.SSLD.34.tk",
            
            "T3.SSLD.AEI.tk", "T3b.SSLD.AEI.tk", "T4.SSLD.AEI.tk",
            "T3.SSLD.EOP.tk", "T3b.SSLD.EOP.tk", "T4.SSLD.EOP.tk",
            "T3.SSLD.II.tk", "T3b.SSLD.II.tk", "T4.SSLD.II.tk",
            "T3.SSLD.SC.tk", "T3b.SSLD.SC.tk", "T4.SSLD.SC.tk",
            "T3.SSLD.WS.tk", "T3b.SSLD.WS.tk", "T4.SSLD.WS.tk",
            "T3.SSLD.IO.tk", "T3b.SSLD.IO.tk", "T4.SSLD.IO.tk",
            
            "T3.totSSLD.tk", "T3b.totSSLD.tk", "T4.totSSLD.tk",
            
            "T3.SSLD.1.tv", "T3b.SSLD.1.tv", "T4.SSLD.1.tv",
            "T3.SSLD.2.tv", "T3b.SSLD.2.tv", "T4.SSLD.2.tv",
            "T3.SSLD.3.tv", "T3b.SSLD.3.tv", "T4.SSLD.3.tv",
            "T3.SSLD.4.tv", "T3b.SSLD.4.tv", "T4.SSLD.4.tv",
            "T3.SSLD.5.tv", "T3b.SSLD.5.tv", "T4.SSLD.5.tv",
            "T3.SSLD.6.tv", "T3b.SSLD.6.tv", "T4.SSLD.6.tv",
            "T3.SSLD.7.tv", "T3b.SSLD.7.tv", "T4.SSLD.7.tv",
            "T3.SSLD.8.tv", "T3b.SSLD.8.tv", "T4.SSLD.8.tv",
            "T3.SSLD.9.tv", "T3b.SSLD.9.tv", "T4.SSLD.9.tv",
            "T3.SSLD.10.tv", "T3b.SSLD.10.tv", "T4.SSLD.10.tv",
            "T3.SSLD.11.tv", "T3b.SSLD.11.tv", "T4.SSLD.11.tv",
            "T3.SSLD.12.tv", "T3b.SSLD.12.tv", "T4.SSLD.12.tv",
            "T3.SSLD.13.tv", "T3b.SSLD.13.tv", "T4.SSLD.13.tv",
            "T3.SSLD.14.tv", "T3b.SSLD.14.tv", "T4.SSLD.14.tv",
            "T3.SSLD.15.tv", "T3b.SSLD.15.tv", "T4.SSLD.15.tv",
            "T3.SSLD.16.tv", "T3b.SSLD.16.tv", "T4.SSLD.16.tv",
            "T3.SSLD.17.tv", "T3b.SSLD.17.tv", "T4.SSLD.17.tv",
            "T3.SSLD.18.tv", "T3b.SSLD.18.tv", "T4.SSLD.18.tv",
            "T3.SSLD.19.tv", "T3b.SSLD.19.tv", "T4.SSLD.19.tv",
            "T3.SSLD.20.tv", "T3b.SSLD.20.tv", "T4.SSLD.20.tv",
            "T3.SSLD.21.tv", "T3b.SSLD.21.tv", "T4.SSLD.21.tv",
            "T3.SSLD.22.tv", "T3b.SSLD.22.tv", "T4.SSLD.22.tv",
            "T3.SSLD.23.tv", "T3b.SSLD.23.tv", "T4.SSLD.23.tv",
            "T3.SSLD.24.tv", "T3b.SSLD.24.tv", "T4.SSLD.24.tv",
            "T3.SSLD.25.tv", "T3b.SSLD.25.tv", "T4.SSLD.25.tv",
            "T3.SSLD.26.tv", "T3b.SSLD.26.tv", "T4.SSLD.26.tv",
            "T3.SSLD.27.tv", "T3b.SSLD.27.tv", "T4.SSLD.27.tv",
            "T3.SSLD.28.tv", "T3b.SSLD.28.tv", "T4.SSLD.28.tv",
            "T3.SSLD.29.tv", "T3b.SSLD.29.tv", "T4.SSLD.29.tv",
            "T3.SSLD.30.tv", "T3b.SSLD.30.tv", "T4.SSLD.30.tv",
            "T3.SSLD.31.tv", "T3b.SSLD.31.tv", "T4.SSLD.31.tv",
            "T3.SSLD.32.tv", "T3b.SSLD.32.tv", "T4.SSLD.32.tv",
            "T3.SSLD.33.tv", "T3b.SSLD.33.tv", "T4.SSLD.33.tv",
            "T3.SSLD.34.tv", "T3b.SSLD.34.tv", "T4.SSLD.34.tv",
            
            "T3.SSLD.AEI.tv", "T3b.SSLD.AEI.tv", "T4.SSLD.AEI.tv",
            "T3.SSLD.EOP.tv", "T3b.SSLD.EOP.tv", "T4.SSLD.EOP.tv",
            "T3.SSLD.II.tv", "T3b.SSLD.II.tv", "T4.SSLD.II.tv",
            "T3.SSLD.SC.tv", "T3b.SSLD.SC.tv", "T4.SSLD.SC.tv",
            "T3.SSLD.WS.tv", "T3b.SSLD.WS.tv", "T4.SSLD.WS.tv",
            "T3.SSLD.IO.tv", "T3b.SSLD.IO.tv", "T4.SSLD.IO.tv",
            
            "T3.totSSLD.tv", "T3b.totSSLD.tv", "T4.totSSLD.tv",
            
            "T3.PTGI.1", "T3b.PTGI.1", "T4.PTGI.1",
            "T3.PTGI.2", "T3b.PTGI.2", "T4.PTGI.2",
            "T3.PTGI.3", "T3b.PTGI.3", "T4.PTGI.3",
            "T3.PTGI.4", "T3b.PTGI.4", "T4.PTGI.4",
            "T3.PTGI.5", "T3b.PTGI.5", "T4.PTGI.5",
            "T3.PTGI.6", "T3b.PTGI.6", "T4.PTGI.6",
            "T3.PTGI.7", "T3b.PTGI.7", "T4.PTGI.7",
            "T3.PTGI.8", "T3b.PTGI.8", "T4.PTGI.8",
            "T3.PTGI.9", "T3b.PTGI.9", "T4.PTGI.9",
            "T3.PTGI.10", "T3b.PTGI.10", "T4.PTGI.10",
            "T3.PTGI.11", "T3b.PTGI.11", "T4.PTGI.11",
            "T3.PTGI.12", "T3b.PTGI.12", "T4.PTGI.12",
            "T3.PTGI.13", "T3b.PTGI.13", "T4.PTGI.13",
            "T3.PTGI.14", "T3b.PTGI.14", "T4.PTGI.14",
            "T3.PTGI.15", "T3b.PTGI.15", "T4.PTGI.15",
            "T3.PTGI.16", "T3b.PTGI.16", "T4.PTGI.16",
            "T3.PTGI.17", "T3b.PTGI.17", "T4.PTGI.17",
            "T3.PTGI.18", "T3b.PTGI.18", "T4.PTGI.18",
            "T3.PTGI.19", "T3b.PTGI.19", "T4.PTGI.19",
            "T3.PTGI.20", "T3b.PTGI.20", "T4.PTGI.20",
            "T3.PTGI.21", "T3b.PTGI.21", "T4.PTGI.21"
                       )
    
    ## List variables to be selected, removed and changed 
      dat.t5.remo <- c("Patnr", "T5.verand.burg.staat", "T5.sport",
                       "T5.huid.sport", "T5.huid.sport.uitleg",
                       "T5.niv.sport", "T5.niv.sport.uitleg",
                       "T5.uren.sport", "T5.uren.sport.uitleg",
                       "T5.jur.verw.", "T5.jur.verw.uitleg",
                       "T5.jur.verw.gaande", "T5.jur.verw.gaande.wan.",
                       
                       "T5.ha.hoev", "T5.RIAGG",
                       "T5.RIAGG.hoev.", "T5.psy.pr",
                       "T5.psy.pr.hoev", "T5.psy.zk",
                       "T5.psy.zk.hoev", "T5.psy.srt.zk",
                       "T5.srt.zk.and", "T5.ba",
                       "T5.ba.hoev", "T5.spec.zk",
                       "T5.spec.srt", "T5.spec.hoev",
                       "T5.fys", "T5.fys.hoev",
                       "T5.mw", "T5.mw.hoev",
                       "T5.CAD", "T5.CAD.hoev",
                       "T5.thuisz", "T5.thuisz.hoev",
                       "T5.alt.gen", "T5.alt.gen.srt",
                       "T5.alt.gen.hoev", "T5.beh",
                       "T5.beh.hoev", "T5.beh.inst",
                       "T5.beh.inst.and", "T5.zelfh",
                       "T5.zelfh.srt", "T5.zelf.hoev",
                       
                       "T5.opgen.hoev", "T5.opgen.inst",
                       "T5.inst.and", "T5.opgen.zk",
                       "T5.opgen.zk.hoev", "T5.opgen.zk.kl",
                       "T5.opgen.rev", "T5.opgen.hoeveel",
                       "T5.opgen.rev.kl", "T5.med",
                       "T5.med.srt", "T5.med.dosis",
                       "T5.keer.p.dag", "T5.dag.4wk",
                       "T5.ziekt.verz", "T5.bet.werk",
                       "T5.u.p.wk", "T5.hoev.dg",
                       "T5.zelfd.ber.", "T5.nee.nu",
                       "T5.niet.gew.", "T5.niet.gew.hoel.",
                       "T5.nu.werk", "T5.nee.reden",
                       "T5.ziekte.afg.jr", "T5.verz.2wk",
                       "T5.verz.2wk.hoev", "T5.verz.langer",
                       "T5.gez.prob.2wk", "T5.hoev.dg.werk.gez.pr.",
                       "T5.effic", "T5.concentr",
                       "T5.tempo", "T5.afzonderen",
                       "T5.beslis", "T5.uitstellen",
                       "T5.overnemen", "T5.and.prob",
                       "T5.and.prob.uitleg", "T5.werk.inhalen",
                       "T5.inkomen", "T5.geen.bet.werk",
                       "T5.geen.bet.werk.sinds", "T5.hoev.werk.vroeger",
                       "T5.huish.werk", "T5.boodsch",
                       "T5.klussen", "T5.kinderen",
                       "T5.huish.anders", "T5.huish.ja.uitleg",
                       "T5.huish.ja.uitleg.uur", "T5.TOTAL.missing")
      
      ## Extract the type of variables for the original variables (to be copied later)
        colnames.type.key <- c("ALCAFH",
        "ALCMIS", "MIDSPEC", "MIDAFH.LT", "MIDAFH.H", "MIDMIS.H", "T4.Hypochondrie",
        "T4.Gen.Angstst", "T4.Gen.Angstst.medisch", "T4.Gen.Angstst.middelen", "T4.Gen.Angstst.leeftij",
        "T4.Gen.Angstst.duur", "Acute.Stress.Stoornis.Mini.Plus",
        "CAPS.C4.alternatief", "T3.CAPS.Crit.A", "T3.CAPS.Crit.B", "T3.CAPS.Crit.C",
        "T3.CAPS.Crit.D", "T3.CAPS.PTSD.DSM", 
        "CAPS.no.B", "CAPS.no.C", "CAPS.no.D", "CAPS.DSM",
        "substance.current",
        ## Self Report Measures
        "T3.TSQ1", "T3.TSQ2", "T3.TSQ3", "T3.TSQ4", "T3.TSQ5", "T3.TSQ6",
        "T3.TSQ7", "T3.TSQ8", "T3.TSQ9", "T3.TSQ10",
        "AUDIT1", "AUDIT2", "AUDIT3", "AUDIT4",
        "AUDIT5", "AUDIT6", "AUDIT7", "AUDIT8",
        "AUDIT9", "AUDIT10",
        "SSLD.1", "SSLD.2", "SSLD.3", "SSLD.4",
        "SSLD.5", "SSLD.6", "SSLD.7", "SSLD.8",
        "SSLD.9", "SSLD.10","SSLD.11", "SSLD.12",
        "SSLD.13","SSLD.14","SSLD.15", "SSLD.16",
        "SSLD.17","SSLD.18","SSLD.19", "SSLD.20",
        "SSLD.21","SSLD.22","SSLD.23", "SSLD.24",
        "SSLD.25","SSLD.26","SSLD.27", "SSLD.28",
        "SSLD.29","SSLD.30","SSLD.31", "SSLD.32",
        "SSLD.33","SSLD.34",
        "T4.WHO.1","T4.WHO.2","T4.WHO.3","T4.WHO.4","T4.WHO.5",
        "T4.WHO.6","T4.WHO.7","T4.WHO.8","T4.WHO.9","T4.WHO.10",
        "T4.WHO.11","T4.WHO.12","T4.WHO.13", "T4.WHO.14","T4.WHO.15",
        "T4.WHO.16","T4.WHO.17","T4.WHO.18", "T4.WHO.19","T4.WHO.20",
        "T4.WHO.21","T4.WHO.22","T4.WHO.23", "T4.WHO.24","T4.WHO.25",
        "T4.WHO.26",
        "T4.HUI12.1","T4.HUI12.2","T4.HUI12.3","T4.HUI12.4","T4.HUI12.5",
        "T4.HUI12.6","T4.HUI12.7","T4.HUI12.8","T4.HUI12.9","T4.HUI12.10",
        "T4.HUI12.11","T4.HUI12.12",
        
        "T4.HUI15.1","T4.HUI15.2","T4.HUI15.3","T4.HUI15.4","T4.HUI15.5",
        "T4.HUI15.6","T4.HUI15.7","T4.HUI15.8","T4.HUI15.9","T4.HUI15.10",
        "T4.HUI15.11","T4.HUI15.12","T4.HUI15.13","T4.HUI15.14","T4.HUI15.15",
        "T4.ha", "T4.opgen", "T4.dagen", "T4.ger.trauma", "T4.opgen.reval.",
        "T4.rev.dagen","T4.rev.ger.trauma", "T4.schadeverg.", "T4.type.schadeverg",
        "T4.reden.nee", "T4.uitleg.ja", "T4.aanvraag", "T4.nee.afgerond",
        "T4.schadeverg.ontv.", "T4.tevr.uitkomst", "T4.rechtv.uitkomst",
        "T4.EQ6D.1", "T4.EQ6D.2", "T4.EQ6D.3", "T4.EQ6D.4", "T4.EQ6D.5", 
        "T4.EQ6D.6", "T4.EQ6D.7", "T4.EQ6D.8",  "EQ6D.6.B", "EQ6D.7.B", 
        "EQ6D.8.B", "T4.mo", "T4.sc", "T4.ua", "T4.pd", "T4.ad",
        "AVvervolg2", "nee.afgerond2", "totMissing",
        "T4.HUI_Q1", "T4.HUI_Q2", "T4.HUI_Q3", "T4.HUI_Q4", "T4.HUI_Q5", 
        "T4.HUI_Q6", "T4.HUI_Q7", "T4.HUI_Q8", "T4.HUI_Q9", "T4.HUI_Q10", 
        "T4.HUI_Q11", "T4.HUI_Q12", "T4.HUI_Q13", "T4.HUI_Q14", "T4.HUI_Q15", 
        "T4.HUI3VL",  "T4.HUI3HL", "T4.HUI3SL", "T4.HUI3EL", "T4.HUI3PL", "T4.HUI3AL", 
        "T4.HUI3DL",  "T4.HUI3CL", 
        "T4.a1", "T4.a2", "T4.a3", "T4.a4", 
        "T4.a5", "T4.a6", "T4.a7", "T4.a8", 
        "T4.HUI2SL", "T4.HUI2ML", "T4.HUI2CL", 
        "T4.HUI2SCL", "T4.HUI2EL", "T4.HUI2PL", 
        "T4.b1", "T4.b2", "T4.b3", "T4.b4", "T4.b5", "T4.b6",
        "T3.totTSQ", "T3.totAUDIT", 
        "T4.WHOQOL.BREF.DOM1","T4.WHOQOL.BREF.DOM2", 
        "T4.WHOQOL.BREF.DOM3", "T4.WHOQOL.BREF.DOM4",
        "T4.MVH_A1", "T4.HUI3.UTI", "T4.HUI2.UTI",
        
        "SSLD.1.tk", "SSLD.2.tk", "SSLD.3.tk","SSLD.4.tk", "SSLD.5.tk",
        "SSLD.6.tk", "SSLD.7.tk", "SSLD.8.tk","SSLD.9.tk", "SSLD.10.tk",
        "SSLD.11.tk","SSLD.12.tk","SSLD.13.tk","SSLD.14.tk","SSLD.15.tk",
        "SSLD.16.tk","SSLD.17.tk","SSLD.18.tk","SSLD.19.tk","SSLD.20.tk",
        "SSLD.21.tk","SSLD.22.tk","SSLD.23.tk","SSLD.24.tk","SSLD.25.tk",
        "SSLD.26.tk","SSLD.27.tk","SSLD.28.tk","SSLD.29.tk",
        "SSLD.30.tk","SSLD.31.tk","SSLD.32.tk","SSLD.33.tk","SSLD.34.tk",
        
        "SSLD.AEI.tk","SSLD.EOP.tk","SSLD.II.tk","SSLD.SC.tk",
        "SSLD.WS.tk","SSLD.IO.tk","totSSLD.tk",
        
        "SSLD.1.tv","SSLD.2.tv","SSLD.3.tv","SSLD.4.tv","SSLD.5.tv",
        "SSLD.6.tv","SSLD.7.tv","SSLD.8.tv","SSLD.9.tv","SSLD.10.tv",
        "SSLD.11.tv","SSLD.12.tv","SSLD.13.tv","SSLD.14.tv","SLD.15.tv",
        "SSLD.16.tv","SSLD.17.tv","SSLD.18.tv","SSLD.19.tv","SSLD.20.tv",
        "SSLD.21.tv","SSLD.22.tv","SSLD.23.tv","SSLD.24.tv","SSLD.25.tv",
        "SSLD.26.tv","SSLD.27.tv","SSLD.28.tv","SSLD.29.tv","SSLD.30.tv",
        "SSLD.31.tv","SSLD.32.tv","SSLD.33.tv","SSLD.34.tv",
        
        "SSLD.AEI.tv","SSLD.EOP.tv","SSLD.II.tv","SSLD.SC.tv",
        "SSLD.WS.tv","SSLD.IO.tv","totSSLD.tv",
        
        "PTGI.1","PTGI.2","PTGI.3","PTGI.4","PTGI.5",
        "PTGI.6","PTGI.7","PTGI.8","PTGI.9","PTGI.10",
        "PTGI.11","PTGI.12","PTGI.13","PTGI.14","PTGI.15","PTGI.16",
        "PTGI.17","PTGI.18","PTGI.19","PTGI.20","PTGI.21"
        )
        
      ## Extract variables regarding type 
        ## Select potential variables to cut 
        colnames.timeT3T5.copy2 <- tidyselect::vars_select(
          names(dat.copy), contains(match = c("Patnr", "T3.", "T3b.", "T4.", "T5.")))
        dat.copy.timeT3T5.copy2 <- dat.copy[colnames.timeT3T5.copy2]
        
        colnames.other.nonT <- tidyselect::vars_select(
          names(dat.copy), !contains(match = c("T3.", "T3b.", "T4.", "T5.")))
        dat.copy.other.nonT <- dat.copy[colnames.other.nonT]
        
        dat.type.cols <- tidyselect::vars_select(
          names(dat.copy.timeT3T5.copy2), ends_with(match = colnames.type.key))
        dat.type <- dat.copy[dat.type.cols]
        dat.type.cols.2 <- colnames(dat.type)
        ## Remove prefix
        dat.type.cols.2 <- gsub("T3.", "",  dat.type.cols.2)
        dat.type.cols.2 <- gsub("T3b.", "",  dat.type.cols.2)
        dat.type.cols.2 <- gsub("T4.", "",  dat.type.cols.2)
        dat.type.cols.2 <- gsub("T5.", "",  dat.type.cols.2)
        ## Rename columns 
        colnames(dat.type) <- dat.type.cols.2
        
        
        attr(dat.type.class, 'label') <- NULL
        
        dat.type.class <- map(dat.type, class)
        
        dat.type.class.df <- data.frame(matrix(unlist(dat.type.class), nrow = 613))
        ## Subset variables 
        colnames(dat.type.class.df) <- "V1"
        dat.type.class.df <- dat.type.class.df[which(dat.type.class.df$V1 == "integer" |
                                                       dat.type.class.df$V1 == "numeric" |
                                                       dat.type.class.df$V1 == "double" | 
                                                       dat.type.class.df$V1 == "character" | 
                                                       dat.type.class.df$V1 == "logical"), ]
        
        dat.type.class.df <- cbind(dat.type.cols.2, dat.type.class.df)
        
        
        ## Select Variables
      dat.t5.isol <- dat.copy[,dat.t5.remo]
      
      ## Remove T5 prefix 
      colnames.t5.iso <- colnames(dat.t5.isol)
      ## Allocate general column names for new copies
      colnames.t5.iso <- as.data.frame(colnames.t5.iso)
      ## Remove "T5."
      colnames.t5.iso[2:97,] <- gsub("T5.", "", colnames.t5.iso[2:97,])
      colnames.t5.iso <- colnames.t5.iso$colnames.t5.iso
      
      ## Rename Columns accordingly
      colnames(dat.t5.isol) <- colnames.t5.iso
      
      
      
      ## Replace variables accordingly 
      dat.copy <- remove.vars(data = dat.copy, 
                              names = dat.t5.remo[2:97],
                              info = TRUE)
      ## Return variables which are corrected
      dat.copy <- inner_join(x = dat.copy, 
                             y = dat.t5.isol, 
                             by = "Patnr")
      
      
    ## Create Empty dataframe 
    dat.empty <- as.data.frame(matrix(data = NA, nrow = 852, ncol = 798))
    colnames(dat.empty) <- new.var
    ## Add empty dataframe to copied data
    dat.copy <- cbind(dat.copy, dat.empty)
    
    
    ## Split off time sensitive data
    colnames.timeT3T5.copy <- tidyselect::vars_select(
      names(dat.copy), contains(match = c("Patnr", "T3.", "T3b.", "T4.", "T5.")))
    dat.copy.timeT3T5.org <- dat.copy[colnames.timeT3T5.copy]
    
    
    ## Remove Variables which can not be compared / moved in time.
    ## T5 points removed and converted to general points (these were measured where possible at the end of the study)
    colnames.timeT3T5.rem <- tidyselect::vars_select(
      names(dat.copy.timeT3T5.org), ends_with(match = c("smoke.cat", "cigarette.quant", "cigar.quant",
                                           "pip.quant", "available.int", "available.bklt",
                                           "cat.part", "stop.prior", "dropout",
                                           "T3.med.psych", "T3.med.psych.exp")))
    
    dat.copy.timeT3T5 <- remove.vars(data = dat.copy.timeT3T5.org,
                                     names = colnames.timeT3T5.rem, 
                                     info = TRUE)
    
    colnames.timeT3T5.nonrem <- tidyselect::vars_select(
      names(dat.copy.timeT3T5.org), !ends_with(match = c("Patnr", "smoke.cat", "cigarette.quant", "cigar.quant",
                                                    "pip.quant", "available.int", "available.bklt",
                                                    "cat.part", "stop.prior", "dropout",
                                                    "T3.med.psych", "T3.med.psych.exp")))
    
    dat.copy.removedT3T5 <- remove.vars(data = dat.copy.timeT3T5.org,
                                        names = colnames.timeT3T5.nonrem, 
                                        info = TRUE)
    
    
    ## Reshape manually 
    ## Split dataset by Original Time point 
    colnames.CAPST3.copy <- tidyselect::vars_select(
      names(dat.copy.timeT3T5), contains(match = c("Patnr", "T3.")))
    dat.caps.T3.copy <- dat.copy.timeT3T5[colnames.CAPST3.copy]
    
    colnames.CAPST3b.copy <- tidyselect::vars_select(
      names(dat.copy.timeT3T5), contains(match = c("Patnr", "T3b.")))
    dat.caps.T3b.copy <- dat.copy.timeT3T5[colnames.CAPST3b.copy]
    
    colnames.CAPST4.copy <- tidyselect::vars_select(
      names(dat.copy.timeT3T5), contains(match = c("Patnr", "T4.")))
    dat.caps.T4.copy <- dat.copy.timeT3T5[colnames.CAPST4.copy]
    
    colnames.CAPST5.copy <- tidyselect::vars_select(
      names(dat.copy.timeT3T5), contains(match = c("Patnr", "T5.")))
    dat.caps.T5.copy <- dat.copy.timeT3T5[colnames.CAPST5.copy]
    
    ## Remove cases with NULL dates 
    for (i in 1:852){
      if (is.na(dat.caps.T3.copy[i, "T3.dat"]) == TRUE)
        (dat.caps.T3.copy[i, 4:528] = NA)
    }
    
    for (i in 1:852){
      if (is.na(dat.caps.T3.copy[i, "T3b.dat"]) == TRUE)
        (dat.caps.T3b.copy[i, 4:528] = NA)
    }
    
    for (i in 1:852){
      if (is.na(dat.caps.T3.copy[i, "T4.dat"]) == TRUE)
        (dat.caps.T4.copy[i, 4:528] = NA)
    }
    
    for (i in 1:852){
      if (is.na(dat.caps.T3.copy[i, "T5.dat"]) == TRUE)
        (dat.caps.T5.copy[i, 4:528] = NA)
    }
    
    ## Allocate general column names for new copies
    colnames.CAPS.copy <- as.data.frame(colnames.CAPST3.copy)
    colnames.CAPS.copy <- colnames.CAPS.copy$colnames.CAPST3.copy
    ## Remove "T3."
    colnames.CAPS.copy[2:528] <- gsub("T3.", "", colnames.CAPS.copy[2:528])
    
    ## Change Colnames for the split variables 
    colnames(dat.caps.T3.copy) <- colnames.CAPS.copy
    colnames(dat.caps.T3b.copy) <- colnames.CAPS.copy
    colnames(dat.caps.T4.copy) <- colnames.CAPS.copy
    colnames(dat.caps.T5.copy) <- colnames.CAPS.copy
    
    ## Add Reference of Original Time point 
    dat.caps.T3.copy$org_t <- "T3"
    dat.caps.T3b.copy$org_t <- "T3b"
    dat.caps.T4.copy$org_t <- "T4"
    dat.caps.T5.copy$org_t <- "T5"
    
    ## Ensure consistency for all variable types 
    
    ## Extract those missing types from each split dataframe 
    dat.caps.T3.copy.2 <- dat.caps.T3.copy %>%
      select(all_of(dat.type.cols.2))
    dat.caps.T3b.copy.2 <- dat.caps.T3b.copy %>%
      select(all_of(dat.type.cols.2))
    dat.caps.T4.copy.2 <- dat.caps.T4.copy %>%
      select(all_of(dat.type.cols.2))
    dat.caps.T5.copy.2 <- dat.caps.T5.copy %>%
      select(all_of(dat.type.cols.2))
    
    ## Remove these variables 
    dat.caps.T3.copy.3 <- dat.caps.T3.copy %>%
      select(!all_of(dat.type.cols.2))
    dat.caps.T3b.copy.3 <- dat.caps.T3b.copy %>%
      select(!all_of(dat.type.cols.2))
    dat.caps.T4.copy.3 <- dat.caps.T4.copy %>%
      select(!all_of(dat.type.cols.2))
    dat.caps.T5.copy.3 <- dat.caps.T5.copy %>%
      select(!all_of(dat.type.cols.2))
    
    ## Apply expected variable types accordingly 
    dat.caps.T3.copy.2[c(1,2,4:24)] <- sapply(
      dat.caps.T3.copy.2[c(1,2,4:24)], FUN = as.integer)
    dat.caps.T3.copy.2[c(3)] <- sapply(dat.caps.T3.copy.2[c(3)], FUN = as.factor)
    dat.caps.T3.copy.2[c(25:133, 135:136, 138:140, 143, 145:154, 301:321)] <- sapply(
      dat.caps.T3.copy.2[c(25:133, 135:136, 138:140, 143, 145:154, 301:321)], FUN = as.double)
    dat.caps.T3.copy.2[c(141:142, 144, 164:165)] <- sapply(
      dat.caps.T3.copy.2[c(141:142, 144, 164:165)], FUN = as.character)
    dat.caps.T3.copy.2[c(134, 137, 155:163, 166:300)] <- sapply(
      dat.caps.T3.copy.2[c(134, 137, 155:163, 166:300)], FUN = as.double)
    
    dat.caps.T3b.copy.2[c(1,2,4:24)] <- sapply(
      dat.caps.T3b.copy.2[c(1,2,4:24)], FUN = as.integer)
    dat.caps.T3b.copy.2[c(3)] <- sapply(
      dat.caps.T3b.copy.2[c(3)], FUN = as.factor)
    dat.caps.T3b.copy.2[c(25:133, 135:136, 138:140, 143, 145:154, 301:321)] <- sapply(
      dat.caps.T3b.copy.2[c(25:133, 135:136, 138:140, 143, 145:154, 301:321)], FUN = as.double)
    dat.caps.T3b.copy.2[c(141:142, 144, 164:165)] <- sapply(
      dat.caps.T3b.copy.2[c(141:142, 144, 164:165)], FUN = as.character)
    dat.caps.T3b.copy.2[c(134, 137, 155:163, 166:300)] <- sapply(
      dat.caps.T3b.copy.2[c(134, 137, 155:163, 166:300)], FUN = as.double)
    
    dat.caps.T4.copy.2[c(1,2,4:24)] <- sapply(
      dat.caps.T4.copy.2[c(1,2,4:24)], FUN = as.integer)
    dat.caps.T4.copy.2[c(3)] <- sapply(
      dat.caps.T4.copy.2[c(3)], FUN = as.factor)
    dat.caps.T4.copy.2[c(25:133, 135:136, 138:140, 143, 145:154, 301:321)] <- sapply(
      dat.caps.T4.copy.2[c(25:133, 135:136, 138:140, 143, 145:154, 301:321)], FUN = as.double)
    dat.caps.T4.copy.2[c(141:142, 144, 164:165)] <- sapply(
      dat.caps.T4.copy.2[c(141:142, 144, 164:165)], FUN = as.character)
    dat.caps.T4.copy.2[c(134, 137, 155:163, 166:300)] <- sapply(
      dat.caps.T4.copy.2[c(134, 137, 155:163, 166:300)], FUN = as.numeric)
    
    dat.caps.T5.copy.2[c(1,2,4:24)] <- sapply(
      dat.caps.T5.copy.2[c(1,2,4:24)], FUN = as.integer)
    dat.caps.T5.copy.2[c(3)] <- sapply(
      dat.caps.T5.copy.2[c(3)], FUN = as.factor)
    dat.caps.T5.copy.2[c(25:133, 135:136, 138:140, 143, 145:154, 301:321)] <- sapply(
      dat.caps.T5.copy.2[c(25:133, 135:136, 138:140, 143, 145:154, 301:321)], FUN = as.double)
    dat.caps.T5.copy.2[c(141:142, 144, 164:165)] <- sapply(
      dat.caps.T5.copy.2[c(141:142, 144, 164:165)], FUN = as.character)
    dat.caps.T5.copy.2[c(134, 137, 155:163, 166:300)] <- sapply(
      dat.caps.T5.copy.2[c(134, 137, 155:163, 166:300)], FUN = as.numeric)

    ## Combine groups back 
    dat.caps.T3.prejoin <- cbind(dat.caps.T3.copy.3, dat.caps.T3.copy.2)
    dat.caps.T3.prejoin[c(160:199)] <- sapply(dat.caps.T3.prejoin[c(160:199)], FUN = as.double)
    dat.caps.T3b.prejoin <- cbind(dat.caps.T3b.copy.3, dat.caps.T3b.copy.2)
    dat.caps.T3b.prejoin[c(160:199)] <- sapply(dat.caps.T3b.prejoin[c(160:199)], FUN = as.double)
    dat.caps.T4.prejoin <- cbind(dat.caps.T4.copy.3, dat.caps.T4.copy.2)
    dat.caps.T4.prejoin[c(160:199)] <- sapply(dat.caps.T4.prejoin[c(160:199)], FUN = as.double)
    dat.caps.T5.prejoin <- cbind(dat.caps.T5.copy.3, dat.caps.T5.copy.2)
    dat.caps.T5.prejoin[c(160:199)] <- sapply(dat.caps.T5.prejoin[c(160:199)], FUN = as.double)
    
    dat.caps.long.copy <- rbind(dat.caps.T3.prejoin, dat.caps.T3b.prejoin, dat.caps.T4.prejoin, dat.caps.T5.prejoin)
    
    ## Mutate in corrected date
    dat.caps.long.copy.2 <- dat.caps.long.copy %>%
      ## Use ifelse conditions, to split off data in sections, 
      ## T3 < 60, T3b < 136, T4 < 280, T5 everything else
      mutate(new_t = ifelse(TIJD.IN < 61, "T3", 
                            ifelse(TIJD.IN < 136, "T3b",
                                   ifelse(TIJD.IN < 281, "T4", "T5"))))
    
    dat.caps.long.copy.2[c(213, 220, 224, 232, 293, 308, 399, 416, 421, 459, 521, 
                           535, 561, 586, 587, 588, 595, 610, 632, 658, 659, 667, 
                           704, 718, 723, 737, 754, 770, 829, 830, 842, 843, 850,
                           1236, 1254, 1277, 1337, 1338, 1341, 1346, 1352, 1360, 
                           1392, 1405, 1409, 1442, 1457, 1460, 1477, 1487, 1492, 
                           1503, 1506, 1508, 1521, 1559, 1566, 1592, 1609, 1676, 
                           1679, 1693, 1695, 1696, 1699, 1822, 1826, 1839, 1844, 
                           1849, 1860, 1914, 2049, 2055, 2067, 2109, 2140, 2203, 
                           2226, 2264, 2274, 2285, 2348, 2355, 2361, 2364, 2392, 2452, 
                           2669), "new_t"] <- NA 
    
    
    ## Split up the combined dataset by the new time point variables 
    dat.caps.long.T3 <- dat.caps.long.copy.2 %>%
      filter(new_t == "T3")
    dat.caps.long.T3b <- dat.caps.long.copy.2 %>%
      filter(new_t == "T3b")
    dat.caps.long.T4 <- dat.caps.long.copy.2 %>%
      filter(new_t == "T4")
    dat.caps.long.T5 <- dat.caps.long.copy.2 %>%
      filter(new_t == "T5")
    
    ## Rename columns according to time point
      ## Collect all general time points
    colnamest.long <- colnames(dat.caps.long.T3)
      ## Rename all based on Time Point (T3-T5)
    colnamest3.long <- paste0('T3.', colnamest.long)
    colnamest3b.long <- paste0('T3b.', colnamest.long)
    colnamest4.long <- paste0('T4.', colnamest.long)
    colnamest5.long <- paste0('T5.', colnamest.long)
      ## Relabel First to Patnr 
    colnamest3.long[1] <- "Patnr"
    colnamest3b.long[1] <- "Patnr"
    colnamest4.long[1] <- "Patnr"
    colnamest5.long[1] <- "Patnr"
      ## Rename all columns accordingly 
    colnames(dat.caps.long.T3) <- colnamest3.long
    colnames(dat.caps.long.T3b) <- colnamest3b.long
    colnames(dat.caps.long.T4) <- colnamest4.long
    colnames(dat.caps.long.T5) <- colnamest5.long
      ## Rejoin into wide format
    ## Generate empty matrix for full join
    patnr.col <- as.data.frame(dat$Patnr)
    colnames(patnr.col) <- "Patnr"
    
    dat.caps.wide <- left_join(patnr.col, dat.caps.long.T3, by = "Patnr")
    dat.caps.wide <- left_join(dat.caps.wide, dat.caps.long.T3b, by = "Patnr")
    dat.caps.wide <- left_join(dat.caps.wide, dat.caps.long.T4, by = "Patnr")
    dat.caps.wide <- left_join(dat.caps.wide, dat.caps.long.T5, by = "Patnr")
    dat.caps.wide <- left_join(dat.caps.wide, dat.copy.other.nonT, by = "Patnr")
    dat.caps.wide <- left_join(dat.caps.wide, dat.copy.removedT3T5, by = "Patnr")
    
    ## Remove org_t and new_t variables 
    t_remove <- c("T3.org_t", "T3b.org_t", "T4.org_t", "T5.org_t",
                  "T3.new_t", "T3b.new_t", "T4.new_t", "T5.new_t")
    
    
    dat.caps.wide.cleaned <- remove.vars(data = dat.caps.wide,
                                        names = t_remove, 
                                        info = TRUE)
    
    ## Export as CSV
    write.csv(dat.caps.wide.cleaned, "datcleaned.csv")
 
################################################################################
##### Ensure Correct Variable Type #######
################################################################################   
  
  dat.type.update <- colnames(dat.caps.wide.cleaned)
    class(dat.caps.wide$T3.CAPS.A1.levensbedreigend)
    dat.caps.wide.cleaned$T3.CAPS.A1.levensbedreigend <- as.factor(dat.caps.wide.cleaned$T3.CAPS.A1.levensbedreigend)
  dat.type.update <- as.data.frame(dat.type.update)
  dat.type.update$var_type <- NA
  
  
  numeric.conv <- c(1,
                    ## T3
                    3,91:92,107:108,114:115,122:123,126:127,130:131,135:136,
                    148:149,154:157,200:207,218:219, 339:354, 355:365, 371:373,397:404, 
                    411:416, 417:418, 443:449,484:487, 490, 
                    ## T3b
                    530, 618:619,634:635, 641:642, 649:650, 653:654, 657:658,
                    662:663, 675:676, 681:684, 727:734, 745:746, 866:881, 882:892,
                    898:900, 924:931, 938:943, 944:945, 970:976, 1011:1014, 1017,
                    ## T4
                    1057, 1145:1146, 1161:1162, 1168:1169, 1176:1177, 1180:1181, 1184:1185,
                    1189:1190, 1202:1203, 1208:1211, 1254:1261, 1272:1273, 1393:1408, 
                    1409:1419, 1425:1427, 1451:1458, 1465:1470, 1471:1472, 1497:1503, 
                    1538:1541, 1544,
                    ## T5
                    1584, 1672:1673, 1688:1689, 1695:1696, 1703:1704, 1707:1708,
                    1711:1712, 1716:1717, 1729:1730, 1735:1738, 1781:1788, 1799:1800,
                    1920:1935, 1936:1946, 1952:1954, 1978:1985, 1992:1997, 1998:1999,
                    2024:2030, 2065:2068, 2071,
                    ## Self Report
                    2121, 2125, 2134:2137, 2138, 2238, 2261:2263, 2276, 2296, 2297, 
                    2299:2300, 
                    ## T2
                    2308, 2315:2316, 2335:2336, 2342:2343, 2350:2351, 2358:2359,
                    2363:2364, 2373, 2386:2387, 2391:2392, 2485:2488, 2505, 2506,
                    2903:2919, 2954:2960, 2995:3001, 3003, 3009)
  
  dat.conv <- c(2, 529, 1056, 1583, 2237, 2239:2240, 2248, 2280, 2307, 
                  2498)
  
  fact.conv <- c(
    ## T3
    4:83, 85:90, 93:106, 109:113,
    116:121, 124:125, 128, 132:134,
    137:147, 150:151, 152:153, 158:159, 
    160:181, 182:195, 196:199, 208:217, 
    220:231, 232:241, 242:251, 252:285, 
    286:311, 312:323, 324:338, 366:370, 
    374:388, 389:396, 405:410, 424:425,
    419:423, 426:442, 450:466, 467:483, 
    488:489, 491:507, 508:528,
    ## T3b
    531:610, 612:617, 620:633, 636:640,
    643:648, 651:652, 655, 659:661,
    664:674, 677:678, 679:680, 685:686,
    687:708, 709:722, 723:726, 735:744,
    747:758, 759:768, 769:778, 779:812,
    813:838, 839:850, 851:865, 893:897,
    901:915, 916:923, 932:937, 951:952, 
    946:950, 953:969, 977:993, 994:1010,
    1015:1016, 1018:1034, 1035:1055,
    ## T4
    1058:1137, 1139:1144, 1147:1160, 1163:1167,
    1170:1175, 1178:1179, 1182, 1186:1188,
    1191:1201, 1204:1205, 1206:1207, 1212:1213,
    1214:1235, 1236:1249, 1250:1253, 1262:1271,
    1274:1285, 1286:1295, 1296:1305, 1306:1339,
    1340:1365, 1366:1377, 1378:1392, 1420:1424,
    1428:1442, 1443:1450, 1459:1464, 1478:1479,
    1473:1477, 1480:1496, 1504:1520, 1521:1537, 
    1542:1543, 1545:1561, 1562:1582,
    ## T5
    1585:1664, 1666:1671, 1674:1687, 1690:1694,
    1697:1702, 1705:1706, 1709, 1713:1715,
    1718:1728, 1731:1732, 1733:1734, 1739:1740,
    1741:1762, 1763:1776, 1777:1780, 1789:1798,
    1801:1812, 1813:1822, 1823:1832, 1833:1866,
    1867:1892, 1893:1904, 1905:1919, 1947:1951,
    1955:1969, 1970:1977, 1986:1991, 2005:2006,
    2000:2004, 2007:2023, 2031:2047, 2048:2064,
    2069:2070, 2072:2088, 2089:2109,
    ## Self
    2110:2120, 2122, 2124, 2127:2132, 2139, 2142:2165,
    2166, 2168, 2171:2178, 2179:2180, 2182:2184, 2186, 2188:2193,
    2196:2218, 2219:2220, 2223:2231, 2233:2236, 2242, 
    2244:2247, 2249, 2250:2260, 2264, 2266, 2268, 2270, 2272:2273,
    2277:2278, 2282:2283, 2285, 2287, 2289:2295, 2298, 
    2301:2306, 2309:2314, 2317:2334, 2337:2341, 2344:2349, 
    2352:2353, 2356, 2360:2362, 2365:2372, 2374:2384, 
    2388:2390, 2393:2398, 2400:2474, 2476:2484, 2489:2497,
    2499:2504, 2510:2514, 2518:2521, 2526:2530, 2534:2536, 
    2540:2544, 2548:2552, 2557:2560, 2565:2568, 2572:2575,
    2581:2584, 2589:2592, 2596:2599, 2603:2606, 2611:2613,
    2618:2620, 2624:2626, 2631:2633, 2637:2639, 2645:2647, 
    2651:2653, 2657:2659, 2663:2665, 2670:2674, 2679:2684, 
    2687:2692, 2695:2698, 2699:2902, 2920:2953, 2961:2994, 
    3002, 3004:3006, 3008, 3010:3011, 3012:3026)
  
  str.conv <- c(84, 129, 2111, 2123, 2126, 2133, 2140, 2141, 2167, 2169:2170,
                2181, 2185, 2187, 2194:2195, 2221:2222, 2232, 2241, 2243, 2265,
                2267, 2269, 2271, 2274:2275, 2279, 2281, 2284, 2286, 2288, 
                2354:2355, 2357, 2385, 2399, 2475, 2507:2509, 2515:2517, 2522:2525,
                2531:2533, 2537:2539, 2545:2547, 2553:2556, 2561:2564, 2569:2571,
                2576:2580, 2585:2588, 2593:2595, 2600:2602, 2607:2610, 2614:2617,
                2621:2623, 2627:2630, 2634:2636, 2640:2644, 2648:2650, 2654:2656,
                2660:2662, 2666:2669, 2675:2678, 2685:2686, 2693:2694, 3007)
  
  dat.caps.wide.cleaned[,numeric.conv] <- sapply(
    dat.caps.wide.cleaned[,numeric.conv], FUN = as.numeric)
  
  ## Dates already converted earlier 
  
  dat.caps.wide.cleaned[,fact.conv] <- sapply(
    dat.caps.wide.cleaned[,fact.conv], FUN = as.numeric)
  
  ## dat.caps.wide.cleaned[,fact.conv] <- as.factor(dat.caps.wide.cleaned[,fact.conv])
  
  dat.caps.wide.cleaned[,str.conv] <- sapply(
    dat.caps.wide.cleaned[,str.conv], FUN = as.character)
  
################################################################################
##### Save and Export #######
################################################################################

    ## Export RData 
  write.csv(dat.caps.wide.cleaned, "datcleaned2.csv")
    saveRDS(dat.caps.wide.cleaned, "datacleaned.rds")
  
  
  
