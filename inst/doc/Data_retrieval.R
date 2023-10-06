## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
# Primary Packages #
library(tcpl)
library(tcplfit2)
# Data Formatting Packages #
library(dplyr)
library(magrittr)
# Plotting Packages #
library(ggplot2)
library(RColorBrewer)
library(colorspace)
library(viridis)
# Table Packages #
library(htmlTable)
library(kableExtra)

## ----tcplLoad, eval = FALSE---------------------------------------------------
#  # List all assay source IDs
#  tcplLoadAsid()
#  # Create table of all assay endpoint ids (aeids) per assay source
#  aeids <- tcplLoadAeid(fld="asid", # field to query on
#                        val=14, # value for each field
#                                # values should match their corresponding 'fld'
#                        add.fld = c("aid", "anm", "acid", "acnm")) # additional fields to return

## ----tcplquery, eval = FALSE--------------------------------------------------
#  # Load sample table using a MySQL query.
#  samples <- tcplQuery("SELECT * FROM sample;")

## ----tcplPrepOtpt, eval=FALSE-------------------------------------------------
#  # Load multi concentration data from level 2,
#  # and map only the chemical annotation information.
#  mc2_fmtd <- tcplPrepOtpt(
#    tcplLoadData(
#      lvl = 2, # data level
#      fld = 'acid', # field to query on
#      val = 49, # value for each field
#               # values should match their corresponding 'fld'
#      type = 'mc' # data type
#    ),
#    ids = 'spid' # additional annotation fields to add - just chemical info
#                 # - (Default): map assay and chemical annotation
#                 # - 'acid' OR 'aeid': map only assay annotation
#                 # - 'spid': map only chemical annotation
#  )
#  # Print the first 6 rows of 'mc2_fmtd'
#  head(mc2_fmtd)

## ----annotation_query_ex, eval = FALSE----------------------------------------
#  #load libraries and connections
#  library(RMySQL)
#  con <- dbConnect(drv = RMySQL::MySQL(), user="user", pass="pass", db="InvitroDB", host="host")
#  #query database using RMySQL:
#  #use source table to identify which ids are needed in subsequent queries.
#  tcplLoadAsid()
#  source <- tcplLoadAeid(fld="asid", val=1, add.fld = c("aid", "anm", "acid", "acnm"))
#  #select annotation and subset by ids or name
#  assay <- dbGetQuery(con, "SELECT * FROM invitrodb.assay where aid=1;")
#  component <- dbGetQuery(con, "SELECT * FROM invitrodb.assay_component;")
#  component <- subset(component, acid %in% source$acid)
#  endpoint <- dbGetQuery(con, "SELECT * FROM invitrodb.assay_component_endpoint;")
#  endpoint <- endpoint[grepl("ATG", endpoint$assay_component_endpoint_name),]

## ----mthd_list, eval = FALSE--------------------------------------------------
#  # Create a function to list all available methods function (SC & MC).
#  method_list <- function() {
#    # Single Concentration
#    ## Level 1
#    sc1 <- tcplMthdList(1, 'sc')
#    sc1[, lvl := "sc1"]
#    setnames(sc1, c("sc1_mthd", "sc1_mthd_id"), c("mthd", "mthd_id"))
#    ## Level 2
#    sc2 <- tcplMthdList(2, 'sc')
#    sc2[, lvl := "sc2"]
#    setnames(sc2, c("sc2_mthd", "sc2_mthd_id"), c("mthd", "mthd_id"))
#  
#    # Multiple Concentration
#    ## Level 2
#    mc2 <- tcplMthdList(2, 'mc')
#    mc2[, lvl := "mc2"]
#    setnames(mc2, c("mc2_mthd", "mc2_mthd_id"), c("mthd", "mthd_id"))
#    ## Level 3
#    mc3 <- tcplMthdList(3, 'mc')
#    mc3[, lvl := "mc3"]
#    setnames(mc3, c("mc3_mthd", "mc3_mthd_id"), c("mthd", "mthd_id"))
#    ## Level 4
#    mc4 <- tcplMthdList(4, 'mc')
#    mc4[, lvl := "mc4"]
#    setnames(mc4, c("mc4_mthd", "mc4_mthd_id"), c("mthd", "mthd_id"))
#    ## Level 5
#    mc5 <- tcplMthdList(5, 'mc')
#    mc5[, lvl := "mc5"]
#    setnames(mc5, c("mc5_mthd", "mc5_mthd_id"), c("mthd", "mthd_id"))
#    # Compile the Output
#    mthd.list <- rbind(sc1, sc2, mc2, mc3, mc4, mc5)
#    mthd.list <- mthd.list[, c("lvl", "mthd_id", "mthd", "desc")]
#    # Return the Results
#    return(mthd.list)
#  }
#  
#  # Run the 'method_list' functions and store output.
#  amthds <- method_list()
#  # Print the available methods list.
#  amthds

## ----sc0, eval = FALSE--------------------------------------------------------
#  # Load Level 0 single concentration data for a single acid to R.
#  sc0 <- tcplLoadData(lvl=0, # data level
#                      fld="acid", # field to query on
#                      val=1, # value for each field
#                             # values should match their corresponding 'fld'
#                      type = "sc") # data type - single concentration
#  
#  # Alternatively, load data in and format with tcplPrepOtpt.
#  sc0 <- tcplPrepOtpt(tcplLoadData(lvl=0, fld="acid", val=1, type = "sc"))

## ----sc0_data_ex,warning=FALSE,message=FALSE----------------------------------
# Load the example data from the package.
data(sc_vignette,package = 'tcpl')
# Save the single concentration level 0 data in the 'sc0' object.
sc0 <- sc_vignette[["sc0"]]
# Print the first 6 rows of the data.
head(sc0) %>%
  # format output into a table
  kbl() %>%
  # format the output rendering to allow horizontal scrolling
  scroll_box(width = "100%") %>% 
  # reduce the size of the table text to improve readability
  kable_styling(font_size = 10)

## ----mc0, eval = FALSE--------------------------------------------------------
#  # Load Level 0 multiple concentration data.
#  mc0 <- tcplPrepOtpt(
#    tcplLoadData(lvl=0, # data level
#                 fld="acid", # field to query on
#                 val=1, # value for each field
#                        # values should match their corresponding 'fld'
#                 type = "mc") # data type - multiple concentrations
#  )

## ----mc0_data_ex,warning=FALSE,message=FALSE----------------------------------
# Load the example data from the package.
data(mc_vignette,package = 'tcpl')
# Save the multiple concentration level 0 data in the 'mc0' object.
mc0 <- mc_vignette[["mc0"]]
# Print the first 6 rows of the data.
head(mc0) %>%
  # format output into a table
  kbl() %>%
  # format the output rendering to allow horizontal scrolling
  scroll_box(width = "100%") %>% 
  # reduce the size of the table text to improve readability
  kable_styling(font_size = 10)

## ----mc0_aq, eval = FALSE-----------------------------------------------------
#  # Create a function to review assay quality metrics using indexed Level 0 data.
#  aq <- function(ac){
#    # obtain level 1 multiple concentration data for specified acids
#    dat <- tcplPrepOtpt(tcplLoadData(1L, "acid", aeids$acid, type="mc"))
#  
#    # keep only observations with good well quality (wllq = 1)
#    dat <- dat[wllq==1]
#  
#    # obtain summary values for data and remove missing data (i.e. NA's)
#    agg <- dat[ ,
#                list(
#                  # median response values (rval) of neutral wells (wllt = n)
#                  nmed = median(rval[wllt=="n"], na.rm=TRUE),
#                  # median absolute deviation (mad) of neutral wells (wllt = n)
#                  nmad = mad(rval[wllt=="n"], na.rm=TRUE),
#                  # median response values of positive control wells (wllt = p)
#                  pmed = median(rval[wllt=="p"], na.rm=TRUE),
#                  # median absolute deviation of positive control wells (wllt = p)
#                  pmad = mad(rval[wllt=="p"], na.rm=TRUE),
#                  # median response values of negative control wells (wllt = m)
#                  mmed = median(rval[wllt=="m"], na.rm=TRUE),
#                  # median absolute deviation of negative control wells (wllt = m)
#                  mmad = mad(rval[wllt=="m"], na.rm=TRUE)
#                  ),
#                # aggregate on assay component id, assay component name,
#                # and assay plate id
#                by = list(acid, acnm, apid)]
#  
#    # Z prime factor: separation between positive and negative controls,
#    # indicative of likelihood of false positives or negatives.
#    # - Between 0.5 - 1 are excellent,
#    # - Between 0 and 0.5 may be acceptable,
#    # - Less than 0 not good
#    # obtain the z-prime factor for positive controls and neutral
#    agg[ , zprm.p := 1 - ((3 * (pmad + nmad)) / abs(pmed - nmed))]
#    # obtain the z-prime factor for negative controls and neutral
#    agg[ , zprm.m := 1 - ((3 * (mmad + nmad)) / abs(mmed - nmed))]
#  
#    agg[ , ssmd.p := (pmed - nmed) / sqrt(pmad^2 + nmad^2 )]
#    agg[ , ssmd.m := (mmed - nmed) / sqrt(mmad^2 + nmad^2 )]
#  
#    # Coefficient of Variation (cv) of neutral control
#    # - Ideally should be under 25%
#    agg[ , cv     := nmad / nmed]
#  
#    agg[ , sn.p :=  (pmed - nmed) / nmad]
#    agg[ , sn.m :=  (mmed - nmed) / nmad]
#    agg[ , sb.p :=  pmed / nmed]
#    agg[ , sb.m :=  mmed / nmed]
#  
#    agg[zprm.p<0, zprm.p := 0]
#    agg[zprm.m<0, zprm.m := 0]
#  
#    acqu <- agg[ , list( nmed   = signif(median(nmed, na.rm = TRUE)),
#                         nmad   = signif(median(nmad, na.rm = TRUE)),
#                         pmed   = signif(median(pmed, na.rm = TRUE)),
#                         pmad   = signif(median(pmad, na.rm = TRUE)),
#                         mmed   = signif(median(mmed, na.rm = TRUE)),
#                         mmad   = signif(median(mmad, na.rm = TRUE)),
#                         zprm.p = round(median(zprm.p, na.rm=TRUE),2),
#                         zprm.m = round(median(zprm.m, na.rm=TRUE),2),
#                         ssmd.p = round(median(ssmd.p, na.rm=TRUE),0),
#                         ssmd.m = round(median(ssmd.m, na.rm=TRUE),0),
#                         cv = round(median(cv, na.rm=TRUE),2),
#                         sn.p = round(median(sn.p, na.rm=TRUE),2),
#                         sn.m = round(median(sn.m, na.rm=TRUE),2),
#                         sb.p = round(median(sb.p, na.rm=TRUE),2),
#                         sb.m = round(median(sb.m, na.rm=TRUE),2)
#    ), by = list(acid, acnm)]
#    # Return the Results.
#    return(acqu)
#  } #per acid
#  
#  # Run the 'aq' function & store the output.
#  assayq <- aq(ac)
#  # Print the first 6 rows of the assay quality results.
#  head(assayq)

## ----sc2, eval = FALSE--------------------------------------------------------
#  # Load Level 2 single concentration data for a single aeid.
#  sc2 <- tcplPrepOtpt(
#    tcplLoadData(lvl=2, # data level
#                 fld="aeid", # id field to query on
#                 val=3, # value for the id field
#                 type = "sc") # data type - single concentration
#  )
#  # Alternatively, data for a set of aeids can be loaded with a vector of ids.
#  sc2 <- tcplPrepOtpt(
#    tcplLoadData(lvl=2, fld="aeid", val=aeids$aeid, type = "sc")
#  )

## ----sc2_mthd, eval = FALSE---------------------------------------------------
#  # Create a function to load methods for single concentration data processing
#  # steps for given aeids.
#  sc_methods <- function(aeids) {
#    # load the level 1 methods assigned for the single concentration aeid's
#    sc1_mthds <- tcplMthdLoad(lvl=1, type ="sc", id=aeids$aeid)
#    # aggregate the method id's by aeid
#    sc1_mthds<- aggregate(mthd_id ~ aeid, sc1_mthds, toString)
#    # reset the names of the sc1_mthds object
#    setnames(sc1_mthds, "mthd_id", "sc1_mthd_id")
#  
#    # load the level 2 methods assigned for the single concentration aeid's
#    sc2_mthds <- tcplMthdLoad(lvl=2, type ="sc", id=aeids$aeid)
#    # aggregate the method id's by aeid
#    sc2_mthds<- aggregate(mthd_id ~ aeid, sc2_mthds, toString)
#    # reset the names of the sc2_mthds object
#    setnames(sc2_mthds, "mthd_id", "sc2_mthd_id")
#  
#    # Compile the Output
#    methods <- merge( merge(aeids, sc1_mthds,  by = "aeid", all = TRUE),
#                    sc2_mthds, by = "aeid", all = TRUE )
#    # Return the Results
#    return(methods)
#  }
#  
#  # Run the 'sc_methods' function and store the output.
#  smthds <- sc_methods(aeids)
#  
#  # Print the assigned sc methods.
#  smthds

## ----mc5_data, eval = FALSE---------------------------------------------------
#  # Load Level 5 MC data summary values for a set of aeids.
#  # (NOTE: As before, the user can obtain data for individual aeids.)
#  mc5 <- tcplPrepOtpt(
#    tcplLoadData(lvl=5, # data level
#                 fld="aeid", # fields to query on
#                 val=aeids$aeid, # value for each field
#                                 # values should match their corresponding 'fld'
#                 type = "mc") # data type - MC
#  )
#  
#  # For tcpl v3.0.0 and future releases, to output mc5_param information with
#  # the default mc5 results then 'add.fld' must be set to TRUE.
#  # (NOTE: Default for add.fld is FALSE, unless otherwise specified.)
#  mc5 <- tcplPrepOtpt(
#    tcplLoadData(lvl=5, # data level
#                 fld="aeid", # fields to query on
#                 val=aeids$aeid, # value for each field
#                                 # values should match their corresponding 'fld'
#                 type = "mc", # data type - multiple concentration
#                 add.fld=TRUE) # return additional parameters from mc5_param
#    )

## ----mc5_methods, eval = FALSE------------------------------------------------
#  # Create a function to load methods for MC data processing
#  # for select aeids.
#  mc_methods <- function(aeids) {
#    # acid
#    ## load the methods assigned to level 2 for given acids
#    mc2_mthds <- tcplMthdLoad(2,aeids$acid)
#    ## aggregate the assigned methods by acid
#    mc2_mthds<- aggregate(mthd_id ~ acid, mc2_mthds, toString)
#    ## rename the columns for the 'mc2_mthds' object
#    setnames(mc2_mthds, "mthd_id", "mc2_mthd_id")
#  
#    # aeid
#    ## load the methods assigned to level 3 for given aeids
#    mc3_mthds <- tcplMthdLoad(3,aeids$aeid)
#    ## aggregate the assigned methods by aeid
#    mc3_mthds<- aggregate(mthd_id ~ aeid, mc3_mthds, toString)
#    ## rename the columns for the 'mc3_mthds' object
#    setnames(mc3_mthds, "mthd_id", "mc3_mthd_id")
#    ## load the methods assigned to level 4 for given aeids
#    mc4_mthds <- tcplMthdLoad(4,aeids$aeid)
#    ## aggregate the assigned methods by aeid
#    mc4_mthds<- aggregate(mthd_id ~ aeid, mc4_mthds, toString)
#    ## rename the columns for 'mc4_mthds' object
#    setnames(mc4_mthds, "mthd_id", "mc4_mthd_id")
#    ## load the methods assigned to level 5 for given aeids
#    mc5_mthds <- tcplMthdLoad(5,aeids$aeid)
#    ## aggregate the assigned methods by aeid
#    mc5_mthds<- aggregate(mthd_id ~ aeid, mc5_mthds, toString)
#    ## rename the columns for 'mc5_mthds' object
#    setnames(mc5_mthds, "mthd_id", "mc5_mthd_id")
#  
#    # Compile the Results.
#    ## merge the aeid information with the level 2 methods by acid
#    acid.methods <- merge(aeids, mc2_mthds,by.x = "acid", by.y = "acid")
#    ## merge the level 3, 4, and 5 methods by aeid
#    mthd35 <- merge(
#      merge(mc3_mthds, mc4_mthds, by = "aeid", all = TRUE),
#      mc5_mthds, by = "aeid", all = TRUE
#      )
#    ## merge all methods information by aeid
#    methods <- merge(acid.methods, mthd35,by.x = "aeid", by.y = "aeid")
#    # Print the Results.
#    print(methods)
#    # Return the Results.
#    return(methods)
#  }
#  
#  # Run the 'methods' function and store the output.
#  mmthds <- mc_methods(aeids)
#  
#  # Print the assigned mc methods.
#  mmthds

## ----mc_plot_pdf_aeid, eval = FALSE-------------------------------------------
#  # Plot Level 5 MC data for aeids 3157-3159 and outputs plots separate pdfs by aeid.
#  tcplPlot(lvl = 5, # data level
#           fld = "aeid", # field to query on
#           val = 3157:3159, # values must be listed for each corresponding 'fld'
#           by = "aeid", # parameter to divide files
#           multi = TRUE, # multiple plots per page - output 4 per page
#           verbose = TRUE, # output all details if TRUE
#           output = "pdf") # output as pdf
#  
#  # Loading required mc_vignette data for example below
#  data(mc_vignette, package = 'tcpl')
#  mc5 <- mc_vignette[["mc5"]]
#  
#  # Plot Level 5 MC data from the mc_vignette R data object for a single aeid 80 and
#  # spids "TP0001652B01", 01504209", "TP0001652D01", "TP0001652A01", and "1210314466"
#  tcplPlot(lvl = 5, # data level
#           fld = c("aeid", "spid"), # field to query on
#           val = list(mc5$aeid, mc5$spid), # values must be listed for each corresponding 'fld'
#           by = "aeid", # parameter to divide files
#           multi = TRUE, # multiple plots per page - output 4 per page
#           verbose = TRUE, # output all details
#           output = "pdf", # output as pdf
#           fileprefix = "output_pdf") # prefix of the filename

## ----mc_plot_jpg, eval = FALSE------------------------------------------------
#  # Plot a verbose plot of Level 5 MC data for single aeid 80 and spid 01504209 and
#  # output as jpg.
#  tcplPlot(lvl = 5, # data level
#           fld = c('aeid','spid'), # field to query on
#           val = list(80,'01504209'), # values must be listed for each corresponding 'fld'
#           # values should match their corresponding 'fld'
#           multi = FALSE, # single plot per page
#           verbose = TRUE, # output all details
#           output = "jpg", # output as jpg
#           fileprefix = "output_jpg")

## ----mc_plot_console, eval = FALSE--------------------------------------------
#  # Create Level 4 plot for a single m4id.
#  tcplPlot(lvl = 4,  # data level
#           fld = "m4id", # field to query on
#           val = 482273, # values must be listed for each corresponding 'fld'
#           multi = FALSE, # single plot
#           verbose = FALSE, # do not output all details
#           output = "console") # output in R console
#  
#  # Plot of Level 5 MC data for single aeid (80) and spid (01504209)
#  # and output to console.
#  tcplPlot(lvl = 5, # data level
#           fld = c('aeid','spid'), # field to query on
#           val = list(80, '01504209'), # values must be listed for each corresponding 'fld'
#           multi = FALSE, # single plot
#           verbose = FALSE, # do not output all details
#           output = "console") # output in R console

## ----BPA, eval = FALSE--------------------------------------------------------
#  # Provide the chemical name and assign to 'chnm'.
#  chnm <- 'Bisphenol A'
#  # Load the chemical data from the database.
#  chem <- tcplLoadChem(field = 'chnm',val = chnm)
#  # Load mc5 data from the database for the specified chemical.
#  BPA.mc5 <- tcplLoadData(lvl = 5, # data level
#                          fld = 'spid', # field to query on
#                          val = chem[,spid], # value for each field (fld)
#                          type = 'mc') # data type - MC

## ----spid_plot, eval=FALSE----------------------------------------------------
#  # Load Level 5 multiple concentration data summary values for select aeids.
#  mc5 <- tcplPrepOtpt(
#    tcplLoadData(lvl=5, # data level
#                 fld='aeid', # id field to query on
#                 val=tcplLoadAeid(fld="asid",val = 25)$aeid, # value for each field
#                 type='mc', # data type - MC
#                 add.fld=TRUE) # return additional parameters from mc5_param
#    )
#  
#  # Identify sample subset.
#  spid.mc5 <- mc5[spid %in% c("EPAPLT0018N08", "EPAPLT0023A16", "EPAPLT0020C11", 	
#                              "EPAPLT0018B13","EPAPLT0018B14","EPAPLT0018B15"),]
#  
#  # Plot by endpoint for sample subset.
#  tcplPlot(lvl = 5, # data level
#           fld = c("spid","aeid"), # fields to query on
#           val = list( # value for each field, must be same order as 'fld'
#             spid.mc5$spid, # sample id's
#             spid.mc5$aeid  # assay endpoint id's
#             ),
#           by = "aeid", # parameter to divide files
#           multi = TRUE, # multiple plots per page - output 6 per page if TRUE
#           verbose = TRUE, # output all details if TRUE
#           output = "pdf", # output as pdf
#           fileprefix = "output/upitt") # prefix of the filename

