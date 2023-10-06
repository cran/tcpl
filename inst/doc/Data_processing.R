## ----eval = TRUE, message = FALSE,warning=FALSE-------------------------------
# Primary Packages #
library(tcpl)
library(tcplfit2)
# Data Formatting Packages #
library(data.table)
library(dplyr)
library(magrittr)
library(reshape2)
# Plotting Packages #
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(colorspace)
library(viridis)
# Table Packages #
library(htmlTable)
library(kableExtra)

## ----eval = TRUE, echo = FALSE, message = FALSE, results = "hide"-------------
# This chunk copies the tcplLite local directory to the temp directory used in installation
# to comply with CRAN policies on not writing to the installation directory
tmpdir <- tempdir()
#dbfile <- file.path(system.file(package = "tcpl"), "csv")
#file.copy(from = dbfile, tmpdir, recursive  = TRUE)
dbfile_temp <- file.path(tmpdir, "csv")
dir.create(dbfile_temp, showWarnings = F)
tcplConf(db = dbfile_temp, drvr = 'tcplLite')


## ----eval = TRUE--------------------------------------------------------------
## Obtain the Data ##
# Load the 'chdat' data from the package.
data(chdat, package = "tcpl")
# Convert 'chdat' object to a data.table.
setDT(chdat)
# View the first 6 rows of the table.
head(chdat)

## ----eval = TRUE, message = FALSE, warning=FALSE------------------------------
## Register the Chemicals ##
# Obtain chemicals already registered in the database.
cmap <- tcplLoadChem()
# Find chemicals in 'chdat' that are not registered yet.
chdat.register <- chdat[!(chdat$code %in% cmap$code)] 
# Register the chemicals not yet in the database.
tcplRegister(what = "chid", 
             flds = chdat.register[,
                        unique(.SD),
                        .SDcols = c("casn", "chnm", "dsstox_substance_id", "code", "chid")])

## ----eval = TRUE, message = FALSE---------------------------------------------
## Register Sample IDs (spids) ##
tcplRegister(what = "spid",
             flds = merge(chdat[ , list(spid, casn)],
                          chdat.register[ , list(casn, chid)],
                          by = "casn")[ , list(spid, chid)])

## ----eval = FALSE-------------------------------------------------------------
#  ## Register Chemical Libraries ##
#  # Subdivide even numbered chemicals in group 1.
#  grp1 <- cmap[chid %% 2 == 0, unique(chid)]
#  # Subdivide odd numbered chemicals in group 2.
#  grp2 <- cmap[chid %% 2 == 1, unique(chid)]
#  # Register the group 1 chemicals.
#  tcplRegister(what = "clib",
#               flds = list(clib = "group_1", chid = grp1))
#  # Register the group 2 chemicals.
#  tcplRegister(what = "clib",
#               flds = list(clib = "group_2", chid = grp2))

## ----eval = FALSE-------------------------------------------------------------
#  tcplRegister(what = "clib",
#               flds = list(clib = "other", chid = 1:2))
#  tcplLoadClib(field = "chid", val = 1:2)

## ----eval = FALSE-------------------------------------------------------------
#  ## Obtain the Data ##
#  # Load the multi-concentration data from the package.
#  data(mcdat, package = 'tcpl')
#  # Convert 'mcdat' to a data.table.
#  setDT(mcdat)

## ----eval = FALSE, message = FALSE--------------------------------------------
#  # Write/load the 'mcdat' into the database.
#  tcplWriteLvl0(dat=mcdat, type ="mc")

## ----eval = FALSE, message = FALSE--------------------------------------------
#  # Load the level 0 data from the database to R.
#  tcplLoadData(lvl=0, fld="acid", val=1, type = "mc")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Type <- c('SC', 'SC', 'MC', 'MC', 'MC', 'MC', 'MC', 'MC')
Level <- c('Lvl1', 'Lvl2', 'Lvl1', 'Lvl2', 'Lvl3', 'Lvl4', 'Lvl5', 'Lvl6')
InputID <- c('acid', 'aeid', 'acid', 'acid', 'acid', 'aeid', 'aeid', 'aeid')
MethodID <- c('aeid', 'aeid', 'N/A', 'acid', 'aeid', 'N/A', 'aeid', 'aeid')
Table <- data.frame(Type, Level, InputID, MethodID)

htmlTable(Table,
         rnames = FALSE ,
          caption="Table 1: Processing checklist.",
          tfoot = "The Input ID column indicates the ID used for each processing step; Method ID indicates the ID used for assigning methods for data processing, when necessary. SC = single-concentration; MC = multiple-concentration. Level 6 processing will not be possible in  tcpl v3.0, but may be included in future versions.")
          

## ----eval= FALSE--------------------------------------------------------------
#  ## Methods Assignment ##
#  # For illustrative purposes, assign level 2 MC methods to ACIDs 97, 98, and 99.
#  # First check for available methods.
#  mthds <- tcplMthdList(lvl = 2, type = "mc")
#  mthds[1:2]
#  # Assign some methods to ACID 97, 98, and 99.
#  tcplMthdAssign(lvl = 2,
#                 id = 97:99,
#                 mthd_id = c(3, 4, 2),
#                 ordr = 1:3,
#                 type = "mc")
#  # Check the assigned methods for ACID 97, 98, and 99 in the database.
#  tcplMthdLoad(lvl = 2, id = 97:99, type = "mc")
#  
#  # Methods can be cleared one at a time for the given id(s)
#  tcplMthdClear(lvl = 2, id = 99, mthd_id = 2, type = "mc")
#  # Check the assigned methods for the single id updated, namely ACID 99.
#  tcplMthdLoad(lvl = 2, id = 99, type = "mc")
#  
#  # Or all methods can be cleared for the given id(s)
#  tcplMthdClear(lvl = 2, id = 97:98, type = "mc")
#  # Check the assigned methods for the all updated ids, namely ACID 97 and 98.
#  tcplMthdLoad(lvl = 2, id = 97:98, type = "mc")

## ----warning = FALSE, echo = FALSE--------------------------------------------
output <- 
  matrix(c("1. bval.apid.nwlls.med", "2. resp.fc", "1. bval.apid.lowconc.med", "2. bval.apid.pwlls.med",
"3. resp.log2", "4. resp.mult.neg1", "3. resp.pc", "4. resp.multneg1 ", 
"1. bval.apid.lowconc.med", "2. resp.fc", "1. bval.spid.lowconc.med", "2. pval.apid.mwlls.med", 
"3. resp.log2", "4. \t", "3. resp.pc", "4. \t" ,
"1. none", "2. resp.log10", "1. none", "2. resp.multneg1",
"3. resp.blineshift.50.spid", "4. \t", "3. \t", "4. \t"), 
         ncol=4, byrow = TRUE)

htmlTable(output,
          rnames = FALSE,
          rgroup = c("Scheme 1",
                     "Scheme 2", "Scheme 3"),
          n.rgroup = c(2,2),
          cgroup = c("Fold-Change", "\\%Control"),
          n.cgroup = c(2,2), 
          caption="Table 2: Example normalization method assignments.")
          

## ----warning = FALSE, echo = FALSE--------------------------------------------
Level <- c(" Lvl 0", "Lvl 1  ", "Lvl 2  ")
Description <- c("Pre-processing: Vendor/dataset-specific pre-processing to organize heterogeneous raw data to the uniform format for processing by the *tcpl* package&dagger;",
                 "Normalize: Apply assay endpoint-specific normalization listed in the \'sc1_aeid\' table to the raw data to define response",
                 "Activity Call: Collapse replicates by median response, define the response cutoff based on methods in the \'sc2_aeid\' table, and determine activity"
                 )

output <- 
  data.frame(Level, Description)

htmlTable(output,
        
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
      
        align = 'l',
        align.header = 'l',
        caption="Table 3: Summary of the tcpl single-concentration pipeline.",
        tfoot="&dagger; Level 0 pre-processing occurs outside the package and specifics are covered in the Introduction vignette.")


## ----eval = FALSE-------------------------------------------------------------
#  # Load the 'aeid' values for 'acid = 2'.
#  tcplLoadAeid(fld = "acid", val = 2)

## ----eval = FALSE-------------------------------------------------------------
#  # Assign the level 1 methods to AEID 1 & 2.
#  tcplMthdAssign(lvl = 1,  # processing level
#                 id = 1:2, # assay endpoint ID's to assign methods
#                 mthd_id = c(1, 11, 13), # method(s) to be assigned
#                 ordr = 1:3, # order the method(s) should be applied
#                 type = "sc") # the data/processing type

## ----eval = FALSE-------------------------------------------------------------
#  # Assign a fourth step to the normalization processing - for AEID 2 only.
#  tcplMthdAssign(lvl = 1, # processing level
#                 id = 2, # assay endpoint ID's to assign methods
#                 mthd_id = 16, # method(s) to be assigned
#                 ordr = 1, # order the method(s) should be applied
#                 type = "sc") # the data/processing type

## ----echo=FALSE, eval = FALSE-------------------------------------------------
#  # With the assay endpoints and normalization methods defined, the data are ready for level 1 processing.
#  
#  ## Do level 1 processing for acid 1
#  sc1_res <- tcplRun(id = 1, slvl = 1, elvl = 1, type = "sc")

## ----warning = FALSE, echo = FALSE--------------------------------------------
## Create the sc BMAD calculation Table ##
# Specify column 1 in the table - Methods.
Method <- c(1,2)
# Specify column 2 in the table - Description.
Description <- c(
  "Median absolute deviation (MAD) of all treatment wells across the assay component (acid).",
  "Median absolute deviation (MAD) of all blank wells across the assay component (acid)."
)
# Specify column 3 in the table - Observations.
Observations <- c(
  "$y_{i} = y_{(s,w)}$", # method 1
  "$y_{i} = y_{(s,w)}$" # method 2
)
# Specify column 4 in the table - Observation ID.
ID <- c(
  "$s \\in \\{1,...,n_{acid}\\}$, \n$w = t$",
  "$s \\in \\{1,...,n_{acid}\\}$, \n$w = n$"
)
# Specify column 5 in the table - Details about the Observation ID.
Details <- c(
  "$s$ indicates the sample id within an 'acid' & $w$ indicates the well type",
  "$s$ indicates the sample id within an 'acid' & $w$ indicates the well type"
)
# Create the output table.
output <- 
  data.frame(Method,Description,Observations,ID,Details)
# Out put the table in an 'html' format.
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 4: Single concentration *baseline* estimation by method assignment in Level 2 processing."
)

## ----eval = FALSE-------------------------------------------------------------
#  # Assign a cutoff value of log2(1.2).
#  tcplMthdAssign(lvl = 2, # processing level
#                 id = 1,  # assay endpoint ID's to assign methods
#                 mthd_id = 3, # method(s) to be assigned
#                 type = "sc") # the data/processing type

## ----echo=FALSE, eval = FALSE-------------------------------------------------
#  # With the methods assigned and the cutoff set, the data are ready for level 2 processing.
#  
#  ## Do level 2 processing for acid 1
#  sc2_res <- tcplRun(id = 1, slvl = 2, elvl = 2, type = "sc")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Level <- c("Lvl 0 ", "Lvl 1", "Lvl 2", "Lvl 3", "Lvl 4", "Lvl 5", "Lvl 6")
Description <- c("Pre-processing: Vendor/dataset-specific pre-processing to organize heterogeneous raw data to the uniform format for processing by the *tcpl* package&dagger;",
                 "Index: Define the replicate and concentration indices to facilitate
all subsequent processing",
                 "Transform: Apply assay component (acid) specifc transformations
listed in the \'mc2_acid\' table to the raw data to define the
corrected data",
"Normalize: Apply assay endpoint (aeid) specifc normalization listed in
the \'mc3_aeid\' table to the corrected data to define response",
"Fit: Model the concentration-response data utilizing ten
objective curve-fitting functions from tcplfit2: (1) constant, (2) hill, (3) gain-loss, (4) polynomial-linear, (5) polynomial-quadratic, (6) power, (7) exponential-2, (8) exponential-3, (9) exponential-4, (10) exponential-5",
"Model Selection/Acitivty Call: Select the winning model, define
the response cutoff based on methods in the \'mc5_aeid\' table, and
determine activity",
"Flag: Flag potential false positive and false negative endings based
on methods in the \'mc6_aeid\' table&ddagger;"
                 )

output <- 
  data.frame(Level, Description)

htmlTable(output,
        
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        align = 'l',
        align.header = 'l',
        caption="Table 5: Summary of the tcpl multiple-concentration pipeline.",
        tfoot="&dagger; Level 0 pre-processing occurs outside the package and specifics are covered in Introduction vignette. \n&ddagger;Level 6 processing *will not* be possible in  tcpl v3.0, but may be included in future versions.")


## ----echo=FALSE, eval = FALSE, message = FALSE--------------------------------
#  # The following demonstrates how to carry out the level 1 processing and look at the resulting data:
#  
#  ## Do level 1 processing for acid 1
#  mc1_res <- tcplRun(id = 1, slvl = 1, elvl = 1, type = "mc")

## ----eval = FALSE-------------------------------------------------------------
#  ## Evaluate Level 1 Indexing ##
#  # Load the level 1 data from the database.
#  m1dat <- tcplLoadData(lvl = 1,
#                        fld = "acid",
#                        val = 1,
#                        type = "mc")
#  # Prepare the data into a readable format.
#  m1dat <- tcplPrepOtpt(m1dat)
#  # Sort the data based on the concentration and replicate inidices.
#  setkeyv(m1dat, c("repi", "cndx"))
#  # Display the 'cndx' and 'repi' values.
#  m1dat[chnm == "Bisphenol A",
#        list(chnm, conc, cndx, repi)]

## ----eval = FALSE, warning = FALSE, message = FALSE, fig.width = 30, fig.height= 20----
#  tcplPlotPlate(dat = m1dat, apid = "TP0001915")

## ----eval = FALSE, message = FALSE--------------------------------------------
#  ## Methods Assignment ##
#  # Assign the level 2 transformation method 'none' to ACID 1.
#  tcplMthdAssign(lvl = 2, # processing level
#                 id =1, # assay component ID's to assign methods
#                 mthd_id = 1, # method(s) to be assigned
#                 ordr = 1, # order of the method(s) should be assigned
#                 type = "mc") # the data/processing type

## ----echo=FALSE, eval = FALSE,  warning = FALSE, message = FALSE--------------
#  # With the method assigned, the processing can be completed.
#  
#  ## Do level 2 processing for acid 1
#  mc2_res <- tcplRun(id = 1, slvl = 2, elvl = 2, type = "mc")

## ----eval = FALSE-------------------------------------------------------------
#  # Look at the assay endpoints for acid 1.
#  tcplLoadAeid(fld = "acid", val = 1)

## ----eval = FALSE, message = FALSE--------------------------------------------
#  ## Methods Assignment ##
#  # Assign the baseline calculation and normalization methods to AEID's 1 & 2.
#  tcplMthdAssign(lvl = 3, # processing level
#                 id = 1:2, # assay endpoint ID to assign methods
#                 mthd_id = c(17, 9, 7), # method(s) to be assigned
#                 ordr = 1:3, # order the method(s) should be applied
#                 type = "mc") # the data/processing type

## ----echo=FALSE, eval = FALSE,warning = FALSE, message = FALSE----------------
#  # With the assay endpoints and normalization methods defined, the data are ready for level 3 processing.
#  
#  ## Do level 3 processing for acid 1
#  mc3_res <- tcplRun(id = 1, slvl = 3, elvl = 3, type = "mc")

## ----warning = FALSE, echo = FALSE--------------------------------------------
# First column with the method assignment index.
Method <- c(
  1,2,3
)
# Second column with the general methods description.
Description <- c(
  "Median absolute deviation (MAD) of all observations in the lowest two concentrations of across samples (spid) in the assay endpoint (aeid).",
  "Median absolute deviation (MAD) of all observations in the solvent/untreated control observations across samples (spid) in the assay endpoint (aeid).",
  "Standard deviation (SD) of all observations in the lowest two concentrations of across samples (spid) in the assay endpoint (aeid)."
)
# Third column with the observation information.
Observations <- c(
  "$y_{i} = y_{(s,w,d)}$", # method 1
  "$y_{i} = y_{(s,w)}$", # method 2
  "$y_{i} = y_{(s,w,d)}$" # method 3
)
# Fourth column with the observation ID information.
ID <- c(
  "$s \\in \\{1,...,n_{aeid}\\}$, \n$w = t$, \n$d \\in \\{ 1,2 \\}$",
  "$s \\in \\{1,...,n_{aeid}\\}$, \n$w = n$",
  "$s \\in \\{1,...,n_{aeid}\\}$,\n$w = t$,\n$d \\in \\{ 1,2 \\}$"
)
# Fifth column with the details on the ID's.
Details <- c(
  "$s$ indicates the sample id within an 'aeid', $w$ indicates the well type, & $d$ indicates the concentration group index",
  "$s$ indicates the sample id within an 'aeid', $w$ indicates the well type",
  "$s$ indicates the sample id within an 'aeid', $w$ indicates the well type, & $d$ indicates the concentration group index"
)
# Compile all of the information for the table.
output <- 
  data.frame(Method,Description,Observations,ID,Details)
# Export/print the table to an html rendered table.
htmlTable(output,
          align = 'l',
          align.header = 'l',
          rnames = FALSE  ,
          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
          caption="Table 6: Multiple concentration *baseline* variability estimation by method assignment in Level 4 processing."
          )

## ----eval=FALSE---------------------------------------------------------------
#  ## Evaluate the Level 3 Data ##
#  # Load the level 3 data from the database.
#  mc3 <- tcplLoadData(lvl = 3,
#                      type = 'mc',
#                      fld = 'aeid',
#                      val = 80)
#  # Prepare the data into a readable format.
#  mc3 <- tcplPrepOtpt(mc3)
#  # Display the data.
#  mc3

## ----message=FALSE------------------------------------------------------------
# Load the example data from the `tcpl` package.
data(mc_vignette,package = 'tcpl')

# Allocate the level 3 example data to `mc3`.
mc3 <- mc_vignette[['mc3']]

## ----fig.align='center',class.source="scroll-100",message=FALSE,fig.dim=c(8,10)----
## Obtain Data ##
# Obtain the level 4 example data.
mc4 <- mc_vignette[["mc4"]]
# Obtain the minimum response observed and the 'logc' group - 'resp_min'.
level3_min <- mc3 %>%
  dplyr::group_by(spid,chnm) %>% 
  dplyr::filter(resp == min(resp)) %>% 
  dplyr::filter(spid == "01504209")
# Obtain the maximum response observed and the 'logc' group - 'resp_max'.
level3_max <- mc3 %>% 
  dplyr::group_by(spid,chnm) %>% 
  dplyr::filter(resp == max(resp)) %>% 
  dplyr::filter(spid == "01504209")
# Obtain the level 3 data and 'center' estimates for responses per 'logc' group.
level3_summary <- mc3 %>%
  dplyr::filter(spid == "01504209") %>% 
  dplyr::select(.,c(spid,chnm,logc,resp)) %>%
  dplyr::group_by(spid,chnm,logc) %>% 
  dplyr::summarise(mean_resp = mean(resp),med_resp = median(resp))
## Generate Individual Summary Plots ##
# Plot the mean responses for each log-concentration group.
A <- mc3 %>%
  dplyr::filter(spid == "01504209") %>% 
  ggplot(data = .,aes(logc,resp))+
  geom_point(pch = 1,size = 2)+
  geom_point(data = level3_summary,
             aes(x = logc,y = mean_resp,
                 col = 'mean responses'),
             alpha = 0.75,size = 2)+
  scale_color_manual(values = 'paleturquoise3',
                     aesthetics = 'col')+
  labs(lty = "",colour = "")+
  xlab(expression(paste(log[10],"(Concentration) ",mu,"M")))+
  ylab(expression(paste(log[2],"(Fold Induction)")))+
  ggtitle("Mean Responses")+
  theme_bw()+
  theme(legend.position = 'bottom')
# Plot the median responses for each log-concentration group.
B <- mc3 %>%
  dplyr::filter(spid == "01504209") %>% 
  ggplot(data = .,aes(logc,resp))+
  geom_point(pch = 1,size = 2)+
  geom_point(data = level3_summary,
             aes(x = logc,y = med_resp,
                 col = 'median response'),
             alpha = 0.75,size = 2)+
  scale_color_manual(values = 'hotpink',
                     aesthetics = 'col')+
  labs(lty = "",colour = "")+
  xlab(expression(paste(log[10],"(Concentration) ",mu,"M")))+
  ylab(expression(paste(log[2],"(Fold Induction)")))+
  ggtitle("Median Responses")+
  theme_bw()+
  theme(legend.position = 'bottom')
# Plot the maximum mean & median responses at the related log-concentration -
#   'max_mean' & 'max_mean_conc'.
C <- mc3 %>%
  dplyr::filter(spid == "01504209") %>% 
  ggplot(data = .,aes(logc,resp))+
  geom_point(pch = 1,size = 2)+
  geom_point(data = dplyr::filter(mc4,spid == "01504209"),
             aes(x = max_mean_conc,y = max_mean,
                 col = 'maximum mean response'),
             alpha = 0.75,size = 2)+
  scale_color_manual(values = 'paleturquoise3',
                     aesthetics = 'col')+
  labs(lty = "",colour = "")+
  xlab(expression(paste(log[10],"(Concentration) ",mu,"M")))+
  ylab(expression(paste(log[2],"(Fold Induction)")))+
  ggtitle(label = "Maximum Mean Response")+
  theme_bw()+
  theme(legend.position = 'bottom')
# Plot the maximum mean & median responses at the related log-concentration -
#   'max_med' & 'max_med_conc'.
D <- mc3 %>%
  dplyr::filter(spid == "01504209") %>% 
  ggplot(data = .,aes(logc,resp))+
  geom_point(pch = 1,size = 2)+
  geom_point(data = dplyr::filter(mc4,spid == "01504209"),
             aes(x = max_med_conc,y = max_med,
                 col = "maximum median response"),
             alpha = 0.75,size = 2)+
  scale_color_manual(values = 'hotpink',
                     aesthetics = 'col')+
  labs(lty = "",colour = "")+
  xlab(expression(paste(log[10],"(Concentration) ",mu,"M")))+
  ylab(expression(paste(log[2],"(Fold Induction)")))+
  ggtitle(label = "Maximum Median Response")+
  theme_bw()+
  theme(legend.position = 'bottom')
# Plot the minimum & maximum observed responses.
E <- mc3 %>%
  dplyr::filter(spid == "01504209") %>% 
  ggplot(data = .,aes(logc,resp))+
  geom_point(pch = 1,size = 2)+
  geom_point(data = level3_min,
             aes(x = logc,y = resp,
                 col = "minimum response"),
             alpha = 0.75,size = 2)+
  geom_point(data = level3_max,
             aes(x = logc,y = resp,
                 col = "maximum response"),
             alpha = 0.75,size = 2)+
  scale_color_manual(values = c('red','blue'),
                     aesthetics = 'col')+
  labs(lty = "",colour = "")+
  xlab(expression(paste(log[10],"(Concentration) ",mu,"M")))+
  ylab(expression(paste(log[2],"(Fold Induction)")))+
  ggtitle(label = "Minimum & Maximum\nResponses")+
  theme_bw()+
  theme(legend.position = 'bottom')
# Plot the minimum & maximum experimental log-concentration groups -
#   'logc_min' & 'logc_max'.
G <- mc3 %>%
  dplyr::filter(spid == "01504209") %>% 
  ggplot(data = .,aes(logc,resp))+
  geom_point(pch = 1,size = 2)+
  geom_vline(data = dplyr::filter(mc4,spid == "01504209"),
             aes(xintercept = logc_min,
                 col = 'minimum concentration'),
             lty = "dashed")+
  geom_vline(data = dplyr::filter(mc4,spid == "01504209"),
             aes(xintercept = logc_max,
                 col = 'maximum concentration'),
             lty = "dashed")+
  scale_color_manual(values = c('red','blue'),
                     aesthetics = 'col')+
  labs(lty = "",colour = "")+
  xlab(expression(paste(log[10],"(Concentration) ",mu,"M")))+
  ylab(expression(paste(log[2],"(Fold Induction)")))+
  ggtitle(label = "Minimum & Maximum\nConcentrations")+
  theme_bw()+
  theme(legend.position = 'bottom')
## Compile Summary Plots in One Figure ##
gridExtra::grid.arrange(
  A,B,C,D,E,G,
  nrow = 3,ncol = 2,
  top = mc3[which(mc4[,spid] == "01504209"),aenm]
)

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  # Randomly selecting ATG AEID's.
#  atg.aeid <- tcplLoadAeid(fld = 'asid',val = 3) %>%
#    dplyr::filter(grepl(aenm,pattern = "_up$"))
#  atg.aeid
#  set.seed(1629)
#  sampid <- sample(atg.aeid[,aeid],size = 3,replace = FALSE)

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  # Load mc3 data
#  mc3 <- tcpl::tcplPrepOtpt(
#    tcpl::tcplLoadData(
#      lvl = 3L,
#      type = 'mc',
#      fld = 'aeid',
#      val = sampid
#    )
#  )

## ----warning = FALSE, echo = FALSE--------------------------------------------
# First column - tcplfit2 available models.
Model <- c(
  "Constant", "Linear", "Quadratic","Power", "Hill", "Gain-Loss",
  "Exponential 2", "Exponential 3","Exponential 4", "Exponential 5"
)
# Second column - model abbreviations used in invitrodb & tcplfit2.
Abbreviation <- c(
  "cnst", "poly1", "poly2","pow", "hill", "gnls",
  "exp2", "exp3", "exp4", "exp5"
)
# Third column - model equations.
Equations <- c(
  "$f(x) = 0$", # constant
  "$f(x) = ax$", # linear
  "$f(x) = a(\\frac{x}{b}+(\\frac{x}{b})^{2})$", # quadratic
  "$f(x) = ax^p$", # power
  "$f(x) = \\frac{tp}{1 + (\\frac{ga}{x})^{p}}$", # hill
  "$f(x) = \\frac{tp}{(1 + (\\frac{ga}{x})^{p} )(1 + (\\frac{x}{la})^{q} )}$", # gain-loss
  "$f(x) = a*(exp(\\frac{x}{b}) - 1)$", # exp 2
  "$f(x) = a*(exp((\\frac{x}{b})^{p}) - 1)$", # exp 3
  "$f(x) = tp*(1-2^{\\frac{-x}{ga}})$", # exp 4
  "$f(x) = tp*(1-2^{-(\\frac{x}{ga})^{p}})$" # exp 5
)
# Fourth column - model parameter descriptions.
OutputParameters <- c(
  "", # constant
  "a (y-scale)", # linear,
  "a (y-scale), b (x-scale)", # quadratic
  "a (y-scale), p (power)", # power
  "tp (top), ga (gain AC50), p (gain-power)", # hill
  "tp (top), ga (gain AC50), p (gain power), la (loss AC50), q (loss power)", # gain-loss
  "a (y-scale), b (x-scale)", # exp2
  "a (y-scale), b (x-scale), p (power)", # exp3
  "tp (top), ga (AC50)", # exp4
  "tp (top), ga (AC50), p (power)" # exp5
)
# Fifth column - additional model details.
Details <- c(
  "Parameters always equals 'er'.", # constant
  "", # linear 
  "", # quadratic
  "", # power
  "Concentrations are converted internally to log10 units and optimized with f(x) = tp/(1 + 10^(p*(gax))), then ga and ga_sd are converted back to regular units before returning.", # hill
  "Concentrations are converted internally to log10 units and optimized with f(x) = tp/[(1 + 10^(p*(gax)))(1 + 10^(q*(x-la)))], then ga, la, ga_sd, and la_sd are converted back to regular units before returning." , # gain-loss
  "", # exp2
  "", # exp3
  "", # exp4
  "") # exp5
# Consolidate all columns into a table.
output <- 
  data.frame(Model, Abbreviation, Equations,
             OutputParameters, Details)
# Export/print the table into an html rendered table.
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 7: *tcplfit2* model details.",
        tfoot = "Model descriptions are pulled from tcplFit2 manual at <https://cran.r-project.org/package=tcplfit2/tcplfit2.pdf>."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
# First column - tcplfit2 additional fit parameters.
FitParameters <- c(
  "er", "success", "cov", "aic", "rme", "modl",
  "parameters", "parameters sds", "pars", "sds"
)
# Second column - description of additional fit parameters.
Description <- c(
  "Error term","Success of Fit/Model Convergenece","Success of Covariance",
  "Aikaike Information Criteria", "Root Mean Squared Error",
  "Vector of Model Estimated Values at Given Concentrations",
  "Model Parameter Values", "Standard deviation of Model Parameter Values",
  "Vector of Parameter Names","Vectors of Parameter Standard Deviation Names"
)
# Consolidate all columns into a table.
output <- 
  data.frame(FitParameters, Description)
# Export/print the table into an html rendered table.
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        
          caption="Table 8: Model output descriptions." ,
          tfoot = "Model output descriptions are pulled from tcplFit2 manual at <https://cran.r-project.org/package=tcplfit2/tcplfit2.pdf>."
)

## ----class.source="scroll-100",fig.align='center'-----------------------------
## Example Data ##
# example fit concentration series
ex_conc <- seq(10^(min(mc3[,logc])),10^(max(mc3[,logc])),length.out = 100)

## Obtain the Continuous Fit of Level 4 Model Estimates ##
fits <- data.frame(
  # log-scale concentrations
  logc = log10(ex_conc),
  # parametric model fits from `tcplfit2`
  constant = tcplfit2::cnst(ps = c(er = 0.1),ex_conc),
  poly1 = tcplfit2::poly1(ps = c(a = 3.5,er = 0.1),x = ex_conc),
  poly2 = tcplfit2::poly2(ps = c(a = 0.13,b = 2,er = 0.1),x = ex_conc),
  power = tcplfit2::pow(ps = c(a = 1.23,p = 1.45,er = 0.1),x = ex_conc),
  hill = tcplfit2::hillfn(ps = c(tp = 750,ga = 5,p = 1.76,er = 0.1),
                          x = ex_conc),
  gnls = tcplfit2::gnls(ps = c(tp = 750,ga = 15,p = 1.45,la = 50,q = 1.34,
                               er = 0.1),
                        x = ex_conc),
  exp2 = tcplfit2::exp2(ps = c(a = 0.45,b = 13.5,er = 0.1),
                        x = ex_conc),
  exp3 = tcplfit2::exp3(ps = c(a = 1.67,b = 12.5,p = 0.87,er = 0.1),
                        x = ex_conc),
  exp4 = tcplfit2::exp4(ps = c(tp = 895,ga = 15,er = 0.1),x = ex_conc),
  exp5 = tcplfit2::exp5(ps = c(tp = 793,ga = 6.25,p = 1.25,er = 0.1),
                        x = ex_conc)
) %>% 
  reshape2::melt(data = .,measure.vars = c(
    "constant",
    "poly1","poly2","power",
    "hill","gnls","exp2","exp3","exp4","exp5"
  ))

## Updated Colors ##
fit_cols <-
  # choose 10 distinct colors
  viridis::magma(n = 10,direction = 1) %>% 
  # darken the original colors to make them more visible
  colorspace::darken(.,amount = 0.2)

## Plot ##
fits %>%
  ggplot()+
  geom_line(aes(x = logc,y = value,lty = variable,colour = variable))+
  facet_wrap(facets = "variable")+
  theme_bw()+
  labs(lty = "Models",colour = "Models")+
  scale_colour_manual(values = fit_cols)+
  ggtitle("General Shape of Models Included in `tcplfit2`")+
  xlab(expression(paste(log[10],"(Concentration) ",mu,"M")))+
  ylab("Response")

## ----eval=FALSE---------------------------------------------------------------
#  ## Methods Assignment ##
#  # Assign the Level 4 Processing Methods to AEID 80.
#  tcplMthdAssign(
#      lvl = 4, # processing level
#      id = 80, # assay endpoint ID(s) to assign method(s)
#      mthd_id = c(1,3), # method(s) to be assigned
#      ordr = 1:2, # order the method(s) should be applied
#      type = "mc") # the data/processing type

## ----eval=FALSE,echo=FALSE----------------------------------------------------
#  # We show two example of how a user can assign methods using <font face="CMTT10"> tcpl </font>.  First, we assign methods for a single assay endpoint.  Second, we assign methods for all assay endpoints within an assay.  Please note, the second approach can also be used for a subset of aeid's within an assay that is larger than one.
#  
#  ## Obtain the 'aeid' Values ##
#  # here we are pulling attagene aeid's
#  atg.aeid <- tcpl::tcplGetAeid(name = "ATG")
#  ## Assign the Level 4 Processing Methods for All AEID's ##
#  tcpl::tcplMthdAssign(
#      lvl = 4,
#      id = atg.aeid[,aeid],
#      mthd_id = c(1,3), # the set of methods to be assigned
#      ordr = 1:2, # order in which they should be assigned
#      type = "mc"
#    )

## ----echo=FALSE, eval=FALSE---------------------------------------------------
#  # With the methods assigned, the level 4 processing can be completed for the desired set of assay endpoints (aeid).
#  
#  tcpl::tcplRun(
#    id = atg.aeid[,aeid], # assay endpoint id to pipeline
#    slvl = 4L, # level to start pipelining on
#    elvl = 4L, # level to end pipelining on
#    type = 'mc' # endpoint processing type - 'mc' = "multiple concentrations"
#  )

## ----eval=FALSE---------------------------------------------------------------
#  ## Explore the Level 4 Data ##
#  # Load the level 4 data.
#  mc4 <- tcplLoadData(lvl = 4,
#                      type = 'mc',
#                      fld = 'aeid',
#                      val = 80,
#                      add.fld = TRUE)
#  # Prepare the data into a readable format.
#  mc4 <- tcplPrepOtpt(mc4)
#  # Display the data.
#  mc4

## -----------------------------------------------------------------------------
# Allocate the level 4 data in `mc_vignette` to the `mc4` object.
mc4 <- mc_vignette[["mc4"]]

## ----eval = FALSE,echo=FALSE--------------------------------------------------
#  ## Load the level 4 data
#  m4dat <- tcplPrepOtpt(tcplLoadData(lvl = 4, type = "mc"))
#  ## List the first m4ids where the hill model converged
#  ## for AEID 1
#  m4dat[hill == 1 & aeid == 1, head(m4id)]

## ----fig.align='center',fig.dim=c(8,5.5),class.source = "scroll-100"----------
## Create a Sequence of Concentration Values within Observed Range ##
X <- seq(
 10^(mc4[which(mc4[,spid] == "01504209"),logc_min]),
 10^(mc4[which(mc4[,spid] == "01504209"),logc_max]),
 length.out = 100
)
## Obtain the Continuous Fit of Level 4 Model Estimates ##
# Apply each model fit to continous concentration values (X) and estimated
# parameters from 'tcplfit2'.
estDR <- mc4 %>% 
  dplyr::filter(spid == "01504209") %>% 
  dplyr::summarise(
    cnst  = tcplfit2::cnst(.[,c(cnst_er)],X),
    poly1 = tcplfit2::poly1(.[,c(poly1_a,poly1_er)],X),
    poly2 = tcplfit2::poly2(.[,c(poly2_a,poly2_b,poly2_er)],X),
    power = tcplfit2::pow(.[,c(pow_a,pow_p,pow_er)],X),
    hill  = tcplfit2::hillfn(.[,c(hill_tp,hill_ga,hill_p)],X),
    gnls  = tcplfit2::gnls(.[,c(gnls_tp,gnls_ga,gnls_p,gnls_la,gnls_q,gnls_er)],
                           x = X),
    exp2  = tcplfit2::exp2(.[,c(exp2_a,exp2_b,exp2_er)],x = X),
    exp3  = tcplfit2::exp3(.[,c(exp3_a,exp3_b,exp3_p,exp3_er)],x = X),
    exp4  = tcplfit2::exp4(.[,c(exp4_tp,exp4_ga,exp4_er)],x = X),
    exp5  = tcplfit2::exp5(.[,c(exp5_tp,exp5_ga,exp5_p,exp5_er)],x = X)
  )
# Format data into a data.frame for ease of plotting.
estDR <- cbind.data.frame(X,estDR) %>%
  reshape2::melt(data = .,measure.vars = c(
    "cnst","poly1","poly2","power","hill","gnls","exp2","exp3","exp4","exp5"
  ))

## Updated Colors ##
fit_cols <-
  # choose 10 distinct colors
  viridis::magma(n = 10,direction = 1) %>% 
  # darken the original colors to make them more visible
  colorspace::darken(.,amount = 0.2)

## Plot the Model Fits from Level 4 ##
mc3 %>% 
  dplyr::filter(spid == "01504209") %>% 
  ggplot(.,aes(x = logc,y = resp))+
  geom_point(pch = 1,size = 2)+
  geom_line(data = estDR,
            aes(x = log10(X),y = value,colour = variable,lty = variable))+
  labs(colour = "Models",lty = "Models")+
  scale_colour_manual(values = fit_cols)+
  xlab(expression(paste(log[10],"(Concentration) ",mu,"M")))+
  ylab(expression(paste(log[2],"(Fold Induction)")))+# )+
  ggtitle(
    label = paste("Level 4 Model Fits",
                  mc4[which(mc4[,spid] == "01504209"),dsstox_substance_id],
                  sep = "\n"),
    subtitle = paste("Assay Endpoint: ",
                     mc4[which(mc4[,spid] == "01504209"),aenm]))+
  theme_bw()

## ----echo=FALSE---------------------------------------------------------------
# Obtain the AIC values from each of the model fits from
# the level 4 data.
mc4_aic <- mc4 %>% 
  dplyr::select(.,grep(colnames(.),pattern = "aic")) %>% 
  round(.,3) %>% 
  apply(.,MARGIN = 1,FUN = function(x){
    cell_spec(x,color = ifelse(x == min(x),
                               yes = "blue",
                               no = "black"))
  }) %>% t() %>% 
  data.frame() %>% 
  cbind.data.frame(mc4[,dsstox_substance_id],.)
# Rename the columns.
colnames(mc4_aic) <-
  colnames(mc4)[grep(colnames(mc4),pattern = "aic")] %>%
  stringr::str_remove(.,pattern = "_aic") %>% c("dsstox_id",.)
# Export/display the table in an HTML format.
mc4_aic %>%
  kbl(
    escape = FALSE,
    format = 'html',
    centering = TRUE,
    caption = "Table 9: AIC values from All Fits in Level 4.  Values highlighted in blue indicate the winning model for the compound.") %>%
  kable_styling(
    font_size = 14,
    c("striped","hover"),
    full_width = FALSE
  )

## ----eval=FALSE---------------------------------------------------------------
#  ## Methods Assignment ##
#  # Assign the Level 5 Processing Methods to AEID 80.
#  tcplMthdAssign(
#    lvl = 5, # processing level
#    id = 80, # assay endpoint ID(s) to assign method(s)
#    mthd_id = c(1,3,5), # method(s) to be assigned
#    ordr = 1:3, # order the method(s) should be assigned
#    type = "mc") # the data/processing type

## ----echo=FALSE, eval=FALSE---------------------------------------------------
#  # With the level 5 methods assigned, the data are ready for level 5 processing:
#  
#  tcpl::tcplRun(
#    id = atg.aeid[,aeid], # assay endpoint id to pipeline
#    slvl = 5L, # level to start pipelining on
#    elvl = 5L, # level to end pipelining on
#    type = 'mc' # endpoint processing type - 'mc' = "multiple concentrations"
#  )

## ----eval=FALSE---------------------------------------------------------------
#  ## Explore the Level 5 Data ##
#  # Load the level 5 data.
#  mc5 <- tcplLoadData(lvl = 5,
#                      type = 'mc',
#                      fld = 'aeid',
#                      val = 80,
#                      add.fld = TRUE)
#  # Prepare the data into a readable format.
#  mc5 <- tcplPrepOtpt(mc5)
#  # Display the data.
#  mc5

## -----------------------------------------------------------------------------
# Allocate the level 5 data in `mc_vignette` to the `mc5` object.
mc5 <- mc_vignette[["mc5"]]

## ----eval=FALSE---------------------------------------------------------------
#  ## Plot the Interactive Level 5 Figure ##
#  # Obtain the `m4id` for AEID 80 and spid `01504209`
#  my_id <- tcplLoadData(lvl = 5,
#                        fld = c('aeid','spid'),
#                        val = list(80,'01504209'))$m4id
#  # Report the `m4id`.
#  my_id
#  # Generate the interactive concentration-response plot.
#  tcplPlot(lvl = 5,val = my_id,output = 'console')

## ----fig.align='center',fig.dim=c(8,5.5),class.source = "scroll-100"----------
## Obtain Data ##
# First, we need to obtain the subset of data related to spid = "01504209",
# which is our example spid.
mc3_ss <- mc3 %>% dplyr::filter(spid == "01504209") # Level 3 - conc-resp series
mc4_ss <- mc4 %>% dplyr::filter(spid == "01504209") # Level 4 - model fits
mc5_ss <- mc5 %>% dplyr::filter(spid == "01504209") # Level 5 - best fit & est.
# Next, we need to obtain the smooth curve estimate for the best model found
# in the Level 5 analyses of the `tcpl` pipeline.
estDR <- estDR %>%
  dplyr::mutate(.,best_modl = ifelse(variable == mc5_ss[,modl],
                                     yes = "best model",no = NA))

## Generate a Base Concentration-Response Plot ##
basePlot <- mc3_ss %>% 
  # Observed Concentration-Response Data
  ggplot()+
  geom_point(aes(x = logc,y = resp),pch = 1,size = 2)+
  # Cutoff Band
  geom_rect(data = mc5_ss,
            aes(xmin = logc_min,xmax = logc_max,ymin = -coff,ymax = coff),
            alpha = 0.15,fill = "skyblue")+
  # Best Model Fit
  geom_line(data = dplyr::filter(estDR,variable == mc5_ss[,modl]),
            aes(x = log10(X),y = value,color = mc5_ss[,modl]))+
  scale_colour_manual(values = c("royalblue3"),aesthetics = "color")+
  # Other Model Fits
  geom_line(data = dplyr::filter(estDR,variable != mc5_ss[,modl]),
            aes(x = log10(X),y = value,lty = variable),
            alpha = 0.3,show.legend = TRUE)+
  # Legend Information
  labs(lty = "Other Models",color = "Best Fit")+
  # Titles and Labels
  xlab(expression(paste(log[10],"(Concentration) ",mu,"M")))+
  ylab(expression(paste(log[2],"(Fold Induction)")))+# )+
  ggtitle(
    label = paste("Level 5 Best Model Fit",
                  mc4[which(mc4[,spid] == "01504209"),dsstox_substance_id],
                  sep = "\n"),
    subtitle = paste("Assay Endpoint: ",
                     mc4[which(mc4[,spid] == "01504209"),aenm]))+
  # Background Plot Theme
  theme_bw()

## Potency Estimate Layers ##
# First, we need to obtain/assign colors for the potency estimates to be
# displayed.
potency_cols <-
  # choose 5 distinct colors
  viridis::plasma(n = 5,direction = -1) %>% 
  # darken the original colors to make them more visible
  colorspace::darken(.,amount = 0.1)
  
## Compile the Full Level 5 Plot ##
linePlot <-
  # Start with the `basePlot` object.
  basePlot +
  # Next, add the various potency layers.
  # BMD
  geom_hline(
    data = mc5_ss,
    aes(yintercept = bmr),
    col = potency_cols[1]
  ) +
  geom_segment(
    data = mc5_ss,
    aes(x = log10(bmd), xend = log10(bmd), y = -0.5, yend = bmr),
    col = potency_cols[1]
  ) +
  geom_hline(
    data = mc5_ss,
    aes(yintercept = coff),
    col = potency_cols[2]
  ) +
  geom_segment(
    data = mc5_ss,
    aes(x = log10(acc), xend = log10(acc), y = -0.5, yend = coff),
    col = potency_cols[2]
  ) +
  geom_hline(
    data = mc5_ss,
    aes(yintercept = max_med * 0.5),
    col = potency_cols[3]
  ) +
  geom_segment(
    data = mc5_ss,
    aes(
      x = log10(ac50), xend = log10(ac50),
      y = -0.5, yend = max_med * 0.5
    ),
    col = potency_cols[3]
  ) +
  geom_hline(
    data = mc5_ss,
    aes(yintercept = max_med * 0.1),
    col = potency_cols[4]
  ) +
  geom_segment(
    data = mc5_ss,
    aes(
      x = log10(ac10), xend = log10(ac10),
      y = -0.5, yend = max_med * 0.1
    ),
    col = potency_cols[4]
  ) +
  geom_hline(
    data = mc5_ss,
    aes(yintercept = max_med * 0.05),
    col = potency_cols[5]
  ) +
  geom_segment(
    data = mc5_ss,
    aes(
      x = log10(ac5), xend = log10(ac5),
      y = -0.5, yend = max_med * 0.05
    ),
    col = potency_cols[5]
  )


# create data table for potency estimate points
mc5_points <- mc5_ss %>%
  select(bmd, acc, ac50, ac10, ac5) %>%
  tidyr::pivot_longer(everything(), names_to = "Potency Estimates") %>%
  mutate(x = log10(value)) %>%
  mutate(mc_color = potency_cols) %>%
  mutate(`Potency Estimates` = toupper(`Potency Estimates`))
yvals <- mc5_ss %>%
  select(bmr, coff, max_med) %>%
  tidyr::pivot_longer(everything()) %>%
  select(value) %>%
  mutate(reps = c(1, 1, 3)) %>%
  tidyr::uncount(reps) %>%
  mutate(y = value * c(1, 1, .5, .1, .05)) %>%
  select(y)
mc5_points <- mc5_points %>% cbind(yvals)

# add Potency Estimate Points and set colors
fullPlot <- linePlot + geom_point(
  data = mc5_points,
  aes(x = x, y = y, fill = `Potency Estimates`), shape = 21, cex = 2.5
) +
  scale_fill_manual(values = mc5_points %>% arrange(`Potency Estimates`) %>% pull(mc_color))


## Display the Compiled Plot ##
fullPlot

## ----lvl-6-flag-table, warning = FALSE, echo = FALSE--------------------------
# First column - Level 6 Method ID
Method <- c(5:11, 13:15, 17:20)

# Second column - Level 6 Flag Names
FlagNames <- c("modl.directionality.fail", "singlept.hit.high", "singlept.hit.mid",
               "multipoint.neg", "bmd.high", "noise", "border", "low.nrep", 
               "low.nconc", "gnls.lowconc", "efficacy.50", "ac50.lowconc", 
               "viability.gnls", "no.med.gt.3bmad")
# Third column - Level 6 Flag Descriptions
FlagDescription <- c("Flag series if model directionality is questionable, i.e. if the winning model
                     direction was opposite, more responses $(resp)$ would have exceeded the cutoff 
                     $(coff)$. If loss was winning directionality $(top < 0)$,
                     flag if $count(resp < -1 * coff) < 2 * count(resp > coff)$. 
                     If gain was winning directionality
                     $(top > 0)$, flag if $count(resp > coff) < 2 * count(resp < -1 * coff)$.",
                     "Flag single-point hit that's only at the highest conc tested, where series is 
                     an active hit call $(hitc >= 0.9)$ with the median response observed above 
                     baseline occurring only at the highest tested concentration tested.",
                     "Flag single-point hit that's not at the highest conc tested, where series is 
                     an active hit call $(hitc >= 0.9)$ with the median response observed above 
                     baseline occurring only at one concentration and not the highest concentration
                     tested.", 
                     "Flag multi-point miss, where series is an inactive hit call $(hitc < 0.9)$ 
                     with multiple median responses observed above baseline.",
                     "Flag series if modeled benchmark dose $(BMD)$ is greater than AC~50~ 
                     (concentration at 50% maximal response). This is indicates high 
                     variability in baseline response in excess of more than half of the maximal 
                     response.", 
                     "Flag series as noisy if the quality of fit as calculated by the root mean 
                     square error $(rmse)$ for the series is greater than the cutoff $(coff)$; 
                     $rmse > coff$",
                     "Flag series if borderline activity is suspected based on modeled top 
                     parameter $(top)$ relative to cutoff $(coff)$; $|top| <= 1.2 * coff$ or 
                     $|top|>= 0.8 * coff$.",
                     "Flag series if the average number of replicates per concentration is less than
                     2; $nrep < 2$.",
                     "Flag series if 4 concentrations or less were tested; $nconc <= 4$.",
                     "Flag series where winning model is gain-loss $(gnls)$ and the gain AC~50~ is less 
                     than the minimum tested concentration, and the loss AC~50~ is less than the mean 
                     tested concentration.",
                     "Flag low efficacy hits if series has an active hit call $(hitc >= 0.9)$ and 
                     efficacy values (e.g. top and maximum median response) less than 50%; 
                     intended for biochemical assays. If $hitc >= 0.9$ and $coff >= 5$, then flag when 
                     $top < 50$ or $max\\_med < 50$. If $hitc >= 0.9$ and $coff < 5$, then flag when 
                     $top < \\log_{2}(1.5)$ or $max\\_med < \\log_{2}(1.5)$.",
                     "Flag series with an active hit call $(hitc >= 0.9)$ if AC~50~ (concentration 
                     at 50% maximal response) is less than the lowest concentration tested; if 
                     $hitc >= 0.9$ and $AC_{50} < 10^{\\log_c(min)}$, then flag.", 
                     "Flag series with an active hit call $(hitc >= 0.9)$ if denoted as cell 
                     viability assay with winning model is gain-loss $(gnls)$; if $hitc >= 0.9$, 
                     $modl = gnls$ and $cell\\_viability\\_assay = 1$, then flag.",
                     "Flag series where no median response values are greater than baseline as 
                     defined by 3 times the baseline median absolute deviation $(bmad)$;
                     $nmed\\_gtbl = 0$, where $nmed\\_gtbl$ is the number of medians greater than 
                     $3 * bmad$.")

# Consolidate all columns into a table.
output <- data.frame(Method, FlagNames, FlagDescription)

# Export/print the table into an html rendered table.
htmlTable(output,
          align = 'l',
          align.header = 'l',
          header = c("Method", "Name", "Description"),
          rnames = FALSE,
          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
          
          caption = "Table 10: Level 6 Methods - Cautionary Flags" #,
          #tfoot = "Level 6 flag descriptions are pulled from ##SOURCE## <SOURCE>."
)

## ----eval=FALSE---------------------------------------------------------------
#  ## Methods Assignment ##
#  # Level 1 - Normalization
#  tcplMthdAssign(lvl = 1, # data level
#                 id = 1:2, # assay component
#                 mthd_id = c(1, 11, 13), # method(s) to be applied
#                 ordr = 1:3, # order sequence for applied methods
#                 type = "sc") # data type - single concentration
#  
#  # Level 2 - Bioactivity Hit Call
#  tcplMthdAssign(lvl = 2, # data level
#                 id = 1, # assay component
#                 mthd_id = 3, # method(s) to be applied
#                 type = "sc") # data type - single concentration

## ----eval=FALSE---------------------------------------------------------------
#  ## Pipeline Assay Components (acid's) from level 0 to 2.
#  sc_res <- tcplRun(id = 1, # assay component id to pipeline
#                    type = "sc", # type of data being processed
#                    slvl = 1, # level to start pipelining on
#                    elvl = 2) # level to end pipelining on

## ----eval=FALSE---------------------------------------------------------------
#  ## Methods Assignment.
#  # Level 1 - Indexing
#  #   * No methods need to be assigned.
#  
#  # Level 2 - Data Transformation
#  tcplMthdAssign(lvl = 2, # data level
#                 id = 49, # assay component
#                 mthd_id = c(3,4,2), # method(s) to be applied
#                 ordr = 1:3, # the order method(s) are to be applied
#                 type = "mc") # data type - multiple concentration
#  
#  # Level 3 - Normalization
#  tcplMthdAssign(lvl = 3, # data level
#                 id = 80, # assay endpoint
#                 mthd_id = 1, # method(s) to be applied
#                 ordr = 1, # the order method(s) are to be applied
#                 type = "mc") # data type - multiple concentration
#  
#  # Level 4 - Concentration-Response Modeling
#  tcplMthdAssign(lvl = 4, # data level
#                 id = 80, # assay endpoint
#                 mthd_id = c(1,3), # method(s) to be applied
#                 ordr = 1:2, # the order method(s) are to be applied
#                 type = "mc") # data type - multiple concentration
#  
#  # Level 5 - Best Model Fit
#  tcplMthdAssign(lvl = 5, # data level
#                 id = 80, # assay endpoint
#                 mthd_id = c(1,3,5), # method(s) to be applied
#                 ordr = 1:3, # the order method(s) are to be applied
#                 type = "mc") # data type - multiple concentration

## ----eval=FALSE---------------------------------------------------------------
#  ## Assign the Number of Processing Cores.
#  mycores <- 1 # DO NOT leverage parallel computing.
#               # Users that want to leverage parallel computing set to > 1,
#               # but less than the total number of cores (i.e. need at least
#               # 1 core open for overhead).  Please see below.
#  # mycores <- parallel::detectCores() - 1 # Maximum allowed number of cores.
#  
#  ## Pipeline Assay Components (acid's) from level 0 to 5.
#  mc_res <- tcpl::tcplRun(
#    id = atg.aeid[,acid], # assay component id to pipeline
#    type = "mc", # type of data being processed
#    slvl = 0L, # level to start pipelining on
#    elvl = 5L, # level to end pipelining on
#    mc.cores = mycores # number of computing cores
#  )

## ----eval=FALSE---------------------------------------------------------------
#  ## Assign the Number of Processing Cores.
#  mycores <- 1 # DO NOT leverage parallel computing.
#               # Users that want to leverage parallel computing set to > 1.
#  
#  ## Pipeline Assay Components (acid's) from level 0 to 3.
#  mc_res_0.3 <- tcpl::tcplRun(
#    id = atg.aeid[,acid], # assay component id to pipeline
#    type = "mc",
#    slvl = 0L, # level to start pipelining on
#    elvl = 3L,  # level to end pipelining on
#    mc.cores = mycores # number of computing cores
#  )
#  
#  ## Pipeline Assay Endpoints (aeid's) from level 4 to 5.
#  mc_res_4.5 <- tcpl::tcplRun(
#    id = atg.aeid[,aeid], # assay endpoint id to pipeline
#    type = "mc",
#    slvl = 4L, # level to start pipelining on
#    elvl = 5L,  # level to end pipelining on
#    mc.cores = mycores # number of computing cores
#  )

