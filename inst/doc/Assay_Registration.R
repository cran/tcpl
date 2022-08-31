## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE, message = FALSE-------------------------------------
library(tcpl)

## ----eval = TRUE, message = FALSE---------------------------------------------
#tcplLoadAsid()
#tcplRegister(what = "asid", flds = list(asid = 1, asnm = "Tox21"))

## ----eval = TRUE, message = FALSE---------------------------------------------
#tcplLoadAid(what = "asid", val=1)
#tcplRegister(what = "aid", 
#             flds = list(asid = 1, 
#                        anm = "TOX21_ERa_BLA_Agonist", 
#                         assay_footprint = "1536 well"))

## ----eval = TRUE, message = FALSE---------------------------------------------
#tcplLoadAcid(what = "asid", val=1, add.fld=c("aid", "anm"))
#tcplRegister(what = "acid", 
#             flds = list(aid = 1,
#                         acnm = "TOX21_ERa_BLA_Agonist_ratio"))

## ----eval = TRUE, message = FALSE---------------------------------------------
#tcplRegister(what = "acsn", flds = list(acid = 1, acsn = "TCPL-MC-Demo"))

## ----eval = TRUE, message = FALSE---------------------------------------------
#tcplLoadAeid(fld="asid",val=1, add.fld = c("aid", "anm", "acid", "acnm")) 
#tcplRegister(what = "aeid", 
#             flds = list(acid = 1, 
#                         aenm = "TOX21_ERa_BLA_Agonist_ratio",
#                         normalized_data_type = "percent_activity",
#                         export_ready = 1,
#                         burst_assay = 0,
#                         fit_all = 0))

## ----eval = TRUE, message = FALSE---------------------------------------------
# tcplUpdate(what = "acid", 
#              flds = list(aid = 1,
#                          acnm = "TOX21_ERa_BLA_Agonist_ratio"))

