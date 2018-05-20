### R code from vignette source 'tcpl_Overview.Rnw'

###################################################
### code chunk number 1: tcpl_Overview.Rnw:43-54
###################################################
options(continue = "  ", width = 60, warn = -1)
pdf.options(pointsize = 10)
library(tcpl)

## This chunk copies the sqlite db to the temp directory used in installation
## to comply with CRAN policies on not writing to the installation directory
dbfile <- file.path(system.file(package = "tcpl"), "sql", "tcpldb.sqlite")
dbfile_temp <- file.path(tempdir(), "tcpldb.sqlite")
file.copy(from = dbfile, dbfile_temp)
tcplConf(db = dbfile_temp)
tcpl:::.clearSQLite(dbfile_temp)


###################################################
### code chunk number 2: tcpl_Overview.Rnw:96-100
###################################################
library(data.table)
library(tcpl)
## Store the path the tcpl directory for loading data
pkg_dir <- system.file(package = "tcpl")


###################################################
### code chunk number 3: tcpl_Overview.Rnw:123-128 (eval = FALSE)
###################################################
## tcplConf(drvr = "MySQL", 
##          user = "root", 
##          pass = "", 
##          host = "localhost",
##          db   = "toxcastdb")


###################################################
### code chunk number 4: tcpl_Overview.Rnw:162-166
###################################################
## Add a new assay source, call it CTox,
## that produced the data
tcplRegister(what = "asid", flds = list(asnm = "CTox"))
tcplLoadAsid()


###################################################
### code chunk number 5: tcpl_Overview.Rnw:171-175
###################################################
tcplRegister(what = "aid", 
             flds = list(asid = 1, 
                         anm = "Steroidogenesis", 
                         assay_footprint = "96 well"))


###################################################
### code chunk number 6: tcpl_Overview.Rnw:182-193
###################################################
tcplRegister(what = "acid", 
             flds = list(aid = 1, acnm = "CTox_CORT"))
tcplRegister(what = "aeid", 
             flds = list(acid = c(1, 1), 
                         aenm = c("CTox_CORT_up", 
                                  "CTox_CORT_dn"),
                         normalized_data_type = 
                         rep("log2_fold_induction", 2),
                         export_ready = c(1, 1),
                         burst_assay = c(0, 0),
                         fit_all = c(0, 0)))


###################################################
### code chunk number 7: tcpl_Overview.Rnw:204-212
###################################################
ch <- fread(file.path(pkg_dir, "sql", "chdat.csv"))
head(ch)

## Register the unique chemicals
tcplRegister(what = "chid", 
             flds = ch[ , 
                        unique(.SD), 
                        .SDcols = c("casn", "chnm")])


###################################################
### code chunk number 8: tcpl_Overview.Rnw:217-222
###################################################
cmap <- tcplLoadChem()
tcplRegister(what = "spid", 
             flds = merge(ch[ , list(spid, casn)], 
                          cmap[ , list(casn, chid)], 
                          by = "casn")[ , list(spid, chid)])


###################################################
### code chunk number 9: tcpl_Overview.Rnw:227-233
###################################################
grp1 <- cmap[chid %% 2 == 0, unique(chid)]
grp2 <- cmap[chid %% 2 == 1, unique(chid)]
tcplRegister(what = "clib", 
             flds = list(clib = "group_1", chid = grp1))
tcplRegister(what = "clib", 
             flds = list(clib = "group_2", chid = grp2))


###################################################
### code chunk number 10: tcpl_Overview.Rnw:238-241
###################################################
tcplRegister(what = "clib", 
             flds = list(clib = "other", chid = 1:2))
tcplLoadClib(field = "chid", val = 1:2)


###################################################
### code chunk number 11: tcpl_Overview.Rnw:246-249
###################################################
scdat <- fread(file.path(pkg_dir, "sql", "scdat.csv"))
mcdat <- fread(file.path(pkg_dir, "sql", "mcdat.csv"))
c(unique(scdat$acsn), unique(mcdat$acsn))


###################################################
### code chunk number 12: tcpl_Overview.Rnw:254-256
###################################################
tcplRegister(what = "acsn", 
             flds = list(acid = 1, acsn = "cort"))


###################################################
### code chunk number 13: tcpl_Overview.Rnw:261-263
###################################################
tcplWriteLvl0(dat = scdat, type = "sc")
tcplWriteLvl0(dat = mcdat, type = "mc")


###################################################
### code chunk number 14: tcpl_Overview.Rnw:268-269
###################################################
tcplLoadData(lvl = 0, fld = "acid", val = 1, type = "sc")


###################################################
### code chunk number 15: tcpl_Overview.Rnw:326-343
###################################################
## For illustrative purposes, assign level 2 MC methods to 
## ACIDs 98, 99. First check for available methods.
mthds <- tcplMthdList(lvl = 2, type = "mc")
mthds[1:2]
## Assign some methods to ACID 97, 98 & 99
tcplMthdAssign(lvl = 2, 
               id = 97:99, 
               mthd_id = c(3, 4, 2), 
               ordr = 1:3, 
               type = "mc")
tcplMthdLoad(lvl = 2, id = 97:99, type = "mc")
## Methods can be cleared one at a time for the given id(s)
tcplMthdClear(lvl = 2, id = 99, mthd_id = 2, type = "mc")
tcplMthdLoad(lvl = 2, id = 99, type = "mc")
## Or all methods can be cleared for the given id(s)
tcplMthdClear(lvl = 2, id = 97:98, type = "mc")
tcplMthdLoad(lvl = 2, id = 97:98, type = "mc")


###################################################
### code chunk number 16: tcpl_Overview.Rnw:425-426
###################################################
tcplLoadAeid(fld = "acid", val = 1)


###################################################
### code chunk number 17: tcpl_Overview.Rnw:433-443
###################################################
tcplMthdAssign(lvl = 1, 
               id = 1:2,
               mthd_id = c(1, 11, 13), 
               ordr = 1:3,
               type = "sc")
tcplMthdAssign(lvl = 1, 
               id = 2,
               mthd_id = 16, 
               ordr = 4,
               type = "sc")


###################################################
### code chunk number 18: tcpl_Overview.Rnw:451-453
###################################################
## Do level 1 processing for acid 1
sc1_res <- tcplRun(id = 1, slvl = 1, elvl = 1, type = "sc")


###################################################
### code chunk number 19: tcpl_Overview.Rnw:469-474
###################################################
## Assign a cutoff value of log2(1.2)
tcplMthdAssign(lvl = 2,
               id = 1:2,
               mthd_id = 3,
               type = "sc")


###################################################
### code chunk number 20: tcpl_Overview.Rnw:482-484
###################################################
## Do level 1 processing for acid 1
sc2_res <- tcplRun(id = 1:2, slvl = 2, elvl = 2, type = "sc")


###################################################
### code chunk number 21: tcpl_Overview.Rnw:527-529
###################################################
## Do level 1 processing for acid 1
mc1_res <- tcplRun(id = 1, slvl = 1, elvl = 1, type = "mc")


###################################################
### code chunk number 22: tcpl_Overview.Rnw:534-542
###################################################
## Load the level 1 data and look at the cndx and repi values
m1dat <- tcplLoadData(lvl = 1, 
                      fld = "acid", 
                      val = 1, 
                      type = "mc")
m1dat <- tcplPrepOtpt(m1dat)
setkeyv(m1dat, c("repi", "cndx"))
m1dat[chnm == "3-Phenylphenol", list(chnm, conc, cndx, repi)]


###################################################
### code chunk number 23: l1apid
###################################################
tcplPlotPlate(dat = m1dat, apid = "09Apr2014.Plate.17")


###################################################
### code chunk number 24: tcpl_Overview.Rnw:563-568
###################################################
tcplMthdAssign(lvl = 2,
               id = 1,
               mthd_id = 1, 
               ordr = 1, 
               type = "mc")


###################################################
### code chunk number 25: tcpl_Overview.Rnw:573-575
###################################################
## Do level 2 processing for acid 1
mc2_res <- tcplRun(id = 1, slvl = 2, elvl = 2, type = "mc")


###################################################
### code chunk number 26: tcpl_Overview.Rnw:587-589
###################################################
## Look at the assay endpoints for acid 1
tcplLoadAeid(fld = "acid", val = 1)


###################################################
### code chunk number 27: tcpl_Overview.Rnw:593-603
###################################################
tcplMthdAssign(lvl = 3,
               id = 1:2,
               mthd_id = c(17, 9, 7), 
               ordr = 1:3, 
               type = "mc")
tcplMthdAssign(lvl = 3, 
               id = 2,
               mthd_id = 6, 
               ordr = 4, 
               type = "mc")


###################################################
### code chunk number 28: tcpl_Overview.Rnw:609-611
###################################################
## Do level 3 processing for acid 1
mc3_res <- tcplRun(id = 1, slvl = 3, elvl = 3, type = "mc")


###################################################
### code chunk number 29: tcpl_Overview.Rnw:701-703
###################################################
## Do level 4 processing for aeid 1 and load the data
mc4_res <- tcplRun(id = 1:2, slvl = 4, elvl = 4, type = "mc")


###################################################
### code chunk number 30: tcpl_Overview.Rnw:708-713
###################################################
## Load the level 4 data 
m4dat <- tcplLoadData(lvl = 4, type = "mc")
## List the first m4ids where the hill model convered
## for AEID 1
m4dat[hill == 1 & aeid == 1, head(m4id)]


###################################################
### code chunk number 31: l4plt
###################################################
## Plot a fit for m4id 21
tcplPlotM4ID(m4id = 686, lvl = 4)


###################################################
### code chunk number 32: tcpl_Overview.Rnw:752-757
###################################################
## Assign a cutoff value of bmad*6
tcplMthdAssign(lvl = 5,
               id = 1:2,
               mthd_id = 6, 
               type = "mc")


###################################################
### code chunk number 33: podplt
###################################################
par(family = "mono", mar = rep(1, 4), pty = "m")
plot.new()
plot.window(xlim = c(0, 30), ylim = c(-30, 100))
# axis(side = 2, lwd = 2, col = "gray35")
rect(xleft = par()$usr[1],
     xright = par()$usr[2], 
     ybottom = -15, 
     ytop = 15,
     border = NA, 
     col = "gray45",
     density = 15, 
     angle = 45)
abline(h = 26, lwd = 3, lty = "dashed", col = "gray30")
tmp <- list(modl = "gnls", gnls_ga = 12, gnls_tp = 80, 
            gnls_gw = 0.18, gnls_lw = 0.7, gnls_la = 25)
tcplAddModel(pars = tmp, lwd = 3, col = "dodgerblue2")

abline(v = 8.46, lwd = 3, lty = "solid", col = "firebrick")
text(x = 8.46, y = par()$usr[4]*0.9, 
     font = 2, labels = "ACB", cex = 2, pos = 2, srt = 90)
abline(v = 10.24, lwd = 3, lty = "solid", col = "yellow2")
text(x = 10.24, y = par()$usr[4]*0.9, 
     font = 2, labels = "ACC", cex = 2, pos = 2, srt = 90)
abline(v = 12, lwd = 3, lty = "solid", col = "dodgerblue2")
text(x = 12, y = par()$usr[4]*0.9, 
     font = 2, labels = "AC50", cex = 2, pos = 2, srt = 90)

points(x = c(8.46, 10.24, 12), y = c(15, 26, 40),
       pch = 21, cex = 2, col = "gray30", lwd = 2,
       bg = c("firebrick", "yellow2", "dodgerblue2"))


###################################################
### code chunk number 34: fitc1
###################################################
tcplPlotFitc()


###################################################
### code chunk number 35: tcpl_Overview.Rnw:823-825
###################################################
## Do level 5 processing for aeid 1 and load the data
mc5_res <- tcplRun(id = 1:2, slvl = 5, elvl = 5, type = "mc")


###################################################
### code chunk number 36: l5plt1
###################################################
tcplPlotM4ID(m4id = 370, lvl = 5)


###################################################
### code chunk number 37: fitc2
###################################################
m5dat <- tcplLoadData(lvl = 5, type = "mc")
tcplPlotFitc(fitc = m5dat$fitc)


###################################################
### code chunk number 38: tcpl_Overview.Rnw:850-853
###################################################
head(m5dat[fitc == 21, 
           list(m4id, hill_tp, gnls_tp, 
                max_med, coff, hitc)])


###################################################
### code chunk number 39: l5plt2
###################################################
tcplPlotM4ID(m4id = 45, lvl = 5)


###################################################
### code chunk number 40: tcpl_Overview.Rnw:872-877
###################################################
tcplMthdAssign(lvl = 6,
               id = 1:2,
               mthd_id = c(6:8, 10:12, 15:16), 
               type = "mc")
tcplMthdLoad(lvl = 6, id = 1, type = "mc")


###################################################
### code chunk number 41: tcpl_Overview.Rnw:884-887
###################################################
## Do level 6 processing
mc6_res <- tcplRun(id = 1:2, slvl = 6, elvl = 6, type = "mc")
m6dat <- tcplLoadData(lvl = 6, type = "mc")


###################################################
### code chunk number 42: tcpl_Overview.Rnw:892-893
###################################################
m6dat[m4id == 46]


###################################################
### code chunk number 43: l6plt
###################################################
tcplPlotM4ID(m4id = 46, lvl = 6)


