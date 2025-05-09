#-------------------------------------------------------------------------------
# sc2: Perform level 2 single-concentration processing
#-------------------------------------------------------------------------------

#' @template proclvl
#' @templateVar LVL 2
#' @templateVar type sc
#' 
#' @param ae Integer of length 1, assay endpoint id (aeid) for processing.
#' @param wr Logical, whether the processed data should be written to the tcpl
#' database
#'  
#' @details
#' Level 2 single-concentration processing defines the bmad value, and uses the
#' activity cutoff methods from sc2_aeid and sc2_methods to make an activity
#' call.
#' 
#' @seealso \code{\link{Method functions}}, \code{\link{SC2_Methods}}
#' 
#' @import data.table
#' @importFrom stats mad median

sc2 <- function(ae, wr = FALSE) {
  
  ## Variable-binding to pass R CMD Check
  max_tmp <- bmad <- resp <- wllt <- tmp <- spid <- conc <- hitc <- max_med <- NULL
  
  owarn <- getOption("warn")
  options(warn = 1)
  on.exit(options(warn = owarn))
  
  ## Check the ae input
  if (length(ae) > 1) {
    warning("ae must be of length 1. Level 2 processing incomplete; no ",
            "updates\n  made to the sc2 table for AEIDS ",
            paste(ae, collapse = ", "), ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  stime <- Sys.time()
  
  ## Load level 1 data
  dat <- tcplLoadData(lvl = 1L, type = "sc", fld = "aeid", val = ae)
  
  ## Check if any level 1 data was loaded
  if (nrow(dat) == 0) {
    warning("No level 1 data for AEID", ae, ". Level 2 processing incomplete;",
            " no updates\n  made to the sc2 table for AEID", ae, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Loaded L1 AEID", ae, " (", nrow(dat), " rows; ", ttime,")\n", sep = "")
  
  stime <- Sys.time()
  
  ## Calculate bmad
  dat[ , bmad := mad(resp[wllt == "t"])]
  
  ## Collapse by spid
  dat[ , tmp := median(resp), by = list(spid, wllt, conc)]
  
  ## take absolute value for bidirectional fitting.
  dat[ , c("tmpi", "max_med","max_tmp") := list(.GRP, max(abs(tmp)), tmp[which.max(abs(tmp))]), by = c("spid")]
  
  ## Initialize coff vector
  coff <- 0
  
  ## Load cutoff functions
  mthd_funcs <- sc2_mthds()
  
  ## Load cutoff methods
  ms <- tcplMthdLoad(lvl = 2L, id = ae, type = "sc")
  if (nrow(ms) == 0) {
    warning("No level 5 methods for AEID", ae, " -- cutoff will be 0.")
  }
  
  ## Extract methods that need to overwrite bmad
  ms_overwrite <- ms[grepl("ow_",mthd),]
  ## ignore any other methods
  ms <- ms[!grepl("ow_",mthd),]
  
  ## Apply bmad/max_med overwrite methods first if needed
  if (nrow(ms_overwrite) > 0) {
    exprs <- lapply(mthd_funcs[ms_overwrite$mthd], do.call, args = list())
    fenv <- environment()
    invisible(rapply(exprs, eval, envir = fenv))
  }
  
  ## Apply cutoff methods
  exprs <- lapply(mthd_funcs[ms$mthd], do.call, args = list())
  fenv <- environment()
  invisible(rapply(exprs, eval, envir = fenv))
  
  ## Determine final cutoff
  dat[ , coff := max(coff)]
  
  ## Determine hit-call
  dat[ , hitc := as.integer(max_med >= coff)]
  
  ## set max med back to the tmp value so we conserve directionality.
  dat[ , max_med := max_tmp]
  
  ## overwrite hitc for gain direction endpoints
  if ("ow_bidirectional_gain" %in% ms_overwrite$mthd) {
    dat <- dat[max_med < 0, hitc := hitc*-1]
  }
  
  ## overwrite hitc for loss direction endpoints
  if ("ow_bidirectional_loss" %in% ms_overwrite$mthd) {
    dat <- dat[max_med > 0, hitc := hitc*-1]
  }

  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Processed L2 AEID", ae, " (", nrow(dat), 
      " rows; ", ttime, ")\n", sep = "")
  
  res <- TRUE
  
  outcols <- c("s0id", "s1id", "spid", "aeid", "max_med", 
               "bmad", "coff", "hitc", "tmpi")
  dat <- dat[ , .SD, .SDcols = outcols]
  
  ## Load into sc2 table -- else return results
  if (wr) {
    stime <- Sys.time()
    tcplWriteData(dat = dat, lvl = 2L, type = "sc")
    
    ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
    ttime <- paste(unclass(ttime), units(ttime))
    cat("Wrote L2 AEID", ae, " (", nrow(dat), 
        " rows; ", ttime, ")\n", sep = "")
  } else {
    res <- c(res, list(dat))
  }
  
  return(res)

}
#-------------------------------------------------------------------------------
