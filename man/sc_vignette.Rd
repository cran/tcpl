% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sc_vignette}
\alias{sc_vignette}
\title{List with single-concentration data for the vignette}
\format{
\enumerate{
\item \strong{sc0} A data frame with 10 rows and 18 columns containing
                   level 0 formatted raw data.
\describe{
  \item{spid}{Sample ID}
  \item{chid}{Unique chemical ID number for tcpl}
  \item{casn}{Chemical Abstract Service(CAS) number}
  \item{chnm}{Chemical name}
  \item{dsstox_substance_id}{Chemical-specific DTXSID}
  \item{code}{CAS number compressed into numeric string}
  \item{acid}{Assay Component ID}
  \item{acnm}{Assay Component Name}
  \item{s0id}{Level 0 (sc0) ID}
  \item{apid}{Assay plate ID}
  \item{rowi}{Row Index}
  \item{coli}{Column Index}
  \item{wllt}{Well Type}
  \item{wllq}{Well Quality (0 or 1)}
  \item{conc}{Concentration in micromolar}
  \item{rval}{Raw assay component readout value}
  \item{srcf}{Source file containing the raw data}
  \item{conc_unit}{Concentration Units}
  }


\item \strong{sc1} A data frame with 10 rows and 20 columns containing
                   level 1 normalized data.
\describe{
  \item{spid}{Sample ID}
  \item{chid}{Unique chemical ID number for tcpl}
  \item{casn}{Chemical Abstract Service(CAS) number}
  \item{chnm}{Chemical name}
  \item{dsstox_substance_id}{Chemical-specific DTXSID}
  \item{code}{CAS number compressed into numeric string}
  \item{aeid}{Assay Component Endpoint ID}
  \item{aenm}{Assay endpoint name (i.e., assay_component_endpoint_name)}
  \item{acid}{Assay Component ID}
  \item{acnm}{Assay Component Name}
  \item{s0id}{Level 0 (sc0) ID}
  \item{s1id}{Level 1 (sc1) ID}
  \item{apid}{Assay plate ID}
  \item{rowi}{Row Index}
  \item{coli}{Column Index}
  \item{wllt}{Well Type}
  \item{logc}{Log base 10 concentration}
  \item{resp}{Normalized response value}
  \item{resp_unit}{Response Units}
  \item{conc_unit}{Concentration Units}
  }


\item \strong{sc2} A data frame with 10 rows and 15 columns containing
                   level 2 efficacy/hit designation data.
\describe{
  \item{spid}{Sample ID}
  \item{chid}{Unique chemical ID number for tcpl}
  \item{casn}{Chemical Abstract Service(CAS) number}
  \item{chnm}{Chemical name}
  \item{dsstox_substance_id}{Chemical-specific DTXSID}
  \item{code}{CAS number compressed into numeric string}
  \item{aeid}{Assay Component Endpoint ID}
  \item{aenm}{Assay endpoint name (i.e., assay_component_endpoint_name)}
  \item{s2id}{Level 2 (sc2) ID}
  \item{bmad}{The median absolute deviation of all treatment wells (default option) or blank wells }
  \item{max_med}{Maximum median response}
  \item{hitc}{Hitcall}
  \item{coff}{Cutoff}
  \item{resp_unit}{Response Units}
  \item{conc_unit}{Concentration Units}
  }
}
}
\usage{
sc_vignette
}
\description{
This dataset is a list with 3 data.tables (sc0,sc1,sc2).
}
\keyword{datasets}
