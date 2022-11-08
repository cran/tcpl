## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------

library(htmlTable)

## ----eval=TRUE, message=FALSE-------------------------------------------------
library(data.table)
library(plotly)
library(tcplfit2)
library(tcpl)
## Store the path for the tcpl directory for loading data
pkg_dir <- system.file(package = "tcpl")


## ----eval = FALSE-------------------------------------------------------------
#  tcpl (v1.3) loaded with the following settings:
#    TCPL_DB:    C:/Users/user/R-3.4.4/library/tcpl/csv
#    TCPL_USER:  NA
#    TCPL_HOST:  NA
#    TCPL_DRVR:  NA
#  Default settings stored in TCPL.conf. See ?tcplConf for more information.

## ----eval = FALSE-------------------------------------------------------------
#  tcplConf(db   = "invitrodb",
#           user = "username",
#           pass = "password",
#           host = "localhost",
#           drvr = "MySQL")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("s0id ", "acid", "spid", "apid", "rowi", "coli", "wllt", "wllq", "conc", "rval", "srcf")
Description <- c("Level 0 ID",
                 "Assay component ID",
                 "Sample ID",
                 "Assay plate ID",
                 "Assay plate row index",
                 "Assay plate column index",
                 "Well type&dagger;",
                 "1 if the well quality was good, else 0;",
                 "Concentration is micromolar",
                 "Raw assay component value/readout from vendor",
                 "Filename of the source file containing the data"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 1: Fields in sc0 table.",
        tfoot="&dagger;Information about the different well types is available in Appendix B.")


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("s1id ", "s0id", "acid", "aeid", "logc", "bval", "pval", "resp")
Description <- c("Level 1 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Assay component endpoint ID",
                 "Log base 10 concentration",
                 "Baseline value",
                 "Positive control value",
                 "Normalized response value"
              
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 2: Fields in sc1 table."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("aeid ", "s0id", "s1id", "s2id")
Description <- c("Assay component endpoint ID",
                 "Level 0 ID",
                 "Level 1 ID",
                 "Level 2 ID"
              
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 3: Fields in sc2_agg table."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("s2id ", "aeid", "spid", "bmad", "max_med", "hitc", "coff", "tmpi")
Description <- c("Level 2 ID",
                 "Assay component endpoint ID",
                 "Sample ID",
                 "Baseline median absolute deviation",
                 "Maximum median response value",
                 "Hit-/activity-call: 1 if active, 0 if inactive&dagger;",
                 "Efficacy cutoff value",
                 "Ignore, temporary index used for uploading purposes"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 4: Fields in sc2 table.",
        tfoot = "&dagger; As sc data are not curve-fit, the hitcalling procedure performed at sc2 remains binary (hitc=1 or hitc=0)."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("s2id", "chid_rep")

Description <- c("Level 2 ID",
                 "Representative sample designation for a tested chemical: 1 if representative sample, else 0"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 5: Fields in sc2_chid.",
        
     
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m1id", "m0id", "acid", "cndx", "repi")
Description <- c("Level 1 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Concentration index",
                 "Replicate index"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 6: Fields in mc1 table."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m2id", "m0id", "acid", "m1id", "cval")
Description <- c("Level 2 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Level 1 ID",
                 "Corrected value"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 7: Fields in mc2 table."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m3id", "aeid", "m0id", "acid", "m1id", "m2id", "bval", "pval", "logc", "resp")
Description <- c("Level 3 ID",
                 "Assay endpoint ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Level 1 ID",
                 "Level 2 ID",
                 "Baseline value",
                 "Positive control value",
                 "Log base 10 concentration",
                 "Normalized response value"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 8: Fields in mc3 table."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("aeid", "m0id", "m1id", "m2id", "m3id", "m4id")
Description <- c(
   "Assay endpoint ID","Level 0 ID",
                 "Level 1 ID",
                 "Level 2 ID",
                 "Level 3 ID",
                 "Level 4 ID"
                 
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 9: Fields in mc4_agg table."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------

Field <- c("m4id", "aeid", "spid", "bmad", "resp_max", "resp_min", "max_mean", "max_mean_conc", "max_med", "max_med_conc", "logc_max", "logc_min", 
           "nconc", "npts", "nrep", "nmed_gtbl", "tmpi")


Description <- c("Level 4 ID",
                 "Assay endpoint ID",
                 "Sample ID",
                 "Baseline median absolute deviation",
                 "Maximum response value",
                 "Minimum response value",
                 "Maximum mean response value",
                 "Log concentration at *max_mean*",
                 "Maximum median response value",
                 "Log concentration at *max_med*",
                 "Maximum log concentration tested",
                 "Minimum log concentration tested",
                 "Number of concentrations tested ",
                 "Number of points in the concentration series",
                 "Number of replicates in the concentration series",
                 "Number of median values greater than *3bmad*",
                 "Ignore, temporary index used for uploading purposes"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 10: Fields in mc4 table."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m4id", "aeid", "model", "model_param", "model_val")

Description <- c("Level 4 ID",
                 "Assay endpoint ID",
                 "Model that was fit",
                 "Key for the parameter that was fit with the corresponding model",
                 "Value for the associated key in the corresponding model"
                )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 11: Fields in mc4_param table."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------

Field <- c("m5id", "m4id", "aeid", "modl", "hitc", "fitc", "coff", "actp", "model_type")


Description <- c("Level 5 ID",
                 "Level 4 ID",
                 "Assay endpoint ID",
                 "Winning model",
                 "Hit-/activity-call, generally a continuous value from 0 to 1 if using *tcplFit2* fitting&dagger;" ,
                 "Fit category",
                 "Efficacy cutoff value",
                "Activity probability (1 - *const_prob* not used with *tcplFit2*)",
                "Model type placeholder for use when number of fitting methodologies increases"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 12: Fields in mc5 table.",
        tfoot = "&dagger; The continuous hitcalls produced resultant to *tcplFit2* curve-fitting are described in more detail in library(tcplFit2) and Sheffield et al. 2021 (https://doi.org/10.1093/bioinformatics/btab779)."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m5id", "aeid", "hit_param", "hit_val")

Description <- c("Level 5 ID",
                 "Assay endpoint ID",
                 "Key for the parameter that was fit with winning model",
                 "Value for the associated key in the winning model"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 13: Fields in mc5_param table."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m5id", "chid_rep")

Description <- c("Level 5 ID",
                 "Representative sample designation for a tested chemical: 1 if representative sample, else 0"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 14: Fields in mc5_chid." )


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("assay", "assay_component", "assay_component_endpoint", "assay_component_map", "assay_reagent**", "assay_reference**", "assay_source", "chemical", "chemical_library", "citations**", "gene**", "intended_target**", "organism**", "sample")

Description <- c("Assay-level annotation",
                 "Assay component-level annotation",
                 "Assay endpoint-level annotation",
                 "Assay component source names and their corresponding assay component ids",
                 "Assay reagent information",
                 "Map of citations to assay",
                 "Assay source-level annotation",
                 "List of chemicals and associated identifiers",
                 "Map of chemicals to different chemical libraries",
                 "List of citations",
                 "Gene identifiers and descriptions",
                 "Intended assay target at the assay endpoint level",
                 "Organism identifiers and descriptions",
                 "Sample ID information and chemical ID mapping")

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        caption="Table 15: List of annotation tables.",
        tfoot = "** indicates tables not currently used by the *tcpl* package",
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em '
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("aid", "asid&dagger;", "assay_name&dagger;", "assay_desc", "timepoint_hr", 
            "organism_id", "organism",'tissue',"cell_format",
            'cell_free_component_source',
            'cell_short_name', 
            'cell_growth_mode',
            "assay_footprint&dagger;", 
            "assay_format_type" ,
            "assay_format_type_sub" ,
            "content_readout_type",  
            "dilution_solvent" , 
            "dilution_solvent_percent_max")

Description <- c("Assay ID",
                 "Assay source ID",
                 "Assay name (abbreviated \"anm\" within the package)",
                 "Assay description",
                 "Treatment duration in hours",
                 "NCBI taxonomic identifier, available here <https://www.ncbi.nlm.nih.gov/taxonomy>",
                "Organism of origin",
                "Tissue of origin", "Description of cell format",
                "Description of source for targeted cell-free components",
                "Abbreviation of cell line",
                "Cell growth modality", 
                "Microtiter plate size",
                "General description of assay format",
                "Specific description of assay format" ,
                "Description of well characteristics being measured", 
                "Solvent used in sample dilution",
                "Maximum percent of dilution solvent used, from 0 to 1.")

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        caption="Table 16: Fields in assay.",
        tfoot = "&dagger; Required fields for registration",
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em '
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("acid", "aid&dagger;", "assay_component_name&dagger;", "assay_component_desc", "assay_component_target_desc", "parameter_readout_type","assay_design_type", "assay_design_type_sub", "biological_process_target", "detection_technology_type", "detection_technology_type_sub", "detection_technology", "key_assay_reagent_type", "key_assay_reagent", "technological_target_type", "technological_target_type_sub")

Description <- c("Assay component ID",
                 "Assay ID",
                 "Assay component name (abbreviated \"acnm\" within the package)",
                 "Assay component description", 
                 "Assay component target description. Generally includes information about mechanism of action with assay target, how disruption is detected, or significance of target disruption.",
                 "Description of parameters measured", 
                "General description of the biological or physical process is translated into a detectable signal by assay mechanism",
                "Specific description of method through which a biological or physical process is translated into a detectable signal measured",
                "General biological process being chemically disrupted",
                "General description of assay platform or detection signals measured",
                "Description of signals measured in assay platform",
                "Specific description of assay platform used",
                "Type of critical reactant being measured",
                "Critical reactant measured",
                "General description of technological target measured in assay platform",
                "Specific description of technological target measured in assay platform"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        tfoot = "&dagger; Required fields for registration",
        caption="Table 17: Fields in assay_component."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("asid&dagger;", "assay_source_name&dagger;", "assay_source_long_name", "assay_source_desc")

Description <- c("Assay source ID",
                 "Assay source name (typically an abbreviation of the assay_source_long_name, abbreviated \"asnm\" within the package)",
                 "Full assay source name", 
                 "Assay source description"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        tfoot = "&dagger; Required fields for registration",
        caption="Table 18: Fields in assay_source."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("aeid", "acid", "assay_component_endpoint_name", "assay_component_endpoint_desc", "assay_function_type", "normalized_data_type&dagger;", "burst_assay&dagger;", "key_positive_control", "signal_direction", "intended_target_type", "intended_target_type_sub", "intended_target_family", "intended_target_family_sub", "fit_all&dagger;", "cell_viability_assay")
           
Description <- c("Assay component endpoint ID",
                 "Assay component ID",
                 "Assay component endpoint name (abbreviated \"aenm\" within the package)", 
                 "Assay component endpoint description",
                 "Description of targeted mechanism and the purpose of the analyzed readout in relation to others from the same assay",
                 "Normalization approach for which the data is displayed",
                 "Indicator if endpoint is included in the burst distribution (1) or not (0); Burst phenomenon can describe confounding activity, such as cytotoxicity due to non-specific activation of many targets at certain concentrations", 
                 "Tested chemical sample expected to produce activity; Used to assess assay validity",
                 "Directionality of raw data signals from assay (gain or loss); Defines analysis direction",
                "General group of intended targets measured",
                "Specific subgroup of intended targets measured", 
                "Family of intended target measured; Populated on ToxCast chemical activity plot within CompTox dashboard",
                "Specific subfamily of intended target measured",
                "Indicator if all results should be fit, regardless of whether max_med surpasses 3bmad cutoff (1) or not (0)",
                "Indicator of the impact of cytotoxicity in confounding (1) or no cytotoxic impact (0)" )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 19: Fields in assay_component_endpoint.",
        tfoot = "&dagger; Required fields for registration"
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("acid", "acsn")

Description <- c("Assay component ID",
                 "Assay component source name"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 20: Fields in assay_component_map table."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("chid", "casn", "chnm", "dsstox_substance_id")

Description <- c("Chemical ID&dagger;",
                 "CAS Registry Number",
                 "Chemical name",
                 "Unique identifier from U.S. EPA Distributed Structure-Searchable Toxicity (DSSTox) Database"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 21: Fields in chemical table." ,
        tfoot = "&dagger; This is the DSSTox GSID within the ToxCast data, but can be any integer and will be auto-generated (if not explicitly defined) for newly registered
chemicals"
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("chid", "clib")

Description <- c("Chemical ID",
                 "Chemical library"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 22: Fields in chemical_library table." 
     
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("spid", "chid", "stkc", "stkc_unit", "tested_conc_unit")

Description <- c("Sample ID",
                 "Chemical ID",
                 "Stock concentration" ,
                 "Stock concentration unit",
                 "The concentration unit for the concentration values in the data-containing tables"
                 )

output <- 
  data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 23: Fields in sample table." 
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("acsn", "spid", "cpid", "apid", "rowi", 
           "coli", "wllt", "wllq", "conc", "rval", "srcf")

Description <- c("Assay component source name",
                 "Sample ID",
                 "Chemical plate ID" ,
                 "Assay plate ID",
                 "Assay plate row index, as an integer",
                 "Assay plate column index, as an integer",
                 "Well type",
                 "1 if the well quality was acceptable, else 0",
                 "Concentration in micromolar",
                 "Raw assay component value or readout from vendor",
                 "Filename of the source file containing the data"
                 )
`N/A` <- c("No", "No", "Yes", "Yes","Yes","Yes", "No", "No", "No&dagger;", "Yes&ddagger;", "No")

output <- 
  data.frame(Field, Description, `N/A`)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        
          caption="Table 24: Required fields in level 0 pre-processing." ,
          tfoot = "The N/A column indicates whether the field can be N/A in the pre-processed data. 
 &dagger; In past versions of *tcpl*, there were some exceptions where concentrations could be N/A. For *tcpl_v3*, conc values must be numeric for processing since N/A values will result in processing error.  
 &ddagger;If the raw value is N/A, well quality must be 0."
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
`Well Type` <- c("t", "c", "p", "n", "m",  "o", "b", "v")

Description <- c("Test compound",
                 "Gain-of-signal control in multiple concentrations",
                 "Gain-of-signal control in single concentration" ,
                 "Neutral/negative control",
                 "Loss-of-signal control in multiple concentrations",
                 "Loss-of-signal control in single concentration",
                 "Blank well",
                 "Viability control"
                 )


output <- 
  data.frame(`Well Type`, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 25: Well types" 
)


