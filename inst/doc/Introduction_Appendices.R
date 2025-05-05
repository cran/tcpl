params <-
list(my_css = "css/rmdformats.css")

## ----setup, include = FALSE---------------------------------------------------
library(httptest)
start_vignette("api")

## ----echo=FALSE, include=FALSE, eval=FALSE------------------------------------
#  # devtools::load_all() #use this instead of lbrary(tcpl) when dev versions are installed locally

## ----message = FALSE, warning = FALSE,  class.source="fold-hide"--------------
# Primary Packages #
library(tcpl)
library(tcplfit2)
# Data Formatting Packages #
library(data.table)
library(dplyr)
library(magrittr)
library(reshape2)
library(knitr)
# Plotting Packages #
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(colorspace)
library(viridis)
# Table Packages #
library(htmlTable)
library(kableExtra)

## ----eval=FALSE, message=FALSE------------------------------------------------
#  library(data.table) # recommended for interacting with `tcpl` data frame-like objects
#  library(tcpl)

## ----setup-api, eval=FALSE----------------------------------------------------
#  tcplConf(pass = "API key provided by emailing CTX API support at ccte_api@epa.gov",
#           drvr = "API")

## ----eval = FALSE-------------------------------------------------------------
#  tcpl (v3.1.0) loaded with the following settings:
#    TCPL_DB:    NA
#    TCPL_USER:  NA
#    TCPL_HOST:  https://api-ccte.epa.gov/bioactivity
#    TCPL_DRVR:  API
#  Default settings stored in tcpl config file. See ?tcplConf for more information.

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
                 "Well type",
                 "Well quality: 1 was good, else 0",
                 "Concentration is micromolar",
                 "Raw assay component value or readout",
                 "Filename of the source file containing the data")

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped") 

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("s1id ", "s0id", "acid", "aeid", "conc", "bval", "pval", "resp")
Description <- c("Level 1 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Assay component endpoint ID",
                 "Concentration is micromolar",
                 "Baseline value",
                 "Positive control value",
                 "Normalized response value" )

output <-  data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("s2id ", "aeid", "spid", "bmad", "max_med", "coff", "hitc", "tmpi")
Description <- c("Level 2 ID",
                 "Assay component endpoint ID",
                 "Sample ID",
                 "Baseline median absolute deviation",
                 "Maximum median response value",
                 "Efficacy cutoff value",
                 "Binary hit call value, 1 if active, 0 if inactive",
                 "Ignore, temporary index used for uploading purposes" )

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("aeid ", "s0id", "s1id", "s2id")
Description <- c("Assay component endpoint ID",
                 "Level 0 ID",
                 "Level 1 ID",
                 "Level 2 ID" )

output <- 
  data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("s2id", "chid_rep")

Description <- c("Level 2 ID",
                 "Representative sample designation for a tested chemical: 1 if representative sample, else 0")

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m1id", "m0id", "acid", "cndx", "repi")
Description <- c("Level 1 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Concentration index",
                 "Replicate index" )

output <-  data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m2id", "m0id", "acid", "m1id", "cval")
Description <- c("Level 2 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Level 1 ID",
                 "Corrected value"   )

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m3id", "aeid", "m0id", "acid", "m1id", "m2id", "bval", "pval", "conc", "resp")
Description <- c("Level 3 ID",
                 "Assay endpoint ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Level 1 ID",
                 "Level 2 ID",
                 "Baseline value",
                 "Positive control value",
                 "Concentration is micromolar",
                 "Normalized response value")

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("aeid", "m0id", "m1id", "m2id", "m3id", "m4id")
Description <- c(
   "Assay endpoint ID","Level 0 ID",
                 "Level 1 ID",
                 "Level 2 ID",
                 "Level 3 ID",
                 "Level 4 ID" )

output <-  data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------

Field <- c("m4id", "aeid", "spid", "bmad", "resp_max", "resp_min", "max_mean", "max_mean_conc", "min_mean", "min_mean_conc", "max_med", "max_med_conc", "min_med", "min_med_conc", "max_med_diff", "max_med_diff_conc", "conc_max", "conc_min", "nconc", "npts", "nrep", "nmed_gtbl_pos", "nmed_gtbl_neg", "tmpi")


Description <- c("Level 4 ID",
                 "Assay endpoint ID",
                 "Sample ID",
                 "Baseline median absolute deviation",
                 "Maximum response value",
                 "Minimum response value",
                 "Maximal mean response at a given concentration",
                 "Corresponding concentration of *max_mean*",
                 "Minimum mean response value at a given concentration",
                 "Corresponding concentration of *min_mean*",
                 "Maximum median response value at a given concentration",
                 "Corresponding concentration of *max_med*",
                 "Minimum median response value at a given concentration",
                 "Corresponding concentration of *min_med*",
                 "Absolute difference between maximal and minimal median response at a given concentration",
                 "Absolute difference between corresponding concentration of max_med and min_med",
                 "Maximum concentration tested",
                 "Minimum concentration tested",
                 "Number of concentrations tested",
                 "Number of points in the concentration series",
                 "Number of replicates in the concentration series",
                 "Number of median response values greater than baseline of 3 * *BMAD*",
                 "Number of median response values less than baseline of -3 * *BMAD*",
                 "Ignore, temporary index used for uploading purposes"  )

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped") %>%
  kableExtra::scroll_box(width="100%", height="400px")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m4id", "aeid", "model", "model_param", "model_val")

Description <- c("Level 4 ID",
                 "Assay endpoint ID",
                 "Model that was fit",
                 "Key for the parameter that was fit with the corresponding model",
                 "Value for the associated key in the corresponding model")

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m5id", "m4id", "aeid", "modl", "hitc", "fitc", "coff", "actp", "model_type")

Description <- c("Level 5 ID",
                 "Level 4 ID",
                 "Assay endpoint ID",
                 "Winning model",
                 "Continuous activity hit call value ranging from -1 to 1" ,
                 "Fit category",
                 "Efficacy cutoff value",
                "Activity probability (1 - *const_prob* not used with *tcplFit2*)",
                "Model type. Options include: </br>
                2: Bidirectional: Data is fit bidirectionally.</br>
                3: Gain: Data is fit bidirectionally, but gain is the intended direction of response. Hit call (hitc) is multiplied by -1 if winning model is fit in the negative analysis direction. </br>
                4: Loss: Data is fit bidirectionally, but loss is the intended direction of response. Hit call (hitc) is multiplied by -1 if winning model is fit in the positive analysis direction." )

output <- data.frame(Field, Description)

htmlTable(output,
          align='l',
          align.header='l',
          rnames=FALSE,
          css.cell=' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m5id", "aeid", "hit_param", "hit_val")

Description <- c("Level 5 ID",
                 "Assay endpoint ID",
                 "Key for the parameter that was fit with winning model",
                 "Value for the associated key in the winning model" )

output <-  data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m5id", "chid_rep")

Description <- c("Level 5 ID",
                 "Representative sample designation for a tested chemical: 1 if representative sample, else 0" )

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m6id", "m5id", "m4id", "aeid", "mc6_mthd_id", "flag")

Description <- c("Level 6 ID",
                 "Level 5 ID",
                 "Level 4 ID",
                 "Assay endpoint ID",
                 "Level 6 method ID",
                 "Short flag description to be displayed in data retrieval and plotting. Extended description available in mc6_Methods table." )

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("m7id", "m4id", "aeid", "potency_val_type", "aed_type", "aed_val", "aed_val_unit", "interindividual_var_perc", "httk_model", "invitrodb_version", "httk_version")

Description <- c("Level 7 ID",
                 "Level 4 ID",
                 "Assay endpoint ID", "Potency value type used in the calc_mc_oral_equiv() calculation", 
                 "Descriptive vector that begins with “aed,” followed by potency metric used, followed by a short name of the httk model used, ending with the percentile from the modeled population with respect to interindividual variability", 
                 "Numeric value of the AED",
                 "Unit associated with AED, mg/kg/day", "Interindividual variability percentile, either 50th or 95th", 
                 "The httk model used; 3-compartment steady state (3compartmentss) or pbtk; note that all models used here were for adult humans.", 
                 "invitrodb version of data",
                 "Version of [httk R package](https://CRAN.R-project.org/package=httk) used" )

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Table <- c("assay_source", "assay", "assay_component", "assay_component_endpoint", "assay_component_map", "assay_descriptions**", "assay_reagent**", "assay_reference**",  "chemical", "chemical_analytical_qc**", "chemical_lists", "citations**", "gene**", "intended_target**", "organism**", "sample")

Description <- c("Assay source-level annotation", "Assay-level annotation",
                 "Assay component-level annotation",
                 "Assay endpoint-level annotation",
                 "Assay component source names and their corresponding assay component ids",
                 "Additional assay descriptions curated per OECD Guidance Document 211 (GD211)",
                 "Assay reagent information",
                 "Map of citations to assay",
                 "List of chemicals and associated identifiers",
                 "Analytical QC information curated at the chemical substance or sample level to inform applicability domain",
                 "Lists of chemicals and their presence in curated chemical lists",
                 "List of citations",
                 "Gene identifiers and descriptions",
                 "Intended assay target at the assay endpoint level",
                 "Organism identifiers and descriptions",
                 "Sample identifiers and chemical provenance information")

output <- data.frame(Table, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("asid", "assay_source_name", "assay_source_long_name", "assay_source_desc")

Description <- c("Assay source ID. Required for registration.",
                 "Assay source name, typically an abbreviation of the assay_source_long_name and abbreviated \"asnm\" within the package. Required for registration",
                 "Full assay source name", 
                 "Assay source description"
                 )

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("aid", "asid;", "assay_name", "assay_desc", "timepoint_hr", 
            "ncbi_taxon_id", "organism",'tissue',"cell_format",
            'cell_free_component_source',
            'cell_short_name', 
            'cell_growth_mode',
            "assay_footprint", 
            "assay_format_type" ,
            "assay_format_type_sub" ,
            "content_readout_type",  
            "dilution_solvent" , 
            "dilution_solvent_percent_max")

Description <- c("Assay ID",
                 "Assay source ID. Required for registration.",
                 "Assay name, abbreviated \"anm\" within the package. Required for registration.",
                 "Assay description",
                 "Treatment duration in hours",
                 "NCBI taxonomic identifier for organism, available at https://www.ncbi.nlm.nih.gov/taxonomy",
                 "Organism of origin",
                 "Tissue of origin", "Description of cell format",
                 "Description of source for targeted cell-free components",
                 "Abbreviation of cell line",
                 "Cell growth modality", 
                 "Microtiter plate size. Required for registration.",
                 "General description of assay format",
                 "Specific description of assay format" ,
                 "Description of well characteristics being measured", 
                 "Solvent used in sample dilution",
                 "Maximum percent of dilution solvent used, from 0 to 1")

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped") %>%
  kableExtra::scroll_box(width="100%", height="400px")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("acid", "aid", "assay_component_name", "assay_component_desc", "assay_component_target_desc", "parameter_readout_type","assay_design_type", "assay_design_type_sub", "biological_process_target", "detection_technology_type", "detection_technology_type_sub", "detection_technology", "key_assay_reagent_type", "key_assay_reagent", "technological_target_type", "technological_target_type_sub")

Description <- c("Assay component ID",
                 "Assay ID. Required for registration.",
                 "Assay component name, abbreviated \"acnm\" within the package. Required for registration.",
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
                "Specific description of technological target measured in assay platform")

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped") %>%
  kableExtra::scroll_box(width="100%", height="400px")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("aeid", "acid", "assay_component_endpoint_name", "assay_component_endpoint_desc", "assay_function_type", "normalized_data_type", "burst_assay", "key_positive_control", "signal_direction", "intended_target_type", "intended_target_type_sub", "intended_target_family", "intended_target_family_sub", "cell_viability_assay")
           
Description <- c("Assay component endpoint ID",
                 "Assay component ID. Required for registration.",
                 "Assay component endpoint name, abbreviated \"aenm\" within the package. Required for registration.", 
                 "Assay component endpoint description",
                 "Description of targeted mechanism and the purpose of the analyzed readout in relation to others from the same assay",
                 "Normalization approach for which the data is displayed",
                 "Indicator if endpoint is included in the burst distribution (1) or not (0); Burst phenomenon can describe confounding activity, such as cytotoxicity due to non-specific activation of many targets at certain concentrations. Required for registration.", 
                 "Tested chemical sample expected to produce activity; Used to assess assay validity",
                 "Directionality of raw data signals from assay (gain or loss); Defines analysis direction",
                 "General group of intended targets measured",
                 "Specific subgroup of intended targets measured", 
                 "Family of intended target measured; Populated on ToxCast chemical activity plot within CompTox dashboard",
                 "Specific subfamily of intended target measured",
                 "Indicator of the impact of cytotoxicity in confounding (1) or no cytotoxic impact (0)" )

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped") %>%
  kableExtra::scroll_box(width="100%", height="400px")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("acid", "acsn")

Description <- c("Assay component ID",
                 "Assay component source name" )

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("aeid", "assay_title", "assay_objectives", "assay_throughput", "scientific_principles", "biological_responses", "analytical_description", 
           "basic_procedures", "experimental_system", "xenobiotic_biotransformation", "proprietary_elements")
           
Description <- c("Assay component endpoint ID", 
                 "Short and descriptive title for the assay; opposed to assay component endpoint name", 
                 "Purpose of the test method: Inserted after assay_component_target_desc; the claimed purpose and rationale for intended use of the method (e.g. alternative to an existing method, screening, provision of novel information in regulatory decision-making, mechanistic information, adjunct test, replacement, etc.) should be explicitly described and documented. The response measured in the assay should be put in the context of the biology/physiology leading to the in vivo response or effect. If the biological activity or response refers to a key event or molecular initiating event (MIE), provide a short description indicating what key event within an existing or developing AOP, or in relation to a mechanism or mode of action, the assay is aiming to characterize (i.e. which level of biological organization the assay may be attributed (e.g. sub-cellular, cellular, tissue, organ or individual), and where the assay might fit in the context of an existing regulatory hazard (i.e. adverse outcome). In the absence of any AOP, provide an indication of the plausible linkage between the mechanism(s) the assay is measuring and the resulting hazard endpoint.", 
                 "Information about the throughput of the assay: Indicate the throughput of the assay to provide an indication of likely resource intensity e.g. low (manual assay, one chemical tested at a time), lowmoderate, moderate, moderate-high, high throughput (e.g. in 96 well-plate and higher), and qualify with e.g. approximate number of chemicals/concentrations per run. If appropriate indicate whether a manual assay could be run in a higher throughput mode.", 
                 "Scientific principle of the method: Provide the scientific rationale, supported by bibliographic references to articles, for the development of the assay. A summary description of the scientific principle including the biological/physiological basis and relevance (e.g. modeling of a specific organ) and/or mechanistic basis (e.g. modeling a particular mechanism by biochemical parameters) should be described. If possible, indicate what the anchor point is within an AOP.", 
                 "Response and Response Measurement: Response here makes reference to any biological effect, process, or activity that can be measured. Specify precisely and describe the response and its measurement, e.g. corneal opacity measured using an opacitometer; half maximal activity concentration (AC50) derived from a competitive binding assay in human estrogen receptor assay or from the up-regulation of the proinflammatory antiangiogenic chemokine CXCL10.", 
                 "Data analysis: Comment on the response value in terms of a boundary or range to provide a context for interpretation.", 
                 "Description of the experimental system exposure regime: Provide a summary description of the essential information pertaining to the exposure regime (dosage and exposure time including observation frequency) of the test compounds to the experimental system including information on metabolic competence if appropriate; number of doses/concentrations tested or testing range, number of replicates, the use of control(s) and vehicle. Also, describe any specialized equipment needed to perform the assay and measure the response. Indicate whether there might be potential solubility issues with the test system, and solutions proposed to address the issue.", 
                 "Tissue, cells or extracts utilised in the assay and the species source: indicate the experimental system for the activity or response being measured.", "Metabolic competence of the test system: Describe and discuss the extent to which the test system can be considered metabolically competent, either by itself, or with the addition of an enzymatic fraction, if appropriate. Provide reference if available.", 
                 "Status of method development and uses: Compile information for the following sections if appropriate. Considerations could include: i) Development status: Indicate if the assay is still under development, and the estimated timeline for completion as far as possible ii) Known uses: Summarise the current and/or past use of the assay by different laboratories iii) Evaluation study: Summarise the main conclusions or refer to individual protocol if available iv) Validation study: Indicate participation in a formal validation study/studies and summarise the conclusions and their outcomes or refer to the individual protocol if available v) Regulatory use: Provide details of any potential regulatory application and of the toxicological hazard endpoint being addressed by the assay.")

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped") %>%
  kableExtra::scroll_box(width="100%", height="400px")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("chid", "casn", "chnm", "dsstox_substance_id")

Description <- c("Chemical ID", "CAS Registry Number", "Chemical name",
                 "Unique identifier from U.S. EPA Distributed Structure-Searchable Toxicity (DSSTox) Database")

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("chemical_lists_id", "chid", "dsstox_substance_id", "list_acronym", "list_name", "list_desc")

Description <- c("Chemical List ID", "Chemical ID", "Unique identifier from U.S. EPA Distributed Structure-Searchable Toxicity (DSSTox) Database", 
                 "Chemical list acronym", "Chemical list name", "Chemical list description")

output <-  data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("spid", "chid", "stkc", "stkc_unit", "tested_conc_unit", "bottle_barcode", "source",
           "bottle_type", "lot_number", "purity_percentage", "solubility")

Description <- c("Sample ID",
                 "Chemical ID",
                 "Stock concentration" ,
                 "Stock concentration unit",
                 "The concentration unit for the concentration values in the data-containing tables", 
                 "Bottle barcode of sample", "Source (i.e. manufacturer or supplier) of procured bottle", 
                 "Type of bottle, which can reflect storage condition", "Lot or batch number of bottle", 
                 "Reported purity percentage from bottle Certificate of Analysis (CoA)", "Solubility")

output <- data.frame(Field, Description)

kable(output)%>% 
  kable_styling("striped")

## ----eval = FALSE, message = FALSE--------------------------------------------
#  tcplLoadAsid()
#  tcplRegister(what = "asid", flds = list(asid = 1, asnm = "Tox21"))

## ----eval = FALSE, message = FALSE--------------------------------------------
#  tcplLoadAid(what = "asid", val = 1)
#  tcplRegister(what = "aid", flds = list(asid = 1, anm = "TOX21_ERa_BLA_Agonist", assay_footprint = "1536 well"))

## ----eval = FALSE, message = FALSE--------------------------------------------
#  tcplLoadAcid(what = "asid", val = 1, add.fld = c("aid", "anm"))
#  tcplRegister(what = "acid", flds = list(aid = 1, acnm = "TOX21_ERa_BLA_Agonist_ratio"))

## ----eval = FALSE, message = FALSE--------------------------------------------
#  tcplRegister(what = "acsn", flds = list(acid = 1, acsn = "TCPL-mc-Demo"))

## ----eval = FALSE, message = FALSE--------------------------------------------
#  tcplLoadAeid(fld = "asid", val = 1, add.fld = c("aid", "anm", "acid", "acnm"))
#  tcplRegister(what = "aeid", flds = list(acid = 1, aenm = "TOX21_ERa_BLA_Agonist_ratio", normalized_data_type = "percent_activity", export_ready = 1, burst_assay = 0))

## ----eval = FALSE, message = FALSE--------------------------------------------
#  tcplUpdate(what = "acid", flds = list(aid = 1, acnm = "TOX21_ERa_BLA_Agonist_ratio"))

## ----echo=FALSE---------------------------------------------------------------
chdat <- data.table(
  spid = c("Tox21_400088","Tox21_303655","Tox21_110011","Tox21_400081","DMSO","Tox21_400037"),
  casn = c("80-05-7","521-18-6","150-30-1","22224-92-6","67-68-5","95-83-0"),
  chnm = c("Bisphenol A","5alpha-Dihydrotestosterone","Phenylalanine","Fenamiphos","Dimethyl sulfoxide","4-Chloro-1,2-diaminobenzene"),
  dsstox_substance_id = c("DTXSID7020182","DTXSID9022364","DTXSID9023463","DTXSID3024102","DTXSID2021735","DTXSID5020283"),
  code = c("C80057","C521186","C150301","C22224926","C67685","C95830"),
  chid = c("20182","22364","23463","24102","21735","20283")
)

kable(chdat)%>% 
  kable_styling("striped")

## ----eval = FALSE-------------------------------------------------------------
#  # Obtain chemicals already registered in the database.
#  cmap <- tcplLoadChem()
#  # Find chemicals in 'chdat' that are not registered yet.
#  chdat.register <- chdat[!(chdat$code %in% cmap$code)]
#  # Register the chemicals not yet in the database.
#  tcplRegister(what = "chid", flds = chdat.register[,unique(.SD), .SDcols = c("casn", "chnm", "dsstox_substance_id", "code", "chid")])

## ----eval = FALSE, message = FALSE--------------------------------------------
#  tcplRegister(what = "spid",
#               flds = merge(chdat[ , list(spid, casn)],
#                            chdat.register[ , list(casn, chid)], by = "casn")[ , list(spid, chid)])

## ----eval = FALSE-------------------------------------------------------------
#  tcplLoadChemList(field = "chid", val = 1:2)

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("'acsn' or 'acid'", "spid", "apid", "rowi", "coli", "wllt", "wllq", "conc", "rval", "srcf")

Description <- c("Assay component source name can be used to map to assay component ID, or acid can be directly provided",
                 "Sample ID",
                 "Assay plate ID",
                 "Assay plate row index, as an integer",
                 "Assay plate column index, as an integer",
                 "Well type",
                 "Well quality: 1 was good, else 0",
                 "Concentration in micromolar",
                 "Raw assay component value or readout from vendor",
                 "Filename of the source file containing the data"  )
Required_NULL_allowed <- c("No", "No", "Yes","Yes","Yes", "No", "No", "No", "Yes", "No")

output <- data.frame(Field, Description, Required_NULL_allowed)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
`Well Type` <- c("t", "c", "p", "n", "m",  "o", "b", "v")

Description <- c("Test compound",
                 "Gain-of-signal control in multiple concentrations",
                 "Gain-of-signal control in single concentration" ,
                 "Neutral/negative control",
                 "Loss-of-signal control in multiple concentrations",
                 "Loss-of-signal control in single concentration",
                 "Blank well",
                 "Viability control" )
output <-  data.frame(`Well Type`, Description)

kable(output)%>% 
  kable_styling("striped")

## ----eval = FALSE, message = FALSE--------------------------------------------
#  # Write/load the Level 0 into the database.
#  tcplWriteLvl0(dat = mcdat, type = "mc")

## ----eval = FALSE, message = FALSE--------------------------------------------
#  # Load the level 0 data from the database to R
#  tcplLoadData(lvl = 0, fld = "acid", val = 1, type = "mc")
#  tcplPrepOtpt(tcplLoadData(lvl = 0, fld = "acid", val = 1, type = "mc"))

## ----warning = FALSE, echo = FALSE--------------------------------------------
Type <- c('SC', 'SC', 'mc', 'mc', 'mc', 'mc', 'mc', 'mc')
Level <- c('Lvl1', 'Lvl2', 'Lvl1', 'Lvl2', 'Lvl3', 'Lvl4', 'Lvl5', 'Lvl6')
InputID <- c('acid', 'aeid', 'acid', 'acid', 'acid', 'aeid', 'aeid', 'aeid')
MethodID <- c('aeid', 'aeid', 'N/A', 'acid', 'aeid', 'N/A', 'aeid', 'aeid')
output <- data.frame(Type, Level, InputID, MethodID)

kable(output)%>% 
  kable_styling("striped")

## ----eval= FALSE--------------------------------------------------------------
#  ## Methods Assignment ##
#  # For illustrative purposes, assign level 2 mc methods to ACIDs 97, 98, and 99.
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
#  # Methods can be cleared one at a time for the given ID(s)
#  tcplMthdClear(lvl = 2, id = 99, mthd_id = 2, type = "mc")
#  # Check the assigned methods for the single ID updated, namely ACID 99.
#  tcplMthdLoad(lvl = 2, id = 99, type = "mc")
#  
#  # Clear assigned methods for the given ID(s)
#  tcplMthdClear(lvl = 2, id = 97:98, type = "mc")
#  # Check the assigned methods for the all updated ID(s), namely ACID 97 and 98.
#  tcplMthdLoad(lvl = 2, id = 97:98, type = "mc")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Normalization <- c('', 'Fold Change', '%Control')
Scheme_1 <- c('Scheme 1', '1. bval.apid.nwlls.med</br> 2. resp.fc</br> 3. resp.log2 </br> 4. resp.mult.neg1', 
              '1. bval.apid.lowconc.med</br> 2. bval.apid.pwlls.med</br> 3. resp.pc</br> 4. resp.multneg1')
Scheme_2 <- c('Scheme 2', '1. bval.apid.lowconc.med</br> 2. resp.fc</br> 3. resp.log2', 
              '1. bval.spid.lowconc.med</br> 2. pval.apid.mwlls.med</br> 3. resp.pc')
Scheme_3 <- c('Scheme 3', '1. none</br> 2. resp.log10</br> 3. resp.blineshift.50.spid', 
              '1. none</br> 2. resp.multneg1')

output <- t(data.frame(Normalization, Scheme_1, Scheme_2, Scheme_3))

# Export/print the table to an html rendered table.
htmlTable(output,
          align = 'l',
          align.header = 'l',
          rnames = FALSE  ,
          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
          caption = "Examples of Normalization Schemes"
          )

## ----warning = FALSE, echo = FALSE--------------------------------------------
Level <- c(" Lvl 0", "Lvl 1  ", "Lvl 2  ")
Description <- c("Pre-processing: Vendor/dataset-specific pre-processing to organize heterogeneous raw data to the uniform format for processing by the *tcpl* package",
                 "Normalize: Apply assay endpoint-specific normalization listed in the \'sc1_aeid\' table to the raw data to define response",
                 "Activity Call: Collapse replicates by median response, define the response cutoff based on methods in the \'sc2_aeid\' table, and determine activity" )

output <- data.frame(Level, Description)

kable(output)%>% 
  kable_styling("striped")

## ----eval = FALSE-------------------------------------------------------------
#  # Load the 'aeid' values for acid 2
#  tcplLoadAeid(fld = "acid", val = 2)
#  
#  # Assign the Level 1 methods to aeid 1 and 2
#  tcplMthdAssign(lvl = 1,  # processing level
#                 id = 1:2, # assay endpoint ID's to assign methods
#                 mthd_id = c(1, 11, 13), # method(s) to be assigned
#                 ordr = 1:3, # order the method(s) should be applied
#                 type = "sc") # the data/processing type

## ----eval = FALSE-------------------------------------------------------------
#  # Assign an additional method to invert data for AEID 2 only
#  tcplMthdAssign(lvl = 1, # processing level
#                 id = 2, # assay endpoint ID's to assign methods
#                 mthd_id = 16, # method(s) to be assigned
#                 ordr = 4, # order the method(s) should be applied
#                 type = "sc") # the data/processing type

## ----echo=FALSE, eval = FALSE-------------------------------------------------
#  # Run Level 1 processing for acid 1
#  tcplRun(id = 1, slvl = 1, elvl = 1, type = "sc")

## ----warning = FALSE, echo = FALSE--------------------------------------------
## Create the sc BMAD calculation Table ##
# Specify column 1 in the table - Methods.
Method <- c(1,2)
# Specify column 2 in the table - Description.
Description <- c("Median absolute deviation (MAD) of all treatment wells across the assay component (acid).",
  "Median absolute deviation (MAD) of all blank wells across the assay component (acid).")
# Specify column 3 in the table - Observations.
Observations <- c(
  "$y_{i} = y_{(s,w)}$", # method 1
  "$y_{i} = y_{(s,w)}$" # method 2
  )
# Specify column 4 in the table - Observation ID.
ID <- c(
  "$s \\in \\{1,...,n_{acid}\\}$, \n$w = t$",
  "$s \\in \\{1,...,n_{acid}\\}$, \n$w = n$")
# Specify column 5 in the table - Details about the Observation ID.
Details <- c( "$s$ indicates the sample ID within an 'acid' & $w$ indicates the well type",
  "$s$ indicates the sample ID within an 'acid' & $w$ indicates the well type")
# Create the output table.
output <- data.frame(Method,Description,Observations,ID,Details)

kable(output)%>% 
  kable_styling("striped")

## ----eval = FALSE-------------------------------------------------------------
#  # Assign a cutoff value of log2(1.2)
#  tcplMthdAssign(lvl = 2, # processing level
#                 id = 1,  # assay endpoint ID's to assign methods
#                 mthd_id = 3, # method(s) to be assigned
#                 type = "sc") # the data/processing type
#  
#  # Run Level 2 processing for acid 1
#  tcplRun(id = 1, slvl = 2, elvl = 2, type = "sc")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Method <- c(25,27)
Method_Name <- c( "ow_bidirectional_gain", "ow_bidirectional_loss")
Description <- c(
  "Reponses in in the positive direction only are biologically relevant, therefore overwrite the max_med and max_tmp values, which were calculated using absolute value, to a calculation using a true maximum for uni-directional data.",
  "Responses in the negative direction only are biologically relevant, therefore overwrite the max_med and max_tmp values, which were calculated using absolute value, to a calculation using a true minimum for uni-directional data.")

output <- data.frame(Method,Method_Name,Description)

kable(output)%>% 
  kable_styling("striped")

## ----warning = FALSE, echo = FALSE--------------------------------------------
Level <- c("Lvl 0 ", "Lvl 1", "Lvl 2", "Lvl 3", "Lvl 4", "Lvl 5", "Lvl 6", "Lvl 7")
Description <- c("Pre-processing: Vendor/dataset-specific pre-processing to organize heterogeneous raw data to the uniform format for processing by the *tcpl* package",
                 "Index: Define the replicate and concentration indices to facilitate
all subsequent processing",
                 "Transform: Apply assay component (acid) specifc transformations
listed in the \'mc2_acid\' table to the raw data to define the
corrected data",
"Normalize: Apply assay endpoint (aeid) specifc normalization listed in
the \'mc3_aeid\' table to the corrected data to define response",
"Fit: Model the concentration-response data utilizing ten
objective curve fitting functions from tcplfit2: (1) constant, (2) hill, (3) gain-loss, (4) polynomial-linear, (5) polynomial-quadratic, (6) power, (7) exponential-2, (8) exponential-3, (9) exponential-4, (10) exponential-5",
"Model Selection/Activity Call: Select the winning model, define
the response cutoff based on methods in the \'mc5_aeid\' table, and
determine activity",
"Flag: Flag potential false positive and false negative fits", 
"Extrapolate: Convert bioactive concentrations to Adminstered Equivalent Doses"  )
output <- data.frame(Level, Description)

kable(output)%>% 
  kable_styling("striped")

## ----echo=FALSE, eval = FALSE, message = FALSE--------------------------------
#  # Run Level 1 processing for acid 1
#  tcplRun(id = 1, slvl = 1, elvl = 1, type = "mc")
#  
#  ## Evaluate mc1 Indexing ##
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
#  m1dat[chnm == "Bisphenol A", list(chnm, conc, cndx, repi)]

## ----eval = FALSE, warning = FALSE, message = FALSE, fig.width = 30, fig.height= 20----
#  tcplPlotPlate(dat = m1dat, apid = "4009721")

## ----eval = FALSE, message = FALSE--------------------------------------------
#  # Assign the level 2 transformation method 'none' to ACID 1.
#  tcplMthdAssign(lvl = 2, # processing level
#                 id = 1, # assay component ID's to assign methods
#                 mthd_id = 1, # method(s) to be assigned
#                 ordr = 1, # order of the method(s) should be assigned
#                 type = "mc") # the data/processing type
#  
#  # Run Level 2 processing for acid 1
#  tcplRun(id = 1, slvl = 2, elvl = 2, type = "mc")

## ----eval = FALSE-------------------------------------------------------------
#  # Look at the assay endpoints for acid 1
#  tcplLoadAeid(fld = "acid", val = 1)
#  
#  ## Methods Assignment
#  # Assign the baseline calculation and normalization methods to aeids 1 and 2.
#  tcplMthdAssign(lvl = 3, # processing level
#                 id = 1:2, # assay endpoint ID to assign methods
#                 mthd_id = c(17, 9, 7), # method(s) to be assigned
#                 ordr = 1:3, # order the method(s) should be applied
#                 type = "mc") # the data/processing type
#  
#  ## Run Level 3 processing for acid 1
#  tcplRun(id = 1, slvl = 3, elvl = 3, type = "mc")

## ----warning = FALSE, echo = FALSE--------------------------------------------
# First column with the method assignment index.
Method <- c(1,2)

# Second column with the general methods description.
Description <- c(
  "Median absolute deviation (MAD) of all observations in the lowest two concentrations of across samples (spid) in the assay endpoint (aeid). </br>
  Standard deviation (SD) of all observations in the lowest two concentrations of across samples (spid) in the assay endpoint (aeid).",
  "Median absolute deviation (MAD) of all observations in the solvent/untreated control observations across samples (spid) in the assay endpoint (aeid).  </br>
  Standard deviation (SD) of all observations solvent/untreated control observations of across samples (spid) in the assay endpoint (aeid)."
)

# Third column with the observation information.
Observations <- c(
  "$y_{i} = y_{(s,w,d)}$", # method 1
  "$y_{i} = y_{(s,w)}$" # method 2
)

# Fourth column with the observation ID information.
ID <- c(
  "$s \\in \\{1,...,n_{aeid}\\}$, \n$w = t$, \n$d \\in \\{ 1,2 \\}$",
  "$s \\in \\{1,...,n_{aeid}\\}$, \n$w = n$"
)

# Fifth column with the details on the ID's.
Details <- c(
  "$s$ indicates the sample ID within an 'aeid', $w$ indicates the well type, & $d$ indicates the concentration group index",
  "$s$ indicates the sample ID id within an 'aeid', $w$ indicates the well type")

# Compile all of the information for the table.
output <- data.frame(Method,Description,Observations,ID,Details)

# Export/print the table to an html rendered table.
htmlTable(output,
          align = 'l',
          align.header = 'l',
          rnames = FALSE  ,
          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ' )

## ----eval=FALSE---------------------------------------------------------------
#  ## Evaluate the mc3 Data ##
#  # Load the mc3 data from the database
#  mc3 <- tcplLoadData(lvl = 3,
#                      type = 'mc',
#                      fld = 'aeid',
#                      val = 80)
#  # Prepare the data into a readable format
#  mc3 <- tcplPrepOtpt(mc3)

## ----class.source="fold-hide", fig.align='center',message=FALSE,message=FALSE,fig.dim=c(8,10),eval = TRUE----
# Load the example data from the `tcpl` package.
data(mc_vignette, package = 'tcpl')
# Allocate the level 3 example data to `mc3`.
mc3_example <- mc_vignette[['mc3']]
# level 3 does not store logc anymore, create it for plotting purposes
mc3_example[, logc := log10(conc)]
# Obtain the mc4 example data.
mc4_example <- mc_vignette[["mc4"]]
# Obtain the minimum response observed and the 'logc' group - 'resp_min'.

level3_min <- mc3_example %>%
  group_by(spid, chnm) %>% 
  filter(resp == min(resp)) %>% 
  filter(spid == "01504209")
# Obtain the maximum response observed and the 'logc' group - 'resp_max'.
level3_max <- mc3_example %>% 
  group_by(spid, chnm) %>% 
  filter(resp == max(resp)) %>% 
  filter(spid == "01504209")
# Obtain the level 3 data and 'center' estimates for responses per 'logc' group.
level3_summary <- mc3_example %>%
  filter(spid == "01504209") %>% 
  select(., c(spid, chnm, logc, resp)) %>%
  group_by(spid, chnm, logc) %>% 
  summarise(mean_resp = mean(resp), med_resp = median(resp))

## Generate Individual Summary Plots ##
# Plot the mean responses for each log-concentration group.
A <- mc3_example %>%
  filter(spid == "01504209") %>% 
  ggplot(data = ., aes(logc, resp)) +
  geom_point(pch = 1, size = 2) +
  geom_point(data = level3_summary,
             aes(x = logc, y = mean_resp,
                 col = 'mean responses'),
             alpha = 0.75,size = 2) +
  scale_color_manual(values = 'paleturquoise3',
                     aesthetics = 'col') +
  labs(lty = "", colour = "")+
  xlab(expression(paste(log[10],"(Concentration) ", mu, "M"))) +
  ylab(expression(paste(log[2], "(Fold Induction)"))) +
  ggtitle("Mean Responses") +
  theme_bw() +
  theme(legend.position = 'bottom')
# Plot the median responses for each log-concentration group.
B <- mc3_example %>%
  filter(spid == "01504209") %>% 
  ggplot(data = .,aes(logc,resp)) +
  geom_point(pch = 1, size = 2) +
  geom_point(data = level3_summary,
             aes(x = logc, y = med_resp,
                 col = 'median response'),
             alpha = 0.75, size = 2) +
  scale_color_manual(values = 'hotpink',
                     aesthetics = 'col') +
  labs(lty = "", colour = "")+
  xlab(expression(paste(log[10], "(Concentration) ", mu, "M"))) +
  ylab(expression(paste(log[2], "(Fold Induction)"))) +
  ggtitle("Median Responses") +
  theme_bw() +
  theme(legend.position = 'bottom')
# Plot the maximum mean & median responses at the related log-concentration -
#   'max_mean' & 'max_mean_conc'.

C <- mc3_example %>%
  filter(spid == "01504209") %>% 
  ggplot(data = .,aes(logc, resp)) +
  geom_point(pch = 1, size = 2) +
  geom_point(data = filter(mc4_example, spid == "01504209"),
             aes(x = log10(max_mean_conc), y = max_mean,
                 col = 'maximum mean response'),
             alpha = 0.75, size = 2)+
  scale_color_manual(values = 'paleturquoise3',
                     aesthetics = 'col') +
  labs(lty = "", colour = "")+
  xlab(expression(paste(log[10], "(Concentration) ", mu, "M"))) +
  ylab(expression(paste(log[2], "(Fold Induction)"))) +
  ggtitle(label = "Maximum Mean Response") +
  theme_bw() +
  theme(legend.position = 'bottom')
# Plot the maximum mean & median responses at the related log-concentration -
#   'max_med' & 'max_med_conc'.
D <- mc3_example %>%
  filter(spid == "01504209") %>% 
  ggplot(data = ., aes(logc, resp)) +
  geom_point(pch = 1, size = 2) +
  geom_point(data = filter(mc4_example, spid == "01504209"),
             aes(x = log10(max_med_conc), y = max_med,
                 col = "maximum median response"),
             alpha = 0.75, size = 2)+
  scale_color_manual(values = 'hotpink',
                     aesthetics = 'col') +
  labs(lty = "", colour = "") +
  xlab(expression(paste(log[10], "(Concentration) ", mu, "M"))) +
  ylab(expression(paste(log[2], "(Fold Induction)"))) +
  ggtitle(label = "Maximum Median Response") +
  theme_bw() +
  theme(legend.position = 'bottom')
# Plot the minimum & maximum observed responses.
E <- mc3_example %>%
  filter(spid == "01504209") %>% 
  ggplot(data = ., aes(logc, resp)) +
  geom_point(pch = 1, size = 2) +
  geom_point(data = level3_min,
             aes(x = logc, y = resp,
                 col = "minimum response"),
             alpha = 0.75, size = 2) +
  geom_point(data = level3_max,
             aes(x = logc, y = resp,
                 col = "maximum response"),
             alpha = 0.75, size = 2) +
  scale_color_manual(values = c('red', 'blue'),
                     aesthetics = 'col') +
  labs(lty = "", colour = "") +
  xlab(expression(paste(log[10], "(Concentration) ", mu,"M"))) +
  ylab(expression(paste(log[2], "(Fold Induction)"))) +
  ggtitle(label = "Minimum & Maximum\nResponses") +
  theme_bw() +
  theme(legend.position = 'bottom')
# Plot the minimum & maximum experimental log-concentration groups -
#   'logc_min' & 'logc_max'.
G <- mc3_example %>%
  filter(spid == "01504209") %>% 
  ggplot(data = ., aes(logc, resp)) +
  geom_point(pch = 1, size = 2) +
  geom_vline(data = filter(mc4_example, spid == "01504209"),
             aes(xintercept = log10(conc_min),
                 col = 'minimum concentration'),
             lty = "dashed") +
  geom_vline(data = filter(mc4_example, spid == "01504209"),
             aes(xintercept = log10(conc_max),
                 col = 'maximum concentration'),
             lty = "dashed") +
  scale_color_manual(values = c('red', 'blue'),
                     aesthetics = 'col') +
  labs(lty = "", colour = "") +
  xlab(expression(paste(log[10], "(Concentration) ", mu, "M"))) +
  ylab(expression(paste(log[2], "(Fold Induction)"))) +
  ggtitle(label = "Minimum & Maximum\nConcentrations") +
  theme_bw() +
  theme(legend.position = 'bottom')
## Compile Summary Plots in One Figure ##

aenm = "TOX21_ERa_BLA_Agonist_ratio"
spid = "01504209"
gridExtra::grid.arrange(
  A,B,C,D,E,G,
  nrow = 3, ncol = 2,
  top = mc3_example[which(mc4_example[,spid] == "01504209"), aenm]
)

## ----warning = FALSE, echo = FALSE--------------------------------------------
# First column - tcplfit2 available models.
Model <- c(
  "Constant", "Linear", "Quadratic","Quadratic","Power", "Hill", "Gain-Loss",
  "Exponential 2", "Exponential 3","Exponential 4", "Exponential 5"
)
# Second column - model abbreviations used in invitrodb & tcplfit2.
Abbreviation <- c(
  "cnst", "poly1", "poly2-monotonic only","poly2-biphasic","pow", "hill", "gnls",
  "exp2", "exp3", "exp4", "exp5"
)
# Third column - model equations.
Equations <- c(
  "$f(x) = 0$", # constant
  "$f(x) = ax$", # linear
  "$f(x) = a(\\frac{x}{b}+(\\frac{x}{b})^{2})$", # quadratic
  "$f(x) = b1*x + b2*x^{2}$", # biphasic poly2
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
  "a (y-scale) </br> b (x-scale)", # quadratic
  "a (y-scale) </br> b (x-scale)", # quadratic
  "a (y-scale) </br> p (power)", # power
  "tp (top parameter) </br> ga (gain AC50) </br> p (gain-power)", # hill
  "tp (top parameter) </br> ga (gain AC50) </br> p (gain power) </br> la (loss AC50) </br> q (loss power)", # gain-loss
  "a (y-scale) </br> b (x-scale)", # exp2
  "a (y-scale) </br> b (x-scale) </br> p (power)", # exp3
  "tp (top parameter) </br> ga (AC50)", # exp4
  "tp (top parameter) </br> ga (AC50) </br> p (power)" # exp5
)
# Fifth column - additional model details.
Details <- c(
  "Parameters always equals 'er'.", # constant
  "", # linear 
  "", # quadratic
  "", # biphasic poly2
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
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em '
)


## ----warning = FALSE, echo = FALSE--------------------------------------------
# First column - tcplfit2 additional fit parameters.
FitParameters <- c("er", "success", "cov", "aic", "rme", "modl",
  "parameters", "parameters sds", "pars", "sds")
# Second column - description of additional fit parameters.
Description <- c(
  "Error term","Success of Fit/Model Convergenece","Success of Covariance",
  "Akaike Information Criteria", "Root Mean Squared Error",
  "Vector of Model Estimated Values at Given Concentrations",
  "Model Parameter Values", "Standard deviation of Model Parameter Values",
  "Vector of Parameter Names","Vectors of Parameter Standard Deviation Names")
# Consolidate all columns into a table.
output <- data.frame(FitParameters, Description)
# Export/print the table into an html rendered table.
kable(output)%>% 
  kable_styling("striped")

## ----class.source="fold-hide", fig.align='center'-----------------------------
## Example Data ##
# example fit concentration series
ex_conc <- seq(0.03, 100, length.out = 100)

## Obtain the Continuous Fit of Level 4 Model Estimates ##
fits <- data.frame(
  # log-scale concentrations
  logc = log10(ex_conc),
  # illustrate example parametric model fits from `tcplfit2`
   constant = cnst(ps = c(er = 0.1), ex_conc),
   poly1    = poly1(ps = c(a = 3.5, er = 0.1),x = ex_conc),
   poly2.mono.only = poly2(ps = c(a = 0.13, b = 2, er = 0.1), x = ex_conc),
   poly2.biphasic  = poly2bmds(ps = c(b1 = 14, b2 = -0.1, er = 0.1), x = ex_conc),
   power = pow(ps = c(a = 1.23, p = 1.45, er = 0.1), x = ex_conc),
   hill = hillfn(ps = c(tp = 750, ga = 5, p = 1.76, er = 0.1), x = ex_conc),
   gnls = gnls(ps = c(tp = 750, ga = 15, p = 1.45, la = 50, q = 1.34, er = 0.1),
                        x = ex_conc),
   exp2 = exp2(ps = c(a = 0.45, b = 13.5, er = 0.1), x = ex_conc),
   exp3 = exp3(ps = c(a = 1.67, b = 12.5, p = 0.87, er = 0.1), x = ex_conc),
   exp4 = exp4(ps = c(tp = 895, ga = 15, er = 0.1), x = ex_conc),
   exp5 = exp5(ps = c(tp = 793, ga = 6.25, p = 1.25, er = 0.1), x = ex_conc)
) %>% 
  melt(data = .,measure.vars = c("constant","poly1","poly2.mono.only","poly2.biphasic",
    "power", "hill","gnls","exp2","exp3","exp4","exp5"))

## Updated Colors ##
fit_cols <-
  # Choose 10 distinct colors
  magma(n = 11, direction = 1) %>% 
  # Darken the original colors to make them more visible
  darken(., amount = 0.2)

## Plot ##
fits %>%
  ggplot() +
  geom_line(aes(x = logc, y = value, lty = variable, colour = variable)) +
  facet_wrap(facets = "variable") +
  theme_bw() +
  labs(lty = "Models", colour = "Models") +
  scale_colour_manual(values = fit_cols) +
  ggtitle("General Shape of Models Included in `tcplfit2`") +
  xlab(expression(paste(log[10], "(Concentration) ", mu, "M"))) +
  ylab("Response")

## ----eval=FALSE---------------------------------------------------------------
#  ## Methods Assignment Option 1 ##
#  # Assign the mc4 processing methods to aeid 80
#  tcplMthdAssign(
#      lvl = 4, # processing level
#      id = 80, # assay endpoint ID(s) to assign method(s)
#      mthd_id = c(1), # method(s) to be assigned
#      ordr = 1, # order the method(s) should be applied
#      type = "mc") # the data/processing type
#  
#  ## Methods Assignment Option 2 ##
#  # Obtain the 'aeid' Values for all endpoints containing "ATG" string, where "ATG" is the abbreviated assay source name of Attagene
#  atg.aeid <- tcplGetAeid(name = "ATG")
#  # Assign the mc4 processing methods for subset of aeids
#  tcplMthdAssign( lvl = 4,
#      id = atg.aeid[, aeid],
#      mthd_id = c(1),  ordr = 1, type = "mc")

## ----echo=FALSE, eval=FALSE---------------------------------------------------
#  # mc4 Processing for subset of aeids
#  tcplRun( id = atg.aeid[, aeid], slvl = 4L, elvl = 4L, type = 'mc' )
#  
#  # Load the mc4 data
#  mc4 <- tcplLoadData(lvl = 4, type = 'mc', fld = 'aeid', val = 80,  add.fld = TRUE)
#  # Prepare the data into a readable format
#  mc4 <- tcplPrepOtpt(mc4)

## ----class.source="fold-hide", fig.align='center',fig.dim=c(8,5.5), warnings=FALSE, message=FALSE----
# Load the example data from the `tcpl` package.
data(mc_vignette, package = 'tcpl')
# Allocate the Level 3 example data
mc3_example <- mc_vignette[['mc3']]
# Level 3 does not store logc anymore, but it can be created for plotting purposes
mc3_example[, logc := log10(conc)]
# Obtain the mc4 example data.
mc4_example <- mc_vignette[["mc4"]]

## Create a Sequence of Concentration Values within Observed Range ##
X <- seq(
 mc4_example[which(mc4_example[, spid] == "01504209"), conc_min],
 mc4_example[which(mc4_example[, spid] == "01504209"), conc_max],
 length.out = 100
)
## Obtain the Continuous Fit of Level 4 Model Estimates ##
# Apply each model fit to continous concentration values (X) and estimated
# concentration-response (estCR) parameters from 'tcplfit2'.
estCR <- mc4_example %>% 
  filter(spid == "01504209") %>% 
  reframe(
    cnst  = cnst(.[, c(cnst_er)], x = X),
    poly1 = poly1(.[, c(poly1_a, poly1_er)], x =  X),
    poly2 = poly2(.[, c(poly2_a, poly2_b, poly2_er)], x = X),
    power = pow(.[, c(pow_a, pow_p, pow_er)], x = X),
    hill  = hillfn(.[, c(hill_tp, hill_ga, hill_p)], x = X),
    gnls  = gnls(.[, c(gnls_tp, gnls_ga, gnls_p, gnls_la, gnls_q, gnls_er)], x = X),
    exp2  = exp2(.[,c(exp2_a, exp2_b, exp2_er)], x = X),
    exp3  = exp3(.[,c(exp3_a, exp3_b, exp3_p, exp3_er)], x = X),
    exp4  = exp4(.[,c(exp4_tp, exp4_ga, exp4_er)], x = X),
    exp5  = exp5(.[,c(exp5_tp, exp5_ga, exp5_p, exp5_er)], x = X) )
# Format data into a data.frame for ease of plotting.
estCR <- cbind.data.frame(X, estCR) %>%
  melt(data = .,measure.vars = c(
    "cnst", "poly1", "poly2", "power", "hill", "gnls", "exp2", "exp3", "exp4", "exp5"))

## Updated Colors ##
fit_cols <-
  # Choose 10 distinct colors
  magma(n = 10,direction = 1) %>% 
  # Darken the original colors to make them more visible
  darken(., amount = 0.2)

## Plot the Model Fits from Level 4 ##
mc3_example %>% 
  filter(spid == "01504209") %>% 
  ggplot(.,aes(x = logc, y = resp))+
  geom_point(pch = 1, size = 2)+
  geom_line(data = estCR,
            aes(x = log10(X), y = value, colour = variable, lty = variable)) +
  labs(colour = "Models", lty = "Models") +
  scale_colour_manual(values = fit_cols) +
  xlab(expression(paste(log[10], "(Concentration) ", mu, "M"))) +
  ylab(expression(paste(log[2], "(Fold Induction)"))) +# )+
  ggtitle(
    label = paste("Level 4 Model Fits",
                  mc4_example[which(mc4_example[,spid] == "01504209"), dsstox_substance_id],
                  sep = "\n"),
    subtitle = paste("Assay Endpoint: ",
                  mc4_example[which(mc4_example[, spid] == "01504209"), aenm])) +
  theme_bw()

## ----echo=FALSE---------------------------------------------------------------
# Obtain the mc4 example data
mc4_example <- mc_vignette[["mc4"]]
# Obtain the AIC values from each of the model fits from the level 4 data
mc4_aic <- mc4_example %>% 
  select(., grep(colnames(.),pattern = "aic")) %>% 
  round(.,3) %>% 
  apply(.,MARGIN = 1, FUN = function(x){
    cell_spec(x,color = ifelse(x == min(x),
                               yes = "blue",
                               no = "black"))
  }) %>% t() %>% 
  data.frame() %>% 
  cbind.data.frame(mc4_example[,dsstox_substance_id],.)
# Rename the columns
colnames(mc4_aic) <-
  colnames(mc4_example)[grep(colnames(mc4_example),pattern = "aic")] %>%
  stringr::str_remove(.,pattern = "_aic") %>% c("dsstox_id",.)
# Export/display the table in an HTML format
mc4_aic %>%
  kbl(
    escape = FALSE,
    format = 'html',
    centering = TRUE) %>%
  kable_styling(
    font_size = 14,
    c("striped", "hover"),
    full_width = FALSE
  )

## ----warning = FALSE, echo = FALSE--------------------------------------------
Method <- c(27,28)
Method_Name <- c("ow_bidirectional_loss", "ow_bidirectional_gain")
Description <- c(
  "Multiply winning model hit call ($hitc$) by -1 for models fit in the positive analysis direction. Typically used for endpoints where only negative responses are biologically relevant.",
  "Multiply winning model hit call ($hitc$) by -1 for models fit in the negative analysis direction. Typically used for endpoints where only positive responses are biologically relevant."
)
# Compile all of the information for the table.
output <- data.frame(Method,Method_Name,Description)
# Export/print the table to an html rendered table.
htmlTable(output,
          align = 'l',
          align.header = 'l',
          rnames = FALSE  ,
          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ' )

## ----warning = FALSE, echo = FALSE--------------------------------------------
Method <- c(15,34)
Method_Name <- c("ow_loec.coff", "include_loec.coff")
Description <- c(
  "Identify the lowest observed effective concentration (LOEC) where the values of all responses are outside the cutoff band (i.e. abs(resp) > cutoff). LOEC is stored alongside winning model and potency estimates. Assume <br/>- modl = 'loec'<br/>- fitc = 100<br/>- model_type = 1<br/>- hit call = 1 if LOEC exists, 0 if not.",
  "Identify the lowest observed effective concentration (LOEC) where the values of all responses are outside the cutoff band (i.e. abs(resp) > cutoff). LOEC is stored alongside winning model and potency estimates."
)
# Compile all of the information for the table.
output <- data.frame(Method,Method_Name,Description)
# Export/print the table to an html rendered table.
htmlTable(output,
          align = 'l',
          align.header = 'l',
          rnames = FALSE  ,
          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')

## ----eval=FALSE---------------------------------------------------------------
#  # Assign the Level 5 Processing Methods to aeid 80
#  tcplMthdAssign(
#    lvl = 5, # processing level
#    id = 80, # assay endpoint ID(s) to assign method(s)
#    mthd_id = c(1, 3, 5), # method(s) to be assigned
#    ordr = 1:3, # order the method(s) should be assigned
#    type = "mc") # the data/processing type
#  
#  # Level 5 processing for aeid 80
#  tcplRun(
#    id = atg.aeid[, aeid], # assay endpoint ID to pipeline
#    slvl = 5L, # level to start pipelining on
#    elvl = 5L, # level to end pipelining on
#    type = 'mc') # endpoint processing type - 'mc' = "multiple concentrations"
#  
#  # Load Level 5 data for aeid 80
#  mc5 <- tcplLoadData(lvl = 5, type = 'mc', fld = 'aeid',val = 80, add.fld = TRUE)
#  # Prepare the data into a readable format.
#  mc5 <- tcplPrepOtpt(mc5)

## -----------------------------------------------------------------------------
# Allocate the level 5 data in `mc_vignette` to the `mc5` object.
mc5_example <- mc_vignette[["mc5"]]

## ----class.source="fold-hide", fig.align='center',fig.dim=c(8,5.5)------------
## Obtain Data ##
# Load the example data from the `tcpl` package
data(mc_vignette,package = 'tcpl')
# Obtain the Level 3 example data
mc3_example <- mc_vignette[['mc3']]
# Level 3 does not store logc anymore, but can be created for plotting purposes
mc3_example[, logc := log10(conc)]
# Obtain the Level 4 example data
mc4_example <- mc_vignette[["mc4"]]
# Obtain the Level 5 example data
mc5_example <- mc_vignette[["mc5"]]

# Subset data related to spid = "01504209"
mc3_ss <- mc3_example %>% dplyr::filter(spid == "01504209") # Level 3 - conc-resp series
mc4_ss <- mc4_example %>% dplyr::filter(spid == "01504209") # Level 4 - model fits
mc5_ss <- mc5_example %>% dplyr::filter(spid == "01504209") # Level 5 - best fit & est.

# Obtain the best model found after curve fitting. See Level 4 for estCR derivation
estCR <- estCR %>%
  mutate(., best_modl = ifelse(variable == mc5_ss[, modl],
                                     yes = "best model", no = NA))

## Generate a Base Concentration-Response Plot ##
basePlot <- mc3_ss %>% 
  # Observed Concentration-Response Data
  ggplot()+
  geom_point(aes(x = logc,y = resp),pch = 1,size = 2) +
  # Cutoff Band
  geom_rect(data = mc5_ss,
            aes(xmin = log10(conc_min), xmax = log10(conc_max), ymin = -coff, ymax = coff),
            alpha = 0.15, fill = "skyblue") +
  # Best Model Fit
  geom_line(data = dplyr::filter(estCR, variable == mc5_ss[,modl]),
            aes(x = log10(X), y = value,color = mc5_ss[,modl])) +
  scale_colour_manual(values = c("royalblue3"), aesthetics = "color") +
  # Other Model Fits
  geom_line(data = dplyr::filter(estCR,variable != mc5_ss[, modl]),
            aes(x = log10(X), y = value, lty = variable),
            alpha = 0.3, show.legend = TRUE) +
  # Legend Information
  labs(lty = "Other Models", color = "Best Fit") +
  # Titles and Labels
  xlab(expression(paste(log[10], "(Concentration) ", mu, "M"))) +
  ylab(expression(paste(log[2], "(Fold Induction)"))) +# )+
  ggtitle(
    label = paste("Level 5 Best Model Fit",
                  mc4_ss[which(mc4_ss[, spid] == "01504209"), dsstox_substance_id],
                  sep = "\n"),
    subtitle = paste("Assay Endpoint: ",
                     mc4_ss[which(mc4_ss[,spid] == "01504209"), aenm])) +
  # Background Plot Theme
  theme_bw()

## Potency Estimate Layers ##
# Obtain and assign colors for the potency estimates to be displayed.
potency_cols <-
  # Choose 5 distinct colors
  viridis::plasma(n = 5, direction = -1) %>% 
  # Darken the original colors to make them more visible
  colorspace::darken(., amount = 0.1)
  
## Compile the Full Level 5 Plot ##
linePlot <-
  # Start with the `basePlot` object
  basePlot +
  # Add the various potency layers
  geom_hline(data = mc5_ss,aes(yintercept = bmr),col = potency_cols[1]) +
  geom_segment(data = mc5_ss,aes(x = log10(bmd), xend = log10(bmd), 
      y = -0.5, yend = bmr), col = potency_cols[1]) +
  geom_hline(data = mc5_ss,aes(yintercept = coff),col = potency_cols[2]) +
  geom_segment(data = mc5_ss, aes(x = log10(acc), xend = log10(acc), 
      y = -0.5, yend = coff), col = potency_cols[2]) +
  geom_hline(data = mc5_ss, aes(yintercept = max_med * 0.5), col = potency_cols[3]) +
  geom_segment(data = mc5_ss,aes(x = log10(ac50), xend = log10(ac50),
      y = -0.5, yend = max_med * 0.5), col = potency_cols[3]) +
  geom_hline(data = mc5_ss, aes(yintercept = max_med * 0.1), col = potency_cols[4]) +
  geom_segment(data = mc5_ss, aes(x = log10(ac10), xend = log10(ac10),
      y = -0.5, yend = max_med * 0.1), col = potency_cols[4]) +
  geom_hline(data = mc5_ss, aes(yintercept = max_med * 0.05), col = potency_cols[5]) +
  geom_segment(data = mc5_ss, aes(x = log10(ac5), xend = log10(ac5),
      y = -0.5, yend = max_med * 0.05), col = potency_cols[5])

# Create data table for potency estimate points
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

# Add Potency Estimate Points and set colors
fullPlot <- linePlot + geom_point(data = mc5_points,
  aes(x = x, y = y, fill = `Potency Estimates`), shape = 21, cex = 2.5) +
  scale_fill_manual(values = mc5_points %>% arrange(`Potency Estimates`) %>% pull(mc_color))

## Display the Compiled Plot ##
fullPlot

## ----eval=FALSE, class.source = "scroll-300"----------------------------------
#  ## Methods Assignment ##
#  tcplMthdAssign(lvl = 1, id = 1:2, mthd_id = c(1, 11, 13), ordr = 1:3, type = "sc")
#  tcplMthdAssign(lvl = 2, id = 1, mthd_id = 3, type = "sc")
#  ## SC Levels 0-2 Processing by acid ##
#  tcplRun(id = 1, type = "sc", slvl = 0, elvl = 2)

## ----eval=FALSE---------------------------------------------------------------
#  ## Methods Assignment ##
#  # No mc1 methods needed
#  tcplMthdAssign(lvl = 2, id = 1, mthd_id = c(3,4,2), ordr = 1:3, type = "mc")
#  tcplMthdAssign(lvl = 3, id = 2, mthd_id = 1, ordr = 1, type = "mc")
#  tcplMthdAssign(lvl = 4, id = 2, mthd_id = 1, ordr = 1:2, type = "mc")
#  tcplMthdAssign(lvl = 5, id = 2, mthd_id = c(1,3,5), ordr = 1:3, type = "mc")
#  tcplMthdAssign(lvl = 5, id = 2, mthd_id = c(1,3,5), ordr = 1:3, type = "mc")
#  
#  ## Assign the Number of Processing Cores to utilize ##
#  mycores <- 1 # If users do NOT want to leverage parallel computing.
#  # Users that want to leverage parallel computing set to > 1, but less than the total number of cores
#  # (i.e. need at least 1 core open for overhead). If not provided, this will be assumed.
#  # "parallel::detectCores()" can be run to understand the maximum allowed number of cores.
#  
#  ## Option A: MC Levels 0-5 Processing by acid ##
#  tcplRun(id = 80, type = "mc",  slvl = 0L,   elvl = 5L,  mc.cores = 20)
#  
#  ##Option B: MC Levels 0-3 Processing by acid, followed by Levels 4-6 by aeid ##
#  tcplRun(id = list$acid, type = "mc", slvl = 0L, elvl = 3L)
#  tcplRun(id = list$aeid, type = "mc", slvl = 4L, elvl = 5L)

## ----warning = FALSE, echo = FALSE--------------------------------------------
Activity_Concentration_uM <- c("AC5", "AC10", "AC20", "AC50", "ACB", "ACC", "AC1SD")
Specified_Level_of_Response <- c("Concentration at 5% of the maximal response",
                 "Concentration at 10% of the maximal response",
                 "Concentration at 20% of the maximal response",
                 "Concentration at 50% of the maximal response",
                 "Concentration at baseline of 3*BMAD", 
                 "Concentration at the user-defined cutoff",
                 "Concentration at 1 standard deviation from baseline")

output <- data.frame(Activity_Concentration_uM, Specified_Level_of_Response)

kable(output)%>% 
  kable_styling("striped")

## ----lvl-6-flag-table, warning = FALSE, echo = FALSE--------------------------
# First column - Level 6 Method ID
Method <- c(5:11, 13:15, 17:20)

# Second column - Level 6 Flag Names
FlagNames <- c("Model directionality questionable", "Active with only highest conc above baseline (3*BMAD)", "Active with one conc (not highest) above baseline (3*BMAD)",
               "Inactive with multiple concs above baseline (3*BMAD)", "Bmd > AC50, indication of high baseline bariability", "Noisy data", "Borderline", "Average number of replicates per conc < 2", 
               "Number of concentrations tested < 4", "Gain AC50 < lowest concn & loss AC50 < mean conc", "Efficacy < 50%", "AC50 < lowest concentration tested", 
               "Cell viability assay fit with gnls winning model", "No median responses above baseline")
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
                      $(top)$ relative to cutoff $(coff)$; $0.8 * coff <= |top| <= 1.2 * coff$",
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
                     defined by 3 times the baseline median absolute deviation $(BMAD)$ or less than baseline as defined by -3 times $BMAD$; both $nmed\\_gtbl\\_pos$ and $nmed\\_gtbl\\_neg = 0$, where $nmed\\_gtbl\\_pos$ is the number of median response values $> 3 * BMAD$ and $nmed\\_gtbl\\_neg$ is the number of median response values $< -3 * BMAD$.")


# Consolidate all columns into a table.
output <- data.frame(Method, FlagNames, FlagDescription) 
colnames(output)<- c("Method", "Flag Name", "Flag Description")

htmlTable(output,
          align = 'l',
          align.header = 'l',
          rnames = FALSE  ,
          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')



## ----varmat_use, eval = FALSE-------------------------------------------------
#  # create matrices with all chemicals and assays; the entire database
#  varmat <- tcplVarMat()
#  # create matrices using a subset of chemicals and/or aeids
#  aeids <- c(80)
#  dtxsid <- c("DTXSID80379721", "DTXSID10379991", "DTXSID7021106", "DTXSID1026081")
#  varmat <- tcplVarMat(aeid = aeids, dsstox_substance_id = dtxsid)
#  # create extra matrices by adding vars
#  varmat <- tcplVarMat(aeid = aeids, add.vars = c("m4id", "resp_max", "max_med"))

## ----varmat_save, eval = FALSE------------------------------------------------
#  library(writexl)
#  write_xlsx(varmat, path = "varmat_output.xlsx")

## ----aed_table, warning = FALSE, echo = FALSE---------------------------------
Parameter <- c("Library(httk)", "httk::calc_mc_oral_equiv()", "httk models used", "httk and QSPRs", "Potency metrics used for httk::calc_mc_oral_equiv()", "Filters on m4id")
Options_Applied <- c("Version 2.3.1", 
                     "species = ‘Human’ </br> 
                      restrictive.clearance=T</br>
                      output.units=’mgpkgpday’</br> 
                      Caco2.options, which revise the fraction bioavailable using estimates of absorption and gut permeability, were kept as default (Caco2.options = list(Caco2.Pab.default = 1.6, Caco2.Fabs=TRUE, Caco2.Fgut=TRUE, overwrite.invivo=FALSE, keepit100=FALSE))",
                      "*3compartmentss*: employs 3 compartments and steady-state assumption with 1 mg/kg/day dosing, assumes clearance = 1/plasma concentration at steady state. When fraction unbound is unavailable, model assumes it is just a very small number </br> 
                      *pbtk*: multi-compartment model that does not assume steady-state kinetics. Requires estimates of intrinsic clearance and fraction unbound; not available for quite as many chemicals as 3compartmentss",
"Quantitative structure property relationships is loaded via load_sipes2017(), load_pradeep2020(), and load_dawson2021() to be able to make AED estimates for as many chemicals as possible.", 
  "ac50, acc, bmd",
  "Hitc >= 0.9 </br>
Number of mc6 flags is < 4 </br>
Fit category is not 36. This removes borderline responses resulting in AC50 below the concentration range screened, which is not considered to be quantitatively informative.
.")

# Compile all of the information for the table.
output <-  data.frame(Parameter, Options_Applied)

# Export/print the table to an html rendered table.
htmlTable(output,
          align = 'l',
          align.header = 'l',
          rnames = FALSE  ,
          css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
          caption = "Options Applied when extrapolating Adminstered Equivalent Doses."
          )

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("dtxsid", "chnm", "spid", "qc_level", "pass_or_caution", "t0", "t4", 
"call", "annotation", "flags", "average_mass", "log10_vapor_pressure_OPERA_pred", "logKow_octanol_water_OPERA_pred")

Description <- c("Unique identifier from U.S. EPA Distributed Structure-Searchable Toxicity (DSSTox) Database", 
                 "Chemical name", "Sample ID or ToxCast bioactivity data, connected by bottle lineage", "Level of analytical QC: substance or sample", 
                 "Indication of analytical QC pass or caution: </br> 
                 **Pass** = No issues in detection observed </br> 
                 **Caution** = See additional information. Considered caution unless T0 or T4 in (A, B), or T0 and T4 are I with S call", 
                 "Grade at T0 (Time zero). Measurements were taken when compound was freshly taken out of freezer. Grade options include: </br>
                 **A**: Molecular Weight (MW) Confirmed, Purity >90% </br> 
                 **B**: MW Confirmed, Purity 75-90% </br> 
                 **C**: MW Confirmed, Purity 50-75% </br> 
                 **D**: CAUTION Purity <50% </br> 
                 **Ac**: Purity > 90% CAUTION Low Conc. 5-30% of expected value </br> 
                 **Bc**: Purity 75-90% CAUTION, Low Conc. 5-30% of expected value </br>
                 **Cc**: Purity 50-75% CAUTION, Low Conc. 5-30% of expected value</br>
                 **Fc**: CAUTION Very Low Conc. <5% of expected value. Biological Activity Unreliable</br>
                 **Z**: MW Confirmed, No Purity Info </br> 
                 **I**: ISOMERS Two or more isomers detected </br>
                 **M**: DEFINED MIXTURE Two or more components </br> 
                 **F**: CAUTION Incorrect MW. Biological Activity Unreliable</br>
                 **Fns**: CAUTION No Sample Detected. Biological Activity Unreliable</br>
                 **U**: Unknown/Inconclusive</br>
                 **ND**: Not Determined</br>
                 **W**: Sample Withdrawn",
                 "Grade at T4 (Time 4 months). Measurements were taken after compound was kept at room temperature for 4 months). Same options at T0.", 
                 "Stability in DMSO over time. Call options include:</br> 
                 **S**: Stable</br>
                 **T**: CAUTION Chemical transformation </br>
                 **L**: CAUTION Physical loss</br>
                 **X**: CAUTION Unstable, reason undetermined", 
"Annotation note from analytical QC manual curation",
"Interpretative flags set based on observed substance or sample level QC (T0, T4, calls) or physicochemical properties. Flag options incude: </br>
**Room temperature stability decreases over time**: T0 in (A, B, C) *AND* call is in (T,L,X)) </br>
**Low concentration possible**: T0 *OR* T4 in (Ac,Bc,Cc). Call may be pass or caution depending on T0</br>
**Extreme loss at room temperature over time**: T0 in (A, B, C) *AND* t4 in (Fc, Fns)</br>
**Missing data for room temperature stability**: T4 in (U,ND,NA,Z,W)</br>
**Initial purity between 75-90%**: T0 in (B, Bc)</br>
**Initial purity between 50-75%**: T0 in (C,Cc)</br>
**Likely fail**: t0 in (D, F, Fns, W) OR T0 in (Z) & T4 in (D, F,Fc,Fns)</br>
**Examine physicochemical properties**: Predicted log10-VP >= 1 *OR* logKow >= 6.5</br>
**Confirmed isomer or mixture**: T0 in (M,I) *AND* T4 in (M,ND,I)</br>
**Missing purity information**: T0 in (Z) *AND* T4 in (A,Ac,B,C)", 
"Average mass of chemical substance",
"OPERA predicted log10 vapor pressure", 
"OPERA predicted Octanol-water partition coefficient")

output <- data.frame(Field, Description)

htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ')


## ----tcplLoad, eval = FALSE---------------------------------------------------
#  # List all assay source IDs
#  tcplLoadAsid()
#  # Create table of all assay endpoint ids (aeids) per assay source
#  aeids <- tcplLoadAeid(fld = "asid", # field to query on
#                        val = 14, # value for each field
#                        add.fld = c("aid", "anm", "acid", "acnm")) # additional fields to return

## ----annotation_query_ex, eval = FALSE----------------------------------------
#  # Select annotation and subset by ids or name, ex.
#  assay <- tcplQuery("SELECT * FROM invitrodb.assay where aid=1;")
#  component <- tcplQuery("SELECT * FROM invitrodb.assay_component;")
#  component <- subset(component, acid %in% source$acid)
#  endpoint <- tcplQuery("SELECT * FROM invitrodb.assay_component_endpoint;")
#  endpoint <- endpoint[grepl("ATG", endpoint$assay_component_endpoint_name),]
#  
#  # Or select all annotations by joining multiple tables
#  annotations <- tcplQuery("SELECT * FROM invitrodb.assay
#                    INNER JOIN invitrodb.assay_source on assay.asid=assay_source.asid
#                    INNER JOIN invitrodb.assay_component on  assay_component.aid=assay.aid
#                    INNER JOIN invitrodb.assay_component_endpoint on assay_component_endpoint.acid=assay_component.acid;")

## ----eval = FALSE-------------------------------------------------------------
#  tcplLoadChem()
#  tcplLoadChemList(field = "chid", val = 1:2)

## ----mthd_list, fig.align='center',message=FALSE, eval=FALSE------------------
#  # Create a function to list all available methods function (SC & mc).
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

## ----tcplquery, eval = FALSE--------------------------------------------------
#  # Load sample table using a MySQL query.
#  samples <- tcplQuery("SELECT * FROM sample;")

## ----sc0, eval = FALSE--------------------------------------------------------
#  # Load Level 0 single concentration (SC0) data for a single acid to R.
#  sc0 <- tcplLoadData(lvl = 0, fld = "acid", val = 1, type = "sc") # data type - single concentration
#  # Alternatively, load data in and format with tcplPrepOtpt.
#  sc0 <- tcplPrepOtpt(tcplLoadData(lvl = 0, fld = "acid", val = 1, type = "sc"))

## ----mc0, eval = FALSE--------------------------------------------------------
#  # Load Level 0 multiple concentration (mc0) data.
#  mc0 <- tcplPrepOtpt(tcplLoadData(lvl = 0, fld = "acid", val = 1, type = "mc"))

## ----mc0_aq, fig.align='center',  message=FALSE, eval=FALSE-------------------
#  # Create a function to review assay quality metrics using indexed Level 0 data
#  
#  aq <- function(ac){
#    # obtain level 1 multiple concentration data for specified acids
#    dat <- tcplPrepOtpt(tcplLoadData(1L, "acid", aeids$acid, type = "mc"))
#  
#    # keep only observations with good well quality (wllq = 1)
#    dat <- dat[wllq == 1]
#  
#    # obtain summary values for data and remove missing data (i.e. NA's)
#    agg <- dat[ ,
#                list(
#                  # median response values (rval) of neutral wells (wllt = n)
#                  nmed = median(rval[wllt == "n"], na.rm = TRUE),
#                  # median absolute deviation (mad) of neutral wells (wllt = n)
#                  nmad = mad(rval[wllt == "n"], na.rm = TRUE),
#                  # median response values of positive control wells (wllt = p)
#                  pmed = median(rval[wllt == "p"], na.rm = TRUE),
#                  # median absolute deviation of positive control wells (wllt = p)
#                  pmad = mad(rval[wllt == "p"], na.rm = TRUE),
#                  # median response values of negative control wells (wllt = m)
#                  mmed = median(rval[wllt == "m"], na.rm = TRUE),
#                  # median absolute deviation of negative control wells (wllt = m)
#                  mmad = mad(rval[wllt == "m"], na.rm = TRUE)
#                  ),
#                # aggregate on assay component id, assay component name,
#                # and assay plate id
#                by = list(acid, acnm, apid)]
#  
#    # Z prime (Z') factor: separation between positive and negative controls,
#    # indicative of likelihood of false positives or negatives.
#    # - Between 0.5 - 1 are excellent,
#    # - Between 0 and 0.5 may be acceptable,
#    # - Less than 0 not good
#    # obtain the Z' for positive controls and neutral
#    agg[ , zprm.p := 1 - ((3 * (pmad + nmad)) / abs(pmed - nmed))]
#    # obtain the Z' factor for negative controls and neutral
#    agg[ , zprm.m := 1 - ((3 * (mmad + nmad)) / abs(mmed - nmed))]
#  
#    agg[ , ssmd.p := (pmed - nmed) / sqrt(pmad^2 + nmad^2)]
#    agg[ , ssmd.m := (mmed - nmed) / sqrt(mmad^2 + nmad^2)]
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
#                         zprm.p = round(median(zprm.p, na.rm = TRUE), 2),
#                         zprm.m = round(median(zprm.m, na.rm = TRUE), 2),
#                         ssmd.p = round(median(ssmd.p, na.rm = TRUE), 0),
#                         ssmd.m = round(median(ssmd.m, na.rm = TRUE), 0),
#                         cv = round(median(cv, na.rm = TRUE), 2),
#                         sn.p = round(median(sn.p, na.rm = TRUE), 2),
#                         sn.m = round(median(sn.m, na.rm = TRUE), 2),
#                         sb.p = round(median(sb.p, na.rm = TRUE), 2),
#                         sb.m = round(median(sb.m, na.rm = TRUE), 2)
#    ), by = list(acid, acnm)]
#    # Return the Results.
#    return(acqu)
#  } #per acid
#  
#  # Run the 'aq' function & store the output.
#  assayq <- aq(ac)

## ----sc2, eval = FALSE--------------------------------------------------------
#  # Load sc2 data for a single aeid
#  sc2 <- tcplPrepOtpt(tcplLoadData(lvl = 2, fld = "aeid", val = 3, type = "sc"))
#  # Alternatively, sc2 data for a set of aeids can be loaded with a vector of ids
#  sc2 <- tcplPrepOtpt(tcplLoadData(lvl = 2, fld = "aeid", val = aeids$aeid, type = "sc"))

## ----sc2_mthd, fig.align='center',message=FALSE, eval=FALSE-------------------
#  # Create a function to load methods for sc data processing for select aeids
#  sc_methods <- function(aeids) {
#    # load the level 1 methods assigned for the single concentration aeid's
#    sc1_mthds <- tcplMthdLoad(lvl = 1, type = "sc", id = aeids$aeid)
#    # aggregate the method id's by aeid
#    sc1_mthds<- aggregate(mthd_id ~ aeid, sc1_mthds, toString)
#    # reset the names of the sc1_mthds object
#    setnames(sc1_mthds, "mthd_id", "sc1_mthd_id")
#  
#    # load the level 2 methods assigned for the single concentration aeid's
#    sc2_mthds <- tcplMthdLoad(lvl = 2, type = "sc", id = aeids$aeid)
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

## ----mc5_data, eval = FALSE---------------------------------------------------
#  # Load mc5 data for a set of aeids
#  # Note: to output mc5_param information with the mc5 results, 'add.fld' is set to TRUE by default.
#  mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5, fld = "aeid", val = aeids$aeid, type = "mc"))

## ----mc5_methods, fig.align='center',message=FALSE, eval=FALSE----------------
#  # Create a function to load methods for mc data processing for select aeids
#  mc_methods <- function(aeids) {
#    # acid
#    ## load the methods assigned to level 2 for given acids
#    mc2_mthds <- tcplMthdLoad(2, aeids$acid)
#    ## aggregate the assigned methods by acid
#    mc2_mthds<- aggregate(mthd_id ~ acid, mc2_mthds, toString)
#    ## rename the columns for the 'mc2_mthds' object
#    setnames(mc2_mthds, "mthd_id", "mc2_mthd_id")
#  
#    # aeid
#    ## load the methods assigned to level 3 for given aeids
#    mc3_mthds <- tcplMthdLoad(3, aeids$aeid)
#    ## aggregate the assigned methods by aeid
#    mc3_mthds<- aggregate(mthd_id ~ aeid, mc3_mthds, toString)
#    ## rename the columns for the 'mc3_mthds' object
#    setnames(mc3_mthds, "mthd_id", "mc3_mthd_id")
#    ## load the methods assigned to level 4 for given aeids
#    mc4_mthds <- tcplMthdLoad(4, aeids$aeid)
#    ## aggregate the assigned methods by aeid
#    mc4_mthds<- aggregate(mthd_id ~ aeid, mc4_mthds, toString)
#    ## rename the columns for 'mc4_mthds' object
#    setnames(mc4_mthds, "mthd_id", "mc4_mthd_id")
#    ## load the methods assigned to level 5 for given aeids
#    mc5_mthds <- tcplMthdLoad(5, aeids$aeid)
#    ## aggregate the assigned methods by aeid
#    mc5_mthds<- aggregate(mthd_id ~ aeid, mc5_mthds, toString)
#    ## rename the columns for 'mc5_mthds' object
#    setnames(mc5_mthds, "mthd_id", "mc5_mthd_id")
#  
#    # Compile the Results.
#    ## merge the aeid information with the level 2 methods by acid
#    acid.methods <- merge(aeids, mc2_mthds, by.x = "acid", by.y = "acid")
#    ## merge the level 3, 4, and 5 methods by aeid
#    mthd35 <- merge(
#      merge(mc3_mthds, mc4_mthds, by = "aeid", all = TRUE),
#        mc5_mthds, by = "aeid", all = TRUE)
#    ## merge all methods information by aeid
#    methods <- merge(acid.methods, mthd35, by.x = "aeid", by.y = "aeid")
#    # Print the Results.
#    print(methods)
#    # Return the Results.
#    return(methods)
#  }
#  
#  # Run the 'methods' function and store the output.
#  mmthds <- mc_methods(aeids)

## ----tcplQueryAPI, eval = FALSE-----------------------------------------------
#  # Request and load all assays+annotations for specified asid
#  data <- tcplQueryAPI(resource = "data", # resource to query from API, either 'data' or 'assay'
#                       fld = "aeid", val = 891, # field and val to query on
#                       return_fld = c("spid", "chnm", "hitcall")) # specify the return fields, leave NULL for all fields

## ----annotation_query_api_ex, eval = FALSE------------------------------------
#  # Load all assays and their annotations
#  assays <- tcplQueryAPI(resource = "assay")

## ----data_by_aeid-------------------------------------------------------------
# Load mc5 data by aeid
mc5 <- tcplLoadData(lvl = 5,         # data level
                    fld = "aeid",    # fields to query on
                    val = 704,       # values should match their corresponding 'fld'
                    type = "mc",     # default. Note: sc data is not available on APIs yet
                    add.fld = FALSE) # restrict to just level 5 parameters 

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(mc5))%>% 
 kableExtra::kable_styling("striped") %>% 
 kableExtra::scroll_box(width = "100%")

## ----data_by_id, eval=FALSE---------------------------------------------------
#  ## Load mc5 data by spid
#  mc5 <- tcplLoadData(lvl=5, fld = "spid", val = "TP0000904H05", type = "mc", add.fld = FALSE)
#  
#  ## Load mc5 data by m4id
#  mc5 <- tcplLoadData(lvl=5, fld = "m4id", val = 1842443, type = "mc", add.fld = FALSE)
#  
#  ## Load mc data by dtxsid
#  mc5 <- tcplLoadData(lvl=5, fld = "dtxsid", val = "DTXSID30944145", type = "mc", add.fld = FALSE)

## ----data_level_3, eval=FALSE-------------------------------------------------
#  ## Load mc3. This returns m4id, spid, conc, aeid, logc, resp
#  mc3 <- tcplLoadData(lvl = 3, fld = "m4id", val = 1842443, type = "mc", add.fld = FALSE)
#  
#  ## Load mc4. This returns m4id, spid, bmad, resp_max, resp_min, max_mean,	max_mean_conc,	
#  # max_med,	max_med_conc,	logc_max,	logc_min,	nconc,	npts,	nrep,	nmed_gtbl
#  mc4 <- tcplLoadData(lvl = 4, fld = "m4id", val= 1842443, type = "mc", add.fld = FALSE)
#  
#  ## Load mc6. This returns mc5 parameters plus flags
#  mc6 <- tcplLoadData(lvl = 6, fld = "m4id", val = 1842443, type = "mc", add.fld = FALSE)
#  
#  ## Load mc4 agg. This returns mc3 and mc4 parameters
#  agg <- tcplLoadData(lvl = "agg", fld = "m4id", val = 1842443, type = "mc", add.fld = FALSE)
#  
#  ## Load data with add.fld = TRUE to return all available processed data fields
#  all_fields <- tcplLoadData(lvl = 3,  fld = "m4id", val = 1842443, type = "mc", add.fld = TRUE)

## ----load_aeid----------------------------------------------------------------
# Load aeid and aenm for given acid
aeid <- tcplLoadAeid(fld = "acid", val = 400)
print(aeid)

## ----load_aeid_plus-----------------------------------------------------------
# Subset all aeids by using multiple fields -- NOTE: val must be same length in list form!
aeids <- tcplLoadAeid(fld = c("intended_target_type", "detection_technology_type"), 
                      val = list("protein", c("Colorimetric", "Fluorescence"))) # list length == 2!

## ----load_acid----------------------------------------------------------------
# Load acid and acnm for given aeid
acid <- tcplLoadAcid(fld = "aeid", val = c(663,891))

# Subset all acids by using multiple fields -- val must be same length in list form!
acids <- tcplLoadAcid(fld = c("organism", "tissue"), 
                      val = list("rat", "liver"),
                      add.fld = c("aeid", "aid", "asid", "signal_direction")) 

## ----load_aid, eval=FALSE-----------------------------------------------------
#  # Load aid and anm for given aeid
#  aid <- tcplLoadAid(fld = "aeid", val = 663)
#  
#  # Subset all aids by using multiple fields -- val must be same length in list form!
#  aids <- tcplLoadAid(fld = c("organism", "tissue"),
#                        val = list("rat", "liver"),
#                        add.fld = c("aeid", "acid", "asid", "signal_direction"))

## ----load_asid, eval=FALSE----------------------------------------------------
#  # Load asid and asnm for given aeid
#  asid <- tcplLoadAsid(fld = "aeid", val = 663)
#  
#  # Subset all asids by using multiple fields -- val must be same length in list form!
#  asids <- tcplLoadAsid(fld = c("organism", "tissue"),
#                        val = list("rat", "liver"),
#                        add.fld = c("aeid", "acid", "asid", "signal_direction"))

## ----load_unit----------------------------------------------------------------
# Load resp_unit for given aeid
unit <- tcplLoadUnit(aeid = c(663, 891))

## ----load_conc_unit-----------------------------------------------------------
# Load conc_unit for given spid
conc_unit <- tcplLoadConcUnit(spid = "TP0000904H05")
print(conc_unit)

## ----load_chem----------------------------------------------------------------
# Load chem_info for given spid
chem_info <- tcplLoadChem(field = "spid", val = "TP0000904H05")
print(chem_info)

## ----warning = FALSE, echo = FALSE--------------------------------------------
Field <- c("type", "dat", "fld", "val", "compare", "output", "verbose", "multi", "by",
           "fileprefix", "'nrow' and 'ncol'", "dpi", "flags", "yuniform", "yrange", 
           "group.fld","group.threshold","hide_losing_models")
Description <- c("'mc' assumed as default. `type = 'mc'` plots available mc data fit by all models and highlights the winning model with activity hit call presented whereas `type = 'sc'` plots available SC data including response values, maximum median, and cutoff with activity hit call presented.",
                 "Parameter permits the user to supply plot-ready data rather than automatically loading it within tcplPlot. Use cases include plotting across multiple database connections or using tcplPlot to plot other tcplfit2-fit data. See the advanced comparison plotting section and ?tcplPlotLoadData for more information.",
                 "Required parameter for field to query on",
                 "Required parameter for values to query on that must be listed for each corresponding 'fld'",
                 "Parameter is used to generate comparison plots joining endpoint-samples by the provided field name contained in the loaded plot data. The default, being $m4id$ or $s2id$ for `type = 'mc'` and `type = 'sc'` respectively, will not generate comparison plots since such ids are unique in nature. To create comparison plots, such as across the same chemicals, use a chemical identifier like $dsstox\\_substance\\_id$. Likewise, to create a comparison plot across the same sample ids, use $spid$. Use $aeid$ to create a comparison plot across the same assay component endpoints, which will likely trigger a simpler plot style for large comparison groups; for more info, see the `group.threshold` parameter. To use a custom field to create comparisons, `dat` should be supplied as a data.table generated from `tcplPlotLoadData` with the custom column included. If `dat` is instead a list of data.tables (for manual control of the comparison groups), the `compare` parameter will be ignored in favor of the list groups.",
                 "Parameter indicates how the plots will be presented. In addition to outputs viewable with the R `console`, tcplPlot supports a variety of publication-quality file type options, including raster graphics (`PNG`, `JPG`, and `TIFF`) to retain color quality when printing to photograph and vector graphics (`SVG` and `PDF`) to retain image resolution when scaled to large formats. For a more customizable option, an indivdiual plot can be output in environment as a `ggplot`",
                 "Parameter results in a plot that includes a table containing potency and model performance metrics, and a table of endpoint-sample information for comparison plots; `verbose = TRUE` is default but is `FALSE` when using console output.  When `verbose = TRUE` the model aic values are listed in descending order and generally the winning model will be listed first. It is recommended to leave this toggle on for comparison plots, as endpoint-sample information is made available in table format above the plots.",
                 "Parameter allows for single or multiple plots per page. `multi = TRUE` is the default option for PDF outputs when there is more than one plot, and `FALSE` if there is just one, whereas `multi = FALSE` is the only option for other outputs. If using the parameter option `multi = TRUE`, the default number of plots per page is set by the `type` parameter. The default number of plots per page is 4 plots per page if `type = 'mc'` and 6 plots per page if `type = 'sc'`. Comparison plots are typically printed 2 plots per page, but large comparisons might take up entire pages individually.",
                 "Parameter indicates how files should be divided, typically by $aeid$ or $spid$", 
                 "Parameter allows the user to set a custom filename prefix. The standard filename is tcplPlot_sysDate().output (example: tcplPlot_2023_08_02.jpg) or, if `by` parameter is set, tcplPlot_sysDate()_by.output (example: tcplPlot_2023_08_02_aeid_80.pdf). When a `fileprefix` is assigned the default _tcplPlot_ prefix is replaced with the new filename.", 
                 "The 'nrow' parameter specifies the number of rows for the multiple plots per page; this is 2 by default. The `ncol` parameter specifies the number of columns for the multiple plots per page; this is 3 by default. If `verbose = FALSE`, `ncol` is 2. `nrow` and `ncol` can customize the number of plots included per page. Both `nrow` and `ncol` must be greater than 0. While there is no hard coded upper limit to the number of rows and columns, the underlying technology has a dimension limitation of `nrow = 9` and `ncol = 7`.", 
                 "Parameter specifies image print resolution for image file output types (PNG, JPG, TIFF, SVG); this is 600 by default.", 
                 "Parameter toggles the output of Level 6 flags. The default option is `flag = FALSE`. If `type = 'sc`, setting `flag = TRUE` will result in warning, since `invitrodb` does not store flags for single-concentration data.", 
                 "Parameter toggles automatic uniform y-axis scaling. The default option is `yuniform = FALSE`. If set to `TRUE`, tcplPlot will set each plot's y-axis range to be the minimum and maximum of response values and cutoffs across every requested plot. For example, when plotting a percent activity endpoint if the maximal response was 100% and minimal was -50%, while the cutoff was 20%, the y-axis range for every plot will be set to be from -50% to 100%. This is most useful for across-plot interpretation.", 
                 "Parameter toggles user-specified uniform y-axis scaling. `yrange` is required to be an integer of length 2: c(min,max). By default, c(NA,NA) will not set any uniform range. For example, when plotting a percent activity endpoint, the user may wish to set the range to c(-100,100) so every plot is contained to -100% and 100%.",
                 "Parameter groups curve color by a plot-data field when the number of curves on any given comparison plot equals or exceeds `group.threshold`. Any column provided within tcplPlotLoadData can be used, but discretized values, like binarized 'hitc', 'modl', or 'fitc' are suggested. By default, `group.fld` is 'modl' or 'hitc' for `type = 'mc'` and `type = 'sc'` respectively. Use `dat` as a data.table to set `group.fld` to a custom column.",
                 "Parameter sets the minimum number of curves which any given comparison plot should delineate curve color by `group.fld`, instead of by endpoint-sample ($m4id$). By default, this number of curves is 9. To effectively disable this behavior (not recommended), set `group.threshold` to a value higher than the number of curves in largest comparison group you expect in the output of `tcplPlot()`.",
                 "Parameter toggles whether individual mc plot output should hide the losing models, only printing the winning model. By default, this parameter is `FALSE`.")

output <- data.frame(Field, Description)

knitr::kable(output) |> kable_styling("striped")  |>
  kableExtra::scroll_box(width="100%", height="400px")

## ----mc_plot_pdf_aeid, eval = FALSE-------------------------------------------
#  # Plot mc data for aeids 3157-3159 and outputs plots separate pdfs by aeid.
#  tcplPlot(type = "mc",     # not required; "mc" is default
#           fld = "aeid",    # field to query on
#           val = 3157:3159, # values should match their corresponding 'fld'
#           by = "aeid",     # parameter to divide files
#           multi = TRUE,    # multiple plots per page - output 4 per page
#           output = "pdf")  # output as pdf
#  
#  # Load required mc_vignette data for example below
#  data(mc_vignette, package = 'tcpl')
#  mc5 <- mc_vignette[["mc5"]]
#  
#  # Plot mc data from the mc_vignette R data object for a single aeid 80 and
#  # spids "TP0001652B01", "01504209", "TP0001652D01", "TP0001652A01", and "1210314466"
#  tcplPlot(fld = c("aeid", "spid"),        # field to query on
#           val = list(mc5$aeid, mc5$spid), # values must be listed for each corresponding 'fld'
#           by = "aeid", multi = TRUE, flags = TRUE,  yrange = c(-0.5, 1.5),
#           output = "pdf", fileprefix = "output_pdf")

## ----mc_plot_jpg, eval = FALSE------------------------------------------------
#  # Plot a verbose plot of mc data for single aeid 80 and spid 01504209 and
#  # output as jpg.
#  tcplPlot(type = "mc", fld = c('aeid','spid'), val = list(80,'01504209'), multi = FALSE,
#           flags = TRUE, output = "jpg", fileprefix = "output_jpg")

## ----mc_plot_console, eval = FALSE--------------------------------------------
#  # Create MC plot for a single m4id from 'mc_vignette' data
#  tcplPlot(type = "mc", fld = "m4id", val = 482273,
#           multi = FALSE, verbose = FALSE, output = "console")
#  
#  # Plot of MC data for single aeid (80) and spid (01504209) and output to console
#  tcplPlot(type = "mc", fld = c('aeid','spid'), val = list(80, '01504209'),
#           multi = FALSE, verbose = FALSE, output = "console")

## ----sc_plot_pdf_aeid, eval = FALSE-------------------------------------------
#  # Plot SC data for aeid 704 and outputs plots separate pdfs by aeid
#  tcplPlot(type = "sc", fld = "aeid", val = 704, multi = TRUE,
#           output = "pdf", fileprefix = "sc_output")

## ----plot_compare, eval = FALSE-----------------------------------------------
#  spids <- c("EPAPLT0108M13", "EPAPLT0108H01", "EPAPLT0108C17", "EPAPLT0106J20")
#  # default parameters used here: type = "mc", verbose = TRUE
#  tcplPlot(fld = c("spid", "aeid"),         # field(s) to query on
#           val = list(spids, c(3074,3076)),         # values must be listed for each corresponding `fld`
#           compare = "spid",
#           output = "pdf", multi = TRUE, flags = TRUE,
#           yuniform = TRUE, fileprefix = "plot_compare")

## ----sc_plot_compare, eval = FALSE--------------------------------------------
#  spids <- c("MLS", "DMSO", "Tox21_400088", "Tox21_200265", "Tox21_200001", "Tox21_200266")
#  # Plot comparison across two different endpoints with same samples
#  tcplPlot(type = "sc", fld = c("spid","aeid"), val = list(spids, c(3017,3018)),
#            compare = "spid", output = "pdf", multi = TRUE, fileprefix = "sc_plot_compare")

## ----medium_plot_compare, eval = FALSE----------------------------------------
#  # default parameters used here: type = "mc", verbose = TRUE
#  tcplPlot(fld = "aeid",
#           val = 2642:2646,
#           compare = "dsstox_substance_id",
#           output = "pdf", multi = TRUE, flags = TRUE,
#           fileprefix = "medium_plot_compare")

## ----large_plot_compare, eval = FALSE-----------------------------------------
#  # default parameters used here: type = "mc", verbose = TRUE, group.fld = "modl", group.threshold = 9
#  tcplPlot(fld = "aeid",
#           val = 3168,
#           compare = "aeid",
#           output = "jpg", flags = TRUE, dpi = 100,
#           fileprefix = "large_plot_compare")

## ----plot_standalone, eval = FALSE--------------------------------------------
#  # tcplConf() configured with some connection like database or CTX APIs
#  plot_data1 <- tcplPlotLoadData(fld = "aeid", val = 704, type = "mc", flags = TRUE)
#  
#  # Fill with different database connection information
#  tcplConf(user = "", pass = "", db = "invitrodb", drvr = "MySQL", host = "")
#  
#  # Load more data
#  plot_data2 <- tcplPlotLoadData(fld = "aeid", val = 704, type = "mc", flags = TRUE)
#  
#  # Combine the data.tables together
#  plot_data <- rbind(plot_data1, plot_data2, fill = TRUE)
#  
#  # Plot comparisons of aeid 704 from one database version to another and output to pdf
#  tcplPlot(dat = plot_data, compare = c("aeid", "spid"),
#           output = "pdf", multi = TRUE, flags = TRUE, fileprefix = "version_compare")

## ----plot_aeid_spid, eval = FALSE---------------------------------------------
#  # Load all matching spids
#  plot_data <- tcplPlotLoadData(fld = "spid", val = "01504209", flags = TRUE) # 81 rows of data
#  
#  # Subset using the aeid desired to find m4id
#  m4id <- plot_data[aeid == 80]$m4id # subset to 1 aeid extract m4id
#  # Default parameters used here: fld = "m4id", verbose = TRUE, type = "mc"
#  # (type can never be "sc" when connected to API)
#  tcplPlot(val = m4id, flags = TRUE, output = "jpg", fileprefix = "output_jpg")
#  
#  # Alternatively, subset the plot_data object and use it in 'dat'
#  plot_data <- plot_data[aeid == 80]
#  # Default parameters used here: verbose = TRUE, type = "mc"
#  tcplPlot(dat = plot_data, flags = TRUE, output = "jpg", fileprefix = "output_jpg")

## ----api_compare, eval = FALSE------------------------------------------------
#  # default parameters used here: type = "mc", verbose = TRUE
#  tcplPlot(fld = "aeid", val = 2642:2646, compare = "chnm", output = "pdf",
#           multi = TRUE, flags = TRUE, fileprefix = "api_compare")

## ----BPA, eval = FALSE--------------------------------------------------------
#  # Provide the chemical name and assign to 'chnm'. Synonyms will not be matched, so other chemical identifiers may be more appropriate to query.
#  chnm <- 'Bisphenol A'
#  # Load the chemical data from the database
#  chem <- tcplLoadChem(field = 'chnm', val = chnm)
#  # Load mc5 data from the database for the specified chemical
#  BPA.mc5 <- tcplLoadData(lvl = 5, fld = 'spid', val = chem[, spid], type = 'mc')

## ----spid_plot_1, eval=FALSE--------------------------------------------------
#  # Load mc% data summary values for select aeids
#  mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5, fld = 'aeid', val = tcplLoadAeid(fld = "asid", val = 25)$aeid, type = 'mc', add.fld = TRUE))
#  
#  # Identify sample subset
#  spid.mc5 <- mc5[spid %in% c("EPAPLT0018N08", "EPAPLT0023A16", "EPAPLT0020C11", "EPAPLT0018B13", "EPAPLT0018B14", "EPAPLT0018B15"),]
#  
#  # Plot by endpoint for sample subset
#  tcplPlot(fld = c("spid", "aeid"), val = list(spid.mc5$spid, spid.mc5$aeid), by = "aeid", multi = TRUE, output = "pdf", fileprefix = "upitt")

## ----spid_plot_2, eval=FALSE--------------------------------------------------
#  plot_data <- tcplPlotLoadData(fld = 'aeid', val = tcplLoadAeid(fld = "asid", val = 25)$aeid, type = 'mc')
#  
#  # Identify sample subset.
#  plot_data <- plot_data[spid %in% c("EPAPLT0018N08", "EPAPLT0023A16", "EPAPLT0020C11", "EPAPLT0018B13", "EPAPLT0018B14", "EPAPLT0018B15"),]
#  
#  # Plot by endpoint for sample subset.
#  tcplPlot(dat = plot_data, by = "aeid", multi = TRUE, output = "pdf", fileprefix = "upitt")

## ----plot_loec, eval = FALSE--------------------------------------------------
#  # default parameters used here: type = "mc"
#  tcplPlot(fld = "aeid", val = 3211, verbose = FALSE,
#           output = "pdf", multi = TRUE, fileprefix = "loec")

## ----txrf-data-liver-subset, eval=FALSE---------------------------------------
#  toxref_chnm_liver <- toxref_batch_download_chnm %>%
#    filter(endpoint_target == 'liver')
#  
#  toxref_chnm_liver_lel <- toxref_chnm_liver %>%
#    summarise(lel = min(dose_adjusted[treatment_related == 1]),
#              loael = min(dose_adjusted[critical_effect == 1]))

## ----txct-annotations-liver-subset, warning=FALSE, eval=FALSE-----------------
#  toxcast_annotations_subset <- tcplLoadAeid(fld = "tissue", val = "liver", add.fld = "tissue")

## ----txct-data-pull, fig.align='center',class.source="scroll-100",message=FALSE, eval=FALSE----
#  # Load the chemical data from the database
#  chnm <- 'Pentachlorophenol'
#  chem <- tcplLoadChem(field = 'chnm', val = chnm)
#  
#  # Load Level 5 data from the database for the specified chemical
#  mc5 <- tcplLoadData(lvl = 5, fld = 'spid', val = chem[, spid], type = 'mc')
#  
#  # Join with Level 6 flag information
#  mc6 <- tcplPrepOtpt(tcplLoadData(lvl = 6, fld = 'm4id', val = mc5$m4id, type = 'mc'))
#  setDT(mc6)
#  mc6_mthds <- mc6[ , .( mc6_mthd_id = paste(mc6_mthd_id, collapse=",")), by = m4id]
#  mc6_flags <- mc6[ , .( flag = paste(flag, collapse=";")), by = m4id]
#  mc5$mc6_flags <- mc6_mthds$mc6_mthd_id[match(mc5$m4id, mc6_mthds$m4id)]
#  mc5[, flag.length := ifelse(!is.na(mc6_flags),
#                       count.fields(textConnection(mc6_flags), sep = ','), NA)]
#  
#  # Filter the potency and activity using coarse filters related to hitc, flags, fitc
#  mc5[hitc >= 0.9 & flag.length < 3, use.me := 1]
#  mc5[hitc >= 0.9 & is.na(flag.length), use.me := 1]
#  mc5[hitc >= 0.9 & flag.length >= 3, use.me := 0]
#  mc5[fitc %in% c(36, 45), use.me := 0]
#  mc5[hitc < 0.9, use.me := 0]
#  mc5[use.me == 0, ac50 := as.numeric(NA)]
#  mc5[use.me == 0, hitc := 0]
#  mc5[hitc == 0, ac50 := as.numeric(NA)]
#  mc5[hitc >= 0.9, ac50_uM := ifelse(!is.na(ac50), ac50, NA)]
#  
#  #Filter to only liver endpoints
#  toxcast_mc5_liver <- mc5[aeid %in% toxcast_annotations_subset$aeid,]

## ----httk-prep, eval=FALSE----------------------------------------------------
#  # Calculating summary statistics for AC50 values for httk processing to calculate AED
#  toxcast_mc5_liver_summary <- toxcast_mc5_liver[,list(
#    p5.ac50uM = quantile(ac50_uM, probs = c(0.05), na.rm = T),
#    p50.ac50uM = quantile(ac50_uM, probs = c(0.50), na.rm = T),
#    mean.ac50uM = mean(ac50_uM, na.rm = T))]

## ----httk-aed, warning=FALSE, message=FALSE, eval=FALSE-----------------------
#  # Generate AEDs
#  toxcast_aed_liver_summary <- toxcast_mc5_liver_summary %>%
#        summarize(aed.p5ac50.hu.css.50 = calc_mc_oral_equiv(conc=p5.ac50uM,
#                    dtxsid = 'DTXSID7021106', which.quantile = c(0.95),
#                    species ='Human', restrictive.clearance = T,
#                    output.units = 'mgpkgpday', model = '3compartmentss'),
#          aed.p50ac50.hu.css.50 = calc_mc_oral_equiv(conc = p50.ac50uM,
#                    dtxsid = 'DTXSID7021106', which.quantile = c(0.95),
#                    species = 'Human', restrictive.clearance = T,
#                    output.units = 'mgpkgpday', model = '3compartmentss'),
#          aed.meanac50.hu.css.50 = calc_mc_oral_equiv(conc = mean.ac50uM,
#                    dtxsid = 'DTXSID7021106', which.quantile = c(0.95),
#                    species = 'Human', restrictive.clearance = T,
#                    output.units = 'mgpkgpday', model = '3compartmentss'))

## ----compare, echo=FALSE------------------------------------------------------
# Create comparison table
POD <- c("ToxRefDB LEL", 
            "ToxRefDB LOAEL",
            "ToxCast AED at 5th percentile AC50", 
            "ToxCast AED at 50th percentile/median AC50", 
            "ToxCast AED at mean AC50")
Value <- c("1.5",
          "1.5", 
          "2.273744",
          "7.666872", 
          "16.09772")

Table <- as.data.table(t(data.frame(POD, Value)))
setnames(Table, as.character(Table[1,]))
Table <- Table[-1,]

kable(Table)%>% 
  kable_styling("striped")

## ----txct-data-pull2, fig.align='center',class.source="scroll-100",message=FALSE, eval=FALSE----
#  # Identify the lel and loaels from toxref chemical subset
#  toxref_chnm_POD<-toxref_chnm_EE2 %>%
#    summarise(lel=min(dose_adjusted[treatment_related==1]),
#              loael=min(dose_adjusted[critical_effect==1]))
#  
#  # Load the chemical data from the database
#  chem <- tcplLoadChem(field = 'dsstox_substance_id',val = "DTXSID5020576")
#  
#  # Load Level 5 data from the database for the specified chemical
#  mc5 <- tcplLoadData(lvl = 5, # data level
#                          fld = 'spid', # field to query on
#                          val = chem[,spid], # value for each field (fld)
#                          type = 'mc') # data type - mc
#  
#  # Join with Level 6 flag information
#  mc6 <- tcplPrepOtpt(tcplLoadData(lvl = 6, fld = 'm4id', val = mc5$m4id, type = 'mc'))
#  setDT(mc6)
#  mc6_mthds <- mc6[ , .( mc6_mthd_id = paste(mc6_mthd_id, collapse = ",")), by = m4id]
#  mc6_flags <- mc6[ , .( flag = paste(flag, collapse = ";")), by = m4id]
#  mc5$mc6_flags <- mc6_mthds$mc6_mthd_id[match(mc5$m4id, mc6_mthds$m4id)]
#  mc5[, flag.length := ifelse(!is.na(mc6_flags),
#                       count.fields(textConnection(mc6_flags), sep =','), NA)]
#  
#  # Filter the potency and activity using coarse filters related to hitc, flags, fitc
#  mc5[hitc >= 0.9 & flag.length < 3, use.me := 1]
#  mc5[hitc >= 0.9 & is.na(flag.length), use.me := 1]
#  mc5[hitc >= 0.9 & flag.length >= 3, use.me := 0]
#  mc5[fitc %in% c(36,45), use.me := 0]
#  mc5[hitc < 0.9, use.me := 0]
#  mc5[use.me == 0, ac50 := as.numeric(NA)]
#  mc5[use.me == 0, hitc := 0]
#  mc5[hitc == 0, ac50 := as.numeric(NA)]
#  mc5[hitc >= 0.9,ac50_uM := ifelse(!is.na(ac50), ac50, NA)]
#  
#  # Calculate summary statistics for AC50 values for httk processing to calculate AED
#  toxcast_mc5_EE2_summary <- mc5[,list(
#    p5.ac50uM = quantile(ac50_uM, probs = c(0.05), na.rm=T),
#    p50.ac50uM = quantile(ac50_uM, probs = c(0.50), na.rm=T),
#    mean.ac50uM = mean(ac50_uM, na.rm=T))]
#  
#  # Generate AEDs
#  toxcast_aed_EE2_summary <- toxcast_mc5_EE2_summary %>%
#        summarize(aed.p5ac50.hu.css.50 = calc_mc_oral_equiv(conc = p5.ac50uM,
#                    dtxsid = 'DTXSID5020576', which.quantile = c(0.95),
#                    species ='Human', restrictive.clearance = T,
#                    output.units = 'mgpkgpday', model = '3compartmentss'),
#          aed.p50ac50.hu.css.50 = calc_mc_oral_equiv(conc = p50.ac50uM,
#                    dtxsid = 'DTXSID5020576', which.quantile = c(0.95),
#                    species ='Human', restrictive.clearance = T,
#                    output.units='mgpkgpday', model='3compartmentss'),
#          aed.meanac50.hu.css.50 = calc_mc_oral_equiv(conc=mean.ac50uM,
#                    dtxsid = 'DTXSID5020576', which.quantile = c(0.95),
#                    species = 'Human', restrictive.clearance = T,
#                    output.units = 'mgpkgpday', model = '3compartmentss'),
#          aed.minac50.aeid807.hu.css.50 = calc_mc_oral_equiv(conc = 0.0002448276,
#                    dtxsid = 'DTXSID5020576', which.quantile = c(0.95),
#                    species ='Human', restrictive.clearance = T,
#                    output.units = 'mgpkgpday', model = '3compartmentss'))

## ----compare2, echo=FALSE-----------------------------------------------------
# Create comparison table
POD <- c("ToxRefDB LEL",
         "ToxRefDB LOAEL",
         "ToxCast AED at 5th percentile AC50", 
         "ToxCast AED at 50th percentile/median AC50", 
         "ToxCast AED at mean AC50")
Value <- c("0.00012",
           "0.00021", 
           "2.26e-07",
           "0.00661", 
           "0.01994")

Table <- as.data.table(t(data.frame(POD, Value)))
setnames(Table, as.character(Table[1,]))
Table <- Table[-1,]

kable(Table)%>% 
  kable_styling("striped")

## ----include=FALSE------------------------------------------------------------
end_vignette()

