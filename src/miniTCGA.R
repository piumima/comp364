## miniTCGA.R
##
## Description:
##   This file contains scripts related to the
##   miniTCGA dataset, to be used on COMP 364:
##   Computer Tools for the Life Sciences.
##
## Author:
##   Daniel Del Balso <daniel.delbalso2@mail.mcgill.ca>
##
## (c) BCI Lab - McGill University, 2015 (Michael T. Hallett, PI)
##
## This code may not be used, modified, distributed, read or thought-about
## without the permission of the copyright holder,
## unless it is being used to complete the course requirements of COMP 364.


load.miniTCGA <- function(dataDir,
                          dataTypes="exprs"){
  ## The load.miniTCGA function is the gateway
  ## to this data. The student runs this function,
  ## specifying the directory where they have cloned their
  ## data.
  ## Almost always this will be:
  ## "/path-to-the-cloned-repo/data/"
  ##
  ## Args:
  ##  dataDir   : The data directory in the cloned repo
  ##  dataTypes : A vector specificying what you would like to
  ##              load, besides the clinical info (which is
  ##              always loaded).
  ##              Supports:
  ##                  exprs           - Gene expression
  ##                  cnv             - Copy number variations
  ##                  somatic.curated - Curated somatic mutations
  ## TODO             mirna           - Micro RNA
  ## TODO             methyl          - Methylation Data
  ## TODO             rna-seq         - RNA Seq data
  ## TODO             dna-seq         - DNA Seq data
  ##                  all             - All available types
  ##                  none            - Loads only the clinical data.
  ##
  ##
  ## Returns:
  ##  Returns the data structure miniTCGA.

  ## Sanity checks.
  if ("none" %in% dataTypes && length(dataTypes) > 1) {
    stop("Conflicting arguments to load.miniTCGA!\nYou cannot specify \"none\" along with other data types.")
  }
  if ("all" %in% dataTypes && length(dataTypes) > 1) {
    stop("Conflicting arguments to load.miniTCGA!\nYou cannot specify \"all\" along with other data types.")
  }
  ## If "all" is specified, load everything.
  if (all(dataTypes == "all")) {
    dataTypes <- c("exprs", "cnv", "somatic.curated")
  }
  ## Load clinical
  miniTCGA <- list(clinical=readRDS(gsub("//","/", paste(dataDir, "/clinical.rds", sep=""))))
  ## If "none", stop.
  if(all(dataTypes == "none")) {
    return(miniTCGA)
  }
  ## If we want something...
  ## Loop over what we should load.
  for(toLoad in dataTypes) {
    ## Load the file and give it the name we want.
    dat <- readRDS(gsub("//","/",paste(dataDir, "/", toLoad, ".rds",sep="")))
    ## Assign
    miniTCGA[[toLoad]] <- dat
  }
  ## Return miniTCGA object
  return(miniTCGA)
}
