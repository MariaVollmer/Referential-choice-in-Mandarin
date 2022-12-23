# This script is used in the analysis of the following paper:
# Vollmer, Maria. Accepted with minor revision. Comparing zero and referential 
# choice in eight languages with a focus on Mandarin Chinese. Studies in language.

# If you have any questions, you can contact me via email: mariacvollmer@gmail.com

# This script computes the antecedent distance of referents 
# which is a variable in the computation of the decision trees.

# Nils Schiborr (University of Bamberg) has kindly shared this script with me, 
# which I have slightly adapted.
# Any remaining errors are my own. 

#' all rows without referents (i.e. without RefIND glosses) get \code{NA_integer_} in the
#' distance columns.

mc_dist_clause <- function(x, direction = "ante") {
  # copy table
  text <- copy(x)
  
  # index left clause boundaries sequentially
  text[grepl("#", graid), cid := seq(1, .N, 1), by = "text"]
  
  # label all words in each clause with its index
  # NOTE: unsure why cumsum() does what it does here!
  text[, cid := cid[which(!is.na(cid))], by = cumsum(!is.na(cid))]
  
  # remember row indices
  text[, I := .I]
  
  
  # subset rows with referents by file and refind
  indc <- text[refind != "", .(cid, I), by = c("text", "refind")]
  
  # sort by row index
  setkey(indc, I)
  
  
  # check directionality
  if (direction == "ante") {
    # calculate distance in clause units by subtracting from each cid the next cid
    # with the same refind and within the same file
    indc[, cDist := cid - shift(cid, n = 1, type = "lag"), by = c("text", "refind")]
    
    # merge tables
    text[indc[, c("I", "cDist")], cDist := cDist, on = "I"]
    
    # remember sorted result
    tmp <- text$cDist
    
  } else if (direction == "post") {
    # calculate distance in clause units by subtracting each cid from the previous cid
    # with the same refind and within the same file
    indc[, nCDist := shift(cid, n = 1, type = "lead") - cid, by = c("text", "refind")]
    
    # merge tables
    text[indc[, c("I", "nCDist")], nCDist := nCDist, on = "I"]
    
    # remember sorted result
    tmp <- text$nCDist
    
  } else {
    stop(paste0("Invalid direction key '", ante, "'"))
  }
  
  # assign special value to first mentions
  #indc[is.na(cdist), cdist := -1]
  
  # return vector
  return(tmp)
}
