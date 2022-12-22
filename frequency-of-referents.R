# This script was written by Maria Vollmer (University of Freiburg, 
# Australian National University) and Jan Boockmann (University of Bamberg).

# This script is used in the analysis of the following paper:
# Vollmer, Maria. Accepted with minor revision. Comparing zero and referential 
# choice in eight languages with a focus on Mandarin Chinese. Studies in language.

# If you use this script, please make sure to cite the paper above. 
# If you have any questions, you can contact me via email: mariacvollmer@gmail.com

# This script computes the overall frequency of referents 
# which is a variable in the computation of the decision trees.


# returns a vector stating whether a refind was in the top `limit` occurences 
# within each text. 1 if it is in the top, 0 otherwise.

frequency_of_referents <- function(x, limit=4) {
  # copy table
  text <- copy(refs)
  
  xWithinTop <- c()
  
  # for each text ...
  for (name in unique(refs$text)) {
    # rows of the current text
    items <- refs[refs$text == name]
    
    # observed refinds of the current text
    refinds <- unique(items$refind)
    
    # assign each refind the number of its occurence
    for (xrefind in refinds) {
      items[xrefind==items$refind, occurence := nrow(items[xrefind==items$refind])]
    }
    
    # sort the number of occurences
    occurences <- sort(unique(items$occurence), decreasing = TRUE)
    
    # pick the first `limit` elements, i.e., the ones that occured most often
    top <- head(occurences, limit)
    
    # if a refind is within the top -> assign 1, otherwise 0
    items[is.element(occurence, top), withinTop := 1]
    items[!is.element(occurence, top), withinTop := 0]
   
     # add the withinTop vector of the current text to the overall vector
    xWithinTop <- c(xWithinTop, items$withinTop)
  }
  return (xWithinTop)
}
