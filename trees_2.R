# Authors:
# - Maria Vollmer (University of Freiburg, Australian National University) 
# - Jan Boockmann (University of Bamberg)
#
# This script is used in the analysis of the following paper:
# Vollmer, Maria. Accepted with minor revision. Comparing zero and referential 
# choice in eight languages with a focus on Mandarin Chinese. Studies in language.
#
# If you use this script, please make sure to cite the paper above. 
# If you have any questions, you can contact me via email: mariacvollmer@gmail.com
#
# Description:
# This script produces the binary classification trees for the decision 
# between pronoun and zero; including all variables; once for all languages 
# and once for only Mandarin.


#### library
library(curl)
library(multicastR)
getwd()
library(data.table)
library(ggplot2)
theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)
library(RColorBrewer)
library(rms)
library(rpart)
library(rpart.plot)

# set to the directory in which you have your data
setwd(".")

source("antecedent-distance.R")
source("frequency-of-referents.R")


### loading data
mc <- fread("multicast_1905.tsv", header = T,sep = "\t", colClasses = list(factor= 1:2, character = 3:11), encoding = "UTF-8")
mandarin <- fread("mc_mandarin_190626.tsv", header = T,sep = "\t", colClasses = list(factor= 1:2, character = 3:11), encoding = "UTF-8")
speaker <-fread("mc_metadata.tsv", header = T,sep = "\t", encoding = "UTF-8")


refs <- rbind(mc, mandarin)

### getting speaker metadata
refs <- refs[speaker, speaker := speaker, on = c("text")]
refs[is.na(speaker), speaker := text]

### preparing the data
refs[, I := .I]

refs[, cDist := mc_dist_clause(refs, "ante")]

### get overall frequency of referents
refs[, withinTop := frequency_of_referents(refs,2)]

# remove rows without refind
# note that this also removes all texts that do not contain refinds at all, e.g., kent02
refs <- refs[!""==refs$refind,]


# remove rows where cDist is NA (= first mentions without antecedent and forms without referent index are excluded)
refs <- refs[!is.na(refs$cDist),]


# alternative that was not implemented in my study:
# replace NA with 1000
# refs <- refs[is.na(cDist), cDist := 1000]

# getting just 0, np, and pro (without other etc.)
refs <- refs[grepl("0|np|pro", gform), ]

# excluding 'nc'
refs <- refs[!grepl("nc", gform), ]

# excluding persian
refs <- refs[!grepl("persian", corpus), ]

# correcting h_s mistake
unique(refs$ganim)
refs[ganim == "h_s", ganim := "h"]


## Binary classification trees

## Package 

library(rpart)
library(rpart.plot)

## Getting only np, pro and zero
unique(refs$gform)
refs[!grepl("f0", gform), ]
refs[grepl("np", gform), gform := "np"]
refs[grepl("pro", gform), gform := "pro"]
refs[grepl("0", gform), gform := "0"]

## Anthropomorphised referents are analysed as human
unique(refs$ganim)
refs[grepl("d", ganim), ganim := "h"]

## Tidying up syntactic functions
unique(refs$gfunc)
refs[grepl("pred", gfunc), gfunc := "other"]
refs[grepl("voc", gfunc), gfunc := "other"]
refs[grepl("dt", gfunc), gfunc := "dt"]
refs[grepl("appos", gfunc), gfunc := "other"]
refs[grepl("lvc", gfunc), gfunc := "other"]
refs[grepl("ncs", gfunc), gfunc := "s"]
refs[grepl("s_cp", gfunc), gfunc := "s"]
refs[grepl("a_cp", gfunc), gfunc := "a"]
refs[grepl("s_cv", gfunc), gfunc := "s"]
refs[grepl("a_cps_cv", gfunc), gfunc := "a"]
refs[grepl("s_ds_cv", gfunc), gfunc := "s_ds"]
refs[grepl("a_cv", gfunc), gfunc := "a"]
refs[grepl("s_cps_ds_cv", gfunc), gfunc := "s_ds"]
refs[grepl("s_cps", gfunc), gfunc := "s"]
refs[grepl("s_cps_ds", gfunc), gfunc := "s_ds"]
refs[grepl("a_ds_cv", gfunc), gfunc := "a"]
refs[grepl("s_in", gfunc), gfunc := "s"]
refs[grepl("a_pc", gfunc), gfunc := "a"]
refs[grepl("a_cps_in", gfunc), gfunc := "a"]
refs[grepl("a_cpa", gfunc), gfunc := "a"]
refs[grepl("s_pc", gfunc), gfunc := "s"]
refs[grepl("a_cpa_pc", gfunc), gfunc := "a"]
refs[grepl("s_cpa", gfunc), gfunc := "s"]
refs[grepl("s_ds_pc", gfunc), gfunc := "s_ds"]
refs[grepl("a_cpa_in", gfunc), gfunc := "a"]
refs[grepl("s_cps_in", gfunc), gfunc := "s"]
refs[grepl("g", gfunc), gfunc := "obl"]
refs[grepl("l", gfunc), gfunc := "obl"]
refs[grepl("a_ds", gfunc), gfunc := "a"]
refs[grepl("a_in", gfunc), gfunc := "a"]
refs[grepl("a_a", gfunc), gfunc := "a"]
refs[grepl("p_a", gfunc), gfunc := "p"]
refs[grepl("p_u", gfunc), gfunc := "p"]
refs[grepl("a_u_ds", gfunc), gfunc := "a"]
refs[grepl("obl_dom", gfunc), gfunc := "obl"]
refs[grepl("other_ds", gfunc), gfunc := "other"]
refs[grepl("a_u", gfunc), gfunc := "a"]

## Excluding persian, and some syntactic functions
refs <- refs[!grepl("persian", corpus), ]
refs <- refs[!grepl("other", gfunc), ]
refs <- refs[!grepl("poss", gfunc), ]
refs <- refs[!grepl("s_ds", gfunc), ]
refs <- refs[!grepl("p2", gfunc), ]

## Only including subjects, objects, and obliques
refs <- refs[grepl("s|a|p|obl", gfunc), ]

### remove NP
refs <- refs[!"np"==refs$gform,]


## Binary classification tree (all languages)
mytree <- rpart(
  gform ~ ganim + gfunc + corpus + cDist + withinTop, 
  # data = refs[1:24000,],
  data = refs,
  control = rpart.control(minsplit = 10, maxdepth = 30)
)

## Plot

rpart.plot(mytree)


## Binary classification tree (only Mandarin)

mytree2 <- rpart(
  gform ~ ganim + gfunc + corpus + speaker + cDist + withinTop, 
  # data = refs[1:24000,],
  data = refs[grepl("mandarin", corpus), ],
  control = rpart.control(minsplit = 10, maxdepth = 30)
)


## Plot
rpart.plot(mytree2)





