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
# This script produces the rates of zero, pronoun and lexical expressions.

#### library
library(curl)
library(multicastR)
getwd()
# set to the directory in which you have your data
setwd(".")
source("antecedent-distance.R")
getwd()
library(data.table)
library(ggplot2)
theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)
library(RColorBrewer)
library(rms)

### loading data
mc <- fread("mc_1905.tsv", header = T,sep = "\t", colClasses = list(factor= 1:2, character = 3:11), encoding = "UTF-8")
mandarin <- fread("mc_mandarin_190626.tsv", header = T,sep = "\t", colClasses = list(factor= 1:2, character = 3:11), encoding = "UTF-8")
speaker <-fread("mc_metadata.tsv", header = T,sep = "\t", encoding = "UTF-8")

refs <- rbind(mc, mandarin)

### getting speaker metadata
refs <- refs[speaker, speaker := speaker, on = c("text")]
refs[is.na(speaker), speaker := text]

### preparing the data
refs[, I := .I]

# getting just 0, np, and pro (without other etc.)
refs <- refs[grepl("0|np|pro", gform), ]

# only third person
refs <- refs[!grepl("1|2", ganim), ]

# excluding f0
refs <- refs[!grepl("f0", gform), ]

# excluding 'nc'
refs <- refs[!grepl("nc", gform), ]

# excluding persian
refs <- refs[!grepl("persian", corpus), ]

# correcting h_s mistake
unique(refs$ganim)
refs[ganim == "h_s", ganim := "h"]

# only including subjects, objects, goals and obliques
unique(refs$gfunc)
refs <- refs[!grepl("other", gfunc), ]
refs <- refs[!grepl("pred", gfunc), ]
refs <- refs[!grepl("poss", gfunc), ]
refs <- refs[!grepl("voc", gfunc), ]
refs[gfunc == "s_ds", gfunc := "s"]
refs <- refs[!grepl("appos", gfunc), ]
refs <- refs[!grepl("dt", gfunc), ]
refs <- refs[!grepl("lvc", gfunc), ]
refs[gfunc == "obl_dom", gfunc := "obl"]
refs[gfunc == "s_cpa_cv", gfunc := "s"]
refs[gfunc == "a_ds_cps", gfunc := "a"]
refs[gfunc == "a_a", gfunc := "a"]
refs[gfunc == "p_a", gfunc := "p"]
refs[gfunc == "a_cps_cv", gfunc := "a"]
refs[gfunc == "s_ds_cv", gfunc := "s"]
refs[gfunc == "a_cpa_in", gfunc := "a"]
refs[gfunc == "s_cps_in", gfunc := "s"]
refs[gfunc == "a_cps_pc", gfunc := "a"]
refs <- refs[!grepl("ncs_cpa_cv", gfunc), ]
refs <- refs[!grepl("ncs_cpa_pc", gfunc), ]
refs[gfunc == "a_cps_cps_cv", gfunc := "a"]
refs[gfunc == "p_u", gfunc := "p"]
refs[gfunc == "a_u_ds", gfunc := "a"]
refs[gfunc == "l", gfunc := "butter"]
refs <- refs[!grepl("butter", gfunc), ]
refs <- refs[!grepl("ncs", gfunc), ]
refs <- refs[!grepl("p2", gfunc), ]
refs[gfunc == "a_ds", gfunc := "a"]
refs[gfunc == "s_cv", gfunc := "s"]
refs[gfunc == "a_cv", gfunc := "a"]
refs[gfunc == "s_cps_cv", gfunc := "s"]
refs[gfunc == "a_cps", gfunc := "a"]
refs[gfunc == "s_cps_ds_cv", gfunc := "s"]
refs[gfunc == "s_cps", gfunc := "s"]
refs[gfunc == "s_cps_ds", gfunc := "s"]
refs[gfunc == "a_ds_cv", gfunc := "a"]
refs[gfunc == "s_in", gfunc := "s"]
refs[gfunc == "a_pc", gfunc := "a"]
refs[gfunc == "a_cps_in", gfunc := "a"]
refs[gfunc == "a_cpa", gfunc := "a"]
refs[gfunc == "s_pc", gfunc := "s"]
refs[gfunc == "a_cpa_pc", gfunc := "a"]
refs[gfunc == "a_in", gfunc := "a"]
refs[gfunc == "s_cpa", gfunc := "s"]
refs[gfunc == "s_ds_pc", gfunc := "s"]
refs[gfunc == "a_in", gfunc := "a"]
refs[gfunc == "a_cpa_cv", gfunc := "a"]
refs[gfunc == "s_cps_pc", gfunc := "s"]
refs[gfunc == "s_ds_cps_cv", gfunc := "s"]
refs[gfunc == "a_ds_cps_cv", gfunc := "a"]



### calculating tables

nAll <- refs[, .N, by = "corpus"]
nZero <- refs[grepl("0", gform), .N, by = "corpus", ]
nPro <- refs[grepl("pro", gform), .N, by = "corpus"]
counts <- nAll
counts$nZero <- nZero$N
counts$nPro <- nPro$N
counts$nNP <- nAll$N - (nZero$N + nPro$N)

counts$pZero <- round(100 * counts$nZero / counts$N, 2)
counts$pPro <- round(100 * counts$nPro / counts$N, 2)
counts$pNP <- round(100 * counts$nNP / counts$N, 2)

setorder(counts, -pZero)
counts[, corpus := factor(corpus, ordered = TRUE, levels = counts$corpus)]

#### ggplot rate of zero
rateofzero <- ggplot(counts, aes(x = corpus, y = pZero, fill = corpus)) +
                geom_col() +
                scale_y_continuous(limits = c(0, 100)) +
                scale_fill_manual(values = c("mediumturquoise", "lightblue", "mediumturquoise", "lightcoral",
                                             "mediumturquoise",  "lightblue", "mediumturquoise", "lightblue")) +
                theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

rateofzero


#### ggplot rate of pros

setorder(counts, -pPro)
counts[, corpus := factor(corpus, ordered = TRUE, levels = counts$corpus)]
rateofpro <- ggplot(counts, aes(x = corpus, y = pPro, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", "lightblue", "mediumturquoise",  "lightblue",
                               "mediumturquoise", "lightcoral", "mediumturquoise", "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

rateofpro

#### ggplot rate of nps

setorder(counts, -pNP)
counts[, corpus := factor(corpus, ordered = TRUE, levels = counts$corpus)]
rateofnp <- ggplot(counts, aes(x = corpus, y = pNP, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("lightcoral", "mediumturquoise", "lightblue", "mediumturquoise",  "lightblue",
                               "mediumturquoise", "lightblue", "mediumturquoise", "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

rateofnp

#### getting speaker data in table, and getting animacy

nAll <- refs[, .N, by = c("corpus", "speaker")]
nZero <- refs[grepl("0", gform), .N, by = c("corpus", "speaker")]
nPro <- refs[grepl("pro", gform), .N, by = c("corpus", "speaker")]
counts <- nAll
counts$nZero <- nZero$N
counts$nPro <- nPro$N
counts$nNP <- nAll$N - (nZero$N + nPro$N)

counts$pZero <- round(100 * counts$nZero / counts$N, 2)
counts$pPro <- round(100 * counts$nPro / counts$N, 2)
counts$pNP <- round(100 * counts$nNP / counts$N, 2)

counts <- counts[corpus != "persian"]

# speaker variation in frequency of noun phrases
setorder(counts, -pNP)
counts[, corpus := factor(corpus, ordered = TRUE, levels = counts$corpus)]
boxplot1 <- ggplot(counts, aes(x = corpus, y = pNP, fill = corpus)) +
  #facet_wrap(~ corpus) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", "lightblue", "mediumturquoise",  "lightblue",
                               "mediumturquoise", "lightblue", "mediumturquoise", "lightcoral")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

boxplot1

# speaker variation in frequency of zero

setorder(counts, -pZero)
counts[, corpus := factor(corpus, ordered = TRUE, levels = counts$corpus)]
boxplot2 <- ggplot(counts, aes(x = corpus, y = pZero, fill = corpus)) +
  #facet_wrap(~ corpus) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", "lightblue", "mediumturquoise",  "lightblue",
                               "mediumturquoise", "lightblue", "mediumturquoise", "lightcoral")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

boxplot2



# only subjects
unique(refs2$gfunc)

refs2 <- refs[grepl("s|a", gfunc), ]
refs2 <- refs2[!grepl("appos|ds|nc|poss|dt", gfunc), ]


nAll2 <- refs2[, .N, by = "corpus"]
nZero2 <- refs2[grepl("0", gform), .N, by = "corpus", ]
nPro2 <- refs2[grepl("pro", gform), .N, by = "corpus"]
counts2 <- nAll2
counts2$nZero2 <- nZero2$N
counts2$nPro2 <- nPro2$N
counts2$nNP2 <- nAll2$N - (nZero2$N + nPro2$N)

counts2$pZero2 <- round(100 * counts2$nZero2 / counts2$N, 2)
counts2$pPro2 <- round(100 * counts2$nPro2 / counts2$N, 2)
counts2$pNP2 <- round(100 * counts2$nNP2 / counts2$N, 2)

setorder(counts2, -pZero2)
counts2[, corpus := factor(corpus, ordered = TRUE, levels = counts2$corpus)]

#### ggplot rate of zero (subjects)
rateofzero02 <- ggplot(counts2, aes(x = corpus, y = pZero2, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", "lightblue", "mediumturquoise", "lightcoral",
                               "mediumturquoise",  "lightblue", "mediumturquoise", "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

rateofzero02


#### ggplot rate of pro (subjects)

setorder(counts2, -pPro2)
counts2[, corpus := factor(corpus, ordered = TRUE, levels = counts2$corpus)]
rateofpro22 <- ggplot(counts2, aes(x = corpus, y = pPro2, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", "lightblue", "mediumturquoise",  "lightblue", "lightcoral",
                               "mediumturquoise", "mediumturquoise", "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

rateofpro22

#### ggplot rate of np subjects

setorder(counts2, -pNP2)
counts2[, corpus := factor(corpus, ordered = TRUE, levels = counts2$corpus)]
rateofnp32 <- ggplot(counts2, aes(x = corpus, y = pNP2, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("lightcoral", "mediumturquoise", "lightblue", "mediumturquoise",  "lightblue",
                               "mediumturquoise", "lightblue", "mediumturquoise", "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

rateofnp32

################################

### calulating tables with syntactic funtion

# all syntactic functions except for subjects
unique(refs$gfunc)

refs3 <- refs[grepl("p|g|l|poss|dt|obl|appos", gfunc), ]
refs3 <- refs3[!grepl("s_|a_|ncs_|pred|lvc", gfunc), ]

unique(refs3$gfunc)

nAll3 <- refs3[, .N, by = "corpus"]
nZero3 <- refs3[grepl("0", gform), .N, by = "corpus", ]
nPro3 <- refs3[grepl("pro", gform), .N, by = "corpus"]
counts3 <- nAll3
counts3$nZero3 <- nZero3$N
counts3$nPro3 <- nPro3$N
counts3$nNP3 <- nAll3$N - (nZero3$N + nPro3$N)

counts3$pZero3 <- round(100 * counts3$nZero3 / counts3$N, 2)
counts3$pPro3 <- round(100 * counts3$nPro3 / counts3$N, 2)
counts3$pNP3 <- round(100 * counts3$nNP3 / counts3$N, 2)

setorder(counts3, -pZero3)
counts3[, corpus := factor(corpus, ordered = TRUE, levels = counts3$corpus)]

#### ggplot rate of all except subjects (object, goal, oblique)
rateofall03 <- ggplot(counts3, aes(x = corpus, y = pZero3, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", "lightblue", "mediumturquoise", 
                                "mediumturquoise",  "lightcoral", "lightblue", "mediumturquoise", "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

rateofall03


####################### only objects
refs4 <- refs[grepl("p", gfunc), ]
refs4 <- refs4[!grepl("s_|a_|ncs_|pred|lvc|poss|dt|appos", gfunc), ]
unique(refs4$gfunc)

nAll4 <- refs4[, .N, by = "corpus"]
nZero4 <- refs4[grepl("0", gform), .N, by = "corpus", ]
nPro4 <- refs4[grepl("pro", gform), .N, by = "corpus"]
counts4 <- nAll4
counts4$nZero4 <- nZero4$N
counts4$nPro4 <- nPro4$N
counts4$nNP4 <- nAll4$N - (nZero4$N + nPro4$N)

counts4$pZero4 <- round(100 * counts4$nZero4 / counts4$N, 2)
counts4$pPro4 <- round(100 * counts4$nPro4 / counts4$N, 2)
counts4$pNP4 <- round(100 * counts4$nNP4 / counts4$N, 2)

setorder(counts4, -pZero4)
counts4[, corpus := factor(corpus, ordered = TRUE, levels = counts3$corpus)]

onlyobjects <- ggplot(counts4, aes(x = corpus, y = pZero4, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", "lightblue", "mediumturquoise", 
                               "mediumturquoise",  "lightcoral", "lightblue", "mediumturquoise", "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

onlyobjects

############################# cite R and RStudio

citation(package = "RStudio")
RStudio.Version()

#################################### barplots only zero and pro (no np)

### loading data
mc <- fread("mc_1905.tsv", header = T,sep = "\t", colClasses = list(factor= 1:2, character = 3:11), encoding = "UTF-8")
mandarin <- fread("mc_mandarin_190626.tsv", header = T,sep = "\t", colClasses = list(factor= 1:2, character = 3:11), encoding = "UTF-8")
speaker <-fread("mc_metadata.tsv", header = T,sep = "\t", encoding = "UTF-8")

refs <- rbind(mc, mandarin)

### getting speaker metadata
refs <- refs[speaker, speaker := speaker, on = c("text")]
refs[is.na(speaker), speaker := text]

### preparing the data
refs[, I := .I]

# getting just 0 and pro (without np, other etc.)
refs <- refs[grepl("0|pro", gform), ]

# excluding f0
refs <- refs[!grepl("f0", gform), ]

# excluding 'nc'
refs <- refs[!grepl("nc", gform), ]

# excluding persian
refs <- refs[!grepl("persian", corpus), ]

# correcting h_s mistake
unique(refs$ganim)
refs[ganim == "h_s", ganim := "h"]

# only including subjects, objects, goals and obliques
unique(refs$gfunc)
refs <- refs[!grepl("other", gfunc), ]
refs <- refs[!grepl("pred", gfunc), ]
refs <- refs[!grepl("poss", gfunc), ]
refs <- refs[!grepl("voc", gfunc), ]
refs[gfunc == "s_ds", gfunc := "s"]
refs <- refs[!grepl("appos", gfunc), ]
refs <- refs[!grepl("dt", gfunc), ]
refs <- refs[!grepl("lvc", gfunc), ]
refs[gfunc == "obl_dom", gfunc := "obl"]
refs[gfunc == "s_cpa_cv", gfunc := "s"]
refs[gfunc == "a_ds_cps", gfunc := "a"]
refs[gfunc == "a_a", gfunc := "a"]
refs[gfunc == "p_a", gfunc := "p"]
refs[gfunc == "a_cps_cv", gfunc := "a"]
refs[gfunc == "s_ds_cv", gfunc := "s"]
refs[gfunc == "a_cpa_in", gfunc := "a"]
refs[gfunc == "s_cps_in", gfunc := "s"]
refs[gfunc == "a_cps_pc", gfunc := "a"]
refs <- refs[!grepl("ncs_cpa_cv", gfunc), ]
refs <- refs[!grepl("ncs_cpa_pc", gfunc), ]
refs[gfunc == "a_cps_cps_cv", gfunc := "a"]
refs[gfunc == "p_u", gfunc := "p"]
refs[gfunc == "a_u_ds", gfunc := "a"]
refs[gfunc == "l", gfunc := "butter"]
refs <- refs[!grepl("butter", gfunc), ]
refs <- refs[!grepl("ncs", gfunc), ]
refs <- refs[!grepl("p2", gfunc), ]
refs[gfunc == "a_ds", gfunc := "a"]
refs[gfunc == "s_cv", gfunc := "s"]
refs[gfunc == "a_cv", gfunc := "a"]
refs[gfunc == "s_cps_cv", gfunc := "s"]
refs[gfunc == "a_cps", gfunc := "a"]
refs[gfunc == "s_cps_ds_cv", gfunc := "s"]
refs[gfunc == "s_cps", gfunc := "s"]
refs[gfunc == "s_cps_ds", gfunc := "s"]
refs[gfunc == "a_ds_cv", gfunc := "a"]
refs[gfunc == "s_in", gfunc := "s"]
refs[gfunc == "a_pc", gfunc := "a"]
refs[gfunc == "a_cps_in", gfunc := "a"]
refs[gfunc == "a_cpa", gfunc := "a"]
refs[gfunc == "s_pc", gfunc := "s"]
refs[gfunc == "a_cpa_pc", gfunc := "a"]
refs[gfunc == "a_in", gfunc := "a"]
refs[gfunc == "s_cpa", gfunc := "s"]
refs[gfunc == "s_ds_pc", gfunc := "s"]
refs[gfunc == "a_in", gfunc := "a"]
refs[gfunc == "a_cpa_cv", gfunc := "a"]
refs[gfunc == "s_cps_pc", gfunc := "s"]
refs[gfunc == "s_ds_cps_cv", gfunc := "s"]
refs[gfunc == "a_ds_cps_cv", gfunc := "a"]


### calulating tables

nAll <- refs[, .N, by = "corpus"]
nZero <- refs[grepl("0", gform), .N, by = "corpus", ]
nPro <- refs[grepl("pro", gform), .N, by = "corpus"]
counts <- nAll
counts$nZero <- nZero$N
counts$nPro <- nPro$N

counts$pZero <- round(100 * counts$nZero / counts$N, 2)
counts$pPro <- round(100 * counts$nPro / counts$N, 2)

setorder(counts, -pZero)
counts[, corpus := factor(corpus, ordered = TRUE, levels = counts$corpus)]

#### ggplot rate of zero
rateofzero <- ggplot(counts, aes(x = corpus, y = pZero, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", "lightblue", "lightcoral", "mediumturquoise", 
                               "mediumturquoise",  "lightblue", "mediumturquoise", "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

rateofzero

