library(shiny)
library(dplyr)

library(RColorBrewer)
library(scales)
library(lattice)

library(data.table)
library(gdata) # read xlsx file

#
# Load modules
#

source("/Users/song/OneDrive/code/R_Shiny/皇帝寿命/modules/birthPlace.R", chdir = TRUE)
personalInfo <- read.csv2("/Users/song/OneDrive/code/R_Shiny/皇帝寿命/data/personalInfo.csv", header = TRUE, sep = ",")
daynstyInfo <- read.csv2("/Users/song/OneDrive/code/R_Shiny/皇帝寿命/data/dynastyInfo.csv", header = TRUE, sep = ",")
