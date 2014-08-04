# Header ------------------------------------------------------------------

#****************************************************************************************************
#****************************************************************************************************
#*** Script title: Enrolments Pathways Analysis
#*** Purpose: # This script analyses articulation pathways between programs within RMIT. 
#*** This is based on data extracted from SAP BO Web Intelligence, from the SAMS universe, and expects the following columns:
#*** "Student.Hash.Identifier"               "Term.Year"                             "Program.Code"                          "Program.Description"                  
#*** "Program.College.Code"                  "Program.College.Name"                  "Program.School.Code"                   "Program.School.Name"                  
#*** "Program.Primary.Minor.FOE.Code"        "Program.Primary.Minor.FOE.Description" "Program.Career.Level"                  "Term.Career.Level"                    
#*** "Term.Code"                             "EFTSL"  
#***
#*** Authors: Niall Ridge, Andrew Dun 
#*** Created: July 21, 2014
#***
#*** Notes on code:
#***
#****************************************************************************************************
#****************************************************************************************************

# Global Setup -------------------------------------------------------

############# SCRIPT PARAMETERS 

### FOLDER PARAMETERS
# Niall's Dropbox Path
# basePath <- "/Users/niall/Dropbox/BI swap folder/R code/Pathways/"
# Andrew's Mac Path
# basePath <- "/Users/andrewdun/Documents/R/"
# Andrew's PC Path
basePath <- "\\\\ntapprdfs01n01.rmit.internal/el8/E00148/20140616 Pathways BI Output/"

setwd(basePath)

dataPath <- "PMerge/"
outputPath <- "Pdf/"

# dataPath <- paste0(basePath, "PMerge/"
# outputPath <- paste0(basePath, "Pdf/")

### PRINT PARAMETERS AND FUNCTIONS

defaultUnits <- "cm"
defaultWidth <- 65.0
defaultHeight <- 29.7



# outputPDF(outBoundPlot, x, y, "following", programs)
# outputNormalPDF <- function (ggPlot, x, y, descriptor, programsData) { 
#   
#   
#   
#   # Get full description for program x, as well as school and college info
#   collegeCode <- getDescription(x, "Program.College.Code", programsData)
#   collegeName <- getDescription(x, "Program.College.Name", programsData)
#   schoolCode <- getDescription(x, "Program.School.Code", programsData)
#   schoolName <- getDescription(x, "Program.School.Name", programsData)
#   progDescription <- getDescription(x, "Program.Description", programsData)
#   
#   standardFileName <- buildFileName(x, y, progDescription, collegeCode, schoolCode, schoolName, descriptor)
#   
#   ggsave(filename=paste0(outputPath,standardFileName), 
#         plot=ggPlot, width=defaultWidth, height=defaultHeight, units=defaultUnits) }
# 
# outputErrorPDF <- function (x, y, descriptor, programsData) { 
#   
#                                 # Get full description for program x, as well as school and college info
#   collegeCode <- getDescription(x, "Program.College.Code", programsData)
#   collegeName <- getDescription(x, "Program.College.Name", programsData)
#   schoolCode <- getDescription(x, "Program.School.Code", programsData)
#   schoolName <- getDescription(x, "Program.School.Name", programsData)
#   progDescription <- getDescription(x, "Program.Description", programsData)
#                                 # paste0 etc - and same for filenames
#                                 
#                                 collegeFUllDesc <- paste0(collegeCode," - ",collegeName)
#                                 schoolFUllDesc <- paste0(schoolCode," - ",schoolName)
#                                 programFullDesc <- paste0(x," - ",progDescription)
#                                 
#                                 errorFileName <- buildFileName(x, y, progDescription, collegeCode, schoolCode, schoolName, descriptor)
#   
#                                 pdf(paste0(outputPath, errorFileName), 
#                                 width=cmToInches(defaultWidth), height=cmToInches(defaultHeight))
#                                 errorPlot <- plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
#                                 par(ps = 24) # Set text size to 24 point
#                                 text(5, 8, paste0("No observations of enrolments in other programs ",descriptor," enrolment in ",x," (",progDescription,")"))
#                                 text(5, 6, paste0("Program Information:"))
#                                 text(5, 5, paste0("Program: ", programFullDesc))
#                                 text(5, 4, paste0("School: ", schoolFUllDesc))
#                                 text(5, 3, paste0("College: ", collegeFUllDesc))
#                                 dev.off()
#                                 print(paste0("Warning: No observations of ",x," ",descriptor," programs")) }

outputPDF <- function (ggPlot, x, y, descriptor, programsData, type) { 
  
  buildFileName <- function (x, y, progDescription, collegeCode, schoolCode, schoolName, descriptor) {
    
    # clnse <- function(x) gsub("[[:punct:]]", " ", x)
    
    # trunc <- 15
    # paste0(Sys.Date()," ",collegeCode," ",schoolCode," - ",x," - 2009-2013 top ",y," ",descriptor," programs by year.pdf")
    # paste0(Sys.Date()," ",collegeCode," ",schoolCode," ",x," ",schoolName," - ",progDescription," - 2009-2013 Top ",y," ",descriptor," programs by year.pdf")
    baseName <- paste0(Sys.Date()," ",collegeCode," ",schoolCode," ",schoolName," - ",x," ",progDescription," - 2009-2013 Top ",y," ",descriptor," progs by year.pdf")
    gsub("/", " ", baseName)
  }
  
  getDescription <- function(progCode, targetColumn, programsD) {
    eval(parse(text=paste0("subset(programsD, Program.Code==progCode)$",targetColumn)))
  }

  # Get full description for program x, as well as school and college info
  collegeCode <- getDescription(x, "Program.College.Code", programsData)
  collegeName <- getDescription(x, "Program.College.Name", programsData)
  schoolCode <- getDescription(x, "Program.School.Code", programsData)
  schoolName <- getDescription(x, "Program.School.Name", programsData)
  progDescription <- getDescription(x, "Program.Description", programsData)
  # paste0 etc - and same for filenames
  
  collegeFUllDesc <- paste0(collegeCode," - ",collegeName)
  schoolFUllDesc <- paste0(schoolCode," - ",schoolName)
  programFullDesc <- paste0(x," - ",progDescription)
  
  fileName <- buildFileName(x, y, progDescription, collegeCode, schoolCode, schoolName, descriptor)
  
  if (type=="Normal") {
  
    ggsave(filename=paste0(outputPath,fileName), 
           plot=ggPlot, width=defaultWidth, height=defaultHeight, units=defaultUnits) 
  
  } else {
  
    pdf(paste0(outputPath, fileName), 
        width=cmToInches(defaultWidth), height=cmToInches(defaultHeight))
    errorPlot <- plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    par(ps = 24) # Set text size to 24 point
    text(5, 8, paste0("No observations of enrolments in other programs ",descriptor," enrolment in ",x," (",progDescription,")"))
    text(5, 6, paste0("Program Information:"))
    text(5, 5, paste0("Program: ", programFullDesc))
    text(5, 4, paste0("School: ", schoolFUllDesc))
    text(5, 3, paste0("College: ", collegeFUllDesc))
    dev.off()
    print(paste0("Warning: No observations of ",x," ",descriptor," programs")) }

}

### UTILITY FUNCTIONS

# For later use, define a function for formatting to percentage with 1 decimal place
Perc <- function(x) sprintf("%1.1f%%", 100*x)

cmToInches <- function(x) x/cm(1) # convert from inches to centimeteres using the inverse function cm()

#****************************************************************************************************
#****************************************************************************************************

#############################################################################################
##  Initial data extraction                                                               ###
#############################################################################################

# This data needs to be exported by year from BI as a CSV file (can't do all years at once due to export file size limitations)
# Place these files in a folder and merge into a single CSV from command prompt
# > copy *.csv PMerge.csv

# Import merged CSV to R dataframe. Using system.time to compare the efficiency of the operation on different machines
system.time(pMerge <- read.csv(paste0(dataPath,"PMerge.csv")), header=T))

#remove repeating file headers. This step can be excluded if file is cleaned up
pMergeStudProg <- pMerge[pMerge$Program.Code != "Program Code", ]

# Concurrent diplomas (i.e. Diploma in Languages, DP005) are likely to complicate pathways analysis, as they'll
# result in multiple enrolments for a given term code. These aren't essential to our pathways analysis,
# therefore we'll remove them from our data. Turns out this removes less than 200 rows
pMergeStudProg <- pMergeStudProg[pMergeStudProg$Program.Code != "DP005", ]

# exclude rows where student ID is blank (there shouldn't be any such rows but in sample data there was one row in the 2012 data)
pMergeStudProg <- pMergeStudProg[pMergeStudProg$Student.Hash.Identifier != "", ]

# add leading zeroes on to term code to allow alphabetic sorting to work
pMergeStudProg$Term.Code <- sprintf("%04s",pMergeStudProg$Term.Code)

# add a key to the end of input file so we can track records as they flow through 
pMergeStudProg$Key <- paste(pMergeStudProg$Student.Hash.Identifier, pMergeStudProg$Program.Code,pMergeStudProg$Term.Code, sep=":")

# create a reference table of program data
programs <- pMergeStudProg[,c("Program.Code","Program.Description","Program.College.Code","Program.College.Name","Program.School.Code","Program.School.Name","Program.Primary.Minor.FOE.Code","Program.Primary.Minor.FOE.Description","Program.Career.Level")]
programs <- programs[!duplicated(programs),]

# make a copy of the programs table to hold the next program (program articulated to), and set the names to be the same as original, but with "Next." appended to the front
nextPrograms <- programs
colnames(nextPrograms) <- lapply(X=colnames(programs),function(x) sprintf("Next.%s",x))

# ===================== Optionally create output file for debugging ======================== 
#write.csv(pMergeStudProg, file = "/Users/niall/Dropbox/BI swap folder/R code/Pathways/full_ordered_data.csv")

#############################################################################################
##  Create a single line per studentID:Program combination                                ###
#############################################################################################

# We now need to take our data and for each student ID, reduce the data to a chronological set of unique
# program codes (i.e. showing only the record for the term where the student first enrolled in each attempted program)
# REF: http://stackoverflow.com/questions/12495345/find-indices-of-duplicated-rows

# first sort by student, program, then term, in preparation for deduplication
pMergeStudProg <- pMergeStudProg[order(pMergeStudProg$Student.Hash.Identifier, pMergeStudProg$Program.Code, pMergeStudProg$Term.Code),]

# then remove duplicates, keeping only the earliest term for each student/program combination
pMergeStudProgDeDup <- pMergeStudProg[!duplicated(pMergeStudProg[,c("Student.Hash.Identifier","Program.Code")]),]

#############################################################################################
##  Getting details of program that student articulated to after first enrolment (if any) ###
#############################################################################################
# now that we've filtered to only show latest term for each student/program combination, we want to sort the programs for each 
# student in term order so we can do chronological comparisons
pStudProgSort <- pMergeStudProgDeDup[order(pMergeStudProgDeDup$Student.Hash.Identifier, pMergeStudProgDeDup$Term.Code, pMergeStudProgDeDup$Program.Code),]

# Now for each row, we want to mark it as an articulation row, iff Student.Hash.Id in the next row equals that of the current row.
# As a first step, we need to grab the student ID of the next row. The following grabs all the student IDs except the first ('-1' means exclude position 1), 
# appends "" to the _end_ of the list to keep the list length the same as the main table, and inserts the list as a new column that shows in each case the next student ID
# http://r.789695.n4.nabble.com/Refer-to-previous-row-tt4654852.html#a4654876
# We also use as.character.factor() to ensure that we capture next ID as string, as factors are by default reprsentated in R as levels (integers)
# REF http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
pStudProgSort$NextID <- c(as.character.factor(pStudProgSort$Student.Hash.Identifier[-1]),"")

# Knowing that the table is sorted by student ID and term code, and that only the first enrolment in a program for each student is captured,
# we can infer that where we have two consecutive rows for the same student, an articulation has taken place from one program to another.
# (Note that because graduations information is currently unavailable, all articulations are recorded, regardless of whether the prior
# program in any given pair was actually completed. Given the size of the dataset, any inaccuracies introduced by this assumption should 
# be reasonably consistent and hence should not impair the capacity of the analysis to accurately identify articulation trends. This 
# methodology is as such consistent with the 'Big Data' ethos of big, noisy data over small, highly sanitized data.)
# We can therefore derive a column marking relevant rows as articulations:
pStudProgSort$IsArtic <-  as.character.factor(pStudProgSort$Student.Hash.Identifier) == pStudProgSort$NextID

# To make the code for the following operations more concise, we'll now rename our data frame
df <- pStudProgSort
nrow(df[df$IsArtic,])
# We can now create our first set of articulation codes, starting off with program articulations
# Firstly, let's create a column which for each row shows the next program code
# as.character.factor tells R to use the text value, and not the factor number
df$NextProg <- c(as.character.factor(df$Program.Code[-1]),"")
df$Next.Term.Year <- c(as.character.factor(df$Term.Year[-1]),"")
df$Next.Term.Code <- c(df$Term.Code[-1],"")

# for all programs where an articulation occurred, populate details of the next program by merging to the nextPrograms reference table
df <- merge(x = df, y = nextPrograms, by.x="NextProg", by.y="Next.Program.Code", all.x=TRUE)
# may be worth noting here that we lose our ordering as above

# populate some values in the form of <current>.<next>, for easier plotting later on

df$ArticByProgram <- ifelse(df$IsArtic, paste(df$Program.Code, df$NextProg,sep="."), "")

# College
#df$ArticByColg <- c(paste(df$Program.College.Code, df$Next.Program.College.Code,sep="."))
df$ArticByColg <- ifelse(df$IsArtic, paste(df$Program.College.Code, df$Next.Program.College.Code,sep="."), "")

# College Name
#df$ArticByColg <- c(paste(df$Program.College.Code, df$Next.Program.College.Code,sep="."))
df$ArticByColgName <- ifelse(df$IsArtic, paste(df$Program.College.Name, df$Next.Program.College.Name,sep="."), "")

# School Code
#df$ArticBySchoolName <- c(paste(df$Program.School.Name, df$Next.Program.School.Name,sep="."))
df$ArticBySchl <- ifelse(df$IsArtic, paste(df$Program.School.Code, df$Next.Program.School.Code,sep="."), "")

# School Name
#df$ArticBySchoolName <- c(paste(df$Program.School.Name, df$Next.Program.School.Name,sep="."))
df$ArticBySchlName <- ifelse(df$IsArtic, paste(df$Program.School.Name, df$Next.Program.School.Name,sep="."), "")

# Program Description
# df$ArticByProgDesc <- ifelse(df$IsArtic, paste(as.character.factor(df$Program.Description),df$NextProgDesc,sep="."), "")
df$ArticByProgDesc <- ifelse(df$IsArtic, paste(df$Program.Description, df$Next.Program.Description,sep="."), "")

# Term Code
#df$ArticByTerm <- c(paste(df$Term.Code, df$Next.Term.Code,sep="."))
df$ArticByTerm <- ifelse(df$IsArtic, paste(df$Term.Code, df$Next.Term.Code,sep="."), "")

# Term Year
#df$ArticByTermYear <- c(paste(df$Term.Year, df$Next.Term.Year,sep="."))
df$ArticByTerm <- ifelse(df$IsArtic, paste(df$Term.Year, df$Next.Term.Year,sep="."), "")

# Career
#df$ArticByCareer <- c(paste(df$Program.Career.Level, df$Next.Program.Career.Level,sep="."))
df$ArticByCareer <- ifelse(df$IsArtic, paste(df$Program.Career.Level, df$Next.Program.Career.Level,sep="."), "")

# FOE
#df$ArticByFOE <- c(paste(df$Program.Primary.Minor.FOE.Code, df$Next.Program.Primary.Minor.FOE.Code,sep="."))
df$ArticByFOE <- ifelse(df$IsArtic, paste(df$Program.Primary.Minor.FOE.Code, df$Next.Program.Primary.Minor.FOE.Code,sep="."), "")

# FOE Description
#df$ArticByFOE <- c(paste(df$Program.Primary.Minor.FOE.Code, df$Next.Program.Primary.Minor.FOE.Code,sep="."))
df$ArticByFOEDesc <- ifelse(df$IsArtic, paste(df$Program.Primary.Minor.FOE.Description, df$Next.Program.Primary.Minor.FOE.Description,sep="."), "")

###################### ADJUST FOR 2013 ##############################################
# Need to exclude 2013 starts from analysis as these haven't had an opportunity to articulate (within this data set)
# This works because taking 2009-2012 rows still incudes 2013 in second parts of articulation pairs
dfExclude2013Starts <- subset(df, df$Term.Year != 2013)

# ===================== Optionally create output file for debugging ======================== 
#write.csv(df, file = "/Users/niall/Dropbox/BI swap folder/R code/Pathways/filtered_data.csv")

#####################################################################################
###########################  Counting and testing output  ###########################
#####################################################################################

###############################################################
############ Career Level Articulations #######################
###############################################################

# Articulations by Career Level
ArticCareer <- function(x) nrow(subset(dfExclude2013Starts, grepl(paste(x,".",sep=""),dfExclude2013Starts$ArticByCareer))) / nrow(subset(dfExclude2013Starts, grepl(x,dfExclude2013Starts$Program.Career.Level)))

# Articulations by Career Level from DSC (to anything)
ArticCareerDSC <- function(x) nrow(subset(dfExclude2013Starts, grepl(paste(x,".",sep=""),dfExclude2013Starts$ArticByCareer) & grepl("DSC", dfExclude2013Starts$Program.College.Code))) / nrow(subset(dfExclude2013Starts, grepl(x,dfExclude2013Starts$Program.Career.Level) & grepl("DSC", dfExclude2013Starts$Program.College.Code)))

# Let's compare the two functions to get DSC's performance
ArticCareerPerformance <- function(x) ArticCareerDSC(x) / ArticCareer(x)

# Let's define a function to compare DSC Career articulation performance to RMIT baseline
ArticCareerDSCCompare <- function(x) paste("Career: ", x," DSC: ",Perc(ArticCareerDSC(x))," RMIT: ",Perc(ArticCareer(x))," DSC Performance: ",Perc(ArticCareerDSC(x)/ArticCareer(x)))
# ArticCareerDSCCompareM <- function(x) c(x,Perc(ArticCareerDSC(x)),Perc(ArticCareer(x)),Perc(ArticCareerDSC(x)/ArticCareer(x)))

# Let's get a vector of all unique career codes
UniqueCareers <- dfExclude2013Starts[!duplicated(dfExclude2013Starts$Program.Career.Level),]$Program.Career.Level

# Let's run out comparison function across that vector
# CompareCareers <- lapply(UniqueCareers, ArticCareerDSCCompare)

# Now let's refactor to get these outputs into a dataframe
# Let's create a data frame to hold our results
CareersDF <- data.frame(UniqueCareers)
colnames(CareersDF) <- c("Career")
# http://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
# Unlist - http://adv-r.had.co.nz/Functionals.html
CareersDF$DSC <- unlist(lapply(CareersDF$Career, ArticCareerDSC)) # Using unlist to keep dataframe simple
CareersDF$RMIT <- unlist(lapply(CareersDF$Career, ArticCareer))
CareersDF$Performance <- unlist(lapply(CareersDF$Career, ArticCareerPerformance))

# Order data frame by DSC Performance
CareersDF <- CareersDF[order(-CareersDF$Performance),]

# Let's change our data frame values to percentages - try to refactor this...
# CareersDF$DSC <- lapply(CareersDF$DSC, Perc)
# CareersDF$RMIT <- lapply(CareersDF$RMIT, Perc)
# CareersDF$Performance <- lapply(CareersDF$Performance, Perc)
CareersDFPerc <- CareersDF
CareersDFPerc[,2:4] <- apply(CareersDFPerc[,2:4], 1:2, Perc)

# Output a pdf of the data table
# if(!"gridExtra" %in% rownames(installed.packages())) install.packages("gridExtra") # install ggplot2 if not already present
# library(gridExtra)
# pdf("data_output.pdf", height=11, width=8.5)
# grid.table(CareersDFPerc)
# dev.off()

# Plot the data
if(!"ggplot2" %in% rownames(installed.packages())) install.packages("ggplot2") # install ggplot2 if not already present
library(ggplot2)
# Using the reshape library, let's grab a flat table of the RMIT and DSC columns
# http://www.statmethods.net/management/reshape.html
if(!"reshape" %in% rownames(installed.packages())) install.packages("reshape") # install ggplot2 if not already present
library(reshape)
CareersDF.m <- melt(CareersDF[,1:3], id.vars="Career")
# Reorder careers as categorical variable, by value
# Not completely clear how this works, will it take the DSC value or the RMIT value? Works for this chart anyway
CareersDF.m <- transform(CareersDF.m, Career = reorder(Career, -value))
# Assign better column name for 'variable'
colnames(CareersDF.m)[2] <- "Segment"
# Then plot http://stackoverflow.com/questions/18158461/grouped-bar-plot-in-ggplot
if(!"scales" %in% rownames(installed.packages())) install.packages("scales")
library(scales)
# perfPlot <-  ggplot(CareersDF.m, aes(x=Career, y=value, fill=Segment)) + 
#   geom_bar(position = "dodge", stat="identity") + 
#   geom_text(aes(x=Career, y=value, ymax=value, label=percent(value)), position=position_dodge(width=1), vjust=-0.25) +
#   ggtitle("2009-2013 DSC vs RMIT Articulation Performance by Career") + ylab("Percentage Articulating")
# perfPlot
# Try a different theme
# http://docs.ggplot2.org/0.9.2.1/theme.html
# perfPlot + theme_bw()
# perfPlot + theme_grey()
# Modify colours 
# http://novyden.blogspot.com.au/2013/09/how-to-expand-color-palette-with-ggplot.html
if(!"RColorBrewer" %in% rownames(installed.packages())) install.packages("RColorBrewer") # install ggplot2 if not already present
library(RColorBrewer)
# perfPlot + theme_grey() + scale_fill_brewer(palette="Blues")
# perfPlot + theme_bw() + scale_fill_brewer(palette="Set1")
# perfPlot + theme_grey() + scale_fill_brewer(palette="Reds")

perfPlot <-  ggplot(CareersDF.m, aes(x=Career, y=value, fill=Segment)) + 
  geom_bar(position = "dodge", stat="identity") + 
  geom_text(aes(x=Career, y=value, ymax=value, label=percent(value)), position=position_dodge(width=1), vjust=-0.25) +
  ggtitle("2009-2013 DSC vs RMIT Articulation Performance by Career") + ylab("Percentage Articulating") +
  theme_grey() + scale_fill_brewer(palette="Reds")
perfPlot

# ggsave(filename=paste0(outputPath,Sys.Date()," 2009-2013 DSC vs RMIT Articulation Performance by Career.pdf"), 
#        plot=perfPlot, width=65.0, height=29.7, units="cm") # Additional width improves axis label spacing

outputPDF(perfPlot, "2009-2013 DSC vs RMIT Articulation Performance by Career")

# CONTINUE: Do same thing for program level articulations - faceted by school

###############################################################
############ Program Level Articulations #######################
###############################################################

# NONE OF THIS CODE HAS ANY DEPENDENCY ON THE CAREER LEVEL ARTICULATIONS CODE ABOV

# What is actually right:
nrow(subset(dfExclude2013Starts, grepl("C2084.",dfExclude2013Starts$ArticByProgram))) / nrow(subset(dfExclude2013Starts, grepl("C2084",dfExclude2013Starts$Program.Code)))
# Result 47%

# Associate Degree articulations %
nrow(subset(dfExclude2013Starts, grepl("AD[0-9]{3}.",dfExclude2013Starts$ArticByProgram))) / nrow(subset(dfExclude2013Starts, grepl("AD[0-9]{3}",dfExclude2013Starts$Program.Code)))
# Result 37%

# Associate Degree - DSC only - articulations %
nrow(subset(dfExclude2013Starts, grepl("AD[0-9]{3}.",dfExclude2013Starts$ArticByProgram) & grepl("DSC", dfExclude2013Starts$Program.College.Code))) / nrow(subset(dfExclude2013Starts, grepl("AD[0-9]{3}",dfExclude2013Starts$Program.Code) & grepl("DSC", dfExclude2013Starts$Program.College.Code)))
# Result 18%

# Now that we've got the hang of this we can define what we're doing in functional terms
ArticProgEnrolments <- function(x) nrow(subset(dfExclude2013Starts, grepl(x,dfExclude2013Starts$Program.Code)))
ArticProgArticulations <- function(x) nrow(subset(dfExclude2013Starts, grepl(paste(x,".",sep=""),dfExclude2013Starts$ArticByProgram)))
ArticProg <- function(x) Perc(ArticProgArticulations(x) / ArticProgEnrolments(x))

# The following function is attended to be used to constrain results to DSC college where a regular expression is provided that could match programs from outside DSC
ArticProgDSC <- function(x) nrow(subset(dfExclude2013Starts, grepl(paste(x,".",sep=""),dfExclude2013Starts$ArticByProgram) & grepl("DSC", dfExclude2013Starts$Program.College.Code))) / nrow(subset(dfExclude2013Starts, grepl(x,dfExclude2013Starts$Program.Code) & grepl("DSC", dfExclude2013Starts$Program.College.Code)))

# We can now determine our articulation % for C2085 using
ArticProg("C2085")

# Articulations from DSC Bachelor degrees
ArticProgDSC("BP[0-9]{3}")

# Likewise for program - FOR THIS WOULD BE USEFUL TO HAVE TOTAL ENROLMENTS TO TOTAL ARTICS ETC
# ArticProgCompare <- function(x) paste("Program: ",x," Enrolments: ",ArticProgEnrolments(x)," Articulations: ",ArticProgArticulations(x)," Performance: ", ArticProg(x))

# Let's get a vector of all unique DSC program codes
# UniqueDSCProgs <- dfExclude2013Starts$Program.Code[!duplicated(subset(dfExclude2013Starts,dfExclude2013Starts$Program.College.Code == "DSC")),]$Program.Code
DSCProgs <- subset(dfExclude2013Starts,dfExclude2013Starts$Program.College.Code == "DSC")
DSCProgsUnique <- DSCProgs[!duplicated(DSCProgs$Program.Code),]$Program.Code

# Now create a set of comparisons for all DSC programs
# system.time(CompareDSCProgs <- lapply(DSCProgsUnique, ArticProgCompare))

# Now let's set up a data frame
ProgsDF <- data.frame(DSCProgsUnique)
colnames(ProgsDF) <- c("Program")

system.time(ProgsDF$Enrolments <- unlist(lapply(ProgsDF$Program, ArticProgEnrolments))) # Using unlist to keep dataframe simple
system.time(ProgsDF$Articulations <- unlist(lapply(ProgsDF$Program, ArticProgArticulations)))
system.time(ProgsDF$Performance <- ProgsDF$Articulations / ProgsDF$Enrolments)

# Let's merge back in our program data
ProgsDF2 <- merge(x = ProgsDF, y = programs, by.x="Program", by.y="Program.Code", all.x=TRUE)

# For pdf output, let's...
# Order data frame by performance
# ProgsPdf <- ProgsDF2[order(-ProgsDF2$Performance),]
# Create a minimal version of the dataframe with just our desired columns
# ProgsPdf <- ProgsPdf[, c("Program.School.Code", "Program.School.Name", "Program", "Program.Description", "Program.Career.Level", "Program.Primary.Minor.FOE.Code", "Program.Primary.Minor.FOE.Description", "Enrolments", "Articulations", "Performance")]
# Update data frame with percentages for performance (NB: this replaces performance column with strings rather than integers)
# ProgsPdf$Performance <- unlist(lapply(ProgsPdf$Performance, Perc))
# Output data frame to csv
# setwd("\\\\ntapprdfs01n01.rmit.internal/el8/E00148/20140616 Pathways BI Output/Pdf")
#write.csv(file="2009-2013 DSC Program Level Articulation Performance.csv", x=ProgsPdf)
# Or direct to xlsx
#install.packages('xlsx')
#library(xlsx)
#write.xlsx(x = ProgsPdf, file = "2009-2013 DSC Program Level Articulation Performance.xlsx", sheetName = "2009-2013_DSCArticulations", row.names = FALSE)
# Output to pdf as was done for career level analysis
# library(gridExtra)
# pdf("2009-2013 DSC Program Level Articulation Performance.pdf", height=11, width=8.5)
# grid.table(ProgsPdf)
# dev.off()
# getwd()

# Plot the data
library(ggplot2)
# Using the reshape library, let's grab a flat table of the RMIT and DSC columns
# http://www.statmethods.net/management/reshape.html
library(reshape)
ProgsDF2.m <- melt(ProgsDF2, id.vars=c("Program", "Program.Primary.Minor.FOE.Code", "Program.Description", "Program.College.Code", "Program.School.Code", "Program.School.Name", "Program.Primary.Minor.FOE.Code", "Program.Primary.Minor.FOE.Description", "Program.College.Name", "Program.Career.Level"))
# Subset to remove 'Performance'
ProgsDF3.m <- subset(ProgsDF2.m, variable!="Performance")
# Rename 'variable'
colnames(ProgsDF3.m)[11] <- "Type"
# Then plot http://stackoverflow.com/questions/18158461/grouped-bar-plot-in-ggplot
library(scales)
# progPlot <-  ggplot(ProgsDF3.m, aes(x=Program, y=value, fill=Type)) + 
#   geom_bar(position = "dodge", stat="identity") + 
#   geom_text(aes(x=Program, y = value, ymax=value, label=value), position=position_dodge(width=1), vjust=-0.25) +
#   ggtitle("2009-2013 DSC Articulation Performance by Program") + ylab("Number")
# progPlot
# Way too much data, need facet by school
# progPlot + facet_wrap( ~ Program.School.Code, ncol=4, scales="free") + ggtitle("2009-2013 DSC Articulation Performance by Program (by School)")
# Still too much data, let's take top 30 for college, according to enrolment numbers
# top_programs <- ProgsDF3.m$Program [order (ProgsDF2$Enrolments, decreasing = TRUE)]
# progPlot <-  ggplot(subset(ProgsDF3.m, Program %in% top_programs[1:30]), aes(x=Program, y=value, fill=Type)) + 
#   geom_bar(position = "dodge", stat="identity") + 
#   geom_text(aes(x=Program, y = value, ymax=value, label=value), position=position_dodge(width=1), vjust=-0.25) +
#   ggtitle("2009-2013 DSC Articulation Performance by Top 30 Highest Enrolling DSC Programs (by School)") + ylab("Number")
# This may give error: "position_dodge requires constant width: output may be incorrect". However, it seems this can probably be disregarded: 
# http://stackoverflow.com/questions/14476961/why-do-i-get-position-dodge-requires-constant-width-even-though-widths-are-con
# progPlot + facet_wrap( ~ Program.School.Name, ncol=4, scales="free") 

# Let's take top 30 for college, according to enrolment numbers
top_programs <- ProgsDF3.m$Program [order (ProgsDF2$Enrolments, decreasing = TRUE)]

progPlot <-  ggplot(subset(ProgsDF3.m, Program %in% top_programs[1:30]), aes(x=Program, y=value, fill=Type)) + 
  geom_bar(position = "dodge", stat="identity") + 
  geom_text(aes(x=Program, y = value, ymax=value, label=value), position=position_dodge(width=1), vjust=-0.25) +
  ggtitle("2009-2013 DSC Articulation Performance (enrolments vs subsequent articulations) by Top 30 Highest Enrolling DSC Programs (by School)") + ylab("Number") +
  facet_wrap( ~ Program.School.Name, ncol=4, scales="free")
progPlot
# Nice!

# #### UNCOMMENT FOR PDF OUTPUT # format(Sys.Date(), "%Y%m%d")
# ggsave(filename=paste0(outputPath,Sys.Date()," 2009-2013 DSC Top 30 Articulation Performance.pdf"), 
#                        plot=progPlot, width=65.0, height=29.7, units="cm") # Additional width improves axis label spacing

outputPDF(progPlot, "2009-2013 DSC Top 30 Articulation Performance")

# ProgsDF4.m <- subset(ProgsDF2.m, variable=="Performance")
# # Rename 'variable'
# colnames(ProgsDF4.m)[11] <- "Type"
# # Then plot http://stackoverflow.com/questions/18158461/grouped-bar-plot-in-ggplot
# library(scales)
# ProgsDF5.m <- subset(ProgsDF4.m, value>0.9)
# progPlot2 <- ggplot(ProgsDF5.m, aes(x=Program, y=value, fill=Type)) + 
#   geom_bar(position = "dodge", stat="identity") + 
#   geom_text(aes(x=Program, y = value, ymax=value, label=percent(value)), position=position_dodge(width=1), vjust=-0.25) +
#   ggtitle("2009-2013 DSC Articulation Performance > 90%") + ylab("Number")
# progPlot2

###############################################################
############ Pathway Level Analysis ###########################
###############################################################

# The goal in this section is to provide a methology to readily list all the outbound and inbound pathway programs
# (i.e. forward and reverse articualtions) from a particular program code or set of program codes

# Some functions derived from above
ProgramAllRows <- function(x) subset(dfExclude2013Starts, grepl(x,dfExclude2013Starts$Program.Code))
ProgramArticulationsFrom <- function(x) subset(dfExclude2013Starts, grepl(paste(x,".",sep=""),dfExclude2013Starts$ArticByProgram))
ProgramArticulationsTo <- function(x) subset(dfExclude2013Starts, grepl(paste(".",x,sep=""),dfExclude2013Starts$ArticByProgram))
ProgramArticulationsAny <- function(x) subset(dfExclude2013Starts, grepl(x,dfExclude2013Starts$ArticByProgram))
ProgramArticulationsFrom_Performance <- function(x) Perc(nrow(ProgramArticulationsFrom(x)) / nrow(ProgramAllRows(x)))

# Quick function check
nrow(ProgramArticulationsAny("C5256")) - nrow(ProgramArticulationsFrom("C5256")) - nrow(ProgramArticulationsTo("C5256"))
# Should = 0

### OUTBOUND PROGRAMS FROM CODE OR CODES ##############################################

# Specific codes to be examined
# C5256
C5256Artics <- ProgramArticulationsFrom("C5256")
# Create contingency table to count NextProg
C5256ArticCounts <- data.frame(table(C5256Artics$NextProg))
# Add in colname
colnames(C5256ArticCounts) <- c("NextProg","Freq")
# Include a column referencing the orginating program
C5256ArticCounts$FromProgram <- "C5256"
# Merge back in rows from lookup table
C5256ArticCounts <- merge(x = C5256ArticCounts, y = nextPrograms, by.x="NextProg", by.y="Next.Program.Code", all.x=TRUE)
# Nicer to have the 'FromProgram' column first
C5256ArticCounts <- C5256ArticCounts[c(2, 1, 3:11)]

# Now generalise to a function

ProgramArticulationsFromDF <- function (x) {
  
  PrgArtics <- ProgramArticulationsFrom(x)
  PrgArticsCounts <- data.frame(table(PrgArtics$NextProg))
  # C5256ArticCounts[c(2, 1, 3:11)]
#   if(length(colnames(PrgArticsCounts)) < 2) { 
#     PrgArticsCounts$NextProg <- vector() 
#     PrgArticsCounts <- PrgArticsCounts[c(2, 1)] }
  if(length(colnames(PrgArticsCounts)) ==2 ) { # Probably unnecessary. Errors can be handled at a higher level.
    colnames(PrgArticsCounts) <- c("NextProg","Freq")
    PrgArticsCounts$FromProgram <- x
    PrgArticsCounts <- merge(x = PrgArticsCounts, y = nextPrograms, by.x="NextProg", by.y="Next.Program.Code", all.x=TRUE) }
  # return(PrgArticsCounts) # Return turns out to be unnecessary
}

# The following codes have been provided as codes of interest to DSC
C5256ArticCounts <- ProgramArticulationsFromDF("C5256")
C6097ArticCounts <- ProgramArticulationsFromDF("C6097")
C4274ArticCounts <- ProgramArticulationsFromDF("C4274")

# We could be a result via ("C5256","C6097","C4274") but instead let's define 
# a function so we can pass codes in as a vector and merge to a single data frame

# Let's place our test codes in a vector
testCodes <- c("C5256","C6097","C4274")

# Define an iterative function
ProgramArticulationsFromMulti <- function (x) {
  
  tempDF <- data.frame()
  
  for (i in x) {
    
    articsDF <- ProgramArticulationsFromDF(i)
    
    if (length(articsDF) > 0) { tempDF <- rbind(tempDF, articsDF) }

  }
  # tempDF$Freq <- order(levels(tempDF$Freq))
  return(tempDF)
}

# Execute the function on our test codes
testArtics <- ProgramArticulationsFromMulti(testCodes)

# Now let's chart, faceting by program code

# Get libraries if needed
if(!"ggplot2" %in% rownames(installed.packages())) install.packages("ggplot2") # install ggplot2 if not already present
library(ggplot2)
if(!"scales" %in% rownames(installed.packages())) install.packages("scales") # install ggplot2 if not already present
library(scales)

# For the purposes of plotting, reorder factors according to frequency
# testArtics$NextProg <- reorder(testArtics$NextProg, X=as.numeric(testArtics$Freq))

# For the purposes of plotting, merge back in the full program description for FromProgram
testArtics <- merge(x = testArtics, y = programs[, 1:2], by.x = "FromProgram", by.y = "Program.Code", all.x = TRUE)
testArtics$FromProgramDescFull <- paste(testArtics$FromProgram,testArtics$Program.Description,sep=" - ")

# Also create a full description for next program
testArtics$NextProgramDescFull <- paste(testArtics$NextProg,testArtics$Next.Program.Description,sep=" - ") 
testArtics$NextProgramDescFull <- reorder(testArtics$NextProgramDescFull, X=as.numeric(testArtics$Freq))

# outBoundPlot <-  ggplot(testArtics, aes(x=NextProg, y=Freq, fill=Next.Program.College.Code)) + 
#   geom_bar(position = "dodge", stat="identity") + 
#   geom_text(aes(x=NextProg, y = Freq, ymax=Freq, label=Freq), position=position_dodge(width=1), vjust=-0.25) +
#   ggtitle("2009-2013 DSC Selected Outbound") + ylab("Number") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# outBoundPlot
# Facet by program code
# outBoundPlot + facet_wrap( ~ FromProgram, ncol=4, scales="free") + ggtitle("2009-2013 DSC Selected Outbound (by Originating Program)")
# Just fits okay on A3

# Use just the program code on the x axis
# outBoundPlot <-  ggplot(testArtics, aes(x=NextProg, y=Freq, fill=Next.Program.College.Code)) + 
#   geom_bar(position = "dodge", stat="identity") + 
#   geom_text(aes(x=NextProg, y = Freq, ymax=Freq, label=Freq), position=position_dodge(width=1), vjust=-0.25) +
#   ylab("Number") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   facet_wrap( ~ FromProgramDescFull, ncol=4, scales="free") + ggtitle("2009-2013 DSC Selected Outbound (by Originating Program)") +
#   guides(fill=guide_legend(title="Program College"))
# outBoundPlot

# Use code and description on the x axis
outBoundPlot <-  ggplot(testArtics, aes(x=NextProgramDescFull, y=Freq, fill=Next.Program.College.Code)) + 
  geom_bar(position = "dodge", stat="identity") + 
  geom_text(aes(x=NextProgramDescFull, y = Freq, ymax=Freq, label=Freq), position=position_dodge(width=1), vjust=-0.25) +
  ylab("Number of enrolments") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Destination Program") +
  facet_wrap( ~ FromProgramDescFull, ncol=4, scales="free") + ggtitle("2009-2013 DSC Selected Outbound (by Originating Program)") +
  guides(fill=guide_legend(title="Destination Program College"))
outBoundPlot

# ggsave(filename="~/2009-2013 DSC Selected Outbound.pdf", plot=outBoundPlot, width=42.0, height=29.7, units="cm") # Regular A3 - https://www.google.com.au/webhp?sourceid=chrome-instant&ion=1&espv=2&es_th=1&ie=UTF-8#q=a3+page+size&safe=off
#### UNCOMMENT FOR PDF OUTPUT
# ggsave(filename="~/Documents/R/2009-2013 DSC Selected Outbound.pdf", plot=outBoundPlot, width=65.0, height=29.7, units="cm") # Additional width improves axis label spacing

# #### UNCOMMENT FOR PDF OUTPUT # format(Sys.Date(), "%Y%m%d")
# ggsave(filename=paste0(outputPath,Sys.Date()," 2009-2013 DSC Selected Outbound (by Originating Program).pdf"), 
#        plot=outBoundPlot, width=65.0, height=29.7, units="cm") # Additional width improves axis label spacing

outputPDF(outBoundPlot, "2009-2013 DSC Selected Outbound (by Originating Program)")

### INBOUND PROGRAMS FROM CODE OR CODES ##############################################

ArticsBP105 <- ProgramArticulationsTo("BP105")
# Create contingency table to count Program.Code
ArticsBP105Counts <- data.frame(table(as.character(ArticsBP105$Program.Code))) # as.character needed because Program.Code is factor
# Add in colname
colnames(ArticsBP105Counts) <- c("Program.Code","Freq")
# Include a column referencing the destination program
ArticsBP105Counts$NextProg<- "BP105"
# Merge back in rows from lookup table
ArticsBP105Counts <- merge(x = ArticsBP105Counts, y = nextPrograms, by.x="Program.Code", by.y="Next.Program.Code", all.x=TRUE)
# Nicer to have the 'FromProgram' column first
#C5256ArticCounts <- C5256ArticCounts[c(2, 1, 3:11)]

# Define as a function

ProgramArticulationsToDF <- function (x) {
  
  PrgArtics <- ProgramArticulationsTo(x)
  PrgArticsCounts <- data.frame(table(as.character(PrgArtics$Program.Code)))
  if(length(colnames(PrgArticsCounts)) ==2 ) {
    colnames(PrgArticsCounts) <- c("Program.Code","Freq")
    PrgArticsCounts$NextProg <- x
    PrgArticsCounts <- merge(x = PrgArticsCounts, y = programs, by.x="Program.Code", by.y="Program.Code", all.x=TRUE) } # NB Not using 'nextPrograms' in this context
  # return(PrgArticsCounts) # Return turns out to be unnecessary
}

# Give it a whirl
ArticsBP105Counts <- ProgramArticulationsToDF("BP105")

# Create a test vector
# inBoundTestCodes <- c("BP105","BP250","BP254")
inBoundTestCodes <- c("BP105") # For the intial analysis, we only want BP105

# Define an iterative function
ProgramArticulationsToMulti <- function (x) {
  
  tempDF <- data.frame()
  
  for (i in x) {
    articsDF <- ProgramArticulationsToDF(i)
    
    if (length(articsDF) > 0) { tempDF <- rbind(tempDF, articsDF) } 
  }
  
  return(tempDF)
}

# Execute the function on our test codes
testInBoundArtics <- ProgramArticulationsToMulti(inBoundTestCodes)

# Again let's chart, faceting by program code

# Get libraries if needed
if(!"ggplot2" %in% rownames(installed.packages())) install.packages("ggplot2") # install ggplot2 if not already present
library(ggplot2)
if(!"scales" %in% rownames(installed.packages())) install.packages("scales") # install ggplot2 if not already present
library(scales)

# For the purposes of plotting, merge back in the full program description for FromProgram
testInBoundArtics <- merge(x = testInBoundArtics, y = programs[, 1:2], by.x = "NextProg", by.y = "Program.Code", all.x = TRUE)
testInBoundArtics$NextProgramDescFull <- paste(testInBoundArtics$NextProg,testInBoundArtics$Program.Description.y,sep=" - ")

# Also create a full description for originating program
testInBoundArtics$ProgramDescFull <- paste(testInBoundArtics$Program.Code,testInBoundArtics$Program.Description.x,sep=" - ") 
testInBoundArtics$ProgramDescFull <- reorder(testInBoundArtics$ProgramDescFull, X=as.numeric(testInBoundArtics$Freq))

# For the purposes of plotting, reorder factors according to frequency
testInBoundArtics$ProgramDescFull <- reorder(testInBoundArtics$ProgramDescFull, X=as.numeric(testInBoundArtics$Freq))

# outBoundPlot <-  ggplot(testInBoundArtics, aes(x=Program.Code, y=Freq, fill=Program.College.Code)) + 
#   geom_bar(position = "dodge", stat="identity") + 
#   geom_text(aes(x=Program.Code, y = Freq, ymax=Freq, label=Freq), position=position_dodge(width=1), vjust=-0.25) +
#   ggtitle("2009-2013 DSC Selected Inbound") + ylab("Number") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# outBoundPlot
# Facet by program code
# outBoundPlot + facet_wrap( ~ NextProg, ncol=4, scales="free") + ggtitle("2009-2013 DSC Selected Inbound (by Destination Program)")

inBoundPlot <-  ggplot(testInBoundArtics, aes(x=ProgramDescFull, y=Freq, fill=Program.College.Code)) + 
  geom_bar(position = "dodge", stat="identity") + 
  geom_text(aes(x=ProgramDescFull, y = Freq, ymax=Freq, label=Freq), position=position_dodge(width=1), vjust=-0.25) +
  ylab("Number of enrolments") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Originating Program") +
  facet_wrap( ~ NextProgramDescFull, ncol=4, scales="free") + 
  ggtitle("2009-2013 DSC Selected Inbound to BP105 (by Originating Program)") +
  guides(fill=guide_legend(title="Originating Program College"))
inBoundPlot

outputPDF(inBoundPlot, "2009-2013 DSC Selected Inbound to BP105 (by Originating Program)")

# # Use code and description on the x axis
# outBoundPlot <-  ggplot(testArtics, aes(x=NextProgramDescFull, y=Freq, fill=Next.Program.College.Code)) + 
#   geom_bar(position = "dodge", stat="identity") + 
#   geom_text(aes(x=NextProgramDescFull, y = Freq, ymax=Freq, label=Freq), position=position_dodge(width=1), vjust=-0.25) +
#   ylab("Number") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Destination Program") +
#   facet_wrap( ~ FromProgramDescFull, ncol=4, scales="free") + ggtitle("2009-2013 DSC Selected Outbound (by Originating Program)") +
#   guides(fill=guide_legend(title="Destination Program College"))
# outBoundPlot
# 

################# YEAR-WISE ANALYSIS #######################################

# Let's create some year-wise versions of our functions

# ProgramArticulationsFromProgYear <- function(x, y) subset(dfExclude2013Starts, grepl(paste(x,".",sep=""),dfExclude2013Starts$ArticByProgram) & )
# ProgramArticulationsToProgYear <- function(x, y) subset(dfExclude2013Starts, grepl(paste(".",x,sep=""),dfExclude2013Starts$ArticByProgram))

# ArticsBP105 <- ProgramArticulationsTo("BP105")
# Create contingency table to count Program.Code
# ArticsBP105YearCounts <- data.frame(table(as.character(ArticsBP105$Program.Code),as.character(ArticsBP105$Term.Year))) # as.character needed because Program.Code is factor
# Add in colname
# colnames(ArticsBP105YearCounts) <- c("Program.Code","Term.Year","Freq")
# Include a column referencing the destination program
# ArticsBP105YearCounts$NextProg<- "BP105"
# Merge back in rows from lookup table
# ArticsBP105YearCounts <- merge(x = ArticsBP105YearCounts, y = nextPrograms, by.x="Program.Code", by.y="Next.Program.Code", all.x=TRUE)
# Nicer to have the 'FromProgram' column first
#C5256ArticCounts <- C5256ArticCounts[c(2, 1, 3:11)]

ArticsFromBP105 <- ProgramArticulationsFrom("BP105")
# Create contingency table to count Program.Code
ArticsFromBP105YearCounts <- data.frame(table(as.character(ArticsFromBP105$NextProg),as.character(ArticsFromBP105$Term.Year))) # as.character needed because Program.Code is factor
# Add in colname
colnames(ArticsFromBP105YearCounts) <- c("NextProg","Term.Year","Freq")
# Include a column referencing the originating program
ArticsFromBP105YearCounts$Program.Code <- "BP105"
# Merge back in rows from lookup table
ArticsFromBP105YearCounts <- merge(x = ArticsFromBP105YearCounts, y = nextPrograms, by.x="NextProg", by.y="Next.Program.Code", all.x=TRUE)
# Nicer to have the 'Program.Code' column first
# ArticsFromBP105YearCounts <- ArticsFromBP105YearCounts[c(4, 1:4, 5:12)]

# Define as functions

ProgramArticulationsByYearFromDF <- function (x) {
  
  PrgArtics <- ProgramArticulationsFrom(x)
  PrgArticsYearCounts <- data.frame(table(as.character(PrgArtics$NextProg),as.character(PrgArtics$Term.Year)))
  if (nrow(PrgArticsYearCounts) != 0) { # Block avoids erorr if above line produces empty dataframe
    colnames(PrgArticsYearCounts) <- c("NextProg","Term.Year","Freq")
    PrgArticsYearCounts$Program.Code <- x
    PrgArticsYearCounts <- merge(x = PrgArticsYearCounts, y = nextPrograms, by.x="NextProg", by.y="Next.Program.Code", all.x=TRUE) # NB Not using 'nextPrograms' in this context
    
    # Extra fields to assist plotting
    # Merge back in the full program description for originating (function variable) program
    PrgArticsYearCounts <- merge(x = PrgArticsYearCounts, y = nextPrograms[, 1:2], by.x = "Program.Code", by.y = "Next.Program.Code", all.x = TRUE)
    PrgArticsYearCounts$SourceProgramDescFull <- paste(PrgArticsYearCounts$Program.Code,PrgArticsYearCounts$Next.Program.Description.y,sep=" - ")
    # Also create a full description for destination
    PrgArticsYearCounts$ProgramDescFull <- paste(PrgArticsYearCounts$NextProg,PrgArticsYearCounts$Next.Program.Description.x,sep=" - ")
  }
  return(PrgArticsYearCounts)
}

ProgramArticulationsByYearToDF <- function (x) {
  
  PrgArtics <- ProgramArticulationsTo(x)
  PrgArticsYearCounts <- data.frame(table(as.character(PrgArtics$Program.Code),as.character(PrgArtics$Term.Year)))
  if (nrow(PrgArticsYearCounts) != 0) { # Block avoids erorr if above line produces empty dataframe
    colnames(PrgArticsYearCounts) <- c("Program.Code","Term.Year","Freq")
    PrgArticsYearCounts$NextProg <- x
    PrgArticsYearCounts <- merge(x = PrgArticsYearCounts, y = programs, by.x="Program.Code", by.y="Program.Code", all.x=TRUE) # NB Not using 'nextPrograms' in this context
    
    # Extra fields to assist plotting
    # Merge back in the full program description for Next (function variable) Program
    PrgArticsYearCounts <- merge(x = PrgArticsYearCounts, y = programs[, 1:2], by.x = "NextProg", by.y = "Program.Code", all.x = TRUE)
    PrgArticsYearCounts$NextProgramDescFull <- paste(PrgArticsYearCounts$NextProg,PrgArticsYearCounts$Program.Description.y,sep=" - ")
    # Also create a full description for originating program
    PrgArticsYearCounts$ProgramDescFull <- paste(PrgArticsYearCounts$Program.Code,PrgArticsYearCounts$Program.Description.x,sep=" - ")
  }
  return(PrgArticsYearCounts)
}

# Give it a whirl
ArticsBP105FromYearCounts <- ProgramArticulationsByYearFromDF("BP105")
ArticsBP105YearCounts <- ProgramArticulationsByYearToDF("BP105")

# Retrieve top programs only

TopProgramArticulationsByYearToDF <- function (x, y) { # x is program, y is number of top results
  
  topProgs <- ProgramArticulationsToDF(x)
  YearProgs <- ProgramArticulationsByYearToDF(x)
  
  if (length(topProgs) > 0) {
    topProgs <- topProgs[order(-topProgs$Freq),]
    topProgs <- topProgs[1:y,]
    
    topYearProgs <- subset(YearProgs, Program.Code %in% topProgs$Program.Code) }
  
}

TopProgramArticulationsByYearFromDF <- function (x, y) { # x is program, y is number of top results
  
  topProgs <- ProgramArticulationsFromDF(x)
  YearProgs <- ProgramArticulationsByYearFromDF(x)
  
  if (length(topProgs) > 0) {
    topProgs <- topProgs[order(-topProgs$Freq),]
    topProgs <- topProgs[1:y,] 
    
    topYearProgs <- subset(YearProgs, NextProg %in% topProgs$NextProg) 
  }
}

TopYearArticsToBP105 <- TopProgramArticulationsByYearToDF("BP105", 7)
TopYearArticsFromBP105 <- TopProgramArticulationsByYearFromDF("BP105", 7)

### Plotting

# MAKE THIS A FUNCTION????
# inBoundYearPlot <-  ggplot(ArticsBP105YearCountsSelect, aes(x=Term.Year, y=Freq, group=ProgramDescFull, colour=ProgramDescFull)) + 
#   geom_line(stat="identity") + 
#   geom_text(aes(x=Term.Year, y = Freq, ymax=Freq, label=Program.Code), position=position_dodge(width=0.6)) + 
#   ylab("Number of enrolments") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Term Year") +
#   facet_wrap( ~ NextProgramDescFull, ncol=4, scales="free") + 
#   ggtitle("2009-2013 DSC Selected Inbound to BP105 (by latest enrolment year of originating program)") +
#   guides(fill=guide_legend(title="Originating Program College"))
# inBoundYearPlot

plotArticsByYearTo <- function (x, top=0) {
    
  inBoundPlot <- ggplot(x, aes(x=Term.Year, y=Freq, group=ProgramDescFull, colour=ProgramDescFull)) + 
    geom_line(stat="identity") + 
    geom_text(aes(x=Term.Year, y = Freq, ymax=Freq, label=Program.Code), position=position_dodge(width=0.6)) + 
    ylab("Number of enrolments") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Term Year") +
    facet_wrap( ~ NextProgramDescFull, ncol=4, scales="free") + 
    ggtitle(paste0("2009-2013 Top ", top, " preceeding programs before ",
                   x$NextProg[1],
                   " (by latest enrolment year of preceeding program). Does not test for completion of preceeding program.")) +
    guides(fill=guide_legend(title="Preceeding Program")) + theme_grey()
  
  inBoundPlot
  
}

plotArticsByYearFrom <- function (x, top=0) {
  
  outBoundPlot <- ggplot(x, aes(x=Term.Year, y=Freq, group=ProgramDescFull, colour=ProgramDescFull)) + 
    geom_line(stat="identity") + 
    geom_text(aes(x=Term.Year, y = Freq, ymax=Freq, label=NextProg), position=position_dodge(width=0.6)) + 
    ylab("Number of enrolments") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Term Year") +
    facet_wrap( ~ SourceProgramDescFull, ncol=4, scales="free") + 
    ggtitle(paste0("2009-2013 Top ", top, " following programs after ",
                   x$Program.Code[1],
                   " (by earliest year of following program). Does not test for completion of ",x$Program.Code[1],".")) +
    guides(fill=guide_legend(title="Succeeding Program")) + theme_grey()
  
  outBoundPlot
  
}

# Give it a whirl
InBoundYearPlotBP105 <- plotArticsByYearTo(TopYearArticsToBP105, 7)
OutBoundYearPlotBP105 <- plotArticsByYearFrom(TopYearArticsFromBP105, 7)

InBoundYearPlotBP105
OutBoundYearPlotBP105

# #### UNCOMMENT FOR PDF OUTPUT
outputPDF(inBoundYearPlot, "2009-2013 DSC Selected Inbound to BP105 by year")

# Create a function to generate data, plot, and output as pdf

ArticulationAnalysisByYearFor <- function(x, y, programs) { # x is program, y is number of top results, programs is our programs data for error pdf ouput
  
  # tryCatch
  DFTo <- TopProgramArticulationsByYearToDF(x, y)
  
  if (length(DFTo) != 0) {
    inBoundPlot <- plotArticsByYearTo(DFTo, y)
    outputPDF(inBoundPlot, x, y, "preceeding", programs, "Normal") }
  else { outputPDF(NULL, x, y, "preceeding", programs, "Error") }
  
  DFFrom <- TopProgramArticulationsByYearFromDF(x, y)
  
  if (length(DFFrom) != 0) {
    outBoundPlot <- plotArticsByYearFrom(DFFrom, y)
    outputPDF(outBoundPlot, x, y, "following", programs, "Normal") }
  else { outputPDF(NULL, x, y, "following", programs, "Error") }
#     outputNormalPDF(outBoundPlot, x, y, "following", programs) }
#   else { outputErrorPDF(x, y, "following", programs) }
}

# Give it a whirl

ArticulationAnalysisByYearFor("BP105", 7, programs)
# ArticulationAnalysisByYearFor("FS016", 1)

# Now create an iterative version to accept a vector

ArticulationAnalysisByYearMulti <- function (x, y, programs) { # x is a vector of codes, y is the number of programs to be plotted for each code
  # programs is our programs data for error pdf ouput
  
  for (i in x) {
    ArticulationAnalysisByYearFor(i, y, programs)
  }
}

# Now create a vector of foundation studies program codes for DSC

foundationDSC <- subset(programs, grepl("FS[0-9]{3}",programs$Program.Code) & Program.College.Code == "DSC")$Program.Code

# Output FS Pdf

ArticulationAnalysisByYearMulti(foundationDSC, 7, programs)

# Now create a vector of all PCPM programs

programsPCPM <- subset(programs, grepl("325[A-Z]{1}",programs$Program.School.Code))$Program.Code

programsPCPM <- sort(as.character(programsPCPM))

system.time(ArticulationAnalysisByYearMulti(programsPCPM, 7, programs))

# Now create a vector of all DSC programs

programsDSC <- subset(programs, Program.College.Code == "DSC")$Program.Code

programsDSC <- sort(as.character(programsDSC))

system.time(ArticulationAnalysisByYearMulti(programsDSC, 7, programs))

# Now create a vector of all programs

programsRMIT <- programs$Program.Code

programsRMIT <- sort(as.character(programsRMIT))

system.time(ArticulationAnalysisByYearMulti(programsRMIT, 7, programs))

# CONTINUE: Figure out how to catch errors for eg FS016?????????????????????????

# This is nice, but really we want something that will give us a single pdf per program
# http://stackoverflow.com/questions/18068914/combine-multiple-pdf-plots-into-one-file
# if(!"sweave" %in% rownames(installed.packages())) install.packages("ggplot2") # install ggplot2 if not already present
# library(sweave)
# if(!"knitr" %in% rownames(installed.packages())) install.packages("ggplot2") # install ggplot2 if not already present
# library(knitr)

# For the purposes of plotting, merge back in the full program description for FromProgram
# ArticsBP105FromYearCounts <- merge(x = ArticsBP105FromYearCounts, y = nextPrograms[, 1:2], by.x = "Program.Code", by.y = "Next.Program.Code", all.x = TRUE)
# ArticsBP105YearCounts$NextProgramDescFull <- paste(ArticsBP105YearCounts$NextProg,ArticsBP105YearCounts$Program.Description.y,sep=" - ")

# Also create a full description for originating program
# ArticsBP105YearCounts$ProgramDescFull <- paste(ArticsBP105YearCounts$Program.Code,ArticsBP105YearCounts$Program.Description.x,sep=" - ") 
# testInBoundArtics$ProgramDescFull <- reorder(testInBoundArtics$ProgramDescFull, X=as.numeric(testInBoundArtics$Freq))

# For the purposes of plotting, merge back in the full program description for FromProgram
# ArticsBP105YearCounts <- merge(x = ArticsBP105YearCounts, y = programs[, 1:2], by.x = "NextProg", by.y = "Program.Code", all.x = TRUE)
# ArticsBP105YearCounts$NextProgramDescFull <- paste(ArticsBP105YearCounts$NextProg,ArticsBP105YearCounts$Program.Description.y,sep=" - ")

# Also create a full description for originating program
# ArticsBP105YearCounts$ProgramDescFull <- paste(ArticsBP105YearCounts$Program.Code,ArticsBP105YearCounts$Program.Description.x,sep=" - ") 
# testInBoundArtics$ProgramDescFull <- reorder(testInBoundArtics$ProgramDescFull, X=as.numeric(testInBoundArtics$Freq))

# For the purposes of plotting, create a subset with key programs of interest
# What we really want here is to take the sum of frequency by program code for the whole table, then subset to the top 7ish programs by overall Freq
# To do this we can just take the highest ranking progs from ArticsBP105Counts eg.

# MAKE THIS A FUNCTION TOO
# ArticsBP105Counts <- ProgramArticulationsToDF("BP105")
# ArticsBP105Counts <- ArticsBP105Counts[order(-ArticsBP105Counts$Freq),]
# ArticsBP105CountsSelect <- ArticsBP105Counts[1:7,] # Take top 7 to prevent chart getting too cluttered
# 
# ArticsBP105YearCountsSelect <- subset(ArticsBP105YearCounts, Program.Code %in% ArticsBP105CountsSelect$Program.Code)


# Niall Ridge
# Potentially interesting extension of the pathways stuff... link it up to the R059 program attrition stuff 
# get some measure of whether/when they drop out. I believe they're based on the same universe, so should be feasible.

### CONSIDER BRINGING IN R059

#############################################################
############ ANDREW NEW CODE END ############################
#############################################################

# ===================== Create a summary table of the number of occurences of each articulation pathway (including non-articulation) ======================== 

# Articulation Level Analysis ---------------------------------------------


# create a table to count the number of instances of each articulation pathway (i.e. articulation from Program.Code to NextProg)
ArticCountDF  <- data.frame(table(paste(df$Program.Code, df$NextProg, sep=":")))

# split this key out to give the individual programs articulated from and to 
ArticCountDF[,c("Program.Code","NextProg")] <-data.frame(do.call(rbind, strsplit(as.vector(ArticCountDF$Var1), split = ":")))

#merge additional program information from reference table back against the counts
ArticCountDF <- merge(x = ArticCountDF, y = programs, by="Program.Code", all=TRUE)
View(ArticCountDF)

# ===================== As above, but compare terms of first enrolment vs articulation to work out how long between steps ======================== 
# TODO: this could be expanded to pull in all relevant reference data for a term, like in section above. e.g. term description, year, career level etc
#Only look at articulations
ArticTermCountDF <- df[df$IsArtic,]

# Check for records where next program is earlier than current one - should be zero if our code worked properly
#nrow(ArticTermCountDF[ArticTermCountDF$Term.Code > ArticTermCountDF$Next.Term.Code,])

# Compare full term code
ArticTermCountDF <- data.frame(table(paste(ArticTermCountDF$Term.Code, ArticTermCountDF$Next.Term.Code, sep=":")))

# Compare year only
ArticTermCountDF <- df[df$IsArtic,] # Need to refresh our source
ArticTermCountDF <- data.frame(table(paste(substr(ArticTermCountDF$Term.Code,1,2), substr(ArticTermCountDF$Next.Term.Code,1,2), sep=":")))
View(ArticTermCountDF)

getwd() # Display working directory to retrieve file location

