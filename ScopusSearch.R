#############################################################
#####             Age of Living estimation              #####
#####                  Scopus Analysis                  #####
#############################################################
#####                     Keywords                      #####
#############################################################

#Clear all lists from memory to avoid unintentional errors

rm(list=ls())

#############################################################
#####                 File requirement                  #####
#############################################################
# The files to be imported is generated from Scopus.
# The columns will need to contain:
#   Year; Title; Source.title; Authors; AuthorID; Author.Keywords; Index.Keywords; EID

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

#############################################################
#####                      Function                     #####
#############################################################

#### Function to search and replace ####

# Include function to duplicate {Marc Schwartz (via MN) on http://r.789695.n4.nabble.com/replace-values-in-data-frame-td803416.html}
gsr <- function(Source, Search, Replace) 
{ 
  if (length(Search) != length(Replace)) 
    stop("Search and Replace Must Have Equal Number of Items\n") 
  
  Changed <- as.character(Source) 
  
  for (i in 1:length(Search)) 
  { 
    cat("Replacing: ", Search[i], " With: ", Replace[i], "\n") 
    Changed <- replace(Changed, Changed == Search[i], Replace[i]) 
  } 
  
  cat("\n") 
  
  Changed 
}


# function to replace accented characters with unaccented equivalents 
# adapted from https://stackoverflow.com/questions/15253954/replace-multiple-letters-with-accents-with-gsub
removeDiacritics <- function(string) {
  chartr(
    "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöøùúûüýÿ",
    "SZszYAAAAAACEEEEIIIIDNOOOOOOUUUUYaaaaaaceeeeiiiidnoooooouuuuyy", 
    string
  )
}




#############################################################
#####                    Data loading                   #####
#############################################################

# set working directory

# read the export *.csv document from Scopus, separation ",", and place it in data.frame "ScopusOriginal"
ScopusOriginalData <- read.csv("FACompile2024.csv", sep=",", header=TRUE)

# rename some of the columns to remove special characters or encoding
names(ScopusOriginalData)[1:3] <- c("Authors","Author.full.names", "AuthorID")

# Select column label $Year, $Title,  $Source.title, $Author.Keywords, $Index.Keyword
ScopusReducedDataSet <- ScopusOriginalData %>%
  select(Year,Title,Source.title,Authors,AuthorID,Author.Keywords,Index.Keywords)

#############################################################
#####                     Keywords                      #####
#############################################################

# This section is looks at Keywords

#############################################################
#####     Select one of the following three options     #####
#############################################################

#   Author Keywords only
# names(ScopusReducedDataSet) <- sub("Author.Keywords","AIKeywords", names(ScopusReducedDataSet))

#   Index Keywords only
# names(ScopusReducedDataSet) <- sub("Index.Keywords","AIKeywords", names(ScopusReducedDataSet))

#   Index and Author Keywords
# Combine Columns Author.Keywords and Index.Keywords and place in Column name "AIKeywords" and remove original columns
 ScopusReducedDataSet <- ScopusReducedDataSet %>%
  unite("AIKeywords", Author.Keywords, Index.Keywords,sep = ";", remove = TRUE)

#############################################################

ScopusReducedDataSet$AIKeywords <- removeDiacritics(as.character(ScopusReducedDataSet$AIKeywords))


#Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
ScopusKeywordList <- ScopusReducedDataSet %>% 
  mutate(AIKeywords = strsplit(as.character(AIKeywords), ";")) %>% 
  unnest(AIKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AIKeywords" in "ScopusKeywordList" and save in dataframe
# Extract list of "AIkeywords" and remove duplicate
ScopusKeywordList$AIKeywords <- toupper(ScopusKeywordList$AIKeywords)
KeywordList <- ScopusKeywordList %>%
  select(AIKeywords)
Keyword <- KeywordList %>%
  distinct()

#############################################################
#####                  Data cleansing                   #####
#############################################################

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.

#read the corrected list of keywords and combine it to the original list
KeywordsCorrected <- read.csv("KeywordsCorrectionFull.txt", sep="\t", header=TRUE)
KeywordsCorrected <- as.data.frame(KeywordsCorrected)
ScopusKeywordList$KeywordsCorrected <- gsr(as.character(ScopusKeywordList$AIKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorrectedAIKeywords))

# if no corrections are required then run
# ScopusKeywordList$KeywordsCorrected <- ScopusKeywordList$AIKeywords

#############################################################
#####               Data analysis - Keywords            #####
#############################################################

#Count to number of time the same year is repeated in the "ScopusKeywordList$Year" and save in a data.frame "Year" 
PublicationYear<- data.frame(table(ScopusReducedDataSet$Year));PublicationYear
names(PublicationYear) <- c("Year","Publications")

#count the number of keywords per title paper 
ScopusKeywordListTemp1 <- ScopusKeywordList  %>%
  select(Year,Title,Source.title,KeywordsCorrected) %>%
  distinct()
ScopusKeywordListTemp2 <-ScopusKeywordListTemp1[complete.cases(ScopusKeywordListTemp1), ]
sum(is.na(ScopusKeywordListTemp2$KeywordsCorrected))

ScopusKeywordYearCount <- aggregate(ScopusKeywordListTemp2$Year, by=list(Year=ScopusKeywordListTemp2$Year, Rtitle=ScopusKeywordListTemp2$KeywordsCorrected), FUN=length)
ScopusKeywordTotalCount <- aggregate(ScopusKeywordListTemp2$Year, by=list(Rtitle=ScopusKeywordListTemp2$KeywordsCorrected), FUN=length)
# ScopusKeywordListTemp5 <- aggregate(ScopusKeywordListTemp2, by=list(ScopusKeywordListTemp2$Year), FUN=length)

# narrowing range for plot
ScopusKeywordNarrowRangeGraph <- subset(ScopusKeywordTotalCount,x>4)

# write for keyword correction list
# write.csv(ScopusKeywordNarrowRangeGraph, file = "ScopusKeywordNarrowRangeGraph.csv", row.names = FALSE)

SubsetKeywordNarrowRangeGraph <-subset(ScopusKeywordYearCount,Rtitle %in% ScopusKeywordNarrowRangeGraph$Rtitle)
#Reduced <- subset(Condensed, SummaryKeywords$weight>0.007)
SubsetKeywordNarrowRangeGraph$x <- as.numeric(SubsetKeywordNarrowRangeGraph$x)

#############################################################
#####                      GRAPH                        #####
#############################################################

# Create a new variable from incidence
SubsetKeywordNarrowRangeGraph$Incidenceweight <- cut(SubsetKeywordNarrowRangeGraph$x,
                                                     breaks = c(-1,0,1,2,5,10,20,30,max(SubsetKeywordNarrowRangeGraph$x,na.rm=T)),
                                                     labels=c("0","1","2","3-5","6-10","11-20","21-30",">30"))

GraphTemp1 <- SubsetKeywordNarrowRangeGraph %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Rtitle,levels=rev(sort(unique(Rtitle))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(-1,0,1,2,5,10,20,30,max(x,na.rm=T)),
                         labels=c("0","1","2","3-5","6-10","11-20","21-30",">30")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# ScopusKeywordList$WYear <- gsr(ScopusKeywordList$Year,year$Var1,1/year$Freq)
GraphTemp2 <- aggregate(GraphTemp1[, 1], list(GraphTemp1$KeywordsCorrected), min)

GraphTemp1$graphorder <- as.numeric(gsr(GraphTemp1$KeywordsCorrected,GraphTemp2$Group.1,GraphTemp2$x))

# assign text colour
textcol <- "black"

# further modified ggplot
p <- ggplot(GraphTemp1,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  #  labs(x="",y="",title="Keywords found in gunshot residue publication")+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(1965,1975,1985,1995,2005,2015))+
  scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#d5ee52","#77c86c","#66afc6","#ddf1da"),na.value = "grey90")+
  #coord_fixed()+
  theme_grey(base_size=8)+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=8,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))

show(p)

ggsave("KeywordTrend.png", p, width = 6, height = 8, units = "in", dpi=150)

#############################################################
#####                     Authors                       #####
#############################################################

# This section is looks at Authors to generate a table:
#                                   the total number of authors and publications per year,
#                                   the number of publications per author,
#                                   the first year an author published,
#                                   the number of new author  

#####______________Data analysis - Authors______________#####

# Split Column "Authors" in row by first replacing "," with the separator ";" and place it in AuthorListExtended
AuthorList <- ScopusOriginalData %>%
  select(Year,Title,Authors,EID)

AuthorList$Authors <- removeDiacritics(as.character(AuthorList$Authors))

AuthorList$Authors <- gsub(", Jr"," Jr",AuthorList$Authors)
AuthorList$Authors <- gsub(",",";",AuthorList$Authors)

#Split Column "Authors" in row by the separator ";", remove leading white space to generate list
AuthorListExtended <- AuthorList %>% 
  mutate(Authors = strsplit(as.character(Authors), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)

#read the corrected list of "Authors" and combine it to the original list
# AuthorCorrected <- read.csv("AuthorCorrection.txt", sep="\t", header=TRUE)
# AuthorListExtended$AuthorsCor <- gsr(AuthorListExtended$Authors,AuthorCorrected$OriginalAuthor,as.character(AuthorCorrected$CorrectedAuthor))

AuthorListExtended$AuthorsCor <- AuthorListExtended$Authors


AuthorCountPaper <- aggregate(AuthorListExtended$AuthorsCor, list(AuthorListExtended$AuthorsCor), FUN=length)
names(AuthorCountPaper) <- c("Author","Frequency")

# Number of Authors per year
NumberAuthorYear <- aggregate(AuthorListExtended$AuthorsCor,list(AuthorListExtended$Year), FUN=length)
names(NumberAuthorYear) <- c("Year","Author")

# List of "Author" with one publication only
AuthorCountSinglePaper <- subset(AuthorCountPaper,Frequency<2)
AuthorCountSinglePaperReduced <-subset(AuthorListExtended,AuthorsCor %in% AuthorCountSinglePaper$Author)

# List of "Author" with one publication only and their publication "Year"
YearNewAuthorSinglePaper <- aggregate(AuthorCountSinglePaperReduced$AuthorsCor,list(AuthorCountSinglePaperReduced$Year), FUN=length)
names(YearNewAuthorSinglePaper) <- c("Year","Single Author")

# List of Authors with multiple publications only
AuthorCountMultiplePaper <- subset(AuthorCountPaper,Frequency>1)
AuthorCountMultiplePaperReduced <- subset(AuthorListExtended,AuthorsCor %in% AuthorCountMultiplePaper$Author)

# List of "Authors" with multiple output and their first "Year"
AuthorFirstAppearanceMultipleEntry<- aggregate(AuthorCountMultiplePaperReduced$Year, list(AuthorCountMultiplePaperReduced$AuthorsCor), min)
names(AuthorFirstAppearanceMultipleEntry) <- c("Author","Year")

# List of "Authors" with multiple output and their last "Year"
 AuthorMultipleOutputLastYear<- aggregate(AuthorCountMultiplePaperReduced$Year, list(AuthorCountMultiplePaperReduced$AuthorsCor), max)
 names(AuthorMultipleOutputLastYear) <- c("Author","Year")

#####__________________Gephi Plots - Authors_________________#####

 #generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
ListAuthor <- AuthorCountMultiplePaperReduced %>% group_by(Year,Title) %>%
  summarise(AuthorCorrected = paste(AuthorsCor, collapse = ";"))
ListAuthor <- as.data.frame(ListAuthor)

GephiAuthor <- ListAuthor %>%
  select(AuthorCorrected)
names(GephiAuthor) <- c("Author")

#Export to Gephi plot - Year;Title;Authors
write.table(GephiAuthor, file = "GephiAuthor.csv", quote = F, sep = "\t", row.names = F)

#Export to Gephi plot - Authors;Year
write.table(AuthorMultipleOutputLastYear, file = "GephiListAuthorLastYear.csv", quote = F, sep = "\t", row.names = F)

