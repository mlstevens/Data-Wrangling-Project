#Zack Lasek & Max Stevens
#5.1.2023
#Project Code

rm(list=ls())

library(xml2)
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"

#initialize variables to start
years <- character(0)
schools <- character(0)
soses <- character(0)
srses <- character(0)
tovs <- character(0)
tpts <- character(0)
opp_pts <- character(0)

#Load in data through Web Scrapping
for (y in 13:19) {
  url <- paste("https://www.sports-reference.com/cbb/seasons/men/20",y,"-school-stats.html",sep="")
  page <- read_html(url, user_agent)
  Sys.sleep(5)
  year <- paste0("20", y)
  n_rows <- length(xml_find_all(page, "//td[@data-stat='school_name']/a/text()"))
  years <- c(years, rep(year, n_rows))
  school <- xml_text(xml_find_all(page, "//td[@data-stat='school_name']/a/text()"))
  schools <-c(schools, school)
  sos <- xml_text(xml_find_all(page, "//td[@data-stat='sos']/text()"))
  soses <- c(soses, sos)
  srs <- xml_text(xml_find_all(page, "//td[@data-stat='srs']/text()"))
  srses <- c(srses, srs)
  tov <- xml_text(xml_find_all(page, "//td[@data-stat='tov']/text()"))
  tovs <- c(tovs, tov)
  tpt <- xml_text(xml_find_all(page, "//td[@data-stat='pts']/text()"))
  tpts <- c(tpts, tpt)
  opp_pt <- xml_text(xml_find_all(page, "//td[@data-stat='opp_pts']/text()"))
  opp_pts <- c(opp_pts, opp_pt)
  rv <- sample(2:20, 1)
  Sys.sleep(rv)
}

# Create data frame from Web scrapping
cbb <- data.frame(schools, soses, srses, tovs, tpts, opp_pts, years, stringsAsFactors = FALSE)
colnames(cbb) <- c("School", "SOS", "SRS", "TOV", "PTS", "OPP_PTS", "Year")

# Load in Kaggle data and make column names match that are merging on
cbb2 <- read.csv("cbb.csv", na.strings = c(""," ", "NA"))
names(cbb2)[names(cbb2) == "TEAM"] <- "School"
names(cbb2)[names(cbb2) == "YEAR"] <- "Year"

#Clean the data
str(cbb)
cbb$SOS <- as.numeric(cbb$SOS)
cbb$SRS <- as.numeric(cbb$SRS)
cbb$TOV <- as.numeric(cbb$TOV)
cbb$PTS <- as.numeric(cbb$PTS)
cbb$OPP_PTS <- as.numeric(cbb$OPP_PTS)
cbb$Year <- as.integer(cbb$Year)
str(cbb)

str(cbb2)

#Merge the two data sets
cbb3 <- merge(cbb2,cbb, by=c("School","Year"), all = TRUE)

#Save merged and cleanred data to a file that can be loaded for the analysis R script
write.csv(cbb3, "cbb3.csv", row.names=FALSE)
