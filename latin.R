if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
library(plyr)
library(ggplot2)

if (!require("pacman")) install.packages("pacman") # for package manangement
pacman::p_load("textcat")
pacman::p_load("cld2")
pacman::p_load("cld3")

workingDir = '/Users/michaeltauberg/billboard/'
setwd(workingDir)

csvName = "latin.csv"
data_name = "latin"

latin = read.csv(csvName)
latin = latin[order(latin$weeks, decreasing=TRUE),]

latin = latin[!duplicated(latin[,c('title','artist')], fromLast=FALSE),]

## -------   Songs per year  -------
years = c()
for (year in levels(droplevels(factor(latin$year)))) {
  row = c(year, nrow(latin[latin$year == year,]))
  years = rbind(years, row)
}
years = data.frame(years)
years$year = as.numeric(as.character(years$year))
years$num_songs = as.numeric(as.character(years$num_songs))
colnames(years) = c("year", "num_songs")

latin = latin[order(as.Date(latin$date, format="%m/%d/%y"), decreasing=TRUE),]
write.csv(latin, "latin_all_uniq.csv", row.names = FALSE)


## -------   Spanish songs per year  -------
years = years[years$year != "2018", ]
years$num_songs = as.numeric(as.character(years$num_songs))

p = ggplot(years, aes(x=year, y=num_songs, group=1)) 
#p = p + geom_bar(stat="identity") 
p = p + geom_point() 
#p = p + geom_smooth(method = "lm", se = FALSE)
p = p + geom_line()
#p = p + stat_smooth(aes(y=num_songs),method = "lm", formula = y ~ x + I(x^2), size = 1, color = "green")
p = p + ggtitle("Songs by Hispanic Musians on the Billboard Hot-100 per year")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Year") + ylab("Num Songs on Hot-100") 
p = p + scale_y_continuous(limits = c(30, 65)) 

ggsave(filename = sprintf("./%s_songs_per_year.png", data_name) , plot=p, width=8, height=6)


latin$lyrics = as.character(latin$lyrics)
cld2 = cld2::detect_language(text = latin$lyrics, plain_text = FALSE)
cld2[cld2 == "en"] = 0
cld2[cld2 == "es"] = 1
cld2[is.na(cld2)] = 0
latin$language = as.numeric(cld2)

years = c()
for (year in levels(droplevels(factor(latin$year)))) {
  print(year)
  y = latin[latin$year == year,]
  row = c(year, sum(y$language,na.rm = TRUE))
  years = rbind(years, row)
}
years = data.frame(years)
years$year = as.numeric(as.character(years$year))
years$num_songs = as.numeric(as.character(years$num_songs))
colnames(years) = c("year", "num_songs")

years = years[years$year != "2018", ]
p = ggplot(years, aes(x=year, y=num_songs)) + geom_point() 
#p = p + geom_smooth(method='lm', se = FALSE)
p = p + geom_line()
#p = p + stat_smooth(aes(y=num_songs),method = "lm", formula = y ~ x + I(x^2), size = 1, color = "green")
p = p + scale_x_discrete(limits = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))
p = p + ggtitle("Spanish Language Songs on the Billboard Hot-100 per year")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Year") + ylab("Num Songs on Hot-100") 
ggsave(filename = sprintf("./%s_language_songs_per_year.png", data_name) , plot=p, width=8, height=6)


## -------   Weeks  per year  -------
weeks = ddply(latin,"year",numcolwise(sum))

weeks = weeks[weeks$year != "2018", ]
p = ggplot(weeks, aes(x=year, y=weeks)) 
#p = + geom_bar(stat="identity") 
p = p + geom_point() 
#p = p + geom_smooth(method='lm', se = FALSE)
#asin2x+bsin22x+csin24x−2a/π
p = p + stat_smooth(aes(y=weeks),method = "lm", formula = y ~ x + I(x^2), size = 1, color = "green")
ggsave(filename = sprintf("./%s_songs_weeks_per_year.png", data_name) , plot=p, width=8, height=6)


## -------   Artists    -------
artists = ddply(latin,"main_artist",numcolwise(sum))
artists = artists[order(artists$weeks, decreasing=TRUE),]
artists$main_artist = factor(artists$main_artist, levels = artists$main_artist[order(artists$weeks, decreasing=TRUE)])
artists = artists[1:30,]
p = ggplot(artists, aes(x=main_artist, y=weeks)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Latin Artists")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Artists") + ylab("Weeks on Hot-100") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)


## -------   Regaeton    -------
reggaeton = latin[grep("reggaeton",latin$genre),]
years = c()
for (year in levels(droplevels(factor(reggaeton$year)))) {
  row = c(year, nrow(reggaeton[reggaeton$year == year,]))
  years = rbind(years, row)
}
years = data.frame(years)
years$year = as.numeric(as.character(years$year))
years$num_songs = as.numeric(as.character(years$num_songs))
colnames(years) = c("year", "num_songs")

data_name = "reggaeton"
years = years[years$year != "2018", ]
p = ggplot(years, aes(x=year, y=num_songs, group=1)) 
#p = p + geom_bar(stat="identity") 
p = p + geom_point() 
p = p + geom_line()
#p = p + geom_smooth(method='lm', se = FALSE)
p = p + scale_x_discrete(limits = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017))
p = p + ggtitle("Reggaeton Songs on the Billboard Hot-100 per year")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Year") + ylab("Num Songs on Hot-100") 
#p = p + stat_smooth(aes(y=num_songs),method = "lm", formula = y ~ x + I(x^2), size = 1, color = "red")
ggsave(filename = sprintf("./%s_reggaeton_per_year.png", data_name) , plot=p, width=8, height=6)
