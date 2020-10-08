#####################################
###StaMPS-Visualizer global script###
#####################################

###Loading and preparing PS data from StaMPS export
#library package
library(lubridate)

#looking for study site data in stusi folder
dat.path <- list.files("./stusi/")

#prepare objects to read data
ps.loc <- list()
dates.days <- list()
dates.date <- list()
ref.points <- list()

#read data into R
for(i in 1:length(dat.path)){
  dat <- read.csv(paste("./stusi/", dat.path[i], sep = ""))
  dates.days[[i]] <- as.vector(t(dat[1, 4:ncol(dat)]))
  dates.date[[i]] <- as_date(dates.days[[i]], origin = "0000-01-01")
  ref.points[[i]] <- as.vector(t(dat[1, 1:2]))
  dat <- dat[c(2:nrow(dat)), ]
  rownames(dat) <- seq(length = nrow(dat))
  ps.loc[[i]] <- cbind(uid = 1:nrow(dat), lon = dat[, 1],
                              lat = dat[, 2], disp = dat[, 3],
                              dat[, 4:ncol(dat)])
}

#prepare study site name
str.rev <- function(x){sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")}
stusi <- str.rev(dat.path)
stusi <- substr(stusi, 5, 50)
stusi <- str.rev(stusi)

#tidy up
rm(dat)

###Loading and preparing custom event marker for showing 
###information as points with pop-up-dialog on map

#looking for event marker data in event_marker folder
event.path <- list.files("./event_marker/")

#prepare objects to read data
event.loc <- list()
event.info <- list()

#read data into R
for(i in 1:length(event.path)){
  dat <- read.csv(paste("./event_marker/", event.path[i], sep = ""), stringsAsFactors = FALSE)
  event.info[[i]] <- dat[ , 3]
  event.loc[[i]] <- data.frame(lon = dat[, 1], lat = dat[, 2])
}

#prepare study site name
event <- str.rev(event.path)
event <- substr(event, 5, 50)
event <- str.rev(event)
event <- c("---", event)

#tidy up
rm(dat)

#load event marker icon
event.icon <- makeIcon("./icons/event_marker_pin.png",
                       iconWidth = 15,
                       iconHeight = 25)
