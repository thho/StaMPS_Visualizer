#####################################
###StaMPS-Visualizer global script###
#####################################

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