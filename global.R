#####################################
###StaMPS-Visualizer global script###
#####################################

#library package
library(lubridate)

#looking for study site data in stusi folder
dirs <- list.dirs("./stusi")

#prepare objects to read data
ps.loc <- list()
dates.days <- list()
dates.date <- list()
ref.points <- list()

#read data into R
for(i in 1:(length(dirs)-1)){
  dat.path <- list.files(dirs[i+1], pattern = ".csv")
  dat <- read.csv(paste(dirs[i+1], dat.path, sep = "/"))
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
stusi <- substr(dirs[2:length(dirs)], 9, 50)
