#####################################
###StaMPS-Visualizer global script###
#####################################

####################
###Visualizer Map###
####################

# Loading and preparing PS data from StaMPS export
# library package
library(lubridate)

# looking for study site data in stusi folder
dat.path <- list.files("input/stusi/")

# prepare objects to read data
ps.loc <- list()
dates.days <- list()
dates.date <- list()
ref.points <- list()

# read data into R
for(i in 1:length(dat.path)){
  dat <- read.csv(paste("input/stusi/", dat.path[i], sep = ""))
  dates.days[[i]] <- as.vector(t(dat[1, 4:ncol(dat)]))
  dates.date[[i]] <- as_date(dates.days[[i]], origin = "0000-01-01")
  ref.points[[i]] <- as.vector(t(dat[1, 1:2]))
  dat <- dat[c(2:nrow(dat)), ]
  rownames(dat) <- seq(length = nrow(dat))
  ps.loc[[i]] <- cbind(uid = 1:nrow(dat), lon = dat[, 1],
                              lat = dat[, 2], disp = dat[, 3],
                              dat[, 4:ncol(dat)])
}

# prepare study site name
str.rev <- function(x){sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")}
stusi <- str.rev(dat.path)
stusi <- substr(stusi, 5, 50)
stusi <- str.rev(stusi)

# tidy up
rm(dat)

##############################

#################
##Baseline Plot##
#################

## in Linux use this to prepare the SNAP export
# sed 's/?/0/g' stack_all_baselines.csv | sed 's/ //g'

# looking for baseline info csv files
dat.path <- list.files("input/baseline_info/", pattern = ".csv")

str.rev <- function(x){sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")}
bl.info <- str.rev(dat.path)
bl.info <- substr(bl.info, 5, 100)
bl.info <- str.rev(bl.info)
