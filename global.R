#####################################
###StaMPS-Visualizer global script###
#####################################

####################
###Visualizer Map###
####################

# looking for study site data in stusi folder
dat.path <- list.files("input/stusi/")
# prepare study site name
str.rev <- function(x){sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")}
stusi <- str.rev(dat.path)
stusi <- substr(stusi, 5, 50)
stusi <- str.rev(stusi)
stusi <- c("---", stusi)

#################
##Baseline Plot##
#################

## in Linux use this to prepare the SNAP export
# sed 's/?/0/g' stack_all_baselines.csv | sed 's/ //g'

# looking for baseline info csv files
dat.path <- list.files("input/baseline_info/", pattern = ".csv")

bl.info <- str.rev(dat.path)
bl.info <- substr(bl.info, 5, 100)
bl.info <- str.rev(bl.info)
