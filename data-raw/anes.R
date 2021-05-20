library(foreign)
library(mice)

# Download data from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/24672

anes <- foreign::read.dta("./rawdata/base2008_3.dta")
anes[["jobs_r"]] <- unclass(anes[["jobs_r"]])

##### CODE FROM Kropko et. al 
# BEGIN >>> 
anes <- anes[ ,c(6:8, 11:13, 25, 42, 44:46)]

levels(anes$imp_enviro) <- c( "Not important" , "Not important", "Important", "Important" )

levels(anes$religion) <- c("Protestant", "Catholic/Orthodox", "Atheist/Other", "Atheist/Other")
anes <- anes[!as.logical(rowSums(is.na(anes[,c(2,3,9,11)]))),] 

anes[["education"]] <- factor(anes[["education"]], ordered=TRUE)
# anes[["jobs_r"]] <- factor(anes[["jobs_r"]], ordered=TRUE)
anes[["income"]] <- factor(anes[["income"]], ordered=TRUE)
# <<< END 

anes[['time']] <- log(anes[['time']])

vars <- c("age", "female", 
	"white", "education", "income", "religion", 
	"married", "jobs_r", "imp_enviro", "vote", "time")

anes08 <- anes[, vars]


# Impute 
#########

anes08_rng <- mice(anes08, method='sample', seed=42)
anes08_pmm <- mice(anes08, method='pmm', seed=42)


usethis::use_data(anes08, overwrite = TRUE)
usethis::use_data(anes08_rng, overwrite = TRUE)
usethis::use_data(anes08_pmm, overwrite = TRUE)
