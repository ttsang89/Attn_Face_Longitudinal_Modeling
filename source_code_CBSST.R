.required_packages <- c('rstanarm', 'data.table', 'ggplot2', 'readxl', 'lme4')

# install source packages
for (pkg in .required_packages) {
  if (!requireNamespace(pkg)) {
    install.packages(pkg, dependencies=TRUE)
  }
  library(pkg, character.only = TRUE)
}

.custom_pkgs <- c('mejr')

for (pkg in .custom_pkgs) {
  if (requireNamespace(pkg)) {
    library(pkg, character.only = TRUE)
  }
}

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


CBSST_salience <- read_excel("~/Desktop/Projects/ETdataFiles/IMFAR2018/CBSST/07-27-17_CB_salience.xlsx", sheet = "SN_CBSST")
cbsst <- as.data.table(CBSST_salience)
cbsst <- cbsst[, .(sib, ave_face=`average_face`, pface = `Percent face`, psal = `Psal`, face_sal = `Face over Sal`, sal_face = `Sal over Face`, SN_LRvsHR_OFC, SN_HRvsLR_SMA, sex, age_centered, age_sq  = age_centered*age_centered, RX, StimType, pface_2=Percent_dyad, Mean_Fix_MS, sub_id = as.factor(SubjectID))]

cbsst_nosibs <- cbsst[cbsst$sib==0, ]
CB_dat <- cbsst[cbsst$StimType==0, ]
SST_dat <- cbsst[cbsst$StimType==1, ]

table_show <- function(dt) {
  print(as.data.frame(dt))
}


loo_ci <- function(loo_obj, prob = 0.95) {
  l_vals <- as.vector(loo_obj)
  x <- l_vals[1]
  s <- l_vals[2]
  se <- abs(qnorm((1-prob) / 2)) * s
  ci <- c(x - se, x + se)
  p_l <- (1-prob) / 2
  p_r <- prob + p_l
  names(ci) <- c(sprintf('%.1f%%', 100*p_l), sprintf('%.1f%%', 100*p_r))
  return(ci)
}
