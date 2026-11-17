#Installs non-CRAN DEQ packages

devtools::install_github('TravisPritchardODEQ/odeqIRextdata')
devtools::install_github('OR-Dept-Environmental-Quality/odeqtmdl')
devtools::install_github('TravisPritchardODEQ/odeqIRtools', force = TRUE)
devtools::install_github('TravisPritchardODEQ/deqalcrit')
3
View(odeqIRtools::AU_info)