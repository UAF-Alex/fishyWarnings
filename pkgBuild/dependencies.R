
update_dependencies <- function(){
	devtools::use_package("RcppRoll", type="Imports")
	devtools::use_package("zoo", type="Imports")
	devtools::use_package("stats", type="Imports")
	devtools::use_package("graphics", type="Suggests")
}



