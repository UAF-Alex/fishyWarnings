
update_dependencies <- function(){
	devtools::use_package("RcppRoll", type="Suggests")
	devtools::use_package("zoo", type="Suggests")
	devtools::use_package("stats", type="Imports")
}



