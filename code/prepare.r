#====================================================================
# load packages and defind functions
#====================================================================
libs = c('dplyr', 'openxlsx', 'stringr', 'pracma', 'xts', 'ggplot2', 'ggsci', 'ggpubr', 'scales')
lapply(libs, require, character.only = TRUE)
options(stringsAsFactors=F)

# study period
days = seq(as.Date('2022/11/01'), by = "day", length.out = 61)
adjust_day = as.Date('2022-12-15')

# get items with matched pattern
# e.g, get('a', c('a1', 'a', 'c'))
get = function(key, vector, exact=F, v=F){
    if (exact==T){out =vector[grep(key, vector, invert=v)]}
    else {out = vector[grep(toupper(key), toupper(vector), invert=v)]}
    return(out)
}

# convert character vector to numeric 
# e.g, df = df%>%mutate_if(is_numeric,as.numeric)
is_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

# find col with all na
not_all_na = function(x) any(!is.na(x))