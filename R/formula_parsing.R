# 
# 
# Given a formula with a "|" in it, splits it into two formulas
# 

.split_groups <- function(form) {
  
  # Get string representation
  form_str <- as.character(form[[2]])
  
  # Retrieve symbols
  vars   <- as.formula(paste0('~',form_str[2]))
  groups <- as.formula(paste0('~',form_str[3]))
  return(list(groups=groups, vars=vars))
}

# Function that tests if there are groups passed (finds a "|" in the formula
# definition
.has_groups <- function(form) {
  form_str <- paste(as.character(form),collapse='')
  return( grepl('\\|',form_str) ) 
}
