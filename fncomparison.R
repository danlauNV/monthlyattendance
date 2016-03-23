
fn.comp <- function(comparisonstring){
  # use eg . fn.comp("== year_max")(yearvariable)
  # comparisonstring must have one whitespace. second elem is a variable
  # returns function
  function(x, ...) {
    elem <- unlist(strsplit( comparisonstring, " "))
    comparand <- try(get(elem[2]))
    if (class(comparand) == "try-error") {
      cat("Error in fn.comp is ok if last word of comparisonstring is not an object, rather atomic/literal")
      comparand <- elem[2]
    }
    comparison <- match.fun(elem[1])
    comparison(x, comparand)
  }
  # eval(parse()) may be an alternative to this function fn.comp
}




fn.indexsubset.df.var <- function(df, str_rootword, var_to_eval, comparisonstring){
  # returns vec of True False,length of df
  # use like df.m$var1>3  & fn.subset.df.var(df.m, "var", evalvar, "compstring")
  #eg.  fn.subset.df.var(df.m, "factor.month.", month_predicting, "== month_max")
  # requires fn.comp # ie. fn.comp("== yrmax") (var<evaluatedvar>)
  str_var_evaluatedvar <- paste0(str_rootword, var_to_eval)
  vec <- fn.comp(comparisonstring) (df[[str_var_evaluatedvar]] ) #note [[]] evaluates string
  return(vec)
}
#eg fn.indexsubset.df.var(df.m[1:10,], "factor.month.", month_predicting, "== month_max")


