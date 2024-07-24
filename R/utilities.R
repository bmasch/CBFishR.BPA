unMoney <- function(monetaryStrings) {
  #' Remove the dollar sign and commas and converts numbers within parentheses to negative numbers
  #'
  #' @param monetaryStrings String vector

  #' @return numeric vector
  #' @export
  #' @examples unMoney(moneystr)
  #'
  cleanedStrings <- gsub("\\$", "", monetaryStrings)
  cleanedStrings <- gsub(",", "", cleanedStrings)

  # Convert to numeric, handling negative values represented by parentheses
  numericValues <- sapply(cleanedStrings, function(x) {
    if (grepl("\\(", x)) {
      # Remove parentheses and convert to negative numeric value
      return(-as.numeric(gsub("[()]", "", x)))
    }
    else if(is.na(x)){
      return(0)
    }
    else {
      # Convert positive values directly to numeric
      return(as.numeric(x))
    }
  })

  return(unname(numericValues))
}

unPct <- function(amt){
  #' Remove the percent sign and divides by 100
  #'
  #' @param amt String vector

  #' @return numeric vector
  #' @export
  #' @examples unPct(pctstr)
  #'
  value=(as.numeric(gsub('\\%','',amt))/100)
  value <- ifelse(is.na(value),0,value)
  return(value)
}

setUnspecified <- function(name){
  #' Set values="Unspecified" if ""
  #'
  #' @param name String vector

  #' @return String vector
  #' @export
  #' @examples setUnspecified(example)
  #'
  return (ifelse(name=="","Unspecified",name))
}

trimMult <- function(x, char=" ") {
  #' I have no idea
  #'
  #' @param amt String vector

  #' @return String vector
  #' @export
  #' @examples trimMult(example)
  #'
  return(gsub(paste0("^", char, "*|(?<=", char, ")", char, "|", char, "*$"),
              "", x, perl=T))
}




