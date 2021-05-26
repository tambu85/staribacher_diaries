library(stringi)
library(XML)

html_txt <- function(str) {
  xpathApply(htmlParse(str, asText=TRUE),
             "//body//text()", 
             xmlValue)[[1]] 
}

##The html_txt can parse the &#227 etc chars to their respective UTF values which can further be taken by stringi functions to convert into english alphabets

x <- names
txt <- html_txt(x)
Encoding(txt) <- "UTF-8" #encoding to utf-8, It is optional you may avoid it
splt_txt <-strsplit(txt,split="\n")[[1]]
stringi::stri_trans_general(splt_txt, "latin-ascii")