
require(reticulate)
reticulate::source_python("https://raw.githubusercontent.com/benjaminguinaudeau/tiktokr/master/stealth.py")
reticulate::source_python("https://raw.githubusercontent.com/benjaminguinaudeau/tiktokr/master/browser.py")

#' @post /
#' @serializer unboxedJSON
function(req){
  res <- jsonlite::fromJSON(req$postBody)

  url <- res$url
  ua <- res$ua

  tries <- 0
  br <- tryCatch(py$browser(url, ua), error = function(e) NULL, warning = function(w) NULL, message=  function(m) NULL)
  while(tries < 3 & inherits(br, "try-error")){
    br <- tryCatch(py$browser(url, ua), error = function(e) NULL, warning = function(w) NULL, message=  function(m) NULL)
    tries <- tries + 1
  }

  return(br$signature)

}
