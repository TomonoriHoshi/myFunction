#Rのlsでは、マッチしか出来ないので、除外した場合のフィルターをできるように、npatternの指定をできるようにした。
ls2 <- function(name, pos = -1L, envir = as.environment(pos), 
                       all.names = FALSE, pattern, npattern, sorted = TRUE) 
{
    # Original name handling logic
    if (!missing(name)) {
        pos <- tryCatch(name, error = function(e) e)
        if (inherits(pos, "error")) {
            name <- substitute(name)
            if (!is.character(name)) 
                name <- deparse(name)
            warning(gettextf("%s converted to character string", 
                sQuote(name)), domain = NA)
            pos <- name
        }
    }
    
    # Get all names using internal function
    all.names <- .Internal(ls(envir, all.names, sorted))
    
    # Handle pattern matching
    if (!missing(pattern)) {
        if ((ll <- length(grep("[", pattern, fixed = TRUE))) && 
            ll != length(grep("]", pattern, fixed = TRUE))) {
            if (pattern == "[") {
                pattern <- "\\["
                warning("replaced regular expression pattern '[' by  '\\\\['")
            }
            else if (length(grep("[^\\\\]\\[<-", pattern))) {
                pattern <- sub("\\[<-", "\\\\\\[<-", pattern)
                warning("replaced '[<-' by '\\\\[<-' in regular expression pattern")
            }
        }
        all.names <- grep(pattern, all.names, value = TRUE)
    }
    
    # Add npattern filtering
    if (!missing(npattern)) {
        if ((ll <- length(grep("[", npattern, fixed = TRUE))) && 
            ll != length(grep("]", npattern, fixed = TRUE))) {
            if (npattern == "[") {
                npattern <- "\\["
                warning("replaced regular expression pattern '[' by  '\\\\['")
            }
            else if (length(grep("[^\\\\]\\[<-", npattern))) {
                npattern <- sub("\\[<-", "\\\\\\[<-", npattern)
                warning("replaced '[<-' by '\\\\[<-' in regular expression pattern")
            }
        }
        all.names <- all.names[!grepl(npattern, all.names)]
    }
    
    all.names <- all.names[all.names != "ls2"]
  
    all.names
}
                        
