cv.t <- function(alpha, df, alternative="not.equal", alpha.lower, alpha.upper, verbose=FALSE)
{

if(missing(alpha) & missing(alpha.lower) & missing(alpha.upper)) stop("You must specify either 'alpha' or 'alpha.lower' and 'alpha.upper'.")

if(missing(alpha)) alpha <- NULL
if(missing(alpha.lower)) alpha.lower <- NULL
if(missing(alpha.upper)) alpha.upper <- NULL

if(missing(df)) stop("You must specify the degrees of freedom (i.e., 'df').")
  
if(!is.null(alpha))
{
if(!is.null(alpha.lower)) stop("You have specified both 'alpha' and 'alpha.lower'; only use one approach.")
if(!is.null(alpha.upper)) stop("You have specified both 'alpha' and 'alpha.upper'; only use one approach.")
if(alpha <= 0 | alpha >= 1) stop("Specify 'alpha' to be greater than zero and less than 1.")

if(alternative %in% c("greater than", "greater-than", "greater", "greater.than", "gt", "g", ">", ">="))
{
alpha.lower <- 0
alpha.upper <- alpha
}
  
if(alternative %in% c("less than", "less-than", "lesser", "less.than", "lt", "l", "<", "<="))
{
alpha.lower <- alpha
alpha.upper <- 0
}
  
if(alternative %in% c("ne", "not equal", "two.sided", "two sided", "two-sided", "!=", "not.equal"))
{
alpha.lower <- alpha.upper <- alpha/2
}
}
  
if(is.null(alpha))
{
if(is.null(alpha.lower)) stop("With 'alpha=NULL' you need to specify 'alpha.lower' (and alpha.upper)")
if(is.null(alpha.upper)) stop("With 'alpha=NULL' you need to specify 'alpha.upper' (and alpha.lower)")
}
  
if(alpha.lower < 0 | alpha.lower >= .5) stop("Specify 'alpha.lower' to be greater than or equal to zero but less than .50")
if(alpha.upper < 0 | alpha.upper >= .5) stop("Specify 'alpha.upper' to be greater than or equal to zero but less than .50.")

critical.values <- qt(p=c(alpha.lower, 1-alpha.upper), df=df) # allows one or two tailed. 

if(verbose==TRUE) return(c(lower.cv=critical.values[1], upper.cv=critical.values[2], alpha.lower=alpha.lower, alpha.upper=alpha.upper, alpha=(alpha.lower + alpha.upper)))
if(verbose==FALSE) return(c(lower.cv=critical.values[1], upper.cv=critical.values[2]))
}

# Example uses
# cv.t(alpha=.05, alternative="not.equal", alpha.lower = NULL, alpha.upper = NULL, verbose=FALSE)
# cv.t(alpha=.05, df=10, alternative="not.equal")
# cv.t(alpha=.05, df=10, alternative="not.equal", alpha.lower = NULL, alpha.upper = NULL, verbose=TRUE)

# cv.t(df=10, alternative="not.equal", alpha.lower = 0, alpha.upper = .05, verbose=TRUE)
# cv.t(alpha=.05, df=10, alternative="not.equal", alpha.lower = NULL, alpha.upper = NULL, verbose=FALSE)

