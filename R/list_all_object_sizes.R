#' List all objects in environment by size, only where over 100k
#'
#' \code{wdwd} is a function that...
#' @export


allobj <- sort(sapply(ls(), function(x) {    # Apply object.size to all objects
    object.size(get(x)) }))


allobj[allobj>100000]

