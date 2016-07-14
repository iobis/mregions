##' Get related records based on their MRGID.
##'
##' @title Related records
##' @param mrgid the MRGID (Marineregions Global Identifier) for the
##'     record of interest
##' @param direction in which direction of the geographical hierarchy
##'     should the records be retrieved?
##' @param type what kind of relations should the records retrieve
##'     have with the place?
##' @param ... curl options to be passed on to \code{\link[httr]{GET}}
##' @examples \dontrun{
##'   tikehau <- geo_code("tikehau")
##'   place_relations(tikehau$MRGID)
##' }
##' @author Francois Michonneau <francois.michonneau@gmail.com>
##' @export
place_relations <- function(mrgid, direction = c("upper", "lower", "both"),
                            type = c("partof", "partlypartof", "adjacentto",
                                     "similarto", "administrativepartof",
                                     "influencedby", "all"), ...) {

    direction <- match.arg(direction)
    type <- match.arg(type)
    base <- paste0(mr_base(), "/getGazetteerRelationsByMRGID.json/%s/%s/%s")
    url <- sprintf(base, mrgid, direction, type)
    res <- httr::GET(url, ...)
    httr::stop_for_status(res)
    jsonlite::fromJSON(contutf8(res), flatten = TRUE)
}