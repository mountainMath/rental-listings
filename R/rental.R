#' @export
#' @param start_date starting date for listings (inclusive)
#' @param end_date ending date for listings (exclusive)
#' @param region a geographic region
#' @param beds optional, filter by number of bedrooms. Can be integer or vector with several values
#' @param size optional, filter by size, vector with min and max size
#' @param sanity optional, sanity filter to exclude extremely low or high price listings
#' @param filter optional, possible values are "all", "furnished" or "unfurnished".
#' @param quiet optional, displays some debug info if true
get_listings <- function(start_date,end_date,region,beds=NA,size=NA,sanity=c(400,10000),filter='all',quiet=TRUE) {
  conn = RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), dbname = getOption("rental.dbname"), host=getOption("rental.host"), user=getOption("rental.user"), password=getOption("rental.password"))
  conditions <- c(
    paste0("\"postDate\" >= '",start_date,"'"),
    paste0("\"postDate\" < '",end_date,"'"),
    "location is not null"
  )
  if (!is.na(beds)){
    if (is.vector(beds)) {
      conditions <- c(conditions,
                      paste0("(",
                             paste(as.character(lapply(beds,function(b){
                               paste0("beds = '",b,"'")
                             })),collapse=" or "),")"
                      )
      )
    } else {
      conditions <- c(conditions,
                      paste0("beds = '",beds,"'")
      )
    }
  }
  if (length(sanity)>1){
    conditions <- c(conditions,
                    paste0("price >= ",sanity[1]," and price <= ",sanity[2])
    )
  }
  if (filter=='furnished') {
    conditions <- c(conditions,
                    "attributes like '%furnished%'"
    )
  } else if (filter=='unfurnished') {
    conditions <- c(conditions,
                    "not (attributes like '%furnished%')"
    )
  }
  if (!is.na(size)){
    conditions <- c(conditions,
                    "size is not null",
                    paste0("size >= '",size[1],"'"),
                    paste0("size < '",size[2],"'")
    )
  }
  if (!quiet) {print(paste(conditions,collapse = " and "))}
  conditions <- c(#"location is not null",
    #"st_isvalid(location)",
    paste0("ST_Intersects(location::geometry, ST_GeometryFromText('",sf::st_as_text(region),"',",attributes(region)$crs$epsg,")::geometry)"),
    conditions
  )
  query_string=paste(conditions,collapse = " and ")
  ls <- sf::st_read_db(conn, query = paste0("select \"postDate\" as post_date, location, price, beds, size from vancraig where ",
                                        query_string,";"), geom_column = "location")
  RPostgreSQL::dbDisconnect(conn)

  return(ls)
}
