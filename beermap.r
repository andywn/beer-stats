map.beer.types <- function(beers) {
  if (is.null(beers$type)) {
    return("EEP")
  } else {
    summary.type <- lapply(beers$type, function(type) {
      if (is.null(type)) {
        result <- "Other"
      } else if (grepl("^.*(IPA|India(n?)\ Pale\ Ale).*$", type, ignore.case=TRUE) | type == "Imperial Pale Ale") {
        result <- "IPA"
      } else if (grepl("^.*(weizen|Wheat|Witbier).*$", type, ignore.case=TRUE)) {
        result <- "Wheat Beer"
      } else if (grepl("^.*(Red|Brown)\ Ale.*$", type, ignore.case=TRUE)) {
        result <- "Red/Bown Ale"
      } else if (grepl("^.*(Bock|Dunkel|Stout|Doppelbock|Porter|Dark).*$", type, ignore.case=TRUE)) {
        result <- "Dark"
      } else if (grepl("^.*(Pilsner|Kolsch|Kölsch).*$", type, ignore.case=TRUE)) {
        result <- "Pilsner"
      } else if (grepl("^.*(Golden|Blonde)\ Ale.*$", type, ignore.case=TRUE)) {
        result <- "Golden Ale"
      } else if (grepl("^(Sour|Saison|Fruit|Spiced|Herb|Ginger|Lambic).*$", type, ignore.case=TRUE)) {
        result <- "Sour/Fruit/Spiced"
      } else if (grepl("^.*Cider.*$", type, ignore.case=TRUE)) {
        result <- "Cider"
      } else if (grepl("^.*(Strong|Bitter|Dubel|Belgian\ Tripel|Belgian\ Dubbel).*$", type, ignore.case=TRUE)) {
        result <- "Strong/Bitter"
      } else if (grepl("^.*Pale\ Ale.*$", type, ignore.case=TRUE)) {
        result <- "Pale Ale"
      } else if (grepl("^.*(Lager|Kellerbier|Zwickelbier).*$", type, ignore.case=TRUE) | type == "California Common") {
        result <- "Lager"
      } else {
        result <- "Other"
      }
      return (result)
    })
    return (unlist(summary.type))
  }
}