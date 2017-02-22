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
      } else if (grepl("^.*(Bock|Dunkel|Doppelbock|Dark).*$", type, ignore.case=TRUE)) {
        result <- "Dark"
      } else if (grepl("^.*(Stout|Porter).*$", type, ignore.case=TRUE)) {
        result <- "Stout/Porter"
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
map.beer.cumulative.sum <- function(beers) {
  sum.ipa <- cumsum(lapply(beers$summary.type, function(x) { if (x == "IPA") 1 else 0}))
  sum.lager <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Lager") 1 else 0}))
  sum.cider <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Cider") 1 else 0}))
  sum.wheat <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Wheat Beer") 1 else 0}))
  sum.red <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Red/Bown Ale") 1 else 0}))
  sum.dark <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Dark") 1 else 0}))
  sum.stout <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Stout/Porter") 1 else 0}))
  sum.pilsner <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Pilsner") 1 else 0}))
  sum.golden <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Golden Ale") 1 else 0}))
  sum.sour <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Sour/Fruit/Spiced") 1 else 0}))
  sum.strong <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Strong/Bitter") 1 else 0}))
  sum.pale <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Pale Ale") 1 else 0}))
  sum.other <- cumsum(lapply(beers$summary.type, function(x) { if (x == "Other") 1 else 0}))
  idx <- order(c(seq_along(sum.ipa), seq_along(sum.lager), seq_along(sum.cider), seq_along(sum.wheat), seq_along(sum.red), seq_along(sum.dark), seq_along(sum.stout), seq_along(sum.pilsner), seq_along(sum.golden), seq_along(sum.sour), seq_along(sum.strong), seq_along(sum.pale),seq_along(sum.other)))
  sum.all <- unlist(c(sum.ipa, sum.lager, sum.cider, sum.wheat, sum.red, sum.dark, sum.stout, sum.pilsner, sum.golden, sum.sour, sum.strong, sum.pale, sum.other))[idx]
  sum.var <- rep(c("IPA", "Lager", "Cider", "Wheat Beer", "Red/Brown Ale", "Dark", "Stout/Porter", "Pilsner", "Golden Ale", "Sour/Fruit/Spiced", "Strong/Bitter", "pale Ale", "Other"), times = length(sum.ipa))
  sum.time <- rep(0:(length(sum.ipa)-1), each = 13)
  sumdata = data.frame(time=sum.time, var=sum.var, val=sum.all, ipa=rep(sum.ipa, times = 13))
  return(sumdata)
}