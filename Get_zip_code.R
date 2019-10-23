deg2rad <- function(deg) {
  return(deg * (pi / 180))
}

getDistanse <- function(lat1,lon1, lat2, lon2) {
  R <- 6371
  dLat <-  deg2rad(lat2-lat1)
  dLon <- deg2rad(lon2-lon1)
  a <- sin(dLat/2)^2 + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dLon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  return(d)
}

which.min(getDistanse(code$X2, code$X3, crime$Lat[1], crime$Long[1]))

code$X1[57]

getthecode <- function(Lat2, Lon2) {
  ind <- which.min(getDistanse(code$X2, code$X3, Lat2, Lon2))
  return(code$X1[ind])
}

getthecode(crime$Lat[1], crime$Long[1])