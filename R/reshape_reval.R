reshape_reval <- function(x,y) {
  ans <- fifelse(x<y,y-x,
                fcase(
                    x%%y %in% c(0,5,10,15,20,25), 5,
                    x%%y %in% c(1,6,11,16,21), 4,
                    x%%y %in% c(2,7,12,17,22),3,
                    x%%y %in% c(3,8,13,18,23), 2,
                    x%%y %in% c(4,9,14,19,24), 1
                    ))
  return(ans)
}