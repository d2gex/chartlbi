bin_mat <- function(data, binwidth) {
  # First column is the current length class
  # Remaining columns are years
  # Returns data frame:
  # lclass, lmidp, and years

  current_binwidth <- data[2, 1] - data[1, 1]

  if (current_binwidth > binwidth) {
    stop("Bin width (", binwidth, ") should be greater than original bin width (",
         current_binwidth, ").")
  }

  data <- as.data.frame(data,
                        StringsAsFactors = FALSE)

  data[is.na(data)] <- 0

  minCL <- floor((min(data[, 1]) - .5) / binwidth) * binwidth
  maxCL <- ceiling((max(data[, 1]) + .5) / binwidth) * binwidth

  minCL <- ifelse(minCL <= 0, 0, minCL)

  break_list <- seq(minCL,
                    maxCL,
                    binwidth)

  data$LC <- cut(data[, 1],
                 breaks = break_list,
                 include.lowest = T)

  dWide <- aggregate(data[, 3:ncol(data) - 1],
                     by = list(data$LC),
                     sum, na.rm = FALSE)

  dWide <- merge(dWide,
                 data.frame(LC = as.character(levels(data$LC))),
                 by.x = "Group.1",
                 by.y = "LC",
                 all = TRUE,
                 sort = FALSE)

  dWide[is.na(dWide)] <- 0

  ints <- seq(minCL + binwidth / 2,
              maxCL + binwidth / 2,
              binwidth)

  dWide <- data.frame(lclass = dWide[, 1],
                      lmidp = ints[-length(ints)],
                      dWide[, 2:ncol(dWide)])
  return(dWide)
}

#' Calculate the wanted LBI indicators
#'
#' Generate a matrix with the calculated LBI indicators so that they can be compared with thresholds.
#' This is the untouched ICES function.
#'
#' @param data length-frequency matrix.
#' @param binwidth A number indicating the width of each bin.
#' @param linf A number indicating the asymptotic length.
#' @param lmat A number indicating the first sexual maturity.
#' @param weight weight-at-length matrix.
#' @returns A dataframe with all calculated indicators.
#' @source  https://raw.githubusercontent.com/ices-tools-dev/LBI_shiny/master/utilities.R
#' @export
lb_ind_ices <- function(data,
                        binwidth,
                        linf,
                        lmat,
                        weight) {

  # if(is.null(weight)) {
  #   message(paste0("Note: without weight, Lmaxy and associated reference points cannot be calculated.",
  #                  " The plots will still show up."))
  # } else {
  weight <- bin_mat(weight, binwidth)
  # }
  newDat <- bin_mat(data, binwidth)

  cols <- as.numeric(gsub("X", "", colnames(newDat)[-c(1:2)]))
  startyear <- min(cols)
  endyear <- max(cols)

  ind_names <- c("Year", "L75", "L25", "Lmed",
                 "L90", "L95", "Lmean", "Lc",
                 "LFeM", "Lmaxy", "Lmat", "Lopt",
                 "Linf", "Lmax5", "Lmean_LFeM",
                 "Lc_Lmat", "L25_Lmat", "Lmean_Lmat",
                 "Lmean_Lopt", "L95_Linf",
                 "Lmaxy_Lopt", "Lmax5_Linf", "Pmega", "Pmegaref")
  Ind <- data.frame(matrix(ncol = length(ind_names),
                           nrow = endyear - startyear + 1))
  names(Ind) <- ind_names
  Ind$Year <- startyear:endyear

  #  regrouping with selected length class width
  longDat <- reshape2::melt(newDat[, -1],
                            id.var = "lmidp",
                            value.name = "number",
                            variable.name = "year")
  longDat$year <- as.numeric(gsub("X", "", as.character(longDat$year)))

  Year <- seq(startyear, endyear)
  res <- data.frame(year = Year,
                    lmidp = NA,
                    nmax = NA,
                    lc = NA)

  # newDat <- bin_mat(data, binwidth)

  for (j in 3:ncol(newDat)) {
    for (i in 2:nrow(newDat)) {
      if (newDat[i + 1, j] - newDat[i, j] >= 0) {
        next
      } else {
        res$lmidp[j - 2] <- newDat$lmidp[i]
        res$nmax[j - 2] <- newDat[i, j]
        a <- res$nmax[j - 2] / 2
        df1 <- newDat[, c(2, j)]
        for (k in 1:nrow(df1)) {
          if (df1[k, 2] < a) {
            next
          } else {
            res$lc[j - 2] <- df1[k, 1]
          }
          break
        }
      }
      break
    }
  }

  Ind$Lc <- res$lc
  Ind$Lmat <- lmat
  Ind$Lopt <- 2 / 3 * linf
  Ind$Linf <- linf

  final <- newDat[, -1]
  for (jj in (1:length(Year)) + 1) {
    j <- jj - 1

    final2 <- final[, c(1, jj)]
    colnames(final2) <- c("lngth", "number")

    final2$cumsum <- cumsum(final2[, 2])
    final2$cumsum_perc <- final2$cumsum / sum(final2$number)

    # find mean top 5%
    # from largest starting
    numb <- as.data.frame(final2[rev(order(final2$lngth)), "number"])
    colnames(numb) <- "number"
    numb$cum <- cumsum(numb$number)
    numb$lngth <- final2[rev(order(final2$lngth)), "lngth"]
    numb$cumperc <- round(numb$cum / sum(numb$number), 5)
    numb$num5 <- 0
    numb[numb$cumperc <= 0.05, "num5"] <- numb[numb$cumperc <= 0.05, "number"]
    numb[max(which(numb$cumperc <= 0.05)) + 1,
         "num5"] <- (0.05 - numb[max(which(numb$cumperc <= 0.05)),
                                 "cumperc"]) * sum(numb$number)
    Ind[j, "Lmax5"] <- sum(numb$num5 * numb$lngth) / sum(numb$num5)

    # indicators
    Ind[j, "L75"] <- min(final2[which(final2$cumsum_perc >= 0.75), "lngth"])
    Ind[j, "L25"] <- min(final2[which(final2$cumsum_perc >= 0.25), "lngth"])
    Ind[j, "Lmed"] <- min(final2[which(final2$cumsum_perc >= 0.5), "lngth"])
    Ind[j, "L95"] <- min(final2[which(final2$cumsum_perc >= 0.95), "lngth"])
    Ind[j, "L90"] <- min(final2[which(final2$cumsum_perc >= 0.90), "lngth"])

    # calculate mean of individuals above Lc
    final3 <- final2[final2$lngth >= Ind[j, "Lc"],]
    Ind[j, "Lmean"] <- sum(final3$lngth * final3$number) / sum(final3$number)

    # length class with max yield
    if (!is.null(weight)) {
      final2$biomass <- final2$number * weight[, jj]
      Ind[j, "Lmaxy"] <- max(final2[final2$biomass == max(final2$biomass), "lngth"])
    } else {
      Ind[j, "Lmaxy"] <- NA
    }

    Lopt <- (2 / 3) * linf

    # proportion larger Lopt+10%
    Ind[j, "Pmega"] <- sum(final2[which(final2$lngth >= (Lopt + 0.1 * Lopt)),
                                  "number"]) / sum(final2$number)
    Ind[j, "Year"] <- Year[j]
    Ind[j, "Pmegaref"] <- 0.3   # proxy reference point of 30% in catch
    Ind[j, "LFeM"] <- 0.75 * Ind[j, "Lc"] + 0.25 * Ind[j, "Linf"]
  }

  #calculate various ratios
  Ind$Lmaxy_Lopt <- Ind$Lmaxy / Ind$Lopt
  Ind$L95_Linf <- Ind$L95 / Ind$Linf
  Ind$Lmean_LFeM <- Ind$Lmean / Ind$LFeM
  Ind$Lmean_Lmat <- Ind$Lmean / Ind$Lmat
  Ind$Lmean_Lopt <- Ind$Lmean / Ind$Lopt
  Ind$Lmax5_Linf <- Ind$Lmax5 / Ind$Linf
  Ind$Lc_Lmat <- Ind$Lc / Ind$Lmat
  Ind$L25_Lmat <- Ind$L25 / Ind$Lmat
  return(Ind)
}

#' Calculate the wanted LBI indicators
#'
#' Generate a matrix with the calculated LBI indicators so that they can be compared with thresholds.
#' This is the modified version of 'lb_ind_ices' and does not allow to use the weight-at-length matrix needed only
#' to calculate the 'Optimal Yield' as the maximum length that holds the largest biomass. The reason
#' being is that there are two bugs as follows: a) the multiplication of the frequency by the weight-at-length matrices
#' is performed in such a way that not the same year columns are picked and multiplied in the two matrices - columns
#' are shifted - and b) multiple length classes may have the same biomass yielding a returned dimension error.
#' Given these bugs and the fact that the affected indicator - Lmaxy/Lopt- isn't ued in the the traffic-light
#' visualisation form -lb_table-, this indicator is no longer calculated in this version of the function.
#'
#' @param data length-frequency matrix.
#' @param binwidth A number indicating the width of each bin.
#' @param linf A number indicating the asymptotic length.
#' @param lmat A number indicating the first sexual maturity.
#' @returns A dataframe with all calculated indicators
#' @export
lb_ind_pretty <- function(data,
                          binwidth,
                          linf,
                          lmat) {

  newDat <- bin_mat(data, binwidth)

  cols <- as.numeric(gsub("X", "", colnames(newDat)[-c(1:2)]))
  startyear <- min(cols)
  endyear <- max(cols)

  ind_names <- c("Year", "L75", "L25", "Lmed",
                 "L90", "L95", "Lmean", "Lc",
                 "LFeM", "Lmat", "Lopt",
                 "Linf", "Lmax5", "Lmean_LFeM",
                 "Lc_Lmat", "L25_Lmat", "Lmean_Lmat",
                 "Lmean_Lopt", "L95_Linf",
                 "Lmax5_Linf", "Pmega", "Pmegaref")
  Ind <- data.frame(matrix(ncol = length(ind_names),
                           nrow = endyear - startyear + 1))
  names(Ind) <- ind_names
  Ind$Year <- startyear:endyear

  #  regrouping with selected length class width
  longDat <- reshape2::melt(newDat[, -1],
                            id.var = "lmidp",
                            value.name = "number",
                            variable.name = "year")
  longDat$year <- as.numeric(gsub("X", "", as.character(longDat$year)))

  Year <- seq(startyear, endyear)
  res <- data.frame(year = Year,
                    lmidp = NA,
                    nmax = NA,
                    lc = NA)

  # newDat <- bin_mat(data, binwidth)

  for (j in 3:ncol(newDat)) {
    for (i in 2:nrow(newDat)) {
      if (newDat[i + 1, j] - newDat[i, j] >= 0) {
        next
      } else {
        res$lmidp[j - 2] <- newDat$lmidp[i]
        res$nmax[j - 2] <- newDat[i, j]
        a <- res$nmax[j - 2] / 2
        df1 <- newDat[, c(2, j)]
        for (k in 1:nrow(df1)) {
          if (df1[k, 2] < a) {
            next
          } else {
            res$lc[j - 2] <- df1[k, 1]
          }
          break
        }
      }
      break
    }
  }

  Ind$Lc <- res$lc
  Ind$Lmat <- lmat
  Ind$Lopt <- 2 / 3 * linf
  Ind$Linf <- linf

  final <- newDat[, -1]
  for (jj in (1:length(Year)) + 1) {
    j <- jj - 1

    final2 <- final[, c(1, jj)]
    colnames(final2) <- c("lngth", "number")

    final2$cumsum <- cumsum(final2[, 2])
    final2$cumsum_perc <- final2$cumsum / sum(final2$number)

    # find mean top 5%
    # from largest starting
    numb <- as.data.frame(final2[rev(order(final2$lngth)), "number"])
    colnames(numb) <- "number"
    numb$cum <- cumsum(numb$number)
    numb$lngth <- final2[rev(order(final2$lngth)), "lngth"]
    numb$cumperc <- round(numb$cum / sum(numb$number), 5)
    numb$num5 <- 0
    numb[numb$cumperc <= 0.05, "num5"] <- numb[numb$cumperc <= 0.05, "number"]
    numb[max(which(numb$cumperc <= 0.05)) + 1,
         "num5"] <- (0.05 - numb[max(which(numb$cumperc <= 0.05)),
                                 "cumperc"]) * sum(numb$number)
    Ind[j, "Lmax5"] <- sum(numb$num5 * numb$lngth) / sum(numb$num5)

    # indicators
    Ind[j, "L75"] <- min(final2[which(final2$cumsum_perc >= 0.75), "lngth"])
    Ind[j, "L25"] <- min(final2[which(final2$cumsum_perc >= 0.25), "lngth"])
    Ind[j, "Lmed"] <- min(final2[which(final2$cumsum_perc >= 0.5), "lngth"])
    Ind[j, "L95"] <- min(final2[which(final2$cumsum_perc >= 0.95), "lngth"])
    Ind[j, "L90"] <- min(final2[which(final2$cumsum_perc >= 0.90), "lngth"])

    # calculate mean of individuals above Lc
    final3 <- final2[final2$lngth >= Ind[j, "Lc"],]
    Ind[j, "Lmean"] <- sum(final3$lngth * final3$number) / sum(final3$number)
    Lopt <- (2 / 3) * linf

    # proportion larger Lopt+10%
    Ind[j, "Pmega"] <- sum(final2[which(final2$lngth >= (Lopt + 0.1 * Lopt)),
                                  "number"]) / sum(final2$number)
    Ind[j, "Year"] <- Year[j]
    Ind[j, "Pmegaref"] <- 0.3   # proxy reference point of 30% in catch
    Ind[j, "LFeM"] <- 0.75 * Ind[j, "Lc"] + 0.25 * Ind[j, "Linf"]
  }

  #calculate various ratios
  Ind$L95_Linf <- Ind$L95 / Ind$Linf
  Ind$Lmean_LFeM <- Ind$Lmean / Ind$LFeM
  Ind$Lmean_Lmat <- Ind$Lmean / Ind$Lmat
  Ind$Lmean_Lopt <- Ind$Lmean / Ind$Lopt
  Ind$Lmax5_Linf <- Ind$Lmax5 / Ind$Linf
  Ind$Lc_Lmat <- Ind$Lc / Ind$Lmat
  Ind$L25_Lmat <- Ind$L25 / Ind$Lmat
  return(Ind)
}
