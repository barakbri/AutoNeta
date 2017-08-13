## Load libraries 
library('ggplot2')


# Yule's index function
yuleIndex <- function(x) { 
  x <- sort(na.omit(x))
  l <- length(x)
  ## Checks 
  if (!any(is.numeric(x))) { 
    print("Can't calculate skewness for non-numeric arguments")
    stop()
  } 
  ## Return 0 if variable is constant 
  if (length(unique(x.no.na)) == 1) {
    return(0)
  }
  if (l < 4) {
    print("Can't calculate skewness for a vector of length 3 or less")   
    stop()
  }
  l_1 <- floor(l / 4)
  m_1 <- mean(x[1:l_1], na.rm = T)
  m_2 <- mean(x[(l_1 + 1):(l - l_1 - 1)], na.rm = T)
  m_3 <- mean(x[(l - l_1):l], na.rm = T)
  numerator <- 0.5 * (m_1 + m_3) - m_2
  denominator <- 0.5 * (m_3 - m_1)
  sk <- numerator / denominator
  return(sk)
}



# Transformations ---------------------------------------------------------

## Logit
logit <- function(x, const = 1 / 3 , b = 1) { 
  return(log((x + const) / (b - x + const)))
}

## Replacing zero with the half the min absolute value of the vector 
## Does nothing otherwise
remZero <- function(x) {
  x.0 <- x == 0
  if (any(x.0, na.rm = T)) {
    x[x.0] <- min(abs(x[!x.0])) / 2 
  }
  return(x)
}


transformList <- function(target.vec, 
                          transform.vec, 
                          a = min(target.vec, na.rm = T), 
                          b = max(target.vec, na.rm = T),
                          to.reverse  = FALSE, 
                          bin.width   = NULL,
                          window.size = NULL,
                          var.name    = NULL) {
  transform.list      <- list()
  target.vec.rem.zero <- remZero(target.vec)
  if ('log' %in% transform.vec) { 
    transform.list[['log']] <- log(target.vec.rem.zero) 
    }
  if ('sqrt' %in% transform.vec) { 
    transform.list[['sqrt']] <- sqrt(target.vec) 
  }
  if ('invert' %in% transform.vec) { 
    transform.list[['invert']] <- 1 / target.vec.rem.zero
  }
  if ('inv.sqrt' %in% transform.vec) { 
    transform.list[['inv.sqrt']] <- 1 / sqrt(target.vec.rem.zero)
  }
  if ('logit' %in% transform.vec) { 
    transform.list[['logit']] <- logit(target.vec, b)
  }
  if ('norm.logit' %in% transform.vec) { 
    transform.list[['norm.logit']] <- logit((target.vec - b) / (b - a), b = 1)
  }
  if ('cumulative.entropy' %in% transform.vec) { 
    transform.list[['cumulative.entropy']] <- cumlativeEntropy(target.vec)
  }
  if ('log.sixth' %in% transform.vec) { 
    transform.list[['log.sixth']] <- log(target.vec + (1 / 6))
  }
  if ('power.2' %in% transform.vec) { 
    transform.list[['power.2']] <- target.vec^2   
  } 
  if ('inv.sqrt.sixth' %in% transform.vec) { 
    transform.list[['inv.sqrt.sixth']] <- 1 / sqrt(target.vec.rem.zero + (1 / 6))
  }
  if ('frac' %in% transform.vec) { 
    transform.list[['frac']]  <- (target.vec - b) / (b - a)
  }
  ## Yule Index Only available 
  yul.ind   <- unlist(lapply(transform.list, yuleIndex)); names(yul.ind) <- names(transform.list)
  ord.ind   <- order(abs(yul.ind))
  transform.list <- list('Transformations' = append(list('None' = target.vec), transform.list[ord.ind]),
                         'Yule Index' = list(c('None' = yuleIndex(target.vec), yul.ind[ord.ind])))
  
  ## Plotting 
  if (is.null(bin.size)) { 
    bin.size <- unlist(lapply(transform.list$Transformations, BinWidthHist))
  } 
  if (is.null(window.size)) { 
    win.size <- unlist(lapply(transform.list$Transformations, BinWidthHist))
  }
  return(list('Transformations' = transform.list, 
              'Plots' = plotTransform(transform.list, 
                                      var.name, 
                                      bin.width.plot = bin.size, 
                                      window.size.plot = win.size)))
}  


plotTransform <- function(transform.list, var.name, bin.width.plot, window.size.plot) { 
  plot.list <- list()
  df.list   <- lapply(transform.list$Transformations, data.frame)
  p <- length(transform.list$Transformations)
  yul.ind <- unlist(transform.list$`Yule Index`)
  for (i in 1:p) { 
    plot.list[[i]] <- ggplot(df.list[[i]], aes_string(x = names(df.list[[i]]))) +
      geom_histogram(aes(y=..density..), binwidth = bin.width.plot[i]) + 
      geom_density(color = 'blue', size = 1.25, bw = window.size.plot[i]) +
      ggtitle(paste(var.name, 'Transformation', names(df.list)[i], 'Yule Index' = round(yul.ind[i], 4))) + 
      xlab('Variable') 
    }
  return(plot.list)
}


cumlativeEntropy <- function(x) { 
  sum.x <- length(x)
  tab.x <- data.frame(table(x) / sum.x)
  p     <- nrow(tab.x)
  ## Dealing p = 0, first group 
  temp.q <- tab.x[1,'Freq'] 
  tab.x[1,'Changed.Value'] <- (temp.q * log(temp.q) + (1 - temp.q) * log(1 - temp.q)) / temp.q 
  ## Dealing q = 1, last group 
  temp.p <-  (1 -  tab.x[p,'Freq']) 
  tab.x[p,'Changed.Value'] <- (-temp.p * log(temp.p) - (1 - temp.p) * log(1 - temp.p)) / (1 - temp.p)
  for (i in 2:(p - 1)) {
    temp.q                   <- sum(tab.x[1:i,'Freq'])
    temp.p                   <- sum(tab.x[1:(i - 1),'Freq'])
    tab.x[i,'Changed.Value'] <- (temp.q * log(temp.q) + (1 - temp.q) * log(1 - temp.q) - 
                                (temp.p * log(temp.p) + (1 - temp.p) * log(1 - temp.p))) / 
                                (temp.q - temp.p)
  }
  x.new <-  tab.x[match(x, tab.x[ ,'x']),'Changed.Value']
  return(x.new)
}

## Function for amounts 
amountFunction <- function(target.vec) { 
  # If right skewed perform sqrt, log and inverse transformation
  if (mean(target.vec, na.rm = TRUE) > median(target.vec, na.rm = TRUE))
  {
    temp.transform <- c('log', 'sqrt', 'invert')
  ## If left skewed perform power of 2 
  } else {
    temp.transform <- c('power.2')
  }
  return(temp.transform)
}

## Functions for counts 
countFunction <- function(target.vec) { 
  tran.vec <- c('log.sixth', 'inv.sqrt.sixth')
  return( tran.vec)
}

## Function for Ratio 
ratioFunction <- function(target.vec) { 
  tran.vec <- c('log')
  return(tran.vec)
}


## Bounded amount function 
boundedAmountFunc <- function(target.vec, a, b) { 
  tran.vec <- c('logit.norm', 'frac') 
  return(tran.vec)
}

## Bounded Count 
boundedCountFunc <- function(target.vec, a, b) { 
  tran.vec <- c('logit.norm') 
  return(tran.vec)
}

## Ranks  
ranksFunc <- function(target.vec, a, b) { 
  tran.vec <- c('logit.norm') 
  return(tran.vec)
}

## Ordered Categories 
orderedCatFunc <- function(target.vec) { 
  tran.vec <- c('cumulative.entropy') 
  return(tran.vec)
}

## Counted fractions 
countFracFunction <- function(target.vec, b) { 
  tran.vec <- c('logit') 
  return(tran.vec)
}


wrapTypes <- function(target.vec, 
                      type,
                      a = min(target.vec, na.rm = T), 
                      b = max(target.vec, na.rm = T),
                      to.reverse  = FALSE, 
                      bin.width   = NULL,
                      window.size = NULL,
                      var.name    = NULL) { 
  if (type == "Amounts") {
    tran.vec <- amountFunction(target.vec)
  }
  if (type == "Counts") {
    tran.vec <- countFunction(target.vec) 
  }
  if (type == "Ratio") {
    tran.vec <- ratioFunction(target.vec, a = a, b = b)
  }
  if (type  == "Proportion") {
    tran.vec <- ratioFunction(target.vec, a = a, b = b)
  }
  if (type == "Counted Fraction") {
    tran.vec <- countFracFunction(target.vec, b = b)
  }
  if (type == "Bounded Amounts") {
    tran.vec <- boundedAmountFunc(target.vec, a = a, b = b)
  }
  if (type == "Bounded Counts") {
    tran.vec <- boundedCountFunc(target.vec, a = a, b = b)
  }
  if (type == "Ranks") {
    tran.vec <- ranksFunc(target.vec, a = a, b = b)
  }
  if (type == "Ordered Categories") {
    tran.vec <- orderedCatFunc(target.vec)
  }
  return(transformList(target.vec = target.vec, 
                        transform.vec = tran.vec,
                        a = a, 
                        b = b,
                        to.reverse = to.reverse,
                        bin.width = bin.width,
                        window.size = window.size,
                        var.name = var.name))
}

## Automatic VarType guesser 
## Cant guess ordered categories or any type of bounded variable 

GuessType <- function(x, cat.thres = 2) {
  x.no.na <- na.omit(x)
  ## If not a number then determined by user
  if (!(is.numeric(x.no.na)) || !(is.integer(x.no.na))) { 
    return(c('a' = NA, 'b' = NA, 'Type' = 'To Be Determined'))
  }
  a.guess   <- min(x.no.na) 
  b.guess   <- max(x.no.na) 
  ## Cheacking var type 
  is.const  <- length(unique(x.no.na)) == 1
  ## Number of unique values is smaller than threshold 
  is.category <- length(unique(x.no.na)) <= cat.thres
  ## Count all observation are positive and round numbers 
  is.count  <- (all(x.no.na == round(x.no.na, 2)) && (a.guess >= 0)) && (length(unique(x.no.na)) != x.no.na)
  ## All the observations are between 0 and 1 
  is.frac   <- (a.guess >= 0) && (b.guess <= 1)
  ## Ranking ranks remains rank 
  is.rank   <- all(sort(x.no.na) == sort(rank(x.no.na)))
  ## Not complete numbers or has negative observations 
  is.amount  <- ((mean(x.no.na - round(x.no.na))^2) > 0) || a.guess < 0 
  if (is.const == T) {
    return(c('a' = a.guess, 'b' = b.guess, 'Type' = "Constant"))
  }
  if (is.category == T) { 
    return(c('a' = a.guess, 'b' = b.guess, 'Type' = "Category"))
  }
  if (is.count == T) { 
    return(c('a' = a.guess, 'b' = b.guess, 'Type' = "Counts"))
  }
  if (is.frac == T) {
    return(c('a' = 0, 'b' = 1, 'Type' = "Proportion"))
  }
  if (is.rank == T) { 
    return(c('a' = a.guess, 'b' = b.guess, 'Type' = "Ranks"))
  }
  if (is.amount == T) { 
    return(c('a' = a.guess, 'b' = b.guess, 'Type' = "Amounts"))
  }
  else { 
    return(c('a' = NA, 'b' = NA, 'Type' = "To Be Determined"))
  }
}


## Using Guess On Variables 

WrapGuess <- function(file) { 
  p         <- ncol(file) 
  name.vec  <- colnames(file)  
  guess.dat <- data.frame('Variable' = rep(NA ,p), 
                          'a' = rep(NA ,p), 
                          'b' = rep(NA ,p), 
                          'Type' = rep(NA ,p), 
                          'To Reverse' = rep(NA ,p))  
  for (i in 1:p) { 
    guess.dat[i,'Variable'] <- name.vec[i] 
    guess.dat[i,c(2:4)]     <- GuessType(file[ ,i])
  }
  return(guess.dat) 
}


## Binwidth Calculater based on Freedman Diaconis 
BinWidthHist <- function(x) { 
  return((2 * IQR(x, na.rm = TRUE) / length(na.omit(x))^(1 / 3)))
}

## Binwidth for density based on bw.nrd0 of base R 
BinWidthDens <- function(x) { 
  return(bw.nrd0(na.omit(x)))
}

