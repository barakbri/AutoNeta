library('ggplot2')

## Barak is stealing all the taami 

# Yule's index function
yuleIndex <- function(x) { 
  x <- sort(x)
  l <- length(x)
  if (!any(is.numeric(x))) { 
    print("Can't calculate skewness for non-numeric arguments")
    stop()
  } 
  if (l < 4) {
    print("Can't calculate skewness for a vector of length 3 or less")   
    stop()
  }
  l_1 <- floor(l / 4)
  m_1 <- mean(x[1:l_1])
  m_2 <- mean(x[(l_1 + 1):(l - l_1 - 1)])
  m_3 <- mean(x[(l - l_1):l])
  numerator <- 0.5 * (m_1 + m_3) - m_2
  denominator <- 0.5 * (m_3 - m_1)
  sk <- numerator / denominator
  return(sk)
}



# Transformations ---------------------------------------------------------
new_categories <- function(x){
  x_val <- as.data.frame(table(x))
  freq <- x_val$Freq
  p <- c(0, rep(NA, length(x_val$Freq) - 1))
  q <- c(rep(NA, length(x_val$Freq) - 1), 1)
  for(i in 1:length(x_val$Freq)){
    if(i == 1){
      p[i] <- 0
      q[i] <- freq[i] / length(x)
    }else{
      p[i] <- q[i - 1]
      q[i] <- q[i - 1] + freq[i] / length(x)     }
  }
  t <- ifelse(p == 0, (q * log(q) + (1-q) * log(1-q)) / q,
              ifelse( q == 1, (-p * log(p) - (1 - p) * log(1 - p)) / (1-p), 
                      (q * log(q) + (1 - q) * log(1 - q) - p * log(p) - (1 - p) * log(1 - p)) / (q - p)))
  x_new <- t[match(x, x_val[, 1])] 
  return(x_new)
}



logit <- function(x, const = 1 / 3 , b = 1) { 
  return(log((x + const) / (b - x + const)))
}

## Replacing zero with the half the min absolute value of the vector 
## Does nothing otherwise
remZero <- function(x) {
  x.0 <- x == 0
  if (any(x.0)) {
    x[x.0] <- min(abs(x[!x.0])) / 2 
  }
  return(x)
}


transformList <- function(target.vec, transform.vec, a = min(target.vec), b = max(target.vec)) {
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
  yul.ind   <- unlist(lapply(transform.list, yuleIndex)); names(yul.ind) <- names(transform.list)
  ord.ind   <- order(yul.ind)
  transform.list <- list('Transformations' = append(list('None' = target.vec), transform.list[ord.ind]),
                         'Yule Index' = list(c('None' = yuleIndex(target.vec), yul.ind[ord.ind])))
  
  return(list('Transformations' = transform.list, 'Plots' = plotTransform(transform.list)))
}  


plotTransform <- function(transform.list) { 
  plot.list <- list()
  df.list   <- lapply(transform.list$Transformations, data.frame)
  p <- length(transform.list)
  yul.ind <- unlist(transform.list$`Yule Index`)
  for (i in 1:p) { 
    plot.list[[i]] <- ggplot(df.list[[i]], aes_string(x = names(df.list[[i]]))) +
      geom_histogram(aes(y=..density..)) + geom_density(color = 'blue', size = 1.25) +
      ggtitle(paste('Transformation', names(df.list)[i], 'Yule Index' = round(yul.ind[i], 4))) + 
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
  return(transformList(target.vec, temp.transform))
}

## Functions for counts 
countFunction <- function(target.vec) { 
  tran.vec <- c('log.sixth', 'inv.sqrt.sixth')
  return(transformList(target.vec, tran.vec))
}

## Function for Ratio 
ratioFunction <- function(target.vec) { 
  tran.vec <- c('log')
  return(transformList(target.vec, tran.vec))
}


## Bounded amount function 
boundedAmountFunc <- function(target.vec, a, b) { 
  tran.vec <- c('logit.norm', 'frac') 
  return(transformList(target.vec, tran.vec, a = a, b = b))
}

## Bounded Count 
boundedCountFunc <- function(target.vec, a, b) { 
  tran.vec <- c('logit.norm') 
  return(transformList(target.vec, tran.vec, a = a, b = b))
}

## Ranks  
ranksFunc <- function(target.vec, a, b) { 
  tran.vec <- c('logit.norm') 
  return(transformList(target.vec, tran.vec, a = a, b = b))
}

## Ordered Categories 
orderedCatFunc <- function(target.vec) { 
  tran.vec <- c('cumulative.entropy') 
  return(transformList(target.vec, tran.vec))
}

countFracFunction <- function(target.vec, b) { 
  tran.vec <- c('logit') 
  return(transformList(target.vec, tran.vec, b = b))
}

wrapTypes <- function(target.vec, type, a, b) { 
  if (type == "Amounts") {
    tran.list <- amountFunction(target.vec)
  }
  if (type == "Counts") {
    tran.list <- countFunction(target.vec) 
  }
  if (type == "Ratio") {
    tran.list <- ratioFunction(target.vec, a = a, b = b)
  }
  if (type  == "Proportion") {
    tran.list <- ratioFunction(target.vec, a = a, b = b)
  }
  if (type == "Counted Fraction") {
    tran.list <- countFracFunction(target.vec, b = b)
  }
  if (type == "Bounded Amounts") {
    tran.list <- boundedAmountFunc(target.vec, a = a, b = b)
  }
  if (type == "Bounded Counts") {
    tran.list <- boundedCountFunc(target.vec, a = a, b = b)
  }
  if (type == "Ranks") {
    tran.list <- ranksFunc(target.vec, a = a, b = b)
  }
  if (type == "Ordered Categories") {
    tran.list <- orderedCatFunc(target.vec)
  }
  return(tran.list)
}