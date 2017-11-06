###########################
# Libraries Load ----------------------------------------------------------
library('ggplot2')
###########################

###########################
# Index Functions  --------------------------------------------------------


## Creating index, currently only supports YULE
## Should always be as following 1. Index value 2. Order index (allowing none to be first)
## 3. Name of index
createIndex <- function(transform.list, index = 'Yule') { 
  if (index == 'Yule') { 
    yul.ind   <- unlist(lapply(transform.list, yuleIndex))
    names(yul.ind) <- names(transform.list)
    if (length(yul.ind) == 1) {
      return(list('Index' = yul.ind, 'Order' = 1, 'Name' = index))
    }
    ord.ind <- c(1, order(abs(yul.ind[2:length(yul.ind)])) + 1)
    return(list('Index' = yul.ind[ord.ind], 'Order' = ord.ind, 'Name' = index))  
  }
}


# Yule's index function
yuleIndex <- function(x) { 
  
  ## Checks 
  if (is.null(x)) { 
    return(NULL)
  }
  if (!any(is.numeric(x))) { 
    print("Can't calculate skewness for non-numeric arguments")
    return(NaN)
  } 
  x <- sort(x)
  l <- length(x)
  ## Return 0 if variable is constant 
  if (length(unique(x)) == 1) {
    return(NaN)
  }
  if (l < 4) {
    return(NaN)
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


## Optimize Yule according to tukey ladder 

optimYule <- function(x, ladder = c(-2, -1, -0.5, 0, 0.5, 1, 2)) { 
  no.sqrt <- any(x <= 0, na.rm = TRUE) 
  do.log  <- any(ladder == 0, na.rm = TRUE) & !(no.sqrt)
  ladder  <- ladder[!(no.sqrt & (abs(round(ladder)) -  abs(ladder)) != 0)] 
  ladder  <- ladder[!(ladder == 0)]
  n <- length(ladder) 
  yule.score <- rep(NA, n + 1 * do.log)
  for (i in 1:n) { 
    yule.score[i] <- yuleIndex(x^ladder[i])
  }
  names(yule.score) <- ladder
  if (do.log) { 
    yule.score[n + 1] <- yuleIndex(log(x))
    names(yule.score)[n + 1] <- '0'
  }
  pos <- which.min(abs(yule.score))
  return(c('Power' = as.numeric(names(yule.score)[pos]),
           'Yule Index' = yule.score[pos]))
}


###########################

###########################
# Transformations ---------------------------------------------------------

## Cumlative Entropy 
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
  x.new <-  tab.x[match(x, tab.x[ ,'x']), 'Changed.Value']
  return(x.new)
}

## Logit 
logit <- function(x) { 
  return(log(x / (1 - x)))
}

## Logit.b
logit.b <- function(x, const = 1 / 3 , b = 1) { 
  return(log((x + const) / (b - x + const)))
}


## Norm Logit 
norm.logit <- function(x, const = 1 / 3, a = 0, b = 1) { 
  frac.x <- (x - a) / (b - a)
  return(logit.b(frac.x, b = b))
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


## Wrapping Transformations 
transformList <- function(target.vec, 
                          transform.vec, 
                          a = NULL, 
                          b = NULL) {
  transform.list      <- list()
  target.vec.rem.zero <- remZero(target.vec)
  ## Always add original at begining 
  transform.list[['none']] <- target.vec
  if ('log' %in% transform.vec) { 
    transform.list[['log']] <- log(target.vec.rem.zero) 
  }
  if ('sqrt' %in% transform.vec) { 
    transform.list[['sqrt']] <- sqrt(target.vec.rem.zero) 
  }
  if ('invert' %in% transform.vec) { 
    transform.list[['invert']] <- 1 / target.vec.rem.zero
  }
  if ('inv.sqrt' %in% transform.vec) { 
    transform.list[['inv.sqrt']] <- 1 / sqrt(target.vec.rem.zero)
  }
  if ('logit' %in% transform.vec) { 
    transform.list[['logit']] <- logit(traget.vec)
    }
  if ('logit.b' %in% transform.vec) { 
    transform.list[['logit.b']] <- logit.b(target.vec, b)
  }
  if ('logit.norm' %in% transform.vec) { 
    transform.list[['norm.logit']] <- norm.logit(target.vec, a = a, b = b)
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
    transform.list[['frac']]  <- (target.vec - a) / (b - a)
  }
  if ('to.reverse' %in% transform.vec) {
    transform.list[['to.reverse']] <- b - target.vec 
  }
  if ('tukey.optim' %in% transform.vec) { 
    optim.yule <- optimYule(target.vec)
    transform.list[[paste0('optim.yule.power.', optim.yule[1])]] <- target.vec ^ optim.yule[1]
    }
  return(transform.list)
}


###########################

###########################
# Plotting Functions  -----------------------------------------------------



## Binwidth Calculater based on Freedman Diaconis 
BinWidthHist <- function(x) { 
  bin.width <- (2 * IQR(x, na.rm = TRUE) / length(na.omit(x))^(1 / 3))
  bin.width <- ifelse(bin.width == 0, 2 / length(na.omit(x))^(1 / 3), bin.width)
  return(bin.width)
}

## Binwidth for density based on bw.nrd0 of base R 
BinWidthDens <- function(x) { 
  return(bw.nrd0(na.omit(x)))
}

## Creating a nicer theme for histograms
themeNice <- function() {
  theme.list <- list()
  theme.list[['Theme']] <-   theme_bw() +
                             theme(axis.line = element_line(size=1, colour = "black"),
                             panel.grid.major = element_line(colour = "#d3d3d3"),
                             panel.grid.minor = element_blank(),
                             panel.border = element_blank(), panel.background = element_blank(),
                             axis.text.x=element_text(colour="black", size = 12),
                             axis.text.y=element_text(colour="black", size = 12),
                             plot.title = element_text(hjust = 0.5))
  theme.list[['Color']] <- c('Density' = 'firebrick4', 'Hist.Col' = '#1F3552', 'Hist.Fill' = 'slategray3')
  return(theme.list)
}


## Plotting 
plotTransform <- function(transform.list, 
                          var.name = NULL, 
                          bin.width.plot = 1, 
                          window.size.plot = 1,
                          KDE = T,
                          ind.vec,
                          ind.name = NULL,
                          theme = 'Nice') { 
  ## Dealing with sizes (KDE, binwidth )
  bin.size   <- unlist(lapply(transform.list, BinWidthHist)) * bin.width.plot 
  if (KDE == T) { 
    win.size <- unlist(lapply(transform.list, BinWidthDens)) * window.size.plot
  }
  plot.list <- list()
  df.list   <- lapply(transform.list, data.frame)
  p       <- length(transform.list)
  for (i in 1:p) { 
    temp.plot <- ggplot(df.list[[i]], aes_string(x = names(df.list[[i]]))) +
      geom_histogram(aes(y=..density..), binwidth = bin.size[i]) + 
      ggtitle(paste('Transformation:', names(df.list)[i],
                    paste0('\n',ind.name), '=', round(ind.vec[i], 4))) + 
      xlab(var.name) + ylab('Density')
    if (theme == 'Nice')  {
      temp.plot <- temp.plot + 
        themeNice()[['Theme']] +
        geom_histogram(aes(y=..density..), binwidth = bin.size[i], 
                       colour = themeNice()[['Color']]['Hist.Col'], 
                       fill = themeNice()[['Color']]['Hist.Fill']) 
    }
    if (KDE == T) { 
      temp.plot <- temp.plot + geom_density(color = themeNice()[['Color']]['Density'], size = 1.25, bw = win.size[i]) 
    }
    plot.list[[i]] <- temp.plot
  }
  return(plot.list)
}
###########################

###########################
# Categories Functions  ---------------------------------------------------


## Function for amounts 
amountFunction <- function(target.vec) { 
  # If right skewed perform sqrt, log and inverse transformation
  if (mean(target.vec, na.rm = TRUE) > median(target.vec, na.rm = TRUE))
  {
    temp.transform <- c('log', 'sqrt', 'invert', 'tukey.optim')
    ## If left skewed perform power of 2 
  } else {
    temp.transform <- c('power.2', 'tukey.optim')
  }
  return(temp.transform)
}

## Functions for counts 
countFunction <- function(target.vec) { 
  tran.vec <- c('log.sixth', 'inv.sqrt.sixth', 'sqrt')
  return(tran.vec)
}

## Function for Ratio 
ratioFunction <- function(target.vec, a, b) { 
  tran.vec <- c('log')
  return(tran.vec)
}

## Function for Proportion 
propFunction <- function() {
  tran.vec <- c('logit', 'logit.b') 
  return(tran.vec)
}

## Bounded amount function 
boundedAmountFunc <- function(target.vec, a, b) { 
  tran.vec <- c('logit.norm', 'frac') 
  return(tran.vec)
}

## Bounded Count 
boundedCountFunc <- function(target.vec, a, b) { 
  tran.vec <- c('logit.norm', 'frac') 
  return(tran.vec)
}

## Ranks  
ranksFunc <- function(target.vec, a, b) { 
  tran.vec <- c('logit.norm', 'frac') 
  return(tran.vec)
}

## Ordered Categories 
orderedCatFunc <- function(target.vec) { 
  tran.vec <- c('cumulative.entropy') 
  return(tran.vec)
}

## Counted fractions 
countFracFunction <- function(target.vec, b) { 
  tran.vec <- c('logit.b' ,'logit.norm') 
  return(tran.vec)
}


###########################

###########################
# Wrapping Function Barak  ------------------------------------------------


wrapTypes <- function(target.vec, 
                      type,
                      a = min(target.vec, na.rm = T), 
                      b = max(target.vec, na.rm = T),
                      to.reverse  = FALSE, 
                      bin.width   = 1,
                      window.size = 1,
                      var.name    = NULL,
                      index.type  = 'Yule') { 
  tran.vec <- NULL
  dens.flag <- TRUE
  if (type == "Amounts") {
    tran.vec <- amountFunction(target.vec)
  }
  if (type %in% c("Counts", "Count", "count", "counts")) {
    tran.vec <- countFunction(target.vec) 
    dens.flag <- FALSE
  }
  if (type == "Ratio") {
    tran.vec <- ratioFunction(target.vec, a = a, b = b)
  }
  if (type == "Proportion") {
    tran.vec <- propFunction(target.vec, a = a, b = b)
  }
  if (type == "Counted Fraction") {
    tran.vec <- countFracFunction(target.vec, b = b)
    dens.flag <- FALSE
  }
  if (type == "Bounded Amounts") {
    tran.vec <- boundedAmountFunc(target.vec, a = a, b = b)
  }
  if (type %in% c("Bounded Counts", "Bounded counts")) {
    tran.vec <- boundedCountFunc(target.vec, a = a, b = b)
    dens.flag <- FALSE
  }
  if (type == "Ranks") {
    tran.vec <- countFracFunction(target.vec, a = a, b = b)
    dens.flag <- FALSE
    
  }
  if (type == "Ordered Categories") {
    tran.vec  <- orderedCatFunc(target.vec)
    dens.flag <- FALSE
  }
  if (type %in% c("Binary (categories)" ,"Category", "Difference")) { 
    tran.vec  <- NULL 
    dens.flag <- FALSE
  }  
  if (type == "To Be Determined") {
    return("Cant Plot")
  }
  if (to.reverse == TRUE) {
    tran.vec <- c(tran.vec, "to.reverse")
  }
  ## Arranging Data 
  transform.list <- transformList(target.vec = target.vec, 
                                  transform.vec = tran.vec,
                                  a = a, 
                                  b = b)
  ## Calculating Index 
  index.list     <- createIndex(transform.list, index = index.type)
  ## Sorting 
  transform.list <- transform.list[index.list[['Order']]]
  ## Plotting 
  plot.list     <- plotTransform(transform.list = transform.list, 
                                 var.name = var.name,
                                 bin.width.plot = bin.width,
                                 window.size.plot = window.size,
                                 KDE = dens.flag, 
                                 ind.vec = index.list[['Index']],
                                 ind.name = index.list[['Name']] ) 
  return(list('Transformations' = transform.list, 'Plots' = plot.list, 'Index' = index.list))
}



###########################

###########################
# Guessing Functions  -----------------------------------------------------


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
                          'To.Reverse' = rep(0 ,p))  
  for (i in 1:p) { 
    guess.dat[i,'Variable'] <- name.vec[i] 
    guess.dat[i,c(2:4)]     <- GuessType(file[ ,i])
  }
  return(guess.dat) 
}

###########################

###########################
# Checking  ---------------------------------------------------------------
# wrapTypes(target.vec = rbinom(100, 1,0.5), type = "Binary (categories)", bin.width = 0.1,var.name = 'Tzvikush', to.reverse = TRUE)
# wrapTypes(target.vec = rnorm(100, 5,0.5), type = "Amounts", var.name = 'Tzvikush', bin.width = 1, window.size = 1)
# wrapTypes(target.vec = rpois(100, 5), type = "Counts", var.name = 'Tzvikush')
# wrapTypes(target.vec = rpois(100, 5) / 10, type = "Ratio", var.name = 'Tzvikush')
# 
# ## Read Data
# dat <- read.csv('merged_db_CM.csv')
# 
# wrapTypes(target.vec = dat$attention_mmse, type = "Bounded counts", bin.width = 1,var.name = 'Tzvikush', to.reverse = F, b =5, a=0)
# wrapTypes(target.vec = dat$MML, type = "Binary (categories)", bin.width = 1,var.name = 'Tzvikush', to.reverse = F, b =1, a=0)
# wrapTypes(target.vec = dat$MMTRIALS, type = "Binary (categories)", bin.width = 1,var.name = 'Tzvikush', to.reverse = F, b =1, a=0)
# wrapTypes(target.vec = dat$NPITOTAL, type = "Amounts")

###########################