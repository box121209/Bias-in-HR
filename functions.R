#############################################################################
# simulation functions & parameters

n.groups <- 2
n.expts <- 20

table.count <- function(arr, n){
  # utility that returns array of counts including 0 
  # where values (up to n) are missing
  v <- rep(0, n)
  tab <- table(arr)
  for(x in dimnames(tab)) v[as.numeric(x)] <- tab[x]
  # return:
  v
}

reduce <- function(staff, 
                   n.levels,
                   loss.rate){
  
  staff.reduced <- list()
  size <- sapply(staff, length)
  for(i in 1:n.levels){
    keep <- sample(c(TRUE,FALSE), size[i], prob=c(1-loss.rate[i], loss.rate[i]), replace=TRUE)
    staff.reduced[[i]] <- staff[[i]][keep]
  }
  # return:
  staff.reduced
}

find.movers <- function(staff, 
                        n.levels,
                        n.move, 
                        eval.mean,
                        eval.sd,
                        eval.upper,
                        eval.lower,
                        uniform=FALSE){
  # n.move should be a vector of length n.levels
  # = how many vacancies at each level
  
  size <- sapply(staff, length)
  candidates <- list()
  # new entrants:
  candidates[[1]] <- sample(n.groups, size[1], replace=TRUE)
  # higher levels:
  if(n.levels > 1)
    for(i in 2:n.levels) candidates[[i]] <- staff[[i-1]]
  
  promotions <- list()
  for(i in 1:n.levels){
    
    if(n.move[i] == 0){ promotions[[i]] <- NA } else {
      # count classes at this level:
      v <- table.count(candidates[[i]], n.groups)
      
      # draw scores from the respective distributions:
      scores <- lapply(1:n.groups, function(g) 
        if(v[g] > 0){
          rbind(rep(g, v[g]),
                if(uniform){
                  runif(v[g], min=eval.lower[g], max=eval.upper[g])
                } else {
                  rnorm(v[g], mean=eval.mean[g], sd=eval.sd[g])
                })
        } else { NULL })
      scores <- do.call(cbind, scores)
      
      # sort and identify the winners:
      n <- size[i]
      promotions[[i]] <- scores[1, order(scores[2,])][(n - n.move[i] + 1):n]
    }
  }
  # return:
  promotions
}

experiment <- function(staff, 
                       n.steps, 
                       n.levels,
                       loss.rate, 
                       eval.mean,
                       eval.sd,
                       eval.upper,
                       eval.lower,
                       uniform=FALSE){
  staff.current <- staff
  
  if(n.steps > 0){
    for(time in 1:n.steps){
      staff.reduced <- reduce(staff.current, n.levels, loss.rate)
      n.staff.current <- sapply(staff.current, length)
      n.staff.reduced <- sapply(staff.reduced, length)
      n.move <- n.staff.current - n.staff.reduced
      movers <- find.movers(staff.current, 
                            n.levels,
                            n.move, 
                            eval.mean,
                            eval.sd,
                            eval.upper,
                            eval.lower,
                            uniform=uniform)
      staff.current <- lapply(1:n.levels, function(i){
          tmp <- c(staff.reduced[[i]], movers[[i]]) 
          # return:
          tmp[!is.na(tmp)]
        })
    }  
  }
  
  # return final count:
  do.call(rbind, lapply(staff.current, function(s) table.count(s, n.groups)))
}

average <- function(expt.set, n.levels){
  n.expts <- length(expt.set)
  profile <- matrix(0, nrow=n.levels, ncol=n.groups)
  for(i in 1:n.expts) profile <- profile + expt.set[[i]]
  # return:
  profile/n.expts
}


#############################################################################
# plotting functions

addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) 
    stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

col <- c(addTrans('red', 150), addTrans('blue', 150))

show.profile <- function(expt.set, n.levels){
  profile <- t(average(expt.set, n.levels))
  ymax <- max(profile)+10
  bp <- barplot(profile, 
                beside=TRUE, col=col,
                names.arg=1:n.levels,
                ylim=c(0,ymax),
                xlab='Job grade',
                ylab='Staff %',
                main="Gender profile by grade")
  text(bp, profile, labels=format(profile, digits=3),
       font=2, pos=3, cex=0.9)
}

show.normal.scores <- function(eval.mean, eval.sd){
  # plot the relative score distributions:
  xlim <- c(min(eval.mean-3*eval.sd),
            max(eval.mean+3*eval.sd))
  curve(dnorm(x, mean=eval.mean[1], sd=eval.sd[1]),
        xlim=xlim, 
        col=col[1], lwd=2,
        xlab='', ylab='',
        main="Distribution of appraisals by gender",
        frame.plot=0, yaxt='n')
  curve(dnorm(x, mean=eval.mean[2], sd=eval.sd[2]),
        col=col[2], lwd=2, add=TRUE)
}

show.unif.scores <- function(eval.lower, eval.upper){
  # plot the relative score distributions:
  xlim <- c(min(eval.lower)-1,
            max(eval.upper)+1)
  y.mx <- max(1/(eval.upper - eval.lower))
  y.mx <- 1.01 * y.mx
  ylim <- c(0, y.mx)
  curve(dunif(x, min=eval.lower[1], max=eval.upper[1]),
        xlim=xlim, ylim=ylim,
        col=col[1], lwd=2,
        xlab='', ylab='',
        main="Distribution of appraisals by gender",
        frame.plot=0, yaxt='n')
  curve(dunif(x, min=eval.lower[2], max=eval.upper[2]),
        col=col[2], lwd=2, add=TRUE)
}
