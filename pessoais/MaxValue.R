## The function MaxValue builds groups of means, according to the method of
## Scott & Knott.
## Basically it is an algorithm for pre-order path in binary decision tree.
## Every node of this tree, represents a different group of means
## and, when the algorithm reaches this node it takes the decision to either
## split the group in two, or form a group of means.
## If the decision is to divide then this node generates two children and the
## algorithm follows for the node on the left, if, on the other hand, the
## decision is to form a group, then it returns to the parent node of that
## node and follows to the right node.
## In this way it follows until the last group is formed, the one containing
## the highest (or the least) mean. In the case the highest (or the least)
## mean it becomes itself a group of one element, the algorithm continues to
## the former group.
## In the end, each node without children represents a group of means.

MaxValue <- function(g,
                     means,
                     mMSE,
                     r,#### ESSE VALOR DEVE SER IMPLEMENTADO NO MAXVLUE DO SCOTTKNOT. É O NÚMERO DE REPETIÇÕES!!!
                     dfr,
                     sig.level,
                     k,
                     group,
                     ngroup,
                     markg,
                     g1=g,
                     sqsum=rep(0, g1))
{
  for(k1 in k:(g-1)) {
    t1 <- sum(means[k:k1])

    k2 <- g-k1

    t2 <- sum(means[(k1+1):g])

    # SK between groups sum of squares
    sqsum[k1] <- t1^2/(k1-k+1) + t2^2/k2 - (t1+t2)^2/(g-k+1)
  }

  # the first element of this vector is the value of k1 which maximizes sqsum (SKBSQS)
  ord1 <- order(sqsum, decreasing=TRUE)[1]
  
  # the maximum value of the between groups sum of squares
  b0   <- max(sqsum)
  
  si02 <- (1 / ((g - k + 1) + dfr)) * (sum((means[k:g] - mean(means[k:g]))^2) + dfr * mMSE/r)
  
  lam  <- (pi / (2 * (pi - 2))) * b0 / si02
  
  valchisq <- qchisq((sig.level),
                     lower.tail=FALSE,
                     df=(g - k + 1) / (pi - 2))
  # if true it returns one node to the right if false it goes forward one node to the left
  if((lam < valchisq) |
     (ord1 == k)) {
    # In the case of a single average left (maximum)
    if(lam > valchisq) {
      # it marks the group to the left consisting of a single mean
      ngroup <- ngroup + 1

      # it forms a group of just one mean (the maximum of the group)
      group[k] <- ngroup

      # lower limit on returning to the right
      k <- ord1 + 1
    }
    if(lam < valchisq) {
      # it marks the groups
      ngroup <- ngroup + 1

      # it forms a group of means
      group[k:g] <- ngroup

      # if this group is the last one
      if (prod(group) > 0)
        # If the upper limit of the latter group formed is equal to the total
        # number of treatments than  the grouping algorithm is ended
        return (group)

      # it marks the lower limit of the group of means to be used in the
      # calculation of the maximum sqsum on returning one node to the right
      k <- g + 1

      # it marks the upper limit of the group of means to be used in the
      # calculation of the maximum sqsum on returning one node to the right
      g <- markg[g]
    }
    while(k == g) {
      # there was just one mean left to the right, so it becomes a group
      ngroup   <- ngroup + 1

      group[g] <- ngroup

      if(prod(group) > 0)
        # If the upper limit of the latter group formed is equal to the total
        # number of treatments than  the grouping algorithm is ended
        return(group)

      # the group of just one mean group had already been formed, a further
      # jump to the right and another check whether there was just one mean
      # left to the right
      k <- g + 1

      g <- markg[g]
    }
  } else {
    # it marks the upper limit of the group split into two to be used on
    # returning to the right later
    markg[ord1] <- g

    g <- ord1
  }
  
  MaxValue(g,
           means,
           mMSE,
           r,
           dfr,
           sig.level,
           k,
           group,
           ngroup,
           markg)
}
