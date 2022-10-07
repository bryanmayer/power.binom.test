#' Calculates the power for one-sample exact binomial test given null and alternative probabilities
#'
#' @param n number of trials
#' @param p0 null probability of success
#' @param pA probability of success in alternative group
#' @param alpha false positive rate or 1-confidence level
#' @param alternative indicates the alternative hypothesis and must be one of "two.sided", "greater" or "less". You can specify just the initial letter.
#'
#' @return
#' @export
#'
#' @examples
#'
#' power.binom.test(10, 0.5, 0.7)
#' power.binom.test(10, 0.5, 0.25, alternative = "less")
#' power.binom.test(15, 0.5, 0.7, alpha = 0.1, alternative = "greater")
#'
power.binom.test = function(n, p0, pA, alpha = 0.05,
                            alternative = c("two.sided", "less", "greater")){
  alternative <- match.arg(alternative)

  # USE SAME ERROR CHECKS AS binom.test
  invisible(binom.test(n, n, p0, alternative = alternative, conf.level = 1 - alpha))
  invisible(binom.test(n, n, pA, alternative = alternative))

  q = .find_crit(n, p0, alpha, alternative)

  switch(alternative,
         less = pbinom(q[1], size = n, prob = pA),
         greater = pbinom(q[1]-1, size = n, prob = pA, lower.tail = F),
         two.sided = pbinom(q[1], size = n, prob = pA) +
           pbinom(q[2]-1, size = n, prob = pA, lower.tail = F)
  )

}

.find_crit = function(n, p, alpha, alternative){

  switch(alternative,
            less = .calc_lower_crit(n, p, alpha),
            greater = .calc_upper_crit(n, p, alpha),
            two.sided = c(.calc_lower_crit(n, p, alpha/2),
                          .calc_upper_crit(n, p, alpha/2))
  )
}

# there are more efficient ways to do this using pbinom and alpha directly
.calc_lower_crit = function(n, p, alpha){
  if(pbinom(0, n, p) > alpha) return(-1)
  max(which(pbinom(0:n, n, p) < alpha)) - 1
}

# there are more efficient ways to do this using pbinom and alpha directly
.calc_upper_crit = function(n, p, alpha){
  if(pbinom(n - 1, n, p, lower.tail = FALSE) > alpha) return(n+1)
  min(which(pbinom((1:n)-1, n, p, lower.tail = FALSE)< alpha))
}
