power.binom.test = function(n, p0, pA, alpha = 0.05,
                            alternative = c("two.sided", "less", "greater")){
  alternative <- match.arg(alternative)

  # USE SAME ERROR CHECKS AS binom.test
  invisible(binom.test(n, n, p0, alternative = alternative, conf.level = 1 - alpha))
  invisible(binom.test(n, n, pA, alternative = alternative))

  q = .find_crit(n, p0, alpha, alternative)

  # temp test
  for(i in q) stopifnot(binom.test(i, n, p0,
                                   alternative = alternative,
                                   conf.level = 1 - alpha)$p.value < alpha)

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

.calc_lower_crit = function(n, p, alpha){
  if(pbinom(0, n, p) > alpha) return(-1)
  max(which(pbinom(0:n, n, p) < alpha)) - 1
}

.calc_upper_crit = function(n, p, alpha){
  if(pbinom(n - 1, n, p, lower.tail = FALSE) > alpha) return(n+1)
  min(which(pbinom((1:n)-1, n, p, lower.tail = FALSE)< alpha))
}


x = rbinom(1000, 100, 0.6)
purrr::map_dbl(x, ~binom.test(., 100, 0.5, alternative = "greater")$p.value < 0.05) |> mean()


library(tidyverse)
pwr = map_df(7:15, function(i){
  map_df(seq(0.65, 1, by = 0.05), function(j){
    tibble(
      n = i,
      pa = j,
      power_barnard = Exact::power.exact.test(p1 = j, p2 = 0.6, n1 = i, n2 = i,
                                              alternative = "greater")$power,
      power_binom =  power.binom.test(n = i, pA = j, p0 = 0.6, alternative = "greater")
    )

  })
})

pwr %>%
  gather(method, power, power_barnard, power_binom) %>%
  ggplot(aes(x = pa, y = power, colour = method)) +
  geom_point()+
  geom_line() +
  facet_wrap(~n)
