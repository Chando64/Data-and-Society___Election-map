library(patchwork)
library(redist)
library(dplyr)


# simulate 500 plans using the SMC algorithm
constr <- redist_constr(map_va) |>
  add_constr_compet(strength = 4, rvote = nrv, dvote = ndv, pow = 0.5) |>
  add_constr_pop_dev(strength =3)

va_plans = redist_smc(map_va, nsims=500, compactness = 5, constraints = constr)
redist.plot.plans(va_plans, 
                  draws=c( "1", "2", "3", "4", "5", "6"), 
                  shp=map_va)





