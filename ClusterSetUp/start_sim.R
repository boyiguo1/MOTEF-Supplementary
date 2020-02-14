library(dplyr)

sim_prmt <- expand.grid(
  p = c(10, 20),
  q = c(3, 6),
  n_train = c(400, 800),
  n_test = c(1000),
  sigma = c("ind", "AR")
) %>%
  data.frame()

# A wrapper function to set up each job
start.sim <- function(
  p, q,
  n_train, n_test,
  sigma
) {
  # names
  ntrain.name <- paste0("ntr", n_train)
  ntest.name <- paste0("nte", n_test)
  p.name <- paste0("p", p)
  q.name <- paste0("q", q)
  sig.name <- paste0(sigma)
  
  job.name <- paste(p.name,
                    q.name,
                    ntrain.name,
                    ntest.name,
                    sig.name, sep="_")
  
  job.flag <- paste0("--job-name=",job.name)
  
  err.flag <- paste0("--error=",job.name,".err")
  
  out.flag <- paste0("--output=",job.name,".out")
  
  arg.flag <- paste0("--export=ntrain=", n_train, ",",
                     "ntest=", n_test, ",",
                     "p=", p, ",",
                     "q=", q, ",",
                     "sigma=", sigma
  )
  
  system(
    paste("sbatch", job.flag, err.flag, out.flag, arg.flag,"MOTTE_sim.job")
  )
}


for(i in 1:NROW(sim_prmt)){
  #  i <- 1
  do.call(start.sim, sim_prmt[i,])
}