library(dplyr)

sim_prmt <- expand.grid(
  p = c(10, 20),
  q = c(3, 6),
  n_train = c(400, 800),
  n_test = c(1000),
  sigma = c("ind", "AR"),
  trt.f = c(1,2),
  link.f = c(1,2)
) %>%
  right_join(data.frame(p=c(10,20),q=c(3,6))) %>% 
  data.frame()

# A wrapper function to set up each job
start.sim <- function(
  p, q,
  n_train, n_test,
  sigma,
  trt.f, link.f
) {
  # names
  ntrain.name <- paste0("ntr", n_train)
  ntest.name <- paste0("nte", n_test)
  p.name <- paste0("p", p)
  q.name <- paste0("q", q)
  sig.name <- paste0(sigma)
  trt.f.name <- paste0("trt", ifelse(trt.f==1, "Lnr", "Poly"))
  link.f.name <- paste0("lnk", ifelse(trt.f==1, "Lnr", "Poly"))
  
  job.name <- paste(p.name,
                    q.name,
                    ntrain.name,
                    ntest.name,
                    sig.name,
                    trt.f.name,
                    link.f.name, sep="_")
  
  job.flag <- paste0("--job-name=",job.name)
  
  err.flag <- paste0("--error=",job.name,".err")
  
  out.flag <- paste0("--output=",job.name,".out")
  
  arg.flag <- paste0("--export=ntrain=", n_train, ",",
                     "ntest=", n_test, ",",
                     "p=", p, ",",
                     "q=", q, ",",
                     "sigma=", sigma,",",
                     "trt", trt.f, ",",
                     "lnk", link.f
  )
  
  system(
    paste("sbatch", job.flag, err.flag, out.flag, arg.flag,"MOTTE_sim.job")
  )
}


for(i in 1:NROW(sim_prmt)){
  #  i <- 1
  do.call(start.sim, sim_prmt[i,])
}