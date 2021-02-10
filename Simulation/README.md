The folder contains necessary code to replicate the result from simulation code. The simulation was run on the [UAB Cheaha Supercomputer](https://www.uab.edu/it/home/research-computing/cheaha) with R version 3.6.0.

## Files:
* `start_sim_express.R`: The R script file initiate the simulation. It will call the job scheduler to schedule a array of job, where each job is one iteration of the simulation. Simply run `R CMD BATCH start_sim_express.R` to start the simulation
* `MOTE_sim_express.job`: The _Slurm_ job scheduling script. 
* `Code/Main.R`: The R script file for each iteration of simulation
* `Code/parse_result.R`: The R script file for summarizing and reporting simulation results
* `clean`: A system script to quickly clean up the log files and simulation results