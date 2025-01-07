# Make progress bar 
# code from Isaac Eckert
set.prog.bar<-function(n_iter){
  #make progress bar
  progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                   total = n_iter,
                   complete = "=",
                   incomplete = "-",
                   current = ">",
                   clear = FALSE,
                   width = 100)
  
} #function to make a progress bar



