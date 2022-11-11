#' Function I want the user to see
#'
#' A description
#'
#' @param X a parameter
#' @return the same thing
#' @export
get_sample_from <- function(nsample, Q, rankdef = 1) {
  e <- eigen(Q, symmetric = TRUE)
  n <- dim(Q)[1]

  x <- matrix(0, nsample, n)
  for(j in 1:nsample) {
    ## this one you can rewrite in matrix form so its faster
    for(i in 1:(n-rankdef)) {
      x[j, ] <- x[j, ] + rnorm(1) * sqrt(1/e$values[i]) * e$vector[, i]
    }
  }
  return(x)
}

#' Function I want the user to see
#'
#' A description
#'
#' @param X a parameter
#' @return the same thing
#' @export
get_random_besag_graph <- function(n){
  while(TRUE)
  {
    p <- 0.3
    A <- matrix(0, n, n)
    A[] <- rbinom(n^2, size = 1, prob = p)
    Q <- A %*% t(A)
    Q[Q != 0] <- -1
    diag(Q) <- 0
    diag(Q) <- -rowSums(Q)
    g <- inla.read.graph(Q)
    ## until number of connected components is 1
    if (g$cc$n == 1) break
  }

  return(Q)
}

fix_paths <- function(path){

  setwd(path)
  #dir.create("pc-priors")

  folder <- "Data"
  folder_res <- "Results"
  if (!file.exists(folder)) dir.create(folder)
  if (!file.exists(folder_res)) dir.create(folder_res)

  MYPATH1 <- paste0(path,"/",folder_res, collapse=NULL)
  setwd(MYPATH1)
  if (!file.exists("GA")) dir.create("GA")
  if (!file.exists("HP")) dir.create("HP")

  MYPATH2 <- paste0(path,"/",folder, collapse=NULL)
  setwd(MYPATH2)
  return(MYPATH2)
}

inla.interpret.formula = function (
    gf,
    debug=FALSE,
    data.same.len = NULL,
    data=NULL,
    data.model = NULL,
    parent.frame = NULL,
    int.f)
{
  check.intercept <-function(gf) {if(all(grepl(" 1 ", trimws(gf), fixed = TRUE)==FALSE) && all(grepl("+ 1 ", trimws(gf), fixed = TRUE)==FALSE) && all(grepl("-1 ", trimws(gf), fixed = TRUE)==FALSE) &&
                                     all(grepl("+ 1 +", trimws(gf), fixed = TRUE)==FALSE) && all(grepl("~ 1", trimws(gf), fixed = TRUE)==FALSE) && all(grepl("~ 1 ", trimws(gf), fixed = TRUE)==FALSE)){return(TRUE)}else{return(FALSE)}}

  if(check.intercept(gf)==FALSE) stop("\n\tIntercept is added only using blocks")

  ## use a copy if data.model is !NULL, as we have to assign an
  ## entry to it. otherwise, just use the
  if (!is.null(data.model)) {
    ## make a copy of the environment
    p.env = new.env(hash = TRUE, parent = environment(gf))
    ## then add the entries in data.model to that environment
    for(nm in names(data.model)) {
      idx = which(names(data.model) == nm)
      assign(nm, data.model[[idx]], envir = p.env)
    }
  } else {
    ## otherwise, just use (for read-only) this environment
    if (missing(parent.frame)) {
      p.env = environment(gf)
    } else {
      p.env = parent.frame
    }
  }

  #rules
  s1 <-  terms.formula(gf, specials = c("f"), data=NULL)
  #----->  rule 1
  if(length(attr(s1, "specials")$f)>0) stop("\n\tUse block not f")

  #Get the formula again
  formula1 = gsub("block", "f", gf)
  formula <- as.formula(paste(formula1[2],formula1[1],formula1[3], sep="", collapse=NULL))
  gf <- formula
  s1 <-  terms.formula(gf, specials = c("f"), data=NULL)

  #----->  rule 2
  s2 <- length(attr(s1,"specials")$f)
  s2 <- s2*s2 + s2
  if (length(attr(s1,"factors")) != s2) stop("\n\tYou can only add blocks to your formula")

  terms <- attr(s1, "term.labels")
  nt <- length(terms)

  #to fix the indices of f(.) from 2 3 ... to 1 2 ...
  rt <- attr(s1, "specials")$f
  vtab <- attr(s1, "factors")
  if (length(rt) > 0) {
    for (i in 1:length(rt)) {
      ind <- (1:nt)[as.logical(vtab[rt[i], ])]
      rt[i] <- ind
    }
  }

  ####
  k =  ks = kp = 1
  len.rt = length(rt)
  random.spec = list()
  if (nt>0) {
    for (i in 1:nt) {
      if (k <= len.rt && ((ks <= len.rt && rt[ks] == i))) {
        st = eval(parse(text = gsub("^f\\(","int.f(", terms[i])))#, envir = data, enclos = p.env)
        random.spec[[k]] = st
        if (ks <= len.rt && rt[ks] == i) {
          ks = ks + 1
        } else {
          kt = kt + 1
        }
        k = k + 1
      } else {
        if (kp > 1) {
          fixf = paste(fixf, " + ", terms[i], sep = "")
        } else {
          fixf = paste(fixf, terms[i], sep = "")
        }
        kp = kp + 1
      }
    }
  }

  return(random.spec)
}

fix.formula <- function(blocks,MYPATH)
{
  setwd(MYPATH)

  nblocks <- length(blocks)
  blocks$ind_intercept <- 0
  blocks$ind_covariate <- 0
  blocks$num_effs <- 0

  K <- 0; s <- 0 ; ID_s <- 0; check_dim <- 0; check_s <- FALSE;
  all_types = c()
  for(i in 1:nblocks) {
    if(is.null(blocks[[i]]$type)) stop("\n\ttype of model shouldn't be empty")
    all_types = c(all_types,blocks[[i]]$type)
    if(!(blocks[[i]]$type %in% c("intercept","covariate","iid_time","iid_space", "RW1","RW2","besag","ICAR","generic_time","generic_space","type1","type2","type3","type4","interaction1","interaction2","interaction3","interaction4"))) {
      #print(all_types[i])
      stop("\n\tAt least one of the block types is not implemented")}
  }

  if(length(grep("intercept", all_types))>1) stop("\n\tOnly one block for intercept can be added")
  if(length(grep("covariate", all_types))>1) stop("\n\tOnly one block for covariate(s) can be added")

  for(i in 1:nblocks)
  {
    if(blocks[[i]]$type=="intercept"){

      INTER = blocks$ind_intercept = 1
      s <- s + 1

      if(!all(lapply(names(blocks[[i]]), function(x) x %in% c("type","Q","prior"))==TRUE)==TRUE) stop("\n\tError: Parameters for intercept block are: type, Q and prior")
      if(is.null(blocks[[i]]$Q)){
        Q = matrix(1e-5)
        blocks[[i]]$Q = Q
      }else {if(dim(blocks[[i]]$Q)[1]>1 || dim(blocks[[i]]$Q)[2]>1) stop("\n\tAtDimension of Q in intercept should be 1x1")}

      prec_mu = blocks[[i]]$Q
      write.table(prec_mu, file = "prec_mu.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
      write.table(INTER, file = "x_mu.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

    }else if(blocks[[i]]$type=="covariate"){

      blocks$ind_covariate = 1
      s = s + 1
      if(is.null(blocks[[i]]$Z)) stop("\n\tZ covariate matrix should be added to covariate block")
      if(!all(lapply(names(blocks[[i]]), function(x) x %in% c("type","Q","Z","prior"))==TRUE)==TRUE) stop("\n\tError: Parameters for covariate block are: type, Q and Z")
      if(is.null(blocks[[i]]$Q)){
        K = dim(blocks[[i]]$Z)[2]
        blocks[[i]]$Q = 0.001*diag(K)
        check_dim = dim(blocks[[i]]$Z)[1]
      }

      MAT = blocks[[i]]$Q
      MAT[upper.tri(MAT, diag = FALSE)] <- ""
      write.table(MAT, file = "prec_beta.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
      MAT = blocks[[i]]$Z
      write.table(MAT, file = "z_covariate.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

    }else{

      if(!(blocks[[i]]$type %in% c("type1","type2","type3","type4","interaction1","interaction2","interaction3","interaction4"))==TRUE ){
        # print(names(blocks[[i]]))
        # print(blocks[[i]]$type)
        if(!all(lapply(names(blocks[[i]]), function(x) x %in% c("type","rankdef","Q","prior","id"))==TRUE)==TRUE){
          #print(names(blocks[[i]]))
          stop("\n\tError: Parameters for random-effect block are: type,rankdef,Q and size")
        }
        if(is.null(blocks[[i]]$type)) stop("\n\tIn blocks: at least one of the types is empty")
        if(is.null(blocks[[i]]$rankdef)) blocks[[i]]$rankdef = 0
        if(is.null(blocks[[i]]$Q)) stop("\n\tIn blocks: at least one of the Q(s) of the main effects is empty")
        if(is.null(blocks[[i]]$size)) blocks[[i]]$size = dim(blocks[[i]]$Q)[1]
      }

      if(!check_s) {
        ID_s = length(blocks[[i]]$id); s = s + ID_s
      }else{if(length(blocks[[i]]$id!=ID_s)) stop("\n\tRandom Effect blocks doesn't have the same length of id") }
      blocks$num_effs = blocks$num_effs + 1

    }

    NB = blocks$num_blocks = blocks$num_effs + blocks$ind_intercept + blocks$ind_covariate
    write.table(NB, file = "num_blocks.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  }

  blocks$s <- s
  blocks$K <- K

  if(blocks$ind_covariate==1) if(check_dim!=ID_s) stop("\n\tWrong dimension of covariate matrix Z")
  write.table(K, file = "Cov_Z.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  n_time <- 0
  m_space <- 0
  for(i in 1:nblocks)
  {
    if((blocks[[i]]$type %in% c("iid_time","RW1","RW2","generic_time"))==TRUE ){
      if(!is.null(blocks[[i]]$size)){
        n_time = blocks[[i]]$size
        break
      }
    }
  }

  for(i in 1:nblocks){
    if((blocks[[i]]$type %in% c("iid_space","besag","generic_space","ICAR"))==TRUE ){
      if(!is.null(blocks[[i]]$size)){
        m_space <- blocks[[i]]$size
        break}}}

  if(n_time==0) print("\n\tPlease check time size")
  if(m_space==0) print("\n\tPlease check space size")

  for(i in 1:nblocks){

    if(blocks[[i]]$type=="iid_time"){
      blocks[[i]]$size <- n_time
    } else if(blocks[[i]]$type=="iid_space"){
      blocks[[i]]$size <- m_space
    }

    if(!blocks[[i]]$type=="intercept" && !blocks[[i]]$type=="covariate"){
      if((blocks[[i]]$type %in% c("iid_space","besag","generic_space","ICAR"))){
        blocks[[i]]$id <- rep(1:m_space, n_time)
      }else if((blocks[[i]]$type %in% c("iid_time","RW1","RW2","generic_time"))==TRUE ){
        blocks[[i]]$id <- rep(1:n_time, each=m_space)
      }else if((blocks[[i]]$type %in% c("type1","type2","type3","type4","interaction1","interaction2","interaction3","interaction4"))==TRUE ){
        blocks[[i]]$id <- 1:(n_time*m_space)
        blocks[[i]]$size <- n_time*m_space}}}
  return(blocks)
}

fix.Qx <- function(blocks,MYPATH,Model,control_opt)
{
  setwd(MYPATH)
  num_effs <- 0; theta_size <- 0; a <- c(); sizes <- c(); id_effi <- c();
  RD_effi <- c(); RD <- c(); effi <- c();
  priors_p1 <- c(); priors_p2 <- c(); priors_types <- c();
  mytail <- 0; prior_blocks <- c(); c <- 1; iter = 1

  iter <- blocks$ind_intercept + blocks$K
  nblocks <- blocks$num_blocks
  c <- 1

  for(i in 1:nblocks)
  {
    if(!(blocks[[i]]$type=="covariate" || blocks[[i]]$type=="intercept"))
    {
      RD_effi[c] <- blocks[[i]]$type
      sizes[c] <- blocks[[i]]$size
      RD[c] <- blocks[[i]]$rankdef
      copy_id <- blocks[[i]]$id

      if(c>1){copy_id <- copy_id + rep(tail(id_effi, n=1),length(blocks[[i]]$id))
      }else{copy_id <- copy_id + rep(iter-1,length(blocks[[i]]$id))}
      #print(copy_id)

      id_effi <- c(id_effi,copy_id)
      name_eff <- paste("eff", toString(c), sep="")
      effi <- c(effi,name_eff)
      c <- c + 1

      ##Priors
      if(is.null(blocks[[i]]$prior) && control_opt$prior=="default") {
        priors_p1 <- c(priors_p1,0)
        priors_p2 <- c(priors_p2,0)
        priors_types <- c(priors_types,"pc.joint")
        prior_blocks <- c(prior_blocks,1)
        control_opt$prior <- "pc.joint"
      }else if(is.null(blocks[[i]]$prior) && control_opt$prior=="pc.joint"){
        priors_p1 <- c(priors_p1,0)
        priors_p2 <- c(priors_p2,0)
        priors_types <- c(priors_types,"pc.joint")
        prior_blocks <- c(prior_blocks,1)
      }else{
        priors_p1 <- c(priors_p1,blocks[[i]]$prior$par1[1])
        priors_p2 <- c(priors_p2,blocks[[i]]$prior$par1[2])
        priors_types <- c(priors_types,blocks[[i]]$prior$type1)
        prior_blocks <- c(prior_blocks,1)
        #print(priors_types)
      }

      #Qx
      if(blocks[[i]]$type %in% c("iid_time","iid_space", "RW1","RW2","besag","ICAR","generic_time","generic_space")){
        Qx <- blocks[[i]]$Q
        Qx <- as.matrix(Qx)
        Qx[upper.tri(Qx, diag = FALSE)] <- ""
        name_eff <- paste("Qxeff", toString(c-1), sep="")
        name_eff <- paste(name_eff, ".txt", sep="")
        write.table(Qx, file = name_eff,quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
      }
      num_effs <- num_effs + 1
    }

  }

  theta_size_Qx <- length(effi)
  write.table(theta_size_Qx, file = "theta_size_Qx.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  if(Model$like=="Gaussian") if(Model$Gaussian_Noise=="R_Gaussian_Noise"){
    if(is.null(Model$prior)){
      prior <- list(par1 = c(1.0,5e-05),type1="loggama.prec")
      Model$prior <- prior
    }
    priors_p1 <- c(priors_p1,Model$prior$par1[1])
    priors_p2 <- c(priors_p2,Model$prior$par1[2])
    priors_types <- c(priors_types,Model$prior$type1)
  }

  table_priors <- table(priors_types)
  if(length(unname(table_priors[names(table_priors)=="NoPrior"]))==0){
    theta_size <- length(priors_types)
  } else{theta_size <- length(priors_types) - unname(table_priors[names(table_priors)=="NoPrior"])}

  x_size <- tail(id_effi, n=1) + 1
  size_priors_types = length(priors_types)
  pc = control_opt$prior
  if(is.null(pc)) pc = "default"

  write.table(theta_size, file = "theta_size.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(num_effs, file = "num_effs.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(id_effi, file = "id_effi.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(sizes, file = "sizes.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(RD_effi, file = "RD_effi.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(x_size, file = "x_size.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(RD, file = "RD.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(effi, file = "effi.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(priors_types, file = "priors_types.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(priors_p1, file = "priors_p1.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(priors_p2, file = "priors_p2.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(prior_blocks, file = "prior_blocks.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(size_priors_types, file = "size_priors_types.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(pc, file = "prior.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  if(is.null(control_opt$theta)) {control_opt$theta = c(); control_opt$theta[1]=-999}
  init <- control_opt$theta

  if(init[1]==-999) {
    init = rep(4,size_priors_types)
    write.table(init, file = "initial.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  } else {
    #FIX THIS
    write.table(init, file = "initial.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  }

  return(list(x_size,theta_size))
}

int.f <- function(
    type = NULL,#RD_eff
    Q = NULL,#Qx
    rankdef = NULL,
    prior = NULL,
    Z = NULL){

  if(type=="covariate")  return(list(type=type, Q= Q, Z = Z, prior=prior))
  if(type=="intercept") return(list(type=type, Q=Q, prior=prior))
  if(type %in% c("type1","type2","type3","type4","interaction1","interaction2","interaction3","interaction4")){
    if(!is.null(Q)) stop("\n\tThe precision matrix for the interaction term will be set automatically")
    return(list(type=type, rankdef=rankdef, prior=prior))
  }else if(type %in% c("iid_time","iid_space")){
    if(is.null(rankdef)) print("\n\tRank deficiency for iid effects is set to zero")
    rankdef = 0
    return(list(type=type,Q=Q, rankdef=rankdef, prior=prior))
  }else{
    return(list(type=type, Q=Q, rankdef=rankdef, prior=prior))
  }
}

export_data <-function(control_strategy,control_opt,Qx_type,inputData,Model,MYPATH)
{
  setwd(MYPATH)

  #inputData
  y_response <- inputData$y_response
  y_size <- length(y_response)

  write.table(y_response, file = "y_response.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(y_size, file = "y_size.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  #Qx_type
  type <- Qx_type$type
  write.table(type, file = "Qx_type.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  #Model
  like <- Model$like
  write.table(like, file = "Model.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  if(like=="Gaussian")
  {
    Gaussian_Noise = Model$Gaussian_Noise
    write.table(Gaussian_Noise, file = "Gaussian_Noise.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  } else if(like=="Binomial")
  {
    Binomial_Size = Model$Binomial_Size
    write.table(Binomial_Size, file = "Binomial_Size.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
    Ntrials = Model$Ntrials
    write.table(Ntrials, file = "Ntrials.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  }else if(like=="Poisson"){

    if(is.null(Model$offset)) Model$offset = rep(1,y_size)
    offset = Model$offset
    write.table(offset, file = "Ntrials.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  }

  #control_opt
  max_linesearch <- control_opt$max_linesearch
  smartGrad <- as.numeric(control_opt$smartGrad)
  central <- as.numeric(control_opt$central)
  grad_stepsize <- control_opt$grad_stepsize
  num_threads <- control_opt$num_threads
  safemode <- as.numeric(control_opt$safemode)

  control_opt$safemode
  write.table(max_linesearch, file = "max_linesearch.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(smartGrad, file = "smartGrad.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(central, file = "central.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(grad_stepsize, file = "grad_stepsize.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(num_threads, file = "num_threads.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  write.table(safemode, file = "safemode.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  #Strategy
  s <- control_strategy$Strategy
  write.table(s, file = "strategy.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
  vbc = 0
  if(control_strategy$vbc) vbc = 1
  write.table(vbc, file = "vbc.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

}

.inla1234 <- function(
    formula,
    Model = list(like="Gaussian"),
    Qx_type = list(type="generic_Qx"),
    control_strategy = list(Strategy = "GA"),
    data = list(y_response=c()),
    control_opt = list(theta=c(-999)),
    password = list(pin=c(""),MYPATH = "~/Data"),
    control_parallel = list(num_omp=1,num_proc=1, sarus_resource = NULL))
{
  cat("Welcome to INLAPLUS Package!\n")
  #print(getwd())
  # if(password$pin=="165715") MYPATH = get_path(password$MYPATH)

  MYPATH_p <- system.file(package = "INLAPLUS")
  MYPATH <- fix_paths(MYPATH_p)
  print(MYPATH_p)

  #control_opt: control parameters for optimization
  if(is.null(control_opt$central)) {control_opt$central = FALSE}
  if(is.null(control_opt$grad_stepsize)) {control_opt$grad_stepsize = 0.005}
  if(is.null(control_opt$safemode)) {control_opt$safemode = FALSE}
  if(is.null(control_opt$smartGrad)) {control_opt$smartGrad = FALSE}
  if(is.null(control_opt$max_linesearch)) {control_opt$max_linesearch = 5}
  if(is.null(control_opt$num_threads)) {control_opt$num_threads = 6}
  if(is.null(control_opt$strategy)) {control_opt$strategy = "classic"}
  if(is.null(control_strategy$vbc)) {control_opt$vbc = FALSE}
  if(!is.null(control_strategy$prior) && control_strategy$prior=="pc.joint") {control_opt$prior = "pc.joint"
  }else if(!is.null(control_strategy$prior)) stop("\n\tPrior is not implemented")
  if(is.null(control_strategy$prior)) {control_opt$prior = "default"; control_strategy$prior = "default"}

  blocks = fix.formula(inla.interpret.formula(gf, int.f = int.f),MYPATH)
  print("Loading...")
  sizes = fix.Qx(blocks,MYPATH,Model,control_opt)
  print("Loading...")
  export_data(control_strategy,control_opt,Qx_type,data,Model,MYPATH)
  print("Loading...")

  setwd('..')

  if(FALSE){
    num_omp = control_parallel$num_omp
    num_proc = control_parallel$num_proc


    commands_sys <- c("export OMP_NUM_THREADS=", toString(num_omp), "; mpirun -np ", toString(num_proc), " --map-by socket:PE=${OMP_NUM_THREADS} ./output_server")
    mycall <- capture.output(cat(commands_sys, sep = ""))
    system(mycall)

    x_size = sizes[[1]]
    inla1234 = list()

    #library(foreach)
    #library(doParallel)
    #registerDoParallel(cores=48)

    #foreach(i=1:(x_size-1)) %dopar% {
    for(i in 1:(x_size-1)){
      s1 = paste("Results/GA/x",toString(i),".txt", sep="")
      s2 = paste("x",toString(i), sep="")
      x <- read.table(s1, quote="\"", comment.char="")$V1
      inla1234[[s2]] = x

      s1 = paste("Results/GA/y",toString(i),".txt", sep="")
      s2 = paste("y",toString(i), sep="")
      x <- read.table(s1, quote="\"", comment.char="")$V1
      inla1234[[s2]] = x
    }
    marg_post_x = inla1234
    system('rm -r Results/GA/*')

    inla1234 = list()
    theta_size = sizes[[2]]
    for(i in 1:theta_size){
      s1 = paste("Results/HP/x",toString(i),".txt", sep="")
      s2 = paste("x",toString(i), sep="")
      x <- read.table(s1, quote="\"", comment.char="")$V1
      inla1234[[s2]] = x

      s1 = paste("Results/HP/y",toString(i),".txt", sep="")
      s2 = paste("y",toString(i), sep="")
      x <- read.table(s1, quote="\"", comment.char="")$V1
      inla1234[[s2]] = x
    }

    marg_post_theta = inla1234
    system('rm -r Results/HP/*')
    return(list(marg_post_x = marg_post_x, marg_post_theta = marg_post_theta))

  }else {

    num_omp = control_parallel$num_omp
    num_proc = control_parallel$num_proc

    if(is.null(control_parallel$sarus_resource)) {

      commands_sys <- c("export OMP_NUM_THREADS=", toString(num_omp), " && mpirun -N ", toString(num_omp) ," -n ", toString(num_proc), " sarus run --mpi --workdir=", MYPATH_p, " esmailabdulfattah/inlaplus:251122 /software/inlacode/output_mpi_mkl")
      print("running")
      mycall <- capture.output(cat(commands_sys, sep = ""))
      print(mycall)
      system(mycall)

    }else{

      ss <- control_parallel$sarus_resource
      commands_sys <- c(toString(ss), " export OMP_NUM_THREADS=", toString(num_omp), " && mpirun -N ", toString(num_omp) ," -n ", toString(num_proc), " sarus run --mpi --workdir=", MYPATH_p, " esmailabdulfattah/inlaplus:251122 /software/inlacode/output_mpi_mkl")
      print("running")
      mycall <- capture.output(cat(commands_sys, sep = ""))
      print(mycall)
      system(mycall)

    }



    x_size = sizes[[1]]
    inla1234 = list()

    #library(foreach)
    #library(doParallel)
    #registerDoParallel(cores=48)

    #foreach(i=1:(x_size-1)) %dopar% {
    for(i in 1:(x_size-1)){
      s1 = paste("Results/GA/x",toString(i),".txt", sep="")
      s2 = paste("x",toString(i), sep="")
      x <- read.table(s1, quote="\"", comment.char="")$V1
      inla1234[[s2]] = x

      s1 = paste("Results/GA/y",toString(i),".txt", sep="")
      s2 = paste("y",toString(i), sep="")
      x <- read.table(s1, quote="\"", comment.char="")$V1
      inla1234[[s2]] = x
    }
    marg_post_x = inla1234
    system('rm -r Results/GA/*')

    inla1234 = list()
    theta_size = sizes[[2]]
    for(i in 1:theta_size){
      s1 = paste("Results/HP/x",toString(i),".txt", sep="")
      s2 = paste("x",toString(i), sep="")
      x <- read.table(s1, quote="\"", comment.char="")$V1
      inla1234[[s2]] = x

      s1 = paste("Results/HP/y",toString(i),".txt", sep="")
      s2 = paste("y",toString(i), sep="")
      x <- read.table(s1, quote="\"", comment.char="")$V1
      inla1234[[s2]] = x
    }

    marg_post_theta = inla1234
    system('rm -r Results/HP/*')
    return(list(marg_post_x = marg_post_x, marg_post_theta = marg_post_theta))




  }

}

#' inla1234 Function
#'
#' This function allows you fit approximate Bayesian inference for interaction types 1,2,3 and 4
#' @param formula it composed of the blocks that construct the precision matrix of the latent field
#' @keywords Model
#' @export
inla1234 <- function(
    formula,
    Model = list(),
    data = list(),
    control_opt = list(),...){

  .inla1234(formula,Model, data, control_opt, ...)

}
