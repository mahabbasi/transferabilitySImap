library(targets)
library(tarchetypes)

tar_source("R/used_libraries.R")
tar_source("R/modeling_functions.R")
tar_source("R/producing_figures.R")

root_dir <- here::here()
data_dir <- file.path(root_dir, "data")
data_dir_eu <- file.path(root_dir, "data/data_eu") # is excluded due to its huge size
results_dir <- file.path(root_dir, "results/01_train_val_approach")
# ------------------------- Modeling development- only European gauging stations (i.e., model variant I) --------------------------
plan_model_eu = tar_plan(
  tar_target(
    name = 'model_data_eu',
    command = {
      eu_data <- qs::qread(file = file.path(root_dir, 'data/model_data/train_set/train_data_eu.qs'))
      
      eu_data[, dates :=NULL][, X :=NULL][, Y :=NULL]
      data.table::setnames(eu_data, 'gaugeid', 'gauge_id')
      eu_data
    }, priority = 1
  ),
  tar_target(
    name = "in_task_eu",
    command = create_task_stepone(in_tbl = model_data_eu[,-1]), priority = 1
  ),
  tar_target(
    name = "measures",
    command = list(classif = msr("classif.bacc"),
                   regr = msr("regr.mae")), priority = 1
  ),
  tar_target(
    name = "baselearners_eu",
    command = create_baselearners(in_task = in_task_eu, ncores = 55), priority = 1
  ),
  tar_target(
    name = "autotuning_eu",
    command = set_tuning(in_learner = baselearners_eu,
                         in_measure = measures,
                         nfeatures = length(in_task_eu$feature_names),
                         insamp_nfolds = 3, insamp_neval = 30,
                         insamp_nbatch = parallel::detectCores(logical = FALSE) - 30),
    priority = 1
    # pattern = map(seplearners)
  ),
  tar_target(
    name = "resamplingset_eu",
    command = set_cvresampling(rsmp_id = "repeated_cv",
                               in_task = in_task_eu,
                               outsamp_nrep = 2,
                               outsamp_nfolds = 3)
  ),
  tar_target(
    name = "rfresampled_classif_eu",
    command = dynamic_resample(in_task = in_task_eu,
                               in_learner = autotuning_eu,
                               in_resampling = resamplingset_eu,
                               store_models = TRUE,
                               type = "classif")
    # pattern = map(autotuning),
    # iteration = "list"
    
  ),
  tar_target(
    name = "rfbm_classif_eu",
    command = combine_bm(in_resampleresults = rfresampled_classif_eu,
                         write_qs = T,
                         inp_resdir = file.path(results_dir, "store_premodels_single"))
  ),
  tar_target(
    name = "selected_learner",
    command = "oversampled.classif.ranger", priority = 1
  ),
  # tar_target(
  #   name = "tasks_featsel_eu",
  #   command = select_features(
  #     in_bm = rfbm_classif_eu,
  #     in_lrnid =  selected_learner,
  #     in_task = in_task_eu,
  #     pcutoff = 0.05,
  #     inp_resdir = file.path(results_dir, "store_premodels_single")
  #   )
  # ),
  tar_target(
    name = "rftuned_eu",
    command = selecttrain_rf(in_rf = rfresampled_classif_eu,
                             in_learnerid = selected_learner,
                             in_task = "binary_class"), priority = 0.4
  ),
  tar_target(
    name = predvars_eu,
    command = predname_df(in_task = rftuned_eu$task), priority = 0.3
  ),
  tar_target(
    name = 'saved_eu',
    command = save_model(
      in_model=rftuned_eu,
      outdir = file.path(results_dir, 'modeldir/rftuned_step1_eu.qs')
    ),
    priority = 0.9
  )
  # tar_target(
  #   name = 'run_model_reaches_eu_allstat_on_sa',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_eu.qs"),
  #                                   path_static = file.path(data_dir, "predictors/static",
  #                                                           "static_preds_net_sa.fst"),
  #                                   path_LR = file.path(data_dir, "predictors/LR"),
  #                                   path_HR = file.path(data_dir, "predictors/HR"),
  #                                   continent='sa',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eu_allstat_on_sa'),
  #  priority = 0.2
  # ),
  # tar_target(
  #   name = 'run_model_reaches_eu_allstat_on_eu',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_eu.qs"),
  #                                   path_static = file.path(data_dir_eu, "Statics",
  #                                                           "static_preds_net_eu.fst"),
  #                                   path_LR = file.path(data_dir_eu, "LR"),
  #                                   path_HR = file.path(data_dir_eu, "HR"),
  #                                   continent='eu',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eu_allstat_on_eu'),
  #   priority = 0.2
  # )
)
# ------------------------- Modeling development- only SA gauging stations (i.e., model variant II)--------------------------
plan_model_sa = tar_plan(
  tar_target(
    name = 'model_data_sa',
    command = {
      sa_data <- qs::qread(file = file.path(root_dir, 'data/model_data/train_set/train_data_sa.qs'))
      
      # sa_data[, year_mon :=NULL]
    }
  ),
  tar_target(
    name = "in_task_sa",
    command = create_task_stepone(in_tbl = model_data_sa[,-1])
  ),
  tar_target(
    name = "baselearners_sa",
    command = create_baselearners(in_task = in_task_sa, ncores = 55)
  ),
  tar_target(
    name = "autotuning_sa",
    command = set_tuning(in_learner = baselearners_sa,
                         in_measure = measures,
                         nfeatures = length(in_task_sa$feature_names),
                         insamp_nfolds = 3, insamp_neval = 30,
                         insamp_nbatch = parallel::detectCores(logical = FALSE) - 30)
    # pattern = map(seplearners)
  ),
  tar_target(
    name = "resamplingset_sa",
    command = set_cvresampling(rsmp_id = "repeated_cv",
                               in_task = in_task_sa,
                               outsamp_nrep = 2,
                               outsamp_nfolds = 3)
  ),
  tar_target(
    name = "rfresampled_classif_sa",
    command = dynamic_resample(in_task = in_task_sa,
                               in_learner = autotuning_sa,
                               in_resampling = resamplingset_sa,
                               store_models = TRUE,
                               type = "classif")
    # pattern = map(autotuning),
    # iteration = "list"
    
  ),
  tar_target(
    name = "rfbm_classif_sa",
    command = combine_bm(in_resampleresults = rfresampled_classif_sa,
                         write_qs = T,
                         inp_resdir = file.path(results_dir, "store_premodels_single"))
  ),
  # tar_target(
  #   name = "tasks_featsel_sa",
  #   command = select_features(
  #     in_bm = rfbm_classif_sa,
  #     in_lrnid =  selected_learner,
  #     in_task = in_task_sa,
  #     pcutoff = 0.05,
  #     inp_resdir = file.path(results_dir, "store_premodels_single")
  #   )
  # ),
  tar_target(
    name = "rftuned_sa",
    command = selecttrain_rf(in_rf = rfresampled_classif_sa,
                             in_learnerid = selected_learner,
                             in_task = "binary_class")
  ),
  tar_target(
    name = predvars_sa,
    command = predname_df(in_task = rftuned_sa$task)
  ),
  tar_target(
    name = 'saved_sa',
    command = save_model(
      in_model=rftuned_sa,
      outdir = 'results/01_train_val_approach/modeldir/rftuned_step1_sa.qs'
    )
  )
  # tar_target(
  #   name = 'run_model_reaches_sa_allstat_on_sa',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_sa.qs"),
  #                                   path_static = file.path(data_dir, "predictors/static",
  #                                                           "static_preds_net_sa.fst"),
  #                                   path_LR = file.path(data_dir, "predictors/LR"),
  #                                   path_HR = file.path(data_dir, "predictors/HR"),
  #                                   continent='sa',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'sa_allstat_on_sa')
  # ),
  # tar_target(
  #   name = 'run_model_reaches_sa_allstat_on_eu',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_sa.qs"),
  #                                   path_static = file.path(data_dir_eu, "Statics",
  #                                                           "static_preds_net_eu.fst"),
  #                                   path_LR = file.path(data_dir_eu, "LR"),
  #                                   path_HR = file.path(data_dir_eu, "HR"),
  #                                   continent='eu',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'sa_allstat_on_eu')
  # )
)
# ------------------------- Modeling development- European and South American stations together (i.e., model variant IIIa)-----------
plan_model_eusa = tar_plan(
  tar_target(
    name = 'model_data_eusa',
    command = {
      eu_data <- qs::qread(file = file.path(root_dir, 'data/model_data/train_set/train_data_eu.qs'))
      sa_data <- qs::qread(file = file.path(root_dir, 'data/model_data/train_set/train_data_sa.qs'))
      
      eu_data[, dates :=NULL][, X :=NULL][, Y :=NULL]
      data.table::setnames(eu_data, 'gaugeid', 'gauge_id')
      cols_to_order <- names(eu_data)
      sa_data <- sa_data[,..cols_to_order]
      out <- rbind(eu_data, sa_data)
    }
  ),
  tar_target(
    name = "in_task_eusa",
    command = create_task_stepone(in_tbl = model_data_eusa[,-1])
  ),
  tar_target(
    name = "baselearners_eusa",
    command = create_baselearners(in_task = in_task_eusa, ncores = 55)
  ),
  tar_target(
    name = "autotuning_eusa",
    command = set_tuning(in_learner = baselearners_eusa,
                         in_measure = measures,
                         nfeatures = length(in_task_eusa$feature_names),
                         insamp_nfolds = 3, insamp_neval = 30,
                         insamp_nbatch = parallel::detectCores(logical = FALSE) - 50)
    # pattern = map(seplearners)
  ),
  tar_target(
    name = "resamplingset_eusa",
    command = set_cvresampling(rsmp_id = "repeated_cv",
                               in_task = in_task_eusa,
                               outsamp_nrep = 2,
                               outsamp_nfolds = 3)
  ),
  tar_target(
    name = "rfresampled_classif_eusa",
    command = dynamic_resample(in_task = in_task_eusa,
                               in_learner = autotuning_eusa,
                               in_resampling = resamplingset_eusa,
                               store_models = TRUE,
                               type = "classif")
    # pattern = map(autotuning),
    # iteration = "list"
    
  ),
  tar_target(
    name = "rfbm_classif_eusa",
    command = combine_bm(in_resampleresults = rfresampled_classif_eusa,
                         write_qs = T,
                         inp_resdir = file.path(results_dir, "store_premodels_eusa"))
  ),
  tar_target(
    name = "tasks_featsel_eusa",
    command = select_features(
      in_bm = rfbm_classif_eusa,
      in_lrnid =  selected_learner,
      in_task = in_task_eusa,
      pcutoff = 0.05,
      inp_resdir = file.path(results_dir, "store_premodels_eusa")
    )
  ),
  tar_target(
    name = "rftuned_eusa",
    command = selecttrain_rf(in_rf = rfresampled_classif_eusa,
                             in_learnerid = selected_learner,
                             in_task = "binary_class")
  ),
  tar_target(
    name = predvars_eusa,
    command = predname_df(in_task = rftuned_eusa$task)
  ),
  tar_target(
    name = 'saved_eusa',
    command = save_model(
      in_model=rftuned_eusa,
      outdir = 'results/01_train_val_approach/modeldir/rftuned_step1_eusa_all_gstat.qs'
    )
  )
  # tar_target(
  #   name = 'run_model_reaches_eusa_on_sa',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_eusa_all_gstat.qs"),
  #                                   path_static = file.path(data_dir, "predictors/static",
  #                                                           "static_preds_net_sa.fst"),
  #                                   path_LR = file.path(data_dir, "predictors/LR"),
  #                                   path_HR = file.path(data_dir, "predictors/HR"),
  #                                   continent='sa',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eusa_all_gstat_on_sa')
  # ),
  # tar_target(
  #   name = 'run_model_reaches_eusa_on_eu',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_eusa_all_gstat.qs"),
  #                                   path_static = file.path(data_dir_eu, "Statics",
  #                                                           "static_preds_net_eu.fst"),
  #                                   path_LR = file.path(data_dir_eu, "LR"),
  #                                   path_HR = file.path(data_dir_eu, "HR"),
  #                                   continent='eu',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eusa_all_gstat_on_eu')
  # ),
  # tar_target(
  #   name = 'run_model_reaches_eu_plus_single_sa_on_sa_prob',
  #   command = runmodels_over_period_withprob(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_eusa_all_gstat.qs"),
  #                                   path_static = file.path(data_dir, "predictors/static",
  #                                                           "static_preds_net_sa.fst"),
  #                                   path_LR = file.path(data_dir, "predictors/LR"),
  #                                   path_HR = file.path(data_dir, "predictors/HR"),
  #                                   continent='sa',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eusa_all_gstat_on_sa')
  # ),
)
# ------------------------- Modeling development- European and South American stations with hyperparameter (i.e., model variant IIIb)-----------
plan_model_eusa_hyperparameter = tar_plan(
  tar_target(
    name = "rftuned_eusa_hyperparameter",
    command = {
      rf_model_2024 <-  qs::qread(file.path(data_dir, 'model_2024_paper/rftuned_step1.qs'))
      
      rf_mtry <- rf_model_2024$model$learner$param_set$values$classif.ranger.mtry
      rf_min_node_size <- rf_model_2024$model$learner$param_set$values$classif.ranger.min.node.size
      rf_smpfr <- rf_model_2024$model$learner$param_set$values$classif.ranger.sample.fraction
      
      lrns <- mlr3::lrn('classif.ranger',
                        num.trees = 800,
                        sample.fraction = rf_smpfr,
                        replace = FALSE,
                        splitrule = 'gini',
                        predict_type = 'prob',
                        importance = 'impurity_corrected',
                        mtry=rf_mtry, min.node.size = rf_min_node_size,
                        respect.unordered.factors = 'order', seed=42)
      set_threads(lrns, n = 55)
      po_oversampled <- mlr3pipelines::po("classbalancing", id = "oversampled", adjust = "minor",
                                          reference = "minor", shuffle = TRUE,
                                          ratio = 32.21)
      
      #Create graph learners so that oversampling happens systematically upstream of all training
      lrns <- mlr3pipelines::GraphLearner$new(
        po_oversampled %>>% lrns)
      
      model_data_eusa <- tar_load(model_data_eusa)
      in_task_eusa <- create_task_stepone(in_tbl = model_data_eusa[,-1])
      lrns$train(in_task_eusa)
    }
  ),
  tar_target(
    name = 'saved_eusa_hyper',
    command = {
      qs::qsave(lrns,file.path(results_dir, "modeldir/rftuned_step1_eusa_IIIb.qs"))
    }
  )
  # tar_target(
  #   name = 'run_model_reaches_eusa_doell_hyper_on_sa',
  #   command = runmodels_over_period((path_model1 = file.path(results_dir, "modeldir/rftuned_step1_eusa_IIIb.qs"),
  #                                                         path_static = file.path(data_dir, "predictors/static",
  #                                                                                 "static_preds_net_sa.fst"),
  #                                                         path_LR = file.path(data_dir, "predictors/LR"),
  #                                                         path_HR = file.path(data_dir, "predictors/HR"),
  #                                                         continent='sa',
  #                                                         outdir = file.path(results_dir, "SI_status_nets"),
  #                                                         start_year=1981, end_year=2019,scenario = 'eusa_doell_hyperparam_on_sa')
  # ),
  # tar_target(
  #   name = 'run_model_reaches_eusa_doell_hyper_on_eu',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_eusa_IIIb.qs"),
  #                                   path_static = file.path(data_dir_eu, "Statics",
  #                                                         "static_preds_net_eu.fst"),
  #                                 path_LR = file.path(data_dir_eu, "LR"),
  #                                 path_HR = file.path(data_dir_eu, "HR"),
  #                                 continent='eu',
  #                                 outdir = file.path(results_dir, "SI_status_nets"),
  #                                 start_year=1981, end_year=2019,scenario = 'eusa_doell_hyperparam_on_eu')
  # ),
  # tar_target(
  #   name = 'run_model_reaches_eu_plus_single_sa_on_sa_prob',
  #   command = runmodels_over_period_withprob(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_eusa_IIIb.qs"),
  #                                   path_static = file.path(data_dir, "predictors/static",
  #                                                           "static_preds_net_sa.fst"),
  #                                   path_LR = file.path(data_dir, "predictors/LR"),
  #                                   path_HR = file.path(data_dir, "predictors/HR"),
  #                                   continent='sa',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eusa_doell_hyperparam_on_sa')
  # ),
)
# ------------------------- Modeling development- European and 210 South American stations (i.e., model variant IV) --------------------------
plan_model_single = tar_plan(
  tar_target(
    name = 'model_data_single',
    command = combine_eusa_data(root_dir = root_dir,
                                st_density = 210,
                                scenario = 'single')
  ),
  tar_target(
    name = "in_task",
    command = create_task_stepone(in_tbl = model_data_single[,-1])
  ),
  tar_target(
    name = "baselearners",
    command = create_baselearners(in_task = in_task, ncores = 55)
  ),
  tar_target(
    name = "autotuning",
    command = set_tuning(in_learner = baselearners,
                         in_measure = measures,
                         nfeatures = length(in_task$feature_names),
                         insamp_nfolds = 3, insamp_neval = 30,
                         insamp_nbatch = parallel::detectCores(logical = FALSE) - 30)
    # pattern = map(seplearners)
  ),
  tar_target(
    name = "resamplingset",
    command = set_cvresampling(rsmp_id = "repeated_cv",
                               in_task = in_task,
                               outsamp_nrep = 2,
                               outsamp_nfolds = 3)
  ),
  tar_target(
    name = "rfresampled_classif",
    command = dynamic_resample(in_task = in_task,
                               in_learner = autotuning,
                               in_resampling = resamplingset,
                               store_models = TRUE,
                               type = "classif")
    # pattern = map(autotuning),
    # iteration = "list"
    
  ),
  tar_target(
    name = "rfbm_classif",
    command = combine_bm(in_resampleresults = rfresampled_classif,
                         write_qs = T,
                         inp_resdir = file.path(results_dir, "store_premodels_single"))
  ),
  # tar_target(
  #   name = "tasks_featsel",
  #   command = select_features(
  #     in_bm = rfbm_classif,
  #     in_lrnid =  selected_learner,
  #     in_task = in_task,
  #     pcutoff = 0.05,
  #     inp_resdir = file.path(results_dir, "store_premodels_single")
  #   )
  # ),
  tar_target(
    name = "rftuned_single",
    command = selecttrain_rf(in_rf = rfresampled_classif,
                             in_learnerid = selected_learner,
                             in_task = "binary_class")
  ),
  tar_target(
    name = predvars_single,
    command = predname_df(in_task = rftuned_single$task)
  ),
  tar_target(
    name = 'saved_single',
    command = save_model(
      in_model=rftuned_single,
      outdir = 'results/01_train_val_approach/modeldir/rftuned_step1_single.qs'
    )
  )
  # tar_target(
  #   name = 'run_model_reaches_eu_plus_single_sa_on_sa',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_single.qs"),
  #                                   path_static = file.path(data_dir, "predictors/static",
  #                                                           "static_preds_net_sa.fst"),
  #                                   path_LR = file.path(data_dir, "predictors/LR"),
  #                                   path_HR = file.path(data_dir, "predictors/HR"),
  #                                   continent='sa',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eu_plus_single_sa_on_sa')
  # ),
  # tar_target(
  #   name = 'run_model_reaches_eu_plus_single_sa_on_eu',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_single.qs"),
  #                                   path_static = file.path(data_dir_eu, "Statics",
  #                                                           "static_preds_net_eu.fst"),
  #                                   path_LR = file.path(data_dir_eu, "LR"),
  #                                   path_HR = file.path(data_dir_eu, "HR"),
  #                                   continent='eu',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eu_plus_single_sa_on_eu')
  # ),
  # tar_target(
  #   name = 'run_model_reaches_eu_plus_single_sa_on_sa_prob',
  #   command = runmodels_over_period_withprob(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_single.qs"),
  #                                   path_static = file.path(data_dir, "predictors/static",
  #                                                           "static_preds_net_sa.fst"),
  #                                   path_LR = file.path(data_dir, "predictors/LR"),
  #                                   path_HR = file.path(data_dir, "predictors/HR"),
  #                                   continent='sa',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eu_plus_single_sa_on_sa')
  # ),
  # tar_target(
  #   name = 'run_model_reaches_eu_plus_single_sa_on_eu_prob',
  #   command = runmodels_over_period_withprob(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_single.qs"),
  #                                   path_static = file.path(data_dir_eu, "Statics",
  #                                                           "static_preds_net_eu.fst"),
  #                                   path_LR = file.path(data_dir_eu, "LR"),
  #                                   path_HR = file.path(data_dir_eu, "HR"),
  #                                   continent='eu',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eu_plus_single_sa_on_eu')
  # )
)


# ------------------------- Modeling development- South American random 210 stations (i.e., model variant V)-----------
plan_model_sa_random_210_stations = tar_plan(
  tar_target(
    name = 'model_data_sa_random_210',
    command = {
      
      tar_load(model_data_single)
      tar_load(model_data_sa)
      sa_st_ids <- model_data_sa[,unique(gauge_id)]
      dt_fixed_210 <- model_data_single[gauge_id %in% sa_st_ids, unique(gauge_id)]
      
      new_dt_sa <- model_data_sa[!(gauge_id %in% dt_fixed_210)]
      gauge_id <- new_dt_sa[, unique(gauge_id)]
      
      set.seed(20250703)
      selected_rows <- sample(length(gauge_id), 210)
      selected_stations <- gauge_id[selected_rows]
      out <- new_dt_sa[gauge_id %in% selected_stations]
      
    }
  ),
  tar_target(
    name = "in_task_sa_random_210",
    command = create_task_stepone(in_tbl = model_data_sa_random_210[,-1])
  ),
  tar_target(
    name = "baselearners_sa_random_210",
    command = create_baselearners(in_task = in_task_sa_random_210, ncores = 55)
  ),
  tar_target(
    name = "autotuning_sa_random_210",
    command = set_tuning(in_learner = baselearners_sa_random_210,
                         in_measure = measures,
                         nfeatures = length(in_task_sa_random_210$feature_names),
                         insamp_nfolds = 3, insamp_neval = 30,
                         insamp_nbatch = parallel::detectCores(logical = FALSE) - 30)
    # pattern = map(seplearners)
  ),
  tar_target(
    name = "resamplingset_sa_random_210",
    command = set_cvresampling(rsmp_id = "repeated_cv",
                               in_task = in_task_sa_random_210,
                               outsamp_nrep = 2,
                               outsamp_nfolds = 3)
  ),
  tar_target(
    name = "rfresampled_classif_sa_random_210",
    command = dynamic_resample(in_task = in_task_sa_random_210,
                               in_learner = autotuning_sa_random_210,
                               in_resampling = resamplingset_sa_random_210,
                               store_models = TRUE,
                               type = "classif")
    # pattern = map(autotuning),
    # iteration = "list"
    
  ),
  tar_target(
    name = "rfbm_classif_sa_random_210",
    command = combine_bm(in_resampleresults = rfresampled_classif_sa_random_210,
                         write_qs = T,
                         inp_resdir = file.path(results_dir, "store_premodels_sa_random_210_stations"))
  ),
  # tar_target(
  #   name = "tasks_featsel_sa_np",
  #   command = select_features(
  #     in_bm = rfbm_classif_sa_np,
  #     in_lrnid =  selected_learner,
  #     in_task = in_task_sa_np,
  #     pcutoff = 0.05,
  #     inp_resdir = file.path(results_dir, "store_premodels_sa_np_stations")
  #   )
  # ),
  tar_target(
    name = "rftuned_sa_random_210",
    command = selecttrain_rf(in_rf = rfresampled_classif_sa_random_210,
                             in_learnerid = selected_learner,
                             in_task = "binary_class")
  ),
  tar_target(
    name = predvars_sa_random_210,
    command = predname_df(in_task = rftuned_sa_random_210$task)
  ),
  tar_target(
    name = 'saved_sa_random_210',
    command = save_model(
      in_model=rftuned_sa_random_210,
      outdir = 'results/01_train_val_approach/modeldir/rftuned_step1_sa_random_210_gstat.qs'
    )
  ),
  tar_target(
    name = 'run_model_reaches_sa_random_210_on_sa',
    command = runmodels_over_period(path_model1 =
                                      file.path(results_dir, "modeldir/rftuned_step1_sa_random_210_gstat.qs"),
                                    path_static = file.path(data_dir, "predictors/static",
                                                            "static_preds_net_sa.fst"),
                                    path_LR = file.path(data_dir, "predictors/LR"),
                                    path_HR = file.path(data_dir, "predictors/HR"),
                                    continent='sa',
                                    outdir = file.path(results_dir, "SI_status_nets"),
                                    start_year=1981, end_year=2019,scenario = 'sa_random_210_gstat_on_sa')
  ),
  tar_target(
    name = 'run_model_reaches_sa_random_210_on_eu',
    command = runmodels_over_period(path_model1 =
                                      file.path(results_dir, "modeldir/rftuned_step1_sa_random_210_gstat.qs"),
                                    path_static = file.path(data_dir_eu, "Statics",
                                                            "static_preds_net_eu.fst"),
                                    path_LR = file.path(data_dir_eu, "LR"),
                                    path_HR = file.path(data_dir_eu, "HR"),
                                    continent='eu',
                                    outdir = file.path(results_dir, "SI_status_nets"),
                                    start_year=1981, end_year=2019,scenario = 'sa_random_210_gstat_on_eu')
  )
)

# ------------------------- Modeling development- South American fixed 210 stations (i.e., model variant VI) -----------
plan_model_sa_fixed_210_stations = tar_plan(
  tar_target(
    name = 'model_data_sa_fixed_210',
    command = {
      
      tar_load(model_data_single)
      tar_load(model_data_sa)
      
      sa_st_ids <- model_data_sa[,unique(gauge_id)]
      dt_out <- model_data_single[gauge_id %in% sa_st_ids]
      
    }
  ),
  tar_target(
    name = "in_task_sa_fixed_210",
    command = create_task_stepone(in_tbl = model_data_sa_fixed_210[,-1])
  ),
  tar_target(
    name = "baselearners_sa_fixed_210",
    command = create_baselearners(in_task = in_task_sa_fixed_210, ncores = 55)
  ),
  tar_target(
    name = "autotuning_sa_fixed_210",
    command = set_tuning(in_learner = baselearners_sa_fixed_210,
                         in_measure = measures,
                         nfeatures = length(in_task_sa_fixed_210$feature_names),
                         insamp_nfolds = 3, insamp_neval = 30,
                         insamp_nbatch = parallel::detectCores(logical = FALSE) - 30)
    # pattern = map(seplearners)
  ),
  tar_target(
    name = "resamplingset_sa_fixed_210",
    command = set_cvresampling(rsmp_id = "repeated_cv",
                               in_task = in_task_sa_fixed_210,
                               outsamp_nrep = 2,
                               outsamp_nfolds = 3)
  ),
  tar_target(
    name = "rfresampled_classif_sa_fixed_210",
    command = dynamic_resample(in_task = in_task_sa_fixed_210,
                               in_learner = autotuning_sa_fixed_210,
                               in_resampling = resamplingset_sa_fixed_210,
                               store_models = TRUE,
                               type = "classif")
    # pattern = map(autotuning),
    # iteration = "list"
    
  ),
  tar_target(
    name = "rfbm_classif_sa_fixed_210",
    command = combine_bm(in_resampleresults = rfresampled_classif_sa_fixed_210,
                         write_qs = T,
                         inp_resdir = file.path(results_dir, "store_premodels_sa_fixed_210_stations"))
  ),
  # tar_target(
  #   name = "tasks_featsel_sa_np",
  #   command = select_features(
  #     in_bm = rfbm_classif_sa_np,
  #     in_lrnid =  selected_learner,
  #     in_task = in_task_sa_np,
  #     pcutoff = 0.05,
  #     inp_resdir = file.path(results_dir, "store_premodels_sa_np_stations")
  #   )
  # ),
  tar_target(
    name = "rftuned_sa_fixed_210",
    command = selecttrain_rf(in_rf = rfresampled_classif_sa_fixed_210,
                             in_learnerid = selected_learner,
                             in_task = "binary_class")
  ),
  tar_target(
    name = predvars_sa_fixed_210,
    command = predname_df(in_task = rftuned_sa_fixed_210$task)
  ),
  tar_target(
    name = 'saved_sa_fixed_210',
    command = save_model(
      in_model=rftuned_sa_fixed_210,
      outdir = 'results/01_train_val_approach/modeldir/rftuned_step1_sa_fixed_210_gstat.qs'
    )
  )
#   tar_target(
#     name = 'run_model_reaches_sa_fixed_210_on_sa',
#     command = runmodels_over_period(path_model1 =
#                                       file.path(results_dir, "modeldir/rftuned_step1_sa_fixed_210_gstat.qs"),
#                                     path_static = file.path(data_dir, "predictors/static",
#                                                             "static_preds_net_sa.fst"),
#                                     path_LR = file.path(data_dir, "predictors/LR"),
#                                     path_HR = file.path(data_dir, "predictors/HR"),
#                                     continent='sa',
#                                     outdir = file.path(results_dir, "SI_status_nets"),
#                                     start_year=1981, end_year=2019,scenario = 'sa_fixed_210_gstat_on_sa')
#   ),
#   tar_target(
#     name = 'run_model_reaches_sa_fixed_210_on_eu',
#     command = runmodels_over_period(path_model1 =
#                                       file.path(results_dir, "modeldir/rftuned_step1_sa_fixed_210_gstat.qs"),
#                                     path_static = file.path(data_dir_eu, "Statics",
#                                                             "static_preds_net_eu.fst"),
#                                     path_LR = file.path(data_dir_eu, "LR"),
#                                     path_HR = file.path(data_dir_eu, "HR"),
#                                     continent='eu',
#                                     outdir = file.path(results_dir, "SI_status_nets"),
#                                     start_year=1981, end_year=2019,scenario = 'sa_fixed_210_gstat_on_eu')
#   ),
#   tar_target(
#     name = 'run_model_reaches_sa_fixed_210_on_sa_prob',
#     command = runmodels_over_period_withprob(path_model1 =
#                                                file.path(results_dir, "modeldir/rftuned_step1_sa_fixed_210_gstat.qs"),
#                                              path_static = file.path(data_dir, "predictors/static",
#                                                                      "static_preds_net_sa.fst"),
#                                              path_LR = file.path(data_dir, "predictors/LR"),
#                                              path_HR = file.path(data_dir, "predictors/HR"),
#                                              continent='sa',
#                                              outdir = file.path(results_dir, "SI_status_nets"),
#                                              start_year=1981, end_year=2019,scenario = 'sa_fixed_210_gstat_on_sa')
#   ),
#   tar_target(
#     name = 'run_model_reaches_sa_fixed_210_on_eu_prob',
#     command = runmodels_over_period_withprob(path_model1 =
#                                                file.path(results_dir, "modeldir/rftuned_step1_sa_fixed_210_gstat.qs"),
#                                              path_static = file.path(data_dir_eu, "Statics",
#                                                                      "static_preds_net_eu.fst"),
#                                              path_LR = file.path(data_dir_eu, "LR"),
#                                              path_HR = file.path(data_dir_eu, "HR"),
#                                              continent='eu',
#                                              outdir = file.path(results_dir, "SI_status_nets"),
#                                              start_year=1981, end_year=2019,scenario = 'sa_fixed_210_gstat_on_eu')
#   )
)

# ------------------------- Modeling development- European only non-perennial stations (i.e., model variant VII) -----------
plan_model_eu_np_stations = tar_plan(
  tar_target(
    name = 'model_data_eu_np',
    command = {
      eu_data <- qs::qread(file = file.path(root_dir, 'data/model_data/train_set/train_data_eu.qs'))

      eu_data[, dates :=NULL][, X :=NULL][, Y :=NULL]
      data.table::setnames(eu_data, 'gaugeid', 'gauge_id')
      cols_to_order <- names(eu_data)
      np_stations_eu <- eu_data[target>0,unique(gauge_id)]
      eu_data[gauge_id %in% np_stations_eu]
    }
  ),
  tar_target(
    name = "in_task_eu_np",
    command = create_task_stepone(in_tbl = model_data_eu_np[,-1])
  ),
  tar_target(
    name = "baselearners_eu_np",
    command = create_baselearners(in_task = in_task_eu_np, ncores = 55)
  ),
  tar_target(
    name = "autotuning_eu_np",
    command = set_tuning(in_learner = baselearners_eu_np,
                         in_measure = measures,
                         nfeatures = length(in_task_eu_np$feature_names),
                         insamp_nfolds = 3, insamp_neval = 30,
                         insamp_nbatch = parallel::detectCores(logical = FALSE) - 30)
    # pattern = map(seplearners)
  ),
  tar_target(
    name = "resamplingset_eu_np",
    command = set_cvresampling(rsmp_id = "repeated_cv",
                               in_task = in_task_eu_np,
                               outsamp_nrep = 2,
                               outsamp_nfolds = 3)
  ),
  tar_target(
    name = "rfresampled_classif_eu_np",
    command = dynamic_resample(in_task = in_task_eu_np,
                               in_learner = autotuning_eu_np,
                               in_resampling = resamplingset_eu_np,
                               store_models = TRUE,
                               type = "classif")
    # pattern = map(autotuning),
    # iteration = "list"

  ),
  tar_target(
    name = "rfbm_classif_eu_np",
    command = combine_bm(in_resampleresults = rfresampled_classif_eu_np,
                         write_qs = T,
                         inp_resdir = file.path(results_dir, "store_premodels_eu_np_stations"))
  ),
  # tar_target(
  #   name = "tasks_featsel_eu_np",
  #   command = select_features(
  #     in_bm = rfbm_classif_eu_np,
  #     in_lrnid =  selected_learner,
  #     in_task = in_task_eu_np,
  #     pcutoff = 0.05,
  #     inp_resdir = file.path(results_dir, "store_premodels_eu_np_stations")
  #   )
  # ),
  tar_target(
    name = "rftuned_eu_np",
    command = selecttrain_rf(in_rf = rfresampled_classif_eu_np,
                             in_learnerid = selected_learner,
                             in_task = "binary_class")
  ),
  tar_target(
    name = predvars_eu_np,
    command = predname_df(in_task = rftuned_eu_np$task)
  ),
  tar_target(
    name = 'saved_eu_np',
    command = save_model(
      in_model=rftuned_eu_np,
      outdir = 'results/01_train_val_approach/modeldir/rftuned_step1_eu_only_np_gstat.qs'
    )
  )
  # tar_target(
  #   name = 'run_model_reaches_eu_only_np_on_sa',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_eu_only_np_gstat.qs"),
  #                                   path_static = file.path(data_dir, "predictors/static",
  #                                                           "static_preds_net_sa.fst"),
  #                                   path_LR = file.path(data_dir, "predictors/LR"),
  #                                   path_HR = file.path(data_dir, "predictors/HR"),
  #                                   continent='sa',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eu_only_np_gstat_on_sa')
  # ),
  # tar_target(
  #   name = 'run_model_reaches_eu_only_np_on_eu',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_eu_only_np_gstat.qs"),
  #                                   path_static = file.path(data_dir_eu, "Statics",
  #                                                           "static_preds_net_eu.fst"),
  #                                   path_LR = file.path(data_dir_eu, "LR"),
  #                                   path_HR = file.path(data_dir_eu, "HR"),
  #                                   continent='eu',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'eu_only_np_gstat_on_eu')
  # )
)

# ------------------------- Modeling development- South American only non-perennial stations (i.e., model variant VIII) -----------
plan_model_sa_np_stations = tar_plan(
  tar_target(
    name = 'model_data_sa_np',
    command = {
      sa_data <- qs::qread(file = file.path(root_dir, 'data/model_data/train_set/train_data_sa.qs'))
      
      np_stations_sa <- sa_data[target>0,unique(gauge_id)]
      sa_data[gauge_id %in% np_stations_sa]
    }
  ),
  tar_target(
    name = "in_task_sa_np",
    command = create_task_stepone(in_tbl = model_data_sa_np[,-1])
  ),
  tar_target(
    name = "baselearners_sa_np",
    command = create_baselearners(in_task = in_task_sa_np, ncores = 55)
  ),
  tar_target(
    name = "autotuning_sa_np",
    command = set_tuning(in_learner = baselearners_sa_np,
                         in_measure = measures,
                         nfeatures = length(in_task_sa_np$feature_names),
                         insamp_nfolds = 3, insamp_neval = 30,
                         insamp_nbatch = parallel::detectCores(logical = FALSE) - 30)
    # pattern = map(seplearners)
  ),
  tar_target(
    name = "resamplingset_sa_np",
    command = set_cvresampling(rsmp_id = "repeated_cv",
                               in_task = in_task_sa_np,
                               outsamp_nrep = 2,
                               outsamp_nfolds = 3)
  ),
  tar_target(
    name = "rfresampled_classif_sa_np",
    command = dynamic_resample(in_task = in_task_sa_np,
                               in_learner = autotuning_sa_np,
                               in_resampling = resamplingset_sa_np,
                               store_models = TRUE,
                               type = "classif")
    # pattern = map(autotuning),
    # iteration = "list"
    
  ),
  tar_target(
    name = "rfbm_classif_sa_np",
    command = combine_bm(in_resampleresults = rfresampled_classif_sa_np,
                         write_qs = T,
                         inp_resdir = file.path(results_dir, "store_premodels_sa_np_stations"))
  ),
  # tar_target(
  #   name = "tasks_featsel_sa_np",
  #   command = select_features(
  #     in_bm = rfbm_classif_sa_np,
  #     in_lrnid =  selected_learner,
  #     in_task = in_task_sa_np,
  #     pcutoff = 0.05,
  #     inp_resdir = file.path(results_dir, "store_premodels_sa_np_stations")
  #   )
  # ),
  tar_target(
    name = "rftuned_sa_np",
    command = selecttrain_rf(in_rf = rfresampled_classif_sa_np,
                             in_learnerid = selected_learner,
                             in_task = "binary_class")
  ),
  tar_target(
    name = predvars_sa_np,
    command = predname_df(in_task = rftuned_sa_np$task)
  ),
  tar_target(
    name = 'saved_sa_np',
    command = save_model(
      in_model=rftuned_sa_np,
      outdir = 'results/01_train_val_approach/modeldir/rftuned_step1_sa_only_np_gstat.qs'
    )
  )
  # tar_target(
  #   name = 'run_model_reaches_sa_only_np_on_sa',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_sa_only_np_gstat.qs"),
  #                                   path_static = file.path(data_dir, "predictors/static",
  #                                                           "static_preds_net_sa.fst"),
  #                                   path_LR = file.path(data_dir, "predictors/LR"),
  #                                   path_HR = file.path(data_dir, "predictors/HR"),
  #                                   continent='sa',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'sa_only_np_gstat_on_sa')
  # ),
  # tar_target(
  #   name = 'run_model_reaches_sa_only_np_on_eu',
  #   command = runmodels_over_period(path_model1 =
  #                                     file.path(results_dir, "modeldir/rftuned_step1_sa_only_np_gstat.qs"),
  #                                   path_static = file.path(data_dir_eu, "Statics",
  #                                                           "static_preds_net_eu.fst"),
  #                                   path_LR = file.path(data_dir_eu, "LR"),
  #                                   path_HR = file.path(data_dir_eu, "HR"),
  #                                   continent='eu',
  #                                   outdir = file.path(results_dir, "SI_status_nets"),
  #                                   start_year=1981, end_year=2019,scenario = 'sa_only_np_gstat_on_eu')
  # )
)



# ------------------ Post-processing: creating figures -------------
create_final_products <- tar_plan(
  tar_target(
    name = 'create_shp_fig1',
    command = create_fig1(in_eu_all_dir=file.path(data_dir, 'model_data/model_data_eu.qs'),
                          in_sa_all_dir=file.path(data_dir, 'model_data/model_data_eu.qs'),
                          in_eu_val_dir=file.path(data_dir, 'model_data/val_set/val_data_eu.qs'),
                          in_sa_val_dir=file.path(data_dir, 'model_data/val_set/val_data_sa.qs'),
                          gauge_eu_dir=file.path(data_dir, 'gauging_stations/final_gauges_eu.shp'),
                          gauge_sa_dir=file.path(data_dir, 'gauging_stations/final_stations_sa.shp'),
                          out_dir=file.path(results_dir, 'figures/figure1'))
  ),
  tar_target(
    name = 'create_shp_fig2',
    command = create_fig2(in_eu_val_dir=file.path(data_dir, 'model_data/val_set/val_data_eu.qs'),
                          in_sa_val_dir=file.path(data_dir, 'model_data/val_set/val_data_sa.qs'),
                          model_i_dir = file.path(results_dir, "modeldir/rftuned_step1_eu.qs"),
                          model_ii_dir = file.path(results_dir, "modeldir/rftuned_step1_sa.qs"),
                          model_iiia_dir = file.path(results_dir, "modeldir/rftuned_step1_eusa_all_gstat.qs"),
                          gauge_eu_dir=file.path(data_dir, 'gauging_stations/final_gauges_eu.shp'),
                          gauge_sa_dir=file.path(data_dir, 'gauging_stations/final_stations_sa.shp'),
                          out_dir=file.path(results_dir, 'figures/figure2'))
  ),
  tar_target(
    name = 'create_shp_fig3',
    command = create_fig3(reaches_SI_status_dir=file.path(results_dir, "SI_status_nets"),
                          reaches_eu_dir=file.path(data_dir, 'river_net/01_dryver_net_eu.shp'),
                          reaches_sa_dir=file.path(data_dir, 'river_net/river_net_southamerica.shp'),
                          out_dir=file.path(results_dir, 'figures/figure3'))
  ),
  tar_target(
    name = 'create_shp_fig4',
    command = create_fig4(in_eu_val_dir=file.path(data_dir, 'model_data/val_set/val_data_eu.qs'),
                          in_sa_val_dir=file.path(data_dir, 'model_data/val_set/val_data_sa.qs'), 
                          model_iiib_dir= file.path(results_dir, "modeldir/rftuned_step1_eusa_IIIb.qs"),
                          reaches_SI_status_dir=file.path(results_dir, "SI_status_nets"),
                          gauge_eu_dir=file.path(data_dir, 'gauging_stations/final_gauges_eu.shp'),
                          gauge_sa_dir=file.path(data_dir, 'gauging_stations/final_stations_sa.shp'),
                          reaches_eu_dir=file.path(data_dir, 'river_net/01_dryver_net_eu.shp'),
                          reaches_sa_dir=file.path(data_dir, 'river_net/river_net_southamerica.shp'),
                          out_dir=file.path(results_dir, 'figures/figure4'))
  ),
  tar_target(
    name = 'create_fig5',
    command = ggvimp(in_rftuned_eu=tar_read(rftuned_eu),
                     in_rftuned_sa=tar_read(rftuned_sa),
                     in_rftuned_eusa=tar_read(rftuned_eusa),
                     in_rftuned_eusa_iiib=tar_read(rftuned_eusa_hyperparameter),
                     in_predvars=tar_read(predvars_eusa))
  ),
  tar_target(
    name = 'create_shp_fig6',
    command = create_fig6(reaches_SI_status_dir=file.path(results_dir, "SI_status_nets"),
                          reaches_eu_dir=file.path(data_dir, 'river_net/01_dryver_net_eu.shp'),
                          reaches_sa_dir=file.path(data_dir, 'river_net/river_net_southamerica.shp'),
                          out_dir=file.path(results_dir, 'figures/figure6'))
  ),
  tar_target(
    name = 'create_shp_figS1',
    command = create_figS1(in_eu_val_dir=file.path(data_dir, 'model_data/val_set/val_data_eu.qs'),
                           in_sa_val_dir=file.path(data_dir, 'model_data/val_set/val_data_sa.qs'),
                           model_iv_dir=file.path(results_dir, "modeldir/rftuned_step1_single.qs"),
                           model_v_dir=file.path(results_dir, "modeldir/rftuned_step1_sa_fixed_210_gstat.qs"),
                           model_vi_dir=file.path(results_dir, "modeldir/rftuned_step1_sa_random_210_gstat.qs"),
                           gauge_eu_dir=file.path(data_dir, 'gauging_stations/final_gauges_eu.shp'),
                           gauge_sa_dir=file.path(data_dir, 'gauging_stations/final_stations_sa.shp'),
                           out_dir=file.path(results_dir, 'figures/figureS1'))
  ),
  tar_target(
    name = 'create_shp_figS2',
    command = create_figS2(in_eu_val_dir=file.path(data_dir, 'model_data/val_set/val_data_eu.qs'),
                           in_sa_val_dir=file.path(data_dir, 'model_data/val_set/val_data_sa.qs'),
                           model_vii_dir=file.path(results_dir, "modeldir/rftuned_step1_eu_only_np_gstat.qs"),
                           model_viii_dir=file.path(results_dir, "modeldir/rftuned_step1_sa_only_np_gstat.qs"),
                           gauge_eu_dir=file.path(data_dir, 'gauging_stations/final_gauges_eu.shp'),
                           gauge_sa_dir=file.path(data_dir, 'gauging_stations/final_stations_sa.shp'),
                           out_dir=file.path(results_dir, 'figures/figureS2'))
  ),
  tar_target(
    name = 'create_figS4',
    command = ggpartialdep(in_rftuned=tar_read(rftuned_eu),
                           in_predvars=tar_read(predvars_eu),
                           model_data = tar_read(model_data_eu),
                           colnums=1:23, nvariate=1, nodupli = FALSE,
                           ngrid = 40, parallel = FALSE, spatial_rsp = FALSE)
  ),
  tar_target(
    name = 'create_figS5',
    command = ggpartialdep(in_rftuned=tar_read(rftuned_eusa),
                           in_predvars=tar_read(predvars_eusa),
                           model_data = tar_read(model_data_eusa),
                           colnums=1:23, nvariate=1, nodupli = FALSE,
                           ngrid = 40, parallel = FALSE, spatial_rsp = FALSE)
  ),
  tar_target(
    name = 'create_figS6',
    command = ggpartialdep(in_rftuned=tar_read(rftuned_sa),
                           in_predvars=tar_read(predvars_sa),
                           model_data = tar_read(model_data_sa),
                           colnums=1:23, nvariate=1, nodupli = FALSE,
                           ngrid = 40, parallel = FALSE, spatial_rsp = FALSE)
  ),
  tar_target(
    name = 'create_shp_figS7',
    command = create_figS7(reaches_SI_status_dir=file.path(results_dir, "SI_status_nets"),
                           reaches_sa_dir=file.path(data_dir, 'river_net/river_net_southamerica.shp'),
                           out_dir=file.path(results_dir, 'figures/figureS7'))
  ),
  tar_target(
    name = 'create_shp_figS8',
    command = create_figS8(reaches_SI_status_dir=file.path(results_dir, "SI_status_nets"),
                           reaches_sa_dir=file.path(data_dir, 'river_net/river_net_southamerica.shp'),
                           out_dir=file.path(results_dir, 'figures/figureS8'))
  )
)
# ------------------ Pipeline of the workflow's plans -------------
## please execute first all the plans before executing the plan for creating the figures to
## produce all the required objects for figure producing, such as the developed RF models.
list(
  # plan_model_eu,
  # plan_model_sa,
  # plan_model_single,
  # plan_model_eusa,
  # plan_model_eusa_hyperparameter,
  # plan_model_eu_np_stations,
  # plan_model_sa_np_stations,
  # plan_model_sa_fixed_210_stations,
  # plan_model_sa_random_210_stations,
  create_final_products

)
