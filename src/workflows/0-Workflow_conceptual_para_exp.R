require("rlang")

# workflow que voy a correr
#Acá, seleccionar el workflow que corresponda según lo que se quiera correr.
#Comentar el que no se haga, y descomentar el que se busca correr.

#Correr baseline largo
PARAM <- "src/workflows/1-Baseline_Largo.R"

#Correr baseline corto
#PARAM <- "src/workflows/2-Baseline_Corto.R"

#Correr PCA Largo
#PARAM <- "src/workflows/3-Largo_con_PCA.R"

#Correr PCA Corto
#PARAM <- "src/workflows/4-Corto_con_PCA.R"

#Correr Suma de PCA Largo
#PARAM <- "src/workflows/5-Largo_SUMA_PCA.R"

#Correr Suma de PCA Corto
#PARAM <- "src/workflows/6-Corto_SUMA_PCA.R"

#Correr análisis Loadings
#PARAM <- "src/workflows/7-Obtencion_de_Cargas_Largo.R"
#PARAM <- "src/workflows/8-Obtencion_de_Cargas_corto.R"

envg <- env()

envg$EXPENV <- list()
envg$EXPENV$repo_dir <- "~/labo2024v2/"

#------------------------------------------------------------------------------

correr_workflow <- function( wf_scriptname )
{
  dir.create( "~/tmp", showWarnings = FALSE)
  setwd("~/tmp" )

  # creo el script que corre el experimento
  comando <- paste0( 
      "#!/bin/bash\n", 
      "source /home/$USER/.venv/bin/activate\n",
      "nice -n 15 Rscript --vanilla ",
      envg$EXPENV$repo_dir,
      wf_scriptname,
      "   ",
      wf_scriptname,
     "\n",
     "deactivate\n"
    )
  cat( comando, file="run.sh" )

  Sys.chmod( "run.sh", mode = "744", use_umask = TRUE)

  system( "./run.sh" )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# aqui efectivamente llamo al workflow
correr_workflow( PARAM )
