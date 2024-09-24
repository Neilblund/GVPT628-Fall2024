
library(reticulate)

# is python available on the system?

py_available()

# if not, then run: 
if(!py_available()){
  install_python(
    version = "3.12:latest",
    list = FALSE,
    force = FALSE,
    optimized = TRUE
  )
}


# then run: 
virtualenv_create("APAN-Python")


# finally, try installing a module
virtualenv_install(envname="APAN-Python", "requests")

# when you run reticulate, force the use of this environment by running:

reticulate::use_virtualenv("APAN-Python")
