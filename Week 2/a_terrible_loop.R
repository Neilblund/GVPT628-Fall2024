# a genuinely terrible loop

# checks if beepr is installed, and installs it if not available
ifelse(!require('beepr'), install.packages('beepr'), 
             print('already installed'))

# beep 1 million times (or crash R while trying)
for(i in 1:1e6){
  beep()
}

