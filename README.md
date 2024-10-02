# CAM: The Community Atmosphere Model

CAM Documentation - https://ncar.github.io/CAM/doc/build/html/index.html

CAM6 namelist settings - http://www.cesm.ucar.edu/models/cesm2/settings/current/cam_nml.html

Please see the [wiki](https://github.com/ESCOMP/CAM/wiki) for information.

## Changes
We modified the following files:
1. `src/physics/cam/phys_control.F90:1158` this is the call out to our ML model to modify temperature and the mixing ratio. Pass phys_state to the module.

We added the following files:
1. `src/physics/cam/cam_nn.F90`: This is the call out to a Neural Net that modifies the temp and mixing ratio. Model takes in: temperature, pressure_mid, and mixing ratios. The model weights need to be in `/weights/cam_nn_model.pt`.  
