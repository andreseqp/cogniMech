Modeling cognitive mechanisms of learning in mutualistic cleaner fish

This repository provides the code to simulate the learning models. The model is
coded in c++ language. 

Actor-critic implementation:

List of files:
ActCrit.cpp
random.cpp
utils.cpp
utils.h
random.h
json.hpp
parTOjson.R
loadData.R
aesth_par.R
data2interp.R
leavProb.R
timeIntervals_Exp.R

The files ActCrit.cpp, random.cpp, utils.cpp, random.h, utils.h and json.hpp, 
can be used to build an executable file with with standard c++ compiler.
The first file contains the algorithm that simulates  the model. The following 4 
files are useful functions and random number generators that can be replaced with 
other libraries. Finally, json.hpp contains a library to allow the integration 
of JSON files with c++, information about such library can be found here:
https://github.com/nlohmann/json
 
JSON files are used to input parameters into the executable file. Parameter 
files necessary to run the simulations for each one of the figures can be found 
inside the corresponding folder. So, for instance, to run the simulations 
presented in figure 2 one must compile an executable named "ActCrit.exe"; and 
run it with parameters for figure 2, like this
.\ActCrit.exe .\Fig2\ecol\parameters.json
.\ActCrit.exe .\Fig2\exper\parameters.json





