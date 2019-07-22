# Modeling cognitive mechanisms of learning in mutualistic cleaner fish

This repository provides the code to simulate the learning models. The model is
coded in c++ language. Visualization of the output is done in R. In this readme 
we describe how to run the simulations and how to plot their outcome. 

## Model simulations

As stated before, code for the simulation model is written in C++ language. 
The files contained at the base folder: random.cpp, utils.cpp, utils.h, random.h
and json.hpp, contain useful functions and random number generators used in the 
stochastic simulations. The source file of the simulation model can be find in 
the folder of the respective implementation: Actor-critic (folder:ActCrit,
source file ActCrit.cpp) and Sarsa (folder: sarsa, source file: Sarsa_Gen.cpp).
With these files executables can be compile for the simulation with standard c++
compilers. Executables get JSON files as parameter inputs. JSON files are 
integrated into c++ using the library contained in json.hpp (for more info see:
https://github.com/nlohmann/json). Folders inside the implementation folder 
contain JSON files to run the simulations visualized in the manuscript figures. 
In the actor-critic implementation Fig2, Fig3 and Fig4. In the Sarsa 
implementation SupFig2, SupFig3 and SupFig4. Thus, assuming the executables are 
in the implementation folder from the command line the following commands would 
run the simulations for all figures corresponding to the actor-critic 
implementation.
Fig2:
.\ActCrit.exe Fig2\ecol\parameters.json
.\ActCrit.exe Fig2\exper\parameters.json
Fig3
.\ActCrit.exe Fig3\Vlp0_\parameters.json
.\ActCrit.exe Fig3\Vlp0.2_\parameters.json
.\ActCrit.exe Fig3\Vlp0.4_\parameters.json
.\ActCrit.exe Fig3\Vlp0.6_\parameters.json
.\ActCrit.exe Fig3\Vlp0.8_\parameters.json
.\ActCrit.exe Fig3\Vlp1_\parameters.json
Fig4
.\ActCrit.exe Fig4\abundance_\parameters.json

And for the Sarsa implementation
SupFig2:
.\Sarsa.exe SupFig2\ecol\parameters.json
SupFig3
.\Sarsa.exe SupFig3\Vlp0_\parameters.json
.\Sarsa.exe SupFig3\Vlp0.2_\parameters.json
.\Sarsa.exe SupFig3\Vlp0.4_\parameters.json
.\Sarsa.exe SupFig3\Vlp0.6_\parameters.json
.\Sarsa.exe SupFig3\Vlp0.8_\parameters.json
.\Sarsa.exe SupFig3\Vlp1_\parameters.json
SupFig4
.\Sarsa.exe SupFig4\abundance_\parameters.json


## Visualization
Figures can be produced by running the r files found in the base folder. 
Figure 2 of the paper is produce by running the file timeIntervals_Exp.R. 
Figure 3 can produced by running leavProb.R. By changing the variable "alg" from 
"ActCrit" to "sarsa" this file also produces the supplementary figure 3. 
Figure 4 is produce running the file Combined_plot.R. Just like in the previous 
case changing input in the eight line to "sarsa", the file can also pruduce 
supplementary figure 4. As for supplementary figure 4, it can be produce by 
running the file timeIntervals.R. Note that all the other R files are loaded by 
the files that produce the figures; thus, they must be in the same folder. 










