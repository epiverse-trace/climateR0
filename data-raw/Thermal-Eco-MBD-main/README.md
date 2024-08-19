# Thermal-Eco-MBD

## Meta Data for Thermal Ecology of Mosquito-borne Disease Project published here: https://doi.org/10.1111/ele.13335

For R0 and trait trajectories:
- Rows are values along the temperature gradient (see Temperature Trajectories directory for files with the specific sequence for each model) 
- Columns are MCMC iteration. 

Vector-pathogen System codes:
- AecaRRVx = Aedes camptorhynchus (+ Culex annulirostris + Aedes vigilax) transmitting Ross River virus (published in Shocket et al. 2018 eLife, appendix)
- AenoRRVx = Aedes notoscriptus (+ Culex annulirostris + Aedes vigilax) transmitting Ross River virus (published in Shocket et al. 2018 eLife, appendix)
- AetrEEEV = Aedes triseriatus (+ Culiseta melanura + Culex pipiens) transmitting Eastern Equine Encephalitis virus (from Shocket et al. in prep)
- AeveRVFV = Aedes vexans (+ Culex pipiens + Aedes taeniorhynchus) transmitting Rift Valley Fever virus (from Shocket et al. in prep)
- AetaRVFV = Aedes taeniorhynchus (+ Culex spp. + Aedes vexans) transmitting Rift Valley Fever virus (from Shocket et al. in prep)
- AetaSINV = Aedes taeniorhynchus (+ Culex pipiens + Aedes vexans) transmitting Sindbis virus (from Shocket et al. in prep)
- AngaPfal = Anopheles gambiae transmitting Falciparum malaria (published in Johnson et al. 2015 Ecology)
- CxanRRVx = Culex annulirostris (+ Aedes vigilax) transmitting Ross River virus (published in Shocket et al. 2018 eLife)
- CxanMVEV = Culex annulirostris transmitting Murray Valley Encephalitis virus (published in Shocket et al. 2018 eLife, appendix)
- CxpiWNVx = Culex pipiens transmitting West Nile virus (from Shocket et al. in prep)
- CxpiSINV = Culex pipiens transmitting Sindbis virus (from Shocket et al. in prep)
- CxqiWNVx = Culex quinquefasciatus (+ Culex tarsalis) transmitting West Nile virus (from Shocket et al. in prep)
- CxtaWEEV = Culex tarsalis (+ Culex pipiens) transmitting Western Equine Encephalitis virus (from Shocket et al. in prep)
- CxtaWNVx = Culex tarsalis (+ Culex pipiens) transmitting West Nile virus (from Shocket et al. in prep)
CxtaSLEV = Culex tarsalis (+ Culex pipiens) transmitting - St. Louis Encephalitis virus (from Shocket et al. in prep)
- CxthRVFV = Culex thelieri (+ Culex spp + Aedes vexans) transmitting Rift Valley fever virus (from Shocket et al. in prep)
- CxunWNVx = Culex univittatus (+ Culex pipiens) transmitting West Nile virus (from Shocket et al. in prep)
- CsmeEEEV = Culiseta melanura (+ Aedes triseriatus + Culex pipiens) transmitting Eastern Equine Encephalitis virus (from Shocket et al. in prep)

Trait codes:
NOTE: not every trait appears in every model
- a = biting rate (1/day, often measured as inverse of gonotrophic cycle duration)
- b = vector competence (proportion of infected that successfully transmit)
- bc = vector competence (proportion of exposed that successfully transmit)
- c = vector competence (proportion of exposed that are successfully infected)
- e2a = egg-to-adult survival (proportion)
- EFD = fecundity (as eggs per female per day)
- EFOC = fecundity (as eggs per female per oviposition cycle)
- EPR = fecundity (as eggs per raft - needs to be multiplied by proportion ovipositing [pO] to get eggs per female per oviposition cycle)
- EV = egg viability (proportion hatching)
- lf = lifespan (days)
- MDR = mosquito development rate (1/day)
- mu = daily mortality (probability or proportion; AngaPfal model only)
- nLR = number of larvae per raft (# / 1000, fit needs to be * 1000/298.63 to turn into a proportion hatching ~= EV; CxanRRVx model only)
- PDR = pathogen development rate (1/day)
- pO = proportion of females ovipositing
- pRH = proportion of egg rafts that hatch (proportion; CxanRRVx model only)
- R0 = predicted relative R0 (transmission potential / thermal suitability for transmission) 
- T = temperature sequence, which varies between models