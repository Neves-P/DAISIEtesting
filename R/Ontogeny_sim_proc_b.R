### R code to run simulations described in "The effects of island ontogeny on
### species diversity and phylogeny"
### Authors: Luis Valente, Rampal Etienne, Albert Phillimore

#' Calculate island area at a given point in time
#'
#' Describe a quadratic relationship between time and area, or
#'   return a constant vale if constant area is assumed.
#' @param t A numeric with current time of simulation at which area should
#'   be calculated.
#' @param Apars A numeric vector with 4 elements:
#'   \itemize{
#'   \item{Apars[1]: The total time which the island is set to exist.}
#'   \item{Apars[2]: The maximum (peak) area.}
#'   \item{Apars[3]: The time at which the peak area will be achieved.}
#'   \item{Apars[4]: The sharpness of the curve. Usually set to 1. Higher
#'     values result in steeper curves.}
#'   }
#' @param shape A numeric either 0, for constant area, or 1, for a beta curve.
#'
#' @return Returns a numeric with area at time \code{t}, given the parametrs
#'
#' @examples
#' area <- DAISIEtesting:::island_area(
#'   t = 4,
#'   Apars = c(10, 5000, 2, 1),
#'   shape = 1
#' )
island_area <- function(t, Apars, shape) {
  if (shape == 0) {
    return(Apars[2])
    }
  if (shape == 1) {
    Tmax <- Apars[1] # total time
    Amax <- Apars[2] # maximum area
    Topt<-Apars[3] #peak position
    peak<-Apars[4] #peakiness - we specify a value of 1 but this is flexible.
    proptime<- t/Tmax
    f <-Topt/Tmax / (1 - Topt/Tmax)
    a <-f*peak/(1+f)
    b <-peak/(1+f)
    At<- Amax * proptime^a * (1 - proptime)^b / ((a/(a+b))^a * (b/(a+b))^b)
    return(At)
  }
  }

#Function to describe relationship between area and extinction rate
ext <- function(t, Apars, Epars, shape, extcutoff) {
  X <- log(Epars[1] / Epars[2]) / log(0.1)
  extrate <- Epars[1] / ((island_area(t, Apars, shape)/Apars[2])^X)
  extrate[which(extrate > extcutoff)] <- extcutoff
  return(extrate)
  }


#Function to describe relationship between area and cladogenesis rate
clado <- function(t, N, Apars, Spars, shape) {
  clado <- Spars[1] * island_area(t,Apars,shape) * (1 - N/(island_area(t,Apars,shape) * Spars[2]))
  return(clado)
}

# Apars= c(total_time, maximum_area, peak_position, peakiness)
# Spars = c(clado0, Kprime)


#Function to describe relationship between area and immigration rate
immi <- function(t, N, Apars, Spars, shape) {
  immi <- Spars[1] * (1 - N/(island_area(t,Apars,shape) * Spars[2]))
  return(immi)
  }
# Apars= c(total_time, maximum_area, peak_position, peakiness)
# Spars = c(immig0, Kprime)


#### THE MAIN MODEL
sim_island <- function(time,
                       mainland_n,
                       immig0,
                       mu_minimum,
                       mu_high,
                       ana0,
                       clado0,
                       samplefreq,
                       Kprime,
                       Amax,
                       Ashape,
                       Apeak,
                       proportiontosim) {
  extcutoff <- max(1000, 1000 * (ana0 + clado0 + immig0))
  g <- 0.5
  timeval <- 0
  mainland_spec <- seq(1, mainland_n, 1)
  maxspecID <- mainland_n
  island_spec <- c()
  #this is made up of 7 columns.
  #1= species ID,
  #2 = ID of mainland ancestor,
  #3 = time sice colonisation
  #4 = species status (I = immigrant, A = anagenesis, C = cladogenesis).
  #5 = sister species or lineage for cladogenetic species
  #6 = time since cladogenesis from extant relative
  #7 = origin of anagenetic species (speciation via anagenesis or extinction of cladogenetic sister species)
  MaxArea <- Amax
  if (samplefreq > 0) {
    island_community <- matrix(nrow = samplefreq + 1, ncol = 9)
    colnames(island_community) <- c("Island_Age",
                                    "Immig_rate",
                                    "Ext_rate",
                                    "Ana_rate",
                                    "Clado_rate",
                                    "Total_spp",
                                    "Immigrant_spp",
                                    "Ana_spp",
                                    "Clado_spp")
    island_community[, 1] <- seq(0, time, time / samplefreq)
    island_community[1,2:9]<-c(mainland_n*immig0,0,0,0,0,0,0,0)
    timeoflastevent<-0}
  #these are the entries at time=0.
  while(timeval < time * proportiontosim) {
    if (timeval>=Apeak) {
      MaxArea <- island_area(timeval,c(time,Amax,Apeak,1),Ashape)
      }
    #updates the area so it can decline.

    if (timeval < Apeak) {
      ext_rate_max <- ext(timeval, c(time, Amax, Apeak, 1), c(mu_minimum, mu_high),Ashape,extcutoff)*length(island_spec[,1])}
    else{
      ext_rate_max<-ext(timeval + g*(time - timeval),c(time,Amax,Apeak,1),c(mu_minimum,mu_high),Ashape,extcutoff)*length(island_spec[,1])}
    #this is a way of ensuring that the max extinction rate does not go too high and slow the algorithm down.

    if(Kprime==Inf){
      clado_rate_max<-clado0*MaxArea*length(island_spec[,1])
      immig_rate_max<-immig0*mainland_n}
    else{
      clado_rate_max<-max(c(length(island_spec[,1])*(clado0* MaxArea*(1 -length(island_spec[,1])/(MaxArea*Kprime))),0),na.rm=T)
      immig_rate_max<-max(c(immig0*mainland_n*(1 -length(island_spec[,1])/(MaxArea*Kprime)),0),na.rm=T)}

    ana_rate<-ana0*length(which(island_spec[,4]=="I"))


    totalrate<-ext_rate_max+clado_rate_max+ana_rate+immig_rate_max

    dt<-rexp(1,totalrate)

    timeval <- timeval +dt

    if(timeval<=time*proportiontosim){
      if(timeval-dt < Apeak && timeval>Apeak){timeval<-Apeak}
      else{if(timeval >= Apeak && timeval > timeval - dt + g *(time - timeval + dt)){timeval <- timeval - dt + g*(time - timeval + dt)}
        else{

          possible_event<-sample(1:4,1,replace=FALSE,c(immig_rate_max,ext_rate_max,ana_rate,clado_rate_max))

          #####################################################
          #In this section we look back over the period since the last timeval and insert rates at the specificied timeslices (given by samplefreq).

          if(samplefreq>0){
            rowstowrite<-intersect(which(island_community[,"Island_Age"]>timeoflastevent),which(island_community[,"Island_Age"]<=timeval))
            if(length(rowstowrite)>0){

              timeslices<-island_community[rowstowrite,"Island_Age"]

              length(island_spec[,1])->total_spec
              length(which(island_spec[,4]=="I"))->total_immig
              length(which(island_spec[,4]=="A"))->total_ana
              length(which(island_spec[,4]=="C"))->total_clado


              islandwide_ext_rate_slice<-length(island_spec[,1])*ext(timeslices,c(time,Amax,Apeak,1),c(mu_minimum,mu_high),Ashape,extcutoff)

              islandwide_immig_rate_slice<-mainland_n*immi(timeslices,length(island_spec[,4]),c(time,Amax,Apeak,1),c(immig0,Kprime),Ashape)

              islandwide_ana_rate_slice<-length(which(island_spec[,4]=="I"))*ana0

              islandwide_clado_rate_slice<-length(island_spec[,1])*clado(timeslices,length(island_spec[,4]),c(time,Amax,Apeak,1),c(clado0,Kprime),Ashape)

              islandwide_immig_rate_slice[which(islandwide_immig_rate_slice<0)]<-0
              islandwide_clado_rate_slice[which(islandwide_clado_rate_slice<0)]<-0
              islandwide_immig_rate_slice[which(is.na(islandwide_immig_rate_slice)==TRUE)]<-0
              islandwide_clado_rate_slice[which(is.na(islandwide_clado_rate_slice)==TRUE)]<-0


              island_community[rowstowrite,2:9]<-matrix(ncol=8,nrow=length(rowstowrite),cbind(islandwide_immig_rate_slice, islandwide_ext_rate_slice, islandwide_ana_rate_slice, islandwide_clado_rate_slice,rep(total_spec,length(rowstowrite)),rep(total_immig,length(rowstowrite)),rep(total_ana,length(rowstowrite)),rep(total_clado,length(rowstowrite))))
            }
            timeoflastevent<-timeval}

          ######################################################

          if(timeval<=time){

            ##########################################
            #IMMIGRATION
            if(possible_event==1){

              maximm<-immig0*(1-(length(island_spec[,4])/(Kprime* MaxArea)))

              if(max(c(0,immi(timeval,length(island_spec[,4]),c(time,Amax,Apeak,1),c(immig0,Kprime),Ashape))) / maximm >= runif(1)){

                colonist<-sample(mainland_spec,1)

                if(length(island_spec[,1])!=0){
                  isitthere<-which(island_spec[,1]==colonist)}

                if(length(island_spec[,1])==0){
                  isitthere<-c()}

                if(length(isitthere)==0){
                  island_spec<-rbind(island_spec,c(colonist,colonist,timeval,"I",NA,NA,NA))}

                if(length(isitthere)!=0){
                  island_spec[isitthere,]<-c(colonist,colonist,timeval,"I",NA,NA,NA)}
              }
            }

            ##########################################
            #EXTINCTION
            if(possible_event==2){

              maxmu<-ext(0,c(time,Amax,Apeak,1),c(mu_minimum,mu_high),Ashape,extcutoff)

              if(ext(timeval,c(time,Amax,Apeak,1),c(mu_minimum,mu_high),Ashape,extcutoff) / maxmu >= runif(1)){
                extinct<-sample(1:length(island_spec[,1]),1)
                #this chooses the row of species data to remove

                typeofspecies<-island_spec[extinct,4]

                if(typeofspecies=="I"){island_spec<-island_spec[-extinct,]}
                #remove immigrant

                if(typeofspecies=="A"){island_spec<-island_spec[-extinct,]}
                #remove anagenetic

                if(typeofspecies=="C"){
                  #remove cladogenetic

                  #first find species with same ancestor AND arrival time
                  sisters<-intersect(which(island_spec[,2]==island_spec[extinct,2]),which(island_spec[,3]==island_spec[extinct,3]))
                  survivors<-sisters[which(sisters!=extinct)]

                  if(length(sisters)==2){
                    #survivors status becomes anagenetic
                    island_spec[survivors,4]<-"A"
                    island_spec[survivors,c(5,6)]<-c(NA,NA)
                    island_spec[survivors,7]<-"Clado_extinct"
                    island_spec<-island_spec[-extinct,]
                  }

                  if(length(sisters)>=3){
                    numberofsplits<-nchar(island_spec[extinct,5])

                    mostrecentspl<-substring(island_spec[extinct,5],numberofsplits)

                    if(mostrecentspl=="B"){ sistermostrecentspl<-"A" }
                    if(mostrecentspl=="A"){ sistermostrecentspl<-"B" }

                    motiftofind<-paste(substring(island_spec[extinct,5],1,numberofsplits-1),sistermostrecentspl,sep="")

                    possiblesister<-survivors[which(substring(island_spec[survivors,5],1,numberofsplits)==motiftofind)]

                    #different rules depending on whether a B or A is removed. B going extinct is simpler because it only carries a record of the most recent speciation
                    if(mostrecentspl=="A"){
                      #change the splitting date of the sister species so that it inherits the early splitting that used to belong to A.
                      tochange<-possiblesister[which(island_spec[possiblesister,6]==max(as.numeric(island_spec[possiblesister,6])))]
                      island_spec[tochange,6]<-island_spec[extinct,6]
                    }

                    #remove the offending A/B from these species
                    island_spec[possiblesister,5]<-paste(substring(island_spec[possiblesister,5],1,numberofsplits-1),substring(island_spec[possiblesister,5],numberofsplits+1,nchar(island_spec[possiblesister,5])),sep="")
                    island_spec<-island_spec[-extinct,]
                  }


                }

                island_spec<-rbind(island_spec)
              }}

            ##########################################
            #ANAGENESIS
            if(possible_event==3){

              immi_specs<-which(island_spec[,4]=="I")
              #we only allow immigrants to undergo anagenesis
              if(length(immi_specs)==1){
                anagenesis<-immi_specs}
              if(length(immi_specs)>1){
                anagenesis<-sample(immi_specs,1)}

              if(length(immi_specs)>=1){

                maxspecID<-maxspecID+1
                island_spec[anagenesis,4]<-"A"
                island_spec[anagenesis,1]<-maxspecID
                island_spec[anagenesis,7]<-"Immig_parent"
              }}

            ##########################################
            #CLADOGENESIS - this splits species into two new species
            if(possible_event==4){
              maxclad<-((clado0 *MaxArea)*(1-(length(island_spec[,4])/(Kprime* MaxArea))))

              if(max(c(0,clado(timeval,length(island_spec[,4]),c(time, Amax,Apeak,1),c(clado0,Kprime),Ashape))) / maxclad >= runif(1)){


                tosplit<-sample(1:length(island_spec[,1]),1)

                #if the species that speciates is cladogenetic
                if(island_spec[tosplit,4]=="C"){

                  #for daughter A

                  island_spec[tosplit,4]<-"C"
                  island_spec[tosplit,1]<-maxspecID+1
                  oldstatus<-island_spec[tosplit,5]
                  island_spec[tosplit,5]<-paste(oldstatus,"A",sep="")
                  #island_spec[tosplit,6]<-timeval
                  island_spec[tosplit,7]<-NA

                  #for daughter B
                  island_spec<-rbind(island_spec,c(maxspecID+2,island_spec[tosplit,2],island_spec[tosplit,3],"C",paste(oldstatus,"B",sep=""),timeval,NA))

                  maxspecID<-maxspecID+2
                }else{
                  #if the species that speciates is not cladogenetic

                  #for daughter A

                  island_spec[tosplit,4]<-"C"
                  island_spec[tosplit,1]<-maxspecID+1
                  island_spec[tosplit,5]<-"A"
                  island_spec[tosplit,6]<-island_spec[tosplit,3]
                  island_spec[tosplit,7]<-NA

                  #for daughter B
                  island_spec<-rbind(island_spec,c(maxspecID+2,island_spec[tosplit,2],island_spec[tosplit,3],"C","B",timeval,NA))

                  maxspecID<-maxspecID+2
                }
              }
            }
          }
        }
      }
    }
  }

  if (samplefreq > 0) {
    rowstowrite<-intersect(which(island_community[,"Island_Age"]>timeoflastevent),which(island_community[,"Island_Age"]<=timeval))
    if(length(rowstowrite)>0){
      timeslices<-island_community[rowstowrite,"Island_Age"]
      length(island_spec[,1])->total_spec
      length(which(island_spec[,4]=="I"))->total_immig
      length(which(island_spec[,4]=="A"))->total_ana
      length(which(island_spec[,4]=="C"))->total_clado
      islandwide_ext_rate_slice<-length(island_spec[,1])*ext(timeslices,c(time,Amax,Apeak,1),c(mu_minimum,mu_high),Ashape,extcutoff)
      islandwide_immig_rate_slice<-mainland_n*immi(timeslices,length(island_spec[,4]),c(time,Amax,Apeak,1),c(immig0,Kprime),Ashape)
      islandwide_ana_rate_slice<-length(which(island_spec[,4]=="I"))*ana0
      islandwide_clado_rate_slice<-length(island_spec[,1])*clado(timeslices,length(island_spec[,4]),c(time,Amax,Apeak,1),c(clado0,Kprime),Ashape)
      islandwide_immig_rate_slice[which(islandwide_immig_rate_slice<0)]<-0
      islandwide_ana_rate_slice[which(islandwide_ana_rate_slice<0)]<-0
      islandwide_clado_rate_slice[which(islandwide_clado_rate_slice<0)] <- 0
      islandwide_immig_rate_slice[which(is.na(islandwide_immig_rate_slice) == TRUE)] <- 0
      islandwide_ana_rate_slice[which(is.na(islandwide_ana_rate_slice) == TRUE)] <- 0
      islandwide_clado_rate_slice[which(is.na(islandwide_clado_rate_slice) == TRUE)] <- 0
      island_community[rowstowrite, 2:9] <- matrix(ncol = 8,
                                                   nrow = length(rowstowrite),
                                                   cbind(islandwide_immig_rate_slice,
                                                         islandwide_ext_rate_slice,
                                                         islandwide_ana_rate_slice,
                                                         islandwide_clado_rate_slice,
                                                         rep(total_spec, length(rowstowrite)),
                                                         rep(total_immig, length(rowstowrite)),
                                                         rep(total_ana, length(rowstowrite)),
                                                         rep(total_clado, length(rowstowrite))))
      }
    island_community[samplefreq + 1, 2:9] <- 0
    return(island_community)
  }
}
