$Title Strategic Network and generation EXpansion planning in the Electricity Market (STEXEM)
*$EOLCOM //

$OnText

 Developed by

    Isaac Camilo González Romero
    Instituto de Investigación Tecnológica
    isaac.gonzalez@iit.comillas.edu
    Septiembre, 2017

$OffText

    $$OnEmpty OnMulti OffListing Oneps
    $$ifthen                       %gams.user2% == ""
    $$setglobal OptSkipExcelInput  0
    $$else
    $$setglobal OptSkipExcelInput  %gams.user2%
    $$endif
    $$ifthen                       %gams.user3% == ""
    $$setglobal OptSkipExcelOutput 0
    $$else
    $$setglobal OptSkipExcelOutput %gams.user3%
    $$endif
    $$OnEmpty OnMulti OffListing

////////////////////////////    OPTIMIZER OPTIONS     //////////////////////////////////
    OPTION   lp  = cplex    ;
    OPTION  mip  = cplex    ;
    OPTION rmip  = gurobi   ;
    OPTION  nlp  = CONOPT   ;
    option miqcp = gurobi   ;
    option minlp = baron    ;
    option mpec  = nlpec    ;
    
    FILE     GOPT / gurobi.opt /
    PUT      GOPT / 'RINS  5000' / 'writeprob equation.lp'  // 'nonconvex = 2'/  //' barqcpconvtol= 0.0000001' / 'optimalitytol= 0.0000001' 
    PUTCLOSE GOPT ;

    OPTION optcr     =       0;   // tolerance to solve MIP until IntGap < OptcR
    OPTION reslim    =  200000;   // maximum run time [sec]
    OPTION threads   =     -1 ;   // number of cores
    OPTION solprint  =    off ;   // print the final solution in the .lst file
    OPTION limrow    =   1000 ;   // maximum number of equations in the .lst file
    OPTION limcol    =   1000 ;   // maximum number of variables in the .lst file
    OPTION bratio    =     0  ;   // 1 doesn't takes previous results
    OPTION decimals  =      6 ;
    OPTION savepoint =       1;

    FILE     CONOPT / CONOPT.opt /
    PUT      CONOPT /'RTNWMA =0.0000001'/
    PUTCLOSE CONOPT ;


////////////////////////////          SETS            //////////////////////////////////
 sets
 // Temporal Indixes
             
   q              /q1*q32/
   p              periods
   pa(p)          active periods
   ps(p)          window periods
   pt(p)          active + window periods
   rp             representative periods
   rpp(rp,p)      relation between representative periods and periods
   y              year
   z (y)          year
   p1(p)          first period
   pn(p)          last period
   IndexRP (p,p)  representative periods and periods
   gr             groups



 // Generators Indixes
   th          technologies
   g           generating   unit
   t (g)       thermal      unit
   h (g)       hydro plant  unit
   hr(g)       Hydraulic (or storage) Generators with        reservoir
   hf(g)       Hydraulic (or storage) Generators with "fast" reservoir (i.e. battery)
   hs(g)       Hydraulic (or storage) Generators with "slow" reservoir (i.e. hydro  )
   w (g)       renewables   unit
   wn(g)       wind unit
   sr(g)       Solar
   cp          companies
   gcp(g,cp)   univocal correspondage from generator to companies
   tg(g,th)    corresopndance generators technology

 // Network Related indexes
   d             node (bus)
   lc   (  d,d ) candidate lines
   le   (  d,d ) existing  lines
   la   (  d,d ) existing  and   candidate lines
   ged  (g,d   ) existing  unit  located at node d
   gedgr(g,d,gr) existing  unit  located at node group g
   gcd  (g,d   ) candidate unit  located at node
   gad  (g,d   ) existing + candidates lines
   gnd  (g,d   ) all psble units located at node d
   dgr  (  d,gr) relation b


 // Aditional Indexes
   c          Result printing /1*7/
   f          Number of loops /1/
   k          steps for aproximation

 ;
 // Alias
   alias (cp,cpp)         ;
   alias (d,dd,di,df)     ;
   alias (rp,rrpp)        ;
   alias (p,pp,ppp,pppp)  ;



////////////////////////////       PARAMETERS         //////////////////////////////////  
 parameters
 // Generation parameters
   pSlopeVarCost  (      g    )   slope     variable cost             [M€ per GWh]
   pInterVarCost  (      g    )   intercept variable cost             [M€ per   h]
   pStartupCost   (      g    )   startup            cost             [M€]
   pGenInvCost    (      g    )   fixed    cost                       [M€]
   pMaxProdGen    (      g    )   maximun output per generator        [GW]
   pMaxProd       (      g,d  )   maximum output per node             [GW]
   pMinProd       (      g    )   minimum output                      [GW]
   pEFOR          (      g    )   EFOR                                [p.u.]
   pWindGen       ( p,   g    )   maximun wind generation             [GW]
   pMaxWindGen    (      g,d  )   maximun wind generation per node    [GW]
   pSolarGen      ( p,   g    )   solar generation                    [GW]
   pSolarGenNode  ( p,   g,d  )   maximun solar generation            [GW]
   pMaxSolarGen   (      g,d  ) 
   pWindGenNode   ( p,   g,d  )   maximun wind generation             [GW]      
   pEmissionrate  (     th    )   emission rate                       ton C02 per GWh
   pTotalEmission (     th    )   total emissions per technology      [tons of C02]
   pSolpe         ( p       )   Hourly Slope 
   pENSCost                       energy non-served cost              [M€ per GWh]
   pPNSCost                       power  non-served cost              [M€ per GW ]
   PCO2price                      price of co2 emisisons              [M€ per ton]
   pHydroCost                     cost for hydro production           [M€ per GWh]
   pConsumCost                    consumption cost                    [M€ per GWh]
   
 // Demand parameters
   pDemand        (  p        )   hourly load by perio                [GW]
   
   pDemandNode    (y,p,d      )   hourly load by node                 [GW]
   pDemIncr       (y          )   yearly demand increment             [p.u.]
   pCumDemIncr    (y          )   cum yearly demand increment         [p.u.]
   pOrder         (y          )   ordinal of the year                 [p.u.]
   pFixedDemand   (  p,d      )   hourly load by node                 [GW]
   pEnergy        (th         )   energy per technology               [GW]  
   pShareGroup    (d,gr       )   share of each node in each group    [GW] 

 // Results Parameter
  pInstalLine    (y,      d,d)   Lines Installed                     [p.u.]
   pInstalCapT    (      g,d  )   generation capacity installed       [p.u.]
   pInstalCapT_can(y,    g,d  )   generation capacity installed       [p.u.]
   pInstalCapTot  (     th    )   generation capacity installed       [p.u.]
   pProduct       (y,p,th     )   generation Output per tech          [GW]
   pProduct_FX    (y,p,  g,d  )   generation output                   [GW]
   pProduct_all   (y,p,th     )   generation output per tech          [GW]
   pFlow          (y,p,    d,d)   flows through lines                 [GW]
   pTheta         (y,p,      d)   angles                              [rad]
   pReserve       (y,p,  g    )   reserve capacity per period         [GW]
   pInflows       (  p,  g    )   inflows                             [GW]
   pWindSpillage  (y,p,  g,  d)   wind spillages                      [GW]
   pNewGen_FX     (y,    g,  d)   new generation                      [1-0]
   pReserve_FX    (y,p,  g,  d)   reserve   per period and node       [GW]
   pSpillage_FX   (y,p,  g,  d)   spillages per period and node       [GW]
   pPrices        (y,p,    d  )   hourly nodal prices                 [M€ per GWh]
   pAveragePrice  (y,      d  )   average nodal prices                [M€ per GWh]
   pMarginalCosts (      g    )   total marginal costs                [M€ per GWh]
   pDemandl       (  p,    d  )   total demand                        [GW]
   pBenefits      (cp )           benefits of the company             [M€]  
   pTotalCost1                    total cost of the system            [M€]
   pTotalCost2                    total cost of the system            [M€]
   pExtendedWelfare               total welfare of the system         [M€]
   pInvFixedCost                  total fixed cost of investment      [M€]  
   pConsumer_Surplus              total surplus                       [M€] 
   pObjFunct     (q,c)            objective funciton parameter        [M€]
   pMaxNewGen                     maximun generators to install       [p.u.]   
   pBilevel                       bilevel excecution parameter        [yes-no]  
   pCongestionRents               total congestion rents received
   pConsump  

 // Network Parameters
   pX            (        d,d)   line reactance                     [p.u.]
   pG            (        d,d)   line conductance                   [p.u.]
   pTTC          (        d,d)   total transfer capacity            [GW]
   pFixedCost    (        d,d)   fixed    cost                      [M€]
   pDemShare     (          gr)   demand share                       [p.u.]
   pSbase                        base power                         [GW]
   pMaxtheta                     maximun angle                      [p.u.]

 // Storage Parameters
   pMaxReserve   (      g    )    maximum reserve                     [GW]
   pMinReserve   (      g    )    minimum reserve                     [GW]
   pIniReserve   (      g    )    initial reserve                     [GW]
   pProdFunct    (      g    )    production function                 [GWh per km3]
   pEffic        (      g    )    pumping efficiency                  [p.u.]
   pMaxCons      (      g    )    maximum consuption of pump units    [GW]
   pFinReserve   (      g    )    final reserve state                 [GW]
   pEprMax       (      g    )    Epr Max                             [p.u.]
   pEprMin                        Epr Min                             [p.u.]

 // Parameters for representatives periods formulation
   pWeight_rp      (rp   )       Representatives periods weight [h]
   pNumPer_rp      (rp   )       Number of periods at each representative period [h]
   pFirstP_rp      (rp,p )       First period of each representative period
   pTransMatrix_rp (rp,rp)       Transition matrixnumber of jumps from rp to rrpp
   pNumPerMinus1_rp(rp   )       Number of periods minus 1 at each representative period [h]
   pMaxTM_rp                     Maximum value of representative period transition matrix
   pMinTransition                Minimum transition to be considered in RP_TM model
   pStorMovWindow                Storage moving window for representative periods [h]
   pLastStorMovW                 parameter to indicate the last hour on ps(p) set

 // Market Model
   Slope         (   p,d    )    
   ConjcVar      (  cp      )    conjectural Variation 0=perfect competition inf = monopoly
   conjcvariation(   p,g,d  )    conjectured variations

 // parameters linearization
   M_alpha_up1                    Big M for max prod existing units
   M_cRo_up1                      Big M for max prod existing units
   M_cRo_lo1                      Big M for min prod existing units
   M_cLambda                      Big M for maximun price
   M_Omega_up1                    Big M for max prod new units
   M_Omega_lo1                    Big M for min prod new units
   M_Betha1                       Big M for cumulative generation
   M_omic_up1                     Big M for max new generation
   M_omic_lo1                     Big M for min new generation
   M_theta_up1                    Big M for linearization for max theta
   M_theta_lo1                    Big M for linearization for min theta
   M_ji_up1                       Big M for linearization for max ens
   M_ji_lo1                       Big M for linearization for min ens
   M_Kappa_up1                    Big M for linearization for max cons existing units
   M_Kappa_lo1                    Big M for linearization for min cons existing units
   M_Delta_lo1                    Big M for linearization for min reserve
   M_Delta_up1                    Big M for linearization for max reserve
   M_Mu_lo1                       Big M for linearization for min spillage
   M_Mu_up1                       Big M for linearization for max spillage
   M_KappN_up1                    Big M for linearization for max cons existing units
   M_KappN_lo1                    Big M for linearization for min cons existing units
   M_DeltN_lo1                    Big M for linearization for min reserve
   M_DeltN_up1                    Big M for linearization for max reserve
   M_DeltN_lo2                    Big M for linearization for final reserve
   M_MuN_lo1                      Big M for linearization for min spillage
   M_MuN_up1                      Big M for linearization for max spillage
   M_phi_lo1                      Big M for linearization for min flow through lines
   M_phi_up1                      Big M for linearization for max flow through lines
   M_zeta_up1                     Big M for max flow through new lines
   M_zeta_lo1                     Big M for min flow through new lines
   M_tau_up1                      Big M for     flow through new lines
   M_tau_lo1                      Big M for     flow through new lines
   M_tau_up2                      Big M for     flow through new lines
   M_tau_lo2                      Big M for     flow through new lines
   M_cRo_up2                      Big M for max prod existing units
   M_cRo_lo2                      Big M for min prod existing units
   M_Omega_up2                    Big M for max prod new units
   M_Omega_lo2                    Big M for min prod new units
   M_Betha1                       Big M for cumulative generation
   M_omic_up2                     Big M for max new generation
   M_omic_lo2                     Big M for min new generation
   M_theta_up2                    Big M for linearization for max theta
   M_theta_lo2                    Big M for linearization for min theta
   M_ji_up2                       Big M for linearization for max ens
   M_ji_lo2                       Big M for linearization for min ens
   M_Kappa_up2                    Big M for linearization for max cons existing units
   M_Kappa_lo2                    Big M for linearization for min cons existing units
   M_Delta_lo2                    Big M for linearization for min reserve
   M_Delta_up2                    Big M for linearization for max reserve
   M_Mu_lo2                       Big M for linearization for min spillage
   M_Mu_up2                       Big M for linearization for max spillage
   M_KappN_up2                    Big M for linearization for max cons existing units
   M_KappN_lo2                    Big M for linearization for min cons existing units
   M_DeltN_lo2                    Big M for linearization for min reserve
   M_DeltN_up2                    Big M for linearization for max reserve
   M_MuN_lo2                      Big M for linearization for min spillage
   M_MuN_up2                      Big M for linearization for max spillage
   M_phi_lo2                      Big M for linearization for min flow through lines
   M_phi_up2                      Big M for linearization for max flow through lines
   M_zeta_up2                     Big M for max flow through new lines
   M_zeta_lo2                     Big M for min flow through new lines
   M                              Multiplying M.
   M_dualpf1                      Big M for linearization product price flow
   M_dualpf2                      Big M for linearization product price flow
   M_tau_up1                      Big M for linearization product price flow
   M_tau_up2                      Big M for linearization product price flow

 // Excecution Parameters

   OF_Cost              (*  )     Total cost model                   [k€]
   GenCPUTime           (*  )     Generation CPU time                [ s]
   SolCPUTime           (*  )     Solve      CPU time                [ s]
   NumVar               (*  )     Number of variables
   NumDVar              (*  )     Number of discrete variables
   NumEqu               (*  )     Number of equations
   NumNZ                (*  )     Number of nonzero entries in the model coefficient matrix
   BestSol              (*  )     The estimate of the best possible solution for a MIP
   OF_ExtWel            (*  )     Extended cost
   ENS_Cost             (*  )     Energy non served cost
   OF_current           (*  )     Current Objective function
   pzero                          Zero for the tolerence of our model
   pTypeComp                      Type of competition (perfect = yes - imperfect = no)
   pLossMod                       Loss model= yes Losses model = no
   pCost
   Profits


 // Efficient Method
  pRo
  pEps;

////////////////////////////       VARIABLES          //////////////////////////////////

 // **** GEP/TEP variables ****
   binary   variables
   vNewLine      (y,    d,d)   line investment decision           [0-1]

 //***************************** GEP variables *****************************
  positive variables
 // Relaxed variables, used for some of the algorithms.
   vNewLine_R    (y,    d,d)   line investment decision           [0-1]
   vNewGen       (y,  g,d  )   cumulative installation decision   [0-1]

 // ****************************  Primal variables  ************************************
   vReserve      (y,p,g,d  )   reserve at the end of period       [GW]
   vReserve_ini  (y,p,g,d  )   reserve at the end of period       [GW]
   vSpillage     (y,p,g,d  )   water spillage                     [GW]
   vConsump      (y,p,g,d  )   consumption of the unit            [GW]
   vProduct      (y,p,g,d  )   production  of the unit            [GW]
   vENS          (y,p,  d  )   energy non served                  [GW]
   vWind         (y,p,g,d  )   wind energy                        [GW]
   vSolar        (y,p,g,d  )   solar energy                       [GW]
   ;
  variables
   vFlow         (y,p,  d,d)   flow                               [GW]
   vTheta        (y,p,    d)   voltage angle                      [rad]
   vTotalTCost                 total system          cost         [M€]
   vTotalFCost                 total system fixed    cost         [M€]
   vTotalVCost                 total system variable cost         [M€]
   vDummyCost                  dummy cost for nlp                 [M€]   
   vDemand       (y,p,    d)   demand at node d                   [GW]
   ;

 // ****************************   Dual variables   ************************************
  positive variables
   b_Alpha_up    (y,p,  d  )   dual for min demand
   b_cRo_up      (y,p,g,d  )   dual for max prod existing units
   b_cRo_lo      (y,p,g,d  )   dual for min prod existing units
   b_cRo_up_w    (y,p,g,d  )   dual for max prod wind
   b_cRo_lo_w    (y,p,g,d  )   dual for min prod wind
   b_Omega_up    (y,p,g,d  )   dual for max prod new units
   b_Omega_lo    (y,p,g,d  )   dual for min prod new units
   b_Omega_up_w  (y,p,g,d  )   dual for max new prod wind
   b_Omega_lo_w  (y,p,g,d  )   dual for min new prod wind
   b_cLambda     (y,p,  d  )   dual for power balanance
   b_phi_up      (y,p,  d,d)   dual for max flow through ext lines
   b_phi_lo      (y,p,  d,d)   dual for min flow through ext lines
   b_tau_up      (y,p,  d,d)   dual for     flow through new lines
   b_tau_lo      (y,p,  d,d)   dual for     flow through new lines
   b_zeta_up     (y,p,  d,d)   dual for max flow through new lines
   b_zeta_lo     (y,p,  d,d)   dual for min flow through new lines
   b_Kappa_up    (y,p,g,d  )   dual for max cons existing units
   b_Kappa_lo    (y,p,g,d  )   dual for max cons existing units
   b_Delta_up    (y,p,g,d  )   dual for max reservoir capacity
   b_Delta_lo    (y,p,g,d  )   dual for min reservoir capacity
   b_Mu_lo       (y,p,g,d  )   dual for min spillage
   b_Mu_up       (y,p,g,d  )   dual for max spillage
   b_KappN_lo    (y,p,g,d  )   dual for max new cons existing units
   b_KappN_up    (y,p,g,d  )   dual for max new cons existing units
   b_DeltN_lo    (y,p,g,d  )   dual for max new reservoir capacity
   b_DeltN_up    (y,p,g,d  )   dual for min new reservoir capacity
   b_muN_lo      (y,p,g,d  )   dual for min new spillage
   b_muN_up      (y,p,g,d  )   dual for max new spillage
   ;
  variables
   //b_theta_up    (y,p,  d  )   dual for max theta
   //b_theta_lo    (y,p,  d  )   dual for min theta
   //b_Betha       (y,  g,d  )   dual for cumulative generation
   //b_omic_up     (y,  g,d  )   dual for upper binary condition
   //b_omic_lo     (y,  g,d  )   dual for lower binary condition
   b_ThetDel     (y,p,  d,d)   dual for delta tetha
   b_ji_lo       (y,p,  d  )   dual for min ens
   b_ji_up       (y,p,  d  )   dual for max ens
   b_dualpf1     (y,p,di,df)   dual for auxiliary variable
   b_dualpf2     (y,p,di,df)   dual for auxiliary variable
   b_phi         (y,p,  d,d)   dual for     flow through ext lines
   b_Psi         (y,p,g,d  )   dual for storage balance
   b_zeus1       (y,p,g,d  )   dual for slow storage units
   b_zeus2       (y,p,g,d  )   dual for slow storage units (last hour)
   ;
 //***************************** MILP-GEP variables ************************************
  binary   variables
   vBinFlow      (y,p,d,d,k)   binary variable for linearization of flow variable
   Y_alpha_up    (y,p,  d  )   Binary for linearization for min demand
   Y_cRo_up      (y,p,g,d  )   Binary for linearization for max prod existing units
   Y_cRo_up_w    (y,p,g,d  )   Binary for linearization for max prod wind existing units
   Y_cRo_lo      (y,p,g,d  )   Binary for linearization for min prod existing units
   Y_cRo_lo_w    (y,p,g,d  )   Binary for linearization for min prod wind existing units
   Y_Omega_lo    (y,p,g,d  )   Binary for linearization for min prod new units
   Y_Omega_up    (y,p,g,d  )   Binary for linearization for max prod new units
   Y_Omega_lo_w  (y,p,g,d  )   Binary for linearization for max prod wind new units
   Y_Omega_up_w  (y,p,g,d  )   Binary for linearization for max prod wind new units
   Y_Betha       (y,  g,d  )   Binary for linearization for cumulative generation
   Y_omic_up     (y,  g,  d)   Binary for linearization for max new generation
   Y_omic_lo     (y,  g,  d)   Binary for linearization for min new generation
   Y_theta_up    (y,p,  d  )   Binary for linearization for max theta
   Y_theta_lo    (y,p,  d  )   Binary for linearization for min theta
   Y_Kappa_up    (y,p,g,d  )   Binary for linearization for max cons existing units
   Y_Kappa_lo    (y,p,g,d  )   Binary for linearization for min cons existing units
   Y_Delta_lo    (y,p,g,d  )   Binary for linearization for min reserve
   Y_Delta_lof   (y,p,g,d  )   Binary for linearization for min reserve
   Y_Delta_up    (y,p,g,d  )   Binary for linearization for max reserve
   Y_Delta_lo2   (y,p,g,d  )   Binary for linearization for final reserve
   Y_Mu_lo       (y,p,g,d  )   Binary for linearization for min spillage
   Y_Mu_up       (y,p,g,d  )   Binary for linearization for max spillage
   Y_KappN_up    (y,p,g,d  )   Binary for linearization for max new cons existing units
   Y_KappN_lo    (y,p,g,d  )   Binary for linearization for min new cons existing units
   Y_DeltN_lo    (y,p,g,d  )   Binary for linearization for min new reserve
   Y_DeltN_up    (y,p,g,d  )   Binary for linearization for max new reserve
   Y_MuN_lo      (y,p,g,d  )   Binary for linearization for min new spillage
   Y_MuN_up      (y,p,g,d  )   Binary for linearization for max new spillage
   Y_ji_up       (y,p,  d  )   Binary for linearization for max ens
   Y_ji_lo       (y,p,  d  )   Binary for linearization for min ens
   Y_phi_lo      (y,p,  d,d)   Binary for linearization for min flow through lines
   Y_phi_up      (y,p,  d,d)   Binary for linearization for max flow through lines
   Y_zeta_up     (y,p,  d,d)   Binary for linearization for max flow through new lines
   Y_zeta_lo     (y,p,  d,d)   Binary for linearization for min flow through new lines
   Y_tau_up      (y,p,  d,d)   Binary for linearization for     flow through new lines
   Y_tau_lo      (y,p,  d,d)   Binary for linearization for     flow through new lines
   Y_phi_app     (y,p,  d,d)   Binary for linearization for loses aproximation
   Y_dualpf1     (y,p,  d,d)   Binary for linearizaiton for product prices flows
   Y_dualpf2     (y,p,  d,d)   Binary for linearizaiton for product prices flows

 //**************************** Auxiliary variables ***********************************
   positive variable
   vBinPrice1FLow (y,p,d,d,k)  binary variable for linearization of price flow variable
   vBinPrice2FLow (y,p,d,d,k)  binary variable for linearization of price flow variable
   ;
   variables
   vPriceFlow1    (y,p,  d,d)  auxiliary variables for linearization of priceflow product
   vPriceFlow2    (y,p,  d,d)  auxiliary variables for linearization of priceflow product;


////////////////////////////  EQUATION DECLARATION    //////////////////////////////////
 equations
 // Objective Function

   eTotalTCost                  total system cost                   [M€]
   eTotalFCost_MinCost          total fixed min-cost                [M€]
   eTotalVCost_MinCost          total variable min- cost            [M€]
   eFCost_Bilevel               total bilevel cost                  [M€]
   eFCost_Bilevel_R             total bilevel cost relaxed          [M€]
   eFCost_MPEC                  total cost for MPEC model           [M€]
   eFExtendedCost               total extended cost for cournot     [M€]
   eOF                          Dummy objective function            [M€]   
   eMerchanInvestor             merchant investor revenues          [M€]

 // Balance Equation
   eBalance_C_can  (y,p,   d  ) load generation balance            [GW]
   eMinDemand      (y,p,   d  )

 // Generation Constraints

   eMaxProd        (y,p, g, d)  max generators production           [GW]
   eMinProd        (y,p, g, d)  min generators production           [GW]
   eMaxProd_New    (y,p, g, d)  min prod some candidates R          [GW]
   eMinProd_New    (y,p, g, d)  max prod some candidates R          [GW]
   eGenInst_R_can  (y,   g, d)  Cumulative generation installation  [p.u.]
   eMaxiGen_R_can  (y,   g, d)  maximun inslatted generation        [p.u.]
   eMiniGen_R_can  (y,   g, d)  minimun installed generation        [p.u.]
   eMaxWind        (y,p, g, d)  max wind production                 [GW]
   eMinWind        (y,p, g, d)  min wind production                 [GW]
   eMaxWindNew     (y,p, g, d)  max new wind production             [GW]
   eMinWindNew     (y,p, g, d)  min new wind production             [GW]
   eMaxSolar       (y,p, g, d)  max solar production                [GW]
   eMinSolar       (y,p, g, d)  min solar production                [GW]
   eMaxSolarNew    (y,p, g, d)  max new solar production            [GW]
   eMinSolarNew    (y,p, g, d)  min new solar production            [GW]
   BudgetSolar     (y,   g, d)  max solar investment                [GW]
   BudgetWind      (y,   g, d)  max wind investment                 [GW]
 // Network Constraints
   eCumLineInstall (y,    d,d)  cumulative lines installation       [GW]
   eCumLineInstallR(y,    d,d)  cumulative lines installation R     [GW]
   eFlowMax        (y,p,  d,d)  max flow for existing capacity      [GW]
   eFlowMin        (y,p,  d,d)  max flow for existing capacity      [GW]
   eFlowExisting1  (y,p,  d,d)  DC aproximation
   eFlowExisting2  (y,p,  d,d)  DC aproximation
   eFlowInstlCap1  (y,p,  d,d)  max flow for installed capacity     [GW]
   eFlowInstlCap2  (y,p,  d,d)  max flow for installed capacity     [GW]
   eFlowNetN1      (y,p,  d,d)  equatiliy                           [GW]
   eFlowNetN2      (y,p,  d,d)                                      [GW]
   eFlowInstlCap1_b(y,p,  d,d)  max flow for installed capacity lin [GW]
   eFlowInstlCap2_b(y,p,  d,d)  max flow for installed capacity lin [GW]
   eFlowNetN1_b    (y,p,  d,d)                                      [GW]
   eFlowNetN2_b    (y,p,  d,d)                                      [GW]
   eFlowNetEx      (y,p,  d,d)  flow for each existing  line        [GW]
   eMaxTheta       (y,p,  d  )  max theta                           [rad]
   eMinTheta       (y,p,  d  )  max theta                           [rad]


 // Storage Constraints
   eMaxCons        (y,p,g,d  ) max consumption existing  units      [GW]
   eMinCons        (y,p,g,d  ) min consumption existing  units      [GW]
   eStorage        (y,p,g,d  ) storage balance equation
   eMinReserve     (y,p,g,d  ) min storage reserve                  [GW]
   eMaxReserve     (y,p,g,d  ) max storage reserve                  [GW]
   eFinalReserve   (y,p,g,d  ) final reserve                        [GW]
   eMaxSpillage    (y,p,g,d  ) max spillage                         [GW]
   eMinSpillage    (y,p,g,d  ) min spillage                         [GW]
   eMinCons_New    (y,p,g,d  ) min consumption candidate units      [GW]
   eMaxCons_New    (y,p,g,d  ) max consumption candidate units      [GW]
   eMinReserve_New (y,p,g,d  ) min reserve candidate units          [GW]
   eMaxReserve_New (y,p,g,d  ) max reserve candidate units          [GW]
   eMinSpillage_New(y,p,g,d  )
   eMaxSpillage_New(y,p,g,d  )
   eRSRVH_RP1_C_CAN(y,p,g,d  ) storage with representative          [GW]
   eRSRVH_RP2_C_CAN(y,p,g,d  ) storage with representative          [GW]

 // Feseablity duality constraints
   dL_dvProd       (y,p,g,d   )  derivative with respect to prod per generator
   dL_dvWind       (y,p,g,d   )  derivative with resprecto to wind prod
   dL_dvProdCP     (y,p,  d,cp)  derivative with respect to prod per company
   dl_dvENS        (y,p,  d   )  derivative with respect to ENS
   dL_dvCon        (y,p,g,d   )  derivative with respect to CON
   dL_dvFlow       (y,p,  d,d )  derivative with respect to FLow
   dL_dvTheta      (y,p,  d   )  derivative with respect to Theta
   dL_dvNewGen     (y,  g,d   )  derivative with respect to NewGen per generator
   dL_dvNewGenCP   (y,    d,cp)  derivative with respect to NewGen per company
   dL_dReserv      (y,p,g,d   )  derivative with respect to Reserv
   dL_dSpill       (y,p,g,d   )  derivative with respect to Spill
   dL_dvDemand     (y,p,  d   )  derivative with respect to Demand
   dL_dVPriceFlow1 (y,p,  d,d )  derivative with respect to
   dL_dVPriceFlow2 (y,p,  d,d )
 // Complementarity Conditions

   c_cLambda     (y,p,  d  )  complementarity for power balanance
   c_cRo_up      (y,p,g,d  )  complementarity for max prod existing units
   c_cRo_lo      (y,p,g,d  )  complementarity for min prod existing units
   c_Omega_up    (y,p,g,d  )  complementarity for max prod new units
   c_Omega_lo    (y,p,g,d  )  complementarity for min prod existing units
   c_phi_up      (y,p,  d,d)  complementarity for max flow through lines
   c_phi_lo      (y,p,  d,d)  complementarity for min flow through lines
   c_phi_lo_loss (y,p,  d,d)  complementarity for min flow with losses
   c_phi_up_loss (y,p,  d,d)  complementarity for max flow with losses
   c_phi         (y,p,  d,d)  complementarity for min flow through lines
   c_zeta_up     (y,p,  d,d)  complementarity for max flow through new lines
   c_zeta_lo     (y,p,  d,d)  complementarity for max flow through new lines
   c_tau_up      (y,p,  d,d)  complementarity for flow through new lines
   c_tau_lo      (y,p,  d,d)  complementarity for flow through new lines
   c_theta_up    (y,p,  d  )  complementarity for max theta
   c_theta_lo    (y,p,  d  )  complementarity for min theta
   c_Psi         (y,p,g,d  )  complementarity for storage balance
   c_ji_lo       (y,p,  d  )  complementarity for min ens
   c_ji_up       (y,p,  d  )  complementarity for max ens
   c_Betha       (y,  g,  d)  complementarity for cumulative generation
   c_omic_up     (y,  g,  d)  complementarity for max new generation
   c_omic_lo     (y,  g,  d)  complementarity for min new generation
   c_Kappa_up    (y,p,g,d  )  complementarity for max cons existing units
   c_Kappa_lo    (y,p,g,d  )  complementarity for min cons existing units
   c_Delta_lo    (y,p,g,d  )  complementarity for min reserve
   c_Delta_up    (y,p,g,d  )  complementarity for max reserve
   c_Delta_lo2   (y,p,g,d  )  complementarity for final reserve
   c_Mu_lo       (y,p,g,d  )  complementarity for min spillage
   c_Mu_up       (y,p,g,d  )  complementarity for max spillage
   c_zeus1       (y,p,g,d  )  complementarity for slow storage constraint

 // Linearized complementarity equations
   l_cRo_up1       (y,p,g,d  )  linearization for max prod existing units
   l_cRo_up2       (y,p,g,d  )  linearization for max prod existing units
   l_cRo_lo        (y,p,g,d  )  linearization for min prod existing units
   l_cRo_lo1       (y,p,g,d  )  linearization for min prod existing units
   l_cRo_lo2       (y,p,g,d  )  linearization for min prod existing units
   l_cRo_upw1      (y,p,g,d  )  linearization for max prod existing units
   l_cRo_upw2      (y,p,g,d  )  linearization for max prod existing units
   l_cRo_low1      (y,p,g,d  )  linearization for min prod existing units
   l_cRo_low2      (y,p,g,d  )  linearization for min prod existing units
   l_Omega_lo1     (y,p,g,d  )  linearization for min prod new units
   l_Omega_lo2     (y,p,g,d  )  linearization for min prod new units
   l_Omega_up1     (y,p,g,d  )  linearization for max prod new units
   l_Omega_up2     (y,p,g,d  )  linearization for max prod new units
   l_Omega_low1    (y,p,g,d  )  linearization for max prod new win units
   l_Omega_low2    (y,p,g,d  )  linearization for max prod new win units
   l_Omega_upw1    (y,p,g,d  )  linearization for max prod new win units
   l_Omega_upw2    (y,p,g,d  )  linearization for max prod new win units
   l_Betha1        (y,  g,  d)  linearization for cumulative generation
   l_Betha2        (y,  g,  d)  linearization for cumulative generation
   l_omic_up1      (y,  g,  d)  linearization for max new generation
   l_omic_up2      (y,  g,  d)  linearization for max new generation
   l_omic_lo1      (y,  g,  d)  linearization for min new generation
   l_omic_lo2      (y,  g,  d)  linearization for min new generation
   l_theta_up1     (y,p,  d  )  linearization for max theta
   l_theta_up2     (y,p,  d  )  linearization for max theta
   l_theta_lo1     (y,p,  d  )  linearization for min theta
   l_theta_lo2     (y,p,  d  )  linearization for min theta
   l_Kappa_up1     (y,p,g,d  )  linearization for max cons existing units
   l_Kappa_up2     (y,p,g,d  )  linearization for max cons existing units
   l_Kappa_lo1     (y,p,g,d  )  linearization for min cons existing units
   l_Kappa_lo2     (y,p,g,d  )  linearization for min cons existing units
   l_Delta_lo1     (y,p,g,d  )  linearization for min reserve
   l_Delta_lo2     (y,p,g,d  )  linearization for min reserve
   l_Delta_up1     (y,p,g,d  )  linearization for max reserve
   l_Delta_up2     (y,p,g,d  )  linearization for max reserve
   l_Mu_lo1        (y,p,g,d  )  linearization for min spillage
   l_Mu_lo2        (y,p,g,d  )  linearization for min spillage
   l_Mu_up1        (y,p,g,d  )  linearization for min spillage
   l_Mu_up2        (y,p,g,d  )  linearization for min spillage
   l_KappN_lo1     (y,p,g,d  )  linearization for max new cons existing units
   l_KappN_lo2     (y,p,g,d  )  linearization for max new cons existing units
   l_KappN_up1     (y,p,g,d  )  linearization for min new cons existing units
   l_KappN_up2     (y,p,g,d  )  linearization for min new cons existing units
   l_DeltN_lo1     (y,p,g,d  )  linearization for min new reserve
   l_DeltN_lo2     (y,p,g,d  )  linearization for min new reserve
   l_DeltN_up1     (y,p,g,d  )  linearization for max new reserve
   l_DeltN_up2     (y,p,g,d  )  linearization for max new reserve
   l_muN_lo1       (y,p,g,d  )  linearization for min new spillage
   l_muN_lo2       (y,p,g,d  )  linearization for min new spillage
   l_MuN_up1       (y,p,g,d  )  linearization for min new spillage
   l_MuN_up2       (y,p,g,d  )  linearization for min new spillage
   l_ji_up1        (y,p,  d  )  linearization for max ens
   l_ji_up2        (y,p,  d  )  linearization for max ens
   l_ji_lo1        (y,p,  d  )  linearization for min ens
   l_ji_lo2        (y,p,  d  )  linearization for min ens
   l_phi_lo1       (y,p,  d,d)  linearization for min flow through lines
   l_phi_lo2       (y,p,  d,d)  linearization for min flow through lines
   l_phi_up1       (y,p,  d,d)  linearization for max flow through lines
   l_phi_up2       (y,p,  d,d)  linearization for max flow through lines
   l_zeta_up1      (y,p,  d,d)  linearization for max flow through new lines
   l_zeta_up2      (y,p,  d,d)  linearization for max flow through new lines
   l_zeta_up3      (y,p,  d,d)  linearization for max flow through new lines aditional condition
   l_zeta_lo1      (y,p,  d,d)  linearization for max flow through new lines
   l_zeta_lo2      (y,p,  d,d)  linearization for max flow through new lines
   l_tau_up1       (y,p,  d,d)  linearization for flow through new lines
   l_tau_up2       (y,p,  d,d)  linearization for flow through new lines
   l_tau_lo1       (y,p,  d,d)  linearization for flow through new lines
   l_tau_lo2       (y,p,  d,d)  linearization for flow through new lines
   l_Alpha_up1     (y,p,  d  )  linearization for min demand
   l_Alpha_up2     (y,p,  d  )  linearization for min demand
   l_dualpf11      (y,p,  d,d)  linearization for product price flow
   l_dualpf12      (y,p,  d,d)  linearization for product price flow
   l_dualpf21      (y,p,  d,d)  linearization for product price flow
   l_dualpf22      (y,p,  d,d)  linearization for product price flow

 // Auxiliary linnearized

   e_intPrice1Flow1  (y,p,  d,d,k)  linearization of price_flow production
   e_intPrice1Flow2  (y,p,  d,d,k)  linearization of price_flow production
   e_intPrice2Flow1  (y,p,  d,d,k)  linearization of price_flow production
   e_intPrice2Flow2  (y,p,  d,d,k)  linearization of price_flow production
   e_intPriceFlow3   (y,p,  d,d,k)  linearization of price_flow production
   e_intPriceFlow4   (y,p,  d,d,k)  linearization of price_flow production
   e_PriceFlow1      (y,p,  d,d  )  linearization of price_flow production
   e_PriceFlow2      (y,p,  d,d  )  linearization of price_flow production
   e_intFLow         (y,p,  d,d  )  linearization of price_flow production
   e_PositiveLines   (y          )  at least one line invested
 ;
////////////////////////////   EQUATION DEFINITION    //////////////////////////////////
 //*********************       OBJECTIVE FUNCTIONS      ********************************
    //*These are the upper level objective functions, as for the lower level the objective funciton is written in its kkt equivalent
    eOF                 .. vDummyCost   =e= 0                         ;
    eTotalTCost         .. vTotalTCost  =e= vTotalFCost + vTotalVCost ;
    
    eFExtendedCost      .. vTotalTCost  =e= //+ sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp) * pSlopeVarCost(t)*vProduct(y,p,t,d)]
                                            + sum[(y,rpp(rp,pa(p))), pWeight_rp(rp) * (sum (gad(t,d), pSlopeVarCost(t)*vProduct(y,p,t,d)))]
                                            + sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pHydroCost        *vProduct(y,p,h,d)]
                                            + sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pConsumCost       *vConsump(y,p,h,d)]
                                            + sum[(y,rpp(rp,pa(p)),tg(g,th),gad(g,d)), pWeight_rp(rp)* pEmissionrate(th) *vProduct(y,p,g,d)*PCO2price]
                                            +(
                                            + sum[(y,rpp(rp,pa(p)),gad(g,d)), (conjcvariation(p,g,d)/2)   * pWeight_rp(rp)* power((vProduct(y,p,g,d)+vWind(y,p,g,d)$(wn(g))-(vConsump(y,p,g,d))$h(g)),2)]//
                                            + sum((y,rpp(rp,pa(p)),       d),-(1/Slope(p,d))$(Slope(p,d)) * pWeight_rp(rp)* [pDemandNode   (y,p,d      )*vDemand(y,p,d)- power(vDemand(y,p,d),2)/2])
                                             )$is_elastic
                                            //+ sum((y,rpp(rp,pa(p)),       d),-(1/Slope(d))$(Slope(d)) * [pWeight_rp(rp)*pDemandNode   (y,p,d      )*vDemand(y,p,d)- power(pWeight_rp(rp)*vDemand(y,p,d),2)/2])
                                            + sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pConsumCost*vConsump(y,p,h,d)]
                                         // + sum[(y,rpp(rp,pa(p)), ged(h,d)), pWeight_rp(rp)* vSpillage(y,p,   h,d  )*0.001]
                                            + sum[(y,    gcd(g,d)),  pGenInvCost (g) *vNewGen(y, g ,d)*pMaxProd     (g,d )]
                                            + sum[(y,    gcd(wn,d)), pGenInvCost (wn)*vNewGen(y, wn,d)*pMaxWindGen  (wn,d)] 
                                            + sum[(y,    gcd(sr,d)), pGenInvCost (sr)*vNewGen(y, sr,d)*pMaxSolarGen (sr,d)]                                     
                                            ;

    eFCost_MPEC         .. vTotalFCost  =e= +sum[(y,lc),      (card(y)-ord(y)+1)*[vNewLine_R(y,lc) - vNewLine_R(y-1,lc)]]  ;
    
    eFCost_Bilevel      .. vTotalFCost  =e=0
                                          +(
                                              +sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine(y,lc)        - vNewLine  (y-1,lc)     ]]
                                              +sum[(y,    gcd(g,d)),  pGenInvCost (g )*vNewGen(y, g, d)*pMaxProd    (g,d) ]
                                              +sum[(y,    gcd(wn,d)), pGenInvCost (wn)*vNewGen(y, wn,d)*pMaxWindGen (wn,d)]
                                              +sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp) * pSlopeVarCost(t) * vProduct(y,p,t,d)]
                                              +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pHydroCost       * vProduct(y,p,h,d)]
                                              +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pConsumCost      * vConsump(y,p,h,d)]
                                            
                                            //-sum[(y,pa(p),la(di,df)), (-vPriceFlow1 (y,p,di,df) + vPriceFlow2(y,p,di,df))]
                                            //+sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine(y,lc)        - vNewLine  (y-1,lc)]]
                                            //- sum[(y,rpp(rp,pa(p)),la(di,df)),pWeight_rp(rp)*(b_cLambda  (y,p,  df ) -b_cLambda  (y,p,  di  ))*vflow(y,p,di,df)]

                                            ) $ (pObjective= 0 or pObjective=1)
                                            +
                                            (  +(      sum((y,rpp(rp,pa(p)),       d),-(1/Slope(p,d))$(Slope(p,d)) * pWeight_rp(rp)* [pDemandNode   (y,p,d      ) * vDemand(y,p,d)
                                                                                                                               -  power(vDemand(y,p,d),2)/2]))
                                              ) $ (pObjective=1)

                                          +(
                                             -sum[(y,rpp(rp,pa(p)),la(di,df)), pWeight_rp(rp)*(-vPriceFlow1 (y,p,di,df) + vPriceFlow2(y,p,di,df))]                                        
                                             +sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine(y,lc)        - vNewLine  (y-1,lc)]]
                                            ) $( pObjective=2 )

                                          +(
                                             + sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine(y,lc)        - vNewLine  (y-1,lc)     ]]
                                             - sum[(y,rpp(rp,pa(p)),la(di,df)),pWeight_rp(rp)*(b_cLambda  (y,p,  df ) -b_cLambda  (y,p,  di  ))*vflow(y,p,di,df)]
                                          ) $( pObjective=3 )

                                            //+sum[ (y,pa(p),gad(wn,d)), 0.0001 *(  pWindGen(p,wn) -  vWind(y,p,wn,d))]
                                            //+sum[(y,rpp(rp,pa(p)),ged(h,d)), pWeight_rp(rp) *vSpillage(y,p,   h,d  )*0.001])
                                            //-sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp) * pDuration(p,s)* b_cLambda  (y,p,  d  )*vProduct(y,p,t,d)] $ (not pTypeComp  )
                                            ;
    eFCost_Bilevel_R    .. vTotalFCost  =e=
                                              +sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine_R(y,lc)        - vNewLine_R  (y-1,lc)     ]]
                                              +sum[(y,gcd(g,d)),(card(y)-ord(y)+1)*pGenInvCost (g)*[vNewGen(y, g,d) - vNewGen(y-1, g,d)]]  $ (not pTypeComp )
                                              +sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp) *pSlopeVarCost(t)*vProduct(y,p,t,d)] $ (not pTypeComp )
                                              +sum((y,rpp(rp,pa(p)),d),-(1/Slope(p,d))$(Slope(p,d)) * pWeight_rp(rp)*[pDemandNode   (y,p,d      ) * (vDemand(y,p,d))
                                                                                                                                                -  power(vDemand(y,p,d),2)/2])
                                              -sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pHydroCost *vProduct(y,p,h,d)]
                                              -sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pConsumCost *vConsump(y,p,h,d)]

                                        ;
    eTotalVCost_MinCost .. vTotalVCost  =e=
                                           
                                              + sum[(y,rpp(rp,pa(p))), pWeight_rp(rp) * (sum (gad(t,d), pSlopeVarCost(t)*vProduct(y,p,t,d)))]
                                              + sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pHydroCost        *vProduct(y,p,h,d)]
                                              + sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pConsumCost       *vConsump(y,p,h,d)]
                                              + sum[(y,rpp(rp,pa(p)),tg(g,th),gad(g,d)), pWeight_rp(rp)* pEmissionrate(th) *vProduct(y,p,g,d)*PCO2price]
                                           // + sum[(y,rpp(rp,pa(p)),ged(h,d)), pWeight_rp(rp) * pDuration(p,s)*vSpillage(y,p,   h,d  )*0.0001]
                                           // + sum[(y,rpp(rp,pa(p)),       d), pWeight_rp(rp) * pDuration(p,s)*pENSCost        *vENS    (y,p,  d)]
                ;
    eTotalFCost_MinCost .. vTotalFCost  =e=
                                            +                   sum[(y,    gcd(g, d)), pGenInvCost (g) *vNewGen(y, g ,d)*pMaxProd    (g,d )]
                                            +                   sum[(y,    gcd(wn,d)), pGenInvCost (wn)*vNewGen(y, wn,d)*pMaxWindGen (wn,d)]
                                            +                   sum[(y,    gcd(sr,d)), pGenInvCost (sr)*vNewGen(y, sr,d)*pMaxSolarGen (sr,d)] // For multiple years
                                            +                   sum[(y,lc           ), (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine(y,lc) - vNewLine(y-1,lc)]]
                                         ;
    



 //**************** EQUALITY CONSTRAINT / NO COMPLEMENTARITY **************************

  eStorage       (y,pa(p),gad(hf,d)) ..                                 // b_Psi    (y,p,g    )
                 + vReserve(y,p-1,hf,d)$[pa(p-1)]     =e=
                 - pIniReserve(hf)$[not pa(p-1)]$ged(hf,d)
                 - vNewGen(y,hf,d)*pIniReserve(hf)$[not pa(p-1)]$gcd(hf,d)
                 + vReserve  (y,p,hf,d)
                 - pInflows  (  p,hf  )  // +  vSpillage(y,p,hf,d)
                 - vConsump  (y,p,hf,d)*pEffic(hf) 
                 + vProduct  (y,p,hf,d)            
    ;

  eBalance_C_can (y,pa(p),   d     ) ..                                                             // b_cLambda(y,p,d  )
                 + sum[gad(g,d),  vProduct(y,p, g,d)$(not wn(g) and not sr(g))] + sum(gad(wn,d),vWind(y,p, wn,d)) + sum(gad(sr,d),vSolar (y,p,sr,d))
                 =e=
   //*           - vENS(y,p,d)
                 + sum[la(d,df),  vFlow(y,p,d,df)] - sum[la(di,d), vFlow(y,p,di,d)]
                 + sum(gad(h,d), vConsump(y,p,h,d))
                 +  pFixedDemand (p,d) $ (pCost )
                 + vDemand(y,p,d)$ (not pTypeComp and not pBilevel and is_elastic) 
                 + ( pDemandNode   (y,p,d) - b_cLambda(y,p,d)*Slope(p,d) ) $((not pTypeComp and pBilevel ))
                 + pDemandNode   (y,p,d) $ (not is_elastic)
                 ;

  eRSRVH_RP1_C_CAN (y,ps(p),gad(hs,d)) $[ORD(p) <= CARD(p) ]..
                   vReserve(y,p,hs,d) =E=                                                              // b_ZEUS1  (y,p,g,d)
                   // Reserve value at p-pStorMovWindow (Moving Window)
                   vReserve(y,p-pStorMovWindow,hs,d)$[ORD(p) >= pStorMovWindow]
                   // Reserve value for first period
                 + pIniReserve(hs)$[(ORD(p) = 1) and ged(hs,d)]
               //  + pIniReserve(hs)$[(ORD(p) = 1) and gcd(hs,d)]
                   // Sum of production/consumption during the moving window
                 + SUM[pp$[ORD(pp) >= ORD(p)+1-pStorMovWindow
                     AND ORD(pp) <= ORD(p)],
                   // Sum taking into acount the RP relation among periods
                   + SUM [ IndexRP (pp,ppp) ,
                      //     Hydro production
                           - vProduct  (y,ppp,hs,d)//pProdFunct(h)
                      //     Spillage
                         - vSpillage  (y,ppp,hs,d)
    //*               //     charging/pumping consumption
                           + vConsump  (y,ppp,hs,d)*pEffic(hs)//pProdFunct(h)
    //*               //     inflows
                           + pInflows(ppp,hs)]
                    ] ;

  e_intPrice1Flow1 (y,pa(p), la(di,df),k)$(pObjective=2 and pMercado=2) ..b_cLambda (y,p,di) - vBinPrice1FLow (y,p,di,df,k)  =l=   + M_cLambda*(1-vBinFlow(y,p,di,df,k)) ;        // b_dualpf1
  e_intPrice1Flow2 (y,pa(p), la(di,df),k)$(pObjective=2 and pMercado=2) ..                     vBinPrice1FLow (y,p,di,df,k)  =l=   + M_cLambda*   vBinFlow(y,p,di,df,k)  ;
  e_intPrice2Flow1 (y,pa(p), la(di,df),k)$(pObjective=2 and pMercado=2) ..b_cLambda (y,p,df) - vBinPrice2FLow (y,p,di,df,k)  =l=   + M_cLambda*(1-vBinFlow(y,p,di,df,k)) ;
  e_intPrice2Flow2 (y,pa(p), la(di,df),k)$(pObjective=2 and pMercado=2) ..                     vBinPrice2FLow (y,p,di,df,k)  =l=   + M_cLambda*   vBinFlow(y,p,di,df,k)  ;
  e_intPriceFlow3  (y,pa(p), la(di,df),k)$(pObjective=2 and pMercado=2) ..b_cLambda (y,p,di) - vBinPrice1FLow (y,p,di,df,k)  =g=   pzero  ;        // b_dualpf1
  e_intPriceFlow4  (y,pa(p), la(di,df),k)$(pObjective=2 and pMercado=2) ..b_cLambda (y,p,df) - vBinPrice2FLow (y,p,di,df,k)  =g=   pzero ;
  e_intFLow        (y,pa(p), la(di,df)  )$(pObjective=2 and pMercado=2) .. vFlow       (y,p,di,df) =e= -pTTC(di,df)                   + (((2)*pTTC(di,df))/power(2,card(k))) * sum( k, power(2,ord(k))*vBinFLow      (y,p,di,df,k)) ;
  e_PriceFlow1     (y,pa(p), la(di,df)  )$(pObjective=2 and pMercado=2) .. vPriceFlow1 (y,p,di,df) =e= -pTTC(di,df)*b_cLambda (y,p,di) + (((2)*pTTC(di,df))/power(2,card(k))) * sum( k, power(2,ord(k))*vBinPrice1FLow (y,p,di,df,k)) ;
  e_PriceFlow2     (y,pa(p), la(di,df)  )$(pObjective=2 and pMercado=2) .. vPriceFlow2 (y,p,di,df) =e= -pTTC(di,df)*b_cLambda (y,p,df) + (((2)*pTTC(di,df))/power(2,card(k))) * sum( k, power(2,ord(k))*vBinPrice2FLow (y,p,di,df,k)) ;


 //*********************       PRIMAL CONSTRAINTS       ********************************
  // Conventioanl and wind constraints
     eMaxProd        (y,pa(p),ged(g,d) )$(not wn(g) and not sr(g)) ..- vProduct(y,p,g,d)           =g= -  pMaxProd(g,d)                                                                 ; // b_cRo_up    (y,p,g  )
     eMinProd        (y,pa(p),ged(g,d) )$(not wn(g) and not sr(g)) ..  vProduct(y,p,g,d)           =g=    pMinProd(g)                                                                   ; // b_cRo_lo    (y,p,g  )
     eMinProd_New    (y,pa(p),gcd(g,d) )$(not wn(g) and not sr(g)) ..  vProduct(y,p,g,d)           =g=    pzero                                                                         ; // b_Omega_lo  (y,p,g  )
     eMaxProd_New    (y,pa(p),gcd(g,d) )$(not wn(g) and not sr(g)) ..- vProduct(y,p,g,d)           =g= -  vNewGen (y, g,d)*pMaxProd(g,d)                                                ; // b_Omega_up  (y,p,g  )
     eMaxWind        (y,pa(p),ged(wn,d)) ..- vWind   (y,p,wn,d)                      =g= -  pWindGenNode (p,wn,d)                                                         ; // b_cRo_up_w  (y,p,g  )
     eMinWind        (y,pa(p),ged(wn,d)) ..  vWind   (y,p,wn,d)                      =g=    pzero                                                                         ; // b_cRo_lo_w  (y,p,g  )
     eMaxWindNew     (y,pa(p),gcd(wn,d)) ..- vWind   (y,p,wn,d)                      =g= -  pWindGenNode (p,wn,d)*vNewGen (y, wn,d)                                       ; // b_Omega_up_w(y,p,g  )
     eMinWindNew     (y,pa(p),gcd(wn,d)) ..  vWind   (y,p,wn,d)                      =g=    pzero                                                                         ;
     eMaxSolar       (y,pa(p),ged(sr,d)) ..- vSolar  (y,p,sr,d)                      =g= -  pSolarGenNode(p,sr,d)                                                         ;
     eMinSolar       (y,pa(p),ged(sr,d)) ..  vSolar  (y,p,sr,d)                      =g=    pzero                                                                         ; // b_Omega_up_w(y,p,g  )                                  ;
     eMaxSolarNew    (y,pa(p),gcd(sr,d)) ..- vSolar  (y,p,sr,d)                      =g= -  pSolarGenNode(p,sr,d)*vNewGen (y, sr,d)                                       ;
     BudgetSolar     (y,      gcd(sr,d)) ..- vNewGen (y, sr,d)                       =g= -  30                                                                            ;
     BudgetWind      (y,      gcd(wn,d)) ..- vNewGen (y, wn,d)                       =g= -  200                                                                           ;
     eMinSolarNew    (y,pa(p),gcd(sr,d)) ..  vSolar  (y,p,sr,d)                      =g=    pzero                                                                         ; // b_Omega_up_w(y,p,g  )                                 ;
     eMinDemand      (y,pa(p),        d) ..   vDemand (y,p,  d)                      =g=    pzero                                                                         ; // b_Alpha_u
   //eGenInst_R_can  (y,      gcd(g,d)(not wn(g)) ) ..- vNewGen  (-1, g,d)         =g= -  vNewGen  (y, g,d  )                                                             ; // b_Betha     (y,  g,d)
     eMiniGen_R_can  (y,      gcd(g,d) ) ..  vNewGen  (y, g,d  )                     =g=    pzero                                                                         ; // b_Omic_lo   (y,  g,d)
     eMaxiGen_R_can  (y,      gcd(g,d) ) ..- vNewGen  (y, g,d  )                     =g= -  pMaxNewGen                                                                    ; // b_Omic_up   (y,  g,d)
     eMaxTheta       (y,pa(p),d        ) ..- vTheta(y,p,d)                           =g= -  pMaxtheta                                                                     ; // b_theta_up  (y,p,  d)
     eMinTheta       (y,pa(p),d        ) ..  vTheta(y,p,d)                           =g= -  pMaxtheta                                                                     ; // b_theta_lo  (y,p,  d)
     e_PositiveLines (y                ) .. sum(lc, vNewLine(y, lc)  )                =g= 1 ; 
  // storage constraints
     eMinReserve     (y,pt(p),ged(h,d))$((pa(p) and hf(h)) or (ps(p)and hs(h)))  ..  vReserve(y,p,h,d)    =g=    pMinReserve(h)                                                       ; // b_Deltas_lo (y,p,g  )
     eMaxReserve     (y,pt(p),ged(h,d))$((pa(p) and hf(h)) or (ps(p)and hs(h)))  ..- vReserve(y,p,h,d)    =g= -  pMaxReserve(h)                                                       ; // b_Deltas_up (y,p,g  )
     eMinReserve_New (y,pt(p),gcd(h,d))$((pa(p) and hf(h)) or (ps(p)and hs(h)))  ..  vReserve(y,p,h,d)    =g=    vNewGen (y,h,d)*pMinProd(h  )*pEprMin(h)                             ; // b_DeltN_lo  (y,p,g  )
     eMaxReserve_New (y,pt(p),gcd(h,d))$((pa(p) and hf(h)) or (ps(p)and hs(h)))  ..- vReserve(y,p,h,d)    =g= -  vNewGen (y,h,d)*pMaxProd(h,d)*pEprMax(h)                             ; // b_DeltN_up  (y,p,g  )
     eMinCons        (y,pa(p),ged(h,d) ) ..  vConsump(y,p,h,d)                                            =g=    pzero                                                                ; // b_Kappa_lo  (y,p,h,d)
     eMaxCons        (y,pa(p),ged(h,d) ) ..- vConsump(y,p,h,d)                                            =g= -  pMaxCons(   h  )                                                     ; // b_Kappa_up  (y,p,h,d)
     eMinCons_New    (y,pa(p),gcd(h,d) ) ..  vConsump(y,p,h,d)                                            =g=    vNewGen (y, h,d)*pMinProd(h)*(2-pEffic(h))                           ; // b_KappN_lo  (y,p,h,d)
     eMaxCons_New    (y,pa(p),gcd(h,d) ) ..- vConsump(y,p,h,d)                                            =g= -  vNewGen (y, h,d)*pMaxProd(h,d)*(2-pEffic(h))                         ; // b_KappN_up  (y,p,h,d)
     eMinSpillage    (y,pa(p),ged(h,d) ) ..  vSpillage(y,p,h,d)                                           =g=    pzero                                                                ; // b_mu_lo     (y,p,g  )
     eMaxSpillage    (y,pa(p),ged(h,d) ) ..- vSpillage(y,p,h,d)                                           =g= -  pMaxReserve(h)                                                       ; // b_mu_up     (y,p,g  )
     eMinSpillage_New(y,pa(p),gcd(h,d))  ..  vSpillage(y,p,h,d)                                           =g=    pzero                                                                ; // b_muN_lo    (y,p,g  )
     eMaxSpillage_New(y,pa(p),gcd(h,d))  ..- vSpillage(y,p,h,d)                                           =g= -  vNewGen (y, h,d)*pMaxProd(h,d)*4                                     ; // b_muN_up    (y,p,g  )
   //eFinalReserve   (y,pn(p),ged(h,d))  ..  vReserve(y,p,h,d)                       =g=    pFinReserve(h)                                                              ; // b_Delta_lo2 (y,p,g  )
  // Network Constraints
     eFlowExisting1  (y,pa(p),le(di,df)) ..  vFlow(y,p, di,df)                       =g= -  pTTC(di,df)                                                                   ; // b_phi_lo   (y,p,s,d,d)
     eFlowExisting2  (y,pa(p),le(di,df)) ..- vFlow(y,p, di,df)                       =g= -  pTTC(di,df)                                                                   ; // b_phi_up   (y,p,s,d,d)
     eFlowInstlCap1_b(y,pa(p),lc(di,df)) ..  vFlow(y,p, di,df) /     pTTC(di,df)     =g= -  vNewLine_R(y,di,df)                                                           ; // NON COMPLEMENTARITY (only duplicated constraint)
     eFlowInstlCap2_b(y,pa(p),lc(di,df)) ..- vFlow(y,p, di,df) /     pTTC(di,df)     =g= -  vNewLine_R(y,di,df)                                                           ; // NON COMPLEMENTARITY (only duplicated constraint)
     eFlowInstlCap1  (y,pa(p),lc(di,df)) ..  vFlow(y,p, di,df) /     pTTC(di,df)     =g= -  vNewLine  (y,di,df)                                                           ; // b_zeta_lo  (y,p,s,d,d)
     eFlowInstlCap2  (y,pa(p),lc(di,df)) ..- vFlow(y,p, di,df) /     pTTC(di,df)     =g= -  vNewLine  (y,di,df)                                                           ; // b_zeta_up  (y,p,s,d,d)
     eCumLineInstall (y,        lc       ) ..- vNewLine(y-1,lc)                      =g= -  vNewLine(y, lc)                                                               ; // fixed variable
     eCumLineInstallR(y,        lc       ) ..- vNewLine_R(y-1,lc)                    =g= -  vNewLine_R(y, lc)                                                             ; // fixed variable
     eFlowNetEx      (y,pa(p),le(di,df)) ..  vFlow(y,p,  di,df)                      =e=   [vTheta(y,p,di) - vTheta(y,p,df)] * pSbase / pX(di,df)                           ; // b_phi (y,p,s,d,d)
     eFlowNetN1      (y,pa(p),lc(di,df)) ..  vFlow(y,p,  di,df) / (1e+3/1e+3)*pTTC(di,df) =g=   [vTheta(y,p,di) - vTheta(y,p,df)] * pSbase / pX(di,df) / (1e+3/1e+3)*pTTC(di,df)
                                                                                                                                      - 1 + vNewLine(y,di,df)             ; // b_tau_lo (y,p,s,d,d)
     eFlowNetN2      (y,pa(p),lc(di,df)) ..- vFlow(y,p,  di,df) / (1e+3/1e+3)*pTTC(di,df) =g= -([vTheta(y,p,di) - vTheta(y,p,df)] * pSbase / pX(di,df) / (1e+3/1e+3)*pTTC(di,df)
                                                                                                                                      + 1 - vNewLine(y,di,df))            ; // b_tau_up (y,p,s,d,d)
     eFlowNetN1_b    (y,pa(p),lc(di,df)) ..  vFlow(y,p,  di,df) / (1e+3/1e+3)*pTTC(di,df) =g=   [vTheta(y,p,di) - vTheta(y,p,df)] * pSbase / pX(di,df) / (1e+3/1e+3)*pTTC(di,df)
                                                                                                                                     - 1 + vNewLine_R(y,di,df)            ; // NON COMPLEMENTARITY(only duplicated constraint)
     eFlowNetN2_b    (y,pa(p),lc(di,df)) ..- vFlow(y,p,  di,df) / (1e+3/1e+3)*pTTC(di,df) =g= -([vTheta(y,p,di) - vTheta(y,p,df)] * pSbase / pX(di,df) / (1e+3/1e+3)*pTTC(di,df)
                                                                                                                                              + 1 - vNewLine_R(y,di,df))  ; // NON COMPLEMENTARITY(only duplicated constraint)
 //*********************  NON-LINEAR COMPLEMENTARITY    ********************************
     c_cRo_up        (y,pa(p),ged(g,d) ) .. b_cRo_up   (y,p,g,d  ) *  (-vProduct(y,p,g,d)     + pMaxProd(g,d)                           )                           =e= 0;
     c_cRo_lo        (y,pa(p),ged(g,d) ) .. b_cRo_lo   (y,p,g,d  ) *  ( vProduct(y,p,g,d)     - pMinProd(g)                           )                           =e= 0;
     c_Omega_lo      (y,pa(p),gcd(g,d) ) .. b_Omega_lo (y,p,g,d  ) *  ( vProduct(y,p,g,d)     - pzero                                 )                           =e= 0;
     c_Omega_up      (y,pa(p),gcd(g,d) ) .. b_Omega_up (y,p,g,d  ) *  (-vProduct(y,p,g,d)     + vNewGen (y, g,d)*pMaxProd(g,d)    )                           =e= 0;
   //c_Betha         (y,      gcd(g,d) ) .. b_Betha    (y,  g,  d) *  (-vNewGen(y-1, g,d) + vNewGen  (y, g,d  )             )                           =l= pEps;
   //c_omic_lo       (y,      gcd(g,d) ) .. b_omic_lo  (y,  g,  d) *  ( vNewGen(y, g,d  ) - pzero                                 )                           =l= pEps;
   //c_omic_up       (y,      gcd(g,d) ) .. b_omic_up  (y,  g,  d) *  (-vNewGen(y, g,d  ) + pMaxNewGen                            )                           =l= pEps;
   //c_theta_up      (y,pa(p),  d      ) .. b_theta_up (y,p,    d) *  (-vTheta(y,p,s,d)         + pMaxtheta                             )                           =l= pEps;
   //c_theta_lo      (y,pa(p),  d      ) .. b_theta_lo (y,p,    d) *  ( vTheta(y,p,s,d)         + pMaxtheta                             )                           =l= pEps;
   //c_ji_up         (y,pa(p),  d      ) .. b_ji_up    (y,p,d    ) *  (-vENS(y,p,s,d)  + pDemand(p,s) * pDemShare(d) * pCumDemIncr(y)   )                           =l= pEps;
   //c_ji_lo         (y,pa(p),  d      ) .. b_ji_lo    (y,p,d    ) *  ( vENS(y,p,s,d)           - pzero                                 )                           =l= pEps;
     c_Kappa_lo      (y,pa(p),ged(h,d) ) .. b_Kappa_lo (y,p,h,  d) *  ( vConsump(y,p,h,d)     - pzero                                 )                           =e= 0;
     c_Kappa_up      (y,pa(p),ged(h,d) ) .. b_Kappa_up (y,p,h,  d) *  (-vConsump(y,p,h,d)     + pMaxCons(h)                           )                           =e= 0;
     c_Delta_lo      (y,pa(p),ged(h,d)   )$((pt(p) and hf(h)) or (ps(p)and hs(h))) .. b_Delta_lo (y,p,h,d    ) *  ( vReserve(y,p,h,d)       - pMinReserve(h)                        )                           =e= 0;
     c_Delta_up      (y,pa(p),ged(h,d)   )$((pt(p) and hf(h)) or (ps(p)and hs(h))) .. b_Delta_up (y,p,h,d    ) *  (-vReserve(y,p,h,d)       + pMaxReserve(h)                        )                           =e= 0;
   //c_Delta_lo2     (y,ps(p),ged(h,d))$pn(p) .. b_Delta_lo2 (y,p,h,d   ) *  (-vReserve(y,p,h,d)       + pMaxReserve(h)                     )                           =l= pEps;
   //c_Mu_lo         (y,pa(p),ged(h,d)   ) .. b_Mu_lo    (y,p,h,d    ) *  ( vSpillage(y,p,h,d)      - pzero                                 )                           =l= pEps;
   //c_Mu_up         (y,pa(p),ged(h,d)   ) .. b_Mu_up    (y,p,h,d    ) *  ( vSpillage(y,p,h,d)      - pMaxReserve(h)                        )                           =l= pEps;
   //c_Delta_lo2    (y,ps(p),ged(h,d))$pn(p) .. b_Delta_lo2 (y,p,h,d   ) *  (-vReserve(y,p,h,d)       + pMaxReserve(h)                      )                           =l= pEps;
   //c_phi_lo        (y,pa(p),s,le(di,df)) .. b_phi_lo   (y,p,s,di,df) *  ( vFlow(y,p,s,  di,df)    + pTTC(di,df)                           )                           =l= pEps;
   //c_phi_up        (y,pa(p),s,le(di,df)) .. b_phi_up   (y,p,s,di,df) *  (-vFlow(y,p,s,  di,df)    + pTTC(di,df)                           )                           =l= pEps;
   //c_zeta_lo       (y,pa(p),s,lc(di,df)) .. b_zeta_lo  (y,p,s,di,df) *  ( vFlow(y,p,s,  di,df) /    pTTC(di,df)  + (vNewLine_R(y,di,df))  )                           =l= pEps;
   //c_zeta_up       (y,pa(p),s,lc(di,df)) .. b_zeta_up  (y,p,s,di,df) *  (-vFlow(y,p,s,  di,df) /    pTTC(di,df)  + (vNewLine_R(y,di,df))  )                           =l= pEps;
   //c_tau_lo        (y,pa(p),s,lc(di,df)) .. b_tau_lo   (y,p,s,di,df) *  ( vFlow(y,p,s,  di,df) / (1e+3/1e+3)*pTTC(di,df) -([vTheta(y,p,s,di) - vTheta(y,p,s,df)] * pSbase / pX(di,df) / (1e+3/1e+3)*pTTC(di,df) - 1 +vNewLine_R(y,di,df)) )=l= pEps;
   //c_tau_up        (y,pa(p),s,lc(di,df)) .. b_tau_up   (y,p,s,di,df) *  (-vFlow(y,p,s,  di,df) / (1e+3/1e+3)*pTTC(di,df) +([vTheta(y,p,s,di) - vTheta(y,p,s,df)] * pSbase / pX(di,df) / (1e+3/1e+3)*pTTC(di,df) + 1 -vNewLine_R(y,di,df)) )=l= pEps;
 //*********************  LINEARIZED COMPLEMENTARITY    ********************************
    l_Delta_lo1     (y,pt(p),ged(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))) .. b_Delta_lo (y,p,h,d    )                                     =L= M_Delta_lo1*    Y_Delta_lo(y,p,h,d    )                       ;
    l_Delta_lo2     (y,pt(p),ged(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))) .. ( vReserve (y,p,h,d)      - pMinReserve(h)                 ) =L= M_Delta_lo2* (1-Y_Delta_lo(y,p,h,d    ))                      ;
    l_Delta_up1     (y,pt(p),ged(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))) .. b_Delta_up (y,p,h,d    )                                     =L= M_Delta_up1*    Y_Delta_up(y,p,h,d    )                       ;
    l_Delta_up2     (y,pt(p),ged(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))) .. (-vReserve (y,p,h,d)      + pMaxReserve(h)                 ) =L= M_Delta_up2* (1-Y_Delta_up(y,p,h,d    ))                      ;                       ;
    l_DeltN_lo1     (y,pt(p),gcd(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))) .. b_DeltN_lo (y,p,h,d    )                                     =L= M_DeltN_lo1*    Y_DeltN_lo(y,p,h,d    )                       ;
    l_DeltN_lo2     (y,pt(p),gcd(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))) .. ( vReserve (y,p,h,d) -vNewGen (y,h,d)*pMinProd(h)*pEprMin(h))=L= M_DeltN_lo2* (1-Y_DeltN_lo(y,p,h,d    ))                      ;
    l_DeltN_up1     (y,pt(p),gcd(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))) .. b_DeltN_up (y,p,h,d    )                                     =L= M_DeltN_up1*    Y_DeltN_up(y,p,h,d    )                       ;
    l_DeltN_up2     (y,pt(p),gcd(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))) .. (-vReserve (y,p,h,d) +vNewGen (y,h,d)*pMaxProd(h,d)*pEprMax(h))=L= M_DeltN_up2* (1-Y_DeltN_up(y,p,h,d    ))                      ;

    l_Alpha_up1     (y,pa(p),      d  ) .. b_Alpha_up (y,p,    d)                                        =l= M_alpha_up1 *    Y_alpha_up (y,p,  d  )    ;
    l_Alpha_up2     (y,pa(p),      d  ) .. vDemand    (y,p,    d)   - 0                                  =l= M_alpha_up1 * (1-Y_alpha_up (y,p,  d  ))   ;                                                                                 ; // b_Alpha_up
    l_cRo_up1       (y,pa(p),ged(g,d) )$(not wn(g) and not sr(g)) .. b_cRo_up   (y,p,g,d  )                            =L= M_cRo_up1   *    Y_cRo_up   (y,p,g,d  )    ;
    l_cRo_up2       (y,pa(p),ged(g,d) )$(not wn(g) and not sr(g)) .. (-vProduct (y,p,g,d  )  + pMaxProd(g,d)      )      =L= M_cRo_up2   * (1-Y_cRo_up   (y,p,g,d  ))   ;
    l_cRo_lo1       (y,pa(p),ged(g,d) )$(not wn(g) and not sr(g)) .. b_cRo_lo   (y,p,g,d  )                            =L= M_cRo_lo1   *    Y_cRo_lo   (y,p,g,d  )    ;
    l_cRo_lo2       (y,pa(p),ged(g,d) )$(not wn(g) and not sr(g)) .. ( vProduct (y,p,g,d  )  - pMinProd(g)       )     =L= M_cRo_lo2   * (1-Y_cRo_lo   (y,p,g,d  ))   ;
    l_cRo_upw1      (y,pa(p),ged(g,d) )$(wn(g)) .. b_cRo_up_w   (y,p,g,d  )                              =L= M_cRo_up1   *   Y_cRo_up_w   (y,p,g,d  )   ;
    l_cRo_upw2      (y,pa(p),ged(g,d) )$(wn(g)) .. (-vProduct (y,p,g,d  )  + pMaxProd(g,d)           )     =L= M_cRo_up2   * (1-Y_cRo_up_w (y,p,g,d  ))   ;
    l_cRo_low1      (y,pa(p),ged(g,d) )$(wn(g)) .. b_cRo_lo_w   (y,p,g,d  )                              =L= M_cRo_lo1   *   Y_cRo_lo_w   (y,p,g,d  )   ;
    l_cRo_low2      (y,pa(p),ged(g,d) )$(wn(g)) .. ( vProduct (y,p,g,d  )  - pMinProd(g)            )    =L= M_cRo_lo2   * (1-Y_cRo_lo_w (y,p,g,d  ))   ;

    l_Omega_lo1     (y,pa(p),gcd(g,d) )$(not wn(g) and not sr(g)) .. b_Omega_lo (y,p,g,d  )                            =L= M_Omega_lo1 *    Y_Omega_lo (y,p,g,d  )    ;
    l_Omega_lo2     (y,pa(p),gcd(g,d) )$(not wn(g) and not sr(g)) .. ( vProduct (y,p,g,d  )  - pzero               )   =L= M_Omega_lo2 * (1-Y_Omega_lo (y,p,g,d  ))   ;
    l_Omega_up1     (y,pa(p),gcd(g,d) )$(not wn(g) and not sr(g)) .. b_Omega_up (y,p,g,d  )                            =L= M_Omega_up1 *    Y_Omega_up (y,p,g,d  )    ;
    l_Omega_up2     (y,pa(p),gcd(g,d) )$(not wn(g) and not sr(g)) .. (-vProduct (y,p,g,d  )+ vNewGen(y,g,d)*pMaxProd(g,d))=L= M_Omega_up2 * (1-Y_Omega_up (y,p,g,d  ))   ;

    l_Omega_low1    (y,pa(p),gcd(g,d) )$(wn(g)) .. b_Omega_lo_w (y,p,g,d  )                              =L= M_Omega_lo1 *    Y_Omega_lo_w (y,p,g,d  )    ;
    l_Omega_low2    (y,pa(p),gcd(g,d) )$(wn(g)) .. ( vProduct (y,p,g,d  )  - pzero                  )    =L= M_Omega_lo2 * (1-Y_Omega_lo_w (y,p,g,d  ))   ;
    l_Omega_upw1    (y,pa(p),gcd(g,d) )$(wn(g)) .. b_Omega_up_w (y,p,g,d  )                              =L= M_Omega_up1 *    Y_Omega_up_w (y,p,g,d  )    ;
    l_Omega_upw2    (y,pa(p),gcd(g,d) )$(wn(g)) .. (-vProduct (y,p,g,d  )  + vNewGen(y,g,d)*pWindGenNode (p,g,d))=L= M_Omega_up2 * (1-Y_Omega_up_w (y,p,g,d  ))   ;

    l_Kappa_lo1     (y,pa(p),ged(h,d) ) .. b_Kappa_lo (y,p,h,d  )                                         =L= M_Kappa_lo1 *    Y_Kappa_lo (y,p,h,d  )    ;
    l_Kappa_lo2     (y,pa(p),ged(h,d) ) .. ( vConsump (y,p,h,d  )  - pzero                           )    =L= M_Kappa_lo2 * (1-Y_Kappa_lo (y,p,h,d  ))   ;
    l_Kappa_up1     (y,pa(p),ged(h,d) ) .. b_Kappa_up (y,p,h,d  )                                         =L= M_Kappa_up1 *    Y_Kappa_up (y,p,h,d  )    ;
    l_Kappa_up2     (y,pa(p),ged(h,d) ) .. (-vConsump (y,p,h,d  )  + pMaxCons(h)                     )    =L= M_Kappa_up2 * (1-Y_Kappa_up (y,p,h,d  ))   ;
    l_KappN_lo1     (y,pa(p),gcd(h,d) ) .. b_KappN_lo (y,p,h,d  )                                         =L= M_KappN_lo1 *    Y_KappN_lo (y,p,h,d  )    ;
    l_KappN_lo2     (y,pa(p),gcd(h,d) ) .. ( vConsump (y,p,h,d  )  - pzero                           )    =L= M_KappN_lo2 * (1-Y_KappN_lo (y,p,h,d  ))   ;
    l_KappN_up1     (y,pa(p),gcd(h,d) ) .. b_KappN_up (y,p,h,d  )                                         =L= M_KappN_up1 *    Y_KappN_up (y,p,h,d  )    ;
    l_KappN_up2     (y,pa(p),gcd(h,d) ) .. (-vConsump (y,p,h,d)+vNewGen(y,h,d)*pMaxProd(h,d)*(2-pEffic(h))) =L= M_KappN_up2 * (1-Y_KappN_up (y,p,h,d  ))   ;
    l_phi_lo1       (y,pa(p),le(di,df)) .. b_phi_lo   (y,p,di,df)                                         =L= M_phi_lo1   *    Y_phi_lo   (y,p,di,df)    ;
    l_phi_lo2       (y,pa(p),le(di,df)) .. ( vFlow    (y,p,di,df)    + pTTC(di,df)                    )   =L= M_phi_lo2   * (1-Y_phi_lo   (y,p,di,df))   ;
    l_phi_up1       (y,pa(p),le(di,df)) .. b_phi_up   (y,p,di,df)                                         =L= M_phi_up1   *    Y_phi_up   (y,p,di,df)    ;
    l_phi_up2       (y,pa(p),le(di,df)) .. (-vFlow    (y,p,di,df)    + pTTC(di,df)                    )   =L= M_phi_up2   * (1-Y_phi_up   (y,p,di,df))   ;
    l_zeta_lo1      (y,pa(p),lc(di,df)) .. b_zeta_lo  (y,p,di,df)                                         =L= M_zeta_lo1  *    Y_zeta_lo  (y,p,di,df)    ;
    l_zeta_lo2      (y,pa(p),lc(di,df)) ..(vFlow      (y,p,di,df) / pTTC(di,df)+ (vNewLine(y,di,df)))     =L= M_zeta_lo2  * (1-Y_zeta_lo  (y,p,di,df))   ;
    l_zeta_up1      (y,pa(p),lc(di,df)) .. b_zeta_up  (y,p,di,df)                                         =L= M_zeta_up1  *    Y_zeta_up  (y,p,di,df)    ;
    l_zeta_up2      (y,pa(p),lc(di,df)) ..(-vFlow     (y,p,di,df) / pTTC(di,df)+ (vNewLine(y,di,df)))     =L= M_zeta_up2  * (1-Y_zeta_up  (y,p,di,df))   ;
    l_dualpf11      (y,pa(p),la(di,df))$(pObjective>1) ..-vPriceFlow1(y,p,di,df)+b_cLambda(y,p,di)+M_cLambda*vFlow(y,p,df,di)=L= M_dualpf1*   Y_dualpf1  (y,p,di,df)    ;
    l_dualpf12      (y,pa(p),la(di,df))$(pObjective>1) .. b_dualpf1  (y,p,di,df)                          =L= M_dualpf1*(1-Y_dualpf1  (y,p,di,df))   ;
    l_dualpf21      (y,pa(p),la(di,df))$(pObjective>1) ..-vPriceFlow2(y,p,di,df)+b_cLambda(y,p,di)+M_cLambda*vFlow(y,p,df,di)=L= M_dualpf2*   Y_dualpf2  (y,p,di,df)    ;
    l_dualpf22      (y,pa(p),la(di,df))$(pObjective>1) .. b_dualpf2  (y,p,di,df)                          =L= M_dualpf2*(1-Y_dualpf2  (y,p,di,df))   ;

    l_tau_lo1       (y,pa(p),lc(di,df)) .. b_tau_lo   (y,p,di,df)                                         =L= M_tau_lo1 * (  Y_tau_lo  (y,p,di,df))                       ;
    l_tau_lo2       (y,pa(p),lc(di,df)) .. ( vFlow(y,p,  di,df) / (1e+3/1e+3)*pTTC(di,df) -([vTheta(y,p,di) -   vTheta(y,p,df)] * pSbase / pX(di,df) / (1e+3/1e+3)*pTTC(di,df)
                                                                               - 1 +vNewLine(y,di,df)) )
                                                                                                          =L= M_tau_lo2 * (1-Y_tau_lo  (y,p,di,df))                       ;
    l_tau_up1       (y,pa(p),lc(di,df)) .. b_tau_up   (y,p,di,df)                                         =L= M_tau_up1 * (  Y_tau_up  (y,p,di,df))                       ;

    l_tau_up2       (y,pa(p),lc(di,df)) .. (-vFlow(y,p,  di,df) / (1e+3/1e+3)*pTTC(di,df) +([vTheta(y,p,di) - vTheta(y,p,df)] * pSbase / pX(di,df) / (1e+3/1e+3)*pTTC(di,df) + 1 -vNewLine(y,di,df)) )
                                                                                                         =L= M_tau_up2 * (1-Y_tau_up  (y,p,di,df))                        ;
    //l_Betha1        (y,        gcd(g,d) ) .. b_Betha    (y,    g,  d)                                     =L= M_Betha1   *    Y_Betha   (y,    g,d  )                      ;
    //l_Betha2        (y,        gcd(g,d) ) .. (-vNewGen(y-1, g,d) + vNewGen  (y, g,d  )      ) =L= M_Betha2   * (1-Y_Betha   (y,    g,d  ))                     ;
    //l_omic_lo1      (y,       gcd(g,d)) .. b_omic_lo  (y,    g,  d)                                       =L= M_omic_lo1 *    Y_omic_lo (y,    g,  d)                     ;
    //l_omic_lo2      (y,       gcd(g,d)) .. (vNewGen(y, g,d  ) - pzero                           )   =L= M_omic_lo2 * (1-Y_omic_lo (y,    g,  d))                    ;
    //l_omic_up1      (y,       gcd(g,d)) .. b_omic_up  (y,    g,  d)                                       =L= M_omic_up1 *    Y_omic_up (y,    g,  d)                     ;
    //l_omic_up2      (y,       gcd(g,d)) .. (-vNewGen(y, g,d  ) + pMaxNewGen                     )   =L= M_omic_up2 * (1-Y_omic_up (y,    g,  d))
    //_theta_up1     (y,pa(p),  d      ) .. b_theta_up (y,p,      d)                                       =L= M_theta_up1*    Y_theta_up(y,p,    d)                       ;
    //_theta_up2     (y,pa(p),  d      ) .. (-vTheta(y,p,  d)         + pMaxtheta                      )   =L= M_theta_up2* (1-Y_theta_up(y,p,    d))                      ;
    //_theta_lo1     (y,pa(p),  d      ) .. b_theta_lo (y,p,      d)                                       =L= M_theta_lo1*    Y_theta_lo(y,p,    d)                       ;
    //_theta_lo2     (y,pa(p),  d      ) .. ( vTheta(y,p,  d)         + pMaxtheta                      )   =L= M_theta_lo2* (1-Y_theta_lo(y,p,    d))                      ;
    //_ji_up1        (y,pa(p),  d      ) .. b_ji_up    (y,p,  d    )                                       =L= M_ji_up1   *    Y_ji_up   (y,p,d    )                       ;
    //_ji_up2        (y,pa(p),  d      ) .. (-vENS(y,p,  d)+ pDemand(p)*pDemShare(d)*pCumDemIncr(y)  )     =L= M_ji_up2   * (1-Y_ji_up   (y,p,d    ))                      ;
    //_ji_lo1        (y,pa(p),  d      ) .. b_ji_lo    (y,p,  d    )                                       =L= M_ji_lo1   *    Y_ji_lo   (y,p,d    )                       ;
    //_ji_lo2        (y,pa(p),  d      ) ..  vENS(y,p, d)            - pzero                               =L= M_ji_lo2   * (1-Y_ji_lo   (y,p,d    ))                      ;
    //l_muN_lo1       (y,pa(p),gcd(h,d) ) .. b_muN_lo   (y,p,h,d  )                                       =L= M_muN_lo1  *    Y_muN_lo  (y,p,h,d    )                     ;
    //l_muN_lo2       (y,pa(p),gcd(h,d) ) .. ( vSpillage(y,p,h,d  )      - pzero                          )   =L= M_muN_lo2  * (1-Y_muN_lo  (y,p,h,d    ))                    ;
    //l_MuN_up1       (y,pa(p),gcd(h,d) ) .. b_MuN_up    (y,p,h,d )                                      =L= M_MuN_up1  *    Y_MuN_up  (y,p,h,d    )                     ;
    //l_MuN_up2       (y,pa(p),gcd(h,d) ) .. (-vSpillage(y,p,h,d) +vNewGen (y, h,d)*pMaxReserve(h))   =L= M_MuN_up2  * (1-Y_MuN_up  (y,p,h,d    ))                    ;
    //l_Mu_lo1        (y,pa(p),ged(h,d) ) .. b_Mu_lo    (y,p,h,d  )                                         =L= M_Mu_lo1   *    Y_Mu_lo   (y,p,h,d  )       ;
    //l_Mu_lo2        (y,pa(p),ged(h,d) ) .. ( vSpillage(y,p,h,d  )  - pzero                           )    =L= M_Mu_lo2   * (1-Y_Mu_lo   (y,p,h,d  ))      ;
    //l_Mu_up1        (y,pa(p),ged(h,d) ) .. b_Mu_up    (y,p,h,d  )                                         =L= M_Mu_up1   *    Y_Mu_up   (y,p,h,d  )       ;
    //l_Mu_up2        (y,pa(p),ged(h,d) ) .. (-vSpillage(y,p,h,d  )  + pMaxReserve(h)                  )    =L= M_Mu_up2   * (1-Y_Mu_up   (y,p,h,d  ))      ;


 //*********************        DUAL CONSTRAINTS        ********************************

    dL_dvDemand  (y,pa(p),d        )$(not pTypeComp )  ..
                                        +  sum(rpp(rp,p), pWeight_rp(rp)*( (pDemandNode   (y,p,d      )/Slope(p,d)
                                        -  vDemand    (y,p,d)  /Slope(p,d))$(Slope(p,d))
                                        -  b_cLambda  (y,p,d)
                                        +  b_Alpha_up (y,p,d) ))

                                        =e= 0   ;



    dL_dvProd    (y,pa(p),gad(g,d) )$( not wn(g) and not sr(g)) ..//$ (pTypeComp )..
                                        - sum(           rpp(rp,p) , pWeight_rp(rp)* pSlopeVarCost(g) )$ (   t(g))$pa(p) //$ (    (pTypeComp ) and t(g))
                                        - sum[           rpp(rp,p) , pWeight_rp(rp)* pHydroCost ]$h(g)
                                        - sum(           rpp(rp,p) , pWeight_rp(rp)*(conjcvariation(p,g,d))*(vProduct(y,p,g,d)-vConsump(y,p,g,d)$h(g))) $(pa(p))//
                                        + sum(           rpp(rp,p) , pWeight_rp(rp)* b_cLambda(y,p,d) )$pa(p) //$(    pTypeComp )
                                        -              ( b_Psi      (y,p,g,d))$(hf(g)and pa(p))//pProdFunct(g))

                                        + sum(ged(g, d), b_cRo_lo   (y,p,g,d))$pa(p)
                                        - sum(ged(g, d), b_cRo_up   (y,p,g,d))$pa(p)
                                        + sum(gcd(g, d), b_Omega_lo (y,p,g,d))$pa(p)
                                        - sum(gcd(g, d), b_Omega_up (y,p,g,d))$pa(p)

                                        + sum(indexRP(ppp,pp)$ (ps(ppp)and ORD(ppp) <= CARD(ppp)),
                                                 sum(pppp$[ORD(pppp) >= ORD(ppp)+1-pStorMovWindow
                                                     AND ORD(pppp) <= ORD(ppp)and indexRP(pppp,p)],
                                                         (b_zeus1 (y,ppp,g,d))$(hs(g))))//pProdFunct(g)
                                        =e= 0;

    dL_dvWind    (y,pa(p),gad(wn,d))$( not pTypeComp )..
                                        + sum(rpp(rp,p) , pWeight_rp(rp)* b_cLambda(y,p,d) )
                                        - sum(rpp(rp,p) , pWeight_rp(rp)*(conjcvariation(p,wn,d))*(vWind(y,p,wn,d)))
                                        + sum(ged(wn, d), b_cRo_lo_w  (y,p,wn,d))
                                        - sum(ged(wn, d), b_cRo_up_w  (y,p,wn,d))
                                        + sum(gcd(wn, d), b_Omega_lo_w(y,p,wn,d))
                                        - sum(gcd(wn, d), b_Omega_up_w(y,p,wn,d))
                                          =e=0;

     //*Hay que mejor definirlo arriba y entonces no se tiene que calcular en cada estancia de la ecuación. Está bien pero no es lo más optimo
    dL_dvProdCP  (y,pa(p),d,    cp )$( (not pTypeComp ) and (sum((gad(g, d),gcp(g,cp)),1)>=1))..

                                        - sum((gcp(t,cp),rpp(rp,p),gad(t,d)), pSlopeVarCost(      t  )*pWeight_rp(rp) )
                                        + sum((gcp(g,cp),rpp(rp,p)         ), b_cLambda    (y,p,  d)  *pWeight_rp(rp) )
                                        - sum((gcp(g,cp),rpp(rp,p),gad(g,d)), vProduct     (y,p,g,d)  *pWeight_rp(rp)*(conjcvariation(p,g,d)) )
                                        - sum((gcp(g,cp)          ,gad(g,d)),(b_Psi        (y,p,g,d) )$(hf(g))) ///pProdFunct(g)
                                        + sum((gcp(g,cp)          ,ged(g,d)), b_cRo_lo     (y,p,g,d)  )
                                        - sum((gcp(g,cp)          ,ged(g,d)), b_cRo_up     (y,p,g,d)  )
                                        + sum((gcp(g,cp)          ,gcd(g,d)), b_Omega_lo   (y,p,g,d)  )
                                        - sum((gcp(g,cp)          ,gcd(g,d)), b_Omega_up   (y,p,g,d)  )

                                        //- sum(gcp(h,cp), sum(indexRP(ppp,pp)$ (ps(ppp)and ORD(ppp) <= CARD(ppp)),
                                        //        sum(pppp$[ORD(pppp) >= ORD(ppp)+1-pStorMovWindow
                                        //                AND ORD(pppp) <= ORD(ppp)and indexRP(pppp,p)],
                                        //                                (b_zeus1 (y,ppp,h,d)/pProdFunct(h)))))
                                        =e= 0;


    dL_dvNewGen  (y,            g,d)$(gcd(g,d)and NOT (pTypeComp ))..
     //*                                    -sum(gcp(g,cp),(card(y)-ord(y)+1)*pGenInvCost (g)  + (card(y)-ord(y))*pGenInvCost (g)) $ (not pTypeComp and )
                                        - pGenInvCost (g) // $ (    pTypeComp  )

                                        + sum((pa(p)), b_Omega_up  (y,p,g,d)*pMaxProd(g,d))
                                        + sum((pa(p)), b_Omega_up_w(y,p,g,d)*pWindGenNode (p,g,d)) $(wn(g))
                                       // - sum((pa(p)), b_KappN_up(y,p,g,d)*pMaxCons   (g))$h(g)
                                        + sum((ps(p)), b_KappN_up  (y,p,g,d)*pMaxProd(g,d)*(2-pEffic(g)))$hs(g)
                                        - sum((ps(p)), b_KappN_lo  (y,p,g,d)*pMinProd(g)*(2-pEffic(g)))$hs(g)
                                        + sum((pa(p)), b_KappN_up  (y,p,g,d)*pMaxProd(g,d)*(2-pEffic(g)))$hf(g)
                                        - sum((pa(p)), b_KappN_lo  (y,p,g,d)*pMinProd(g)*(2-pEffic(g)))$hf(g)

                                        + sum((ps(p)), b_DeltN_up  (y,p,g,d)*pMaxProd(g,d)*pEprMax(g)   )$hs(g)
                                        - sum((ps(p)), b_DeltN_lo  (y,p,g,d)*pMinProd(g)*pEprMin(g)   )$hs(g)
                                        + sum((pa(p)), b_DeltN_up  (y,p,g,d)*pMaxProd(g,d)*pEprMax(g)   )$hf(g)
                                        - sum((pa(p)), b_DeltN_lo  (y,p,g,d)*pMinProd(g)*pEprMin(g)   )$hf(g)
                                        - sum(pa(p), b_Psi  (y,p,g,d ) $[not pa(p-1) and hf(g)])

     //*                                     + sum((pa(p)  ), b_muN_up  (y,p,  g,d)*pMaxReserve(g))$h(g)
     //*                                     - b_omic_up (y,g,d)       +  b_omic_lo (y,    g,d)
     //*                                     + b_Betha   (y,g,d)       -  b_Betha   (y+1,  g,d)
                                        =e= 0;

    dL_dvNewGenCP(y,        d,   cp)$(not pTypeComp  )..
                                        + sum( gcp(g,cp),         -pGenInvCost(      g  )$gcd(g, d))
     //*                                + sum( gcp(t,cp),-(card(y)-ord(y)+1)*pGenInvCost (t)$gcd(t, d)  + (card(y)-ord(y))*pGenInvCost (t)$gcd(t, d))
                                        + sum((gcp(g,cp),pa(p)), b_Omega_up (y,p,g,d)*pMaxProd   (g,d)$gcd(g, d))
                                        + sum((gcp(h,cp),pa(p)), b_KappN_up (y,p,h,d)*pMaxCons   (h)$gcd(h, d))
                                        + sum((gcp(h,cp),pt(p)), b_DeltN_up (y,p,h,d)*pMaxReserve(h)$gcd(h, d))
                                        - sum((gcp(h,cp),pt(p)), b_DeltN_lo (y,p,h,d)*pMinReserve(h)$gcd(h, d))
                                        + sum((gcp(h,cp),pa(p)), b_muN_up   (y,p,h,d)*pMaxReserve(h)$gcd(h, d))
     //                                 + sum(gcp(g,cp), (- b_omic_up (y,g,d)       +  b_omic_lo (y,    g,d))$gcd(g, d)  )
     //                                 + sum(gcp(t,cp), (+ b_Betha   (y,t,d)       -  b_Betha   (y+1,  t,d))$gcd(t, d)  )
                                        =e= 0;

    dl_dvENS     (y,pa(p),d        )..
                                            -sum(rpp(rp,p), pWeight_rp(rp) * pENSCost)
     //*                                     + b_cLambda(y,p,d)
                                        - b_ji_up(y,p,d )
                                        + b_ji_lo(y,p,d )
                                        =e= 0;

    dL_dvFlow    (y,pa(p),la(di,df))..
     //*TAKI INTO ACCOUNT IF WE HAVE TO DEACTIVATE IT OR NOT (FIRST LINE )
     //*                                     +sum[rpp(rp,p),0.1 ]
                                        //+sum(rpp(rp,p), pWeight_rp(rp)*(+ b_cLambda(y,p,di) -  b_cLambda(y,p,df)))//$(pTypeComp )
                                        +(+ b_cLambda(y,p,di) -  b_cLambda(y,p,df))$la(di,df)
                                        + b_phi    (y,p,di,df)$le(di,df)
                                        //- conjcvariation*vflow(y,p,di,df)$le(di,df) + conjcvariation*vFlow(y,p,di,df)$le(df,di)
     //signos cambiados
                                        + b_phi_up (y,p,di,df)$le(di,df)                   - b_phi_lo (y,p,di,df)$le(di,df)
                                        + b_zeta_up(y,p,di,df)$lc(di,df)/      pTTC(di,df) - b_zeta_lo(y,p,di,df)$lc(di,df)/      pTTC(di,df)
                                        - b_tau_up (y,p,di,df)$lc(di,df)/ (1e+3/1e+3)*pTTC(di,df) + b_tau_lo (y,p,di,df)$lc(di,df)/ (1e+3/1e+3)*pTTC(di,df)
                                        //- (b_dualpf1(y,p,di,df)*M_cLambda)$la(df,di)       - (b_dualpf2(y,p,di,df)*M_cLambda)$la(df,di)

                                        =e= 0;

    dL_dvTheta   (y,pa(p),   di    )$(sum(la(di,d),1)+sum(la(d,di),1))..
                                        + sum[df $(le(di,df)) ,b_phi    (y,p,di,df)*pSbase / pX(di,df)]
                                        - sum[df $(le(df,di)) ,b_phi    (y,p,df,di)*pSbase / pX(df,di)]
                                        + sum[df $(lc(di,df)) ,b_tau_lo (y,p,di,df)*pSbase / pX(di,df)/ (1e+3/1e+3)*pTTC(di,df)]
                                        - sum[df $(lc(df,di)) ,b_tau_lo (y,p,df,di)*pSbase / pX(df,di)/ (1e+3/1e+3)*pTTC(df,di)]
                                        - sum[df $(lc(di,df)) ,b_tau_up (y,p,di,df)*pSbase / pX(di,df)/ (1e+3/1e+3)*pTTC(di,df)]
                                        + sum[df $(lc(df,di)) ,b_tau_up (y,p,df,di)*pSbase / pX(df,di)/ (1e+3/1e+3)*pTTC(df,di)]
                                        =e= 0;

     //*TAKI INTO ACCOUNT IF WE HAVE TO DEACTIVATE IT OR NOT (FIRST LINE )
    dL_dvCon     (y,pa(p),gad(h,d) )..
                                        //- sum(ged(h,d),(b_cLambda (y,p,  d)/pEffic(h))) // $(pTypeComp )
                                        - sum(rpp(rp,p), pWeight_rp(rp)*b_cLambda (y,p,  d))$pa(p) // $(pTypeComp ) /pEffic(h)
                                        + sum(rpp(rp,p), pWeight_rp(rp)*(conjcvariation(p,h,d))* (vProduct(y,p,h,d)-vConsump(y,p,h,d) ))$pa(p) //
                                        -  b_Kappa_up(y,p,h,d)$(ged(h,d)and pa(p))  +  b_Kappa_lo(y,p,h,d)$(ged(h,d)and pa(p))
                                        -  b_KappN_up(y,p,h,d)$(gcd(h,d)and pa(p))  +  b_KappN_lo(y,p,h,d)$(gcd(h,d)and pa(p))
                                        - sum[rpp(rp,p) , pWeight_rp(rp)* pConsumCost ]
                                        + (b_Psi     (y,p,  h,d)*pEffic(h)) $ (hf(h)and pa(p))///pProdFunct(h)

                                        - sum(indexRP(ppp,pp)$ (ps(ppp)and ORD(ppp) <= CARD(ppp)),
                                                sum(pppp$[ORD(pppp) >= ORD(ppp)+1-pStorMovWindow
                                                        AND ORD(pppp) <= ORD(ppp)and indexRP(pppp,p)],
                                                                        (b_zeus1 (y,ppp,h,d)*pEffic(h))))$hs(h)///pProdFunct(h)

                                        =e= 0;



    dL_dReserv   (y,pt(p), gad(h,d) )$[(pa(p) and hf(h)) or (ps(p)and hs(h))]..
     //*$(card(p)>ord(p))
                                        +  b_Delta_lo(y,p,h,d)$(ged(h,d)and pa(p)and hf(h))  -  b_Delta_up(y  ,p,h,d)$(ged(h,d)and pa(p)and hf(h))
                                        +  b_Delta_lo(y,p,h,d)$(ged(h,d)and ps(p)and hs(h))  -  b_Delta_up(y  ,p,h,d)$(ged(h,d)and ps(p)and hs(h))
                                        //                                  +  b_Delta_lof(y,p,h,d)$(ged(h,d) and pn(p))
                                        +  b_DeltN_lo(y,p,h,d)$(gcd(h,d)and pa(p)and hf(h))  -  b_DeltN_up(y  ,p,h,d)$(gcd(h,d)and pa(p)and hf(h))
                                        +  b_DeltN_lo(y,p,h,d)$(gcd(h,d)and ps(p)and hs(h))  -  b_DeltN_up(y  ,p,h,d)$(gcd(h,d)and ps(p)and hs(h))

                                        -  b_Psi  (y,p,h,d )   $(pa(p) and hf(h)         )  +  b_Psi     (y,p+1,h,d)$(pa(p)   and hf(h) and pa(p+1) )//
                                        +  b_zeus1(y,p,h,d)                $(ps(p) and hs(h) and (ORD(p) <= CARD(p)))
                                        -  b_zeus1(y,p+pStorMovWindow,h,d) $[ps(p) and hs(h) and (ORD(p) <= CARD(p))]
                                        =e= 0;

    dL_dSpill    (y,pa(p),  gad(h,d) ) ..
                                        //* - sum[(rpp(rp,p)), pWeight_rp(rp) * pzero]
                                        +  b_Mu_lo  (y,p,h,d)$ged(h,d)    -  b_Mu_up    (y,p,h,d)$ged(h,d)
                                        +  b_muN_lo (y,p,h,d)$gcd(h,d)    -  b_muN_up   (y,p,h,d)$gcd(h,d)
                                        -  b_Psi       (y,p,h,d) $hf(h)
                                        -sum[(rpp(rp,p)), pWeight_rp(rp) * 0.001 ]
                                        //-   sum((indexRP(ppp,pp))$ (ps(ppp)and ORD(ppp) <= CARD(ppp)),
                                        //        sum(pppp$[ORD(pppp) >= ORD(ppp)+1-pStorMovWindow
                                        //                AND ORD(pppp) <= ORD(ppp)and indexRP(pppp,p)],
                                        //                                b_zeus1  (y,ppp,h,d)))



                                     =e= 0;

    dL_dVPriceFlow1(y,pa(p),la(di,df) )$(pObjective=2) ..
                                      +sum[(rpp(rp,p)), pWeight_rp(rp) ]
                                      -b_dualpf1 (y,p,di,df )
                                      =e= 0;
    dL_dVPriceFlow2(y,pa(p),la(di,df) )$(pObjective=2) ..
                                      -sum[(rpp(rp,p)), pWeight_rp(rp) ]
                                      -b_dualpf2 (y,p,di,df )
                                      =e= 0;



////////////////////////////    MODEL DEFINITION      //////////////////////////////////
    MODEL GENCOs_CONSTRAINTS
      /
        eMinProd
        eMaxProd
        eMinProd_New
        eMaxProd_New
        eMinWind
        eMaxWind
        eMaxWindNew
        eMinWindNew
        eMinSolar
        eMaxSolar
        eMaxSolarNew
        eMinSolarNew
        BudgetSolar
        BudgetWind
        //*eGenInst_R_can
        //eMiniGen_R_can
        //eMaxiGen_R_can
        eMinCons
        eMaxCons
        eMinReserve
        eMaxReserve
        //*eFinalReserve
        //eMaxSpillage
        //eMinSpillage
        eMinCons_New
        eMaxCons_New
        eMinReserve_New
        eMaxReserve_New
        //eMinSpillage_New
        //eMaxSpillage_New
        //eStorage
        //eRSRVH_RP1_C_CAN
        

      /

    MODEL TSOs_CONSTRAINTS
        /
        eFlowNetEx
        eFlowExisting1
        eFlowExisting2
        eFlowInstlCap1
        eFlowInstlCap2
        eFlowNetN1
        eFlowNetN2
        e_intPrice1Flow1
        e_intPrice1Flow2
        e_intPrice2Flow1
        e_intPrice2Flow2
        e_intPriceFlow3
        e_intPriceFlow4
        e_PriceFlow1
        e_PriceFlow2
        e_intFLow
        //*eMaxTheta
        //*eMinTheta
        //*eCumLineInstall

        /
    MODEL COUPLING_CONSTRAINT
     /
     eBalance_C_can
     eMinDemand
     /

    MODEL PRIMAL_CONSTRAINTS
     /
      COUPLING_CONSTRAINT
      GENCOs_CONSTRAINTS
      TSOs_CONSTRAINTS

     /

    MODEL DUAL_CONSTRAINTS

        /
        dL_dvProd
        dL_dvWind
        //dL_dvProdCP
        //*dl_dvENS
        dL_dvFlow
        dL_dvTheta
        dL_dvNewGen
        //dL_dvNewGenCP
        dL_dvCon
        dL_dReserv
        //dL_dSpill
        dL_dvDemand
        /

    MODEL NON_LIN_COMPLEMENT
      /
      c_cRo_up
      c_cRo_lo
      c_Omega_lo
      c_Omega_up
      c_Delta_lo
      c_Delta_up
      c_Kappa_up
      c_Kappa_lo
      /

    MODEL GEPTEP_COSTMIN
        /
        PRIMAL_CONSTRAINTS
        eTotalTCost
        eTotalFCost_MinCost
        eTotalVCost_MinCost
        /
        ;
    GEPTEP_COSTMIN.SolPrint = 1 ; GEPTEP_COSTMIN.HoldFixed = 1 ;
    MODEL LOW_LEVEL_MCP

        /
        eBalance_C_can.b_cLambda
        eMinProd.b_cRo_lo
        eMaxProd.b_cRo_up
        eMinProd_New.b_Omega_lo
        eMaxProd_New.b_Omega_up
        eMaxWind.b_cRo_up_w
        eMinWind.b_cRo_lo_w
        eMaxWindNew.b_Omega_up_w
        eMinWindNew.b_Omega_lo_w
        eMinDemand.b_Alpha_up
        //*eMinENS.b_ji_lo
        //*eMaxENS.b_ji_up
        //*eGenInst_R_can.b_Betha
        //*eMiniGen_R_can.b_Omic_lo
        //*eMaxiGen_R_can.b_Omic_up
        eMinCons.b_Kappa_lo
        eMaxCons.b_Kappa_up
        eMinReserve.b_Delta_lo
        eMaxReserve.b_Delta_up
        //*eFinalReserve.b_Delta_lo2
        //eMinSpillage.b_Mu_lo
        //eMaxSpillage.b_Mu_up
        eMinCons_New.b_KappN_lo
        eMaxCons_New.b_KappN_up
        eMinReserve_New.b_DeltN_lo
        eMaxReserve_New.b_DeltN_up
        //eMinSpillage_New.b_muN_LO
        //eMaxSpillage_New.b_muN_up
        eStorage.b_Psi
        eRSRVH_RP1_C_CAN.b_zeus1
        //*eRSRVH_RP2_C_CAN.b_zeus2
        eFlowNetEx.b_phi
        eFlowExisting1.b_phi_lo
        eFlowExisting2.b_phi_up
        eFlowInstlCap1_b.b_zeta_lo
        eFlowInstlCap2_b.b_zeta_up
        eFlowNetN1_b.b_tau_lo
        eFlowNetN2_b.b_tau_up
        dL_dvProd.vProduct
        //*dl_dvENS.vENS
        dl_dvWind.vWind
        dL_dvDemand.vDemand
        dL_dvFlow.vFlow
        dL_dvTheta.vTheta
        dL_dvNewGen.vNewGen
        dL_dvCon.vConsump
        dL_dReserv.vReserve
        //dL_dSpill.vSpillage

        /    ;
    LOW_LEVEL_MCP.SolPrint = 1  ; LOW_LEVEL_MCP.HoldFixed = 1 ;
    MODEL CUADRATIC_MARKET
        /
        eFExtendedCost
        PRIMAL_CONSTRAINTS
        //eUniqueSolution
        /      ;
    CUADRATIC_MARKET.SolPrint = 1 ; CUADRATIC_MARKET.HoldFixed = 1 ;  CUADRATIC_MARKET.optfile = 1 ;
    MODEL BILEVEL_KKT_MILP
        /
        //e_PositiveLines
        eFCost_Bilevel
        PRIMAL_CONSTRAINTS
        DUAL_CONSTRAINTS
        l_Alpha_up1
        l_Alpha_up2
        l_cRo_up1
        l_cRo_up2
        l_cRo_lo1
        l_cRo_lo2
        l_cRo_upw1
        l_cRo_upw2
        l_cRo_low1
        l_cRo_low2
        l_phi_lo1
        l_phi_lo2
        l_phi_up1
        l_phi_up2
        l_zeta_up1
        l_zeta_up2
        l_zeta_lo1
        l_zeta_lo2
        l_tau_up1
        l_tau_up2
        l_tau_lo1
        l_tau_lo2
        l_Omega_up1
        l_Omega_up2
        l_Omega_lo1
        l_Omega_lo2
        l_Omega_low1
        l_Omega_low2
        l_Omega_upw1
        l_Omega_upw2
        l_Kappa_up1
        l_Kappa_up2
        l_Kappa_lo1
        l_Kappa_lo2
        l_Delta_lo1
        l_Delta_lo2
        l_Delta_up1
        l_Delta_up2
        l_KappN_lo1
        l_KappN_lo2
        l_KappN_up1
        l_KappN_up2
        l_DeltN_lo1
        l_DeltN_lo2
        l_DeltN_up1
        l_DeltN_up2

        /    ;
    BILEVEL_KKT_MILP.SolPrint = 1 ; BILEVEL_KKT_MILP.optfile = 1 ; BILEVEL_KKT_MILP.HoldFixed = 1 ;
    MODEL NON_LINEAR_LOW_LEVEL
        /
        eOf
        PRIMAL_CONSTRAINTS
        DUAL_CONSTRAINTS
        NON_LIN_COMPLEMENT

        /    ;

    NON_LINEAR_LOW_LEVEL.SolPrint = 1 ;  NON_LINEAR_LOW_LEVEL.HoldFixed = 1 ;NON_LINEAR_LOW_LEVEL.optfile = 1 ;
    MODEL BILEVEL_KKT_REGULAR_MPEC1
        /
        //eFCost_Bilevel
        eFCost_Bilevel_R
        LOW_LEVEL_MCP
        /    ;

    BILEVEL_KKT_REGULAR_MPEC1.SolPrint = 1 ;  BILEVEL_KKT_REGULAR_MPEC1.HoldFixed = 1 ; BILEVEL_KKT_REGULAR_MPEC1.optfile = 1 ;    
    MODEL COST_MINIMISATION
     /
     GENCOs_CONSTRAINTS
     TSOs_CONSTRAINTS
     eTotalFCost_MinCost
     COUPLING_CONSTRAINT
     /
     ;
    COST_MINIMISATION.SolPrint = 1 ;  COST_MINIMISATION.HoldFixed = 1 ; COST_MINIMISATION.optfile = 1 ;
    FILE     KOPT / knitro.opt /
    PUT      KOPT / 'infeastol 1E-9' /
    PUTCLOSE KOPT

////////////////////////////  READING EXCEL INPUT     //////////////////////////////////

 file TMP / %gams.user1%.txt /
 $$OnEcho  > %gams.user1%.txt
   r1=    indices
   o1=tmp_indices.txt
   r2=    param
   o2=tmp_param.txt
   r3=    share
   o3=tmp_share.txt
   r4= HourSlope
   o4= HourSlope.txt
   r6=    thermalgen
   o6=tmp_thermalgen.txt
   r7=    storagegen
   o7=tmp_storagegen.txt
   r8=    inflows
   o8=tmp_inflows.txt
   r9=    Demand
   o9=    Demand
   r10=    network
   o10=tmp_network.txt
   r11=    renewgen
   o11=tmp_renewgen.txt
   r12=    ReprePeriods
   o12=tmp_repreperiods.txt
   r13=TransitionMatrix_rp
   o13=TransitionMatrix_rp
   r15=    Solar
   o15=tmp_solar.txt
   r14=    Hindex_RP
   o14=    indexRP.txt
   r16=    Wind
   o16=tmp_wind.txt
   r22=Weight
   o22=Weight


 $$OffEcho
////////////////////////////  LOADING DATA  TO GAMS   //////////////////////////////////
    // Mac OS X and Linux users must comment the following call and copy and paste the named ranges of the Excel interface into the txt files
    $$ifthen.OptSkipExcelInput '%OptSkipExcelInput%' == '0'
    $$call xls2gms m i="%gams.user1%.xlsm" @"%gams.user1%.txt"
    $$else.OptSkipExcelInput
    $$  log Excel input skipped
    $$endif.OptSkipExcelInput

    sets
    $$include tmp_indices.txt
    $$include tmp_repreperiods.txt
    ;
    $$include tmp_param.txt
    parameter
    $$INCLUDE Weight
    ;
    parameter
    $$INCLUDE HourSlope.txt
    ;
    parameter
    $$include Demand
    ;
    //table    pOperReserve(b)
    //$$include tmp_oprres.txt
     table    pThermalGen(g,*)
    $$include tmp_thermalgen.txt
    table    pStorageGen(g,*)
    $$include tmp_storagegen.txt
    table    pRenewGen(g,*)
    $$include tmp_renewgen.txt
    table    pShare (d,gr,*)
    $$include tmp_share.txt
    table    pWindGen(p,g)
    $$include tmp_wind.txt
    table    pSolarGen(p,g)
    $$include tmp_Solar.txt
    table    pNetwork(d,d,*)
    $$include tmp_network.txt
    table    pInflow(p,g)
    $$include tmp_inflows.txt
    TABLE    pTransMatrix_rp(rp,rrpp)
    $$INCLUDE TransitionMatrix_rp
    sets
    $$include indexrp.txt



    ;


 // Delete the loaded ranges from memory
   // EXECUTE 'del tmp_indices.txt tmp_param tmp_demand tmp_demand_dur'  ;
   // EXECUTE 'del tmp_thermalgen tmp_storagegen tmp_inflows tmp_HourlyDemand' ;
  //  EXECUTE 'del tmp_network tmp_renewgen tmp_repreperiods' ;
    execute 'del "%gams.user1%".txt tmp_indices.txt tmp_param.txt tmp_demand.txt  tmp_duration.txt tmp_thermalgen.txt tmp_storagegen.txt tmp_inflows.txt tmp_network.txt' ;
//////////////////////////// DYNAMIC SETS AND SCALING //////////////////////////////////
 // Activating period and generation sets
     pa(p) $[SUM[rpp(rp,p),1]] = YES ;
     t (g) $[pThermalGen(g,'MaxProd'   ) and pThermalGen(g,'FuelCost')] = yes ;
     h (g) $[pStorageGen(g,'MaxProd'   )                              ] = yes ;
     w (g) $[pRenewGen  (g,'MaxProd'   )                              ] = yes ;
     wn(g) $[pWindGen   ('h7393',     g)                              ] = yes ;
     sr(g) $[pSolarGen  ('h7393',     g)                              ] = yes ;
     hs(h)$[NOT pStorageGen (h,'Type'  )] = YES ; // activating "slow" storage technology
     hf(h)$[    pStorageGen (h,'Type'  )] = YES ; // activating "fast" storage technology
      display hs, hf;
     z            (y)  = yes ;
     pOrder       (y)  = ord(y) ;
     pCumDemIncr  (y)  = prod[z $[pOrder(z) <= ord(y)], 1+pDemIncr(z)] ;
     p1(p)$(ord(p)=1)  = yes;
     pn(p)$(ord(p)=card(p)) = yes;
     pMinTransition=0;
     dgr(d,gr) $ [pShare(d,gr,"Share")]=yes;
     pShareGroup (d,gr) =pShare(d,gr,"Share");
     //display p1,pa;

 // Scaling parameters

    pDemandNode   (y,p,d)   = sum((dgr(d,gr)), pDemand(p) * pDemShare(gr) * pCumDemIncr(y)*pShareGroup(d,gr))* 1e-3 ; // MW to GW  
    display pDemandNode; 
    slope(p,d)          = pslope(p)  ;

    PCO2price           = PCO2price                      * 1e-6 ;
    pEmissionrate(th)        = pEmissionrate(th)                                                    ;
    pENSCost                 = pENSCost                       * 1e-3                                ;
    pGenInvCost  (t)         = pThermalGen(t,'InvCost'      ) * 1e+3                                ;
    pGenInvCost  (w)         = pRenewGen  (w,'InvCost'      ) * 1e+3                                ;
    pGenInvCost  (wn)        = (0.0858)                       * 1e+3                                ;
    pGenInvCost  (sr)        = (0.052)                        * 1e+3                                ;
    pGenInvCost  (h)         = pStorageGen(h,'InvCost'      ) * 1e+3                                ;
    pTTC      (di,df)        = pNetwork   (di,df,'TTC'      ) * 1e-3                                ;
    pEFOR        (t)         = pThermalGen(t,'EFOR'         )                                       ;
    pSlopeVarCost(t)         = pThermalGen(t,'OMVarCost'    ) * 1e-3 + //
                               pThermalGen(t,'SlopeVarCost' ) * 1e-3 * pThermalGen(t,'FuelCost') +
                               pThermalGen(t,'FuelCost'     ) * 1e-3                                ;   
    pFixedCost   (di,df  )   = pNetwork   (di,df,'FixedCost') * pNetwork(di,df,'FxChargeRate') ;
    pX           (di,df  )   = pNetwork   (di,df,'X'        )                                       ;
    pG           (di,df  )   = pNetwork   (di,df,'G'        )                                       ;
    pSbase                   = pSbase                         * 1e-3                                ;
    pMaxProdGen  (  t    )   = pThermalGen(t,'MaxProd'      ) * 1e-3 * [1-pEFOR(t)]                 ;
    pMaxProdGen  (  h    )   = pStorageGen(h,'MaxProd'      ) * 1e-3                                ;
    pMinProd     (  t    )   = pThermalGen(t,'MinProd'      ) * 1e-3 * [1-pEFOR(t)]                 ;
    pMaxProdGen  (  w    )   = pRenewGen  (w,'MaxProd'      ) * 1e-3 * [1-pEFOR(w)]                 ;
    pWindGen     (p,wn   )   = pWindGen   (p,wn             ) * 1e-3                                ;
    pSolarGen    (p,sr   )   = pSolarGen  (p,sr             ) * 1e-3                                ;
   
 // Storage Parameters
    pMaxCons     (h)    = pStorageGen(h,'MaxCons'      ) * 1e-3 ;
    pProdFunct   (h)    = pStorageGen(h,'ProdFunct'    ) * 1e+3 ;
    pEffic       (h)    = pStorageGen(h,'Efficiency'   )        ;
    pMaxReserve  (h)    = pStorageGen(h,'MaxReserve'   ) * 1e-3 ;
    pMinReserve  (h)    = pStorageGen(h,'MinReserve'   ) * 1e-3 ;
    pIniReserve  (h)    = pStorageGen(h,'IniReserve'   ) * 1e-3 ;
    pFinReserve  (h)    = pStorageGen(h,'FinReserve'   ) * 1e-3 ;
    pEprMax      (h)    = pStorageGen(h,'EprMax'       )        ;
    pEprMin      (h)    = 0;
    pInflows    (p,h)   = pInflow     (p,h              )* 1e-3 ; ;//* 1e-6 * 3.6*sum[(s), pDuration(p,s)] ;
    pProdFunct   (h) $[pProdFunct(h) = 0] = 1e3 ;
    pEffic       (h) $[pEffic    (h) = 0] =   1 ;


 // Network set activation
        lc(   di,df ) $pFixedCost(di,df)  = yes ;
        la(   di,df ) $pX        (di,df)  = yes ;
        le(la(di,df)) $[not    lc(di,df)] = yes ;
        ged(g,d) $[sum(gedgr(g,d,gr), 1)]  = yes ;
        gad(g,d) $[gcd(g,d) or ged(g,d)]  = yes;

 // Scaling group node 

  pMaxProd     (   ged(g ,d) )  = sum( (dgr(d,gr),gedgr(g,d,gr)),pMaxProdGen (  g )*pShareGroup(d,gr));
  pMaxProd     (   gcd(g ,d) )  = pMaxProdGen (  g );
  pMaxWindGen  (   gcd(wn,d) )  = smax(p, pWindGen    (p,wn) );
  pMaxSolarGen (   gcd(sr,d) )  = smax(p, pSolarGen   (p,sr) );
  pSolarGenNode(p, gad(sr ,d))  = sum( (dgr(d,gr),gedgr(g,d,gr)),pSolarGen   (p,sr)*pShareGroup(d,gr)) ;
  pWindGenNode (p, gad(wn,d) )  = sum( (dgr(d,gr)              ),pWindGen    (p,wn)*pShareGroup(d,gr)) ;

 // Representative periods information
        pNumPer_rp(rp)                       = SUM[p $[rpp(rp,p)],1]           ;
        pWeight_rp(rp) $[NOT pNumPer_rp(rp)] = 0 ;
        pWeight_rp(rp) $[    pNumPer_rp(rp)] = pWeight_rp(rp) / pNumPer_rp(rp) ;

 // Procedure to find the first period at each representative day
        pFirstP_rp(rp,p) $[NOT rpp(rp,p)] = 0      ;
        pFirstP_rp(rp,p) $[    rpp(rp,p)] = ORD(p) ;
        pFirstP_rp(rp,p)                  = + pFirstP_rp(rp,p  )
                                            - pFirstP_rp(rp,p-1)$[ORD(p) > 1]
                                            + 1                 $[ORD(p) = 1];
        pFirstP_rp(rp,p) $[NOT rpp(rp,p)] = 0 ;
        pFirstP_rp(rp,p) $[    rpp(rp,p) AND pFirstP_rp(rp,p) = 1] = 0 ;
        pFirstP_rp(rp,p) $[    rpp(rp,p) AND           ORD(p) = 1] = 1 ;

  //* Definition of parameter indicating the number of periods in the RP minus 1
        pNumPerMinus1_rp(rp) = pNumPer_rp(rp) - 1 ;

  //* Activating set of periods to calculate storage level for "slow" units in RP-TM model
        ps(p) $(mod(ORD(p)-1,pStorMovWindow)=0) = yes;
        ps(p) $ (ord(p)=1)=yes;
    //display ps;
        ps(p) $[ORD(p) = CARD(p)] = YES ; // also including the last period
        pt(p)$ [ps(p) or pa(p)]= yes;
  //*   gad(g,d) $[gcd(g,d) or ged(g,d)]  = yes;

  //* Parameter to indicate the last hour on ps(p) set
        IF(Mod[CARD(p),pStorMovWindow]=0,
        pLastStorMovW = pStorMovWindow - 1 ;
        ELSE
    pLastStorMovW = CARD(p) - Floor[CARD(p)/pStorMovWindow] * pStorMovWindow - 1 ;
        );



////////////////////////////     SOLVING OPTIONS      //////////////////////////////////



        pTypeCOmp = no ; //yes= Competition, no = cournot  model
        pLossMod  = no ; //yes= Loss model,  no = Lossless model
        pBilevel  = no;
        display slope; 
        pCost = no;
        if (pCompetitionType = 0,
        conjcvariation(p,g,d) = 0;
        else
        conjcvariation(p,g,d)$sum(gad(g,d),1) = sum(gad(g,d),((1/pDiv*Slope(p,d)))$(slope(p,d)))/(sum(gad(g,d),1))/(1+1$(pCompetitionType=2)) ;  //en un node pueden haber dos unidades, pero el conjvariation es el mismo
        )
        ;
        //conjcvariation = 1/slope;
        pHydroCost = 0.0001;
        pConsumCost =0.0001;//0.1;
        pFixedDemand (  p,d      )=0;

        vNewLine.prior  (y,lc)= 0.1;  // priority when solving MIP to find first binary lines
        //vTheta.fx  (y,p,"Node_1") = pzero ;
       // vTheta.fx  (y,p,"Node_1") = pzero ;


//////////////////////////// LOOP SET INITIZLIZATION  //////////////////////////////////

 $$ontext
 
  parameter

   pAct(q,di,df) 
   ;
  pAct(q,di,df) = no;
 $$offtext

 parameter

 pAct(q,di,df)
 /  
    q1	.	grp064	.	grp354	1
    q2	.	grp021	.	grp027	1
    q3	.	grp062	.	grp353	1
    q4	.	grp065	.	grp354	1
    q5	.	grp349	.	grp354	1
    q6	.	grp064	.	grp354	1
    q6	.	grp021	.	grp027	1
    q7	.	grp064	.	grp354	1
    q7	.	grp062	.	grp353	1
    q8	.	grp064	.	grp354	1
    q8	.	grp065	.	grp354	1
    q9	.	grp064	.	grp354	1
    q9	.	grp349	.	grp354	1
    q10	.	grp021	.	grp027	1
    q10	.	grp062	.	grp353	1
    q11	.	grp021	.	grp027	1
    q11	.	grp065	.	grp354	1
    q12	.	grp021	.	grp027	1
    q12	.	grp349	.	grp354	1
    q13	.	grp062	.	grp353	1
    q13	.	grp065	.	grp354	1
    q14	.	grp062	.	grp353	1
    q14	.	grp349	.	grp354	1
    q15	.	grp065	.	grp354	1
    q15	.	grp349	.	grp354	1
    q16	.	grp064	.	grp354	1
    q16	.	grp021	.	grp027	1
    q16	.	grp062	.	grp353	1
    q17	.	grp064	.	grp354	1
    q17	.	grp021	.	grp027	1
    q17	.	grp065	.	grp354	1
    q18	.	grp064	.	grp354	1
    q18	.	grp021	.	grp027	1
    q18	.	grp349	.	grp354	1
    q19	.	grp021	.	grp027	1
    q19	.	grp062	.	grp353	1
    q19	.	grp065	.	grp354	1
    q20	.	grp021	.	grp027	1
    q20	.	grp062	.	grp353	1
    q20	.	grp349	.	grp354	1
    q21	.	grp021	.	grp027	1
    q21	.	grp065	.	grp354	1
    q21	.	grp349	.	grp354	1
    q22	.	grp062	.	grp353	1
    q22	.	grp065	.	grp354	1
    q22	.	grp349	.	grp354	1
    q23	.	grp064	.	grp354	1
    q23	.	grp065	.	grp354	1
    q23	.	grp349	.	grp354	1
    q24	.	grp064	.	grp354	1
    q24	.	grp062	.	grp353	1
    q24	.	grp065	.	grp354	1
    q25	.	grp064	.	grp354	1
    q25	.	grp062	.	grp353	1
    q25	.	grp349	.	grp354	1
    q26	.	grp064	.	grp354	1
    q26	.	grp062	.	grp353	1
    q26	.	grp349	.	grp354	1
    q26	.	grp065	.	grp354	1
    q27	.	grp064	.	grp354	1
    q27	.	grp021	.	grp027	1
    q27	.	grp062	.	grp353	1
    q27	.	grp349	.	grp354	1
    q28	.	grp064	.	grp354	1
    q28	.	grp021	.	grp027	1
    q28	.	grp065	.	grp354	1
    q28	.	grp349	.	grp354	1
    q29	.	grp021	.	grp027	1
    q29	.	grp062	.	grp353	1
    q29	.	grp065	.	grp354	1
    q29	.	grp349	.	grp354	1
    q30	.	grp064	.	grp354	1
    q30	.	grp062	.	grp353	1
    q30	.	grp065	.	grp354	1
    q30	.	grp349	.	grp354	1
    q31	.	grp064	.	grp354	1
    q31	.	grp021	.	grp027	1
    q31	.	grp062	.	grp353	1
    q31	.	grp065	.	grp354	1
    q31	.	grp349	.	grp354	1


 /
 ;
////////////////////////////      MODEL SOLVING       //////////////////////////////////
 //*************************** Solving Initialization **********************************
  // Initilization data for not erasing when overwriting ranges in excel
    put TMP putclose
    'text="Years" rng=GenInv!a1' /   'text="Years" rng==Flow!r1' / 'text="Years" rng==Flow!a1' /
    'text="Years" rng=Angle!a1'   / 'text="Years" rng==Output!a1' / 'text="Years" rng=LineInv!a1' /
    'text="Years" rng=WtrReserve!a1' / 'text="Years" rng==Cost!a1'
    execute_unload   '%gams.user1%.gdx'
    execute          'gdxxrw "%gams.user1%".gdx SQ=n EpsOut=0 O="%gams.user1%".xlsx @"%gams.user1%".txt'
    execute          'del    "%gams.user1%".gdx '   ;


  // Market model Option Executions
   pTypeCOmp = no ; //
   pBilevel  = no ; // yes= Bilevel,     no = One-level

  // Load Big M parameters
   //EXECUTE_LOAD  'LOW_LEVEL_MCP.gdx' ,
   //M_alpha_up1 ,
   //M_cRo_up1   ,
   //M_cRo_up2   ,
   //M_cRo_lo1   ,
   //M_cRo_lo2   ,
   //M_phi_lo1   ,
   //M_phi_lo2   ,
   //M_phi_up1   ,
   //M_phi_up2   ,
   //M_Kappa_lo1 ,
   //M_Kappa_lo2 ,
   //M_Kappa_up1 ,
   //M_Kappa_up2 ,
   //M_Delta_lo1 ,
   //M_Delta_lo2 ,
   //M_Delta_up1 ,
   //M_Delta_up2 ,
   ////M_Mu_lo1    ,
   ////M_Mu_lo2    ,
   ////M_Mu_up1    ,
   ////M_Mu_up2    ,
   //M_Omega_up1 ,
   //M_Omega_up2 ,
   //M_Omega_lo1 ,
   //M_Omega_lo2 ,
   ////M_KappN_lo1 ,
   ////M_KappN_lo2 ,
   ////M_KappN_up1 ,
   ////M_KappN_up2 ,
   ////M_DeltN_lo1 ,
   ////M_DeltN_lo2 ,
   ////M_DeltN_up1 ,
   ////M_DeltN_up2 ,
   ////M_Mun_lo1   ,
   ////M_Mun_lo2   ,
   ////M_Mun_up1   ,
   ////M_Mun_up2   ,
   //Y_alpha_up.l  ,
   //Y_cRo_up.l ,
   //Y_cRo_up.l ,
   //Y_cRo_lo.l ,
   //Y_cRo_lo.l ,
   //Y_Kappa_lo.l,
   //Y_Kappa_lo.l,
   //Y_Kappa_up.l,
   //Y_Kappa_up.l,
   //Y_Delta_lo.l,
   //Y_Delta_lo.l,
   //Y_Delta_up.l,
   //Y_Delta_up.l,
   //Y_Mu_lo.l  ,
   //Y_Mu_lo.l  ,
   //Y_Mu_up.l  ,
   //Y_Mu_up.l  ,
   //Y_phi_lo.l ,
   //Y_phi_lo.l ,
   //Y_phi_up.l ,
   //Y_phi_up.l ,
   //Y_Omega_lo.l,
   //Y_Omega_up.l
   ////Y_KappN_lo.l,
   ////Y_KappN_up.l,
   ////Y_DeltN_lo.l,
   ////Y_Delta_up.l,
   ////Y_MuN_lo.l  ,
   ////Y_MuN_up.l  ,
   ////M_Mu_up2    ;
   ;
  // Options to Run Regularization method
    $$onecho > nlpec.opt
    reftype mult
    aggregate full
    constraint inequality
    initmu 1e9
    numsolves 200
    updatefac 0.9
    finalmu 1e-6
    $$offecho
  // initialize EMP model out of the loop
  ;
  M_cLambda     =10*100;// 1000 ;
  pBilevel  = no ;

 //solve CUADRATIC_MARKET minimizing vTotalTCost using miqcp       ;
 if (   pMercado =1 ,
 $$ontext
 //////////////////////////// Solving Market as a MCP //////////////////////////////////////
  //solve CUADRATIC_MARKET minimizing vTotalTCost using miqcp;
  pBilevel  = no ;
  //if(sum(lc(di,df),1)>1,
   //solve BILEVEL_KKT_REGULAR_MPEC1 using mpec minimizing vtotalfcost ;
  //else
  solve LOW_LEVEL_MCP using mcp ;
   //);
   pTotalCost1=0;
   pExtendedWelfare =0;
   pTotalCost1=

         +sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp) * pSlopeVarCost(t)* vProduct.l      (y,p,t,d)]
         +sum[(y,              gcd(g,d)), pGenInvCost(g) *                   vNewGen.L (y,  g,d)]
         +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pHydroCost      * vProduct.l      (y,p,h,d)]
         +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pConsumCost     * vConsump.l      (y,p,h,d)]
         ;


    pDemandl(pa(p),d) = sum(y,pDemandNode   (y,p,d)  -  Slope*b_cLambda.l(y,p,d) )     ;
    pExtendedWelfare =
             +                   sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp)* pSlopeVarCost(t)*vProduct.l(y,p,t,d)]
             +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pHydroCost      *vProduct.l(y,p,h,d)]
             +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pConsumCost     *vConsump.l(y,p,h,d)]
             +                   sum[(y,rpp(rp,pa(p)),gad(g,d)), pWeight_rp(rp)*(conjcvariation(p,g,d)/2)*power(vProduct.l(y,p,g,d)-(vConsump.l(y,p,g,d))$h(g) ,2)      ] //
             //+(-(1/Slope)       *sum((y,rpp(rp,pa(p)),       d), pWeight_rp(rp)* [ pDemand(p) * pDemShare(d) * pCumDemIncr(y) * pDemandl(p,d) -  power(pDemandl(p,d),2)/2])) $(Slope)
             +                   sum((y,rpp(rp,pa(p)),       d), -(1/Slope(d))$(slope(d))*pWeight_rp(rp)* [ pDemandNode   (y,p,d)   * vDemand.l(y,p,d) -  power(vDemand.l(y,p,d),2)/2]))
             +                   sum[(y,    gcd(g,d)),  pGenInvCost (g)*vNewGen(y, g,d)*pMaxProd(g,d)]
             +                   sum[(y,    gcd(wn,d)), pGenInvCost (wn)*vNewGen(y, wn,d)*pMaxWindGen (wn,d)];

   pBenefits  (cp )   =       + sum((y,rpp(rp,pa(p)),gad(t,d),gcp(t,cp)),pWeight_rp(RP)*(  vProduct.l(y,p,t,d)*(b_cLambda.l(y,p,  d)- pSlopeVarCost(t)   ) ))   //
                               + sum((y,rpp(rp,pa(p)),gad(h,d),gcp(h,cp)),pWeight_rp(RP)*( b_cLambda.l(y,p,  d)*( vProduct.l(y,p,h,d)- vConsump.l(y,p,h,d)) ))
                               - sum((y,              gad(g,d),gcp(g,cp)),   vNewGen.l   (y,   g,d)*pGenInvCost (g))
                               - sum[(y,rpp(rp,pa(p)),gad(h,d),gcp(h,cp)), pWeight_rp(rp)* pHydroCost*vProduct.l(y,p,h,d)]
                               - sum[(y,rpp(rp,pa(p)),gad(h,d),gcp(h,cp)), pWeight_rp(rp)* pConsumCost*vConsump.l(y,p,h,d)]
                               + sum((gad(g,d),gcp(g,cp)), eps);
                            ;

   pPrices         (y,pa(p),    d    )           = sum((rpp(rp,p),s), (b_cLambda.l(y,p,d))*1000)+eps      ;
   pProduct        (y,pa(p),   th    )           = sum((gad(g,d)), vProduct.l  (y,p,g,d)$tg(g,th))        ;
   pProduct_FX     (y,pa(p), gad(g,d))           = vProduct.l        (y,p,g,d  )                          ;
   pInstalCapT_can (y,       gcd(g,d))           = vNewGen.l   (y,    g,d)                          ;
   pFlow           (y,pa(p),       la)           = vFlow.l           (y,p, la  )                          ;
   pInstalLine     (y,             lc)           = vNewLine.l        (y,lc     )                          ;
   pReserve_FX     (y,pt(p),gad(hf,d))           = vReserve.l        (y,p,hf,d )  +eps                    ;
   pReserve_FX     (y,ps(p),gad(hs,d))           = vReserve.l        (y,p,hs,d )  +eps                    ;
   pConsump        (y,pa(p),gad(h ,d))            = vConsump.l       (y,p,h ,d )  +eps                    ;
   OF_Cost    ('Obj Func  Model      [1000 M€]') = pTotalCost1+EPS                                        ;
   OF_ExtWel  ('Ext Welf  Model      [1000 M€]') = pExtendedWelfare + EPS                                 ;
   GenCPUTime ('CPU Time  Model generation [s]') = BILEVEL_KKT_REGULAR_MPEC1.resGen                       ;
   SolCPUTime ('CPU Time  Model solution   [s]') = BILEVEL_KKT_REGULAR_MPEC1.resUsd                       ;
   NumVar     ('Number of variables           ') = BILEVEL_KKT_REGULAR_MPEC1.numVar                       ;
   NumDVar    ('Number of discrete variables  ') = BILEVEL_KKT_REGULAR_MPEC1.numDVar                      ;
   NumEqu     ('Number of equations           ') = BILEVEL_KKT_REGULAR_MPEC1.numEqu                       ;
   NumNZ      ('Number of nonzero elements    ') = BILEVEL_KKT_REGULAR_MPEC1.numNZ                        ;
   BestSol    ('Best possible solution for MIP') = BILEVEL_KKT_REGULAR_MPEC1.objest                       ;
   put TMP putclose
   'par=pInstalCapT_can rdim=1 rng=GenInv!e1:g3'      / 'par=pFlow       rdim=4 rng=Flow!c1:g1000'       / 'par=pProduct_FX rdim=2 rng=Output1!c1:l10000'/
   'par=pInstalLine     rdim=1 rng=LineInv!v1:w3'     / 'par=pReserve_FX rdim=2 rng=WtrReserve!f1:K1000' / 'par=OF_Cost     rdim=1 rng=Cost!c1'        /
   'par=GenCPUTime      rdim=1 rng=Cost!c2'           / 'par=SolCPUTime  rdim=1 rng=Cost!c3'             / 'text="mCP"             rng=Cost!C9:C9'     /
   'par=NumVar          rdim=1 rng=Cost!c4'           / 'par=NumDVar     rdim=1 rng=Cost!c5'             / 'par=NumEqu      rdim=1 rng=Cost!c6'        /
   'par=BestSol         rdim=1 rng=Cost!c7'           / 'par=pBenefits   rdim=1 rng=Prices!c2:c10'       / 'par=pPrices     rdim=3 rng=Prices!c15'     /
   'text="MCP"                 rng=Prices!C1:C1'      / 'text="MCP"             rng=WtrReserve!E1:E1'    / 'text="mCP"             rng=LineInv!W1:W1'  /
   'text="MCP"                 rng=Output!i1'         / 'par=OF_ExtWel   rdim=1 rng=Cost!C8'             / 'text="MCP"             rng=Cost!C9'        /
   'par=pConsump       rdim=2 rng=WtrReserve!r1:W1000' / 'text="MCP"             rng=WtrReserve!r1:r1'   / 'par=pDemandl    rdim=1 rng=Hoja1!a1'
   execute_unload   '%gams.user1%.gdx' pInstalCapT_can pFlow  pProduct_FX pInstalLine pReserve_FX pConsump pDemandl
                                           OF_Cost GenCPUTime SolCPUTime NumVar NumDVar NumEqu   BestSol pBenefits pPrices  OF_ExtWel
   execute          'gdxxrw "%gams.user1%".gdx SQ=n EpsOut=0 O="%gams.user1%".xlsx @"%gams.user1%".txt'
   execute          'del    "%gams.user1%".gdx '
   ;
    
 $$offtext
 elseif pMercado =2 ,


 //////////////////////////// Solving Market as a MIP //////////////////////////////////////
  pBilevel  = no ;
  // Step 1 : Initialization
    solve CUADRATIC_MARKET minimizing vTotalTCost using miqcp;
   // pReserve_FX     (y,pt(p),gad(hf,d))           = vReserve.l        (y,p,hf,d )  +eps                    ;

  // Step 2 : Regularization
   pBilevel  = yes ;
   solve BILEVEL_KKT_REGULAR_MPEC1 using MPEC minimizing  vTotalFCost;

  // Step 3 : Big M initialization
     M= 2;
     M_alpha_up1   =3000;//1000          ;//+    M*smax((y,pa(p),ged(g,d)),   b_Alpha_up.l          (y,p,    d) ) ;//+  M$(smax((y,pa(p),s,ged(g,d)),   b_cRo_up.l            (y,p,s,g,  d) ));
     M_cRo_up1     =30*100;//10*20*5          ;//+    M*smax((y,pa(p),ged(g,d)),   b_cRo_up.l            (y,p,g,  d) ) ;//+  M$(smax((y,pa(p),s,ged(g,d)),   b_cRo_up.l            (y,p,s,g,  d) ));
     M_cRo_up2     =30*500;//10*10*5          ;//+    M*smax((y,pa(p),ged(g,d)),   abs(eMaxProd.l  (y,p,g,  d))) ;//+  M$(smax((y,pa(p),s,ged(g,d)),   abs(eMaxProd.l  (y,p,s,g,  d))));
     M_cRo_lo1     =30*100;//20*10*5          ;//+    M*smax((y,pa(p),ged(g,d)),   b_cRo_lo.l            (y,p,g,  d) ) ;//+  M$(smax((y,pa(p),s,ged(g,d)),   b_cRo_lo.l            (y,p,s,g,  d) ));
     M_cRo_lo2     =30*100;//050*10*5          ;//+    M*smax((y,pa(p),ged(g,d)),   abs(eMinProd.l  (y,p,g,  d))) ;//+  M$(smax((y,pa(p),s,ged(g,d)),   abs(eMinProd.l  (y,p,s,g,  d))));
     M_phi_lo1     =30*100;//10*10*5          ;//+    M*smax((y,pa(p),le(di,df)),  b_phi_lo.l            (y,p,di,df) ) ;//+  M$(smax((y,pa(p),s,le(di,df)),  b_phi_lo.l            (y,p,s,di,df) ));
     M_phi_lo2     =30*100;//10*10*5          ;//+    M*smax((y,pa(p),le(di,df)),  abs(eFlowExisting1.l  (y,p,di,df))) ;//+  M$(smax((y,pa(p),s,le(di,df)),  abs(eFlowExisting1.l  (y,p,s,di,df))));
     M_phi_up1     =30*100;//10*10*5          ;//+    M*smax((y,pa(p),le(di,df)),  b_phi_up.l            (y,p,di,df) ) ;//+  M$(smax((y,pa(p),s,le(di,df)),  b_phi_up.l            (y,p,s,di,df) ));
     M_phi_up2     =30*100;//10*10*5          ;//+    M*smax((y,pa(p),le(di,df)),  abs(eFlowExisting2.l  (y,p,di,df))) ;//+  M$(smax((y,pa(p),s,le(di,df)),  abs(eFlowExisting2.l  (y,p,s,di,df))));
     M_zeta_up2    =30*100;//10*10*5          ;//        ;// +    M*smax((y,pa(p),lc(di,df)),  abs(eFlowInstlCap2.l  (y,p,di,df))) ;//+  M$(smax((y,pa(p),s,lc(di,df)),  abs(eFlowInstlCap2.l  (y,p,s,di,df))));
     M_zeta_up1    =30*100;//10*10*5          ;//        ;// (100*10) ;//+   10 +M*smax((y,pa(p),lc(di,df)),  b_zeta_up.l           (y,p,di,df) ) ;//+  M$(smax((y,pa(p),s,lc(di,df)),  b_zeta_up.l           (y,p,s,di,df) ));
     M_zeta_lo1    =30*100;//10*10*5          ;//        ;// (100*10) ;//+   10 +M*smax((y,pa(p),lc(di,df)),  b_zeta_lo.l           (y,p,di,df) ) ;//+  M$(smax((y,pa(p),s,lc(di,df)),  b_zeta_lo.l           (y,p,s,di,df) ));
     M_zeta_lo2    =30*100;//10*10*5          ;//        ;// (100*20) ;//+   10 +M*smax((y,pa(p),lc(di,df)),  abs(eFlowInstlCap1.l  (y,p,di,df))) ;//+  M$(smax((y,pa(p),s,lc(di,df)),  abs(eFlowInstlCap1.l  (y,p,s,di,df))));
     M_dualpf1     =30*100;//1000            ;//
     M_dualpf2     =30*100;//1000             ;//
     M_tau_up1     =30*100;//1000            ;//        ;// (100*10) ;//+   10 +M*smax((y,pa(p),lc(di,df)),  b_tau_up.l            (y,p,di,df) ) ;//+  M$(smax((y,pa(p),s,lc(di,df)),  b_tau_up.l            (y,p,s,di,df) ));
     M_tau_up2     =30*100;//1000            ;//        ;// (100*10) ;//+   10 +M*smax((y,pa(p),lc(di,df)),  abs(eFlowNetN1.l      (y,p,di,df))) ;//+  M$(smax((y,pa(p),s,lc(di,df)),  abs(eFlowNetN1.l      (y,p,s,di,df))));
     M_tau_lo1     =30*100;//1000            ;//        ;// (100*10) ;//+   10 +M*smax((y,pa(p),lc(di,df)),  b_tau_lo.l            (y,p,di,df) ) ;//+  M$(smax((y,pa(p),s,lc(di,df)),  b_tau_lo.l            (y,p,s,di,df) ));
     M_tau_lo2     =30*100;//1000            ;//        ;// (100*10) ;//+   10 +M*smax((y,pa(p),lc(di,df)),  abs(eFlowNetN2.l      (y,p,di,df))) ;//+  M$(smax((y,pa(p),s,lc(di,df)),  abs(eFlowNetN2.l      (y,p,s,di,df))));
     M_Kappa_lo1   =30*100;//20*10*5          ;//  +   M*smax((y,pa(p),ged(h,d) ),  b_Kappa_lo.l          (y,p,h,  d) ) ;//+  M$(smax((y,pa(p),s,ged(h,d) ),  b_Kappa_lo.l          (y,p,s,h,  d) ));
     M_Kappa_lo2   =30*100;//10*10*5          ;//  +   M*smax((y,pa(p),ged(h,d) ),  abs(eMinCons.l  (y,p,h,  d))) ;//+  M$(smax((y,pa(p),s,ged(h,d) ),  abs(eMinCons.l  (y,p,s,h,  d))));
     M_Kappa_up1   =30*100;//10*10*5          ;//  +   M*smax((y,pa(p),ged(h,d) ),  b_Kappa_up.l          (y,p,h,  d) ) ;//+  M$(smax((y,pa(p),s,ged(h,d) ),  b_Kappa_up.l          (y,p,s,h,  d) ));
     M_Kappa_up2   =30*100;//10*10*5          ;//  +   M*smax((y,pa(p),ged(h,d) ),  abs(eMaxCons.l  (y,p,h,  d))) ;//+  M$(smax((y,pa(p),s,ged(h,d) ),  abs(eMaxCons.l  (y,p,s,h,  d))));
     M_Delta_lo1   =30*100;//10*20*5          ;//  +   M*smax((y,pa(p),ged(h,d) ),  b_Delta_lo.l          (y,p,h,  d) ) ;//+  M$(smax((y,ps(p),  ged(h,d) ),  b_Delta_lo.l          (y,p,  h,  d) ));
     M_Delta_lo2   =30*100;//10*10*5          ;//  +   M*smax((y,pa(p),ged(h,d) ),  abs(eMinReserve.l     (y,p,h,  d))) ;//+  M$(smax((y,ps(p),  ged(h,d) ),  abs(eMinReserve.l     (y,p,  h,  d))));
     M_Delta_up1   =30*100;//10*10*5          ;//  +   M*smax((y,pa(p),ged(h,d) ),  b_Delta_up.l          (y,p,h,  d) ) ;//+  M$(smax((y,ps(p),  ged(h,d) ),  b_Delta_up.l          (y,p,  h,  d) ));
     M_Delta_up2   =30*100;//10*10*5          ;//  +   M*smax((y,pa(p),ged(h,d) ),  abs(eMaxReserve.l     (y,p,h,  d))) ;//+  M$(smax((y,ps(p)  ,ged(h,d) ),  abs(eMaxReserve.l     (y,p,  h,  d))));
     //M_Mu_lo1    =*100;//*0*100         ;//      ;// (100   ) ;//+   +0M*smax((y,pa(p),ged(h,d) ),  b_Mu_lo.l             (y,p,h,  d) ) ;//+  M$(smax((y,pa(p)  ,ged(h,d) ),  b_Mu_lo.l             (y,p,  h,  d) ));
     //M_Mu_lo2    =*100;//*0*100         ;//      ;// (100   ) ;//+  /  +0M*smax((y,pa(p),ged(h,d) ), abs(eMinSpillage.l     (y,p,h,  d))) ;//
     //M_Mu_up1    =*100;//*0*100         ;//      ;// (100   ) ;//+  /  +0M*smax((y,pa(p),ged(h,d) ),  b_Mu_up.l             (y,p,h,  d) ) ;//+  M$(smax((y,pa(p)  ,ged(h,d) ),  b_Mu_up.l             (y,p,  h,  d) ));
     //M_Mu_up2    =*100;//*0*100         ;//      ;// (100   ) ;//+  /  +0M*smax((y,pa(p),ged(h,d) ), abs(eMaxSpillage.l     (y,p,h,  d))) ;//
     M_Omega_up1   =30*100;//0100*10          ;//  +   M*smax((y,pa(p),gcd(g,d) ),  b_Omega_up.l          (y,p,g,d  ) ) ;//+  M$(smax((y,pa(p),s,gcd(g,d) ),  b_Omega_up.l          (y,p,s,g,  d) ));
     M_Omega_up2   =30*100;//100*10          ;//  +   M*smax((y,pa(p),gcd(g,d) ),  abs(eMaxProd_New.l  (y,p,g,d  ))) ;//+  M$(smax((y,pa(p),s,gcd(g,d) ),  abs(eMaxProd_New.l  (y,p,s,g,  d))));
     M_Omega_lo1   =30*100;//0100*10          ;//  +   M*smax((y,pa(p),gcd(g,d) ),  b_Omega_lo.l          (y,p,g,d  ) ) ;//+  M$(smax((y,pa(p),s,gcd(g,d) ),  b_Omega_lo.l          (y,p,s,g,  d) ));
     M_Omega_lo2   =30*100;//0100*10          ;//  +   M*smax((y,pa(p),gcd(g,d) ),  abs(eMinProd_New.l  (y,p,g,d  ))) ;//+  M$(smax((y,pa(p),s,gcd(g,d) ),  abs(eMinProd_New.l  (y,p,s,g,  d))));
     M_KappN_lo1   =30*100;// 100*10            ;//      ;// (100   ) ;//+   10 +M*smax((y,pa(p),gcd(h,d) ),  b_KappN_lo.l          (y,p,h,  d) ) ;
     M_KappN_lo2   =30*100;// 100*10            ;//      ;// (100   ) ;//+   10 +M*smax((y,pa(p),gcd(h,d) ),  abs(eMinCons_New.l  (y,p,h,  d))) ;
     M_KappN_up1   =30*100;// 100*10            ;//      ;// (100   ) ;//+   10 +M*smax((y,pa(p),gcd(h,d) ),  b_KappN_up.l          (y,p,h,  d) ) ;
     M_KappN_up2   =30*100;// 100*10            ;//      ;// (100   ) ;//+   10 +M*smax((y,pa(p),gcd(h,d) ),  abs(eMaxCons_New.l  (y,p,h,  d))) ;
     M_DeltN_lo1   =30*100;// 500*10            ;//      ;// (100   ) ;//+   10 +M*smax((y,pt(p),gcd(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))),  b_DeltN_lo.l          (y,p,h,  d) ) ;
     M_DeltN_lo2   =30*100;// 500*10            ;//      ;// (100   ) ;//+   10 +M*smax((y,pt(p),gcd(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))),  abs(eMaxReserve_New.l (y,p,h,  d))) ;
     M_DeltN_up1   =30*100;// 500*10            ;//      ;// (100   ) ;//+   10 +M*smax((y,pt(p),gcd(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))),  b_DeltN_up.l          (y,p,h,  d) ) ;
     M_DeltN_up2   =30*100;// 500*10            ;//      ;// 200    +   10 +M*smax((y,pt(p),gcd(h,d) )$((pa(p) and hf(h)) or (ps(p)and hs(h))),  abs(eMinReserve_New.l (y,p,h,  d))) ;
     M_cLambda     =30*100;// 1000 ;

  // Step 4 : Binary Starting point
    //Y_cRo_up.l   (y,pa(p),ged(g,d) ) $ (b_cRo_up.l         (y,p,g,d  )> 0)= 1 ;
    //Y_cRo_up.l   (y,pa(p),ged(g,d) ) $ (eMaxProd.l   (y,p,g,d  )> 0)= 0 ;
    //Y_cRo_lo.l   (y,pa(p),ged(g,d) ) $ (b_cRo_lo.l         (y,p,g,d  )> 0)= 1 ;
    //Y_cRo_lo.l   (y,pa(p),ged(g,d) ) $ (eMinProd.l   (y,p,g,d  )> 0)= 0 ;
    //Y_Omega_up.l (y,pa(p),gcd(g,d) ) $ (b_Omega_up.l       (y,p,g,d  )> 0)= 1 ;
    //Y_Omega_up.l (y,pa(p),gcd(g,d) ) $ (eMaxProd_New.l   (y,p,g,d  )> 0)= 0 ;
    //Y_Omega_lo.l (y,pa(p),gcd(g,d) ) $ (b_Omega_lo.l       (y,p,g,d  )> 0)= 1 ;
    //Y_Omega_lo.l (y,pa(p),gcd(g,d) ) $ (eMinProd_New.l   (y,p,g,d  )> 0)= 0 ;
    //Y_Kappa_lo.l (y,pa(p),ged(h,d) ) $ (b_Kappa_lo.l       (y,p,h,d  )> 0)= 1 ;
    //Y_Kappa_lo.l (y,pa(p),ged(h,d) ) $ (eMinCons.l   (y,p,h,d  )> 0)= 0 ;
    //Y_Kappa_up.l (y,pa(p),ged(h,d) ) $ (b_Kappa_up.l       (y,p,h,d  )> 0)= 1 ;
    //Y_Kappa_up.l (y,pa(p),ged(h,d) ) $ (eMaxCons.l   (y,p,h,d  )> 0)= 0 ;
    //Y_Delta_lo.l (y,pa(p),ged(h,d) ) $ (b_Delta_lo.l       (y,p,h,d  )> 0)= 1 ;
    //Y_Delta_lo.l (y,pa(p),ged(h,d) ) $ (eMinReserve.l      (y,p,h,d  )> 0)= 0 ;
    //Y_Delta_up.l (y,pa(p),ged(h,d) ) $ (b_Delta_up.l       (y,p,h,d  )> 0)= 1 ;
    //Y_Delta_up.l (y,pa(p),ged(h,d) ) $ (eMinReserve.l      (y,p,h,d  )> 0)= 0 ;
    ////Y_Mu_lo.l    (y,pa(p),ged(h,d) ) $ (b_Mu_lo.l          (y,p,h,d  )> 0)= 1 ;
    ////Y_Mu_lo.l    (y,pa(p),ged(h,d) ) $ (eMinSpillage.l     (y,p,h,d  )> 0)= 0 ;
    ////Y_Mu_up.l    (y,pa(p),ged(h,d) ) $ (b_Mu_up.l          (y,p,h,d  )> 0)= 1 ;
    ////Y_Mu_up.l    (y,pa(p),ged(h,d) ) $ (eMaxSpillage.l     (y,p,h,d  )> 0)= 0 ;
    //Y_phi_lo.l   (y,pa(p),le(di,df)) $ (b_phi_lo.l         (y,p,di,df)> 0)= 1 ;
    //Y_phi_lo.l   (y,pa(p),le(di,df)) $ (eFlowExisting1.l   (y,p,di,df)> 0)= 0 ;
    //Y_phi_up.l   (y,pa(p),le(di,df)) $ (b_phi_up.l         (y,p,di,df)> 0)= 1 ;
    //Y_phi_up.l   (y,pa(p),le(di,df)) $ (eFlowExisting2.l   (y,p,di,df)> 0)= 0 ;
    //Y_KappN_lo.l (y,pa(p),gcd(h,d) ) $ (b_KappN_lo.l       (y,p,h,d  )> 0)= 1 ;
    //Y_KappN_lo.l (y,pa(p),gcd(h,d) ) $ (eMinCons_New.l   (y,p,h,d  )> 0)= 0 ;
    //Y_KappN_up.l (y,pa(p),gcd(h,d) ) $ (b_KappN_up.l       (y,p,h,d  )> 0)= 1 ;
    //Y_KappN_up.l (y,pa(p),gcd(h,d) ) $ (eMaxCons_New.l   (y,p,h,d  )> 0)= 0 ;
    //Y_DeltN_lo.l (y,pa(p),gcd(h,d) ) $ (b_DeltN_lo.l       (y,p,h,d  )> 0)= 1 ;
    //Y_DeltN_lo.l (y,pa(p),gcd(h,d) ) $ (eMinReserve_New.l  (y,p,h,d  )> 0)= 0 ;
    //Y_Delta_up.l (y,pa(p),gcd(h,d) ) $ (b_DeltN_up.l       (y,p,h,d  )> 0)= 1 ;
    //Y_Delta_up.l (y,pa(p),gcd(h,d) ) $ (eMinReserve_New.l  (y,p,h,d  )> 0)= 0 ;
    //Y_MuN_lo.l   (y,pa(p),gcd(h,d) ) $ (b_MuN_lo.l         (y,p,h,d  )> 0)= 1 ;
    //Y_MuN_lo.l   (y,pa(p),gcd(h,d) ) $ (eMinSpillage_New.l (y,p,h,d  )> 0)= 0 ;
    //Y_MuN_up.l   (y,pa(p),gcd(h,d) ) $ (b_Mu_up.l          (y,p,h,d  )> 0)= 1 ;
    //Y_MuN_up.l   (y,pa(p),gcd(h,d) ) $ (eMaxSpillage_New.l (y,p,h,d  )> 0)= 0 ;
    //Y_zeta_lo.l  (y,pa(p),lc(di,df)) $ (b_zeta_lo.l        (y,p,di,df)> 0)= 1 ;
    //Y_zeta_lo.l  (y,pa(p),lc(di,df)) $ (eFlowInstlCap1_b.l   (y,p,di,df)> 0)= 0 ;
    //Y_zeta_up.l  (y,pa(p),lc(di,df)) $ (b_zeta_up.l        (y,p,di,df)> 0)= 1 ;
    //Y_zeta_up.l  (y,pa(p),lc(di,df)) $ (eFlowInstlCap2_b.l   (y,p,di,df)> 0)= 0 ;
    //Y_tau_up.l   (y,pa(p),lc(di,df)) $ (b_tau_up.l         (y,p,di,df)> 0)= 1 ;
    //Y_tau_up.l   (y,pa(p),lc(di,df)) $ (eFlowNetN1_b.l       (y,p,di,df)> 0)= 0 ;
    //Y_tau_lo.l   (y,pa(p),lc(di,df)) $ (b_tau_lo.l         (y,p,di,df)> 0)= 1 ;
    //Y_tau_lo.l   (y,pa(p),lc(di,df)) $ (eFlowNetN2_b    .l       (y,p,di,df)> 0)= 0 ;

  // Step 5 : MIP Solving
   solve BILEVEL_KKT_MILP minimizing vTotalFCost using MIQCP       ;
   pTotalCost1=0;
   pExtendedWelfare=0;

   pTotalCost1=
         +sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp) * pSlopeVarCost(t)* vProduct.l      (y,p,t,d)]
         +sum[(y,              gcd(g,d)), pGenInvCost(g) *                   vNewGen.L (y,  g,d)]
         +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pHydroCost      * vProduct.l      (y,p,h,d)]
         +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pConsumCost     * vConsump.l      (y,p,h,d)]
         ;
   pDemandl(pa(p),d) = sum(y,pDemandNode(y,p,d) -  Slope(p,d)*b_cLambda.l(y,p,d) )     ;
   //pDemandl(p,d) =sum(y, vDemand.l(y,p,d));
   pExtendedWelfare =
             +                   sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp)* pSlopeVarCost(t)*vProduct.l(y,p,t,d)]
             +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pHydroCost      *vProduct.l(y,p,h,d)]
             +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pConsumCost     *vConsump.l(y,p,h,d)]
             +                   sum[(y,rpp(rp,pa(p)),gad(g,d)), pWeight_rp(rp)*(conjcvariation(p,g,d)/2)*power(vProduct.l(y,p,g,d)-(vConsump.l(y,p,g,d))$h(g) ,2)      ] //
             //+(-(1/Slope)       *sum((y,rpp(rp,pa(p)),       d), pWeight_rp(rp)* [ pDemand(p) * pDemShare(d) * pCumDemIncr(y) * pDemandl(p,d) -  power(pDemandl(p,d),2)/2])) $(Slope)
             +                   sum((y,rpp(rp,pa(p)),       d), -(1/Slope(p,d))$(slope(p,d))*pWeight_rp(rp)* [ pDemandNode   (y,p,d)   * vDemand.l(y,p,d) -  power(vDemand.l(y,p,d),2)/2])
             +                   sum[(y,gcd(g,d)), pGenInvCost (g)*vNewGen.l(y, g,d)]
             +                   sum[(y,lc),      (card(y)-ord(y)+1)*[vNewLine.l(y,lc) - vNewLine.l(y-1,lc)]]
             ;
                            ;
    pPrices        (y,pa(p), d)                   =    sum((rpp(rp,p)), b_cLambda.l (y,p,d)*1000)+eps                                ;
    pProduct       (y,pa(p),  th )                =    sum((gad(g,d)), vProduct.l  (y,p,g,d)$tg(g,th))                             ;
    pProduct_FX    (y,pa(p),gad(g,d))             =    vProduct.l  (y,p,g,d)                                                         ;
    pInstalCapT_can(y,    g,d    )                =    vNewGen.l   (y,    g,d)                                                 ;
    pFlow          (y,pa(p), la)                  =  [ vFlow.l     (y,p, la)]                                                        ;
    pInstalLine    (y,         lc)                =    vNewLine.l  (y,lc)                                                            ;
    pReserve_FX    (y,pt(p),gad(hs,d) )           =    vReserve.l  (y,p,hs,d    ) +eps                                               ;
    pReserve_FX    (y,pt(p),gad(hf,d) )           =    vReserve.l  (y,p,hf,d    ) +eps                                               ;
    pConsump       (y,pa(p),gad(hf,d) )           =    vConsump.l  (y,p,hf,d    )  +eps                                          ;
    OF_Cost    ('Tot Cost  Model      [1000 M€]') = pTotalCost1 + EPS                                                                ;
    OF_ExtWel  ('Ext Welf  Model      [1000 M€]') = pExtendedWelfare + EPS                                                           ;
    GenCPUTime ('CPU Time  Model generation [s]') = BILEVEL_KKT_MILP.resGen                                                          ;
    SolCPUTime ('CPU Time  Model solution   [s]') = BILEVEL_KKT_MILP.resUsd                                                          ;
    NumVar     ('Number of variables           ') = BILEVEL_KKT_MILP.numVar                                                          ;
    NumDVar    ('Number of discrete variables  ') = BILEVEL_KKT_MILP.numDVar                                                         ;
    NumEqu     ('Number of equations           ') = BILEVEL_KKT_MILP.numEqu                                                          ;
    NumNZ      ('Number of nonzero elements    ') = BILEVEL_KKT_MILP.numNZ                                                           ;
    BestSol    ('Best possible solution for MIP') = BILEVEL_KKT_MILP.objest                                                          ;
    OF_current ('Current solution for MIP'      ) = BILEVEL_KKT_MILP.objval                                                          ;

    put TMP putclose
    'par=pInstalCapT_can rdim=1 rng=GenInv!M1'          / 'par=pFlow       rdim=4 rng=Flow!U1'               / 'par=pTheta      rdim=3 rng=Angle!M1'               /
    'par=pProduct_FX     rdim=2 rng=Output2!r1:aF1000'   / 'par=pInstalLine rdim=1 rng=LineInv!p1'            / 'par=pReserve_FX rdim=2 rng=WtrReserve!l1:p1000'    /
    'par=OF_Cost         rdim=1 rng=Cost!M1'            / 'par=GenCPUTime  rdim=1 rng=Cost!M2'               / 'par=SolCPUTime  rdim=1 rng=Cost!m3'                /
    'par=NumVar          rdim=1 rng=Cost!m4'            / 'par=NumDVar     rdim=1 rng=Cost!M5'               / 'par=NumEqu      rdim=1 rng=Cost!m6'                /
    'par=BestSol         rdim=1 rng=Cost!m7'            / 'par=pBenefits   rdim=1 rng=Prices!M2:n10'         / 'par=pPrices     rdim=3 rng=Prices!M15:Q1000'       /
    'par=OF_ExtWel       rdim=1 rng=Cost!m8'            / 'text="BILEVEL"             rng=Prices!M1'         / 'text="BILEVEL"  rng=WtrReserve!M1:M1'              /
    'par=OF_current      rdim=1 rng=Cost!m9'            /
    'text="BILEVEL"             rng=LineInv!q1:q1'      / 'text="BILEVEL"             rng=Cost!M10:M10'        / 'text="BILEVEL"  rng=Output!t1:t1'                  /
    'par=pConsump        rdim=2 rng=WtrReserve!u1:X1000'/ 'text="BILEVEL"             rng=WtrReserve!u1:u1'  / 'par=pDemandl    rdim=1 rng=Hoja1!a1'
    execute_unload   '%gams.user1%.gdx' pInstalCapT_can pFlow pTheta pProduct_FX pInstalLine pReserve_FX pConsump pDemandl OF_current
                                         OF_Cost GenCPUTime SolCPUTime NumVar NumDVar NumEqu NumNZ BestSol  pBenefits  pPrices OF_ExtWel
    execute          'gdxxrw "%gams.user1%".gdx SQ=n EpsOut=0 O="%gams.user1%".xlsx @"%gams.user1%".txt'
    execute          'del    "%gams.user1%".gdx '
    ;
    display pDemandl;
   //pObjFunct (f,'1') =  M                                                               ;
    //pObjFunct (f,'2') =  pTotalCost1 + EPS                                                                ;
    //pObjFunct (f,'3') =  pExtendedWelfare + EPS                      ;


   display pTotalCost1, pExtendedWelfare;
   pExtendedWelfare=0;

  // Step 6 : Expost Verification of Big M constraints
   loop((y,pa(p),gad(g,d),la(di,df)),

   if(     (Y_cRo_up.l   (y,p,g,d) < 1) and  (Y_cRo_up.l   (y,p,g,d)  >  0),         display "Y_cRo_up.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(     (Y_cRo_up.l   (y,p,g,d) < 1) and  (Y_cRo_up.l   (y,p,g,d)  >  0),         display "Y_cRo_up.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(     (Y_cRo_lo.l   (y,p,g,d) < 1) and  (Y_cRo_lo.l   (y,p,g,d)  >  0),         display "Y_cRo_lo.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(     (Y_cRo_lo.l   (y,p,g,d) < 1) and  (Y_cRo_lo.l   (y,p,g,d)  >  0),         display "Y_cRo_lo.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(     (Y_Omega_up.l (y,p,g,d) < 1) and  (Y_Omega_up.l (y,p,g,d)  >  0),         display "Y_Omega_up.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(     (Y_Omega_up.l (y,p,g,d) < 1) and  (Y_Omega_up.l (y,p,g,d)  >  0),         display "Y_Omega_up.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(     (Y_Omega_lo.l (y,p,g,d) < 1) and  (Y_Omega_lo.l (y,p,g,d)  >  0),         display "Y_Omega_lo.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(     (Y_Omega_lo.l (y,p,g,d) < 1) and  (Y_Omega_lo.l (y,p,g,d)  >  0),         display "Y_Omega_lo.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(    ((Y_Kappa_lo.l (y,p,g,d) < 1) and  (Y_Kappa_lo.l (y,p,g,d)  >  0))$h(g),   display "Y_Kappa_lo.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(    ((Y_Kappa_lo.l (y,p,g,d) < 1) and  (Y_Kappa_lo.l (y,p,g,d)  >  0))$h(g),   display "Y_Kappa_lo.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(    ((Y_Kappa_up.l (y,p,g,d) < 1) and  (Y_Kappa_up.l (y,p,g,d)  >  0))$h(g),   display "Y_Kappa_up.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(    ((Y_Kappa_up.l (y,p,g,d) < 1) and  (Y_Kappa_up.l (y,p,g,d)  >  0))$h(g),   display "Y_Kappa_up.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(    ((Y_Delta_lo.l (y,p,g,d) < 1) and  (Y_Delta_lo.l (y,p,g,d)  >  0))$((pa(p) and hf(g)) or (ps(p)and hs(g))),    display "Y_Delta_lo.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(    ((Y_Delta_lo.l (y,p,g,d) < 1) and  (Y_Delta_lo.l (y,p,g,d)  >  0))$((pa(p) and hf(g)) or (ps(p)and hs(g))),    display "Y_Delta_lo.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(    ((Y_Delta_up.l (y,p,g,d) < 1) and  (Y_Delta_up.l (y,p,g,d)  >  0))$((pa(p) and hf(g)) or (ps(p)and hs(g))),    display "Y_Delta_up.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(    ((Y_Delta_up.l (y,p,g,d) < 1) and  (Y_Delta_up.l (y,p,g,d)  >  0))$((pa(p) and hf(g)) or (ps(p)and hs(g))),    display "Y_Delta_up.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    (Y_Mu_lo.l    (y,p,g,d) < 1)$h(g) and  (Y_Mu_lo.l    (y,p,g,d)  >  0)$h(g),    display "Y_Mu_lo.l    NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    (Y_Mu_lo.l    (y,p,g,d) < 1)$h(g) and  (Y_Mu_lo.l    (y,p,g,d)  >  0)$h(g),    display "Y_Mu_lo.l    NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    (Y_Mu_up.l    (y,p,g,d) < 1)$h(g) and  (Y_Mu_up.l    (y,p,g,d)  >  0)$h(g),    display "Y_Mu_up.l    NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    (Y_Mu_up.l    (y,p,g,d) < 1)$h(g) and  (Y_Mu_up.l    (y,p,g,d)  >  0)$h(g),    display "Y_Mu_up.l    NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(   ((Y_phi_lo.l    (y,p,di,df) < 1) and  (Y_phi_lo.l   (y,p,di,df)>  0))$lc(di,df),  display "Y_phi_lo.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(   ((Y_phi_lo.l    (y,p,di,df) < 1) and  (Y_phi_lo.l   (y,p,di,df)>  0))$lc(di,df),  display "Y_phi_lo.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(   ((Y_phi_up.l    (y,p,di,df) < 1) and  (Y_phi_up.l   (y,p,di,df)>  0))$lc(di,df),  display "Y_phi_up.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(   ((Y_phi_up.l    (y,p,di,df) < 1) and  (Y_phi_up.l   (y,p,di,df)>  0))$lc(di,df),  display "Y_phi_up.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(   ((Y_KappN_lo.l  (y,p,g,d  ) < 1) and  (Y_KappN_lo.l (y,p,g,d  )>  0))$h(g)   ,  display "Y_KappN_lo.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(   ((Y_KappN_lo.l  (y,p,g,d  ) < 1) and  (Y_KappN_lo.l (y,p,g,d  )>  0))$h(g)   ,  display "Y_KappN_lo.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(   ((Y_KappN_up.l  (y,p,g,d  ) < 1) and  (Y_KappN_up.l (y,p,g,d  )>  0))$h(g)   ,  display "Y_KappN_up.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   if(   ((Y_KappN_up.l  (y,p,g,d  ) < 1) and  (Y_KappN_up.l (y,p,g,d  )>  0))$h(g)   ,  display "Y_KappN_up.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(   ((Y_DeltN_lo.l  (y,p,g,d  ) < 1) and  (Y_DeltN_lo.l (y,p,g,d  )>  0))$((pa(p) and hf(g)) or (ps(p)and hs(g)))  , display "Y_DeltN_lo.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(   ((Y_DeltN_lo.l  (y,p,g,d  ) < 1) and  (Y_DeltN_lo.l (y,p,g,d  )>  0))$((pa(p) and hf(g)) or (ps(p)and hs(g)))  , display "Y_DeltN_lo.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(   ((Y_Delta_up.l  (y,p,g,d  ) < 1) and  (Y_Delta_up.l (y,p,g,d  )>  0))$((pa(p) and hf(g)) or (ps(p)and hs(g)))  , display "Y_Delta_up.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(   ((Y_Delta_up.l  (y,p,g,d  ) < 1) and  (Y_Delta_up.l (y,p,g,d  )>  0))$((pa(p) and hf(g)) or (ps(p)and hs(g)))  , display "Y_Delta_up.l NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    (Y_MuN_lo.l   (y,p,g,d) < 1)$h(g) and  (Y_MuN_lo.l   (y,p,g,d )  >  0)$h(g), display "Y_MuN_lo.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    (Y_MuN_lo.l   (y,p,g,d) < 1)$h(g) and  (Y_MuN_lo.l   (y,p,g,d )  >  0)$h(g), display "Y_MuN_lo.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    (Y_MuN_up.l   (y,p,g,d) < 1)$h(g) and  (Y_MuN_up.l   (y,p,g,d )  >  0)$h(g), display "Y_MuN_up.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    (Y_MuN_up.l   (y,p,g,d) < 1)$h(g) and  (Y_MuN_up.l   (y,p,g,d )  >  0)$h(g), display "Y_MuN_up.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    ((Y_zeta_lo.l  (y,p,di,df) < 1) and  (Y_zeta_lo.l  (y,p,di,df)> 0))$lc(di,df), display "Y_zeta_lo.l  NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    ((Y_zeta_lo.l  (y,p,di,df) < 1) and  (Y_zeta_lo.l  (y,p,di,df)> 0))$lc(di,df), display "Y_zeta_lo.l  NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    ((Y_zeta_up.l  (y,p,di,df) < 1) and  (Y_zeta_up.l  (y,p,di,df)> 0))$lc(di,df), display "Y_zeta_up.l  NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    ((Y_zeta_up.l  (y,p,di,df) < 1) and  (Y_zeta_up.l  (y,p,di,df)> 0))$lc(di,df), display "Y_zeta_up.l  NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    ((Y_tau_up.l   (y,p,di,df) < 1) and  (Y_tau_up.l   (y,p,di,df)> 0))$lc(di,df), display "Y_tau_up.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    ((Y_tau_up.l   (y,p,di,df) < 1) and  (Y_tau_up.l   (y,p,di,df)> 0))$lc(di,df), display "Y_tau_up.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    ((Y_tau_lo.l   (y,p,di,df) < 1) and  (Y_tau_lo.l   (y,p,di,df)> 0))$lc(di,df), display "Y_tau_lo.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   //if(    ((Y_tau_lo.l   (y,p,di,df) < 1) and  (Y_tau_lo.l   (y,p,di,df)> 0))$lc(di,df), display "Y_tau_lo.l   NON BINARY VARIABLE, NUMERICAL ISSUES");
   if (b_Alpha_up.l (y,p,  d )                                                                       = M_alpha_up1  , display "Binding Constraint b_Alpha_up "  )  ;
   if (b_cRo_up.l   (y,p,g,d )                                                                       = M_cRo_up1  , display "Binding Constraint l_cRo_up1  "  )  ;
   if ((-vProduct.l (y,p,g,d )  + pMaxProd(g,d))                                                       = M_cRo_up2  , display "Binding Constraint l_cRo_up2  " )  ;
   if (b_cRo_lo.l   (y,p,g,d )                                                                       = M_cRo_lo1  , display "Binding Constraint l_cRo_lo1  " )  ;
   if (( vProduct.l (y,p,g,d )  - pMinProd(g))                                                       = M_cRo_lo2  , display "Binding Constraint l_cRo_lo2  " )  ;
   if (b_Omega_lo.l (y,p,g,d )                                                                       = M_Omega_lo1, display "Binding Constraint l_Omega_lo1" )  ;
   if (( vProduct.l (y,p,g,d )  - pzero      )                                                       = M_Omega_lo2, display "Binding Constraint l_Omega_lo2" )  ;
   if (b_Omega_up.l (y,p,g,d )                                                                       = M_Omega_up1, display "Binding Constraint l_Omega_up1" )  ;
   if ((-vProduct.l (y,p,g,d )+vNewGen.l(y,g,d)*pMaxProd(g,d))                                   = M_Omega_up2, display "Binding Constraint l_Omega_up2" )  ;
   if (b_Kappa_lo.l (y,p,g,d )$h(g)                                                                  = M_Kappa_lo1, display "Binding Constraint l_Kappa_lo1" )  ;
   if (( vConsump.l (y,p,g,d )$h(g) - pzero                          )                               = M_Kappa_lo2, display "Binding Constraint l_Kappa_lo2" )  ;
   if (b_Kappa_up.l (y,p,g,d )$h(g)                                                                  = M_Kappa_up1, display "Binding Constraint l_Kappa_up1" )  ;
   if ((-vConsump.l (y,p,g,d ) + pMaxCons(g)                    )      $h(g)                         = M_Kappa_up2, display "Binding Constraint l_Kappa_up2" )  ;
   if (b_Delta_lo.l (y,p,g,d )                 $((pa(p) and hf(g)) or (ps(p)and hs(g))and ged(g,d))  = M_Delta_lo1, display "Binding Constraint l_Delta_lo1" )  ;
   if (( vReserve.l (y,p,g,d)- pMinReserve(g)) $((pa(p) and hf(g)) or (ps(p)and hs(g))and ged(g,d))  = M_Delta_lo2, display "Binding Constraint l_Delta_lo2" )  ;
   if (b_Delta_up.l (y,p,g,d )                 $((pa(p) and hf(g)) or (ps(p)and hs(g))and ged(g,d))  = M_Delta_up1, display "Binding Constraint l_Delta_up1" )  ;
   if ( (vReserve.l (y,p,g,d )+ pMaxReserve(g))$((pt(p) and hf(g)) or (ps(p)and hs(g))and ged(g,d))  = M_Delta_up2, display "Binding Constraint l_Delta_up2" )  ;
   if (b_DeltN_lo.l (y,p,g,d )                $((pa(p) and hf(g)) or (ps(p)and hs(g))and gcd(g,d))   = M_DeltN_lo1, display "Binding Constraint l_Delta_lo1" )  ;
   if (( vReserve.l (y,p,g,d) - vNewGen.l (y, g,d)*pMinProd(g)*4)  $((pa(p) and hf(g)) or (ps(p)and hs(g))and gcd(g,d))  = M_Delta_lo2, display "Binding Constraint l_Delta_lo2" )  ;
   if (b_DeltN_up.l (y,p,g,d )                 $((pa(p) and hf(g)) or (ps(p)and hs(g))and gcd(g,d))  = M_DeltN_up1, display "Binding Constraint l_Delta_up1" )  ;
   if ( (-vReserve.l (y,p,g,d) + vNewGen.l (y, g,d)*pMaxProd(g,d)*4) $((pt(p) and hf(g)) or (ps(p)and hs(g))and gcd(g,d))  = M_DeltN_up2, display "Binding Constraint l_Delta_up2" )  ;
    //if (b_KappN_lo.l     (y,p,g,d)$(gcd(g,d) and h(g))                                                = M_KappN_lo1, display "Binding Constraint l_cRo_up1  " )   ;
   //if (( vConsump.L(y,p,G,d)    - pzero         )  $(gcd(g,d) and h(g))                              = M_KappN_lo2, display "Binding Constraintl_KappN_lo2 " )   ;
   if (b_KappN_lo.l      (y,p,g,d)$(gcd(g,d) and h(g))                                               = M_KappN_lo1, display "Binding Constraintl_KappN_up1 " )   ;
   if ( (vConsump.l (y,p,g,d  )  - pzero                           )  $(gcd(g,d) and h(g))           = M_KappN_lo2, display "Binding Constraintl_KappN_up2 " )   ;
   if (b_KappN_up.l      (y,p,g,d)$(gcd(g,d) and h(g))                                               = M_KappN_up1, display "Binding Constraintl_KappN_up1 " )   ;
   if ((-vConsump.l (y,p,g,d  )  +vNewGen.l(y,g,d)*pMaxProd(g,d)*(2-pEffic(g)))$(gcd(g,d) and h(g))   = M_KappN_up2, display "Binding Constraintl_KappN_up2 " )   ;
      //if (-vReserve.l(y,p,g,d)+ vNewGen.l(y,g,d)*pMaxReserve(g)$((pa(p) and hf(g)) or (ps(p)and hs(g)))= M_DeltN_up2, display "Binding Constraintl_DeltN_up2 " )   ;
   if (  b_phi_lo.l       (y,p,di,df)                                $le(di,df)                      = M_phi_lo1  , display "Binding Constraints l_phi_lo1 " )   ;
   if (( vFlow.l    (y,p,di,df)    + pTTC(di,df)                    )$le(di,df)                      = M_phi_lo2  , display "Binding Constraints l_phi_lo2 " )   ;
   if (  b_phi_up.l       (y,p,di,df)                                $le(di,df)                      = M_phi_up1  , display "Binding Constraints l_phi_up1 " )   ;
   if ((-vFlow.l    (y,p,di,df)    + pTTC(di,df)                    )$le(di,df)                      = M_phi_up2  , display "Binding Constraints l_phi_up2 " )   ;
   if (l_zeta_lo1.l      (y,p,di,df)$lc(di,df)                                                       = M_zeta_lo1 , display "Binding Constraint l_zeta_lo1 " )   ;
   if ((vFlow.l      (y,p,di,df) / pTTC(di,df)+ (vNewLine.l(y,di,df))) $lc(di,df)                    = M_zeta_lo2 , display "Binding Constraint l_zeta_lo2 " )   ;
   if (l_zeta_up1.l      (y,p,di,df)$lc(di,df)                                                       = M_zeta_up1 , display "Binding Constraint l_zeta_up1 " )   ;
   if ((-vFlow.l     (y,p,di,df) / pTTC(di,df)+ (vNewLine.l(y,di,df))) $lc(di,df)                    = M_zeta_up2 , display "Binding Constraint l_zeta_up2 " )   ;
   if (b_tau_lo.l   (y,p,di,df)                                          $lc(di,df)                  = M_tau_lo1  , display "Binding Constraint l_tau_lo1  " )   ;
   if ( ( vFlow.l(y,p,  di,df) / (1e+3/1e+3)*pTTC(di,df) -([vTheta.l(y,p,di)
   -vTheta.l(y,p,df)]*pSbase/pX(di,df) / (1e+3/1e+3)*pTTC(di,df)- 1 +vNewLine.l(y,di,df)))$lc(di,df) = M_tau_lo2  , display "Binding Constraint l_tau_lo2  " )   ;
   if ( b_tau_up.l       (y,p,di,df)$lc(di,df)                                                       = M_tau_up1  , display "Binding Constraint l_tau_up1  " )   ;
   if ((-vFlow.l(y,p,  di,df) / (1e+3/1e+3)*pTTC(di,df) +([vTheta.l(y,p,di)
   -vTheta.l(y,p,df)]*pSbase/pX(di,df) / (1e+3/1e+3)*pTTC(di,df)+ 1 -vNewLine.l(y,di,df)))$lc(di,df) = M_tau_up2  , display "Binding Constraint l_tau_up2  " )   ;

  );
  elseif pMercado =3 ,
 //++++++++++++++++++++++++++++++++ Enumaration Loop Start ++++++++++++++++++++++++++++++++++

  loop(q,
  lc(di,df)                           = no ;
  la(di,df)                           = no ;
  lc(di,df) $[pAct(q,di,df)=1]        = yes;
  la(di,df) $(lc(di,df) or le(di,df)) = yes;
  vNewLine_R.fx  (y,lc)  = 1;
  vNewLine.fx  (y,lc)  = 1;
 
 //////////////////////////// Solving Market as a QCP //////////////////////////////////////
   pBilevel  = no ;
   solve CUADRATIC_MARKET minimizing vTotalTCost using miqcp       ;
    pTotalCost1=0;
    pExtendedWelfare=0;
   pTotalCost1=

        +sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp) * pSlopeVarCost(t)* vProduct.l      (y,p,t,d)]
        +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pHydroCost      * vProduct.l      (y,p,h,d)]
        +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pConsumCost     * vConsump.l      (y,p,h,d)]
        ;

   pWindSpillage (y,pa(p),gad(wn,d)  )            =   pWindGenNode     (p,wn,d)*(vNewGen.l(y,wn,d)$(gcd(wn,d))) -  vWind.l(y,p,wn,d)                              ;

   pExtendedWelfare =
             +                   sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp)* pSlopeVarCost(t)*vProduct.l(y,p,t,d)]
             +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pHydroCost      *vProduct.l(y,p,h,d)]
             +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pConsumCost     *vConsump.l(y,p,h,d)]
             //+(-(1/Slope)     *sum((y,rpp(rp,pa(p)),       d), pWeight_rp(rp)* [ pDemandNode   (y,p,d) -  power(pDemandl(p,d),2)/2])) $(Slope)
             +                   sum((y,rpp(rp,pa(p)),       d), -(1/Slope(p,d))$(slope(p,d)) *pWeight_rp(rp)* [ pDemandNode   (y,p,d)   * vDemand.l(y,p,d) -  power(vDemand.l(y,p,d),2)/2])
             +                   sum[(y,    gcd(g,d)),  pGenInvCost (g )*vNewGen.l(y, g, d)*pMaxProd    (g,d) ]
             +                   sum[(y,    gcd(wn,d)), pGenInvCost (wn)*vNewGen.l(y, wn,d)*pMaxWindGen (wn,d)]
             +                   sum[(y,    gcd(sr,d)), pGenInvCost (sr)*vNewGen.l(y, sr,d)*pMaxSolarGen (sr,d)]
             +                   sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost(lc)*[vNewLine.l(y,lc) - vNewLine.l(y-1,lc)]]  ;
       ;

            //pPrices        (y,pa(p), d)                       =    sum((rpp(rp,p),s), ( +eBalance_C_can.m (y,p,d))*1000/pWeight_rp(rp))+eps                 ;
   pPrices        (y,pa(p), d)                       =  //  sum((rpp(rp,p),s), ( (pDemandNode   (y,p,d)-vDemand.l(y,p,d))/Slope(d) )*1000 )
                                                     +  sum((rpp(rp,p)), ( +eBalance_C_can.m (y,p,d))*1000/pWeight_rp(rp))     
                                                      ;
   pConsumer_Surplus                             = //sum((y,rpp(rp,pa(p)),s,gad(g,d)), pDemandNode   (y,p,d)*vProduct.l  (y,p,s,g,d)*(1/Slope)- vProduct.l  (y,p,s,g,d)**2*(1/(2*Slope)))
                                                            -sum((y,rpp(rp,pa(p)),gad(g,d)), pWeight_rp(rp)*pSlopeVarCost(g)*vProduct.l(y,p,g,d)) -sum((y,gad(g,d)),vNewGen.l   (y,    g,d)*pGenInvCost (g))

                                                            -sum[(y,lc     ), pFixedCost (lc)*vNewLine.l(y,lc)]  ;
             ;

   pCongestionRents =
           //-sum[(y,rpp(rp,pa(p)),la(di,df)),  (-vPriceFlow1 (y,p,di,df) + vPriceFlow2(y,p,di,df))]
           - sum[(y,rpp(rp,pa(p)),la(di,df)),   pWeight_rp(rp)* ( pPrices (y,p,df) - pPrices (y,p,di))*vflow.l(y,p,di,df)/1000]
           + sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine.l(y,lc)        - vNewLine.l  (y-1,lc)]];
            pProduct       (y,pa(p),   th    )             =    sum((gad(g,d)), vProduct.l  (y,p,g,d)$tg(g,th)) ;
            pProduct_FX    (y,pa(p),gad(g,d) )             =    vProduct.l  (y,p,g,d) +  vWind.l(y,p,g,d)$wn(g) ;
            pInstalCapT_can(y,    g,   d     )             =    vNewGen.l   (y,    g,d)                         ;
            pFlow          (y,pa(p),   la    )             =  [ vFlow.l     (y,p, la)]                          ;
            pInstalLine    (y,         lc    )             =    vNewLine.l  (y,lc)                              ;
            pReserve_FX    (y,pt(p),gad(hf,d))             =    vReserve.l  (y,p,hf,d  )  +eps                  ;
            pReserve_FX    (y,ps(p),gad(hs,d))             =    vReserve.l  (y,p,hs,d  )  +eps                  ;
            pConsump       (y,pa(p),gad(h,d) )             =    vConsump.l  (y,p,h,d  )  +eps                   ;
            pDemandl       (  pa(p),   d     )             =  sum(y,  vDemand.l   (y,p,d))          +eps        ;

            //pWeightedCost                                 =  vTotalFCost.l /sum[(y,p,s,g,d), vProduct.l  (y,p,s,g,d)] ;
            $$ontext
              OF_Cost    ('Obj Func  Model      [1000 M€]') = pTotalCost1+EPS ;
              OF_ExtWel  ('Ext Welf  Model      [1000 M€]') = pExtendedWelfare + EPS ;
              GenCPUTime ('CPU Time  Model generation [s]') = CUADRATIC_MARKET.resGen  ;
              SolCPUTime ('CPU Time  Model solution   [s]') = CUADRATIC_MARKET.resUsd  ;
              NumVar     ('Number of variables           ') = CUADRATIC_MARKET.numVar  ;
              NumDVar    ('Number of discrete variables  ') = CUADRATIC_MARKET.numDVar ;
              NumEqu     ('Number of equations           ') = CUADRATIC_MARKET.numEqu  ;
              NumNZ      ('Number of nonzero elements    ') = CUADRATIC_MARKET.numNZ   ;
              BestSol    ('Best possible solution for MIP') = CUADRATIC_MARKET.objest  ;

            
              put TMP putclose
              'par=pInstalCapT_can rdim=1 rng=GenInv!j1:l3'  / 'par=pFlow       rdim=4 rng=Flow!j1:R1000'    / 'par=pTheta      rdim=3 rng=Angle!j1'                    /
              'par=pProduct_FX     rdim=2 rng=Output3!a1'    / 'par=pInstalLine rdim=1 rng=LineInv!j1:n3'    / 'par=pReserve_FX rdim=2 rng=WtrReserve!j1:p100000'       /
              'par=OF_Cost         rdim=1 rng=Cost!j1'       / 'par=GenCPUTime  rdim=1 rng=Cost!j2'          / 'par=SolCPUTime  rdim=1 rng=Cost!j3'                     /
              'par=NumVar          rdim=1 rng=Cost!j4'       / 'par=NumDVar     rdim=1 rng=Cost!j5'          / 'par=NumEqu      rdim=1 rng=Cost!j6'                     /
              'par=BestSol         rdim=1 rng=Cost!j7'       / 'par=pBenefits   rdim=1 rng=Prices!d2:f10'    / 'par=pPrices     rdim=3 rng=Prices!i15:l500'             /
              'text="QCP"                 rng=Prices!j1'     /'text="QCP" rng=WtrReserve!j1:j1'              / 'text="QCP"             rng=LineInv!j1:j1'               /
              'text="QCP"                 rng=Output!ac1'    / 'par=OF_ExtWel         rdim=1 rng=Cost!j8'    / 'text="QCP"             rng=GenInv!j1:l3'                /
              'text="QCP"                 rng=Cost!j9:j9'    /  'par=pConsump    rdim=2 rng=WtrReserve!x1:ab100000'       / 'text="QCP"            rng=WtrReserve!x1:x1'  /
              'par=pDemandl    rdim=1 rng=Hoja1!a1'
              execute_unload   '%gams.user1%.gdx' pInstalCapT_can pFlow pTheta  pProduct_FX pInstalLine pReserve_FX pConsump pDemandl
                                                  OF_Cost GenCPUTime SolCPUTime NumVar NumDVar NumEqu NumNZ BestSol pBenefits pPrices  OF_ExtWel
              execute          'gdxxrw "%gams.user1%".gdx SQ=n EpsOut=0 O="%gams.user1%".xlsx @"%gams.user1%".txt'
             execute          'del    "%gams.user1%".gdx '
            $$offtext
        ;
  pBenefits(cp ) =0; pPrices(y,pa(p), d)  = 0;
 //++++++++++++++++++++++++++++++++ Enumaration Loop End ++++++++++++++++++++++++++++++++++++


 //////////////////////////// Final Printing          //////////////////////////////////////


  pObjFunct (q,'1') =  +sum[(y,gcd(g,d)),(card(y)-ord(y)+1)*pGenInvCost (g)*[vNewGen.l(y, g,d) - vNewGen.l(y-1, g,d)]*(pMaxProd(g,d)+pMaxWindGen (g,d)$wn(g))] + EPS             ;
  pObjFunct (q,'2') =  +sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine.l(y,lc)      - vNewLine.l   (y-1,lc)  ]] + EPS             ;
  pObjFunct (q,'3') =  pTotalCost1     + EPS             ;
  pObjFunct (q,'4') =  +sum[(y,gcd(g,d)),(card(y)-ord(y)+1)*pGenInvCost (g)*[vNewGen.l(y, g,d) - vNewGen.l(y-1, g,d)]*(pMaxProd(g,d)+pMaxWindGen (g,d)$wn(g))] + EPS
                       +sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine.l(y,lc)        - vNewLine.l     (y-1,lc)  ]]
                       + pTotalCost1     + EPS             ;
  pObjFunct (q,'5') =  vtotaltcost.l; //CUADRATIC_MARKET.modelstat        ;
  pObjFunct (q,'6') =  pExtendedWelfare + EPS            ;
  pObjFunct (q,'7') =  pCongestionRents + eps            ;
  vNewLine.lo  (y,lc)  = 0;
  vNewLine.up  (y,lc)  = 1;
  );

  );

  put TMP putclose
      'par=pObjFunct  rdim=1 rng=Prices!w2:az100'/
      'text="GenCost"        rng=Prices!x1'      /'text="LinesCost"         rng=Prices!y1'     /'text="Oper.Cost"         rng=Prices!z1'     /
      'text="TotalCost"      rng=Prices!aa1'     /'text="Optimaal"         rng=Prices!ab1'     /'text="Welfare"         rng=Prices!ac1'     /
      'text="Cong. Rent"     rng=Prices!ad1'  /
      execute_unload   '%gams.user1%.gdx' pObjFunct
      execute          'gdxxrw "%gams.user1%".gdx SQ=n EpsOut=0 O="%gams.user1%".xlsx @"%gams.user1%".txt'
      execute          'del    "%gams.user1%".gdx ';






$onlisting
$log tipo de modelo a ejecutar de pTypeComp = %pTypeCOmp%
