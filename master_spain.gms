$EOLCOM //
option savepoint=     2 ;
PARAMETER
COST_REGRET
WELF_REGRET
parameter
pMin
pObjective
pMax

pExtendedWelfare1
pExtendedWelfare2
pExtendedWelfare05
pTotalCost05
pCompetitionType
pBenefitss
pOptimal
pRegret
Elast
is_elastic
pMercado
pDiv


;

//if(%Competitiontype%=0, pCompetitionType= 0);
pDiv             = 8;
pObjective       = 1;   // 0= Cost Min 1= MaxWelf 2= Merchant_Pieces 3=Merchant_Bilinnear 
pCompetitionType = 1;   // 0= perfect  1= Cournot 2= Intermediate
pMercado         = 3;   // 1= MCP 2=MIP (REGularization) 3=QCP
pRegret          = 0;   // 0= No Regret 1= Actual 2 = Naive 
is_elastic       = yes; 


//////////////////////////       FINDING OPTIMUM FROM  BILEVEL MODEL      //////////////////////////////
$$include STEXEM_SPAIN

set
qq(q);

if ( pMercado = 3, // Post-procesing for Chosing Optimal From Enumeration
  lc(di,df) = no;
  la(di,df) = no;


  
  lc( 'grp064' , 'grp354' ) =   Yes;
  lc( 'grp021' , 'grp027' ) =   Yes;
  lc( 'grp062' , 'grp353' ) =   Yes;
  lc( 'grp065' , 'grp354' ) =   Yes;
  lc( 'grp349' , 'grp354' ) =   Yes;



//

  la(di,df) $(lc(di,df) or le(di,df))=yes;
  vNewline.fx (y,       lc) = 0   ;
  pFlow       (y,pa(p), la) = 0   ;
  pInstalLine (y,       lc) = 0   ;

  display lc, vNewLine.l;
 
  
  //because the welfare is negative

  //display pmin;
  qq(q) = no;
  //display lc;

  if ( pObjective = 0, // Saving vector of minimal Cost
    pMin = smin(q, pObjFunct (q,'4'));
    pAct(q,di,df)$(pObjFunct (q , '4') <> pMin)=no;
    qq(q)$(pObjFunct (q , '4') = pMin)= yes;
    display qq;
  
     loop (qq,
     lc(di,df) = no;
     la(di,df)=no;
     lc(di,df) $[pAct(qq,di,df)=1] = yes;

     vNewLine.fx  (y,lc)  = 1;
     la(di,df) $(lc(di,df) or le(di,df))=yes;
     display lc, vNewLine.l;
     );
  );
  if ( pObjective = 1 ,                 // Saving Vecot for maximun Welfare
    pMax = smin(q, pObjFunct (q,  '6'));
    pAct(q,di,df)$(pObjFunct (q , '6') <> pMax)=no;
    qq(q)$(pObjFunct (q , '6') = pMax)= yes;
    loop (qq,
    lc(di,df) = no;
    la(di,df)=no;
    lc(di,df) $[pAct(qq,di,df)=1] = yes;

    vNewLine.fx  (y,lc)  = 1;
    la(di,df) $(lc(di,df) or le(di,df))=yes;
    //display lc,la,le;
    display lc, vNewLine.l;
    );
 );

  if ( pObjective >=2 ,                 // Saving Vecot for maximun Welfare
    pMax = smin(q, pObjFunct (q,  '7'));
    pAct(q,di,df)$(pObjFunct (q , '7') <> pMax)=no;
    qq(q)$(pObjFunct (q , '7') = pMax)= yes;
    loop (qq,
    lc(di,df) = no;
    la(di,df)=no;
    lc(di,df) $[pAct(qq,di,df)=1] = yes;

    vNewLine.fx  (y,lc)  = 1;
    la(di,df) $(lc(di,df) or le(di,df))=yes;
    //display lc,la,le;
    display lc, vNewLine.l;
    );
 );

//////////////////////////       GETTING RESULTS FROM OPTIMAL BILEVEL     //////////////////////////////
 solve CUADRATIC_MARKET minimizing vTotalTCost using miqcp       ;  
 display vTotalTCost.l, LC;
);

if (pRegret = 0,  // Puting Resuls in Parameters Directly
 
  pTotalCost1=0;
  pExtendedWelfare1=0;

  pTotalCost1=

         +sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp) * pSlopeVarCost(t)* vProduct.l      (y,p,t,d)]
         +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pHydroCost      * vProduct.l      (y,p,h,d)]
         +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pConsumCost     * vConsump.l      (y,p,h,d)]
         //+sum[(y,gcd(g,d)),(card(y)-ord(y)+1)*pGenInvCost (g)*[vNewGen.l(y, g,d) - vNewGen.l(y-1, g,d)]*(pMaxProd(g)+pMaxWindGen (g)$wn(g))] + EPS
         //+sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine.l(y,lc)        - vNewLine.l     (y-1,lc)  ]]
   
         ;

  pWindSpillage (y,pa(p),gad(wn,d)  )            =   pWindGen     (p,wn)*(vNewGen.l(y,wn,d)$(gcd(wn,d))) -  vWind.l(y,p,wn,d)  ;

  pExtendedWelfare1 =
              +                   sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp)* pSlopeVarCost(t)*vProduct.l(y,p,t,d)]
              +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pHydroCost      *vProduct.l(y,p,h,d)]
              +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pConsumCost     *vConsump.l(y,p,h,d)]
              //+(-(1/Slope)     *sum((y,rpp(rp,pa(p)),       d), pWeight_rp(rp)* [ pDemand(p) * pDemShare(d) * pCumDemIncr(y) * pDemandl(p,d) -  power(pDemandl(p,d),2)/2])) $(Slope)
              +                   sum((y,rpp(rp,pa(p)),       d), -(1/Slope(p,d))$(slope(p,d)) *pWeight_rp(rp)* [ pdemandnode(y,p,d) * vDemand.l(y,p,d) -  power(vDemand.l(y,p,d),2)/2])
              +                   sum[(y,    gcd(g,d)),  pGenInvCost (g )*vNewGen.l(y, g, d)*pMaxProd    (g,d) ]
              +                   sum[(y,    gcd(wn,d)), pGenInvCost (wn)*vNewGen.l(y, wn,d)*pMaxWindGen (wn,d)]
              +                   sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost(lc)*[vNewLine.l(y,lc) - vNewLine.l(y-1,lc)]]  ;


  //pCongestionRents =

      //   +sum[(y,rpp(rp,pa(p)),la(di,df)), pWeight_rp(rp)* (-vPriceFlow1 (y,p,di,df) + vPriceFlow2(y,p,di,df))]
      //         -sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine(y,lc)        - vNewLine  (y-1,lc)]]

       //      ;
  pPrices        (y,pa(p), d) $(pMercado = 3)      = // + sum((rpp(rp,p)), ( (pDemand(p) * pDemShare(d) * pCumDemIncr(y)-vDemand.l(y,p,d))/Slope(d) )*1000 ) ;
                                                        + sum((rpp(rp,p)), ( +eBalance_C_can.m (y,p,d))*1000/pWeight_rp(rp)) +eps       ;
    
   
  pPrices        (y,pa(p), d) $(pMercado <> 3)     =    sum((rpp(rp,p)), b_cLambda.l (y,p,d)*1000)+eps                                ;
  pBenefitss                                        =  + sum((y,rpp(rp,pa(p)),gad(t ,d)),( pWeight_rp(RP)* vProduct.l(y,p,t ,d) *( pPrices (y,p,d)/1000- pSlopeVarCost(t)   ) ))   //
                                                        + sum((y,rpp(rp,pa(p)),gad(wn,d)), ( pWeight_rp(RP)* vWind.l   (y,p,wn,d))*( pPrices (y,p,d)/1000 ) )   //
                                                        + sum((y,rpp(rp,pa(p)),gad(h ,d)),(  pWeight_rp(RP)* (pPrices (y,p,d)/1000 ) *(vProduct.l(y,p,h,d)- vConsump.l(y,p,h,d)) ))
                                                        - sum[(y,rpp(rp,pa(p)),gad(h ,d)),  pWeight_rp(rp)* pHydroCost *vProduct.l(y,p,h,d)]
                                                        - sum[(y,rpp(rp,pa(p)),gad(h ,d)),  pWeight_rp(rp)* pConsumCost*vConsump.l(y,p,h,d)]
                                                        - sum[(y,              gcd(g ,d)),  pGenInvCost (g )*vNewGen.l(y, g, d)*pMaxProd    (g,d) ]
                                                        - sum[(y,              gcd(wn,d)),  pGenInvCost (wn)*vNewGen.l(y, wn,d)*pMaxWindGen (wn,d)];
 
  pConsumer_Surplus                                 = //sum((y,rpp(rp,pa(p)),s,gad(g,d)), pDemand(p,s) * pDemShare(d) * pCumDemIncr(y)*vProduct.l  (y,p,s,g,d)*(1/Slope)- vProduct.l  (y,p,s,g,d)**2*(1/(2*Slope)))
                                                    -sum((y,rpp(rp,pa(p)),gad(g,d)), pWeight_rp(rp)*pSlopeVarCost(g)*vProduct.l(y,p,g,d)) -sum((y,gad(g,d)),vNewGen.l   (y,    g,d)*pGenInvCost (g))
                                                  -sum[(y,lc     ), pFixedCost (lc)*vNewLine.l(y,lc)]  ;
     ;
 
  pEnergy        (                     th )         =    sum((y, rpp(rp,pa(p)),gad(g,d)), pWeight_rp(rp)*(vProduct.l  (y,p,g,d)+vWind.l(y,p,g,d)$wn(g)+vSolar.l(y,p,g,d)$sr(g)) $tg(g,th) )         ;
  pProduct       (y,pa(p),  th )                    =    sum((gad(g,d)), vProduct.l  (y,p,g,d)$tg(g,th))               ;
  pProduct_FX    (y,pa(p),gad(g,d))                 =    vProduct.l  (y,p,g,d)   +  vWind.l(y,p,g,d)$wn(g) +eps        ;
  pTotalEmission ( th )                             =    sum ((y, pa(p)), pProduct(y,p,  th )*pEmissionrate(th) )      ;
  pInstalCapT_can(y,    gcd(g,d) )                  =    vNewGen.l   (y,    g,d)      +eps                             ;
  pInstalCapTot    (   th    )                      =    sum((y,gcd(g,d)), vNewGen.l(y,g,d)*(pMaxProdGen(g)$t(g)+pMaxProdGen(g)$(h(g))+ pMaxWindGen  (g,d)$(wn(g))  + pMaxSolarGen (g,d)$(sr(g)) ) $tg(g,th)   )    ;
  pFlow    (y,pa(p), la)                            =  [ vFlow.l     (y,p, la)]       +eps                             ;
  display pInstalLine;
  pInstalLine    (y,         lc)                    =    vNewLine.l  (y,lc)                                            ;
  display pInstalLine;
  pReserve_FX(y,pt(p),gad(hf,d) )                   =    vReserve.l  (y,p,hf,d  )  +eps                                ;
  pReserve_FX(y,ps(p),gad(hs,d) )                   =    vReserve.l  (y,p,hs,d  )  +eps                                ;
  pConsump   (y,pa(p),gad(h,d) )                    =    vConsump.l  (y,p,h,d  )  +eps                                 ;
  pDemandl (pa(p),d)                                =  sum(y,  vDemand.l   (y,p,d))                                    ;

  if ( pMercado = 3, // If results come from enumeration
    OF_Cost    ('Op Cost   Model      [1000 M€]') = pTotalCost1 + EPS                                                                ;
    OF_ExtWel  ('Ext Welf  Model      [1000 M€]') = pExtendedWelfare1 + EPS                                                           ;
    GenCPUTime ('CPU Time  Model generation [s]') = CUADRATIC_MARKET.resGen                                                          ;
    SolCPUTime ('CPU Time  Model solution   [s]') = CUADRATIC_MARKET.resUsd                                                          ;
    NumVar     ('Number of variables           ') = CUADRATIC_MARKET.numVar                                                          ;
    NumDVar    ('Number of discrete variables  ') = CUADRATIC_MARKET.numDVar                                                         ;
    NumEqu     ('Number of equations           ') = CUADRATIC_MARKET.numEqu                                                          ;
    NumNZ      ('Number of nonzero elements    ') = CUADRATIC_MARKET.numNZ                                                           ;
    BestSol    ('Best possible solution for MIP') = CUADRATIC_MARKET.objest   ;                                                       ;
  else               // if results come form MIP
    OF_Cost    ('Op Cost   Model      [1000 M€]') = pTotalCost1 + EPS                                                                ;
    OF_ExtWel  ('Ext Welf  Model      [1000 M€]') = pExtendedWelfare1 + EPS                                                           ;
    GenCPUTime ('CPU Time  Model generation [s]') = BILEVEL_KKT_MILP.resGen                                                          ;
    SolCPUTime ('CPU Time  Model solution   [s]') = BILEVEL_KKT_MILP.resUsd                                                          ;
    NumVar     ('Number of variables           ') = BILEVEL_KKT_MILP.numVar                                                          ;
    NumDVar    ('Number of discrete variables  ') = BILEVEL_KKT_MILP.numDVar                                                         ;
    NumEqu     ('Number of equations           ') = BILEVEL_KKT_MILP.numEqu                                                          ;
    NumNZ      ('Number of nonzero elements    ') = BILEVEL_KKT_MILP.numNZ                                                           ;
    BestSol    ('Best possible solution for MIP') = BILEVEL_KKT_MILP.objest   ;                                                       ;

  );

else              // Computin Regret
  lc(di,df) = no;
  la(di,df) = no;
  lc( 'grp064' , 'grp354' ) =   Yes;
  lc( 'grp021' , 'grp027' ) =   Yes;
  lc( 'grp062' , 'grp353' ) =   Yes;
  lc( 'grp065' , 'grp354' ) =   Yes;
  lc( 'grp349' , 'grp354' ) =   Yes;
  
  la(di,df) $(lc(di,df) or le(di,df))=yes;
 
 pFixedDemand (  p,d      )=  sum(y, vdemand.l (y,p,d));

 pCost=yes;
 
 vNewLine.lo  (y,lc)  = 0;
 vNewLine.up  (y,lc)  = 1;
 
//////////////////////////                SOLVING NAIVE CP                //////////////////////////////

  pTypeComp = yes;
  pCost     = yes;

 solve GEPTEP_COSTMIN using MiP minimizing vTotalFCost;


 if  ( pRegret = 2,  // Storing Results to print for Naive
  pTotalCost05=0;
  pTotalCost05=

         +sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp) * pSlopeVarCost(t)* vProduct.l      (y,p,t,d)]
         +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pHydroCost      * vProduct.l      (y,p,h,d)]
         +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pConsumCost     * vConsump.l      (y,p,h,d)]
      //   +sum[(y,    gcd(g,d),gcp(g,cp)),  pGenInvCost (g )*vNewGen.l(y, g, d)*pMaxProd    (g,d) ]
      //   +sum[(y,    gcd(wn,d),gcp(wn,cp)), pGenInvCost (wn)*vNewGen.l(y, wn,d)*pMaxWindGen (wn,d)]
         +sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost(lc)*[vNewLine.l(y,lc) - vNewLine.l(y-1,lc)]]
         ;
  pPrices        (y,pa(p), d)                       =    sum((rpp(rp,p)), (eBalance_C_can.m (y,p,d)*1000 )/pWeight_rp(RP)  ) ;
  pExtendedWelfare05 =
    +                   sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp)* pSlopeVarCost(t)*vProduct.l(y,p,t,d)]
    +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pHydroCost      *vProduct.l(y,p,h,d)]
    +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pConsumCost     *vConsump.l(y,p,h,d)]
    +                   sum((y,rpp(rp,pa(p)),       d),-(1/Slope(p,d) * pWeight_rp(rp)* [ pdemandnode(y,p,d) *  pFixedDemand(p,d) -  power(pFixedDemand(p,d),2)/2]))
    +                   sum[(y,    gcd(g,d)),  pGenInvCost (g )*vNewGen.l(y, g, d)*pMaxProd    (g,d) ]
    +                   sum[(y,    gcd(wn,d)), pGenInvCost (wn)*vNewGen.l(y, wn,d)*pMaxWindGen (wn,d)]
    +                   sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost(lc)*[vNewLine.l(y,lc) - vNewLine.l(y-1,lc)]]  ;


    pProduct       (y,pa(p),  th )                =    sum((gad(g,d)), vProduct.l  (y,p,g,d)$tg(g,th)) ;
    pProduct_FX    (y,pa(p),gad(g,d))             =    vProduct.l  (y,p,g,d)   +  vWind.l(y,p,g,d)$wn(g) ;
    pInstalCapT_can(y,    g,d      )              =    vNewGen.l   (y,    g,d)                   ;
    pFlow    (y,pa(p), la)                        =  [ vFlow.l     (y,p, la)]                          ;
    pInstalLine    (y,         lc)                =    vNewLine.l  (y,lc)                              ;
    pReserve_FX(y,pt(p),gad(hf,d) )               =    vReserve.l  (y,p,hf,d  )  +eps                  ;
    pReserve_FX(y,ps(p),gad(hs,d) )               =    vReserve.l  (y,p,hs,d  )  +eps                  ;
    pConsump   (y,pa(p),gad(h,d) )                =    vConsump.l  (y,p,h,d  )  +eps                   ;
    pDemandl (pa(p),d)                            =  sum(y,  pFixedDemand(p,d))                             ;
    pPrices        (y,pa(p), d)                   =  pPrices(y,p, d) *1000;

    OF_Cost    ('Op Cost   Model      [1000 M€]') = pTotalCost05 + EPS                                                                ;
    OF_ExtWel  ('Ext Welf  Model      [1000 M€]') = pExtendedWelfare05 + EPS                                                           ;
    GenCPUTime ('CPU Time  Model generation [s]') = CUADRATIC_MARKET.resGen                                                          ;
    SolCPUTime ('CPU Time  Model solution   [s]') = CUADRATIC_MARKET.resUsd                                                          ;
    NumVar     ('Number of variables           ') = CUADRATIC_MARKET.numVar                                                          ;
    NumDVar    ('Number of discrete variables  ') = CUADRATIC_MARKET.numDVar                                                         ;
    NumEqu     ('Number of equations           ') = CUADRATIC_MARKET.numEqu                                                          ;
    NumNZ      ('Number of nonzero elements    ') = CUADRATIC_MARKET.numNZ                                                           ;
    BestSol    ('Best possible solution for MIP') = CUADRATIC_MARKET.objest   ;                                                       ;
 );

 display pExtendedWelfare, pBenefits   ;
  ;
  pOptimal= COST_MINIMISATION.modelstat ;
  vdemand.lo (y,p,d)=0;
  vdemand.up (y,p,d)=inf;
  display  vNewLine.l, lc,pFixedDemand;
  vNewLine.fx  (y,lc)  = vNewline.l (y,lc);
  pTypeComp = no;
  pCost=no;
  )
 ;

if (pRegret =1,
//////////////////////////               SOLVING ACTUAL CP                //////////////////////////////
  solve CUADRATIC_MARKET minimizing vTotalTCost using miqcp       ;
  pTotalCost2=0;
  pTotalCost2=

         +sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp) * pSlopeVarCost(t)* vProduct.l      (y,p,t,d)]
         +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pHydroCost      * vProduct.l      (y,p,h,d)]
         +sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp) * pConsumCost     * vConsump.l      (y,p,h,d)]
         +sum[(y,    gcd(g,d),gcp(g,cp)),  pGenInvCost (g )*vNewGen.l(y, g, d)*pMaxProd    (g,d) ]
         +sum[(y,    gcd(wn,d),gcp(wn,cp)), pGenInvCost (wn)*vNewGen.l(y, wn,d)*pMaxWindGen (wn,d)]
         +sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost(lc)*[vNewLine.l(y,lc) - vNewLine.l(y-1,lc)]]
         ;
  pPrices        (y,pa(p), d)                       =    sum((rpp(rp,p)), ( (pDemandNode(y,p,d)-vDemand.l(y,p,d))/Slope(p,d) )*1000 ) ;
  pExtendedWelfare2 =
    +                   sum[(y,rpp(rp,pa(p)),gad(t,d)), pWeight_rp(rp)* pSlopeVarCost(t)*vProduct.l(y,p,t,d)]
    +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pHydroCost      *vProduct.l(y,p,h,d)]
    +                   sum[(y,rpp(rp,pa(p)),gad(h,d)), pWeight_rp(rp)* pConsumCost     *vConsump.l(y,p,h,d)]
    //+                   sum[(y,rpp(rp,pa(p)),gad(g,d)), pWeight_rp(rp)*(conjcvariation(g)/2)*power(vProduct.l(y,p,g,d)-(vConsump.l(y,p,g,d))$h(g) ,2)      ] //
    //+(-(1/Slope)     *sum((y,rpp(rp,pa(p)),       d), pWeight_rp(rp)* [ pDemand(p) * pDemShare(d) * pCumDemIncr(y) * pDemandl(p,d) -  power(pDemandl(p,d),2)/2])) $(Slope)
    +                   sum((y,rpp(rp,pa(p)),       d),-(1/Slope(p,d) * pWeight_rp(rp)* [ pDemandNode(y,p,d) * vDemand.l(y,p,d) -  power(vDemand.l(y,p,d),2)/2]))
    +                   sum[(y,    gcd(g,d)),  pGenInvCost (g )*vNewGen.l(y, g, d)*pMaxProd    (g,d) ]
    +                   sum[(y,    gcd(wn,d)), pGenInvCost (wn)*vNewGen.l(y, wn,d)*pMaxWindGen (wn,d)]
    +                   sum[(y,lc),      (card(y)-ord(y)+1)*pFixedCost(lc)*[vNewLine.l(y,lc) - vNewLine.l(y-1,lc)]]  ;



    pBenefitss                                        =  + sum((y,rpp(rp,pa(p)),gad(t ,d)),( pWeight_rp(RP)* vProduct.l(y,p,t ,d) *( pPrices (y,p,d)/1000- pSlopeVarCost(t)   ) ))   //
                                                        + sum((y,rpp(rp,pa(p)),gad(wn,d)), ( pWeight_rp(RP)* vWind.l   (y,p,wn,d))*( pPrices (y,p,d)/1000 ) )   //
                                                        + sum((y,rpp(rp,pa(p)),gad(h ,d)),(  pWeight_rp(RP)* (pPrices (y,p,d)/1000 ) *(vProduct.l(y,p,h,d)- vConsump.l(y,p,h,d)) ))
                                                        - sum[(y,rpp(rp,pa(p)),gad(h ,d)),  pWeight_rp(rp)* pHydroCost *vProduct.l(y,p,h,d)]
                                                        - sum[(y,rpp(rp,pa(p)),gad(h ,d)),  pWeight_rp(rp)* pConsumCost*vConsump.l(y,p,h,d)]
                                                        - sum[(y,              gcd(g ,d)),  pGenInvCost (g )*vNewGen.l(y, g, d)*pMaxProd    (g,d) ]
                                                        - sum[(y,              gcd(wn,d)),  pGenInvCost (wn)*vNewGen.l(y, wn,d)*pMaxWindGen (wn,d)];

            ;

    pProduct       (y,pa(p),  th )                =    sum((gad(g,d)), vProduct.l  (y,p,g,d)$tg(g,th))                                ;
    pProduct_FX    (y,pa(p),gad(g,d))             =    vProduct.l  (y,p,g,d)   +  vWind.l(y,p,g,d)$wn(g)                              ;
    pInstalCapT_can(y,    gcd(g,d)    )           =    vNewGen.l   (y,    g,d)        +eps                                            ;
    pInstalCapTot    (   th    )                  =   sum((y,gcd(g,d)), vNewGen.l(y,g,d)*(
                                                                  + pMaxProdGen(g  )$ t (g)  +pMaxProdGen  (g  )$(h (g))
                                                                  + pMaxWindGen(g,d)$(wn(g)) +pMaxSolarGen (g,d)$(sr(g))) $tg(g,th))  ;
    pEnergy        (                     th )         =    sum((y, rpp(rp,pa(p)),gad(g,d)), pWeight_rp(rp)*(vProduct.l  (y,p,g,d)+vWind.l(y,p,g,d)$wn(g)+vSolar.l(y,p,g,d)$sr(g)) $tg(g,th) )        ;                                                              
    pTotalEmission ( th )                             =    sum ((y, pa(p)), pProduct(y,p,  th )*pEmissionrate(th) )      ;
    pFlow    (y,pa(p), la)                        =  [ vFlow.l     (y,p, la)]                                                         ;
    pInstalLine    (y,         lc)                =    vNewLine.l  (y,lc)                                                             ;
    pReserve_FX(y,pt(p),gad(hf,d) )               =    vReserve.l  (y,p,hf,d  )  +eps                                                 ;
    pReserve_FX(y,ps(p),gad(hs,d) )               =    vReserve.l  (y,p,hs,d  )  +eps                                                 ;
    pConsump   (y,pa(p),gad(h,d) )                =    vConsump.l  (y,p,h,d  )  +eps                                                  ;
    pDemandl (pa(p),d)                            =  sum(y,  vDemand.l   (y,p,d))   ;                                                  ;

    OF_Cost    ('Op Cost   Model      [1000 M€]') = pTotalCost2 + EPS                                                                ;
    OF_ExtWel  ('Ext Welf  Model      [1000 M€]') = pExtendedWelfare2 + EPS                                                          ;
    GenCPUTime ('CPU Time  Model generation [s]') = CUADRATIC_MARKET.resGen                                                          ;
    SolCPUTime ('CPU Time  Model solution   [s]') = CUADRATIC_MARKET.resUsd                                                          ;
    NumVar     ('Number of variables           ') = CUADRATIC_MARKET.numVar                                                          ;
    NumDVar    ('Number of discrete variables  ') = CUADRATIC_MARKET.numDVar                                                         ;
    NumEqu     ('Number of equations           ') = CUADRATIC_MARKET.numEqu                                                          ;
    NumNZ      ('Number of nonzero elements    ') = CUADRATIC_MARKET.numNZ                                                           ;
    BestSol    ('Best possible solution for MIP') = CUADRATIC_MARKET.objest   ;                                                       ;



 // DISPLAY   COST_REGRET,WELF_REGRET, pTotalCost2,pTotalCost1,pExtendedWelfare, vNewline.l ;
);

display pBenefitss; 



*Puting data into Excel

put TMP putclose
'par=pInstalCapT_can rdim=1 rng=GenInv!a1:zz4'        / 'par=pFlow          rdim=4 rng=Flow!a1'     / 'par=pTheta      rdim=3 rng=Angle!a1'   /
'par=pProduct        rdim=2 rng=Output!a1'            / 'par=pInstalLine    rdim=1 rng=LineInv!a1'  /
'par=pReserve_FX     rdim=2 rng=WtrReserve!a1'        / 'par=OF_Cost        rdim=1 rng=Cost!a1'     / 'par=GenCPUTime rdim=1 rng=Cost!a2'     / 
'par=NumVar          rdim=1 rng=Cost!a4'              / 'par=NumDVar        rdim=1 rng=Cost!a5'     / 'par=NumEqu      rdim=1 rng=Cost!a6'    /
'par=BestSol         rdim=1 rng=Cost!a7'              / 'par=pBenefitss     rdim=0 rng=Prices!a1'   / 'par=SolCPUTime  rdim=1 rng=Cost!a3'    /
'par=pPrices         rdim=3 rng=Prices!a25:d1000000'  / 'par=pEnergy        rdim=1 rng=Energy!a1:d15'/
'par=pInstalCapTot   rdim=1 rng=GenInv!b6:d20'        / 'par=pTotalEmission  rdim=1 rng=Energy!a25'/
//'par=pHourlySolpe    rdim=3 rng=Prices!f25'   /
execute_unload   '%gams.user1%.gdx' pInstalCapT_can pFlow pTheta  pProduct pInstalLine pReserve_FX pEnergy pInstalCapTot pTotalEmission
                                    OF_Cost GenCPUTime SolCPUTime NumVar NumDVar NumEqu NumNZ BestSol pBenefitss  pPrices  pMarginalCosts
execute          'gdxxrw "%gams.user1%".gdx SQ=n EpsOut=0 O="%gams.user1%".xlsx @"%gams.user1%".txt'
execute          'del    "%gams.user1%".gdx '





