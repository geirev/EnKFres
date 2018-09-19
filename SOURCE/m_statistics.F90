module m_statistics
   use mod_dimensions
   real, public, dimension(nz) ::  Cxz, Cxp, Czp
   real, public, dimension(nz) ::  rx,  ry, dir
   real, public, dimension(nz) :: Cpx, Czx, Czz
   real, public, dimension(nz) :: avep, stdp, pu, pl
   real, public, dimension(nz) :: avex, stdx, xu, xl
   real, public, dimension(nz) :: avez, stdz, zu, zl
   real, public, dimension(nz) :: ratiozx
   real, public :: principal_dir=90.0
contains
subroutine statistics (logperm)

   implicit none
   logical, intent(in) :: logperm

   real aaa,bbb
   integer k,i



! Correlation lengths and orientation.
! rx is the decorrelation length in the principal direction, ry is the
! decorrelation length orthogonal to the principal direction.  If the 
! decorrelation lengths are given corresponding to a spherical variogram
! they must coverted to Gaussian by scaling them with sqrt(3).  The
! direction is given relative to north and with clockwise rotation.
! This must be converted to mathematical units, relative to eastward
! and anticlockwise rotation.

   rx(1)   =1500. ; ry(1)   =600.  ; dir(1)   =  0.0
   rx(2)   =1500. ; ry(2)   =600.  ; dir(2)   =  0.0
   rx(3)   = 600. ; ry(3)   =600.  ; dir(3)   =  0.0
   rx(4:5) =3000. ; ry(4:5) =750.  ; dir(4:5) = 90.0
   rx(6)   =3000. ; ry(6)   =750.  ; dir(6)   = 90.0
   rx(7)   =2000. ; ry(7)   =600.  ; dir(7)   = 20.0
   rx(8:nz)=1000. ; ry(8:nz)=500.  ; dir(8:nz)=155.0

! In the table the decorrelation lengths are given for a spherical variogram.
! Converting from Spherical to Gaussian
   rx=rx/sqrt(3.0)
   ry=ry/sqrt(3.0)

! In table the direction is given relative to north and rotating clockwise.
! Convertion to math angles with principal dir to the east and rotating 
! Anticlockwise.  (e.g. 30 deg in table is 60 in math units).
   dir=principal_dir-dir



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Vertical correlations
! Czz(k) in [0.0:1.0] is the correlation between variables in layer k and k-1.
   Czz(1:nz)=0.0
   Czz(5)=0.8      ! layer 4 and 5 are strongly correlated
   Czz(9:15)=0.8   ! layers 8 to 15 are strongly correlated

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Correlation between poro and log(permx)
   Cpx=0.75

! Correlation between permx and permz
   Czx=0.75

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! PERMX statistics
i=  1; avex(i)=  500.0000; stdx(i)=    0.0000; xl(i)=  500.0000; xu(i)=  500.0000
i=  2; avex(i)=  418.6280; stdx(i)=  468.7701; xl(i)=    0.0000; xu(i)= 5160.9399
i=  3; avex(i)=  423.6478; stdx(i)=  563.2332; xl(i)=    0.0000; xu(i)= 6657.0601
i=  4; avex(i)=  460.2248; stdx(i)=  681.2633; xl(i)=    0.0000; xu(i)= 7327.2500
i=  5; avex(i)=  474.0785; stdx(i)=  653.3462; xl(i)=    0.0000; xu(i)= 7744.4800
i=  6; avex(i)=  394.2477; stdx(i)=  514.1024; xl(i)=    0.0000; xu(i)= 5643.8701
i=  7; avex(i)=  421.5677; stdx(i)=  558.7507; xl(i)=    0.0000; xu(i)= 7279.1802
i=  8; avex(i)=  416.8734; stdx(i)=  513.1647; xl(i)=    0.0000; xu(i)= 5561.2700
i=  9; avex(i)=  426.6996; stdx(i)=  493.1367; xl(i)=    0.0000; xu(i)= 5236.5000
i= 10; avex(i)=  798.7406; stdx(i)=  724.2360; xl(i)=   18.4605; xu(i)= 6413.5098
i= 11; avex(i)=  871.1799; stdx(i)=  969.2995; xl(i)=   20.7272; xu(i)= 8640.6201
i= 12; avex(i)=  776.3117; stdx(i)=  740.2893; xl(i)=   23.2790; xu(i)= 6799.6602
i= 13; avex(i)=  256.4503; stdx(i)=  315.3811; xl(i)=    0.6371; xu(i)= 1278.3500
i= 14; avex(i)=  227.1403; stdx(i)=  311.8534; xl(i)=    0.3920; xu(i)= 1356.5800
i= 15; avex(i)=  219.4358; stdx(i)=  318.6580; xl(i)=    0.2261; xu(i)= 1233.4600
i= 16; avex(i)= 1017.6587; stdx(i)= 1112.4901; xl(i)=    6.6488; xu(i)=11289.5996
i= 17; avex(i)= 1166.8864; stdx(i)= 1404.8967; xl(i)=   25.5137; xu(i)=12875.7998
i= 18; avex(i)= 1774.7196; stdx(i)= 1285.7191; xl(i)=  133.4670; xu(i)= 5595.0200
i= 19; avex(i)= 1642.5829; stdx(i)= 1141.3608; xl(i)=  109.9910; xu(i)= 5800.0000
i= 20; avex(i)= 1595.1266; stdx(i)= 1079.6232; xl(i)=  123.4640; xu(i)= 5580.8198
i= 21; avex(i)= 1566.7245; stdx(i)= 1093.1555; xl(i)=  125.1710; xu(i)= 5628.8901
i= 22; avex(i)= 1329.4135; stdx(i)=  866.1164; xl(i)=  111.5460; xu(i)= 4429.9902
i= 23; avex(i)= 1506.1342; stdx(i)= 1118.8223; xl(i)=   82.8643; xu(i)= 5500.0000
i= 24; avex(i)= 1352.8902; stdx(i)= 1021.4526; xl(i)=   22.4521; xu(i)= 5500.0000
i= 25; avex(i)= 1317.9390; stdx(i)= 1056.7247; xl(i)=   27.3513; xu(i)= 5500.0000
i= 26; avex(i)=  737.4940; stdx(i)=  505.0709; xl(i)=   24.1896; xu(i)= 3243.8101
i= 27; avex(i)= 2106.9776; stdx(i)= 2470.3407; xl(i)=   67.4112; xu(i)=14213.4004
i= 28; avex(i)= 2005.6738; stdx(i)= 2335.3901; xl(i)=   50.3442; xu(i)=14945.2998
i= 29; avex(i)= 1819.4116; stdx(i)= 2178.2501; xl(i)=   54.5298; xu(i)=15000.0000
i= 30; avex(i)= 1574.0989; stdx(i)= 1845.1995; xl(i)=   51.6898; xu(i)=15000.0000
i= 31; avex(i)= 1273.7067; stdx(i)= 1451.0880; xl(i)=   33.7200; xu(i)=12329.9004
i= 32; avex(i)= 1582.4511; stdx(i)= 1010.0198; xl(i)=  109.9290; xu(i)= 5069.9600
i= 33; avex(i)= 1483.8849; stdx(i)=  976.9878; xl(i)=  102.2880; xu(i)= 5317.8198

! PERMZ statistics
i=  1; avez(i)=   50.0000; stdz(i)=    0.0000; zl(i)=   50.0000; zu(i)=   50.0000
i=  2; avez(i)=  118.6803; stdz(i)=  142.5855; zl(i)=    0.0000; zu(i)= 1860.3700
i=  3; avez(i)=  117.6744; stdz(i)=  173.4968; zl(i)=    0.0000; zu(i)= 2546.8201
i=  4; avez(i)=  137.1865; stdz(i)=  231.2814; zl(i)=    0.0000; zu(i)= 2911.0300
i=  5; avez(i)=  137.7914; stdz(i)=  219.1508; zl(i)=    0.0000; zu(i)= 3161.2100
i=  6; avez(i)=  115.6400; stdz(i)=  175.0938; zl(i)=    0.0000; zu(i)= 2279.2100
i=  7; avez(i)=  123.3725; stdz(i)=  187.5221; zl(i)=    0.0000; zu(i)= 2949.8000
i=  8; avez(i)=  122.4408; stdz(i)=  168.2402; zl(i)=    0.0000; zu(i)= 2163.0500
i=  9; avez(i)=  118.4140; stdz(i)=  155.9349; zl(i)=    0.0000; zu(i)= 1660.5000
i= 10; avez(i)=  153.0639; stdz(i)=  120.4952; zl(i)=    2.7476; zu(i)=  862.6370
i= 11; avez(i)=  173.3842; stdz(i)=  188.1833; zl(i)=    3.7318; zu(i)= 1903.9000
i= 12; avez(i)=  152.0485; stdz(i)=  137.7184; zl(i)=    2.9416; zu(i)= 1218.3700
i= 13; avez(i)=   59.4119; stdz(i)=   89.4706; zl(i)=    0.1895; zu(i)=  366.7360
i= 14; avez(i)=   56.4475; stdz(i)=   89.0009; zl(i)=    0.1374; zu(i)=  393.1700
i= 15; avez(i)=   57.4185; stdz(i)=   91.4685; zl(i)=    0.1463; zu(i)=  357.1800
i= 16; avez(i)=  339.0555; stdz(i)=  387.6071; zl(i)=    0.4955; zu(i)= 4181.8301
i= 17; avez(i)=  394.4224; stdz(i)=  513.8497; zl(i)=    1.3435; zu(i)= 4961.0200
i= 18; avez(i)=  684.5460; stdz(i)=  536.0133; zl(i)=   57.9266; zu(i)= 2289.0901
i= 19; avez(i)=  631.0763; stdz(i)=  475.2353; zl(i)=   19.7004; zu(i)= 2378.0000
i= 20; avez(i)=  604.3717; stdz(i)=  443.0045; zl(i)=   45.7385; zu(i)= 2283.3101
i= 21; avez(i)=  576.7857; stdz(i)=  449.1039; zl(i)=   46.8277; zu(i)= 2303.4299
i= 22; avez(i)=  329.3590; stdz(i)=  213.0582; zl(i)=   29.9070; zu(i)= 1096.0300
i= 23; avez(i)=  583.9818; stdz(i)=  465.6916; zl(i)=    8.3381; zu(i)= 2255.0000
i= 24; avez(i)=  505.8922; stdz(i)=  422.3408; zl(i)=   12.8265; zu(i)= 2255.0000
i= 25; avez(i)=  486.6825; stdz(i)=  432.3822; zl(i)=    8.3757; zu(i)= 2255.0000
i= 26; avez(i)=  128.1232; stdz(i)=   93.8971; zl(i)=    4.4026; zu(i)=  648.0780
i= 27; avez(i)=  719.8396; stdz(i)=  893.4123; zl(i)=   27.9492; zu(i)= 5803.3198
i= 28; avez(i)=  677.7836; stdz(i)=  855.5416; zl(i)=   21.8660; zu(i)= 6126.9702
i= 29; avez(i)=  575.7839; stdz(i)=  774.2082; zl(i)=   15.4333; zu(i)= 6150.0000
i= 30; avez(i)=  450.9493; stdz(i)=  603.8095; zl(i)=    1.4586; zu(i)= 6150.0000
i= 31; avez(i)=  240.6151; stdz(i)=  309.0033; zl(i)=    8.0936; zu(i)= 4669.7300
i= 32; avez(i)=  428.4922; stdz(i)=  300.7854; zl(i)=   29.6670; zu(i)= 1758.9100
i= 33; avez(i)=  338.0808; stdz(i)=  230.3058; zl(i)=   24.4888; zu(i)= 1693.0000


! PORO statistics
i=  1; avep(i)=    0.2500; stdp(i)=    0.0000; pl(i)=    0.2500; pu(i)=    0.2500
i=  2; avep(i)=    0.1133; stdp(i)=    0.0561; pl(i)=    0.0002; pu(i)=    0.2892
i=  3; avep(i)=    0.1153; stdp(i)=    0.0578; pl(i)=    0.0003; pu(i)=    0.2902
i=  4; avep(i)=    0.1252; stdp(i)=    0.0635; pl(i)=    0.0007; pu(i)=    0.3000
i=  5; avep(i)=    0.1263; stdp(i)=    0.0622; pl(i)=    0.0002; pu(i)=    0.3000
i=  6; avep(i)=    0.1182; stdp(i)=    0.0595; pl(i)=    0.0005; pu(i)=    0.2944
i=  7; avep(i)=    0.1224; stdp(i)=    0.0604; pl(i)=    0.0003; pu(i)=    0.2988
i=  8; avep(i)=    0.1227; stdp(i)=    0.0601; pl(i)=    0.0001; pu(i)=    0.2953
i=  9; avep(i)=    0.1214; stdp(i)=    0.0584; pl(i)=    0.0002; pu(i)=    0.2987
i= 10; avep(i)=    0.1695; stdp(i)=    0.0435; pl(i)=    0.0696; pu(i)=    0.2576
i= 11; avep(i)=    0.1708; stdp(i)=    0.0447; pl(i)=    0.0682; pu(i)=    0.2791
i= 12; avep(i)=    0.1680; stdp(i)=    0.0432; pl(i)=    0.0698; pu(i)=    0.2646
i= 13; avep(i)=    0.1366; stdp(i)=    0.0309; pl(i)=    0.0513; pu(i)=    0.2469
i= 14; avep(i)=    0.1198; stdp(i)=    0.0274; pl(i)=    0.0332; pu(i)=    0.2357
i= 15; avep(i)=    0.1045; stdp(i)=    0.0261; pl(i)=    0.0292; pu(i)=    0.2181
i= 16; avep(i)=    0.1938; stdp(i)=    0.0562; pl(i)=    0.0610; pu(i)=    0.2809
i= 17; avep(i)=    0.1955; stdp(i)=    0.0570; pl(i)=    0.0717; pu(i)=    0.2945
i= 18; avep(i)=    0.2089; stdp(i)=    0.0583; pl(i)=    0.0763; pu(i)=    0.2779
i= 19; avep(i)=    0.2072; stdp(i)=    0.0567; pl(i)=    0.0732; pu(i)=    0.2783
i= 20; avep(i)=    0.2067; stdp(i)=    0.0561; pl(i)=    0.0742; pu(i)=    0.2752
i= 21; avep(i)=    0.2043; stdp(i)=    0.0556; pl(i)=    0.0814; pu(i)=    0.2747
i= 22; avep(i)=    0.1895; stdp(i)=    0.0476; pl(i)=    0.0638; pu(i)=    0.2461
i= 23; avep(i)=    0.2210; stdp(i)=    0.0616; pl(i)=    0.0578; pu(i)=    0.2985
i= 24; avep(i)=    0.2174; stdp(i)=    0.0609; pl(i)=    0.0637; pu(i)=    0.3071
i= 25; avep(i)=    0.2161; stdp(i)=    0.0607; pl(i)=    0.0617; pu(i)=    0.2996
i= 26; avep(i)=    0.2002; stdp(i)=    0.0519; pl(i)=    0.0645; pu(i)=    0.2951
i= 27; avep(i)=    0.2026; stdp(i)=    0.0531; pl(i)=    0.0790; pu(i)=    0.2972
i= 28; avep(i)=    0.2016; stdp(i)=    0.0515; pl(i)=    0.0785; pu(i)=    0.2991
i= 29; avep(i)=    0.1977; stdp(i)=    0.0493; pl(i)=    0.0722; pu(i)=    0.2935
i= 30; avep(i)=    0.1921; stdp(i)=    0.0473; pl(i)=    0.0804; pu(i)=    0.2973
i= 31; avep(i)=    0.1845; stdp(i)=    0.0424; pl(i)=    0.0717; pu(i)=    0.2894
i= 32; avep(i)=    0.1845; stdp(i)=    0.0348; pl(i)=    0.0788; pu(i)=    0.2582
i= 33; avep(i)=    0.1723; stdp(i)=    0.0280; pl(i)=    0.0739; pu(i)=    0.2503




! Fudging scaling of standard deviations and constant values in each layer.
   stdp=0.5*stdp
   stdx=0.5*stdx
   stdz=0.5*stdz

   avep(1)    =0.25  ; avex(1)     =  500.0; avez(1)    = 050.0       ! LN 35
   avep(2:9)  =0.12  ; avex(1:9)   =  550.0; avez(1:9)  = 200.0       ! LN 21
   avep(10:12)=0.17  ; avex(10:12) =  800.0; avez(10:12)= 200.0       ! Etive
   avep(13:15)=0.12  ; avex(13:15) =  100.0; avez(13:15)= 050.0       ! Rannoch
   avep(16:17)=0.22  ; avex(16:17) = 1000.0; avez(16:17)= 100.0       ! TS
   avep(18:22)=0.23  ; avex(18:22) =  900.0; avez(18:22)= 250.0       ! Lobe 5b
   avep(23:26)=0.24  ; avex(23:26) =  900.0; avez(23:26)= 200.0       ! Lobe 5a
   avep(27:31)=0.22  ; avex(27:31) = 1100.0; avez(27:31)= 400.0       ! Lobe 4 
   avep(32:33)=0.18  ; avex(32:33) = 1000.0; avez(32:33)= 300.0       ! Lobe 3 


! PERMZ statistics
!   avez(:)=avex(:)*ratiozx(:)
!   stdz(:)=stdx(:)*ratiozx(:)

   if (logperm) then
      do k=1,nz
         aaa =log(avex(k)**2/sqrt(stdx(k)**2+avex(k)**2))
         bbb =sqrt(log(stdx(k)**2/avex(k)**2 + 1.0))
         avex(k)=aaa
         stdx(k)=bbb

         aaa =log(avez(k)**2/sqrt(stdz(k)**2+avez(k)**2))
         bbb =sqrt(log(stdz(k)**2/avez(k)**2 + 1.0))
         avez(k)=aaa
         stdz(k)=bbb
      enddo
      xl  =log(xl)
      xu  =log(xu)
   endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Printed imposed statistics
   print *,'czz: '
   print '(5f12.5)', czz(:)
   print *

   print *,'cpx: '
   print '(f12.5)', cpx
   print *

   print *,'avep: '
   print '(5f12.5)', avep(:)
   print *

   print *,'avex: '
   print '(5f12.5)', avex(:)
   print *

   print *,'stdp: '
   print '(5f12.5)', stdp(:)
   print *

   print *,'stdx: '
   print '(5f12.5)', stdx(:)
   print *

   print *,'pl: '
   print '(5f12.5)', pl(:)
   print *

   print *,'pu: '
   print '(5f12.5)', pu(:)
   print *

   print *,'xl: '
   print '(5f12.5)', xl(:)
   print *

   print *,'xu: '
   print '(5f12.5)', xu(:)
   print *

end subroutine
end module
