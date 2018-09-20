      mem4%pres,     &
      mem4%Sg,       &
      mem4%Sw,       &
      mem4%Rs,       &
#ifdef VAPOIL
      mem4%Rv,       &
#endif
      mem4%permx,    &
      mem4%permz,    &
      mem4%poro,     &
#ifdef MULTZ
      mem4%multz,    &
#endif
#ifdef MULTFLT
      mem4%multflt,  &
#endif
#ifdef EQUIL
      mem4%equilWOC, &
      mem4%equilGOC, &
#endif
#ifdef GAUSS2
      mem4%gauss1,   &
      mem4%gauss2,   &
#endif
#ifdef WELLSTATE
      mem4%XGRP,     &
      mem4%RSEG,     &
      mem4%XWEL,     &
      mem4%SCON,     &
      mem4%XCON,     &
#endif
      mem4%THP,      &
      mem4%BHP,      &
      mem4%OPR,      &
      mem4%GPR,      &
      mem4%WPR,      &
      mem4%WCT,      &
      mem4%GOR,      &
      mem4%OPT,      &
      mem4%GPT,      &
      mem4%WPT
