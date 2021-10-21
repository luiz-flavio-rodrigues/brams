
MODULE mod_chem_plumerise_scalar

  IMPLICIT NONE


  PRIVATE


  PUBLIC :: plumerise_driver   ! Subroutine



CONTAINS



  !----------------------------------------------------------------------------
  ! Plume rise model for vegetation fires (CPTEC/INPE 2005-2006)
  ! Refs.:
  ! Freitas, S. R., K. M. Longo, R. Chatfield, D. Latham, M. A. F. Silva Dias,
  !  M. O. Andreae, 
  ! E. Prins, J. C. Santos, R. Gielow and J. A. Carvalho Jr.: Including the 
  ! sub-grid scale 
  ! plume rise of vegetation fires in low resolution atmospheric transport 
  ! models. 
  !  Atmospheric Chemistry and Physics and discussion, 6, 2006.
  !-
  ! Freitas, S. R.; Longo, K. M.; M. Andreae. Impact of including the plume
  ! rise of vegetation 
  ! fires in numerical simulations of associated atmospheric pollutants. 
  ! Geophys. Res. Lett., 
  ! 33, L17808, doi:10.1029/2006GL026608, 2006. 
  !----------------------------------------------------------------------------

  SUBROUTINE plumerise_driver(mzp,mxp,myp,ia,iz,ja,jz,                        &
                              srctime1,chem_nspecies,spc_chem_alloc,src,      &
                              on,off,nmodes,aer_nspecies,spc_aer_alloc,       &
                              aer_bburn,nsrc,bburn,nveg_agreg,tropical_forest,&
                              boreal_forest,savannah,grassland,nzpmax,dtlt,   &
                              time,zt_rams,zm_rams,dzt_rams,g,cp,cpor,p00,    &
                              rgas,theta,pp,pi0,rv,up,vp,rtgt,lpw,pr_time,    &
                              aerosol,chem1_src_g,aer1_g,plume_mean_g,spc_aer_name,&
                              plume_fre_g,                                    &
                              plumerise_flag                                  )
  USE mem_chem1, ONLY:  &
       chem1_src_vars      ! Type
  USE mem_aer1, ONLY:   &
       aer1_vars           ! Type
  USE mem_plume_chem1, ONLY: &
                             plume_mean_vars,  &     ! Type
                             plume_fre_vars ,  &     ! Type
                             iflam_frac     ,  &
                             imean_frp      ,  &
                             istd_frp       ,  &
                             imean_size     ,  &
                             istd_size  


    INTEGER,                INTENT(IN)    :: mzp
    INTEGER,                INTENT(IN)    :: mxp
    INTEGER,                INTENT(IN)    :: myp
    INTEGER,                INTENT(IN)    :: ia
    INTEGER,                INTENT(IN)    :: iz
    INTEGER,                INTENT(IN)    :: ja
    INTEGER,                INTENT(IN)    :: jz
    INTEGER,                INTENT(IN)    :: plumerise_flag
    ! grid_dims
    INTEGER ,               INTENT(IN)    :: nzpmax
    ! mem_basic
    REAL,                   INTENT(IN)    :: theta(:,:,:)
    REAL,                   INTENT(IN)    :: pp   (:,:,:)
    REAL,                   INTENT(IN)    :: pi0  (:,:,:)
    REAL,                   INTENT(IN)    :: rv   (:,:,:)
    REAL,                   INTENT(IN)    :: up   (:,:,:)!srf-AWE
    REAL,                   INTENT(IN)    :: vp   (:,:,:)!srf-AWE
    ! mem_grid
    REAL,                   INTENT(IN)    :: rtgt(:,:)
    INTEGER,                INTENT(IN)    :: lpw (:,:)
    REAL,                   INTENT(IN)    :: dtlt
    REAL,                   INTENT(IN)    :: time

    REAL,                   INTENT(IN)    :: zt_rams (:)
    REAL,                   INTENT(IN)    :: zm_rams (:)
    REAL,                   INTENT(IN)    :: dzt_rams(:)
    ! chem_sources
    REAL,                   INTENT(IN)    :: pr_time
    REAL,                   INTENT(IN)    :: srctime1
    ! chem1_list
    INTEGER,                INTENT(IN)    :: chem_nspecies
    INTEGER,                INTENT(IN)    :: spc_chem_alloc(:,:)
    INTEGER,                INTENT(IN)    :: src
    INTEGER,                INTENT(IN)    :: on
    INTEGER,                INTENT(IN)    :: off
    ! aer1_list
    INTEGER,                INTENT(IN)    :: nmodes
    INTEGER,                INTENT(IN)    :: aer_nspecies
    INTEGER,                INTENT(IN)    :: spc_aer_alloc(:,:,:)
    INTEGER,                INTENT(IN)    :: aer_bburn
    ! mem_chem1
    INTEGER,                INTENT(IN)    :: nsrc
    TYPE (chem1_src_vars),  INTENT(INOUT) :: chem1_src_g(:,:,:)
    INTEGER,                INTENT(IN)    :: bburn
    ! mem_aer1
    TYPE (aer1_vars),       INTENT(INOUT) :: aer1_g(:,:)
    INTEGER,                INTENT(IN)    :: aerosol
    ! mem_plume_chem1
    TYPE (plume_mean_vars), INTENT(IN)    :: plume_mean_g(:)
    TYPE (plume_fre_vars ), INTENT(INOUT) :: plume_fre_g (5)
    INTEGER,                INTENT(IN)    :: nveg_agreg
    INTEGER,                INTENT(IN)    :: tropical_forest
    INTEGER,                INTENT(IN)    :: boreal_forest
    INTEGER,                INTENT(IN)    :: savannah
    INTEGER,                INTENT(IN)    :: grassland
    ! rconstants
    REAL,                   INTENT(IN)    :: g
    REAL,                   INTENT(IN)    :: cp
    REAL,                   INTENT(IN)    :: cpor
    REAL,                   INTENT(IN)    :: p00
    REAL,                   INTENT(IN)    :: rgas
    CHARACTER(LEN=8) , INTENT(IN) :: spc_aer_name(nmodes,aer_nspecies)

    LOGICAL, PARAMETER :: IS2PRINT = .FALSE. !.TRUE.
    CHARACTER(LEN=8)   :: ctime
    CHARACTER(LEN=4)   :: cmynum
    INTEGER            :: nrec

    CHARACTER(LEN=*), PARAMETER :: h="**(plumerise_driver)**"
    CHARACTER(LEN=8) :: c0
    !TO
    INTEGER, PARAMETER :: nkp       = 200
    INTEGER, PARAMETER :: ntime     = 200
    INTEGER            :: n_setgrid = 0

    REAL :: dz
    REAL :: zt (nkp)
    REAL :: zm (nkp)
    REAL :: dzt(nkp)
    REAL :: dzm(nkp)

    IF(MOD(time+.001,pr_time).LE.(dtlt).OR.time.LT..01 .OR. &
       ABS(time-srctime1).LT. 1.e-5 )THEN

!!$       WRITE(c0,"(f8.1)") time
!!$       CALL MsgOne(h,' -----------------------------------------------------')
!!$       CALL MsgOne(h,' plumerise tendencies updated')
!!$       CALL MsgOne(h,' time = '//TRIM(ADJUSTL(c0))//' seconds')
!!$       CALL MsgOne(h,' -----------------------------------------------------')      
       CALL plumerise(mzp,mxp,myp,ia,iz,ja,jz, &
                      theta,pp,pi0,rv,up,vp,rtgt,lpw,zt_rams,zm_rams, &
                      nzpmax,dzt_rams,chem_nspecies,spc_chem_alloc,   &
                      src,on,off,nmodes,aer_nspecies,spc_aer_alloc,   &
                      aer_bburn,nsrc,chem1_src_g,bburn,aer1_g,aerosol,&
                      plume_mean_g,nveg_agreg,tropical_forest,        &
                      boreal_forest,savannah,grassland,g,cp,cpor,     &
                      p00,rgas,spc_aer_name,                          &          
                      plume_fre_g,                                    &
                      plumerise_flag,nkp,ntime,n_setgrid,dz,dzm,dzt,zm,zt       )
    ENDIF

  END SUBROUTINE plumerise_driver
  !-------------------------------------------------------------------------


  SUBROUTINE plumerise(m1,m2,m3,ia,iz,ja,jz, &
                       theta,pp,pi0,rv,up,vp,rtgt,lpw,zt_rams,zm_rams,     &
                       nzpmax,dzt_rams,chem_nspecies,spc_chem_alloc,src,   &
                       on,off,nmodes,aer_nspecies,spc_aer_alloc,aer_bburn, &
                       nsrc,chem1_src_g,bburn,aer1_g,aerosol,plume_mean_g, &
                       nveg_agreg,tropical_forest,boreal_forest,savannah,  &
                       grassland,g,cp,cpor,p00,rgas,spc_aer_name,          &         
                       plume_fre_g,                                        &
                       plumerise_flag,nkp,ntime,n_setgrid,dz,dzm,dzt,zm,zt )
  USE mem_chem1, ONLY:  &
       chem1_src_vars      ! Type
  USE mem_aer1, ONLY:   &
       aer1_vars           ! Type
  USE mem_plume_chem1, ONLY: &
                             plume_mean_vars,  &     ! Type
                             plume_fre_vars ,  &     ! Type
                             iflam_frac     ,  &
                             imean_frp      ,  &
                             istd_frp       ,  &
                             imean_size     ,  &
                             istd_size  

    INTEGER, INTENT(IN)      :: nkp
    INTEGER, INTENT(IN)      :: ntime
    INTEGER, INTENT(INOUT)      :: n_setgrid
    REAL, INTENT(INOUT) :: dz
    REAL, INTENT(INOUT) :: dzm(nkp)
    REAL, INTENT(INOUT) :: dzt(nkp)
    REAL, INTENT(INOUT) :: zt(nkp)
    REAL, INTENT(INOUT) :: zm(nkp)
    INTEGER,               INTENT(IN)     :: m1
    INTEGER,               INTENT(IN)     :: m2
    INTEGER,               INTENT(IN)     :: m3
    INTEGER,               INTENT(IN)     :: ia
    INTEGER,               INTENT(IN)     :: iz
    INTEGER,               INTENT(IN)     :: ja
    INTEGER,               INTENT(IN)     :: jz
    INTEGER,               INTENT(IN)     :: plumerise_flag
    REAL,                  INTENT(IN)     :: theta(:,:,:)
    REAL,                  INTENT(IN)     :: pp   (:,:,:)
    REAL,                  INTENT(IN)     :: pi0  (:,:,:)
    REAL,                  INTENT(IN)     :: rv   (:,:,:)
    REAL,                  INTENT(IN)     :: up   (:,:,:)!srf-AWE
    REAL,                  INTENT(IN)     :: vp   (:,:,:)!srf-AWE
    REAL,                  INTENT(IN)     :: rtgt(:,:)
    INTEGER,               INTENT(IN)     :: lpw (:,:)
    REAL,                  INTENT(IN)     :: zt_rams(:)
    REAL,                  INTENT(IN)     :: zm_rams(:)
    ! grid_dims
    INTEGER,               INTENT(IN)     :: nzpmax
    ! mem_grid
    REAL,                  INTENT(IN)     :: dzt_rams(:)
    ! chem1_list
    INTEGER,               INTENT(IN)     :: chem_nspecies
    INTEGER,               INTENT(IN)     :: spc_chem_alloc(:,:)
    INTEGER,               INTENT(IN)     :: src
    INTEGER,               INTENT(IN)     :: on
    INTEGER,               INTENT(IN)     :: off
    ! aer1_list
    INTEGER,               INTENT(IN)     :: nmodes
    INTEGER,               INTENT(IN)     :: aer_nspecies
    INTEGER,               INTENT(IN)     :: spc_aer_alloc(:,:,:)
    INTEGER,               INTENT(IN)     :: aer_bburn
    ! mem_chem1
    INTEGER,               INTENT(IN)     :: nsrc
    TYPE (chem1_src_vars), INTENT(INOUT)  :: chem1_src_g(:,:,:)
    INTEGER,               INTENT(IN)     :: bburn

    ! mem_aer1
    TYPE (aer1_vars),       INTENT(INOUT) :: aer1_g(:,:)
    INTEGER,                INTENT(IN)    :: aerosol
    ! mem_plume_chem1
    TYPE (plume_mean_vars), INTENT(IN)    :: plume_mean_g(:)
    TYPE (plume_fre_vars) , INTENT(INOUT) :: plume_fre_g(5)
    INTEGER,                INTENT(IN)    :: nveg_agreg
    INTEGER,                INTENT(IN)    :: tropical_forest
    INTEGER,                INTENT(IN)    :: boreal_forest
    INTEGER,                INTENT(IN)    :: savannah
    INTEGER,                INTENT(IN)    :: grassland
    ! rconstants
    REAL,                   INTENT(IN)    :: g
    REAL,                   INTENT(IN)    :: cp
    REAL,                   INTENT(IN)    :: cpor
    REAL,                   INTENT(IN)    :: p00
    REAL,                   INTENT(IN)    :: rgas

    CHARACTER(LEN=8) , INTENT(IN) :: spc_aer_name(nmodes,aer_nspecies)

    INTEGER            :: i, j, k, ixx, iveg_ag, imm, k1, k2, ispc, imode, iloop
    INTEGER            :: kmt
    REAL               :: burnt_area, STD_burnt_area, dz_flam, dzi, FRP, convert_smold_to_flam
    REAL               :: ztopmax(2)
    REAL               :: W_VMD(nkp,2), VMD(nkp,2)
    REAL               :: q_smold_kgm2
    INTEGER            :: it1, it2
    INTEGER, PARAMETER :: use_last = 0
    INTEGER, PARAMETER :: izprint  = 0 ! if = 0 => no printout
    INTEGER, PARAMETER :: wind_eff = 1  

    REAL    :: area           
    INTEGER :: maxtime        
    REAL    :: alpha          
    REAL    :: rsurf          
    REAL    :: fmoist         
    REAL    :: tdur           
    REAL    :: zsurf          
    REAL    :: heating(ntime) 
    REAL    :: thtcon(nkp)    
    REAL    :: rvcon (nkp)    
    REAL    :: picon (nkp)    
    REAL    :: zcon  (nkp)    
    REAL    :: zzcon (nkp)    
    REAL    :: qv    (nkp)    
    REAL    :: qh    (nkp)    
    REAL    :: qi    (nkp)    
    REAL    :: qc    (nkp)    
    REAL    :: txs   (nkp)    
    REAL    :: cvi   (nkp)    
    REAL    :: vth   (nkp)    
    REAL    :: w     (nkp)    
    REAL    :: t     (nkp)    
    REAL    :: qsat  (nkp)    
    REAL    :: rho   (nkp)    
    REAL    :: radius(nkp)    
    REAL    :: visc  (nkp)    
    REAL    :: wc    (nkp)    
    REAL    :: wt    (nkp)    
    REAL    :: est   (nkp)    
    REAL    :: pe    (nkp)    
    REAL    :: te    (nkp)    
    REAL    :: tt    (nkp)    
    REAL    :: qvenv (nkp)    
    REAL    :: cvh   (nkp)    
    REAL    :: vti   (nkp)    
    REAL    :: pke   (nkp)    
    REAL    :: the   (nkp)    
    REAL    :: thve  (nkp)    
    REAL    :: dne   (nkp)    
    REAL    :: sc    (nkp)     
    REAL    :: sct   (nkp)     
    REAL    :: ucon  (nkp)    
    REAL    :: vcon  (nkp)    
    REAL    :: upe   (nkp)
    REAL    :: vpe   (nkp)
    REAL    :: vel_e (nkp)   
    REAL    :: vel_p (nkp)
    REAL    :: rad_p (nkp)
    REAL    :: vel_t (nkp)
    REAL    :: rad_t (nkp)   
    INTEGER omp_get_num_threads,omp_get_thread_num

    !- for biomass burn, only one memory allocation (only one time level)
    it1=1

    !- zera o termo fonte associado `as emissoes com plumerise (k>2)	
    !- chemistry section
    DO ispc = 1,chem_nspecies
       IF(spc_chem_alloc(src,ispc) == off) CYCLE
       chem1_src_g(it1,bburn,ispc)%sc_src(3:m1,:,:) = 0.
    ENDDO

    !- aerosol section
    IF(aerosol == 1) THEN 
       DO imode=1,nmodes
          !- only for biomass burning aerosols
          IF(spc_aer_alloc(src,imode,aer_bburn)== off ) CYCLE    
          aer1_g(imode,aer_bburn)%sc_src(3:m1,:,:) = 0.
       ENDDO
    ELSEIF(aerosol == 2) THEN ! for MATRIX and only for boc_ocar and boc_bcar species

       DO ispc = 1,aer_nspecies
         DO imode=1,nmodes
          IF(spc_aer_alloc(src,imode,ispc) == off ) CYCLE

          !-only for bburn aerosols) 
          IF(spc_aer_name(imode,ispc)=="boc_bcar" .or. &
            spc_aer_name(imode,ispc)=="boc_ocar" ) THEN
            aer1_g(imode,ispc)%sc_src(3:m1,:,:) = 0.
          ENDIF
         ENDDO
       ENDDO  
    ENDIF

    DO j = ja,jz
       DO i = ia,iz
          convert_smold_to_flam=0.0

          !- if the max value of flaming is close to zero => there is not
          !  emission with plume rise => cycle
          IF(PLUMERISE_flag == 1 ) THEN 
             IF(plume_mean_g(tropical_forest)%flam_frac(i,j) + &
               plume_mean_g(boreal_forest  )%flam_frac(i,j) + &
               plume_mean_g(savannah       )%flam_frac(i,j) + &
               plume_mean_g(grassland      )%flam_frac(i,j) < 1.e-6 ) CYCLE
            !- loop over the four types of aggregate biomes with fires
               iloop = nveg_agreg
       
             ELSEIF(PLUMERISE_flag == 2 ) THEN
                IF(plume_fre_g(iflam_frac)%pvar(i,j) < 1.e-6 ) CYCLE
                        iloop = 1
  
             ENDIF

          DO k = 1,m1
             ucon  (k)=up(k,i,j)                ! u wind (m/s)
             vcon  (k)=vp(k,i,j)                ! v wind (m/s)
            !wcon  (k)=wp(k,i,j)		! w wind (m/s)
             thtcon(k)=theta(k,i,j)             ! pot temperature (K)
             picon (k)=(pp(k,i,j)+pi0(k,i,j))   ! exner function
            !tmpcon(k)=thtcon(k)*picon(k)/cp    ! temperature (K)
            !dncon (k)=dn0(k,i,j)		! dry air density (basic state) (kg/m3)
            !prcon (k)=(picon(k)/cp)**cpor*p00  ! pressure (Pa)
             rvcon (k)=rv(k,i,j)                ! water vapor mixing ratio (kg/kg)

             zcon  (k)=zt_rams(k) *rtgt(i,j)    ! termod-point height (m)
             zzcon (k)=zm_rams(k) *rtgt(i,j)    ! W-point height      (m)

             zsurf = 0.
          ENDDO ! end do k

          !- get envinronmental state (temp, water vapor mix ratio, ...)
          CALL get_env_condition(lpw(i,j),m1-1,kmt,qvenv,pke,the,te,pe,thve,dne,upe,vpe,vel_e, &
                                 zcon,zt,thtcon,rvcon,picon,ucon,vcon,zm,dzm,dzt,dz,zsurf,     &
                                 wind_eff,g,cp,cpor,p00,rgas,nkp,n_setgrid)

          !- only for testing (using an external sounding)
          !CALL get_env_condition_sound(lpw(i,j),m1-1,kmt,qvenv,pke,the,te,pe,thve,dne,upe,vpe,vel_e &
          !			 ,zcon,zt,thtcon,rvcon,picon,ucon,vcon,zm,dzm,dzt,dz,zsurf,n_setgrid,wind_eff)


          !- loop over the four types of aggregate biomes with fires
          DO iveg_ag=1,iloop 

             !- verifica a existencia de emissao flaming para um bioma especifico
             IF(PLUMERISE_flag == 1 ) THEN 
               IF(plume_mean_g(iveg_ag)%flam_frac(i,j) < 1.e-6 ) CYCLE 
       
               !-burnt area and standard deviation
               !burnt_area   = 50.*1e4           !m^2, only for testing with a sounding
                burnt_area   = plume_mean_g(iveg_ag)%fire_size(i,j)              
               
               STD_burnt_area= 0.!not em use
       
               !-number to calculate the flaming emission from the amount emitted
               !-during the smoldering phase
               convert_smold_to_flam=plume_mean_g(iveg_ag)%flam_frac(i,j)
             
             ELSEIF(PLUMERISE_flag == 2 ) THEN 
               !-number to calculate the emission during the flaming pahse 
               !-from the amount emitted during the smoldering phase

               convert_smold_to_flam=plume_fre_g(iflam_frac)%pvar(i,j)
               !
               !- check if there is only one fire in a given grid box (=> std =0.)
               if(plume_fre_g(istd_frp )%pvar(i,j) < 1.0e-6) then
       
               !- if yes, we will set it as a 20% of the mean frp as a gross estimation
               !- of the retrieval uncertainty by the sensors.
               !- (we are not taking care about the fire size retrieval)
               !print*,"xx=",I,J,plume_fre_g(istd_frp )%pvar(i,j),0.2*plume_fre_g(imean_frp )%pvar(i,j)
                  plume_fre_g(istd_frp )%pvar(i,j)=0.2*plume_fre_g(imean_frp )%pvar(i,j)
               endif 
             ENDIF

             !- loop over the minimum and maximum heat fluxes/FRP
             DO imm=1,2
                !--------------------
                !ixx=iveg_ag*10 + imm
                ixx=0+imm
                !--------------------             
                
		IF(PLUMERISE_flag == 2 ) THEN
		  if(imm==1 ) then
                    !for imm = 1 => lower injection height
                    burnt_area = max(1.0e4,plume_fre_g(imean_size)%pvar(i,j) - 0.5*plume_fre_g(istd_size)%pvar(i,j))
		    FRP        = max(1000.,plume_fre_g(imean_frp )%pvar(i,j) - 0.5*plume_fre_g(istd_frp )%pvar(i,j))
                 
		  elseif(imm==2 ) then
                    !for imm = 2 => higher injection height
                    burnt_area = max(1.0e4,plume_fre_g(imean_size)%pvar(i,j) + 0.5*plume_fre_g(istd_size)%pvar(i,j))
		    FRP        = max(1000.,plume_fre_g(imean_frp )%pvar(i,j) + 0.5*plume_fre_g(istd_frp )%pvar(i,j))
                  endif
		ENDIF
		! print*,"imm",imm,plume_fre_g(imean_size)%pvar(i,j), plume_fre_g(istd_size)%pvar(i,j),&
		!                    plume_fre_g(imean_frp )%pvar(i,j), plume_fre_g(istd_frp )%pvar(i,j)
                !
		!-just for testing...
		!burnt_area =  burnt_area*30.
                !FRP	    =  FRP*600.    
		!-----------
		!print*,"size(ha)/frp=",i,j,imm, burnt_area/1.e+4, 0.88*1000.*FRP/(burnt_area) ; call flush(6)
        	!write(*,100)"size(ha)/frp(kW/m2)",i,j,imm, burnt_area/1.e+4, 0.88*1.e-3*FRP/(burnt_area) 
                !100 format (1x,A20,1x,3I4,1x,2F15.4)
		    
                !- get fire properties (burned area, plume radius, heating rates ...)
                CALL get_fire_properties(imm,iveg_ag,burnt_area,use_last, &
                                         heating,area,maxtime,alpha,      &
                                         rsurf,fmoist,tdur,               &
					 plumerise_flag,FRP,ntime)

                !------  generates the plume rise    ------

		IF(PLUMERISE_flag == 1 ) THEN
                  !-- only one value for eflux of GRASSLAND
                  IF(iveg_ag == GRASSLAND .AND. imm == 2) THEN 
                     ztopmax(2)=ztopmax(1)
                     ztopmax(1)=zzcon(1)
                     CYCLE
                  ENDIF
                ENDIF

                CALL makeplume(i,j,kmt,ztopmax(imm),ixx,imm,use_last,w,           &
                               t,pe,qsat,est,rho,zt,te,txs,wc,dz,maxtime,zm,  &
                               wt,dzm,tt,qv,qh,qi,qc,qvenv,radius,alpha,visc, &
                               cvi,vth,cvh,vti,dzt,sc,area,rsurf,tdur,heating,&
                               fmoist,vel_e,vel_p,rad_p,vel_t,rad_t,W_VMD,    &
                               izprint,nkp,ntime)     	      

             ENDDO ! enddo of the loop imm
             
             !- defines the vertical layer where the flaming phase emission will
             !  be released in the 3d atmospheric model
             CALL set_flam_vert(ztopmax,k1,k2,zzcon,W_VMD,VMD,nkp)

             !- thickness of the vertical layer
             dz_flam=zzcon(k2)-zzcon(k1-1)

             !print*,"   z1/z2=",k1,k2,zzcon(k1-1),zzcon(k2)!,convert_smold_to_flam ; call flush(6)
             !print*,"=================================================" ; call flush(6)

             !- emission during flamming phase is evenly distributed between levels k1 and k2 
             DO k=k1,k2
                !use this in case the emission src is already in mixing ratio
                !rhodzi= 1./(dn0(k,i,j) * dz_flam)
                
		!use this in case the emission src is tracer density
                dzi= 1./( dz_flam)

                !- chemistry section 
                DO ispc = 1, chem_nspecies
                   IF(spc_chem_alloc(src,ispc) == off ) CYCLE

                   !- get back the smoldering emission in kg/m2  (actually in
                   !   1e-9 kg/m2) 

                   !use this in case the emission src is already in mixing 
                   ! ratio
                   !q_smold_kgm2 = (rtgt(i,j)/dzt_rams(2) *  dn0(2,i,j)	)*   &
                   ! 	         chem1_src_g(it1,bburn,ispc,ng)%sc_src(2,i,j)

                   !use this in case the emission src is tracer density
                   q_smold_kgm2 = (rtgt(i,j)/dzt_rams(2)                   )*   &
                                   chem1_src_g(it1,bburn,ispc)%sc_src(2,i,j)	 

                   ! units = already in ppbm,  don't need "fcu" factor 
                   chem1_src_g(it1,bburn,ispc)%sc_src(k,i,j) = chem1_src_g(it1,bburn,ispc)%sc_src(k,i,j) +&
                                                               convert_smold_to_flam                     *&
                                                              !plume_mean_g(iveg_ag)%flam_frac(i,j)      *&
                                                               q_smold_kgm2                              *&
                                                               dzi    !use this in case the emission src is tracer density
                                                              !rhodzi !use this in case the emission src is already in mixing ratio
                   
                ENDDO
                !- aerosol section ( only for biomass burning aerosols )
                IF(aerosol == 1) THEN 

                   DO imode=1,nmodes

                      IF(spc_aer_alloc(src,imode,aer_bburn)== off ) CYCLE    

                      !- get back the smoldering emission in kg/m2  
                      !  (actually in 1e-9 kg/m2) 

                      !use this in case the emission src is already in 
                      !mixing ratio
                      !q_smold_kgm2 = (rtgt(i,j)/dzt_rams(2) * dn0(2,i,j) )*  &
                      !	               aer1_g(imode,aer_bburn)%sc_src(2,i,j)    

                      !use this in case the emission src is tracer density

                      q_smold_kgm2 = (rtgt(i,j)/dzt_rams(2)                 )*   &
                                      aer1_g(imode,aer_bburn)%sc_src(2,i,j)	 


                      ! units = already in ppbm,  don't need "fcu" factor 

                      aer1_g(imode,aer_bburn)%sc_src(k,i,j) = aer1_g(imode,aer_bburn)%sc_src(k,i,j) +&
                                                              convert_smold_to_flam                 *&
                                                             !plume_mean_g(iveg_ag)%flam_frac(i,j)  *&
                                                              q_smold_kgm2                          *&
                                                              dzi    !use this in case the emission src is tracer density
                                                             !rhodzi !use this in case the emission src is already in mixing ratio

                   ENDDO
                
		ELSEIF(aerosol == 2) THEN ! for MATRIX and only for boc_ocar and boc_bcar species
		
		   DO ispc = 1,aer_nspecies
	              DO imode=1,nmodes
                       IF(spc_aer_alloc(src,imode,ispc) == off ) CYCLE

		       !-only for bburn aerosols - MATRIX MECH=1 
	               IF(spc_aer_name(imode,ispc)=="boc_bcar" .or. &
	                  spc_aer_name(imode,ispc)=="boc_ocar" )    THEN
                        
                          q_smold_kgm2 = (rtgt(i,j)/dzt_rams(2))*   &
                                         aer1_g(imode,ispc)%sc_src(2,i,j)	 
                          
			  ! units = already in ppbm,  don't need "fcu" factor 
                          aer1_g(imode,ispc)%sc_src(k,i,j)= aer1_g(imode,ispc)%sc_src(k,i,j)     +&
                                                           !plume_mean_g(iveg_ag)%flam_frac(i,j) *&
                                                            convert_smold_to_flam                *&
                                                            q_smold_kgm2 * &
                                                            dzi     !use this in case the emission src is tracer density
                                                            !rhodzi !use this in case the emission src is already in mixing ratio
 
		       ENDIF
                      ENDDO
                   ENDDO
		ENDIF

             ENDDO

          ENDDO ! enddo do loop em iveg_ag
       ENDDO  ! loop em i
    ENDDO   ! loop em j
  END SUBROUTINE plumerise
  !-------------------------------------------------------------------------

  SUBROUTINE get_env_condition(k1,k2,kmt,qvenv,pke,the,te,pe,thve,dne,   &
                               upe,vpe,vel_e,zcon,zt,thtcon,rvcon,picon, &
                               ucon,vcon,zm,dzm,dzt,dz,zsurf,            &! n_setgrid,    &
                               wind_eff,g,cp,cpor,p00,rgas,nkp,n_setgrid)

    INTEGER, INTENT(IN)    :: nkp
    INTEGER, INTENT(INOUT)    :: n_setgrid
    INTEGER, INTENT(IN)    :: k1
    INTEGER, INTENT(IN)    :: k2
    INTEGER, INTENT(INOUT) :: kmt 
    INTEGER, INTENT(IN)    :: wind_eff
    ! plumegen_coms
    REAL,    INTENT(INOUT) :: dz
    REAL,    INTENT(IN)    :: zsurf
!   INTEGER, INTENT(INOUT) :: n_setgrid
!!$    REAL,    INTENT(INOUT) :: qvenv (nkp)
!!$    REAL,    INTENT(INOUT) :: pke   (nkp)
!!$    REAL,    INTENT(INOUT) :: the   (nkp)
!!$    REAL,    INTENT(INOUT) :: te    (nkp)
!!$    REAL,    INTENT(INOUT) :: pe    (nkp)
!!$    REAL,    INTENT(INOUT) :: thve  (nkp)
!!$    REAL,    INTENT(INOUT) :: dne   (nkp)  
!!$    REAL,    INTENT(INOUT) :: upe   (nkp)
!!$    REAL,    INTENT(INOUT) :: vpe   (nkp)
!!$    REAL,    INTENT(INOUT) :: vel_e (nkp)
    REAL,    INTENT(INOUT) :: zcon  (nkp)
!!$    REAL,    INTENT(INOUT) :: zt    (nkp)
!!$    REAL,    INTENT(INOUT) :: thtcon(nkp)
!!$    REAL,    INTENT(INOUT) :: rvcon (nkp)
!!$    REAL,    INTENT(INOUT) :: picon (nkp)
!!$    REAL,    INTENT(INOUT) :: ucon  (nkp)
!!$    REAL,    INTENT(INOUT) :: vcon  (nkp)
!!$    REAL,    INTENT(INOUT) :: zm    (nkp)
!!$    REAL,    INTENT(INOUT) :: dzm   (nkp)
!!$    REAL,    INTENT(INOUT) :: dzt   (nkp)
    REAL,    INTENT(INOUT) :: qvenv (:)
    REAL,    INTENT(INOUT) :: pke   (:)
    REAL,    INTENT(INOUT) :: the   (:)
    REAL,    INTENT(INOUT) :: te    (:)
    REAL,    INTENT(INOUT) :: pe    (:)
    REAL,    INTENT(INOUT) :: thve  (:)
    REAL,    INTENT(INOUT) :: dne   (:)  
    REAL,    INTENT(INOUT) :: upe   (:)
    REAL,    INTENT(INOUT) :: vpe   (:)
    REAL,    INTENT(INOUT) :: vel_e (:)
!!$    REAL,    INTENT(INOUT) :: zcon  (:)
    REAL,    INTENT(INOUT) :: zt    (:)
    REAL,    INTENT(INOUT) :: thtcon(:)
    REAL,    INTENT(INOUT) :: rvcon (:)
    REAL,    INTENT(INOUT) :: picon (:)
    REAL,    INTENT(INOUT) :: ucon  (:)
    REAL,    INTENT(INOUT) :: vcon  (:)
    REAL,    INTENT(INOUT) :: zm    (:)
    REAL,    INTENT(INOUT) :: dzm   (:)
    REAL,    INTENT(INOUT) :: dzt   (:)
    ! rconstants
    REAL,    INTENT(IN)    :: g
    REAL,    INTENT(IN)    :: cp
    REAL,    INTENT(IN)    :: cpor
    REAL,    INTENT(IN)    :: p00
    REAL,    INTENT(IN)    :: rgas


    CHARACTER(LEN=*), PARAMETER :: h="**(get_env_condition)**"

    INTEGER :: k,nk
    REAL    :: znz
    LOGICAL :: found
    
    IF( n_setgrid == 0) THEN
       n_setgrid = 1
       ! define vertical grid of plume model
       CALL set_grid(zt,zm,dzm,dzt,dz,zsurf,nkp) 
       ! zt(k) =  thermo and water levels
       ! zm(k) =  dynamical levels 
    ENDIF
    znz=zcon(k2)
    
    
    found = .FALSE.
    DO k=nkp,1,-1
       IF(zt(k).LT.znz) THEN
          found = .TRUE.
          exit
       END IF
    ENDDO
    
    IF (.not. found) THEN
       STOP ' ERROR: Envir stop 12 - chem_plumerise_scalar'
!!$       CALL FatalError(h//" Envir stop 12")
    END IF

!--(DMK-BRAMS-5-INI)------------------------------------------------------
    kmt=min(k,nkp-1)
!--(DMK-BRAMS-5-OLD)------------------------------------------------------
!    kmt=k
!--(DMK-BRAMS-5-FIM)------------------------------------------------------

    nk=k2-k1+1

    IF (nk > nkp) THEN
       STOP ' ERROR: nk > nkp - chem_plumerise_scalar'
    END IF

!!$    CALL htint(nk,  ucon,zcon(k1:nk+k1-1),kmt,upe  ,zt)
!!$    CALL htint(nk,  vcon,zcon(k1:nk+k1-1),kmt,vpe  ,zt)
!!$    CALL htint(nk,thtcon,zcon(k1:nk+k1-1),kmt,the  ,zt)
!!$    CALL htint(nk, rvcon,zcon(k1:nk+k1-1),kmt,qvenv,zt)
    CALL htint(nk,  ucon,zcon,kmt,upe  ,zt)
    CALL htint(nk,  vcon,zcon,kmt,vpe  ,zt)
    CALL htint(nk,thtcon,zcon,kmt,the  ,zt)
    CALL htint(nk, rvcon,zcon,kmt,qvenv,zt)
    DO k=1,kmt
       qvenv(k)=MAX(qvenv(k),1e-8)
    ENDDO

    pke(1)=picon(1)
    DO k=1,kmt
       thve(k)=the(k)*(1.+.61*qvenv(k)) ! virtual pot temperature
    ENDDO
    DO k=2,kmt
       pke(k)=pke(k-1)-g*2.*(zt(k)-zt(k-1))  & ! exner function
            /(thve(k)+thve(k-1))
    ENDDO
    DO k=1,kmt
       te(k)  = the(k)*pke(k)/cp         ! temperature (K) 
       pe(k)  = (pke(k)/cp)**cpor*p00    ! pressure (Pa)
       dne(k)= pe(k)/(rgas*te(k)*(1.+.61*qvenv(k))) !  dry air density (kg/m3)
       
       vel_e(k) = SQRT(upe(k)**2+vpe(k)**2)         !-env wind (m/s)
       !print*,'k,vel_e(k),te(k)=',vel_e(k),te(k)
    ENDDO
    
    !-ewe - env wind effect
    IF(wind_eff < 1)  vel_e(1:kmt) = 0.
    

    !-use este para gerar o RAMS.out
    ! ------- print environment state
    !print*,'k,zt(k),pe(k),te(k)-273.15,qvenv(k)*1000'
    !do k=1,kmt
    ! write(*,100)  k,zt(k),pe(k),te(k)-273.15,qvenv(k)*1000.
    !enddo
    !stop 333


    !--------- nao eh necessario este calculo
    !do k=1,kmt
    !  call thetae(pe(k),te(k),qvenv(k),thee(k))
    !enddo


    !--------- converte press de Pa para kPa para uso modelo de plumerise

    DO k=1,kmt
       pe(k) = pe(k)*1.e-3
    ENDDO

    RETURN 


    !para testes    ----------------------
    !
    !      open(13,file='prn.out')
    !      do 112, i=1,nkp
    !!  112 read (13,1000) zt(i),pe(i),te(i),rhe(i),dne(i),qvenv(i)
    !  112 read (13,1000) dummy,pe(i),te(i),rhe(i),dne(i),qvenv(i)
    !1000 format(1x,6F15.4)
    !      close(13)
    !
    !te(:)=te(:)+273.15
    !--------------------------------------

  END SUBROUTINE get_env_condition
  !-------------------------------------------------------------------------

  SUBROUTINE set_grid(zt,zm,dzm,dzt,dz,zsurf,nkp)

    ! plumegen_coms
    REAL, INTENT(INOUT) :: dz ! (DMK) alterado (OUT) para (INOUT)
    REAL, INTENT(IN)    :: zsurf
    REAL, INTENT(INOUT) :: zt (:)
    REAL, INTENT(INOUT) :: zm (:)
    REAL, INTENT(INOUT) :: dzm(:)
    REAL, INTENT(INOUT) :: dzt(:)
    INTEGER, INTENT(IN) :: nkp
    INTEGER :: k, mzp

    dz=100. ! set constant grid spacing of plume grid model(meters)

    mzp=nkp
    zt(1) = zsurf
    zm(1) = zsurf
    zt(2) = zt(1) + 0.5*dz
    zm(2) = zm(1) + dz
    DO k=3,mzp
       zt(k) = zt(k-1) + dz ! thermo and water levels
       zm(k) = zm(k-1) + dz ! dynamical levels	
    ENDDO
    !print*,zsurf
    !Print*,zt(:)
    DO k = 1,mzp-1
       dzm(k) = 1. / (zt(k+1) - zt(k))
    ENDDO
    dzm(mzp)=dzm(mzp-1)

    DO k = 2,mzp
       dzt(k) = 1. / (zm(k) - zm(k-1))
    ENDDO
    dzt(1) = dzt(2) * dzt(2) / dzt(3)

    !   dzm(1) = 0.5/dz
    !   dzm(2:mzp) = 1./dz
    RETURN
  END SUBROUTINE set_grid
  !-------------------------------------------------------------------------

  SUBROUTINE set_flam_vert(ztopmax,k1,k2,zzcon,W_VMD,VMD,nkp)

!!$    REAL,    INTENT(IN)  :: ztopmax(2)
    REAL,    INTENT(IN)  :: ztopmax(:)
    INTEGER, INTENT(OUT) :: k1
    INTEGER, INTENT(OUT) :: k2
    ! plumegen_coms
!!$    REAL,    INTENT(IN)  :: zzcon(nkp)
    REAL,    INTENT(IN)  :: zzcon(:)
    !- version 2
!!$    REAL,    INTENT(IN)  :: W_VMD(nkp,2)
!!$    REAL,    INTENT(OUT) :: VMD(nkp,2)
    REAL,    INTENT(IN)  :: W_VMD(:,:)
    REAL,    INTENT(OUT) :: VMD(:,:)
    INTEGER, INTENT(IN)  :: nkp

    INTEGER :: imm,k
    INTEGER :: k_lim(2)
    REAL    :: w_thresold, xxx
    INTEGER :: k_initial, k_final, ko, kk4, kl

    !- version 1
    DO imm=1,2
       ! checar 
       !    do k=1,m1-1
       DO k=1,nkp-1
          IF(zzcon(k) > ztopmax(imm) ) EXIT
       ENDDO
       k_lim(imm) = k
    ENDDO
    k1=MAX(3,k_lim(1))
    k2=MAX(3,k_lim(2))

    IF(k2 < k1) THEN
       !print*,'1: ztopmax k=',ztopmax(1), k1
       !print*,'2: ztopmax k=',ztopmax(2), k2
       k2=k1
       !stop 1234
    ENDIF
    
    !- version 2    
    !- vertical mass distribution
    !- 
    w_thresold = 1.
    DO imm=1,2

    
       VMD(1:nkp,imm)= 0.
       xxx=0.
       k_initial= 0
       k_final  = 0
    
       !- define range of the upper detrainemnt layer
       DO ko=nkp-10,2,-1
     
        IF(w_vmd(ko,imm) < w_thresold) CYCLE
     
        IF(k_final==0) k_final=ko
     
        IF(w_vmd(ko,imm)-1. > w_vmd(ko-1,imm)) THEN
          k_initial=ko
          EXIT
        ENDIF
      
       ENDDO
       !- if there is a non zero depth layer, make the mass vertical 
       !  distribution 
       IF(k_final > 0 .AND. k_initial > 0) THEN 
       
           k_initial=INT((k_final+k_initial)*0.5)
       
           !- parabolic vertical distribution between k_initial and k_final
           kk4 = k_final-k_initial+2
           DO ko=1,kk4-1
               kl=ko+k_initial-1
               VMD(kl,imm) = 6.* float(ko)/float(kk4)**2 * (1. - float(ko)/float(kk4))
           ENDDO
	   IF(SUM(VMD(1:NKP,imm)) .NE. 1.) THEN
 	       xxx= ( 1.- SUM(VMD(1:NKP,imm)) )/float(k_final-k_initial+1)
 	       DO ko=k_initial,k_final
 	         VMD(ko,imm) = VMD(ko,imm)+ xxx !- values between 0 and 1.
 	       ENDDO
               ! print*,'new mass=',sum(mass)*100.,xxx
               !pause
           ENDIF
        ENDIF !k_final > 0 .and. k_initial > 

    ENDDO
    
  END SUBROUTINE set_flam_vert

  !-------------------------------------------------------------------------

  SUBROUTINE get_fire_properties(imm,iveg_ag,burnt_area,use_last,              &
                                 heating,area,maxtime,alpha,rsurf,fmoist,tdur, &
				 plumerise_flag,FRP,ntime)

    INTEGER, INTENT(IN)    :: ntime
    INTEGER, INTENT(IN)    :: imm
    INTEGER, INTENT(IN)    :: iveg_ag
    REAL,    INTENT(IN)    :: burnt_area,FRP
    INTEGER, INTENT(IN)    :: use_last
    INTEGER, INTENT(IN)    :: plumerise_flag
    ! plumegen_coms
    REAL,    INTENT(INOUT) :: heating(:)        
    REAL,    INTENT(INOUT) :: area              
    INTEGER, INTENT(INOUT) :: maxtime           
    REAL,    INTENT(OUT)   :: alpha
    REAL,    INTENT(OUT)   :: rsurf
    REAL,    INTENT(OUT)   :: fmoist
    REAL,    INTENT(OUT)   :: tdur


    CHARACTER(LEN=*), PARAMETER :: h="**(get_fire_properties)**"

    INTEGER :: moist, i, icount
    REAL    :: bfract, effload, heat, hinc, heat_fluxW
    REAL    :: heat_flux(2,4)

    INTEGER :: mdur           ! (DMK) scratch, not used
    REAL    :: bload          ! (DMK) scratch, not used

    DATA heat_flux/  &
         !---------------------------------------------------------------------
         !  heat flux      !IGBP Land Cover	    ! 
         ! min  ! max      !Legend and		    ! reference
         !    kW/m^2       !description  	    ! 
         !--------------------------------------------------------------------
         30.0,	 80.0,   &! Tropical Forest         ! igbp 2 & 4
         30.0,   80.0,   &! Boreal forest           ! igbp 1 & 3
         4.4,	 23.0,   &! cerrado/woody savanna   | igbp  5 thru 9
         3.3,	  3.3    /! Grassland/cropland      ! igbp 10 thru 17
         !--------------------------------------------------------------------
    real, parameter :: beta = 0.88  !ref.: Paugam et al., 2015
   !real, parameter :: beta = 5.0   !ref.: Wooster et al., 2005


    !-- fire at the surface
    !area = 20.e+4   ! area of burn, m^2
    area = burnt_area! area of burn, m^2

    IF ( PLUMERISE_flag == 1) THEN
    	!fluxo de calor para o bioma
	heat_fluxW = heat_flux(imm,iveg_ag) * 1000. ! converte para W/m^2
	
    ELSEIF ( PLUMERISE_flag == 2) THEN
	! "beta" factor converts FRP to convective energy
        heat_fluxW = beta*(FRP/area)/0.55 ! in W/m^2

    ENDIF

    mdur = 53        ! duration of burn, minutes
    bload = 10.      ! total loading, kg/m**2 
    moist = 10       ! fuel moisture, %. average fuel moisture,percent dry
    maxtime =mdur+2  ! model time, min
    !maxtime =mdur-1  ! model time, min

    !heat = 21.e6    !- joules per kg of fuel consumed                   
    !heat = 15.5e6   !joules/kg - cerrado
    heat = 19.3e6    !joules/kg - floresta em alta floresta (mt)
    !alpha = 0.1      !- entrainment constant
    alpha = 0.05      !- entrainment constant

    !-------------------- printout ----------------------------------------

    !!WRITE ( * ,  * ) ' SURFACE =', ZSURF, 'M', '  LCL =', ZBASE, 'M'  
    !
    !PRINT*,'======================================================='
    !print * , ' FIRE BOUNDARY CONDITION   :'  
    !print * , ' DURATION OF BURN, MINUTES =',MDUR  
    !print * , ' AREA OF BURN, HA	      =',AREA*1.e-4
    !print * , ' HEAT FLUX, kW/m^2	      =',heat_fluxW*1.e-3
    !print * , ' TOTAL LOADING, KG/M**2    =',BLOAD  
    !print * , ' FUEL MOISTURE, %	      =',MOIST !average fuel moisture,percent dry
    !print * , ' MODEL TIME, MIN.	      =',MAXTIME  
    !
    !
    !
    ! ******************** fix up inputs *********************************
    !

    !IF (MOD (MAXTIME, 2) .NE.0) MAXTIME = MAXTIME+1  !make maxtime even

    MAXTIME = MAXTIME * 60  ! and put in seconds
    !
    RSURF = SQRT (AREA / 3.14159) !- entrainment surface radius (m)

    FMOIST   = MOIST / 100.       !- fuel moisture fraction
    !
    !
    ! calculate the energy flux and water content at lboundary.
    ! fills heating() on a minute basis. could ask for a file at this po
    ! in the program. whatever is input has to be adjusted to a one
    ! minute timescale.
    !

    DO I = 1, ntime         !- make sure of energy release
       HEATING (I) = 0.0001  !- avoid possible divide by 0
    ENDDO
    !                                  
    TDUR = MDUR * 60.       !- number of seconds in the burn

    bfract = 1.             !- combustion factor

    EFFLOAD = BLOAD * BFRACT  !- patchy burning

    !     spread the burning evenly over the interval
    !     except for the first few minutes for stability
    ICOUNT = 1  
    !
    IF(MDUR > NTIME) &
         STOP 'Increase time duration (ntime) in min - see file "plumerise_mod.f90"'
!!$         CALL FatalError(h//" Increase time duration (ntime) in min - see file chem_plumerise_scalar.f90")

    DO WHILE (ICOUNT.LE.MDUR)                             
       !  HEATING (ICOUNT) = HEAT * EFFLOAD / TDUR  ! W/m**2 
       !  HEATING (ICOUNT) = 80000.  * 0.55         ! W/m**2 

       HEATING (ICOUNT) = heat_fluxW  * 0.55     ! W/m**2 (0.55 converte para energia convectiva)
       ICOUNT = ICOUNT + 1  
    ENDDO
    !     ramp for 5 minutes
    IF(use_last /= 1) THEN

       HINC = HEATING (1) / 4.  
       HEATING (1) = 0.1  
       HEATING (2) = HINC  
       HEATING (3) = 2. * HINC  
       HEATING (4) = 3. * HINC  
    ELSE
       IF(imm==1) THEN
          HINC = HEATING (1) / 4.  
          HEATING (1) = 0.1  
          HEATING (2) = HINC  
          HEATING (3) = 2. * HINC  
          HEATING (4) = 3. * HINC 
       ELSE 
          stop "check units and use heat_fluxW"
          HINC = (HEATING (1) - heat_flux(imm-1,iveg_ag) * 1000. *0.55)/ 4.
          HEATING (1) = heat_flux(imm-1,iveg_ag) * 1000. *0.55 + 0.1  
          HEATING (2) = HEATING (1)+ HINC  
          HEATING (3) = HEATING (2)+ HINC  
          HEATING (4) = HEATING (3)+ HINC 
       ENDIF
    ENDIF
    !srf-24jan2007 - end!<<<<<<<<<<<<<<<<<<<<02082007

    RETURN
  END SUBROUTINE get_fire_properties

  !----------------------------------------------------------------------------
  !
  SUBROUTINE MAKEPLUME(i,j,kmt,ztopmax,ixx,imm,use_last,w,t,pe,qsat, &
                       est,rho,zt,te,txs,wc,dz,maxtime,zm,wt,dzm,tt,qv,qh, &
                       qi,qc,qvenv,radius,alpha,visc,cvi,vth,cvh,vti,dzt,  &
                       sc,area,rsurf,tdur,heating,fmoist,vel_e,vel_p,rad_p,&
                       vel_t,rad_t,W_VMD,izprint,nkp,ntime)  
    !
    ! *********************************************************************
    !
    !    EQUATION SOURCE--Kessler Met.Monograph No. 32 V.10 (K)
    !    Alan Weinstein, JAS V.27 pp 246-255. (W),
    !    Ogura and Takahashi, Monthly Weather Review V.99,pp895-911 (OT)
    !    Roger Pielke,Mesoscale Meteorological Modeling,Academic Press,1984
    !    Originally developed by: Don Latham (USFS)
    !
    !
    ! ************************ VARIABLE ID ********************************
    !
    !     DT=COMPUTING TIME INCREMENT (SEC)
    !     DZ=VERTICAL INCREMENT (M)
    !     LBASE=LEVEL ,CLOUD BASE
    !
    !     CONSTANTS:
    !       G = GRAVITATIONAL ACCELERATION 9.80796 (M/SEC/SEC).
    !       R = DRY AIR GAS CONSTANT (287.04E6 JOULE/KG/DEG K)
    !       CP = SPECIFIC HT. (1004 JOULE/KG/DEG K)
    !       HEATCOND = HEAT OF CONDENSATION (2.5E6 JOULE/KG)
    !       HEATFUS = HEAT OF FUSION (3.336E5 JOULE/KG)
    !       HEATSUBL = HEAT OF SUBLIMATION (2.83396E6 JOULE/KG)
    !       EPS = RATIO OF MOL.WT. OF WATER VAPOR TO THAT OF DRY AIR (0.622)
    !       DES = DIFFERENCE BETWEEN VAPOR PRESSURE OVER WATER AND ICE (MB)
    !       TFREEZE = FREEZING TEMPERATURE (K)
    !
    !
    !     PARCEL VALUES:
    !       T = TEMPERATURE (K)
    !       TXS = TEMPERATURE EXCESS (K)
    !       QH = HYDROMETEOR WATER CONTENT (G/G DRY AIR)
    !       QHI = HYDROMETEOR ICE CONTENT (G/G DRY AIR)
    !       QC = WATER CONTENT (G/G DRY AIR)
    !       QVAP = WATER VAPOR MIXING RATIO (G/G DRY AIR)
    !       QSAT = SATURATION MIXING RATIO (G/G DRY AIR)
    !       RHO = DRY AIR DENSITY (G/M**3) MASSES = RHO*Q'S IN G/M**3
    !       ES = SATURATION VAPOR PRESSURE (kPa)
    !
    !     ENVIRONMENT VALUES:
    !       TE = TEMPERATURE (K)
    !       PE = PRESSURE (kPa)
    !       QVENV = WATER VAPOR (G/G)
    !       RHE = RELATIVE HUMIDITY FRACTION (e/esat)
    !       DNE = dry air density (kg/m^3)
    !
    !     HEAT VALUES:
    !       HEATING = HEAT OUTPUT OF FIRE (WATTS/M**2)
    !       MDUR = DURATION OF BURN, MINUTES
    !
    !       W = VERTICAL VELOCITY (M/S)
    !       RADIUS=ENTRAINMENT RADIUS (FCN OF Z)
    !	RSURF = ENTRAINMENT RADIUS AT GROUND (SIMPLE PLUME, TURNER)
    !	ALPHA = ENTRAINMENT CONSTANT
    !       MAXTIME = TERMINATION TIME (MIN)
    !
    !
    !**********************************************************************
    !**********************************************************************

     USE node_mod, only:  &
       mynum

    INTEGER , INTENT(IN) :: nkp
    INTEGER , INTENT(IN) :: ntime
    INTEGER, INTENT(IN)    :: kmt,i,j
    REAL,    INTENT(INOUT) :: ztopmax  ! (DMK) alterado (OUT) para (INOUT)
    INTEGER, INTENT(IN)    :: ixx
    INTEGER, INTENT(IN)    :: imm
    INTEGER, INTENT(IN)    :: use_last
    INTEGER, INTENT(IN)    :: izprint
    ! plumegen_coms
    REAL,    INTENT(IN)    :: dz
    INTEGER, INTENT(IN)    :: maxtime
    REAL,    INTENT(IN)    :: alpha
    REAL,    INTENT(IN)    :: area
    REAL,    INTENT(IN)    :: rsurf
    REAL,    INTENT(IN)    :: tdur
    REAL,    INTENT(IN)    :: fmoist
!!$    REAL,    INTENT(IN)    :: heating(ntime)
!!$    REAL,    INTENT(INOUT) :: w     (nkp)
!!$    REAL,    INTENT(INOUT) :: t     (nkp)
!!$    REAL,    INTENT(IN)    :: pe    (nkp)
!!$    REAL,    INTENT(INOUT) :: qsat  (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: est   (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: rho   (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(IN)    :: zt    (nkp)
!!$    REAL,    INTENT(IN)    :: te    (nkp)
!!$    REAL,    INTENT(INOUT) :: txs   (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: wc    (nkp)
!!$    REAL,    INTENT(IN)    :: zm    (nkp)
!!$    REAL,    INTENT(INOUT) :: wt    (nkp)
!!$    REAL,    INTENT(IN)    :: dzm   (nkp)
!!$    REAL,    INTENT(INOUT) :: tt    (nkp)
!!$    REAL,    INTENT(INOUT) :: qv    (nkp)
!!$    REAL,    INTENT(INOUT) :: qh    (nkp)
!!$    REAL,    INTENT(INOUT) :: qi    (nkp)
!!$    REAL,    INTENT(INOUT) :: qc    (nkp)
!!$    REAL,    INTENT(IN)    :: qvenv (nkp)
!!$    REAL,    INTENT(INOUT) :: radius(nkp)
!!$    REAL,    INTENT(INOUT) :: visc  (nkp)
!!$    REAL,    INTENT(INOUT) :: cvi   (nkp)
!!$    REAL,    INTENT(INOUT) :: vth   (nkp)
!!$    REAL,    INTENT(INOUT) :: cvh   (nkp)
!!$    REAL,    INTENT(INOUT) :: vti   (nkp)
!!$    REAL,    INTENT(IN)    :: dzt   (nkp)
!!$    REAL,    INTENT(IN)    :: sc    (nkp)
!!$!srf-awe    
!!$    REAL,    INTENT(IN)    :: vel_e (nkp)
!!$    REAL,    INTENT(INOUT) :: vel_p (nkp)
!!$    REAL,    INTENT(INOUT) :: rad_p (nkp)
!!$    REAL,    INTENT(INOUT) :: vel_t (nkp)
!!$    REAL,    INTENT(INOUT) :: rad_t (nkp)
!!$    REAL,    INTENT(OUT)   :: W_VMD (nkp,2)
!!$!srf-awe    
    REAL,    INTENT(IN)    :: heating(:)
    REAL,    INTENT(INOUT) :: w     (:)
    REAL,    INTENT(INOUT) :: t     (:)
    REAL,    INTENT(IN)    :: pe    (:)
    REAL,    INTENT(INOUT) :: qsat  (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: est   (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: rho   (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(IN)    :: zt    (:)
    REAL,    INTENT(IN)    :: te    (:)
    REAL,    INTENT(INOUT) :: txs   (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: wc    (:)
    REAL,    INTENT(IN)    :: zm    (:)
    REAL,    INTENT(INOUT) :: wt    (:)
    REAL,    INTENT(IN)    :: dzm   (:)
    REAL,    INTENT(INOUT) :: tt    (:)
    REAL,    INTENT(INOUT) :: qv    (:)
    REAL,    INTENT(INOUT) :: qh    (:)
    REAL,    INTENT(INOUT) :: qi    (:)
    REAL,    INTENT(INOUT) :: qc    (:)
    REAL,    INTENT(IN)    :: qvenv (:)
    REAL,    INTENT(INOUT) :: radius(:)
    REAL,    INTENT(INOUT) :: visc  (:)
    REAL,    INTENT(INOUT) :: cvi   (:)
    REAL,    INTENT(INOUT) :: vth   (:)
    REAL,    INTENT(INOUT) :: cvh   (:)
    REAL,    INTENT(INOUT) :: vti   (:)
    REAL,    INTENT(IN)    :: dzt   (:)
    REAL,    INTENT(IN)    :: sc    (:)
!srf-awe    
    REAL,    INTENT(IN)    :: vel_e (:)
    REAL,    INTENT(INOUT) :: vel_p (:)
    REAL,    INTENT(INOUT) :: rad_p (:)
    REAL,    INTENT(INOUT) :: vel_t (:)
    REAL,    INTENT(INOUT) :: rad_t (:)
    REAL,    INTENT(OUT)   :: W_VMD (:,:)
!srf-awe    
    
 
    !logical :: endspace  
    INTEGER :: k, kk, kkmax, deltak, ilastprint, &
               nrectotal, i_micro, n_sub_step, nrec

    REAL    :: vc, g,  r,  cp,  eps,  &
               tmelt,  heatsubl,  heatfus,  heatcond, tfreeze, &
               wmax, rmaxtime, es,dt_save


    REAL, EXTERNAL :: esat

    REAL    :: DELZ_THRESOLD 

    CHARACTER (len=2) :: cixx	   
    
    !
    !
    ! ******************* SOME CONSTANTS **********************************
    !
    !      XNO=10.0E06 median volume diameter raindrop (K table 4)
    !      VC = 38.3/(XNO**.125) mean volume fallspeed eqn. (K)
    !
    PARAMETER (vc = 5.107387)  
    PARAMETER (g = 9.80796, r = 287.04, cp = 1004., eps = 0.622,  tmelt = 273.3)
    PARAMETER (heatsubl = 2.834e6, heatfus = 3.34e5, heatcond = 2.501e6)
    PARAMETER (tfreeze = 269.3)  
    !

    REAL    :: viscosity     ! (DMK) scratch
    REAL    :: tstpf         ! (DMK) scratch
    INTEGER :: mintime       ! (DMK) scratch
    REAL    :: ztop          ! (DMK) scratch
    REAL    :: dqsdz         ! (DMK) scratch
    REAL    :: time          ! (DMK) scratch
    INTEGER :: nm1           ! (DMK) scratch
    INTEGER :: l             ! (DMK) scratch
    REAL    :: adiabat       ! (DMK) scratch
    REAL    :: vhrel         ! (DMK) scratch
    REAL    :: wbar          ! (DMK) scratch
    REAL    :: dt            ! (DMK) scratch
    REAL    :: ztop_(ntime)  ! (DMK) scratch
    REAL    :: scr1(nkp)     ! (DMK) scratch
    REAL    :: qvt (nkp)     ! (DMK) scratch
    REAL    :: qct (nkp)     ! (DMK) scratch
    REAL    :: qht (nkp)     ! (DMK) scratch
    REAL    :: qit (nkp)     ! (DMK) scratch
  
    tstpf = 2.0     !- timestep factor
    viscosity = 100.!- viscosity constant (original value: 0.001)

    nrectotal=150
    nrec = 0
    !
    !*************** PROBLEM SETUP AND INITIAL CONDITIONS *****************
    mintime = 1  
    ztopmax = 0. 
    ztop    = 0. 
    time    = 0.  
    dt      = 1.
    wmax    = 1. 
    kkmax   = 10
    deltaK  = 20
    ilastprint=0
    L       = 1   ! L initialization
    wbar    = 0.
    W_VMD(:,:) = 0.
    
    !--- initialization
    ! CALL INITIAL(kmt)   
    DELZ_THRESOLD = 1.*dz
    IF(imm==1 .AND. use_last==1)  &
         CALL INITIAL(kmt,txs,w,t,wc,wt,qv,vth,vti,qh,qi,qc,est,qsat,rho, &
                      radius,visc,te,qvenv,rsurf,alpha,zt,viscosity,pe,   &
                      area,dt,l,wbar,dqsdz,cvi,tdur,heating,    &
                      mintime,fmoist,time,VEL_P,rad_p,nkp)
    IF(use_last/=1) &
         CALL INITIAL(kmt,txs,w,t,wc,wt,qv,vth,vti,qh,qi,qc,est,qsat,rho, &
                      radius,visc,te,qvenv,rsurf,alpha,zt,viscosity,pe,   &
                      area,dt,l,wbar,dqsdz,cvi,tdur,heating,    &
                      mintime,fmoist,time,VEL_P,rad_p,nkp)  
    !
    !--- initial print fields:
    IF (izprint.NE.0) THEN
       WRITE(cixx(1:2),'(i2.2)') ixx
       OPEN(2, file = 'debug.'//cixx//'.dat')  
       OPEN(19,file='plumegen.'//cixx//'.gra',         &
            form='unformatted',access='direct',status='unknown',  &
            recl=4*nrectotal)  !PC   
       !     recl=1*nrectotal) !sx6 e tupay
       CALL printout(izprint,nrectotal,mintime,dt,time,ztop, &
                     pe,t,te,qv,qsat,qc,qh, &
                     qi,zt,w,vth,sc,qvenv,nrec)
       ilastprint=2
    ENDIF

    ! ******************* model evolution ******************************
    rmaxtime = float(maxtime)
    !
    DO WHILE (TIME.LE.RMAXTIME)  !beginning of time loop

       !   do itime=1,120

       !-- set model top integration
       nm1 = MIN(kmt, kkmax + deltak)

       !-- set timestep
       !dt = (zm(2)-zm(1)) / (tstpf * wmax)  
       dt = MIN(5.,(zm(2)-zm(1)) / (tstpf * wmax))
       IF(dt < 0.25 .or. maxval(T(:))<0.) THEN
          ztop = 0.
          ztop_(mintime) = ztop
          ztopmax = 0.
          kkmax   = 1 
	  print*,"PRM_UNST=",mynum,i,j,dt
	  call flush(6)
          EXIT
       ENDIF

       !-- elapsed time, sec
       time = time+dt 
       !-- elapsed time, minutes                                      
       mintime = 1 + INT (time) / 60     
       wmax = 1.  !no zeroes allowed.
       !************************** BEGIN SPACE LOOP **************************

       !-- zerout all model tendencies
       CALL tend0_plumerise(nm1,wt,tt,qvt,qct,qht,qit,vel_t,rad_t)

       !-- bounday conditions (k=1)
       L=1
       wbar=W(1)
       CALL LBOUND(qh,qi,qc,w,t,wc,vth,vti,txs,visc,rho,qv,est,qsat,pe, &
                   alpha,area,rsurf,te,viscosity,dt,qvenv,l,wbar,dqsdz,cvi, &
                   tdur,heating, mintime,fmoist,time,VEL_P,rad_p)

       !-- dynamics for the level k>1 
       !-- W advection 
       !   call vel_advectc_plumerise(NM1,WC,WT,DNE,DZM)
       CALL vel_advectc_plumerise(NM1,WC,WT,RHO,DZM)

       !-- scalars advection 1
       CALL scl_advectc_plumerise(NM1,scr1,dt,w,wc,rho,dzm,zt,zm,dzt, &
                                  t,tt,qv,qvt,qc,qct,qi,qit,qh,qht,vel_p, &
                                  vel_t,rad_p,rad_t,nkp)

       !-- scalars advection 2
       !call scl_advectc_plumerise2('SC',NM1)

       !-- scalars entrainment, adiabatic
       CALL scl_misc(NM1,wbar,w,adiabat,alpha,radius,tt,t,te,qvt, &
                     qv,qvenv,qct,qc,qht,qh,qit,qi,vel_e,vel_p,vel_t, &
                     rad_p,rad_t)

       !-- scalars dinamic entrainment
       CALL  scl_dyn_entrain(NM1,wbar,w,adiabat,alpha,radius,tt,t,   &
                             te,qvt,qv,qvenv,qct,qc,qht,qh,qit,qi,vel_e, &
                             vel_p,vel_t,rad_p,rad_t)


       !-- gravity wave damping using Rayleigh friction layer fot T
       CALL damp_grav_wave(1,nm1,deltak,dt,zt,zm,w,t,tt,te)

       !-- microphysics
       !   goto 101 ! bypass microphysics
       dt_save=dt
       n_sub_step=3
       dt=dt/float(n_sub_step)

       DO i_micro=1,n_sub_step
          !-- sedim ?
          CALL fallpart(NM1,rho,vth,vhrel,w,cvh,vti,cvi,qh,qi,zm,qht,qit)
          !-- microphysics
          DO L=2,nm1-1
             WBAR    = 0.5*(W(L)+W(L-1))
             ES      = 0.1*ESAT (T(L))            !BLOB SATURATION VAPOR PRESSURE, EM KPA
             QSAT(L) = (EPS * ES) / (PE(L) - ES)  !BLOB SATURATION LWC G/G DRY AIR
             EST (L) = ES  
             RHO (L) = 3483.8 * PE (L) / T (L) ! AIR PARCEL DENSITY , G/M**3
             !srf18jun2005
             !	IF (W(L) .ge. 0.) DQSDZ = (QSAT(L  ) - QSAT(L-1)) / (ZT(L  ) -ZT(L-1))
             !	IF (W(L) .lt. 0.) DQSDZ = (QSAT(L+1) - QSAT(L  )) / (ZT(L+1) -ZT(L  ))
             IF (W(L) .GE. 0.) THEN 
                DQSDZ = (QSAT(L+1) - QSAT(L-1)) / (ZT(L+1 )-ZT(L-1))
             ELSE
                DQSDZ = (QSAT(L+1) - QSAT(L-1)) / (ZT(L+1) -ZT(L-1))
             ENDIF

             CALL waterbal(qc,l,qh,qi,qv,t,qsat,wbar,dqsdz,dt,rho,est,cvi)
          ENDDO
       ENDDO
       dt=dt_save
       !
101    CONTINUE
       !
       !-- W-viscosity for stability 
       CALL visc_W(nm1,kmt,zt,visc,zm,w,t,qv,qh,qc,qi,wt,tt,qvt,qct,qht,&
                   qit,vel_p,vel_t,rad_p,rad_t)

       !-- update scalars
       CALL update_plumerise(nm1,'S',wt,dt,tt,qvt,qct,qht,w,t,qv,qc,qh, &
                             qit,qi,vel_p,vel_t,rad_p,rad_t)

       CALL hadvance_plumerise(1,nm1,WC,W,mintime) 

       !-- Buoyancy
       CALL buoyancy_plumerise(nm1, t, te, qv, qvenv, qh, qi, qc, wt, scr1)

       !-- Entrainment 
       CALL entrainment(nm1,w,wt,radius,alpha,vel_p,vel_e)

       !-- update W
       CALL update_plumerise(nm1,'W',wt,dt,tt,qvt,qct,qht,w,t,qv,qc, &
                             qh,qit,qi, vel_p,vel_t,rad_p,rad_t)

       CALL hadvance_plumerise(2,nm1,WC,W,mintime) 


       !-- misc
       DO k=2,nm1
          !    pe esta em kpa  - esat do rams esta em mbar = 100 Pa = 0.1 kpa
          es       = 0.1*esat (t(k)) !blob saturation vapor pressure, em kPa
          !    rotina do plumegen calcula em kPa
          !    es    = esat_pr (t(k))  !blob saturation vapor pressure, em kPa
          qsat(k) = (eps * es) / (pe(k) - es)  !blob saturation lwc g/g dry air
          est (k) = es  
          txs (k) = t(k) - te(k)
          rho (k) = 3483.8 * pe (k) / t (k) ! air parcel density , g/m**3
          ! no pressure diff with radius

          IF((ABS(wc(k))).GT.wmax) wmax = ABS(wc(k)) ! keep wmax largest w
       ENDDO

       ! Gravity wave damping using Rayleigh friction layer for W
       CALL damp_grav_wave(2,nm1,deltak,dt,zt,zm,w,t,tt,te)
       !---


       !- update radius
       DO k=2,nm1
        radius(k) = rad_p(k)
       ENDDO

       !-- try to find the plume top (above surface height)
       kk = 1
       DO WHILE (w (kk) .GT. 1.)  
          kk = kk + 1  
          ztop =  zm(kk) 
          !print*,'W=',w (kk)
       ENDDO
       !
       ztop_(mintime) = ztop
       ztopmax = MAX (ztop, ztopmax) 
       kkmax   = MAX (kk  , kkmax  ) 
       !print * ,'ztopmax=', mintime,'mn ',ztop_(mintime), ztopmax

       !
       ! if the solution is going to a stationary phase, exit
       IF(mintime > 10) THEN                 
          !   if(mintime > 20) then                     
          !    if( abs(ztop_(mintime)-ztop_(mintime-10)) < DZ ) exit   
          IF( ABS(ztop_(mintime)-ztop_(mintime-10)) < DELZ_THRESOLD) THEN 
	   	  
	   	  !- determine W parameter to determine the VMD
           	  DO k=2,nm1
	   	   W_VMD(k,imm) = w(k)
           	  ENDDO
	   	  EXIT ! finish the integration
	   ENDIF  
       ENDIF

       IF(ilastprint == mintime .AND. izprint.NE.0) THEN
          CALL printout(izprint,nrectotal,mintime,dt,time,ztop,pe,t,te, &
                        qv,qsat,qc,qh,qi,zt,w,vth,sc,qvenv,nrec)
          ilastprint = mintime+1
       ENDIF


    ENDDO   !do next timestep

    !print * ,' ztopmax=',ztopmax,'m',mintime,'mn '
    !print*,'======================================================='
    !
    !the last printout
    IF (izprint.NE.0) THEN
       CALL printout(izprint,nrectotal,mintime,dt,time,ztop, &
                     pe,t,te,qv,qsat,qc,qh,qi,zt,w,vth,sc,qvenv, &
                     nrec)
       CLOSE (2)            
       CLOSE (19)            
    ENDIF

    RETURN  
  END SUBROUTINE MAKEPLUME

  !----------------------------------------------------------------------------
  !
  SUBROUTINE BURN(EFLUX,WATER,tdur,time,heating,mintime,dt,fmoist)  
    !	
    !- calculates the energy flux and water content at lboundary

    REAL,    INTENT(OUT) :: EFLUX
    REAL,    INTENT(OUT) :: WATER
    ! plumegen_coms
    REAL,    INTENT(IN)  :: tdur
    REAL,    INTENT(IN)  :: time
    INTEGER, INTENT(IN)  :: mintime
    REAL,    INTENT(IN)  :: dt
    REAL,    INTENT(IN)  :: fmoist
!!$    REAL,    INTENT(IN)  :: heating(ntime)
    REAL,    INTENT(IN)  :: heating(:)


    !real, parameter :: HEAT = 21.E6 !Joules/kg
    !real, parameter :: HEAT = 15.5E6 !Joules/kg - cerrado
    REAL, PARAMETER :: HEAT = 19.3E6 !Joules/kg - floresta em Alta Floresta (MT)
    !
    ! The emission factor for water is 0.5. The water produced, in kg,
    ! is then  fuel mass*0.5 + (moist/100)*mass per square meter.
    ! The fire burns for DT out of TDUR seconds, the total amount of
    ! fuel burned is AREA*BLOAD*(DT/TDUR) kg. this amount of fuel is
    ! considered to be spread over area AREA and so the mass burned per
    ! unit area is BLOAD*(DT/TDUR), and the rate is BLOAD/TDUR.
    !        
    IF (TIME.GT.TDUR) THEN !is the burn over?   
       EFLUX = 0.000001    !prevent a potential divide by zero
       WATER = 0.  
       RETURN  
    ELSE  
       !                                                   
       EFLUX = HEATING (MINTIME)                          ! Watts/m**2
       !  WATER = EFLUX * (DT / HEAT) * (0.5 + FMOIST)       ! kg/m**2 
       WATER = EFLUX * (DT / HEAT) * (0.5 + FMOIST) /0.55 ! kg/m**2 
       WATER = WATER * 1000.                              ! g/m**2
       !
       !        print*,'BURN:',time,EFLUX/1.e+9
    ENDIF
    !
    RETURN  
  END SUBROUTINE BURN
  !----------------------------------------------------------------------------
  !
  SUBROUTINE LBOUND(qh,qi,qc,w,t,wc,vth,vti,txs,visc,rho,qv,est,qsat,pe, &
                    alpha,area,rsurf,te,viscosity,dt,qvenv,l,wbar,dqsdz,cvi, &
                    tdur,heating, mintime,fmoist,time,VEL_P,rad_p)  

    !
    ! ********** BOUNDARY CONDITIONS AT ZSURF FOR PLUME AND CLOUD ********
    !
    ! source of equations: J.S. Turner Buoyancy Effects in Fluids
    !                      Cambridge U.P. 1973 p.172,
    !                      G.A. Briggs Plume Rise, USAtomic Energy Commissio
    !                      TID-25075, 1969, P.28
    !
    ! fundamentally a point source below ground. at surface, this produces
    ! a velocity w(1) and temperature T(1) which vary with time. There is
    ! also a water load which will first saturate, then remainder go into
    ! QC(1).
    ! EFLUX = energy flux at ground,watt/m**2 for the last DT
    !

    ! plumegen_coms
    REAL,    INTENT(IN)    :: alpha
    REAL,    INTENT(IN)    :: area
    REAL,    INTENT(IN)    :: rsurf
    REAL,    INTENT(IN)    :: viscosity
    REAL,    INTENT(IN)    :: dt
    INTEGER, INTENT(IN)    :: l
    REAL,    INTENT(IN)    :: wbar
    REAL,    INTENT(IN)    :: dqsdz
    REAL,    INTENT(IN)    :: tdur
    INTEGER, INTENT(IN)    :: mintime
    REAL,    INTENT(IN)    :: fmoist
    REAL,    INTENT(IN)    :: time
!!$    REAL,    INTENT(IN)    :: heating(ntime)
!!$    REAL,    INTENT(INOUT) :: qh   (nkp)
!!$    REAL,    INTENT(INOUT) :: qi   (nkp)
!!$    REAL,    INTENT(INOUT) :: qc   (nkp)
!!$    REAL,    INTENT(INOUT) :: w    (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: t    (nkp)
!!$    REAL,    INTENT(INOUT) :: wc   (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: vth  (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: vti  (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: txs  (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: visc (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: rho  (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: qv   (nkp)
!!$    REAL,    INTENT(INOUT) :: est  (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: qsat (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(IN)    :: pe   (nkp)
!!$    REAL,    INTENT(IN)    :: te   (nkp)
!!$    REAL,    INTENT(IN)    :: qvenv(nkp)
!!$    REAL,    INTENT(IN)    :: cvi  (nkp)
!!$    REAL,    INTENT(INOUT) :: VEL_P(nkp) 
!!$    REAL,    INTENT(INOUT) :: rad_p(nkp) 
    REAL,    INTENT(IN)    :: heating(:)
    REAL,    INTENT(INOUT) :: qh   (:)
    REAL,    INTENT(INOUT) :: qi   (:)
    REAL,    INTENT(INOUT) :: qc   (:)
    REAL,    INTENT(INOUT) :: w    (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: t    (:)
    REAL,    INTENT(INOUT) :: wc   (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: vth  (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: vti  (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: txs  (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: visc (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: rho  (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: qv   (:)
    REAL,    INTENT(INOUT) :: est  (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: qsat (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(IN)    :: pe   (:)
    REAL,    INTENT(IN)    :: te   (:)
    REAL,    INTENT(IN)    :: qvenv(:)
    REAL,    INTENT(IN)    :: cvi  (:)
    REAL,    INTENT(INOUT) :: VEL_P(:) 
    REAL,    INTENT(INOUT) :: rad_p(:) 


    REAL, PARAMETER :: g = 9.80796, r = 287.04, cp = 1004.6, eps = 0.622,tmelt = 273.3
    REAL, PARAMETER :: tfreeze = 269.3, pi = 3.14159, e1 = 1./3., e2 = 5./3.
    REAL            :: es,  eflux, water,  pres, c1,  c2, f, zv,  denscor, xwater

    REAL, EXTERNAL :: esat

    !            
    QH (1) = QH (2)   !soak up hydrometeors
    QI (1) = QI (2)              
    QC (1) = 0.       !no cloud here
    !
    !
    CALL BURN (EFLUX,WATER,tdur,time,heating,mintime,dt,fmoist)  
    !
    !  calculate parameters at boundary from a virtual buoyancy point source
    !
    PRES = PE (1) * 1000.   !need pressure in N/m**2

    C1 = 5. / (6. * ALPHA)  !alpha is entrainment constant

    C2 = 0.9 * ALPHA  

    F = EFLUX / (PRES * CP * PI)  

    F = G * R * F * AREA  !buoyancy flux

    ZV = C1 * RSURF  !virtual boundary height

    W (1) = C1 * ( (C2 * F) **E1) / ZV**E1  !boundary velocity

    DENSCOR = C1 * F / G / (C2 * F) **E1 / ZV**E2   !density correction

    T (1) = TE (1) / (1. - DENSCOR)    !temperature of virtual plume at zsurf

    !
    WC(1) = W(1)

    VEL_P(1) = 0.
    rad_p(1) = rsurf
    !SC(1) = SCE(1)+F/1000.*dt  ! gas/particle (g/g)

    ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !     match dw/dz,dt/dz at the boundary. F is conserved.
    !
    !WBAR = W (1) * (1. - 1. / (6. * ZV) )  
    !ADVW = WBAR * W (1) / (3. * ZV)  
    !ADVT = WBAR * (5. / (3. * ZV) ) * (DENSCOR / (1. - DENSCOR) )  
    !ADVC = 0.  
    !ADVH = 0.  
    !ADVI = 0.  
    !ADIABAT = - WBAR * G / CP  
    VTH (1) = - 4.  
    VTI (1) = - 3.  
    TXS (1) = T (1) - TE (1)  

    VISC (1) = VISCOSITY  

    RHO (1) = 3483.8 * PE (1) / T (1)   !air density at level 1, g/m**3

    XWATER = WATER / (W (1) * DT * RHO (1) )   !firewater mixing ratio

    QV (1) = XWATER + QVENV (1)  !plus what's already there 


    !  PE esta em kPa  - ESAT do RAMS esta em mbar = 100 Pa = 0.1 kPa
    ES       = 0.1*ESAT (T(1)) !blob saturation vapor pressure, em kPa
    !  rotina do plumegen ja calcula em kPa
    !  ES       = ESAT_PR (T(1))  !blob saturation vapor pressure, em kPa

    EST  (1)  = ES                                  
    QSAT (1) = (EPS * ES) / (PE (1) - ES)   !blob saturation lwc g/g dry air

    IF (QV (1) .GT. QSAT (1) ) THEN  
       QC (1) = QV   (1) - QSAT (1) + QC (1)  !remainder goes into cloud drops
       QV (1) = QSAT (1)  
    ENDIF
    !
    CALL WATERBAL(qc,l,qh,qi,qv,t,qsat,wbar,dqsdz,dt,rho,est,cvi)
    !
    RETURN  
  END SUBROUTINE LBOUND

  !----------------------------------------------------------------------------
  !
  SUBROUTINE INITIAL (kmt,txs,w,t,wc,wt,qv,vth,vti,qh,qi,qc,est,qsat,rho,   &
                      radius,visc,te,qvenv,rsurf,alpha,zt,viscosity,pe, &
                      area,dt,l,wbar,dqsdz,cvi,tdur,heating,mintime,  &
                      fmoist,time,VEL_P,rad_p,nkp) 

    !
    ! ************* SETS UP INITIAL CONDITIONS FOR THE PROBLEM ************
    INTEGER , INTENT(IN) :: nkp
    INTEGER , INTENT(IN) :: kmt

    ! plumegen_coms
    REAL,    INTENT(IN)    :: rsurf
    REAL,    INTENT(IN)    :: alpha
    REAL,    INTENT(IN)    :: viscosity
    REAL,    INTENT(IN)    :: area
    REAL,    INTENT(IN)    :: dt
    INTEGER, INTENT(IN)    :: l
    REAL,    INTENT(IN)    :: wbar
    REAL,    INTENT(IN)    :: dqsdz
    REAL,    INTENT(IN)    :: tdur
    INTEGER, INTENT(IN)    :: mintime
    REAL,    INTENT(IN)    :: fmoist
    REAL,    INTENT(IN)    :: time
!!$    REAL,    INTENT(IN)    :: heating(ntime)
!!$    REAL,    INTENT(INOUT) :: txs   (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: w     (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: t     (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: wc    (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: wt    (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: qv    (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: vth   (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: vti   (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: qh    (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: qi    (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: qc    (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: est   (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: qsat  (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: rho   (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: radius(nkp)
!!$    REAL,    INTENT(INOUT) :: visc  (nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(IN)    :: te    (nkp)
!!$    REAL,    INTENT(IN)    :: qvenv (nkp)
!!$    REAL,    INTENT(IN)    :: zt    (nkp)
!!$    REAL,    INTENT(IN)    :: pe    (nkp)
!!$    REAL,    INTENT(IN)    :: cvi   (nkp)
!!$    REAL,    INTENT(INOUT) :: VEL_P (nkp) 
!!$    REAL,    INTENT(INOUT) :: rad_p (nkp) 
    REAL,    INTENT(IN)    :: heating(:)
    REAL,    INTENT(INOUT) :: txs   (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: w     (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: t     (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: wc    (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: wt    (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: qv    (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: vth   (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: vti   (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: qh    (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: qi    (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: qc    (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: est   (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: qsat  (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: rho   (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: radius(:)
    REAL,    INTENT(INOUT) :: visc  (:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(IN)    :: te    (:)
    REAL,    INTENT(IN)    :: qvenv (:)
    REAL,    INTENT(IN)    :: zt    (:)
    REAL,    INTENT(IN)    :: pe    (:)
    REAL,    INTENT(IN)    :: cvi   (:)
    REAL,    INTENT(INOUT) :: VEL_P (:) 
    REAL,    INTENT(INOUT) :: rad_p (:) 


    REAL, PARAMETER :: tfreeze = 269.3
    INTEGER         :: k
    REAL            :: es

    INTEGER         :: n ! (DMK) scratch

    REAL, EXTERNAL :: esat

    !

    N=kmt
    ! initialize temperature structure,to the end of equal spaced sounding,
    DO k = 1, N			  
       TXS (k) = 0.0  
       W (k) = 0.0             
       T (k) = TE(k)       !blob set to environment		  
       WC(k) = 0.0
       WT(k) = 0.0
       QV(k) = QVENV (k)   !blob set to environment             
       VTH(k) = 0.		!initial rain velocity = 0	   
       VTI(k) = 0.		!initial ice  velocity = 0	              
       QH(k) = 0.		!no rain			     
       QI(k) = 0.		!no ice 			     
       QC(k) = 0.		!no cloud drops	                     
       !  PE esta em kPa  - ESAT do RAMS esta em mbar = 100 Pa = 0.1 kPa
       ES       = 0.1*ESAT (T(k)) !blob saturation vapor pressure, em kPa
       !  rotina do plumegen calcula em kPa
       !  ES       = ESAT_PR (T(k))  !blob saturation vapor pressure, em kPa
       EST  (k) = ES  
       QSAT (k) = (.622 * ES) / (PE (k) - ES) !saturation lwc g/g
       RHO  (k) = 3483.8 * PE (k) / T (k) 	!dry air density g/m**3    

       VEL_P(k) = 0.
       rad_p(k) = 0.
       
    ENDDO

    ! Initialize the entrainment radius, Turner-style plume
    radius(1) = rsurf
    rad_p(1)  = rsurf
    DO k=2,N
       radius(k) = radius(k-1)+(6./5.)*alpha*(zt(k)-zt(k-1))
       rad_p(k)  = radius(k)
   ENDDO

    !  Initialize the viscosity
    VISC (1) = VISCOSITY
    DO k=2,N
     !  VISC (k) = VISCOSITY!max(1.e-3,visc(k-1) - 1.* VISCOSITY/float(nkp))
       VISC (k) = MAX(1.e-3,visc(k-1) - 1.* VISCOSITY/float(nkp))
    ENDDO
    !--   Initialize gas/concentration
    !DO k =10,20
    !   SC(k) = 20.
    !ENDDO
    !stop 333

    CALL LBOUND(qh,qi,qc,w,t,wc,vth,vti,txs,visc,rho,qv,est,qsat,pe, &
                alpha,area,rsurf,te,viscosity,dt,qvenv,l,wbar,dqsdz,cvi, &
                tdur,heating,mintime,fmoist,time,VEL_P,rad_p)
    RETURN  
  END SUBROUTINE INITIAL

  !----------------------------------------------------------------------------
  !
  SUBROUTINE damp_grav_wave(ifrom,nm1,deltak,dt,zt,zm,w,t,tt,te)

    INTEGER, INTENT(IN)    :: ifrom
    INTEGER, INTENT(IN)    :: nm1
    INTEGER, INTENT(IN)    :: deltak
    REAL,    INTENT(IN)    :: dt
!!$    REAL,    INTENT(IN)    :: zt(nm1)
!!$    REAL,    INTENT(IN)    :: zm(nm1)
!!$    REAL,    INTENT(INOUT) :: w (nm1)
!!$    REAL,    INTENT(INOUT) :: t (nm1)
!!$    REAL,    INTENT(INOUT) :: tt(nm1)
!!$    REAL,    INTENT(IN)    :: te(nm1)
    REAL,    INTENT(IN)    :: zt(:)
    REAL,    INTENT(IN)    :: zm(:)
    REAL,    INTENT(INOUT) :: w (:)
    REAL,    INTENT(INOUT) :: t (:)
    REAL,    INTENT(INOUT) :: tt(:)
    REAL,    INTENT(IN)    :: te(:)

    REAL :: dummy(nm1)

    IF(ifrom==1) THEN
       CALL friction(ifrom,nm1,deltak,dt,zt,zm,t,tt,te)
       !call friction(ifrom,nm1,dt,zt,zm,qv,qvt,qvenv)
       ! call friction(ifrom,nm1,deltak,dt,zt,zm,vel_p,vel_t,vel_e)
       RETURN
    ENDIF

    dummy(:) = 0.
    IF(ifrom==2) CALL friction(ifrom,nm1,deltak,dt,zt,zm,w,dummy ,dummy)
    !call friction(ifrom,nm1,dt,zt,zm,qi,qit ,dummy)
    !call friction(ifrom,nm1,dt,zt,zm,qh,qht ,dummy)
    !call friction(ifrom,nm1,dt,zt,zm,qc,qct ,dummy)
    RETURN
  END SUBROUTINE damp_grav_wave

  !----------------------------------------------------------------------------
  !
  SUBROUTINE friction(ifrom,nm1,deltak,dt,zt,zm,var1,vart,var2)

    INTEGER, INTENT(IN)    :: ifrom
    INTEGER, INTENT(IN)    :: nm1
    INTEGER, INTENT(IN)    :: deltak
    REAL,    INTENT(IN)    :: dt
    REAL,    INTENT(IN)    :: zt  (nm1)
    REAL,    INTENT(IN)    :: zm  (nm1)
    REAL,    INTENT(INOUT) :: var1(nm1)
    REAL,    INTENT(INOUT) :: vart(nm1)
    REAL,    INTENT(IN)    :: var2(nm1)


    INTEGER :: k, kf
    REAL    :: zmkf, ztop, distim, c1, c2

    !nfpt=50
    !kf = nm1 - nfpt
    !kf = nm1 - INT(deltak/2)
    kf = nm1 - INT(deltak)

    zmkf = zm(kf) !old: float(kf )*dz
    ztop = zm(nm1)
    !distim = min(4.*dt,200.)
    !distim = 60. ! orig
    distim = MIN(3.*dt,60.)

    c1 = 1. / (distim * (ztop - zmkf))
    c2 = dt * c1

    IF(ifrom == 1) THEN  
       DO k = nm1,2,-1
          IF (zt(k) .LE. zmkf) CYCLE
          vart(k) = vart(k)   + c1 * (zt(k) - zmkf)*(var2(k) - var1(k))
       ENDDO
    ELSEIF(ifrom == 2) THEN
       DO k = nm1,2,-1
          IF (zt(k) .LE. zmkf) CYCLE
          var1(k) =  var1(k) + c2 * (zt(k) - zmkf)*(var2(k) - var1(k))
       ENDDO
    ENDIF
    RETURN
  END SUBROUTINE friction

  !----------------------------------------------------------------------------
  !
  SUBROUTINE vel_advectc_plumerise(m1,wc,wt,rho,dzm)

    INTEGER, INTENT(IN)    :: m1
!!$    REAL,    INTENT(IN)    :: wc (m1)
!!$    REAL,    INTENT(INOUT) :: wt (m1)
!!$    REAL,    INTENT(IN)    :: rho(m1)
!!$    REAL,    INTENT(IN)    :: dzm(m1)
    REAL,    INTENT(IN)    :: wc (:)
    REAL,    INTENT(INOUT) :: wt (:)
    REAL,    INTENT(IN)    :: rho(:)
    REAL,    INTENT(IN)    :: dzm(:)

    INTEGER :: k
    REAL    :: flxw(m1)
    REAL    :: dn0(m1) ! var local
    REAL    :: c1z

    !dzm(:)= 1./dz

    dn0(1:m1)=rho(1:m1)*1.e-3 ! converte de cgs para mks

    flxw(1) = wc(1) * dn0(1) 

    DO k = 2,m1-1
       flxw(k) = wc(k) * .5 * (dn0(k) + dn0(k+1))
    ENDDO

    ! Compute advection contribution to W tendency

    c1z = .5 

    DO k = 2,m1-2

       wt(k) = wt(k)  &
            + c1z * dzm(k) / (dn0(k) + dn0(k+1)) *     (   &
            (flxw(k) + flxw(k-1))  * (wc(k) + wc(k-1))   &
            - (flxw(k) + flxw(k+1))  * (wc(k) + wc(k+1))   &
            + (flxw(k+1) - flxw(k-1)) * 2.* wc(k)       )

    ENDDO

    RETURN
  END SUBROUTINE vel_advectc_plumerise

  !----------------------------------------------------------------------------
  !
  SUBROUTINE hadvance_plumerise(iac,m1,wc,wp,mintime)

    INTEGER, INTENT(IN)    :: iac
    INTEGER, INTENT(IN)    :: m1
!!$    REAL,    INTENT(INOUT) :: wc(m1)
!!$    REAL,    INTENT(INOUT) :: wp(m1)
    REAL,    INTENT(INOUT) :: wc(:)
    REAL,    INTENT(INOUT) :: wp(:)
    INTEGER, INTENT(IN)    :: mintime


    REAL :: dummy(m1)
    REAL :: eps

    !     It is here that the Asselin filter is applied.  For the velocities
    !     and pressure, this must be done in two stages, the first when
    !     IAC=1 and the second when IAC=2.


    eps = .2
    IF(mintime == 1) eps=0.5

    !     For both IAC=1 and IAC=2, call PREDICT for U, V, W, and P.
    !
    CALL predict_plumerise(m1,wc,wp,dummy,iac,eps)
    !print*,'mintime',mintime,eps
    !do k=1,m1
    !   print*,'W-HAD',k,wc(k),wp(k),wt(k)
    !enddo
    RETURN
  END SUBROUTINE hadvance_plumerise

  !----------------------------------------------------------------------------
  !
  SUBROUTINE predict_plumerise(npts,ac,ap,af,iac,epsu)

    INTEGER, INTENT(IN)    :: npts
!!$    REAL,    INTENT(INOUT) :: ac(npts)
!!$    REAL,    INTENT(INOUT) :: ap(npts)
!!$    REAL,    INTENT(INOUT) :: af(npts) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: ac(:)
    REAL,    INTENT(INOUT) :: ap(:)
    REAL,    INTENT(INOUT) :: af(:) ! (DMK) alterado (OUT) para (INOUT)
    INTEGER, INTENT(IN)    :: iac
    REAL,    INTENT(IN)    :: epsu


    INTEGER :: m


    !     For IAC=3, this routine moves the arrays AC and AP forward by
    !     1 time level by adding in the prescribed tendency. It also
    !     applies the Asselin filter given by:

    !              {AC} = AC + EPS * (AP - 2 * AC + AF)

    !     where AP,AC,AF are the past, current and future time levels of A.
    !     All IAC=1 does is to perform the {AC} calculation without the AF
    !     term present.  IAC=2 completes the calculation of {AC} by adding
    !     the AF term only, and advances AC by filling it with input AP
    !     values which were already updated in ACOUSTC.
    !

    IF (iac .EQ. 1) THEN
       DO m = 1,npts
          ac(m) = ac(m) + epsu * (ap(m) - 2. * ac(m))
       ENDDO
       RETURN
    ELSEIF (iac .EQ. 2) THEN
       DO m = 1,npts
          af(m) = ap(m)
          ap(m) = ac(m) + epsu * af(m)
       ENDDO
       !elseif (iac .eq. 3) then
       !   do m = 1,npts
       !      af(m) = ap(m) + dtlp * fa(m)
       !   enddo
       !   do m = 1,npts
       !      ap(m) = ac(m) + epsu * (ap(m) - 2. * ac(m) + af(m))
       !   enddo
    ENDIF

    DO m = 1,npts
       ac(m) = af(m)
    ENDDO
    RETURN
  END SUBROUTINE predict_plumerise

  !----------------------------------------------------------------------------
  !
  SUBROUTINE  buoyancy_plumerise(m1, T, TE, QV, QVENV, QH, QI, QC, WT, scr1)

    INTEGER, INTENT(IN)    :: m1
!!$    REAL,    INTENT(IN)    :: T    (m1)
!!$    REAL,    INTENT(IN)    :: TE   (m1)
!!$    REAL,    INTENT(IN)    :: QV   (m1)
!!$    REAL,    INTENT(IN)    :: QVENV(m1)
!!$    REAL,    INTENT(IN)    :: QH   (m1)
!!$    REAL,    INTENT(IN)    :: QI   (m1)
!!$    REAL,    INTENT(IN)    :: QC   (m1)
!!$    REAL,    INTENT(INOUT) :: wt   (m1)
!!$    REAL,    INTENT(INOUT) :: scr1 (m1) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(IN)    :: T    (:)
    REAL,    INTENT(IN)    :: TE   (:)
    REAL,    INTENT(IN)    :: QV   (:)
    REAL,    INTENT(IN)    :: QVENV(:)
    REAL,    INTENT(IN)    :: QH   (:)
    REAL,    INTENT(IN)    :: QI   (:)
    REAL,    INTENT(IN)    :: QC   (:)
    REAL,    INTENT(INOUT) :: wt   (:)
    REAL,    INTENT(INOUT) :: scr1 (:) ! (DMK) alterado (OUT) para (INOUT)


    REAL, PARAMETER :: g = 9.8, eps = 0.622, gama = 0.5 ! mass virtual coeff.
    REAL, PARAMETER :: mu = 0.15 

    INTEGER         :: k
    REAL            :: TV, TVE, QWTOTL, umgamai

    !- orig
    umgamai = 1./(1.+gama) ! compensa a falta do termo de aceleracao associado `as
                           ! pertubacoes nao-hidrostaticas no campo de pressao

    !- new                 ! Siesbema et al, 2004
    !umgamai = 1./(1.-2.*mu)

    DO k = 2,m1-1

       TV =   T(k) * (1. + (QV(k)   /EPS))/(1. + QV(k)   )  !blob virtual temp.                                        	   
       TVE = TE(k) * (1. + (QVENV(k)/EPS))/(1. + QVENV(k))  !and environment

       QWTOTL = QH(k) + QI(k) + QC(k)                       ! QWTOTL*G is drag
       !- orig
       !scr1(k)= G*( umgamai*(  TV - TVE) / TVE   - QWTOTL) 
       scr1(k)= G*  umgamai*( (TV - TVE) / TVE   - QWTOTL) 

       !if(k .lt. 10)print*,'BT',k,TV,TVE,TVE,QWTOTL
    ENDDO

    DO k = 2,m1-2
       wt(k) = wt(k)+0.5*(scr1(k)+scr1(k+1))
       !   print*,'W-BUO',k,wt(k),scr1(k),scr1(k+1)
    ENDDO

  END SUBROUTINE  buoyancy_plumerise

  !----------------------------------------------------------------------------
  !
  SUBROUTINE ENTRAINMENT(m1,w,wt,radius,ALPHA,vel_p,vel_e)

    INTEGER, INTENT(IN)    :: m1
    REAL,    INTENT(IN)    :: alpha
!!$    REAL,    INTENT(IN)    :: w     (m1)
!!$    REAL,    INTENT(INOUT) :: wt    (m1)
!!$    REAL,    INTENT(IN)    :: radius(m1)
!!$    REAL,    INTENT(IN)    :: vel_p (m1)
!!$    REAL,    INTENT(IN)    :: vel_e (m1)
    REAL,    INTENT(IN)    :: w     (:)
    REAL,    INTENT(INOUT) :: wt    (:)
    REAL,    INTENT(IN)    :: radius(:)
    REAL,    INTENT(IN)    :: vel_p (:)
    REAL,    INTENT(IN)    :: vel_e (:)


    REAL, PARAMETER :: mu = 0.15 ,gama = 0.5 ! mass virtual coeff.

    INTEGER         :: k
    REAL            :: DMDTM, WBAR, RADIUS_BAR, umgamai, DYN_ENTR

    !- new - Siesbema et al, 2004
    !umgamai = 1./(1.-2.*mu)

    !- orig
    !umgamai = 1
    umgamai = 1./(1.+gama) ! compensa a falta do termo de aceleracao associado `as
                           ! pertubacoes nao-hidrostaticas no campo de pressao

    !
    !-- ALPHA/RADIUS(L) = (1/M)DM/DZ  (W 14a)
    DO k=2,m1-1

       !-- for W: WBAR is only W(k)
       WBAR=W(k)          
       RADIUS_BAR = 0.5*(RADIUS(k) + RADIUS(k-1))
       ! orig
       !DMDTM =           2. * ALPHA * ABS (WBAR) / RADIUS_BAR  != (1/M)DM/DT
       DMDTM  = umgamai * 2. * ALPHA * ABS (WBAR) / RADIUS_BAR  != (1/M)DM/DT
       
       !--  DMDTM*W(L) entrainment,
       wt(k) = wt(k)  - DMDTM*ABS (WBAR)

       !if(VEL_P (k) - VEL_E (k) > 0.) cycle

       !-   dynamic entrainment
       DYN_ENTR =  (2./3.1416)*0.5*ABS (VEL_P(k)-VEL_E(k)+VEL_P(k-1)-&
            VEL_E(k-1)) /RADIUS_BAR

       wt(k) = wt(k)  - DYN_ENTR*ABS (WBAR)
       
       !- entraiment acceleration for output only
       !dwdt_entr(k) =  - DMDTM*ABS (WBAR)- DYN_ENTR*ABS (WBAR)

    ENDDO
  END SUBROUTINE  ENTRAINMENT

  !----------------------------------------------------------------------------
  !
  SUBROUTINE scl_advectc_plumerise(mzp,scr1,dt,w,wc,rho,dzm,zt,zm,dzt, &
                                   t,tt,qv,qvt,qc,qct,qi,qit,qh,qht,vel_p, &
                                   vel_t,rad_p,rad_t,nkp)

    INTEGER, INTENT(IN) :: nkp
    INTEGER, INTENT(IN) :: mzp
    ! plumegen_coms
    REAL, INTENT(IN)    :: dt
!!$    REAL, INTENT(INOUT) :: scr1 (nkp)
!!$    REAL, INTENT(IN)    :: w    (nkp)
!!$    REAL, INTENT(IN)    :: wc   (nkp)
!!$    REAL, INTENT(IN)    :: rho  (nkp)
!!$    REAL, INTENT(IN)    :: dzm  (nkp)
!!$    REAL, INTENT(IN)    :: zt   (nkp)
!!$    REAL, INTENT(IN)    :: zm   (nkp)
!!$    REAL, INTENT(IN)    :: dzt  (nkp)
!!$    REAL, INTENT(IN)    :: t    (nkp)
!!$    REAL, INTENT(INOUT) :: tt   (nkp)
!!$    REAL, INTENT(IN)    :: qv   (nkp)
!!$    REAL, INTENT(INOUT) :: qvt  (nkp)
!!$    REAL, INTENT(IN)    :: qc   (nkp)
!!$    REAL, INTENT(INOUT) :: qct  (nkp)
!!$    REAL, INTENT(IN)    :: qi   (nkp)
!!$    REAL, INTENT(INOUT) :: qit  (nkp)
!!$    REAL, INTENT(IN)    :: qh   (nkp)
!!$    REAL, INTENT(INOUT) :: qht  (nkp)
!!$    REAL, INTENT(IN)    :: vel_p(nkp)
!!$    REAL, INTENT(INOUT) :: vel_t(nkp)
!!$    REAL, INTENT(IN)    :: rad_p(nkp)
!!$    REAL, INTENT(INOUT) :: rad_t(nkp)
    REAL, INTENT(INOUT) :: scr1 (:)
    REAL, INTENT(IN)    :: w    (:)
    REAL, INTENT(IN)    :: wc   (:)
    REAL, INTENT(IN)    :: rho  (:)
    REAL, INTENT(IN)    :: dzm  (:)
    REAL, INTENT(IN)    :: zt   (:)
    REAL, INTENT(IN)    :: zm   (:)
    REAL, INTENT(IN)    :: dzt  (:)
    REAL, INTENT(IN)    :: t    (:)
    REAL, INTENT(INOUT) :: tt   (:)
    REAL, INTENT(IN)    :: qv   (:)
    REAL, INTENT(INOUT) :: qvt  (:)
    REAL, INTENT(IN)    :: qc   (:)
    REAL, INTENT(INOUT) :: qct  (:)
    REAL, INTENT(IN)    :: qi   (:)
    REAL, INTENT(INOUT) :: qit  (:)
    REAL, INTENT(IN)    :: qh   (:)
    REAL, INTENT(INOUT) :: qht  (:)
    REAL, INTENT(IN)    :: vel_p(:)
    REAL, INTENT(INOUT) :: vel_t(:)
    REAL, INTENT(IN)    :: rad_p(:)
    REAL, INTENT(INOUT) :: rad_t(:)


    REAL    :: dtlto2
    INTEGER :: k
    REAL    :: vt3dc(nkp) ! (DMK) scratch
    REAL    :: vt3df(nkp) ! (DMK) scratch
    REAL    :: vt3dg(nkp) ! (DMK) scratch
    REAL    :: vt3dk(nkp) ! (DMK) scratch
    REAL    :: vctr1(nkp) ! (DMK) scratch
    REAL    :: vctr2(nkp) ! (DMK) scratch

    !  wp => w
    !- Advect  scalars
    dtlto2   = .5 * dt
    !  vt3dc(1) =      (w(1) + wc(1)) * dtlto2 * dne(1)
    vt3dc(1) =      (w(1) + wc(1)) * dtlto2 * rho(1)*1.e-3!converte de CGS p/ MKS
    vt3df(1) = .5 * (w(1) + wc(1)) * dtlto2 * dzm(1)

    DO k = 2,mzp
       !     vt3dc(k) =  (w(k) + wc(k)) * dtlto2 *.5 * (dne(k) + dne(k+1))
       vt3dc(k) =  (w(k) + wc(k)) * dtlto2 *.5 * (rho(k) + rho(k+1))*1.e-3
       vt3df(k) =  (w(k) + wc(k)) * dtlto2 *.5 *  dzm(k)
       !print*,'vt3df-vt3dc',k,vt3dc(k),vt3df(k)
    ENDDO


    !-srf-24082005
    !  do k = 1,mzp-1
    DO k = 1,mzp
       vctr1(k) = (zt(k+1) - zm(k)) * dzm(k)
       vctr2(k) = (zm(k)   - zt(k)) * dzm(k)
       !    vt3dk(k) = dzt(k) / dne(k)
       vt3dk(k) = dzt(k) /(rho(k)*1.e-3)
       !print*,'VT3dk',k,dzt(k) , dne(k)
    ENDDO

    !      scalarp => scalar_tab(n,ngrid)%var_p
    !      scalart => scalar_tab(n,ngrid)%var_t

    !- temp advection tendency (TT)
    scr1=T
!!$    CALL fa_zc_plumerise(mzp,T,scr1(1),vt3dc(1),vt3df(1),vt3dg(1), &
!!$                         vt3dk(1),vctr1,vctr2)
    CALL fa_zc_plumerise(mzp,T,scr1,vt3dc,vt3df,vt3dg, &
                         vt3dk,vctr1,vctr2)

!!$    CALL advtndc_plumerise(mzp,T,scr1(1),TT,dt)
    CALL advtndc_plumerise(mzp,T,scr1,TT,dt)

    !- water vapor advection tendency (QVT)
    scr1=QV
!!$    CALL fa_zc_plumerise(mzp,QV,scr1(1),vt3dc(1),vt3df(1),vt3dg(1), &
!!$                         vt3dk(1),vctr1,vctr2)
    CALL fa_zc_plumerise(mzp,QV,scr1,vt3dc,vt3df,vt3dg, &
                         vt3dk,vctr1,vctr2)

!!$    CALL advtndc_plumerise(mzp,QV,scr1(1),QVT,dt)
    CALL advtndc_plumerise(mzp,QV,scr1,QVT,dt)

    !- liquid advection tendency (QCT)
    scr1=QC
!!$    CALL fa_zc_plumerise(mzp,QC,scr1(1),vt3dc(1),vt3df(1),vt3dg(1), &
!!$                         vt3dk(1),vctr1,vctr2)
    CALL fa_zc_plumerise(mzp,QC,scr1,vt3dc,vt3df,vt3dg, &
                         vt3dk,vctr1,vctr2)

!!$    CALL advtndc_plumerise(mzp,QC,scr1(1),QCT,dt)
    CALL advtndc_plumerise(mzp,QC,scr1,QCT,dt)

    !- ice advection tendency (QIT)
    scr1=QI
!!$    CALL fa_zc_plumerise(mzp,QI,scr1(1),vt3dc(1),vt3df(1),vt3dg(1), &
!!$                         vt3dk(1),vctr1,vctr2)
    CALL fa_zc_plumerise(mzp,QI,scr1,vt3dc,vt3df,vt3dg, &
                         vt3dk,vctr1,vctr2)

!!$    CALL advtndc_plumerise(mzp,QI,scr1(1),QIT,dt)
    CALL advtndc_plumerise(mzp,QI,scr1,QIT,dt)

    !- hail/rain advection tendency (QHT)
    !   if(ak1 > 0. .or. ak2 > 0.) then

    scr1=QH
!!$    CALL fa_zc_plumerise(mzp,QH,scr1(1),vt3dc(1),vt3df(1),vt3dg(1), &
!!$                         vt3dk(1),vctr1,vctr2)
    CALL fa_zc_plumerise(mzp,QH,scr1,vt3dc,vt3df,vt3dg, &
                         vt3dk,vctr1,vctr2)

!!$    CALL advtndc_plumerise(mzp,QH,scr1(1),QHT,dt)
    CALL advtndc_plumerise(mzp,QH,scr1,QHT,dt)
    !   endif

    !- horizontal wind advection tendency (VEL_T)
    scr1=VEL_P
!!$    CALL fa_zc_plumerise(mzp		       &
!!$    			,VEL_P     ,scr1  (1)  &
!!$    			,vt3dc (1) ,vt3df (1)  &
!!$    			,vt3dg (1) ,vt3dk (1)  &
!!$    			,vctr1,vctr2	     )
    CALL fa_zc_plumerise(mzp		  &
    			,VEL_P, scr1  &
    			,vt3dc, vt3df &
    			,vt3dg, vt3dk &
    			,vctr1, vctr2	  )

!!$    CALL advtndc_plumerise(mzp,VEL_P,scr1(1),VEL_T,dt)
    CALL advtndc_plumerise(mzp,VEL_P,scr1,VEL_T,dt)

    !- vertical radius transport

    scr1=rad_p
!!$    CALL fa_zc_plumerise(mzp                   &
!!$             	        ,rad_p     ,scr1  (1)  &
!!$             	        ,vt3dc (1) ,vt3df (1)  &
!!$             	        ,vt3dg (1) ,vt3dk (1)  &
!!$             	        ,vctr1,vctr2	       )
    CALL fa_zc_plumerise(mzp           &
             	        ,rad_p, scr1   &
             	        ,vt3dc, vt3df  &
             	        ,vt3dg, vt3dk  &
             	        ,vctr1, vctr2	   )

!!$    CALL advtndc_plumerise(mzp,rad_p,scr1(1),rad_t,dt)
    CALL advtndc_plumerise(mzp,rad_p,scr1,rad_t,dt)


    RETURN

  END SUBROUTINE scl_advectc_plumerise

  !----------------------------------------------------------------------------
  !
  SUBROUTINE fa_zc_plumerise(m1,scp,scr1,vt3dc,vt3df,vt3dg,vt3dk,vctr1,vctr2)

    INTEGER, INTENT(IN)    :: m1
!!$    REAL,    INTENT(IN)    :: scp  (m1)
!!$    REAL,    INTENT(INOUT) :: scr1 (m1)
!!$    REAL,    INTENT(IN)    :: vt3dc(m1)
!!$    REAL,    INTENT(IN)    :: vt3df(m1)
!!$    REAL,    INTENT(INOUT) :: vt3dg(m1) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(IN)    :: vt3dk(m1)
!!$    REAL,    INTENT(IN)    :: vctr1(m1)
!!$    REAL,    INTENT(IN)    :: vctr2(m1)
    REAL,    INTENT(IN)    :: scp  (:)
    REAL,    INTENT(INOUT) :: scr1 (:)
    REAL,    INTENT(IN)    :: vt3dc(:)
    REAL,    INTENT(IN)    :: vt3df(:)
    REAL,    INTENT(INOUT) :: vt3dg(:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(IN)    :: vt3dk(:)
    REAL,    INTENT(IN)    :: vctr1(:)
    REAL,    INTENT(IN)    :: vctr2(:)


    INTEGER :: k
    REAL    :: dfact

    dfact = .5

    ! Compute scalar flux VT3DG
    DO k = 1,m1-1
       vt3dg(k) = vt3dc(k)                   &
            * (vctr1(k) * scr1(k)        &
            +  vctr2(k) * scr1(k+1)      &
            +  vt3df(k) * (scr1(k) - scr1(k+1)))
    ENDDO

    ! Modify fluxes to retain positive-definiteness on scalar quantities.
    !    If a flux will remove 1/2 quantity during a timestep,
    !    reduce to first order flux. This will remain positive-definite
    !    under the assumption that ABS(CFL(i)) + ABS(CFL(i-1)) < 1.0 if
    !    both fluxes are evacuating the box.

    DO k = 1,m1-1
       IF (vt3dc(k) .GT. 0.) THEN
          IF (vt3dg(k) * vt3dk(k)    .GT. dfact * scr1(k)) THEN
             vt3dg(k) = vt3dc(k) * scr1(k)
          ENDIF
       ELSEIF (vt3dc(k) .LT. 0.) THEN
          IF (-vt3dg(k) * vt3dk(k+1) .GT. dfact * scr1(k+1)) THEN
             vt3dg(k) = vt3dc(k) * scr1(k+1)
          ENDIF
       ENDIF

    ENDDO

    ! Compute flux divergence

    DO k = 2,m1-1
       scr1(k) = scr1(k)  &
            + vt3dk(k) * ( vt3dg(k-1) - vt3dg(k) &
            + scp  (k) * ( vt3dc(k)   - vt3dc(k-1)))
    ENDDO
    RETURN
  END SUBROUTINE fa_zc_plumerise

  !----------------------------------------------------------------------------
  !
  SUBROUTINE advtndc_plumerise(m1,scp,sca,sct,dtl)

    INTEGER, INTENT(IN)    :: m1
    REAL,    INTENT(IN)    :: dtl
!!$    REAL,    INTENT(IN)    :: scp(m1)
!!$    REAL,    INTENT(IN)    :: sca(m1)
!!$    REAL,    INTENT(INOUT) :: sct(m1)
    REAL,    INTENT(IN)    :: scp(:)
    REAL,    INTENT(IN)    :: sca(:)
    REAL,    INTENT(INOUT) :: sct(:)


    INTEGER :: k
    REAL    :: dtli

    dtli = 1. / dtl
    DO k = 2,m1-1
       sct(k) = sct(k) + (sca(k)-scp(k)) * dtli
    ENDDO
    RETURN
  END SUBROUTINE advtndc_plumerise

  !----------------------------------------------------------------------------
  !
  SUBROUTINE tend0_plumerise(nm1,wt,tt,qvt,qct,qht,qit,vel_t,rad_t)

    ! plumegen_coms
    INTEGER, INTENT(IN)    :: nm1
!!$    REAL,    INTENT(INOUT) :: wt   (nkp)  
!!$    REAL,    INTENT(INOUT) :: tt   (nkp)  
!!$    REAL,    INTENT(INOUT) :: qvt  (nkp) 
!!$    REAL,    INTENT(INOUT) :: qct  (nkp) 
!!$    REAL,    INTENT(INOUT) :: qht  (nkp) 
!!$    REAL,    INTENT(INOUT) :: qit  (nkp) 
!!$    REAL,    INTENT(INOUT) :: vel_t(nkp) 
!!$    REAL,    INTENT(INOUT) :: rad_t(nkp) 
    REAL,    INTENT(INOUT) :: wt   (:)  
    REAL,    INTENT(INOUT) :: tt   (:)  
    REAL,    INTENT(INOUT) :: qvt  (:) 
    REAL,    INTENT(INOUT) :: qct  (:) 
    REAL,    INTENT(INOUT) :: qht  (:) 
    REAL,    INTENT(INOUT) :: qit  (:) 
    REAL,    INTENT(INOUT) :: vel_t(:) 
    REAL,    INTENT(INOUT) :: rad_t(:) 


    wt   (1:nm1)  = 0.
    tt   (1:nm1)  = 0.
    qvt  (1:nm1)  = 0.
    qct  (1:nm1)  = 0.
    qht  (1:nm1)  = 0.
    qit  (1:nm1)  = 0.
    vel_t(1:nm1)  = 0.
    rad_t(1:nm1)  = 0.
    !sct (1:nm1)  = 0.
  END SUBROUTINE tend0_plumerise

  !     ****************************************************************
  SUBROUTINE scl_misc(m1,wbar,w,adiabat,alpha,radius,tt,t,te,qvt,qv, &
                      qvenv,qct,qc,qht,qh,qit,qi,vel_e,vel_p,vel_t,rad_p,&
                      rad_t)

    INTEGER, INTENT(IN)    :: m1
    ! plumegen_coms
    REAL,    INTENT(INOUT) :: wbar    ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: adiabat ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(IN)    :: alpha
!!$    REAL,    INTENT(IN)    :: w     (nkp)
!!$    REAL,    INTENT(IN)    :: radius(nkp)
!!$    REAL,    INTENT(INOUT) :: tt    (nkp)
!!$    REAL,    INTENT(IN)    :: t     (nkp)
!!$    REAL,    INTENT(IN)    :: te    (nkp)
!!$    REAL,    INTENT(INOUT) :: qvt   (nkp)
!!$    REAL,    INTENT(IN)    :: qv    (nkp)
!!$    REAL,    INTENT(IN)    :: qvenv (nkp)
!!$    REAL,    INTENT(INOUT) :: qct   (nkp)
!!$    REAL,    INTENT(IN)    :: qc    (nkp)
!!$    REAL,    INTENT(INOUT) :: qht   (nkp)
!!$    REAL,    INTENT(IN)    :: qh    (nkp)
!!$    REAL,    INTENT(INOUT) :: qit   (nkp)
!!$    REAL,    INTENT(IN)    :: qi    (nkp)
!!$    REAL,    INTENT(IN)    :: vel_e (nkp)
!!$    REAL,    INTENT(IN)    :: vel_p (nkp)
!!$    REAL,    INTENT(INOUT) :: vel_t (nkp)
!!$    REAL,    INTENT(INOUT) :: rad_T (nkp)
!!$    REAL,    INTENT(IN)    :: rad_p (nkp)
    REAL,    INTENT(IN)    :: w     (:)
    REAL,    INTENT(IN)    :: radius(:)
    REAL,    INTENT(INOUT) :: tt    (:)
    REAL,    INTENT(IN)    :: t     (:)
    REAL,    INTENT(IN)    :: te    (:)
    REAL,    INTENT(INOUT) :: qvt   (:)
    REAL,    INTENT(IN)    :: qv    (:)
    REAL,    INTENT(IN)    :: qvenv (:)
    REAL,    INTENT(INOUT) :: qct   (:)
    REAL,    INTENT(IN)    :: qc    (:)
    REAL,    INTENT(INOUT) :: qht   (:)
    REAL,    INTENT(IN)    :: qh    (:)
    REAL,    INTENT(INOUT) :: qit   (:)
    REAL,    INTENT(IN)    :: qi    (:)
    REAL,    INTENT(IN)    :: vel_e (:)
    REAL,    INTENT(IN)    :: vel_p (:)
    REAL,    INTENT(INOUT) :: vel_t (:)
    REAL,    INTENT(INOUT) :: rad_T (:)
    REAL,    INTENT(IN)    :: rad_p (:)


    REAL, PARAMETER :: g = 9.81, cp=1004.
    INTEGER         :: k
    REAL            :: dmdtm

    DO k=2,m1-1
       WBAR    = 0.5*(W(k)+W(k-1))  
       !-- dry adiabat
       ADIABAT = - WBAR * G / CP 
       !      
       !-- entrainment     
       DMDTM = 2. * ALPHA * ABS (WBAR) / RADIUS (k)  != (1/M)DM/DT

       !-- tendency temperature = adv + adiab + entrainment
       TT(k) = TT(K) + ADIABAT - DMDTM * ( T  (k) -    TE (k) ) 

       !-- tendency water vapor = adv  + entrainment
       QVT(K) = QVT(K)         - DMDTM * ( QV (k) - QVENV (k) )

       QCT(K) = QCT(K)	      - DMDTM * ( QC (k)  )
       QHT(K) = QHT(K)	      - DMDTM * ( QH (k)  )
       QIT(K) = QIT(K)	      - DMDTM * ( QI (k)  )


      !-- tendency horizontal speed = adv  + entrainment
      VEL_T(K) = VEL_T(K)     - DMDTM * ( VEL_P (k) - VEL_E (k) )

      !-- tendency horizontal speed = adv  + entrainment
      rad_t(K) = rad_t(K)     + 0.5*DMDTM*(6./5.)*RADIUS (k)

       !-- tendency gas/particle = adv  + entrainment
       !      SCT(K) = SCT(K)         - DMDTM * ( SC (k) -   SCE (k) )

    ENDDO
  END SUBROUTINE scl_misc

!     ****************************************************************
  SUBROUTINE scl_dyn_entrain(m1,wbar,w,adiabat,alpha,radius,tt,t, &
                             te,qvt,qv,qvenv,qct,qc,qht,qh,qit,qi,    &
                             vel_e,vel_p,vel_t,rad_p,rad_t)

    INTEGER, INTENT(IN)    :: m1
    ! plumegen_coms
    REAL,    INTENT(INOUT) :: wbar 
    REAL,    INTENT(INOUT) :: adiabat 
    REAL,    INTENT(IN)    :: alpha
!!$    REAL,    INTENT(IN)    :: w     (nkp)
!!$    REAL,    INTENT(IN)    :: radius(nkp)
!!$    REAL,    INTENT(INOUT) :: tt    (nkp)
!!$    REAL,    INTENT(IN)    :: t     (nkp)
!!$    REAL,    INTENT(IN)    :: te    (nkp)
!!$    REAL,    INTENT(INOUT) :: qvt   (nkp)
!!$    REAL,    INTENT(IN)    :: qv    (nkp)
!!$    REAL,    INTENT(IN)    :: qvenv (nkp)
!!$    REAL,    INTENT(INOUT) :: qct   (nkp)
!!$    REAL,    INTENT(IN)    :: qc    (nkp)
!!$    REAL,    INTENT(INOUT) :: qht   (nkp)
!!$    REAL,    INTENT(IN)    :: qh    (nkp)
!!$    REAL,    INTENT(INOUT) :: qit   (nkp)
!!$    REAL,    INTENT(IN)    :: qi    (nkp)
!!$    REAL,    INTENT(IN)    :: vel_e (nkp)
!!$    REAL,    INTENT(IN)    :: vel_p (nkp)
!!$    REAL,    INTENT(INOUT) :: vel_t (nkp)
!!$    REAL,    INTENT(INOUT) :: rad_T (nkp)
!!$    REAL,    INTENT(IN)    :: rad_p (nkp)
    REAL,    INTENT(IN)    :: w     (:)
    REAL,    INTENT(IN)    :: radius(:)
    REAL,    INTENT(INOUT) :: tt    (:)
    REAL,    INTENT(IN)    :: t     (:)
    REAL,    INTENT(IN)    :: te    (:)
    REAL,    INTENT(INOUT) :: qvt   (:)
    REAL,    INTENT(IN)    :: qv    (:)
    REAL,    INTENT(IN)    :: qvenv (:)
    REAL,    INTENT(INOUT) :: qct   (:)
    REAL,    INTENT(IN)    :: qc    (:)
    REAL,    INTENT(INOUT) :: qht   (:)
    REAL,    INTENT(IN)    :: qh    (:)
    REAL,    INTENT(INOUT) :: qit   (:)
    REAL,    INTENT(IN)    :: qi    (:)
    REAL,    INTENT(IN)    :: vel_e (:)
    REAL,    INTENT(IN)    :: vel_p (:)
    REAL,    INTENT(INOUT) :: vel_t (:)
    REAL,    INTENT(INOUT) :: rad_T (:)
    REAL,    INTENT(IN)    :: rad_p (:)


    REAL, PARAMETER :: g = 9.81, cp=1004., pi=3.1416
    INTEGER         :: k
    REAL            :: dmdtm

    DO k=2,m1-1
      !      
      !-- tendency horizontal radius from dyn entrainment
     	   !rad_t(K) = rad_t(K)   +	(vel_e(k)-vel_p(k)) /pi
     	    rad_t(K) = rad_t(K)   + ABS((vel_e(k)-vel_p(k)))/pi
      
      !-- entrainment	  
     	   !DMDTM = (2./3.1416)  *     (VEL_E (k) - VEL_P (k)) / RADIUS (k)  
     	    DMDTM = (2./3.1416)  *  ABS(VEL_E (k) - VEL_P (k)) / RADIUS (k)  
      
      !-- tendency horizontal speed  from dyn entrainment
     	    VEL_T(K) = VEL_T(K)     - DMDTM * ( VEL_P (k) - VEL_E (k) )
      
      !     if(VEL_P (k) - VEL_E (k) > 0.) cycle
      
      !-- tendency temperature  from dyn entrainment
     	    TT(k) = TT(K)	    - DMDTM * ( T (k) - TE  (k) ) 
      
      !-- tendency water vapor  from dyn entrainment
   	    QVT(K) = QVT(K)	    - DMDTM * ( QV (k) - QVENV (k) )
      
     	    QCT(K) = QCT(K)	    - DMDTM * ( QC (k)  )
     	    QHT(K) = QHT(K)	    - DMDTM * ( QH (k)  )
     	    QIT(K) = QIT(K)	    - DMDTM * ( QI (k)  )
      
      !-- tendency gas/particle  from dyn entrainment
      !	 SCT(K) = SCT(K)	 - DMDTM * ( SC (k) - SCE (k) )
    
    ENDDO
   END SUBROUTINE scl_dyn_entrain
  !     ****************************************************************

  SUBROUTINE  visc_W(m1,kmt,zt,visc,zm,w,t,qv,qh,qc,qi,wt,tt,qvt, &
                     qct,qht,qit,vel_p,vel_t,rad_p,rad_t)

    INTEGER, INTENT(IN)    :: m1
    INTEGER, INTENT(IN)    :: kmt
    ! plumegen_coms
!!$    REAL,    INTENT(IN)    :: zt   (nkp)
!!$    REAL,    INTENT(IN)    :: visc (nkp)
!!$    REAL,    INTENT(IN)    :: zm   (nkp)
!!$    REAL,    INTENT(IN)    :: w    (nkp)
!!$    REAL,    INTENT(IN)    :: t    (nkp)
!!$    REAL,    INTENT(IN)    :: qv   (nkp)
!!$    REAL,    INTENT(IN)    :: qh   (nkp)
!!$    REAL,    INTENT(IN)    :: qc   (nkp)
!!$    REAL,    INTENT(IN)    :: qi   (nkp)
!!$    REAL,    INTENT(INOUT) :: wt   (nkp)
!!$    REAL,    INTENT(INOUT) :: tt   (nkp)
!!$    REAL,    INTENT(INOUT) :: qvt  (nkp)
!!$    REAL,    INTENT(INOUT) :: qct  (nkp)
!!$    REAL,    INTENT(INOUT) :: qht  (nkp)
!!$    REAL,    INTENT(INOUT) :: qit  (nkp)  
!!$    REAL,    INTENT(IN)    :: vel_p(nkp)
!!$    REAL,    INTENT(INOUT) :: vel_t(nkp)
!!$    REAL,    INTENT(INOUT) :: rad_T(nkp)
!!$    REAL,    INTENT(IN)    :: rad_p(nkp)
    REAL,    INTENT(IN)    :: zt   (:)
    REAL,    INTENT(IN)    :: visc (:)
    REAL,    INTENT(IN)    :: zm   (:)
    REAL,    INTENT(IN)    :: w    (:)
    REAL,    INTENT(IN)    :: t    (:)
    REAL,    INTENT(IN)    :: qv   (:)
    REAL,    INTENT(IN)    :: qh   (:)
    REAL,    INTENT(IN)    :: qc   (:)
    REAL,    INTENT(IN)    :: qi   (:)
    REAL,    INTENT(INOUT) :: wt   (:)
    REAL,    INTENT(INOUT) :: tt   (:)
    REAL,    INTENT(INOUT) :: qvt  (:)
    REAL,    INTENT(INOUT) :: qct  (:)
    REAL,    INTENT(INOUT) :: qht  (:)
    REAL,    INTENT(INOUT) :: qit  (:)  
    REAL,    INTENT(IN)    :: vel_p(:)
    REAL,    INTENT(INOUT) :: vel_t(:)
    REAL,    INTENT(INOUT) :: rad_T(:)
    REAL,    INTENT(IN)    :: rad_p(:)


    INTEGER :: k, m2
    REAL    :: dz1t, dz1m, dz2t, dz2m, d2wdz, d2tdz, &
               d2qvdz, d2qhdz, d2qcdz, d2qidz,  &
               d2vel_pdz, d2rad_dz

    !srf--- 17/08/2005
    !m2=min(m1+deltak,kmt)
    !m2=MIN(m1,kmt)

    !do k=2,m1-1
    DO k=2,MIN(m1,kmt-1)!m2-1
       DZ1T   = 0.5*(ZT(K+1)-ZT(K-1))
       DZ2T   = VISC (k) / (DZ1T * DZ1T)  
       DZ1M   = 0.5*(ZM(K+1)-ZM(K-1))
       DZ2M   = VISC (k) / (DZ1M * DZ1M)  
       D2WDZ  = (W  (k + 1) - 2 * W  (k) + W  (k - 1) ) * DZ2M  
       D2TDZ  = (T  (k + 1) - 2 * T  (k) + T  (k - 1) ) * DZ2T  
       D2QVDZ = (QV (k + 1) - 2 * QV (k) + QV (k - 1) ) * DZ2T  
       D2QHDZ = (QH (k + 1) - 2 * QH (k) + QH (k - 1) ) * DZ2T 
       D2QCDZ = (QC (k + 1) - 2 * QC (k) + QC (k - 1) ) * DZ2T  
       D2QIDZ = (QI (k + 1) - 2 * QI (k) + QI (k - 1) ) * DZ2T  
       !D2SCDZ = (SC (k + 1) - 2 * SC (k) + SC (k - 1) ) * DZ2T 
       
       d2vel_pdz=(vel_P  (k + 1) - 2 * vel_P  (k) + vel_P  (k - 1) ) * DZ2T
       d2rad_dz =(rad_p  (k + 1) - 2 * rad_p  (k) + rad_p  (k - 1) ) * DZ2T

       WT(k) =   WT(k) + D2WDZ 
       TT(k) =   TT(k) + D2TDZ                          
       QVT(k) =  QVT(k) + D2QVDZ 
       QCT(k) =  QCT(k) + D2QCDZ 
       QHT(k) =  QHT(k) + D2QHDZ 
       QIT(k) =  QIT(k) + D2QIDZ     

       vel_t(k) =   vel_t(k) + d2vel_pdz
       rad_t(k) =   rad_t(k) + d2rad_dz
       
       !SCT(k) =  SCT(k) + D2SCDZ
       !print*,'W-VISC=',k,D2WDZ
    ENDDO

  END SUBROUTINE visc_W

  !     ****************************************************************

  SUBROUTINE update_plumerise(m1,varn,wt,dt,tt,qvt,qct,qht,w,t,qv,qc, &
                              qh,qit,qi, vel_p,vel_t,rad_p,rad_t)

    INTEGER ,         INTENT(IN)    :: m1
    CHARACTER(len=*), INTENT(IN)    :: varn
    ! plumegen_coms
    REAL,             INTENT(IN)    :: dt
!!$    REAL,             INTENT(IN)    :: wt   (nkp)
!!$    REAL,             INTENT(IN)    :: tt   (nkp)
!!$    REAL,             INTENT(IN)    :: qvt  (nkp)
!!$    REAL,             INTENT(IN)    :: qct  (nkp)
!!$    REAL,             INTENT(IN)    :: qht  (nkp)
!!$    REAL,             INTENT(INOUT) :: w    (nkp)
!!$    REAL,             INTENT(INOUT) :: t    (nkp)
!!$    REAL,             INTENT(INOUT) :: qv   (nkp)
!!$    REAL,             INTENT(INOUT) :: qc   (nkp)
!!$    REAL,             INTENT(INOUT) :: qh   (nkp)
!!$    REAL,             INTENT(IN)    :: qit  (nkp) ! (DMK) alterado (INOUT) (IN)
!!$    REAL,             INTENT(INOUT) :: qi   (nkp)
!!$    REAL,             INTENT(INOUT) :: vel_p(nkp)
!!$    REAL,             INTENT(IN)    :: vel_t(nkp)
!!$    REAL,             INTENT(IN)    :: rad_T(nkp)
!!$    REAL,             INTENT(INOUT) :: rad_p(nkp)
    REAL,             INTENT(IN)    :: wt   (:)
    REAL,             INTENT(IN)    :: tt   (:)
    REAL,             INTENT(IN)    :: qvt  (:)
    REAL,             INTENT(IN)    :: qct  (:)
    REAL,             INTENT(IN)    :: qht  (:)
    REAL,             INTENT(INOUT) :: w    (:)
    REAL,             INTENT(INOUT) :: t    (:)
    REAL,             INTENT(INOUT) :: qv   (:)
    REAL,             INTENT(INOUT) :: qc   (:)
    REAL,             INTENT(INOUT) :: qh   (:)
    REAL,             INTENT(IN)    :: qit  (:) ! (DMK) alterado (INOUT) (IN)
    REAL,             INTENT(INOUT) :: qi   (:)
    REAL,             INTENT(INOUT) :: vel_p(:)
    REAL,             INTENT(IN)    :: vel_t(:)
    REAL,             INTENT(IN)    :: rad_T(:)
    REAL,             INTENT(INOUT) :: rad_p(:)


    INTEGER :: k

    IF(varn == 'W') THEN

       DO k=2,m1-1
          W(k) =  W(k) +  WT(k) * DT  
       ENDDO
       RETURN

    ELSE 
       DO k=2,m1-1
          T(k) =  T(k) +  TT(k) * DT  

          QV(k) = QV(k) + QVT(k) * DT  

          QC(k) = QC(k) + QCT(k) * DT !cloud drops travel with air 
          QH(k) = QH(k) + QHT(k) * DT  
          QI(k) = QI(k) + QIT(k) * DT 
          ! SC(k) = SC(k) + SCT(k) * DT 

          !srf---18jun2005  
          QV(k) = MAX(0., QV(k))
          QC(k) = MAX(0., QC(k))
          QH(k) = MAX(0., QH(k))
          QI(k) = MAX(0., QI(k))
	  
	  
	  VEL_P(k) =  VEL_P(k) + VEL_T(k) * DT  

          rad_p(k) =  rad_p(k) + rad_t(k) * DT  

          ! SC(k) = max(0., SC(k))

       ENDDO
    ENDIF
  END SUBROUTINE update_plumerise

  !----------------------------------------------------------------------------
  !
  SUBROUTINE fallpart(m1,rho,vth,vhrel,w,cvh,vti,cvi,qh,qi,zm,qht,qit)

    INTEGER, INTENT(IN)    :: m1
    ! plumegen_coms
    REAL,    INTENT(INOUT) :: vhrel   ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(IN)    :: rho(nkp)
!!$    REAL,    INTENT(INOUT) :: vth(nkp)
!!$    REAL,    INTENT(IN)    :: w  (nkp)
!!$    REAL,    INTENT(INOUT) :: cvh(nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(INOUT) :: vti(nkp)
!!$    REAL,    INTENT(INOUT) :: cvi(nkp) ! (DMK) alterado (OUT) para (INOUT)
!!$    REAL,    INTENT(IN)    :: qh (nkp)
!!$    REAL,    INTENT(IN)    :: qi (nkp)
!!$    REAL,    INTENT(IN)    :: zm (nkp)
!!$    REAL,    INTENT(INOUT) :: qht(nkp)
!!$    REAL,    INTENT(INOUT) :: qit(nkp)
    REAL,    INTENT(IN)    :: rho(:)
    REAL,    INTENT(INOUT) :: vth(:)
    REAL,    INTENT(IN)    :: w  (:)
    REAL,    INTENT(INOUT) :: cvh(:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(INOUT) :: vti(:)
    REAL,    INTENT(INOUT) :: cvi(:) ! (DMK) alterado (OUT) para (INOUT)
    REAL,    INTENT(IN)    :: qh (:)
    REAL,    INTENT(IN)    :: qi (:)
    REAL,    INTENT(IN)    :: zm (:)
    REAL,    INTENT(INOUT) :: qht(:)
    REAL,    INTENT(INOUT) :: qit(:)


    !srf==================================
    !   verificar se o gradiente esta correto 
    !  
    !srf==================================
    !
    !     XNO=1.E7  [m**-4] median volume diameter raindrop,Kessler
    !     VC = 38.3/(XNO**.125), median volume fallspeed eqn., Kessler
    !     for ice, see (OT18), use F0=0.75 per argument there. rho*q
    !     values are in g/m**3, velocities in m/s

    REAL, PARAMETER :: VCONST = 5.107387, EPS = 0.622, F0 = 0.75  
    REAL, PARAMETER :: G = 9.81, CP = 1004.

    INTEGER         :: k
    REAL            :: vtc, dfhz, dfiz, dz1
    REAL            :: virel ! (DMK) scratch

    !
    DO k=2,m1-1

       VTC = VCONST * RHO (k) **.125   ! median volume fallspeed (KTable4)

       !  hydrometeor assembly velocity calculations (K Table4)
       !  VTH(k)=-VTC*QH(k)**.125  !median volume fallspeed, water            
       VTH (k) = - 4.	    !small variation with qh

       VHREL = W (k) + VTH (k)  !relative to surrounding cloud

       !  rain ventilation coefficient for evaporation
       CVH(k) = 1.6 + 0.57E-3 * (ABS (VHREL) ) **1.5  
       !
       !  VTI(k)=-VTC*F0*QI(k)**.125    !median volume fallspeed,ice          
       VTI (k) = - 3.                !small variation with qi

       VIREL = W (k) + VTI (k)       !relative to surrounding cloud
       !
       !  ice ventilation coefficient for sublimation
       CVI(k) = 1.6 + 0.57E-3 * (ABS (VIREL) ) **1.5 / F0  
       !
       !
       IF (VHREL.GE.0.0) THEN  
          DFHZ=QH(k)*(RHO(k  )*VTH(k  )-RHO(k-1)*VTH(k-1))/RHO(k-1)
       ELSE  
          DFHZ=QH(k)*(RHO(k+1)*VTH(k+1)-RHO(k  )*VTH(k  ))/RHO(k)
       ENDIF
       !
       !
       IF (VIREL.GE.0.0) THEN  
          DFIZ=QI(k)*(RHO(k  )*VTI(k  )-RHO(k-1)*VTI(k-1))/RHO(k-1)
       ELSE   
          DFIZ=QI(k)*(RHO(k+1)*VTI(k+1)-RHO(k  )*VTI(k  ))/RHO(k)
       ENDIF

       DZ1=ZM(K)-ZM(K-1)

       qht(k) = qht(k) - DFHZ / DZ1 !hydrometeors don't

       qit(k) = qit(k) - DFIZ / DZ1  !nor does ice? hail, what about

    ENDDO

  END SUBROUTINE fallpart



  !----------------------------------------------------------------------------
  !
  SUBROUTINE printout(izprint,nrectotal,mintime,dt,time,ztop,pe,t,te,qv, &
                      qsat,qc,qh,qi,zt,w,vth,sc,qvenv,nrec)  

    INTEGER, INTENT(IN)    :: izprint
    INTEGER, INTENT(IN)    :: nrectotal
    ! plumegen_coms
    INTEGER, INTENT(IN)    :: mintime
    REAL,    INTENT(IN)    :: dt
    REAL,    INTENT(IN)    :: time
    REAL,    INTENT(IN)    :: ztop
!!$    REAL,    INTENT(IN)    :: pe   (nkp)
!!$    REAL,    INTENT(IN)    :: t    (nkp)
!!$    REAL,    INTENT(IN)    :: te   (nkp)
!!$    REAL,    INTENT(IN)    :: qv   (nkp)
!!$    REAL,    INTENT(IN)    :: qsat (nkp)
!!$    REAL,    INTENT(IN)    :: qc   (nkp)
!!$    REAL,    INTENT(IN)    :: qh   (nkp)
!!$    REAL,    INTENT(IN)    :: qi   (nkp)
!!$    REAL,    INTENT(IN)    :: zt   (nkp)
!!$    REAL,    INTENT(IN)    :: w    (nkp)
!!$    REAL,    INTENT(IN)    :: vth  (nkp)
!!$    REAL,    INTENT(IN)    :: sc   (nkp)
!!$    REAL,    INTENT(IN)    :: qvenv(nkp)
    REAL,    INTENT(IN)    :: pe   (:)
    REAL,    INTENT(IN)    :: t    (:)
    REAL,    INTENT(IN)    :: te   (:)
    REAL,    INTENT(IN)    :: qv   (:)
    REAL,    INTENT(IN)    :: qsat (:)
    REAL,    INTENT(IN)    :: qc   (:)
    REAL,    INTENT(IN)    :: qh   (:)
    REAL,    INTENT(IN)    :: qi   (:)
    REAL,    INTENT(IN)    :: zt   (:)
    REAL,    INTENT(IN)    :: w    (:)
    REAL,    INTENT(IN)    :: vth  (:)
    REAL,    INTENT(IN)    :: sc   (:)
    REAL,    INTENT(IN)    :: qvenv(:)
    ! save attribute
    INTEGER, INTENT(INOUT) :: nrec


    REAL, PARAMETER :: tmelt = 273.3
    INTEGER         :: ko, interval
    REAL            :: pea, btmp, etmp, vap1, vap2, gpkc, gpkh, gpki, deficit

    interval = 1              !debug time interval,min

    !
    IF (IZPRINT.EQ.0) RETURN  

    IF(MINTIME == 1) nrec = 0
    !
    WRITE (2, 430) MINTIME, DT, TIME  
    WRITE (2, 431) ZTOP  
    WRITE (2, 380)  
    !
    ! do the print
    !
    DO 390 KO = 1, nrectotal, interval  

       PEA = PE (KO) * 10.       !pressure is stored in decibars(kPa),print in mb;
       BTMP = T (KO) - TMELT     !temps in Celsius
       ETMP = T (KO) - TE (KO)   !temperature excess
       VAP1 = QV (KO)   * 1000.  !printout in g/kg for all water,
       VAP2 = QSAT (KO) * 1000.  !vapor (internal storage is in g/g)
       GPKC = QC (KO)   * 1000.  !cloud water
       GPKH = QH (KO)   * 1000.  !raindrops
       GPKI = QI (KO)   * 1000.  !ice particles 
       DEFICIT = VAP2 - VAP1     !vapor deficit
       !
       WRITE (2, 400) zt(KO)/1000., PEA, W (KO), BTMP, ETMP, VAP1, &
            VAP2, GPKC, GPKH, GPKI, VTH (KO), SC(KO)
       !
       !
       !                                    !end of printout

390    CONTINUE		  

       nrec=nrec+1
       WRITE (19,rec=nrec) (W (KO), KO=1,nrectotal)
       nrec=nrec+1
       WRITE (19,rec=nrec) (T (KO), KO=1,nrectotal)
       nrec=nrec+1
       WRITE (19,rec=nrec) (TE(KO), KO=1,nrectotal)
       nrec=nrec+1
       WRITE (19,rec=nrec) (QV(KO)*1000., KO=1,nrectotal)
       nrec=nrec+1
       WRITE (19,rec=nrec) (QC(KO)*1000., KO=1,nrectotal)
       nrec=nrec+1
       WRITE (19,rec=nrec) (QH(KO)*1000., KO=1,nrectotal)
       nrec=nrec+1
       WRITE (19,rec=nrec) (QI(KO)*1000., KO=1,nrectotal)
       nrec=nrec+1
       !   write (19,rec=nrec) (SC(KO), KO=1,nrectotal)
       WRITE (19,rec=nrec) (QSAT(KO)*1000., KO=1,nrectotal)
       nrec=nrec+1
       WRITE (19,rec=nrec) (QVENV(KO)*1000., KO=1,nrectotal)

       PRINT*,'ntimes=',nrec/(9)
       !
       RETURN  
       !
       ! ************** FORMATS *********************************************
       !
380    FORMAT(/,' Z(KM) P(MB) W(MPS) T(C)  T-TE   VAP   SAT   QC    QH' &
            '     QI    VTH(MPS) SCAL'/)
       !
400    FORMAT(1H , F4.1,F8.2,F8.2,F7.1,6F6.2,F7.2,1X,F6.2)  
       !
430    FORMAT(1H ,//I5,' MINUTES       DT= ',F6.2,' SECONDS   TIME= ' &
            ,F8.2,' SECONDS')
431    FORMAT(' ZTOP= ',F10.2)  
       !
     END SUBROUTINE printout
     !
     ! *********************************************************************

     SUBROUTINE WATERBAL(qc,l,qh,qi,qv,t,qsat,wbar,dqsdz,dt,rho,est,cvi)

       ! plumegen_coms
       INTEGER, INTENT(IN)    :: l
       REAL,    INTENT(IN)    :: wbar
       REAL,    INTENT(IN)    :: dqsdz
       REAL,    INTENT(IN)    :: dt
!!$       REAL,    INTENT(INOUT) :: qc  (nkp)
!!$       REAL,    INTENT(INOUT) :: qh  (nkp)
!!$       REAL,    INTENT(INOUT) :: qi  (nkp)
!!$       REAL,    INTENT(INOUT) :: qv  (nkp)
!!$       REAL,    INTENT(INOUT) :: t   (nkp)
!!$       REAL,    INTENT(IN)    :: qsat(nkp)
!!$       REAL,    INTENT(IN)    :: rho (nkp)
!!$       REAL,    INTENT(IN)    :: est (nkp)
!!$       REAL,    INTENT(IN)    :: cvi (nkp)
       REAL,    INTENT(INOUT) :: qc  (:)
       REAL,    INTENT(INOUT) :: qh  (:)
       REAL,    INTENT(INOUT) :: qi  (:)
       REAL,    INTENT(INOUT) :: qv  (:)
       REAL,    INTENT(INOUT) :: t   (:)
       REAL,    INTENT(IN)    :: qsat(:)
       REAL,    INTENT(IN)    :: rho (:)
       REAL,    INTENT(IN)    :: est (:)
       REAL,    INTENT(IN)    :: cvi (:)
       !

       IF (QC (L) .LE.1.0E-10) QC (L) = 0.  !DEFEAT UNDERFLOW PROBLEM
       IF (QH (L) .LE.1.0E-10) QH (L) = 0.  
       IF (QI (L) .LE.1.0E-10) QI (L) = 0.  
       !
       CALL EVAPORATE(qc,qh,qi,qv,t,qsat,l,wbar,dqsdz,dt,rho,est,cvi)  !vapor to cloud,cloud to vapor  
       !                             
       CALL SUBLIMATE(t,l,qv,qsat,rho,qi,est,dt)  !vapor to ice  
       !                            
       CALL GLACIATE(qh,l,qv,qsat,t,dt,qi) !rain to ice 

       CALL MELT(qi,l,t,dt,rho,cvi,qh)  !ice to rain
       !         
       !if(ak1 > 0. .or. ak2 > 0.) &
       CALL CONVERT(t,l,qc,rho,qh,dt) !(auto)conversion and accretion 
       !CALL CONVERT2 () !(auto)conversion and accretion 
       !

       RETURN  
     END SUBROUTINE WATERBAL

     ! *********************************************************************
     SUBROUTINE EVAPORATE(qc,qh,qi,qv,t,qsat,l,wbar,dqsdz,dt,rho,est,cvi)  
       !
       !- evaporates cloud,rain and ice to saturation
       !

       ! plumegen_coms
       INTEGER, INTENT(IN)    :: l
       REAL,    INTENT(IN)    :: wbar
       REAL,    INTENT(IN)    :: dqsdz
       REAL,    INTENT(IN)    :: dt
!!$       REAL,    INTENT(INOUT) :: qc  (nkp)
!!$       REAL,    INTENT(INOUT) :: qh  (nkp)
!!$       REAL,    INTENT(INOUT) :: qi  (nkp)
!!$       REAL,    INTENT(INOUT) :: qv  (nkp)
!!$       REAL,    INTENT(INOUT) :: t   (nkp)
!!$       REAL,    INTENT(IN)    :: qsat(nkp)
!!$       REAL,    INTENT(IN)    :: rho (nkp)
!!$       REAL,    INTENT(IN)    :: est (nkp)
!!$       REAL,    INTENT(IN)    :: cvi (nkp)
       REAL,    INTENT(INOUT) :: qc  (:)
       REAL,    INTENT(INOUT) :: qh  (:)
       REAL,    INTENT(INOUT) :: qi  (:)
       REAL,    INTENT(INOUT) :: qv  (:)
       REAL,    INTENT(INOUT) :: t   (:)
       REAL,    INTENT(IN)    :: qsat(:)
       REAL,    INTENT(IN)    :: rho (:)
       REAL,    INTENT(IN)    :: est (:)
       REAL,    INTENT(IN)    :: cvi (:)


       !
       !     XNO=10.0E06
       !     HERC = 1.93*1.E-6*XN035        !evaporation constant
       !
       REAL, PARAMETER :: HERC = 5.44E-4, CP = 1.004, HEATCOND = 2.5E3  
       REAL, PARAMETER :: HEATSUBL = 2834., TMELT = 273., TFREEZE = 269.3
       REAL, PARAMETER :: FRC = HEATCOND / CP, SRC = HEATSUBL / CP

       REAL            :: evhdt, evidt, evrate, evap, sd, &
                          quant, dividend, divisor, devidt

       !
       !
       SD = QSAT (L) - QV (L)  !vapor deficit
       IF (SD.EQ.0.0)  RETURN  
       !IF (abs(SD).lt.1.e-7)  RETURN  

       EVHDT = 0.  
       EVIDT = 0.  
       !evrate =0.; evap=0.; sd=0.0; quant=0.0; dividend=0.0; divisor=0.0; devidt=0.0

       EVRATE = ABS (WBAR * DQSDZ)   !evaporation rate (Kessler 8.32)
       EVAP = EVRATE * DT            !what we can get in DT

       IF (SD.LE.0.0) THEN  !     condense. SD is negative

          IF (EVAP.GE.ABS (SD) ) THEN    !we get it all

             QC (L) = QC  (L) - SD  !deficit,remember?
             QV (L) = QSAT(L)       !set the vapor to saturation  
             T  (L) = T   (L) - SD * FRC  !heat gained through condensation
             !per gram of dry air
             RETURN  

          ELSE  

             QC (L) = QC (L) + EVAP         !get what we can in DT 
             QV (L) = QV (L) - EVAP         !remove it from the vapor
             T  (L) = T  (L) + EVAP * FRC   !get some heat

             RETURN  

          ENDIF
          !
       ELSE                                !SD is positive, need some water
          !
          ! not saturated. saturate if possible. use everything in order
          ! cloud, rain, ice. SD is positive

          IF (EVAP.LE.QC (L) ) THEN        !enough cloud to last DT  
             !

             IF (SD.LE.EVAP) THEN          !enough time to saturate

                QC (L) = QC (L) - SD       !remove cloud                                          
                QV (L) = QSAT (L)          !saturate
                T (L) = T (L) - SD * FRC   !cool the parcel                                          
                RETURN  !done
                !

             ELSE   !not enough time

                SD = SD-EVAP               !use what there is
                QV (L) = QV (L) + EVAP     !add vapor
                T (L) = T (L) - EVAP * FRC !lose heat
                QC (L) = QC (L) - EVAP     !lose cloud
                !go on to rain.                                      
             ENDIF
             !
          ELSE                !not enough cloud to last DT
             !      
             IF (SD.LE.QC (L) ) THEN   !but there is enough to sat

                QV (L) = QSAT (L)  !use it
                QC (L) = QC (L) - SD  
                T  (L) = T (L) - SD * FRC  
                RETURN  

             ELSE            !not enough to sat
                SD = SD-QC (L)  
                QV (L) = QV (L) + QC (L)  
                T  (L) = T (L) - QC (L) * FRC         
                QC (L) = 0.0  !all gone

             ENDIF       !on to rain                           
          ENDIF          !finished with cloud
          !
          !  but still not saturated, so try to use some rain
          !  this is tricky, because we only have time DT to evaporate. 
          !  if there is enough rain, we can evaporate it for dt. ice can 
          !  also sublimate at the same time. there is a compromise 
          !  here.....use rain first, then
          !  ice. saturation may not be possible in one DT time.
          !  rain evaporation rate (W12),(OT25),(K Table 4). 
          !  evaporate rain first
          !  sd is still positive or we wouldn't be here.

          IF (QH (L) .LE.1.E-10) GOTO 33                                  

          !srf-25082005
          !  QUANT = ( QC (L)  + QV (L) - QSAT (L) ) * RHO (L)   !g/m**3
          QUANT = ( QSAT (L)- QC (L) - QV (L)   ) * RHO (L)   !g/m**3
          !
          EVHDT = (DT * HERC * (QUANT) * (QH (L) * RHO (L) ) **.65) / RHO (L)
          !             rain evaporation in time DT

          IF (EVHDT.LE.QH (L) ) THEN           !enough rain to last DT

             IF (SD.LE.EVHDT) THEN  		!enough time to saturate
                QH (L) = QH (L) - SD   	!remove rain	  
                QV (L) = QSAT (L)  		!saturate	  
                T (L) = T (L) - SD * FRC  	!cool the parcel	  

                RETURN  			!done
                !                       
             ELSE                               !not enough time
                SD = SD-EVHDT  		 !use what there is
                QV (L) = QV (L) + EVHDT  	 !add vapor
                T (L) = T (L) - EVHDT * FRC  	 !lose heat
                QH (L) = QH (L) - EVHDT  	 !lose rain

             ENDIF  				  !go on to ice.
             !                                    
          ELSE  !not enough rain to last DT
             !
             IF (SD.LE.QH (L) ) THEN             !but there is enough to sat
                QV (L) = QSAT (L)                !use it
                QH (L) = QH (L) - SD  
                T (L) = T (L) - SD * FRC  
                RETURN  
                !                            
             ELSE                              !not enough to sat
                SD = SD-QH (L)  
                QV (L) = QV (L) + QH (L)  
                T (L) = T (L) - QH (L) * FRC    
                QH (L) = 0.0                   !all gone

             ENDIF                             !on to ice
             !

          ENDIF                                !finished with rain
          !
          !
          !  now for ice
          !  equation from (OT); correction factors for units applied
          !
33        CONTINUE
          IF (QI (L) .LE.1.E-10) RETURN            !no ice there
          !
          DIVIDEND = ( (1.E6 / RHO (L) ) **0.475) * (SD / QSAT (L) &
               - 1) * (QI (L) **0.525) * 1.13
          DIVISOR = 7.E5 + 4.1E6 / (10. * EST (L) )  

          DEVIDT = - CVI(L) * DIVIDEND / DIVISOR   !rate of change

          EVIDT = DEVIDT * DT                      !what we could get
          !
          ! logic here is identical to rain. could get fancy and make 
          ! subroutine but duplication of code is easier. God bless the 
          ! screen editor.
          !

          IF (EVIDT.LE.QI (L) ) THEN             !enough ice to last DT
             !

             IF (SD.LE.EVIDT) THEN  		  !enough time to saturate
                QI (L) = QI (L) - SD   	  !remove ice
                QV (L) = QSAT (L)  		  !saturate
                T (L) = T (L) - SD * SRC  	  !cool the parcel

                RETURN  			  !done
                !

             ELSE                                !not enough time

                SD = SD-EVIDT  		  !use what there is
                QV (L) = QV (L) + EVIDT  	  !add vapor
                T (L) =  T (L) - EVIDT * SRC  	  !lose heat
                QI (L) = QI (L) - EVIDT  	  !lose ice

             ENDIF  				  !go on,unsatisfied
             !                                          
          ELSE                                   !not enough ice to last DT
             !                                         
             IF (SD.LE.QI (L) ) THEN             !but there is enough to sat

                QV (L) = QSAT (L)                !use it
                QI (L) = QI   (L) - SD  
                T (L) =  T   (L) - SD * SRC  

                RETURN  
                !
             ELSE                                 !not enough to sat
                SD = SD-QI (L)  
                QV (L) = QV (L) + QI (L)  
                T (L) = T (L) - QI (L) * SRC             
                QI (L) = 0.0                      !all gone

             ENDIF                                !on to better things
             !finished with ice
          ENDIF
          !                                 
       ENDIF                                   !finished with the SD decision
       !
       RETURN  
       !
     END SUBROUTINE EVAPORATE

     !
     ! *********************************************************************
     SUBROUTINE CONVERT (t,l,qc,rho,qh,dt)  
       !
       !- ACCRETION AND AUTOCONVERSION
       !

       ! plumegen_coms
       INTEGER, INTENT(IN)    :: l
       REAL,    INTENT(IN)    :: dt
!!$       REAL,    INTENT(IN)    :: t  (nkp)
!!$       REAL,    INTENT(INOUT) :: qc (nkp)
!!$       REAL,    INTENT(IN)    :: rho(nkp)
!!$       REAL,    INTENT(INOUT) :: qh (nkp)
       REAL,    INTENT(IN)    :: t  (:)
       REAL,    INTENT(INOUT) :: qc (:)
       REAL,    INTENT(IN)    :: rho(:)
       REAL,    INTENT(INOUT) :: qh (:)


       REAL,    PARAMETER ::  AK1 = 0.001    !conversion rate constant
       REAL,    PARAMETER ::  AK2 = 0.0052   !collection (accretion) rate
       REAL,    PARAMETER ::  TH  = 0.5      !Kessler threshold
       INTEGER, PARAMETER ::  iconv = 1      !- Kessler conversion (=0)
       !real,   parameter :: ANBASE =  50.!*1.e+6   !Berry-number at cloud base #/m^3(maritime)
       REAL,    PARAMETER :: ANBASE =100000.!*1.e+6 !Berry-number at cloud base #/m^3(continental)
       !real,   parameter :: BDISP = 0.366          !Berry--size dispersion (maritime)
       REAL,    PARAMETER :: BDISP = 0.146          !Berry--size dispersion (continental)
       REAL,    PARAMETER :: TFREEZE = 269.3        !ice formation temperature
       !
       REAL               :: accrete, con, q, h, total


       IF (T (L)  .LE. TFREEZE) RETURN  !process not allowed above ice
       !
       IF (QC (L) .EQ. 0.     ) RETURN  

       ACCRETE = 0.  
       CON = 0.  
       Q = RHO (L) * QC (L)  
       H = RHO (L) * QH (L)  
       !
       !     selection rules
       !                         
       !            
       IF (QH (L) .GT. 0.     ) ACCRETE = AK2 * Q * (H**.875)  !accretion, Kessler
       !
       IF (ICONV.NE.0) THEN   !select Berry or Kessler
          !
          !old   BC1 = 120.  
          !old   BC2 = .0266 * ANBASE * 60.  
          !old   CON = BDISP * Q * Q * Q / (BC1 * Q * BDISP + BC2) 	  

          CON = Q*Q*Q*BDISP/(60.*(5.*Q*BDISP+0.0366*ANBASE))
          !
       ELSE  
          !                             
          !   CON = AK1 * (Q - TH)   !Kessler autoconversion rate
          !      
          !   IF (CON.LT.0.0) CON = 0.0   !havent reached threshold

          CON = MAX(0.,AK1 * (Q - TH)) ! versao otimizada
          !
       ENDIF
       !
       !
       TOTAL = (CON + ACCRETE) * DT / RHO (L)  

       !
       IF (TOTAL.LT.QC (L) ) THEN  
          !
          QC (L) = QC (L) - TOTAL  
          QH (L) = QH (L) + TOTAL    !no phase change involved
          RETURN  
          !
       ELSE  
          !              
          QH (L) = QH (L) + QC (L)    !uses all there is
          QC (L) = 0.0  
          !
       ENDIF
       !
       RETURN  
       !
     END SUBROUTINE CONVERT

     ! ice - effect on temperature
     !      TTD = 0.0 
     !      TTE = 0.0  
     !       CALL ICE(QSATW,QSATE,Y(1),Y(2),Y(3), &
     !               TTA,TTB,TTC,DZ,ROH,D,C,TTD,TTE)
     !       DYDX(1) = DYDX(1) + TTD  + TTE ! DT/DZ on Temp
     !
     !**********************************************************************
     !
     SUBROUTINE SUBLIMATE(t,l,qv,qsat,rho,qi,est,dt)  
       !
       ! ********************* VAPOR TO ICE (USE EQUATION OT22)***************

       ! plumegen_coms
       INTEGER, INTENT(IN)    :: l
       REAL,    INTENT(IN)    :: dt
!!$       REAL,    INTENT(INOUT) :: t   (nkp)
!!$       REAL,    INTENT(INOUT) :: qv  (nkp)
!!$       REAL,    INTENT(IN)    :: qsat(nkp)
!!$       REAL,    INTENT(IN)    :: rho (nkp)
!!$       REAL,    INTENT(INOUT) :: qi  (nkp)
!!$       REAL,    INTENT(IN)    :: est (nkp)
       REAL,    INTENT(INOUT) :: t   (:)
       REAL,    INTENT(INOUT) :: qv  (:)
       REAL,    INTENT(IN)    :: qsat(:)
       REAL,    INTENT(IN)    :: rho (:)
       REAL,    INTENT(INOUT) :: qi  (:)
       REAL,    INTENT(IN)    :: est (:)


       !
       REAL, PARAMETER :: EPS = 0.622, HEATFUS = 334., HEATSUBL = 2834., CP = 1.004
       REAL, PARAMETER :: SRC = HEATSUBL / CP, FRC = HEATFUS / CP, TMELT = 273.3
       REAL, PARAMETER :: TFREEZE = 269.3

       REAL            :: dtsubh, dividend, divisor, subl
       !
       DTSUBH = 0.  
       !
       !selection criteria for sublimation
       IF (T (L)  .GT. TFREEZE  ) RETURN  
       IF (QV (L) .LE. QSAT (L) ) RETURN  
       !
       !     from (OT); correction factors for units applied
       !
       DIVIDEND = ( (1.E6 / RHO (L) ) **0.475) * (QV (L) / QSAT (L) &
            - 1) * (QI (L) **0.525) * 1.13
       DIVISOR = 7.E5 + 4.1E6 / (10. * EST (L) )  
       !

       DTSUBH = ABS (DIVIDEND / DIVISOR)   !sublimation rate
       SUBL = DTSUBH * DT                  !and amount possible
       !
       !     again check the possibilities
       !
       IF (SUBL.LT.QV (L) ) THEN  
          !
          QV (L) = QV (L) - SUBL             !lose vapor
          QI (L) = QI (L) + SUBL  	      !gain ice
          T (L) = T (L) + SUBL * SRC         !energy change, warms air

          !print*,'5',l,qi(l),SUBL

          RETURN  
          !
       ELSE  
          !                                     
          QI (L) = QV (L)                    !use what there is
          T  (L) = T (L) + QV (L) * SRC      !warm the air
          QV (L) = 0.0  
          !print*,'6',l,qi(l)
          !
       ENDIF
       !
       RETURN  
     END SUBROUTINE SUBLIMATE

     !
     ! *********************************************************************
     !
     SUBROUTINE GLACIATE(qh,l,qv,qsat,t,dt,qi)
       !
       ! *********************** CONVERSION OF RAIN TO ICE *******************
       !     uses equation OT 16, simplest. correction from W not applied, but
       !     vapor pressure differences are supplied.
       !

       ! plumegen_coms
       INTEGER, INTENT(IN)    :: l
       REAL   , INTENT(IN)    :: dt
!!$       REAL   , INTENT(INOUT) :: qh  (nkp)
!!$       REAL   , INTENT(IN)    :: qv  (nkp)
!!$       REAL   , INTENT(IN)    :: qsat(nkp)
!!$       REAL   , INTENT(INOUT) :: t   (nkp)
!!$       REAL   , INTENT(INOUT) :: qi  (nkp)
       REAL   , INTENT(INOUT) :: qh  (:)
       REAL   , INTENT(IN)    :: qv  (:)
       REAL   , INTENT(IN)    :: qsat(:)
       REAL   , INTENT(INOUT) :: t   (:)
       REAL   , INTENT(INOUT) :: qi  (:)


       !
       REAL, PARAMETER :: HEATFUS = 334., CP = 1.004, EPS = 0.622, HEATSUBL = 2834.
       REAL, PARAMETER :: FRC = HEATFUS / CP, FRS = HEATSUBL / CP, TFREEZE =  269.3
       REAL, PARAMETER :: GLCONST = 0.025   !glaciation time constant, 1/sec
       REAL            :: dfrzh
       !

       DFRZH = 0.    !rate of mass gain in ice
       !
       !selection rules for glaciation
       IF (QH (L) .LE. 0.       ) RETURN  
       IF (QV (L) .LT. QSAT (L) ) RETURN                                        
       IF (T  (L) .GT. TFREEZE  ) RETURN  
       !
       !      NT=TMELT-T(L)
       !      IF (NT.GT.50) NT=50
       !

       DFRZH = DT * GLCONST * QH (L)    ! from OT(16)
       !
       IF (DFRZH.LT.QH (L) ) THEN  
          !
          QI (L) = QI (L) + DFRZH  
          QH (L) = QH (L) - DFRZH  
          T (L) = T (L) + FRC * DFRZH  !warms air

          !print*,'7',l,qi(l),DFRZH


          RETURN  
          !
       ELSE  
          !
          QI (L) = QI (L) + QH (L)  
          T  (L) = T  (L) + FRC * QH (L)  
          QH (L) = 0.0  

          !print*,'8',l,qi(l), QH (L)  
          !
       ENDIF
       !
       RETURN  
       !
     END SUBROUTINE GLACIATE

     !
     !
     ! *********************************************************************
     SUBROUTINE MELT(qi,l,t,dt,rho,cvi,qh)  
       !
       ! ******************* MAKES WATER OUT OF ICE **************************

       ! plumegen_coms
       INTEGER, INTENT(IN)    :: l
       REAL   , INTENT(IN)    :: dt
!!$       REAL   , INTENT(INOUT) :: qi (nkp)
!!$       REAL   , INTENT(INOUT) :: t  (nkp)
!!$       REAL   , INTENT(IN)    :: rho(nkp)
!!$       REAL   , INTENT(IN)    :: cvi(nkp)
!!$       REAL   , INTENT(INOUT) :: qh (nkp)
       REAL   , INTENT(INOUT) :: qi (:)
       REAL   , INTENT(INOUT) :: t  (:)
       REAL   , INTENT(IN)    :: rho(:)
       REAL   , INTENT(IN)    :: cvi(:)
       REAL   , INTENT(INOUT) :: qh (:)


       !                                              
       REAL, PARAMETER :: FRC = 332.27, TMELT = 273., F0 = 0.75   !ice velocity factor
       REAL            :: DTMELT
       !                                    
       DTMELT = 0.   !conversion,ice to rain
       !
       !selection rules
       IF (QI (L) .LE. 0.0  ) RETURN  
       IF (T (L)  .LT. TMELT) RETURN  
       !
       !OT(23,24)
       DTMELT = DT * (2.27 / RHO (L) ) * CVI(L) * (T (L) - TMELT) * ( (RHO(L)  &
            * QI (L) * 1.E-6) **0.525) * (F0** ( - 0.42) )
       !after Mason,1956
       !
       !     check the possibilities
       !
       IF (DTMELT.LT.QI (L) ) THEN  
          !
          QH (L) = QH (L) + DTMELT  
          QI (L) = QI (L) - DTMELT  
          T  (L) = T (L) - FRC * DTMELT     !cools air
          !print*,'9',l,qi(l),DTMELT


          RETURN  
          !
       ELSE  
          !
          QH (L) = QH (L) + QI (L)   !get all there is to get
          T  (L) = T (L) - FRC * QI (L)  
          QI (L) = 0.0  
          !print*,'10',l,qi(l)
          !
       ENDIF
       !
       RETURN  
       !
     END SUBROUTINE MELT

     !-------------------------------------------------------------------------
     FUNCTION ESAT_PR (TEM)  

       REAL, INTENT(IN) :: TEM


       !
       ! ******* Vapor Pressure  A.L. Buck JAM V.20 p.1527. (1981) ***********
       !
       REAL, PARAMETER :: CI1 = 6.1115, CI2 = 22.542, CI3 = 273.48
       REAL, PARAMETER :: CW1 = 6.1121, CW2 = 18.729, CW3 = 257.87, CW4 = 227.3
       REAL, PARAMETER :: TMELT = 273.3

       REAL            :: ESAT_PR, temc, esatm
       !
       !     formulae from Buck, A.L., JAM 20,1527-1532
       !     custom takes esat wrt water always. formula for h2o only
       !     good to -40C so:
       !
       !
       TEMC = TEM - TMELT  
       IF (TEMC.GT. - 40.0) THEN
          ESATM = CW1 * EXP ( ( (CW2 - (TEMC / CW4) ) * TEMC) / (TEMC + CW3))
          ESAT_PR = ESATM / 10.	!kPa			  
       ELSE
          ESATM = CI1 * EXP (CI2 * TEMC / (TEMC + CI3) )  !ice, millibars  
          ESAT_PR = ESATM / 10.	!kPa			  
       END IF

       RETURN  

     END FUNCTION ESAT_PR


   

END MODULE mod_chem_plumerise_scalar
