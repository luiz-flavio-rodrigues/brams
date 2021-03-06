!############################# Change Log ##################################
! 5.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################

subroutine anlavg(n1,n2,n3,n4)

  use grid_dims, only: maxgrds
  use var_tables, only: num_var, vtab_r
  use mem_grid, only: dtlongn, time, jdim, ngrid
  use io_params, only: avgtim, frqmean, frqboth

  implicit none
  include "i8.h"
  integer :: n1,n2,n3,n4

  include 'interface.h'

  integer :: nv,ng
  integer :: n3dadd,n3d,n2dadd,n2d,izero,indvar,indavg

  integer(kind=i8) :: mxyzp, mxyp

  integer, save :: ncall=0,nvars(maxgrds,2),numadd,navg
  real, save :: avgtim1,frq,avgtim2,timem,timecent

  real, pointer :: v_p, vm_p

  ! This routine averages all of the analysis variables over
  ! ANLAVG. It also accumulates precipitation for all of the
  ! microphysics variables since the last analysis write.

  !! Next 2 lines are not necessary more because of a modification in MODEL
  !! ALF
!!$  IF(avgtim == 0.)RETURN
!!$  IF(frqmean == 0.0.AND.frqboth == 0.)RETURN


  if(ncall.eq.0) then

     ncall=1
     !  Calculate the number of timestep to average over. Note that if
     !    AVGTIM>0, then need to have an equal number of timesteps on
     !    both sides of the analysis write, else can have an odd number
     !    of timesteps if the write represent an average at the end
     !    of the averaging period.
     avgtim1=avgtim
     if(avgtim.gt.0)then
        avgtim=max(avgtim,2.*dtlongn(1))
        avgtim=float(nint(avgtim/dtlongn(1)/2.))*dtlongn(1)*2.
     elseif(avgtim.lt.0)then
        avgtim=min(avgtim,-dtlongn(1))
        avgtim=float(nint(avgtim/dtlongn(1)))*dtlongn(1)
     endif
     if(avgtim.ne.avgtim1)then
        print*,' '
        print*,'***************************************************'
        print*,'***Changing AVGTIM so multiple of DTLONG',avgtim
        print*,'***************************************************'
        print*,' '
     endif
     navg=nint(abs(avgtim)/dtlongn(1))
     print*,' '
     print*,'****************************************************'
     print*,'**** AVERAGING OVER THESE # OF TIMESTEPS', navg
     print*,'**** AVERAGING TIME=',avgtim
     print*,'****************************************************'
     print*,' '

     !  Choose a output frequency

     if(frqmean.gt.0.0.and.frqboth.gt.0.)then
        frq=min(frqmean,frqboth)
     elseif(frqmean.gt.0.0.and.frqboth.eq.0.)then
        frq=frqmean
     elseif(frqmean.eq.0.0.and.frqboth.gt.0.)then
        frq=frqboth
     endif

  endif

  mxyzp=n1*n2*n3
  mxyp=n2*n3

  izero=0
  avgtim2=abs(avgtim)/2.
  timem=time+0.01  
  timecent=float(nint(timem/frq))*frq

  ! No need to execute if not in the averaging interval.
  if(avgtim.gt.0.0)then
     if(timem.lt.avgtim2)return
     if(timem.lt.timecent-avgtim2.or.timem.gt.timecent+avgtim2) return
  elseif(avgtim.lt.0.0)then
     if(mod(timem-avgtim,frq).gt.-avgtim)return
  endif

  ! Zero out the averages before accumulating.
  if(avgtim.gt.0.0.and.mod(timem+avgtim2,frq).lt.dtlongn(1)) izero=1
  if(avgtim.lt.0.0.and.mod(timem-avgtim,frq).lt.dtlongn(1)) izero=1

  !      Implement THERMO call for THETA and RV so matches code in rdriv.f
  !         Note that theta is changed, BUT never used on boundary points

  call thermo(n1,n2,n3,1,1,1,n3,'THRM_ONLY')
  call thermo(n1,n2,n3,n2,n2,1,n3,'THRM_ONLY')
  if (jdim .eq. 1) then
     call thermo(n1,n2,n3,1,n2,1,1,'THRM_ONLY')
     call thermo(n1,n2,n3,1,n2,n3,n3,'THRM_ONLY')
  endif

  ! Loop through the main variable table

  do nv = 1,num_var(ngrid)

     if (vtab_r(nv,ngrid)%imean == 1) then

        v_p => vtab_r(nv,ngrid)%var_p
        vm_p=> vtab_r(nv,ngrid)%var_m

        if(izero.eq.1) &
!!$             call azero(vtab_r(nv,ngrid)%npts, vm_p)
             call azero_l(vtab_r(nv,ngrid)%npts, vm_p)
        !CALL azero(mxyzp,vm_p)  !Mod. ALF
        ! mxyzp eh 3D enquanto algumas variaveis sao 2D

        call average(vtab_r(nv,ngrid)%npts,vm_p,v_p,navg)

     endif

  enddo

  return
end subroutine anlavg

!******************************************************************************

subroutine average(m, av, v, navg)

  implicit none
  include "i8.h"
  integer, intent(in)          :: navg
  integer(kind=i8), intent(in) :: m
  real, intent(inout)          :: av(m)
  real, intent(in)             :: v(m)

!!$  integer :: i
!!$
!!$  do i=1,m
!!$     av(i)=av(i)+v(i)/float(navg)
!!$  enddo
  av = av + v / float(navg)

  return
end subroutine average

!*******************************************************************************

!subroutine zeromeangrad(a,ifm,n1,n2,n3,ia,ib,ja,jb)

!implicit none
!real :: a(*)
!integer :: ifm,n1,n2,n3,ia,ib,ja,jb

!include 'rcommons.h'
!include 'interface.h'

!integer :: n3d,ithflg,irvflg

!if(frqmean.eq.0.0.and.frqboth.eq.0.) return

! Check to make sure space allocated for this variable

!ithflg=0
!irvflg=0
!do n3d=1,nm3d(ifm)
!   if(imchr3d(n3d,ifm).eq.'THETAM')ithflg=1
!   if(imchr3d(n3d,ifm).eq.'RVM')irvflg=1
!enddo
!do n3d=1,nb3d(ifm)
!   if(ibchr3d(n3d,ifm).eq.'THETAM')ithflg=1
!   if(ibchr3d(n3d,ifm).eq.'RVM')irvflg=1
!enddo

!if(ithflg.eq.1)call rowcolmn(n1,n2,n3,ia,ib,ja,jb,a(ithetam))
!if(irvflg.eq.1)call rowcolmn(n1,n2,n3,ia,ib,ja,jb,a(irvm))

!return
!end

!*******************************************************************************

subroutine rowcolmn(n1,n2,n3,ia,ib,ja,jb,var)

  implicit none
  integer :: n1,n2,n3,ia,ib,ja,jb
  real :: var(n1,n2,n3)

  integer :: i,j,k

  if(ia.eq.ib.and.ia.eq.1)then
     do j=ja,jb
        do i=ia,ib
           do k=1,n1
              var(k,i,j)=var(k,i+1,j)
           enddo
        enddo
     enddo
  elseif(ia.eq.ib.and.ia.eq.n2)then
     do j=ja,jb
        do i=ia,ib
           do k=1,n1
              var(k,i,j)=var(k,i-1,j)
           enddo
        enddo
     enddo
  elseif(ja.eq.jb.and.ja.eq.1)then
     do j=ja,jb
        do i=ia,ib
           do k=1,n1
              var(k,i,j)=var(k,i,j+1)
           enddo
        enddo
     enddo
  elseif(ja.eq.jb.and.ja.eq.n3)then
     do j=ja,jb
        do i=ia,ib
           do k=1,n1
              var(k,i,j)=var(k,i,j-1)
           enddo
        enddo
     enddo
  endif

  return
end subroutine rowcolmn
