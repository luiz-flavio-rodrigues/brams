!############################# Change Log ##################################
! 5.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################
module ModAcoust_adap
contains

  subroutine acoust_adap(OneGrid, &
       m1,m2,m3,lpu_R,lpv_R,lpw_R            &
       ,scr1,scr2,vt3da,vt3db,vt3dc,vt3dd    &
       ,vt3de,vt3df,vt3dg,vt3dh,vt2da        &
       ,dn0,pi0,th0,up,vp,wp,pp,ut,vt,wt,pt  &
       ,dxu,dyv,fmapui,fmapvi,dxt,dyt,fmapt  &
       ,aru,arv,arw,volt,volu,volv,volw      )
    !--------------------------------------------------------------------
    !                  Acoustic terms small time-step driver
    !
    !     This routine calls all the necessary routines to march the model
    !     through the small timesteps.
    !-----------------------------------------------------------------------
    use mem_grid
    use mem_scratch
    use node_mod
    use ModGrid, only: &
         Grid
    use ModMessageSet, only: &
         PostRecvSendMsgs, &
         WaitRecvMsgs

    implicit none

    type(Grid), pointer :: OneGrid
    integer :: m1,m2,m3
    real, dimension(m1,m2,m3) :: dn0,pi0,th0,up,vp,wp,pp  &
         ,aru,arv,arw,volt,volu,volv,volw
    real, dimension(m2,m3) ::   dxu,dyv,fmapui,fmapvi,dxt,dyt,fmapt
    real, dimension(*) ::       scr1,scr2,vt3da,vt3db,vt3dc,vt3dd  &
         ,vt3de,vt3df,vt3dg,vt3dh,vt2da,ut,vt,wt,pt
    real, dimension(m2,m3) :: lpu_R,lpv_R,lpw_R
    
    integer, dimension(m2,m3) :: lpu,lpv,lpw
    real :: t1,w1,a1da2

    integer :: iter

    lpu=int(lpu_R);lpv=int(lpv_R);lpw=int(lpw_R)

    do iter = 1,nnacoust(ngrid)

       !     Get coefficients for computations

       dts = 2. * dtlt / nnacoust(ngrid)

       if (iter .eq. 1)  &
            call coefz_adap(mzp,mxp,myp,ia,iz,ja,jz,lpw(1,1)      &
            ,vt3dc(1),vt3dd(1),vt3de(1),dn0(1,1,1),pi0(1,1,1)  &
            ,th0(1,1,1),a1da2,vt3df(1),vt3dg(1),scr2(1)        &
            ,vctr1,vctr2,arw(1,1,1),volt(1,1,1),volw(1,1,1)    )

       if (nmachs > 1) then
          if (iter .ne. 1) then
             call WaitRecvMsgs(OneGrid%AcouSendP, OneGrid%AcouRecvP)
          endif
       endif

       call prdctu_adap(mzp,mxp,myp,ia,izu,ja,jz,ibcon,lpu(1,1)    &
            ,up(1,1,1),ut(1),pp(1,1,1),vt3da(1),th0(1,1,1),vt3db(1)  &
            ,dxu(1,1),vt3dh(1),aru(1,1,1),volu(1,1,1),mynum          )

       if (nmachs > 1) then
          if (iter .ne. nnacoust(ngrid)) then
             call PostRecvSendMsgs(OneGrid%AcouSendU, OneGrid%AcouRecvU)
          endif
       endif

       call prdctv_adap(mzp,mxp,myp,ia,iz,ja,jzv,ibcon,lpv(1,1)    &
            ,vp(1,1,1),vt(1),pp(1,1,1),vt3da(1),th0(1,1,1),vt3db(1)  &
            ,dyv(1,1),vt3dh(1),arv(1,1,1),volv(1,1,1)                )

       if (nmachs > 1) then
          if (iter .ne. nnacoust(ngrid)) then
             call PostRecvSendMsgs(OneGrid%AcouSendV, OneGrid%AcouRecvV)
          else
             call PostRecvSendMsgs(OneGrid%AcouSendUV, OneGrid%AcouRecvUV)
          endif
       endif

       call prdctw1_adap(mzp,mxp,myp,ia,iz,ja,jz,ibcon,lpw(1,1)  &
            ,wp(1,1,1),wt(1),pp(1,1,1),vt3dc(1)  &
            ,a1da2,vt3dh(1))

       if (nmachs > 1) then
          if (iter .ne. nnacoust(ngrid)) then
             call WaitRecvMsgs(OneGrid%AcouSendU, OneGrid%AcouRecvU)
             call WaitRecvMsgs(OneGrid%AcouSendV, OneGrid%AcouRecvV)
          else
             call WaitRecvMsgs(OneGrid%AcouSendUV, OneGrid%AcouRecvUV)
          endif
       endif

       call prdctp1_adap(mzp,mxp,myp,ia,iz,ja,jz,jdim,lpw(1,1)                &
            ,pp(1,1,1),up(1,1,1),vp(1,1,1),pi0(1,1,1),dn0(1,1,1),th0(1,1,1)     &
            ,pt(1),vt3da(1),vt3db(1),vt2da(1),fmapui(1,1),fmapvi(1,1),dxt(1,1)  &
            ,dyt(1,1),fmapt(1,1),aru(1,1,1),arv(1,1,1),volt(1,1,1),mynum        )

       call prdctw2_adap(mzp,mxp,myp,ia,iz,ja,jz,lpw(1,1)           &
            ,wp(1,1,1),pp(1,1,1),vt3dc(1),vt3dd(1),vt3de(1),vt3dg(1)  &
            ,scr1(1),scr2(1),vt2da(1)                                 )

       call prdctw3_adap(mzp,mxp,myp,ia,iz,ja,jz,lpw(1,1)                 &
            ,wp(1,1,1),scr1(1),vt3df(1),vt3dg(1),vt3dc(1),vt3dd(1),pp(1,1,1))

       call prdctp2_adap(mzp,mxp,myp,ia,iz,ja,jz,ibcon,lpw(1,1)  &
            ,pp(1,1,1),wp(1,1,1),vt3dd(1),vt3de(1),mynum)

       if (nmachs > 1) then
          if (iter .ne. nnacoust(ngrid)) then
             call PostRecvSendMsgs(OneGrid%AcouSendP, OneGrid%AcouRecvP)
          else
             call PostRecvSendMsgs(OneGrid%AcouSendWP, OneGrid%AcouRecvWP)
             call WaitRecvMsgs(OneGrid%AcouSendWP, OneGrid%AcouRecvWP)
          endif
       endif

    enddo

    return
  end subroutine acoust_adap
end module ModAcoust_adap
!******************************************************************************

subroutine prdctu_adap(m1,m2,m3,ia,iz,ja,jz,ibcon,lpu  &
     ,up,ut,pp,vt3da,th0,dpdx,dxu,vt3dh,aru,volu,mynum)

  use mem_grid

  implicit none

  integer :: m1,m2,m3,ia,iz,ja,jz,i,j,k,ibcon,mynum
  integer, dimension(m2,m3) :: lpu

  real, dimension(m1,m2,m3) :: up,ut,pp,vt3da,th0,dpdx,vt3dh,aru,volu
  real, dimension(m2,m3) ::  dxu

  !     U prediction

!!$call azero(m1*m2*m3,dpdx)
  dpdx = 0.

  !     Calculate acoustic tendency (horizontal pressure gradient)

  do j = ja,jz
     do i = ia,iz
        do k = lpu(i,j),m1-1
           dpdx(k,i,j) = -(th0(k,i,j) + th0(k,i+1,j)) * .5  &
                * aru(k,i,j) / volu(k,i,j) * (pp(k,i+1,j) - pp(k,i,j))
        enddo
     enddo
  enddo

  if (distim .ne. 0.) then
     call rayf_adap(1,m1,m2,m3,ia,iz,ja,jz,ibcon,lpu(1,1),up,th0,vt3dh)
  endif

  do j = 1,m3
     do i = 1,m2
        do k = lpu(i,j),m1
           up(k,i,j) = up(k,i,j) + dts * (dpdx(k,i,j) + ut(k,i,j))
        enddo
     enddo
  enddo

  if (nstbot == 1 .and. itopo == 1)  &
       call botset_adap(m1,m2,m3,ia,iz,ja,jz,ibcon,lpu,up,'U')

  return
end subroutine prdctu_adap

!    ******************************************************************

subroutine prdctv_adap(m1,m2,m3,ia,iz,ja,jz,ibcon,lpv  &
     ,vp,vt,pp,vt3da,th0,dpdy,dyv,vt3dh,arv,volv)

  use mem_grid

  implicit none

  integer :: m1,m2,m3,ia,iz,ja,jz,ibcon,i,j,k
  integer, dimension(m2,m3) :: lpv

  real, dimension (m1,m2,m3) :: vp,vt,th0,dpdy,pp,vt3da,vt3dh,arv,volv
  real, dimension(m2,m3) :: dyv

  !     V prediction

!!$call azero(m1*m2*m3,dpdy)
  dpdy = 0.

  if (jdim .eq. 1) then

     !       calculate acoustic tendency (horizontal pressure gradient)

     do j = ja,jz
        do i = ia,iz
           do k = lpv(i,j),m1-1
              dpdy(k,i,j) = -(th0(k,i,j) + th0(k,i,j+1)) * .5  &
                   * arv(k,i,j) / volv(k,i,j) * (pp(k,i,j+1) - pp(k,i,j))
           enddo
        enddo
     enddo

  endif

  if (distim .ne. 0.) then
     call rayf_adap(1,m1,m2,m3,ia,iz,ja,jz,ibcon,lpv(1,1),vp,th0,vt3dh)
  endif

  do j = 1,m3
     do i = 1,m2
        do k = lpv(i,j),m1
           vp(k,i,j) = vp(k,i,j) + dts * (dpdy(k,i,j) + vt(k,i,j))
        enddo
     enddo
  enddo

  if (nstbot == 1 .and. itopo == 1)  &
       call botset_adap(m1,m2,m3,ia,iz,ja,jz,ibcon,lpv,vp,'V')

  return
end subroutine prdctv_adap

!----------------------------------------------------------------------

subroutine prdctw1_adap(m1,m2,m3,ia,iz,ja,jz,ibcon,lpw  &
     ,wp,wt,pp,acoc,a1da2,vt3dh)

  use mem_grid

  implicit none

  integer :: m1,m2,m3,ia,iz,ja,jz,ibcon,i,j,k
  integer, dimension(m2,m3) :: lpw

  real :: a1da2
  real, dimension(m1,m2,m3) :: wp,wt,pp,acoc,vt3dh

  !     First part of prediction at I,J point

  !     Compute forward part of Crank-Nickelson scheme. This will be total
  !     W prediction for explicit case.

  if (distim .ne. 0.) then
     call rayf_adap(1,m1,m2,m3,ia,iz,ja,jz,ibcon,lpw(1,1),wp,vt3dh,vt3dh)
  endif

  do j = 1,m3
     do i = 1,m2
        do k = lpw(i,j),m1-2
           wp(k,i,j) = wp(k,i,j) + dts * wt(k,i,j)
        enddo
     enddo
  enddo

  do j = ja,jz
     do i = ia,iz
        do k = lpw(i,j),m1-2
           wp(k,i,j) = wp(k,i,j) + a1da2 * acoc(k,i,j)  &
                * (pp(k,i,j) - pp(k+1,i,j))
        enddo
     enddo
  enddo

  return
end subroutine prdctw1_adap

!---------------------------------------------------------------------

subroutine  prdctw2_adap(m1,m2,m3,ia,iz,ja,jz,lpw  &
     ,wp,pp,acoc,acof,acog,amof,amog,acoaa,heatfx1)

  use mem_grid

  implicit none

  integer :: m1,m2,m3,ia,iz,ja,jz,i,j,k
  integer, dimension(m2,m3) :: lpw
  integer :: ka

  real, dimension(m1,m2,m3) :: wp,pp,acoc,acof,acog,amof,amog,acoaa
  real, dimension(m2,m3) :: heatfx1

  if (nsttop == 1) then
     do j = ja,jz
        do i = ia,iz
           wp(m1-1,i,j) = 0.
        enddo
     enddo
  endif

  if (impl == 1) then

     !         First implicit part of the w prediction

     do j = ja,jz
        do i = ia,iz
           do k = lpw(i,j),m1-2
              wp(k,i,j) = wp(k,i,j) - (pp(k+1,i,j) - pp(k,i,j)) * acoc(k,i,j)
           enddo
        enddo
     enddo

     do j = ja,jz
        do i = ia,iz
           ka = lpw(i,j)
           amog(ka-1,i,j) = -wp(ka-1,i,j) / amof(ka-1,i,j)
           do k = ka,m1-2
              amog(k,i,j) = (-wp(k,i,j) - acoaa(k,i,j) * amog(k-1,i,j))  &
                   / amof(k,i,j)
           enddo
        enddo
     enddo

  endif
  return
end subroutine prdctw2_adap

!---------------------------------------------------------------------

subroutine prdctw3_adap(m1,m2,m3,ia,iz,ja,jz,lpw  &
     ,wp,amog,amoe,amof,acoc,acof,pp)

  use mem_grid

  implicit none

  integer :: m1,m2,m3,ia,iz,ja,jz,i,j,k
  integer, dimension(m2,m3) :: lpw

  real, dimension(m1,m2,m3) :: wp,pp,acoc,acof,amof,amog,amoe

  !     Conclusion of implicit w prediction

  if (impl == 1) then
     do j = ja,jz
        do i = ia,iz
           do k = m1-2,lpw(i,j),-1
              wp(k,i,j) = amog(k,i,j) - amoe(k,i,j) * wp(k+1,i,j)
           enddo
        enddo
     enddo
  endif

  if (nstbot == 1) then
     do j = ja,jz
        do i = ia,iz
           do k = 1,lpw(i,j)-1
              !    wp(k,i,j) = wp(lpw(i,j),i,j)
              wp(k,i,j) = 0.
           enddo
        enddo
     enddo
  endif

  return
end subroutine prdctw3_adap

!--------------------------------------------------------------------

subroutine prdctp1_adap(m1,m2,m3,ia,iz,ja,jz,jd,lpw  &
     ,pp,up,vp,pi0,dn0,th0,pt,hdv,hfx  &
     ,hfx1,fmapui,fmapvi,dxt,dyt,fmapt,aru,arv,volt,mynum)


  use mem_grid
  use rconstants

  implicit none

  integer :: m1,m2,m3,ia,iz,ja,jz,jd,i,j,k,mynum
  integer, dimension(m2,m3) :: lpw

  real :: rocvpct
  real, dimension(m1,m2,m3) :: pp,up,vp,pi0,hdv,pt,hfx,dn0,th0,aru,arv,volt
  real, dimension(m2,m3) :: hfx1,fmapui,fmapvi,dxt,dyt,fmapt

!!$call azero(m1*m2*m3,hdv)
  hdv = 0.
  rocvpct = .5 * rocv *sspct ** 2

  do j = 1,m3
     do i = 1,m2
        do k = lpw(i,j),m1
           hfx(k,i,j) = dn0(k,i,j) * th0(k,i,j)
        enddo
     enddo
  enddo

  do j = ja,jz
     do i = ia,iz
        do k = lpw(i,j),m1-1

           hdv(k,i,j) = -rocvpct * pi0(k,i,j) / (hfx(k,i,j) * volt(k,i,j))   &
                
                * ((up(k,i,j) * aru(k,i,j)   * (hfx(k,i,j) + hfx(k,i+1,j))     &
                - up(k,i-1,j) * aru(k,i-1,j) * (hfx(k,i,j) + hfx(k,i-1,j)))    &
                
                + (vp(k,i,j) * arv(k,i,j)    * (hfx(k,i,j) + hfx(k,i,j+jd))    &
                - vp(k,i,j-jd) * arv(k,i,j-jd) * (hfx(k,i,j) + hfx(k,i,j-jd))) )

        enddo
     enddo
  enddo

  do j = ja,jz
     do i = ia,iz
        do k = lpw(i,j),m1
           pp(k,i,j) = pp(k,i,j) + (pt(k,i,j) + hdv(k,i,j)) * dts
        enddo
     enddo
  enddo

  return
end subroutine prdctp1_adap

!--------------------------------------------------------------------

subroutine prdctp2_adap(m1,m2,m3,ia,iz,ja,jz,ibcon,lpw,pp,wp,acof,acog,mynum)

  use mem_grid

  implicit none

  integer :: m1,m2,m3,ia,iz,ja,jz,ibcon,i,j,k,mynum

  real, dimension(m1,m2,m3) :: pp,wp,acof,acog
  integer, dimension(m2,m3) :: lpw

  !           Finish pressure prediction

  do j = ja,jz
     do i = ia,iz
        do k = lpw(i,j),m1-1
           pp(k,i,j) = pp(k,i,j)  &
                + (wp(k,i,j) * acof(k,i,j) + wp(k-1,i,j) * acog(k,i,j))
        enddo
     enddo
  enddo

  if (nstbot .eq. 1) call botset_adap(m1,m2,m3,ia,iz,ja,jz,ibcon,lpw,pp,'P')

  return
end subroutine prdctp2_adap

!******************************************************************************

subroutine coefz_adap(m1,m2,m3,ia,iz,ja,jz,lpw  &
     ,acoc,acof,acog,dn0,pi0,th0,a1da2,amoe,amof,acoaa,acobb,acocc  &
     ,arw,volt,volw)

  use mem_grid
  use mem_scratch
  use rconstants

  implicit none

  integer :: m1,m2,m3,ia,iz,ja,jz,i,j,k

  real :: dt2al2,a1da2,rdto2cv,dt2al2r,rdtr
  real, dimension(m1,m2,m3) :: th0,pi0,dn0,acoc,acof,acog,amoe,amof,acoaa  &
       ,arw,volt,volw
  integer, dimension(m2,m3) :: lpw
  real, dimension(*) :: acobb,acocc
  integer :: ka

  ! +--------------------------------------------------------------------+
  ! \   Calculate coefficients for the vertical pressure gradient        \
  ! \     and divergence terms.  These will be combined later for the    \
  ! \     implicit computations.                                         \
  ! +--------------------------------------------------------------------+

  if (impl .eq. 1) then
     dt2al2 = dts * .75
     a1da2 = 1. / 3.
  else
     dt2al2 = dts
     a1da2 = 1.
  endif
  rdto2cv = sspct ** 2 * rgas * dts / (2.0 * cv)

  do j = ja,jz
     do i = ia,iz
        ka = lpw(i,j)

        !         Coefficient for the vertical pressure gradient term

        dt2al2r = .5 * dt2al2
        do k = ka-1,m1-1
           acoc(k,i,j) = dt2al2r * arw(k,i,j) / volw(k,i,j)  &
                * (th0(k,i,j) + th0(k+1,i,j))
        enddo

        !         Coefficients for the vertical divergence term

        rdtr = rdto2cv
        do k = ka,m1
           vctr12(k) = dn0(k,i,j) * th0(k,i,j)
           vctr11(k) = rdtr * pi0(k,i,j) / (vctr12(k) * volt(k,i,j))
        enddo
        vctr12(ka-1) = dn0(ka-1,i,j) * th0(ka-1,i,j)
        do k = ka,m1-1
           acof(k,i,j) = -vctr11(k) * (vctr12(k) + vctr12(k+1)) * arw(k,i,j)
           acog(k,i,j) = vctr11(k) * (vctr12(k) + vctr12(k-1)) * arw(k-1,i,j)
        enddo
        acog(m1,i,j) = vctr11(m1) * (vctr12(m1) + vctr12(m1-1)) * arw(m1-1,i,j)

        do k = ka,m1-1
           acoaa(k,i,j) = acoc(k,i,j) * acog(k,i,j)
           acobb(k) = acoc(k,i,j) * (acof(k,i,j) - acog(k+1,i,j)) - 1.
           acocc(k) = -acoc(k,i,j) * acof(k+1,i,j)
        enddo
        acobb(ka-1) = -1.
        acocc(ka-1) = 0.
        acoaa(m1,i,j) = 0.
        acobb(m1) = -1.

        amof(ka-1,i,j) = acobb(ka-1)
        amoe(ka-1,i,j) = acocc(ka-1) / amof(ka-1,i,j)
        do k = ka,m1
           amof(k,i,j) = acobb(k) - acoaa(k,i,j) * amoe(k-1,i,j)
           amoe(k,i,j) = acocc(k) / amof(k,i,j)
        enddo

     enddo
  enddo
end subroutine coefz_adap
