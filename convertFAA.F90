module convertFAA

use FAARoutines

implicit none

real(R8),parameter :: SHR_CONST_PI      = 3.14159265358979323846_R8  ! pi
real(R8),parameter :: SHR_CONST_CDAY    = 86400.0_R8      ! sec in calendar day ~ sec
real(R8),parameter :: SHR_CONST_SDAY    = 86164.0_R8      ! sec in siderial day ~ sec
real(R8),parameter :: SHR_CONST_OMEGA   = 2.0_R8*SHR_CONST_PI/SHR_CONST_SDAY ! earth rot ~ rad/sec
real(R8),parameter :: SHR_CONST_REARTH  = 6.37122e6_R8    ! radius of earth ~ m
real(R8),parameter :: SHR_CONST_G       = 9.80616_R8      ! acceleration of gravity ~ m/s^2
real(R8),parameter :: SHR_CONST_STEBOL  = 5.67e-8_R8      ! Stefan-Boltzmann constant ~ W/m^2/K^4
real(R8),parameter :: SHR_CONST_BOLTZ   = 1.38065e-23_R8  ! Boltzmann's constant ~ J/K/molecule
real(R8),parameter :: SHR_CONST_AVOGAD  = 6.02214e26_R8   ! Avogadro's number ~ molecules/kmole
real(R8),parameter :: SHR_CONST_RGAS    = SHR_CONST_AVOGAD*SHR_CONST_BOLTZ       ! Universal gas constant ~ J/K/kmole
real(R8),parameter :: SHR_CONST_MWDAIR  = 28.966_R8       ! molecular weight dry air ~ kg/kmole
real(R8),parameter :: SHR_CONST_MWWV    = 18.016_R8       ! molecular weight water vapor
real(R8),parameter :: SHR_CONST_RDAIR   = SHR_CONST_RGAS/SHR_CONST_MWDAIR        ! Dry air gas constant     ~ J/K/kg
real(R8),parameter :: SHR_CONST_RWV     = SHR_CONST_RGAS/SHR_CONST_MWWV          ! Water vapor gas constant ~ J/K/kg
real(R8),parameter :: SHR_CONST_ZVIR    = (SHR_CONST_RWV/SHR_CONST_RDAIR)-1.0_R8 ! RWV/RDAIR - 1.0


real(R8),parameter :: gravit = SHR_CONST_G             ! gravitional acceleration
real(R8),parameter :: rearth = SHR_CONST_REARTH          ! radius of earth

real(R8),parameter :: rair    = SHR_CONST_RGAS/28.97
real(R8),parameter :: pi     = SHR_CONST_PI  ! pi
real(R8),parameter :: zvir    = SHR_CONST_ZVIR ! RWV/RDAIR - 1.0

type interp_type
   real(r8), pointer :: wgts(:)
   real(r8), pointer :: wgtn(:)
   integer, pointer  :: jjm(:)
   integer, pointer  :: jjp(:)
end type interp_type

type(interp_type) :: lon_wgts1, lat_wgts1
type(interp_type) :: lon_wgts2, lat_wgts2
type(interp_type) :: lon_wgts3, lat_wgts3

real(r8), parameter :: zero=0.0_r8, twopi = 2.0_r8*pi, three60=360.0_r8

contains

 subroutine convertFAAtoMixRat(inputSecsRead,climFile,nlon_FAA, nlat_FAA, nlev_FAA, nfields_FAA, nlon_CLIMO, nlat_CLIMO, &
                               nlev_CLIMO, lon_FAA, lat_FAA, zi_FAA, alt_pres_int, emission_mass_FAA, &
                               P0_CLIMO, pin_CLIMO, hyam_CLIMO, hybm_CLIMO, hyai_CLIMO, hybi_CLIMO, mixing_ratio)
!***********************************************************************************************************************
!----------------------------------------------------------------------------
! This program converts FAA data sets into mixing ratios from emission mass
!----------------------------------------------------------------------------

implicit none

! INPUT Variables
real(r8)                             :: inputSecsRead
character*256                        :: climFile
integer                              :: nlon_FAA, nlat_FAA, nlev_FAA, nfields_FAA
integer                              :: nlon_CLIMO, nlat_CLIMO, nlev_CLIMO
real(r8), dimension(:)               :: lon_FAA, lat_FAA
real(r8), dimension(:,:,:)           :: zi_FAA
! ++ alt pres
real(r8), dimension(:,:,:) :: alt_pres_int
! -- alt pres
real(r8), dimension(:,:,:,:)         :: emission_mass_FAA

! OUTPUT Variables
real(r8) :: P0_CLIMO
real(r8), dimension(:)       :: hyam_CLIMO,hybm_CLIMO
real(r8), dimension(:)       :: hyai_CLIMO, hybi_CLIMO
real(r8), dimension(:,:,:)   :: pin_CLIMO

    ! Only want to store real mixing ratios
real, dimension(:,:,:,:)     :: mixing_ratio

! LOCAL Variables
real(r8)                             :: dtheta

real(r8), dimension(nlat_CLIMO)      :: lat_CLIMO
real(r8), dimension(nlon_CLIMO)      :: lon_CLIMO
real(r8), dimension(nlon_CLIMO,nlat_CLIMO) :: ps_CLIMO, PHIS_CLIMO
real(r8),dimension(nlon_CLIMO,nlat_CLIMO,nlev_CLIMO+1) :: pin1_CLIMO,zi1_CLIMO
real(r8),dimension(nlon_CLIMO,nlat_CLIMO,nlev_CLIMO)   :: T_CLIMO,Q_CLIMO

real(r8), dimension(nlon_FAA,nlat_FAA,nlev_CLIMO+1) :: zi2
real(r8), dimension(nlat_FAA+1) :: clat_staggered
real(r8), dimension(nlat_FAA)   :: w
real(r8), dimension(nlon_FAA,nlat_FAA) :: wght

real(r8), dimension(nlon_FAA,nlat_FAA,nlev_CLIMO,nfields_FAA) :: emission_mass_FAA_rebin

integer :: i,j,k,ifield
!********************** add total_mass1 & total_mass2 to check total mass **************************
real(r8) :: total_mass1, total_mass2
!***************************************************************************************************


!--------------------------------------------------------------------------------------
!1) get model pressure and height at interfaces by CAM climo file 
! CAM climo files:
! /project/utls/satftp1/ccsm3/microp2/test1yr/camdev55_cam3_6_72_u175_1xco2_icealb2
! camdev55_cam3_6_72_u175_1xco2_icealb2_01_climo.nc  --> camdev55_cam3_6_72_u175_1xco2_icealb2_12_climo.nc
!--------------------------------------------------------------------------------------

  call climRead(climFile, P0_CLIMO, lat_CLIMO, lon_CLIMO, hyam_CLIMO, hybm_CLIMO, hyai_CLIMO, hybi_CLIMO,&
                ps_CLIMO, T_CLIMO, Q_CLIMO, PHIS_CLIMO, nlon_CLIMO, nlat_CLIMO, nlev_CLIMO)


!--------------------------------------------------------------------------------------
! pressure & height at interfaces, pin1_CLIMO(1:nlon_CLIMO,1:nlat_CLIMO,1:nlev_CLIMO+1), 
!      zi1_CLIMO(1:nlon_CLIMO,1:nlat_CLIMO,1:nlev_CLIMO+1)
!--------------------------------------------------------------------------------------
  do i=1,nlon_CLIMO
    do j=1,nlat_CLIMO
      do k=1,nlev_CLIMO+1
         pin1_CLIMO(i,j,k) = P0_CLIMO*hyai_CLIMO(k)+ps_CLIMO(i,j)*hybi_CLIMO(k)
      enddo
!      zi1_CLIMO(i,j,nlev_CLIMO+1) = PHIS_CLIMO(i,j)/gravit  ! surface elevation
!      do k=nlev_CLIMO,1,-1
!        zi1_CLIMO(i,j,k) = zi1_CLIMO(i,j,k+1)+rair*T_CLIMO(i,j,k)*(1.0_r8+zvir*Q_CLIMO(i,j,k))/&
!                           gravit*(log(pin1_CLIMO(i,j,k+1))-log(pin1_CLIMO(i,j,k)))
!      enddo
    enddo
  enddo
 
!--------------------------------------------------------------------------------------
!2) interoplate pressure and height at interfaces to the horizontal resolution of FAA data sets
!
! get dimension: nlon_FAA, nlat_FAA, nlev_FAA from FAA data set
!--------------------------------------------------------------------------------------

  call lininterp_init(lon_CLIMO(1:nlon_CLIMO), nlon_CLIMO, lon_FAA(1:nlon_FAA), nlon_FAA, 2, lon_wgts1, zero, three60)
  call lininterp_init(lat_CLIMO(1:nlat_CLIMO), nlat_CLIMO, lat_FAA(1:nlat_FAA), nlat_FAA, 1, lat_wgts1)



  do k=1,nlev_CLIMO+1
    call lininterp2d2d(pin1_CLIMO(1:nlon_CLIMO,1:nlat_CLIMO,k), nlon_CLIMO, nlat_CLIMO, &
                       pin_CLIMO(1:nlon_FAA,1:nlat_FAA,k), nlon_FAA, nlat_FAA, lon_wgts1, lat_wgts1)
  enddo
  do k=1,nlev_CLIMO+1
    call lininterp2d2d(zi1_CLIMO(1:nlon_CLIMO,1:nlat_CLIMO,k), nlon_CLIMO, nlat_CLIMO, &
                       zi2(1:nlon_FAA,1:nlat_FAA,k), nlon_FAA, nlat_FAA, lon_wgts1, lat_wgts1)
  enddo

  call lininterp_finish(lon_wgts1)
  call lininterp_finish(lat_wgts1)
 
!--------------------------------------------------------------------------------------
! 4) rebin emission mass into interpolated CAM vetical grid
! emission_mass_FAA(1:nlon_FAA,1:nlat_FAA,1:nlev_FAA,1:nfields_FAA) = read from FAA data (emission species)

! source: emission_mass_FAA(1:nlon_FAA, 1:nlat_FAA, 1:nlev_FAA,1:nfields_FAA), 
!         zi_FAA(1:nlon_FAA, 1:nlat_FAA, 1:nlev_FAA+1) 
! target: emission_mass_FAA_rebin(1:nlon_FAA, 1:nlat_FAA, 1:nlev_CLIMO,1:nfields_FAA),
!         zi2(1:nlon_FAA, 1:nlat_FAA, 1:nlev_CLIMO+1)
!--------------------------------------------------------------------------------------

  do i=1,nlon_FAA
    do j=1,nlat_FAA
      do ifield=1,nfields_FAA
!        call rebin( nlev_FAA, nlev_CLIMO, zi_FAA(i,j,1:nlev_FAA+1), zi2(i,j,1:nlev_CLIMO+1), &
!                    emission_mass_FAA(i,j,1:nlev_FAA,ifield), emission_mass_FAA_rebin(i,j,1:nlev_CLIMO,ifield) )
        call rebin_v2( nlev_FAA, nlev_CLIMO, alt_pres_int(i,j,1:nlev_FAA+1), pin_CLIMO(i,j,1:nlev_CLIMO+1), &
                    emission_mass_FAA(i,j,1:nlev_FAA,ifield), emission_mass_FAA_rebin(i,j,1:nlev_CLIMO,ifield) )
      enddo
    enddo
  enddo

!*********** check total mass *******************************************
 do ifield = 1,nfields_FAA
  total_mass1 = 0.0
  total_mass2 = 0.0
  do i=1,nlon_FAA
  do j=1,nlat_FAA
   do k=1,nlev_FAA
    total_mass1 = total_mass1 + emission_mass_FAA(i,j,k,ifield)
   enddo
   do k=1,nlev_CLIMO
    total_mass2 = total_mass2 + emission_mass_FAA_rebin(i,j,k,ifield)
   enddo
  enddo
  enddo
  if( total_mass1 .gt. 0.000000000000001 ) then
  if( abs(total_mass2-total_mass1)/total_mass1 .gt. 0.01 ) then
    write(0,*) 'ifield = ', ifield,' total mass changes by more than 1% by rebin'
    write(0,*) 'total mass 1 = ', total_mass1
    write(0,*) 'total mass 2 = ', total_mass2
    write(0,*) 'change in % = ', abs(total_mass2-total_mass1)/total_mass1*100.0_r8
    stop
  endif
  endif
 enddo

!--------------------------------------------------------------------------------------
! 8) convert from emission mass to mixing ratio
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! need wght for each grid cell %
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!--------------------------------------------------------------------------------------
!******************* stick with FAA grid *******************************************

  clat_staggered(1) = -90.0_r8/180.0_r8*pi
  do j=2,nlat_FAA
   clat_staggered(j) = (lat_FAA(j-1)+lat_FAA(j))/2.0_r8/180.0_r8*pi
  enddo
  clat_staggered(nlat_FAA+1) = 90.0_r8/180.0_r8*pi

  do j=1,nlat_FAA
   w(j) = sin(clat_staggered(j+1)) - sin(clat_staggered(j))
  enddo
  
  dtheta = 2.0_r8*pi/float(nlon_FAA)
  do i=1,nlon_FAA
  do j=1,nlat_FAA
   wght(i,j) = rearth*rearth*w(j)*dtheta
  enddo
  enddo
  

! *************************************************************************************

!    Added divide by inputSecsRead for seconds and 1000 for grams to kg

  do i=1,nlon_FAA
    do j=1,nlat_FAA
      do k=1,nlev_CLIMO
!++ Chen, 1 & 2 are distance
        do ifield=1,2
           mixing_ratio(i,j,k,ifield) = emission_mass_FAA_rebin(i,j,k,ifield)/(inputSecsRead)
        enddo
        do ifield=3,nfields_FAA
           mixing_ratio(i,j,k,ifield) = (emission_mass_FAA_rebin(i,j,k,ifield)/ &
                                        ((pin_CLIMO(i,j,k+1)-pin_CLIMO(i,j,k))/gravit*wght(i,j)))/(inputSecsRead*1000.0d0)
        enddo
!        do ifield=1,nfields_FAA
!           mixing_ratio(i,j,k,ifield) = (emission_mass_FAA_rebin(i,j,k,ifield)/ &
!                                        ((pin_CLIMO(i,j,k+1)-pin_CLIMO(i,j,k))/gravit*wght(i,j)))/(inputSecsRead*1000.0d0)
!        enddo
!-- Chen
      enddo
    enddo
  enddo

end subroutine convertFAAtoMixRat

subroutine climRead(climFile, P0_CLIMO, lat_CLIMO, lon_CLIMO, hyam_CLIMO, hybm_CLIMO, hyai_CLIMO, hybi_CLIMO,&
                    ps_CLIMO, T_CLIMO, Q_CLIMO, PHIS_CLIMO, nlon_CLIMO, nlat_CLIMO, nlev_CLIMO)

use netcdf

implicit none

character*256                :: climFile
double precision             :: P0_CLIMO
real(r8), dimension(:)       :: lat_CLIMO,lon_CLIMO,hyam_CLIMO,hybm_CLIMO,hyai_CLIMO,hybi_CLIMO
real(r8), dimension(:,:)     :: ps_CLIMO, PHIS_CLIMO
real(r8), dimension(:,:,:)   :: T_CLIMO, Q_CLIMO

integer,dimension(4) :: start,stride,count
integer :: status,ncid,varid,latid,lonid,ilevid,levid,timeid,istat
integer :: nlat_CLIMO,nlon_CLIMO,nlev_CLIMO,nilev_CLIMO,ntime_CLIMO
integer,dimension(100) :: dimids

!--------------------------------------------------------
! Open the file
!--------------------------------------------------------
   status=nf90_open(climFile,NF90_NOWRITE,ncid)
   if (status /= nf90_noerr) call handleErr(status,'climInit 100')

!--------------------------------------------------------
! Retrieve the dimensions
!--------------------------------------------------------
   status = nf90_inq_dimid(ncid,'lat',latid)
   if (status /= nf90_noerr) call handleErr(status,'climInit 200')
   status = nf90_inquire_dimension(ncid,latid,len=nlat_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climInit 201')
    
   status = nf90_inq_dimid(ncid,'lon',lonid)
   if (status /= nf90_noerr) call handleErr(status,'climInit 210')
   status = nf90_inquire_dimension(ncid,lonid,len=nlon_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climInit 211')
      
   status = nf90_inq_dimid(ncid,'ilev',ilevid)
   if (status /= nf90_noerr) call handleErr(status,'climInit 220')
   status = nf90_inquire_dimension(ncid,ilevid,len=nilev_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climInit 221')
    
   status = nf90_inq_dimid(ncid,'lev',levid)
   if (status /= nf90_noerr) call handleErr(status,'climInit 220')
   status = nf90_inquire_dimension(ncid,levid,len=nlev_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climInit 221')
    
   status = nf90_inq_dimid(ncid,'time',timeid)
   if (status /= nf90_noerr) call handleErr(status,'climInit 230')
   status = nf90_inquire_dimension(ncid,timeid,len=ntime_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climInit 231')
    
!--------------------------------------------------------
! Retrieve the variables
!--------------------------------------------------------

   status = nf90_inq_varid(ncid,'P0',varid)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Can''t find P0')
   status = nf90_get_var(ncid,varid,P0_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Problem reading P0 ')

   status = nf90_inq_varid(ncid,'lat',varid)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Can''t find lat')
   status = nf90_get_var(ncid,varid,lat_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Problem reading lat ')

   status = nf90_inq_varid(ncid,'lon',varid)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Can''t find lon')
   status = nf90_get_var(ncid,varid,lon_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Problem reading lon ')

   status = nf90_inq_varid(ncid,'hyam',varid)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Can''t find hyam')
   status = nf90_get_var(ncid,varid,hyam_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Problem reading hyam ')

   status = nf90_inq_varid(ncid,'hybm',varid)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Can''t find hybm')
   status = nf90_get_var(ncid,varid,hybm_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Problem reading hybm ')

   status = nf90_inq_varid(ncid,'hyai',varid)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Can''t find hyai')
   status = nf90_get_var(ncid,varid,hyai_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Problem reading hyai ')

   status = nf90_inq_varid(ncid,'hybi',varid)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Can''t find hybi')
   status = nf90_get_var(ncid,varid,hybi_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Problem reading hybi ')

   status = nf90_inq_varid(ncid,'PS',varid)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Can''t find PS')
   status = nf90_get_var(ncid,varid,ps_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'climRead - Problem reading PS ')

!   status = nf90_inq_varid(ncid,'T',varid)
!   if (status /= nf90_noerr) call handleErr(status,'climRead - Can''t find T')
!   status = nf90_get_var(ncid,varid,T_CLIMO)
!   if (status /= nf90_noerr) call handleErr(status,'climRead - Problem reading T ')

!   status = nf90_inq_varid(ncid,'Q',varid)
!   if (status /= nf90_noerr) call handleErr(status,'climRead - Can''t find Q')
!   status = nf90_get_var(ncid,varid,Q_CLIMO)
!   if (status /= nf90_noerr) call handleErr(status,'climRead - Problem reading Q ')

!   status = nf90_inq_varid(ncid,'PHIS',varid)
!   if (status /= nf90_noerr) call handleErr(status,'climRead - Can''t find PHIS')
!   status = nf90_get_var(ncid,varid,PHIS_CLIMO)
!   if (status /= nf90_noerr) call handleErr(status,'climRead - Problem reading PHIS ')
return
end subroutine climRead

!====================================================================================================
  subroutine lininterp_init(yin, nin, yout, nout, extrap_method, interp_wgts, &
       cyclicmin, cyclicmax)

  implicit none
!
! Description:
!   Initialize a variable of type(interp_type) with weights for linear interpolation.
!       this variable can then be used in calls to lininterp1d and lininterp2d.
!   yin is a 1d array of length nin of locations to interpolate from - this array must 
!       be monotonic but can be increasing or decreasing
!   yout is a 1d array of length nout of locations to interpolate to, this array need
!       not be ordered
!   extrap_method determines how to handle yout points beyond the bounds of yin
!       if 0 set values outside output grid to 0 
!       if 1 set to boundary value
!       if 2 set to cyclic boundaries
!         optional values cyclicmin and cyclicmax can be used to set the bounds of the 
!         cyclic mapping - these default to 0 and 360.
!

    integer, intent(in) :: nin
    integer, intent(in) :: nout
    real(r8), intent(in) :: yin(:)           ! input mesh
    real(r8), intent(in) :: yout(:)         ! output mesh
    integer, intent(in) :: extrap_method       ! if 0 set values outside output grid to 0 
                                               ! if 1 set to boundary value
                                               ! if 2 set to cyclic boundaries
    real(r8), intent(in), optional :: cyclicmin, cyclicmax

    type (interp_type), intent(out) :: interp_wgts
    real(r8) :: cmin, cmax
    real(r8) :: extrap
    real(r8) :: dyinwrap
    real(r8) :: ratio
    real(r8) :: avgdyin
    integer :: i, j, icount
    integer :: jj,iulog=0
    real(r8), pointer :: wgts(:)
    real(r8), pointer :: wgtn(:)
    integer, pointer :: jjm(:)
    integer, pointer :: jjp(:)
    logical :: increasing
    !
    ! Check validity of input coordinate arrays: must be monotonically increasing,
    ! and have a total of at least 2 elements
    !
    if (nin.lt.2) then
       stop 'LININTERP: Must have at least 2 input points for interpolation'
    end if
    if(present(cyclicmin)) then
       cmin=cyclicmin
    else
       cmin=0_r8
    end if
    if(present(cyclicmax)) then
       cmax=cyclicmax
    else
       cmax=360_r8
    end if
    if(cmax<=cmin) then
       stop 'LININTERP: cyclic min value must be < max value'
    end if
    increasing=.true.
    icount = 0
    do j=1,nin-1
       if (yin(j).gt.yin(j+1)) icount = icount + 1
    end do
    if(icount.eq.nin-1) then
       increasing = .false.
       icount=0
    endif
    if (icount.gt.0) then
       stop 'LININTERP: Non-monotonic input coordinate array found'
    end if
    allocate(interp_wgts%jjm(nout), &
         interp_wgts%jjp(nout), &
         interp_wgts%wgts(nout), &
         interp_wgts%wgtn(nout))

    jjm => interp_wgts%jjm
    jjp => interp_wgts%jjp
    wgts =>  interp_wgts%wgts
    wgtn =>  interp_wgts%wgtn

    !
    ! Initialize index arrays for later checking
    !
    jjm = 0
    jjp = 0

    extrap = 0.
    if(extrap_method.eq.0) then
       !
       ! For values which extend beyond boundaries, set weights
       ! such that values will be 0.
       !
       do j=1,nout
          if(increasing) then
             if (yout(j).lt.yin(1)) then
                jjm(j) = 1
                jjp(j) = 1
                wgts(j) = 0.
                wgtn(j) = 0.
                extrap = extrap + 1.
             else if (yout(j).gt.yin(nin)) then
                jjm(j) = nin
                jjp(j) = nin
                wgts(j) = 0.
                wgtn(j) = 0.
                extrap = extrap + 1.
             end if
          else
             if (yout(j).gt.yin(1)) then
                jjm(j) = 1
                jjp(j) = 1
                wgts(j) = 0.
                wgtn(j) = 0.
                extrap = extrap + 1.
             else if (yout(j).lt.yin(nin)) then
                jjm(j) = nin
                jjp(j) = nin
                wgts(j) = 0.
                wgtn(j) = 0.
                extrap = extrap + 1.
             end if
          end if
       end do
    else if(extrap_method.eq.1) then
       !
       ! For values which extend beyond boundaries, set weights
       ! such that values will just be copied.
       !
       do j=1,nout
          if(increasing) then
             if (yout(j).le.yin(1)) then
                jjm(j) = 1
                jjp(j) = 1
                wgts(j) = 1.
                wgtn(j) = 0.
                extrap = extrap + 1.
             else if (yout(j).gt.yin(nin)) then
                jjm(j) = nin
                jjp(j) = nin
                wgts(j) = 1.
                wgtn(j) = 0.
                extrap = extrap + 1.
             end if
          else
             if (yout(j).gt.yin(1)) then
                jjm(j) = 1
                jjp(j) = 1
                wgts(j) = 1.
                wgtn(j) = 0.
                extrap = extrap + 1.
             else if (yout(j).le.yin(nin)) then
                jjm(j) = nin
                jjp(j) = nin
                wgts(j) = 1.
                wgtn(j) = 0.
                extrap = extrap + 1.
             end if
          end if
       end do
    else if(extrap_method.eq.2) then
       !
       ! For values which extend beyond boundaries, set weights
       ! for circular boundaries 
       !
       dyinwrap = yin(1) + (cmax-cmin) - yin(nin)
       avgdyin = abs(yin(nin)-yin(1))/(nin-1.)
       ratio = dyinwrap/avgdyin
       if (ratio < 0.9 .or. ratio > 1.1) then
          write(iulog,*) 'Lininterp: Bad dyinwrap value =',dyinwrap,&
               ' avg=', avgdyin, yin(1),yin(nin)
          stop 'interpolate_data'
       end if

       do j=1,nout
          if(increasing) then
             if (yout(j) <= yin(1)) then
                jjm(j) = nin
                jjp(j) = 1
                wgts(j) = (yin(1)-yout(j))/dyinwrap
                wgtn(j) = (yout(j)+(cmax-cmin) - yin(nin))/dyinwrap
             else if (yout(j) > yin(nin)) then
                jjm(j) = nin
                jjp(j) = 1
                wgts(j) = (yin(1)+(cmax-cmin)-yout(j))/dyinwrap
                wgtn(j) = (yout(j)-yin(nin))/dyinwrap
             end if
          else
             if (yout(j) > yin(1)) then
                jjm(j) = nin
                jjp(j) = 1
                wgts(j) = (yin(1)-yout(j))/dyinwrap
                wgtn(j) = (yout(j)+(cmax-cmin) - yin(nin))/dyinwrap
             else if (yout(j) <= yin(nin)) then
                jjm(j) = nin
                jjp(j) = 1
                wgts(j) = (yin(1)+(cmax-cmin)-yout(j))/dyinwrap
                wgtn(j) = (yout(j)+(cmax-cmin)-yin(nin))/dyinwrap
             end if

          endif
       end do
    end if

    !
    ! Loop though output indices finding input indices and weights
    !
    if(increasing) then
       do j=1,nout
          do jj=1,nin-1
             if (yout(j).gt.yin(jj) .and. yout(j).le.yin(jj+1)) then
                jjm(j) = jj
                jjp(j) = jj + 1
                wgts(j) = (yin(jj+1)-yout(j))/(yin(jj+1)-yin(jj))
                wgtn(j) = (yout(j)-yin(jj))/(yin(jj+1)-yin(jj))
                exit
             end if
          end do
       end do
    else
       do j=1,nout
          do jj=1,nin-1
             if (yout(j).le.yin(jj) .and. yout(j).gt.yin(jj+1)) then
                jjm(j) = jj
                jjp(j) = jj + 1
                wgts(j) = (yin(jj+1)-yout(j))/(yin(jj+1)-yin(jj))
                wgtn(j) = (yout(j)-yin(jj))/(yin(jj+1)-yin(jj))
                exit
             end if
          end do
       end do
    end if

#ifndef SPMD
    !
    ! Check grid overlap
    !
    extrap = 100.*extrap/real(nout,r8)
!   if (extrap.gt.50. .and. .not. single_column) then
!      write(iulog,*) 'interpolate_data:','yout=',minval(yout),maxval(yout),increasing,nout
!      write(iulog,*) 'interpolate_data:','yin=',yin(1),yin(nin)
!      write(iulog,*) 'interpolate_data:',extrap,' % of output grid will have to be extrapolated'
!      call endrun('interpolate_data: ')
!   end if
#endif

    !
    ! Check that interp/extrap points have been found for all outputs
    !
    icount = 0
    do j=1,nout
       if (jjm(j).eq.0 .or. jjp(j).eq.0) icount = icount + 1
       ratio=wgts(j)+wgtn(j)
       if((ratio<0.9.or.ratio>1.1).and.extrap_method.ne.0) then
          write(iulog,*) j, wgts(j),wgtn(j),jjm(j),jjp(j), increasing,extrap_method
          stop 'Bad weight computed in LININTERP_init'
       end if
    end do
    if (icount.gt.0) then
       stop 'LININTERP: Point found without interp indices'
    end if

  end subroutine lininterp_init

!============================================================================================================

  subroutine lininterp2d2d(arrin, n1, n2, arrout, m1, m2, wgt1, wgt2)
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    integer, intent(in) :: n1, n2, m1, m2
    real(r8), intent(in) :: arrin(n1,n2)    ! input array of values to interpolate
    type(interp_type), intent(in) :: wgt1, wgt2
    real(r8), intent(out) :: arrout(m1,m2) ! interpolated array
    !
    ! locals
    !
    integer i,j                ! indices
    integer, pointer :: iim(:), jjm(:)
    integer, pointer :: iip(:), jjp(:)
                                                                                                  
    real(r8), pointer :: wgts1(:), wgts2(:)
    real(r8), pointer :: wgtn1(:), wgtn2(:)
                                                                                                  
    real(r8) :: arrtmp(n1,m2)
                                                                                                  
                                                                                                  
    jjm => wgt2%jjm
    jjp => wgt2%jjp
    wgts2 => wgt2%wgts
    wgtn2 => wgt2%wgtn
                                                                                                  
    iim => wgt1%jjm
    iip => wgt1%jjp
    wgts1 => wgt1%wgts
    wgtn1 => wgt1%wgtn

    do j=1,m2
      do i=1,n1
        arrtmp(i,j) = arrin(i,jjm(j))*wgts2(j) + arrin(i,jjp(j))*wgtn2(j)
      end do
    end do

    do j=1,m2
      do i=1,m1
        arrout(i,j) = arrtmp(iim(i),j)*wgts1(i) + arrtmp(iip(i),j)*wgtn1(i)
      end do
    end do
                                                                                                  
  end subroutine lininterp2d2d

!====================================================================================================

    subroutine rebin( nsrc, ntrg, src_x, trg_x, src, trg )
    !---------------------------------------------------------------
    !   ... rebin src to trg
    !---------------------------------------------------------------

    implicit none

    !---------------------------------------------------------------
    !   ... dummy arguments
    !---------------------------------------------------------------
    integer, intent(in)   :: nsrc                  ! dimension source array
    integer, intent(in)   :: ntrg                  ! dimension target array
    real(r8), intent(in)      :: src_x(nsrc+1)         ! source coordinates
    real(r8), intent(in)      :: trg_x(ntrg+1)         ! target coordinates
    real(r8), intent(in)      :: src(nsrc)             ! source array
    real(r8), intent(out)     :: trg(ntrg)             ! target array

    !---------------------------------------------------------------
    !   ... local variables
    !---------------------------------------------------------------
    integer  :: i, l, j
    integer  :: si, si1
    integer  :: sil, siu
    real(r8)     :: y
    real(r8)     :: sl, su
    real(r8)     :: tl, tu
    real(r8)     :: bot, top

    !---------------------------------------------------------------
    !   ... check interval overlap
    !---------------------------------------------------------------
    !     if( trg_x(1) < src_x(1) .or. trg_x(ntrg+1) > src_x(nsrc+1) ) then
    !        write(iulog,*) 'rebin: target grid is outside source grid'
    !        write(iulog,*) '       target grid from ',trg_x(1),' to ',trg_x(ntrg+1)
    !        write(iulog,*) '       source grid from ',src_x(1),' to ',src_x(nsrc+1)
    !        call endrun
    !     end if

   do i = 1, ntrg
       tl = trg_x(i+1)
       if( (tl < src_x(1)).and.(trg_x(i) > src_x(nsrc+1)) ) then
          do sil = 1,nsrc
             if( (tl-src_x(sil))*(tl-src_x(sil+1)).le.0.0_r8 ) then
                exit
             end if
          end do

          if( tl.lt.0.) sil = nsrc

          y = 0.0_r8
          bot = max(tl,src_x(nsrc+1))
          top = trg_x(i)
          do j = sil,1,-1
           if( top > src_x(j) ) then
            y = y+(src_x(j)-bot)/(src_x(j)-src_x(j+1))*src(j)
            bot = src_x(j)
           else
            y = y+(top-bot)/(src_x(j)-src_x(j+1))*src(j)
            exit
           endif
          enddo
          trg(i) = y
       else
        trg(i) = 0.0_r8
       end if
    end do

    if( trg_x(ntrg+1).gt.src_x(nsrc+1) ) then
     top = trg_x(ntrg+1)
     bot = src_x(nsrc+1)
     y = 0.0_r8
     do j=nsrc,1,-1
      if( top.gt. src_x(j) ) then
       y = y+(src_x(j)-bot)/(src_x(j)-src_x(j+1))*src(j)
       bot = src_x(j)
      else
       y = y+(top-bot)/(src_x(j)-src_x(j+1))*src(j)
       exit
      endif
     enddo
     trg(ntrg) = trg(ntrg)+y
    endif

  end subroutine rebin

!===============================================================================================================================
    subroutine rebin_v2( nsrc, ntrg, src_x, trg_x, src, trg )
    !---------------------------------------------------------------
    !   ... rebin src to trg
    !---------------------------------------------------------------

    implicit none

    !---------------------------------------------------------------
    !   ... dummy arguments
    !---------------------------------------------------------------
    integer, intent(in)   :: nsrc                  ! dimension source array
    integer, intent(in)   :: ntrg                  ! dimension target array
    real(r8), intent(in)      :: src_x(nsrc+1)         ! source coordinates
    real(r8), intent(in)      :: trg_x(ntrg+1)         ! target coordinates
    real(r8), intent(in)      :: src(nsrc)             ! source array
    real(r8), intent(out)     :: trg(ntrg)             ! target array

    !---------------------------------------------------------------
    !   ... local variables
    !---------------------------------------------------------------
    integer  :: i, l, j
    integer  :: si, si1
    integer  :: sil, siu
    real(r8)     :: y
    real(r8)     :: sl, su
    real(r8)     :: tl, tu
    real(r8)     :: bot, top

    !---------------------------------------------------------------
    !   ... check interval overlap
    !---------------------------------------------------------------
    !     if( trg_x(1) < src_x(1) .or. trg_x(ntrg+1) > src_x(nsrc+1) ) then
    !        write(iulog,*) 'rebin: target grid is outside source grid'
    !        write(iulog,*) '       target grid from ',trg_x(1),' to ',trg_x(ntrg+1)
    !        write(iulog,*) '       source grid from ',src_x(1),' to ',src_x(nsrc+1)
    !        call endrun
    !     end if

   do i = 1, ntrg
       tl = trg_x(i+1)
       if( (tl > src_x(1)).and.(trg_x(i) < src_x(nsrc+1)) ) then
          do sil = 1,nsrc
             if( (tl-src_x(sil))*(tl-src_x(sil+1)).le.0.0_r8 ) then
                exit
             end if
          end do

          if( tl.gt.src_x(nsrc+1)) sil = nsrc

          y = 0.0_r8
          bot = min(tl,src_x(nsrc+1))
          top = trg_x(i)
          do j = sil,1,-1
           if( top < src_x(j) ) then
            y = y+(bot-src_x(j))/(src_x(j+1)-src_x(j))*src(j)
            bot = src_x(j)
           else
            y = y+(bot-top)/(src_x(j+1)-src_x(j))*src(j)
            exit
           endif
          enddo
          trg(i) = y
       else
        trg(i) = 0.0_r8
       end if
    end do

    if( trg_x(ntrg+1).lt.src_x(nsrc+1) ) then
     top = trg_x(ntrg+1)
     bot = src_x(nsrc+1)
     y = 0.0_r8
     do j=nsrc,1,-1
      if( top.lt. src_x(j) ) then
       y = y+(bot-src_x(j))/(src_x(j+1)-src_x(j))*src(j)
       bot = src_x(j)
      else
       y = y+(bot-top)/(src_x(j+1)-src_x(j))*src(j)
       exit
      endif
     enddo
     trg(ntrg) = trg(ntrg)+y
    endif

  end subroutine rebin_v2


  subroutine lininterp_finish(interp_wgts)
    type(interp_type) :: interp_wgts

    deallocate(interp_wgts%jjm, &
         interp_wgts%jjp, &
         interp_wgts%wgts, &
         interp_wgts%wgtn)

    nullify(interp_wgts%jjm, &
         interp_wgts%jjp, &
         interp_wgts%wgts, &
         interp_wgts%wgtn)
  end subroutine lininterp_finish


end module convertFAA
