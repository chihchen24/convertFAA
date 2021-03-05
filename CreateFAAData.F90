program CreateFAAData
!------------------------------------------------------------------------
! This program reads in a series of FAA ascii files and writes out the 
! data to individual netCDF files
!
! The number of times is driven by the number of input files
!------------------------------------------------------------------------

  use netcdf
  use convertFAA
  use FAARoutines

  implicit none

!---------------------------------------------------
! Parameters which need to be set by hand
!---------------------------------------------------
  real(r8),parameter:: inputSpacSec=3600.  ! number of seconds between input data points
  integer,parameter :: STARTDAYSOUT=1
  integer,parameter :: ENDDAYSOUT=365
  logical,parameter :: AVGDAY=.false. ! set directory to below to DayAvg
  logical,parameter :: AVGMONTH=.true. ! set directory to below to MonthlyAvg


! For Photon
! character*256,dimension(1) :: climFile(1)='/data/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_01_climo.nc'
! character(len=80) :: fileprefix=('/data/cacraig/FAAData_netCDF-short3/')

!! For Bluefire
!!  Copied from /project/utls/satftp1/ccsm3/microp2/test1yr/ camdev55_cam3_6_72_u175_1xco2_icealb2 on CGD machines
! character*256 :: climFile(12)=(/'/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_01_climo.nc',&
!                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_02_climo.nc',&
!                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_03_climo.nc',&
!                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_04_climo.nc',&
!                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_05_climo.nc',&
!                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_06_climo.nc',&
!                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_07_climo.nc',&
!                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_08_climo.nc',&
!                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_09_climo.nc',&
!                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_10_climo.nc',&
!                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_11_climo.nc',&
!                                 '/ptmp/cacraig/dataCopies/camdev55_cam3_6_72_u175_1xco2_icealb2_12_climo.nc'/)
!
!  character(len=44) :: fileprefix=('/fis/cgd/home/cacraig/ACCRI_2006_MixRat/')

! For Calgary/Edinburgh
  character*256 :: climFile(12)=(/'/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_01_climo.nc',&
                                  '/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_02_climo.nc',&
                                  '/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_03_climo.nc',&
                                  '/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_04_climo.nc',&
                                  '/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_05_climo.nc',&
                                  '/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_06_climo.nc',&
                                  '/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_07_climo.nc',&
                                  '/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_08_climo.nc',&
                                  '/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_09_climo.nc',&
                                  '/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_10_climo.nc',&
                                  '/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_11_climo.nc',&
                                  '/glade/scratch/cchen/camdev55_cam3_6_72_u175_1xco2_icealb2/camdev55_cam3_6_72_u175_1xco2_icealb2_12_climo.nc'/)

 character(len=80) :: fileprefix=('/glade/scratch/cchen/FAA_monthly/')
  
  type(vids_type) :: vids

  character(len=300)  :: string
  integer             :: i,ierr,j,k,l
  integer             :: ntime

  integer,dimension(500000)               :: month,day,ilat,ilon,ialt
  integer                                 :: didLat,didLon,didlev,didilev,didTime,levid,varid,latid,lonid
  real(r8),dimension(NLON_FAA,NLAT_FAA,NALT_FAA,NFIELDS_FAA):: AllData
  real,allocatable,dimension(:,:,:,:)     :: mixing_ratio  ! (NLON_FAA,NLAT_FAA,NLEV_CLIMO,NFIELDS_FAA2)
  integer,dimension(MAXDIMS)              :: start,count,stride
  integer,dimension(500000)               :: hour
  integer,dimension(1)                    :: date,datesec

  integer :: status,ifield,itime,iday,nDaysRead,nTimesRead,nTimesOut
  integer :: nlev_CLIMO,nlat_CLIMO,nlon_CLIMO,indexRev

  real(r8):: latGrid(NLAT_FAA),lonGrid(NLON_FAA),altgrid_int(NALT_FAA+1),altgrid_int3d(NLON_FAA,NLAT_FAA,NALT_FAA+1)
!++ alt pres
  real(r8) :: alt_pres_int(NLON_FAA,NLAT_FAA,NALT_FAA+1)
!-- alt pres

  real(r8)                                 :: inputSecsRead
  real(r8)                                 :: P0_CLIMO
  real(r8), allocatable,dimension(:)       :: hyam_CLIMO,hybm_CLIMO
  real(r8), allocatable,dimension(:)       :: hyai_CLIMO,hybi_CLIMO
  real(r8), allocatable,dimension(:,:,:)   :: pin_CLIMO

  real(r8), dimension(:), pointer          :: lev

  character(len=30) :: datestr
  character(len=3)  :: monthstr
  character(len=10) :: unitsStr

  character(len=500),dimension(:),pointer     :: inputfile

  character(len=10),dimension(NFIELDS_FAA) :: Fields=(/'SLANT_DIST','TRACK_DIST',&
  'FUELBURN  ','CO        ','HC        ','NOX       ','PMNV      ','PMSO      ',&
  'PMFO      ','CO2       ','H2O       ','SOX       ','BC        ','OC        '/)
  character(len=19) :: filesuffix=('_grid.nc')
  character(len=80) :: filelist
  real(r8) :: alt_pres_20(20)
  data alt_pres_20 /101325., 99507., 97716., 95951., 94212., &
                    92499., 90811., 89148., 87510., 85896., &
                    84307., 82741., 81199., 79681., 78185., &
                    76712., 75262., 73834., 72428., 71044./
!-------------------------------------------------------------------
! Set up the lat/lon/alt arrays
!-------------------------------------------------------------------

  do i=1,NLAT_FAA
    latGrid(i)=-89.5+i-1
  end do

  do i=1,NLON_FAA
    lonGrid(i)=.5+i-1
  end do

! Reverse the order of heights for the model and store as meters
  do i=1,NALT_FAA+1
    indexRev=NALT_FAA+1-i+1
    altGrid_int(indexRev)=0+(i-1)*152.4_r8
    altgrid_int3d(:,:,indexRev)=altgrid_int(indexRev)
  end do

  write(0,*) ' altgrid_int=',altgrid_int

!++ alt pres
  j = 0
  do i=NALT_FAA+1,NALT_FAA-18,-1
    j = j+1
    alt_pres_int(:,:,i) = alt_pres_20(j)
  enddo
  do i=1,NALT_FAA-19
     if( altGrid_int(i) < 11000.0_r8 ) then
         alt_pres_int(:,:,i) = 101325.0_r8*(1.0_r8-0.0065_r8*altGrid_int(i)/288.15_r8)**5.2561_r8
     else
         alt_pres_int(:,:,i) = 22632.0_r8*exp(-9.80665/(287.04_r8*216.65_r8)*(altGrid_int(i)-11000.0_r8))
     endif
  enddo



 do i=1,NALT_FAA+1
 write(*,*) 'altitude pressure = ', i, alt_pres_int(1,1,i), altGrid_int(i)
 enddo

!  do i=1,NALT_FAA+1
!     if( altGrid_int(i) < 11000.0_r8 ) then
!         alt_pres_int(:,:,i) = 101325.0_r8*(1.0_r8-0.0065_r8/288.15_r8)**5.2561_r8
!     else
!         alt_pres_int(:,:,i) = 26632.0_r8*exp(-9.80665/(287.04_r8*216.65_r8)*(altGrid_int(i)-11000.0_r8))
!     endif
!  enddo
!-- alt pres
!-------------------------------------------------------------------
! Loop over all days, skipping by the number of days being averaged
!-------------------------------------------------------------------
  iday=STARTDAYSOUT

  do while(iday <= ENDDAYSOUT)

!-------------------------------------------------------------------
! Determine how many times there are and allocate arrays appropriately
!-------------------------------------------------------------------

    if (AvgMonth) then
      call getFileListMonth(iday,ntime,inputfile,nDaysRead)
      nTimesOut=1
      nTimesRead=ntime
    else if (AvgDay) then
      call getFileList(iday,ntime,inputfile,nDaysRead)
      nTimesOut=1
      nTimesRead=ntime
    else
      call getFileList(iday,ntime,inputfile,nDaysRead)
      nTimesOut=ntime
      nTimesRead=1
    endif

    inputSecsRead=inputSpacSec*nTimesRead

!----------------------------------------------------------
! Determine the first and last days and put in filenames
! NOTE - This assumes that all of the input data contains "2006"
!----------------------------------------------------------

    call getDateStr(inputfile,ntime,datestr,monthstr)
  
!----------------------------------------------------------
! Read the climatology file to retrieve the number of levels and number of levels of intervals
!  (lev and ilev) 
!    ASSUMES THIS IS THE SAME FOR EVERY FILE AND JUST USES THE FIRST ONE
!----------------------------------------------------------

     if (iday == STARTDAYSOUT) then
        call  getClimInfo(climFile(1), nlat_CLIMO, nlon_CLIMO, nlev_CLIMO, lev)

!----------------------------------------------------------
! On the first day - allocate all of the size-dependent variables needed by the program
!----------------------------------------------------------

       allocate(hyam_CLIMO(nlev_CLIMO))
       allocate(hybm_CLIMO(nlev_CLIMO))
       allocate(hyai_CLIMO(nlev_CLIMO+1))
       allocate(hybi_CLIMO(nlev_CLIMO+1))
       allocate(pin_CLIMO(nlon_FAA,nlat_FAA,nlev_CLIMO+1))
       allocate(mixing_ratio(nlon_FAA,nlat_FAA,nlev_CLIMO,nfields_FAA2))

     end if

!----------------------------------------------------------
! Create and write out the netCDF file (using compression)
!    Skip SLANT_DIST and TRACK_DIST
!----------------------------------------------------------

    call createNCDF(fileprefix,monthstr,fields,datestr,filesuffix,nlev_CLIMO,vids)

!----------------------------------------------------------
! Loop over every output file
!----------------------------------------------------------

    do itime=1,ntimesOut

!----------------------------------------------------------
! Read in the data for the entire filelist
!----------------------------------------------------------

       call readData(inputfile,itime,nTimesRead,month,day,hour,ilat,ilon,ialt,AllData)

!++ Chen, discard emission for K > 90
       do i=1,NALT_FAA
         if( altGrid_int(i+1) > 90.0_r8 * 152.4_r8 ) then
          AllData(:,:,i,1:NFIELDS_FAA) = 0.0_r8
         endif
       enddo
! BC & OC
      do i=1,NALT_FAA
         if( altGrid_int(i) <= 6.0_r8 * 152.4_r8 ) then
          AllData(:,:,i,NFIELDS_FAA-1) = AllData(:,:,i,7)
          AllData(:,:,i,NFIELDS_FAA) = AllData(:,:,i,9)
         else
          AllData(:,:,i,NFIELDS_FAA-1) = 0.03_r8 * AllData(:,:,i,3)/1000.0_r8
          AllData(:,:,i,NFIELDS_FAA) = 0.03_r8 * AllData(:,:,i,3)/1000.0_r8
         endif
      enddo
!-- Chen

!----------------------------------------------------------
! Convert from grams/hour mixing ratio (just the fields from index 3 onwards in field direction)
! output units are kg/kg/sec
!     Pick the climFile based on the month(1) in the FAA file
!----------------------------------------------------------

!++ Chen, pass all AllData in!!
!        call convertFAAtoMixRat(inputSecsRead,climFile(month(1)),NLON_FAA, NLAT_FAA, NALT_FAA, NFIELDS_FAA2, nlon_CLIMO, &
!                             nlat_CLIMO, nlev_CLIMO, lonGrid, latGrid, altGrid_int3d, AllData(:,:,:,3:NFIELDS_FAA), &
!                             P0_CLIMO, pin_CLIMO, hyam_CLIMO, hybm_CLIMO, hyai_CLIMO, hybi_CLIMO, mixing_ratio)
        call convertFAAtoMixRat(inputSecsRead,climFile(month(1)),NLON_FAA, NLAT_FAA, NALT_FAA, NFIELDS_FAA2, nlon_CLIMO, &
                             nlat_CLIMO, nlev_CLIMO, lonGrid, latGrid, altGrid_int3d, alt_pres_int, AllData(:,:,:,1:NFIELDS_FAA), &
                             P0_CLIMO, pin_CLIMO, hyam_CLIMO, hybm_CLIMO, hyai_CLIMO, hybi_CLIMO, mixing_ratio)
!-- Chen

!----------------------------------------------------------
! If this is the first pass, fill in the grid vectors for each file (field)
!   Skip the SLANT_DIST and TRACK_DIST
!----------------------------------------------------------

        if (itime == 1) call writeHeaderNCDF(vids,nlev_CLIMO,latgrid,longrid,lev,hyam_CLIMO,&
                                             hybm_CLIMO,hyai_CLIMO,hybi_CLIMO,p0_climo)

!----------------------------------------------------------
! write out the data to each file (field)
!----------------------------------------------------------

        call writeDataNCDF(itime,nlev_CLIMO,vids,fields,pin_CLIMO,month,day,hour,mixing_ratio)

     end do ! itime

!----------------------------------------------------------
! Close the netCDF file
!----------------------------------------------------------

!++ Chen
     do ifield=1,NFIELDS_FAA
!     do ifield = 1, 2
        status=nf90_close(vids%ncid(ifield))
        if (status /= nf90_noerr) call handleErr(status,'160')
     end do
!     do ifield=3,NFIELDS_FAA
!        status=nf90_close(vids%ncid(ifield))
!        if (status /= nf90_noerr) call handleErr(status,'160')
!     end do
!-- Chen

  deallocate(inputfile)

  iday=iday+nDaysRead

end do  ! Number of days

end program CreateFAAData
