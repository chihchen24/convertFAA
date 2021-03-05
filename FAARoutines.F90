module FAARoutines

use netcdf

implicit none
save

integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real
integer,parameter :: R8 = SHR_KIND_R8
  
! All is public

  integer,parameter :: NLAT_FAA=180, NLON_FAA=360, NALT_FAA=150
  integer,parameter :: NFIELDS_FAA=14
  integer,parameter :: NFIELDS_FAA2=12  ! does not include track_dist and fuelburn
  integer,parameter :: MAXDIDS=4
  integer,parameter :: MAXDIMS=4

type vids_type
  integer,dimension(NFIELDS_FAA)   :: ncid
  integer,dimension(NFIELDS_FAA)   :: vidmix,vidlev
  integer,dimension(NFIELDS_FAA)   :: vidlat,vidlon,vidPS,vidalt_int,viddate,viddatesec
  integer,dimension(NFIELDS_FAA)   :: vidhyam,vidhybm,vidp0
  integer,dimension(NFIELDS_FAA)   :: vidhyai,vidhybi
end type

contains


subroutine getFileList(iday,ntime,inputfile,nDaysRead)

!-------------------------------------------------------------------
! This routine gets the entire file list of data which will be output
! in one netCDF file
!-------------------------------------------------------------------

  integer,intent(in)  :: iday
  integer,intent(out) :: ntime
  character(len=500),dimension(:),pointer     :: inputfile
  integer,intent(out) :: nDaysRead

  character(len=80)   :: filelist
  character(len=500)  :: tempChar

  integer             :: i

  nDaysRead=1

!-------------------------------------------------------------------
! Determine how many times there are and allocate arrays appropriately
!-------------------------------------------------------------------
  if (iday < 10) then
    write(filelist,995) 'fileLists/InputFAAFileList-',iday,'.txt'
995 format(a,i1,a)
  else if (iday < 100) then
    write(filelist,994) 'fileLists/InputFAAFileList-',iday,'.txt'
994 format(a,i2,a)
  else
    write(filelist,993) 'fileLists/InputFAAFileList-',iday,'.txt'
993 format(a,i3,a)
  end if

  write(0,*) ' ----- ',filelist
  open(file=filelist,unit=10)

  ntime=0
  do
    ntime=ntime+1
    read(10,997,end=10) tempChar
997 format(a500)
  end do

10 continue

  ntime=ntime-1
  rewind 10

  allocate(inputfile(ntime))

  do i=1,ntime
    read(10,997) inputfile(i)
  end do

end subroutine getFileList

subroutine getFileListMonth(idayStart,ntime,inputfile,nDaysRead)

!-------------------------------------------------------------------
! This routine gets the entire file list of data which will be output
! in one netCDF file for an entire month
!-------------------------------------------------------------------

  integer,intent(in)  :: idayStart
  integer,intent(out) :: ntime
  character(len=500),dimension(:),pointer     :: inputfile
  integer,intent(out) :: nDaysRead

  integer,parameter :: NDAYSMONTH(12) =(/  31,   28,   31,   30,   31,   30,   31,   31,   30,   31,   30,   31 /)
  integer,parameter :: JULDAY(12)     =(/   1,   32,   60,   91,  121,  152,  182,  213,  244,  274,  305,  335 /)

  integer             :: iday,idayStop,ntimesFile
  character(len=80)   :: filelist
  character(len=500)  :: tempChar

  integer             :: icnt,indxMonth,i

  idayStop=0

!-------------------------------------------------------------------
! Figure out which month are averaging based on the input day
!-------------------------------------------------------------------
   do indxMonth=1,12
     if (JULDAY(indxMonth) == idayStart) then
       idayStop=idayStart+NDAYSMONTH(indxMonth)-1
       exit
     endif
   end do

   nDaysRead=NDAYSMONTH(indxMonth)

   if (idayStop == 0) then
     write(0,*) ' Requesting monthly average with start day=',idayStart,' Must start at beginning of month'
     stop
   end if

!-------------------------------------------------------------------
! Determine how many times there are in the first file and allocate arrays appropriately
! Assume that there are the same number of times in every data file
!-------------------------------------------------------------------
  if (idayStart < 10) then
    write(filelist,995) 'fileLists/InputFAAFileList-',idayStart,'.txt'
995 format(a,i1,a)
  else if (idayStart < 100) then
    write(filelist,994) 'fileLists/InputFAAFileList-',idayStart,'.txt'
994 format(a,i2,a)
  else
    write(filelist,993) 'fileLists/InputFAAFileList-',idayStart,'.txt'
993 format(a,i3,a)
  end if


  open(file=filelist,unit=10)

  ntimesFile=0
  do
    ntimesFile=ntimesFile+1
    read(10,997,end=10) tempChar
997 format(a500)
  end do

10 continue

  ntimesFile=ntimesFile-1

  close(10)

  allocate(inputfile(ntimesFile*nDaysRead))

!-------------------------------------------------------------------
! Loop over all days
!-------------------------------------------------------------------
  ntime=0
  do iday=idayStart,idayStop

    if (iday < 10) then
      write(filelist,995) 'fileLists/InputFAAFileList-',iday,'.txt'
    else if (iday < 100) then
      write(filelist,994) 'fileLists/InputFAAFileList-',iday,'.txt'
    else
      write(filelist,993) 'fileLists/InputFAAFileList-',iday,'.txt'
    end if

    write(0,*) ' ----- ',filelist
    open(file=filelist,unit=10)

    do i=1,ntimesFile
      ntime=ntime+1
      read(10,997) inputfile(ntime)
    end do
  end do

end subroutine getFileListMonth

subroutine getDateStr(inputfile,ntime,datestr,monthstr)

!-------------------------------------------------------------------
! This routine returns the date and month strings
!-------------------------------------------------------------------
  character(len=500),dimension(:),pointer     :: inputfile
  integer,intent(in)            :: ntime
  character(len=30),intent(out) :: datestr
  character(len=3),intent(out)  :: monthstr

  integer :: ifirst1,ilast1,ifirst2,ilast2

!----------------------------------------------------------
! Determine the first and last days and put in filenames
! NOTE - This assumes that all of the input data contains "2006"
!----------------------------------------------------------

  ifirst1= index(inputfile(1),'/',.true.)+1
  ifirst2= index(inputfile(1),'_2006',BACK=.true.)+4
  ilast1= index(inputfile(ntime),'/',.true.)+1
  ilast2= index(inputfile(ntime),'_2006',BACK=.true.)+4

  if (inputfile(1)(ifirst1:ifirst2) ==  inputfile(ntime)(ilast1:ilast2)) then
    datestr='_'//inputfile(1)(ifirst1:ifirst2)
  else
    datestr='_'//inputfile(1)(ifirst1:ifirst2)//'_to_'//inputfile(ntime)(ilast1:ilast2)
  end if

  monthstr=inputfile(1)(ifirst1-4:ifirst1-1)

end subroutine getDateStr

subroutine createNCDF(fileprefix,monthstr,fields,datestr,filesuffix,nlev_CLIMO,vids)

!----------------------------------------------------------
! This routine creates the netCDF files (one per field)
!----------------------------------------------------------

  character(len=*),intent(in)              :: fileprefix
  character(len=*),intent(in)              :: monthstr
  character(len=*),dimension(:),intent(in) :: fields
  character(len=*),intent(in)              :: datestr
  character(len=*),intent(in)              :: filesuffix
  integer,intent(in)                       :: nlev_CLIMO
  type (vids_type),intent(out) :: vids
  
  integer :: didLat,didLon,didlev,didilev,didTime,levid,varid,latid,lonid,status
  integer :: dids(MAXDIDS),didone(1),didthree(3),didfour(4)
  integer :: ifield
  integer,dimension(MAXDIMS)    :: chunksizes

!++ Chen
!  do ifield=1,2
  do ifield=1,NFIELDS_FAA
!--Chen

!     Create file
     write(0,*) trim(fileprefix)//trim(monthstr)//'/'//trim(fields(ifield))//trim(datestr)//trim(filesuffix)
     status=nf90_create(path=trim(fileprefix)//trim(monthstr)//'/'//trim(fields(ifield))//&
                             trim(datestr)//trim(filesuffix), cmode=nf90_clobber,ncid=vids%ncid(ifield))
     if (status /= nf90_noerr) call handleErr(status,'100')

!     Create dimensions
     status=nf90_def_dim(vids%ncid(ifield),"lat",NLAT_FAA,didLat)
     if (status /= nf90_noerr) call handleErr(status,'101')
  
     status=nf90_def_dim(vids%ncid(ifield),"lon",NLON_FAA,didLon)
     if (status /= nf90_noerr) call handleErr(status,'102')

     status=nf90_def_dim(vids%ncid(ifield),"lev",nlev_CLIMO,didlev)
     if (status /= nf90_noerr) call handleErr(status,'103')

     status=nf90_def_dim(vids%ncid(ifield),"ilev",nlev_CLIMO+1,didilev)
     if (status /= nf90_noerr) call handleErr(status,'104')

     status=nf90_def_dim(vids%ncid(ifield),"time",nf90_unlimited,didTime)
     if (status /= nf90_noerr) call handleErr(status,'105')

!   Create variables

     dids(1)=didLon
     dids(2)=didLat
     dids(3)=didlev
     dids(4)=didTime

     chunksizes(1)=NLON_FAA
     chunksizes(2)=NLAT_FAA
     chunksizes(3)=1
     chunksizes(4)=1

     status=nf90_def_var(vids%ncid(ifield),'ac_'//trim(fields(ifield)),nf90_float,dids,vids%vidmix(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for ac_'//trim(fields(ifield)))
!++ Chen
     if( ifield < 3) then
     status=nf90_put_att(vids%ncid(ifield),vids%vidmix(ifield),'units','m/sec')
     else
     status=nf90_put_att(vids%ncid(ifield),vids%vidmix(ifield),'units','kg/kg/sec')
     endif
!-- Chen
     if (status /= nf90_noerr) call handleErr(status,'111')

!   Create the lat/lon/alt/time vectors

     didone(1)=didLat
     status=nf90_def_var(vids%ncid(ifield),'lat',nf90_float,didone,vids%vidlat(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for lat')
     status=nf90_put_att(vids%ncid(ifield),vids%vidlat(ifield),'units','deg')
     if (status /= nf90_noerr) call handleErr(status,'111')

     didone(1)=didLon
     status=nf90_def_var(vids%ncid(ifield),'lon',nf90_float,didone,vids%vidlon(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for lon')
     status=nf90_put_att(vids%ncid(ifield),vids%vidlon(ifield),'units','deg')
     if (status /= nf90_noerr) call handleErr(status,'111')

     didthree(1)=didLon
     didthree(2)=didLAT
     didthree(3)=didtime
     status=nf90_def_var(vids%ncid(ifield),'PS',nf90_float,didthree,vids%vidPS(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for PS')
     status=nf90_put_att(vids%ncid(ifield),vids%vidPS(ifield),'units','Pa')
     if (status /= nf90_noerr) call handleErr(status,'111')

     didone(1)=didlev
     status=nf90_def_var(vids%ncid(ifield),'hyam',nf90_float,didone,vids%vidhyam(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for hyam')

     didone(1)=didlev
     status=nf90_def_var(vids%ncid(ifield),'lev',nf90_float,didone,vids%vidlev(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for hyam')

     didone(1)=didlev
     status=nf90_def_var(vids%ncid(ifield),'hybm',nf90_float,didone,vids%vidhybm(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for hybm')

     status=nf90_def_var(vids%ncid(ifield),'P0',nf90_float,varid=vids%vidp0(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for P0')

     didone(1) = didilev
     status=nf90_def_var(vids%ncid(ifield),'hyai',nf90_float,didone,vids%vidhyai(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for hyai')

     didone(1) = didilev
     status=nf90_def_var(vids%ncid(ifield),'hybi',nf90_float,didone,vids%vidhybi(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for hybi')

     didone(1)=didTime
     status=nf90_def_var(vids%ncid(ifield),'date',nf90_int,didone,vids%viddate(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for date')

     didone(1)=didTime
     status=nf90_def_var(vids%ncid(ifield),'datesec',nf90_int,didone,vids%viddatesec(ifield))
     if (status /= nf90_noerr) call handleErr(status,'nf90_def_var for datesec')

!----------------------------------------------------------
! Set the chunking for the array
!----------------------------------------------------------

!    chunksizes(1)=NLON_FAA
!    chunksizes(2)=NLAT_FAA
!    chunksizes(3)=1
!    chunksizes(4)=1

!    status=nf90_def_var_chunking(vids%ncid(ifield),vids%vidmix(ifield),NF90_CHUNKED,chunksizes)
!    if (status /= nf90_noerr) call handleErr(status,'300')

!----------------------------------------------------------
! Set the compression for the array
!----------------------------------------------------------

!    status=nf90_def_var_deflate(vids%ncid(ifield),vids%vidmix(ifield),0,1,2)
!    if (status /= nf90_noerr) call handleErr(status,'200')


!----------------------------------------------------------
! end the definition
!----------------------------------------------------------

     status=nf90_enddef(vids%ncid(ifield))
     if (status /= nf90_noerr) call handleErr(status,'130')
  enddo

end subroutine createNCDF

subroutine getClimInfo(climFile, nlat_CLIMO, nlon_CLIMO, nlev_CLIMO, lev)

!----------------------------------------------------------
! This subroutine get the dimensions of the climatology file
!----------------------------------------------------------

  character(len=*),intent(in)   :: climFile
  integer,intent(out)           :: nlat_CLIMO, nlon_CLIMO, nlev_CLIMO
  real(r8),dimension(:),pointer :: lev

  integer :: status,ncid
  integer                                 :: levid,varid,latid,lonid

!----------------------------------------------------------
! Read the climatology file to retrieve the number of levels and number of levels of intervals
!  (lev and ilev)
!    ASSUMES THIS IS THE SAME FOR EVERY FILE AND JUST USES THE FIRST ONE
!----------------------------------------------------------

   status=nf90_open(climFile,NF90_NOWRITE,ncid)
   write(0,*) ' climFile=',trim(climFile)

   if (status /= nf90_noerr) call handleErr(status,'createFAAData 100')

   status = nf90_inq_dimid(ncid,'lat',latid)
   if (status /= nf90_noerr) call handleErr(status,'createFAAData  get CLIMO lat id ')
   status = nf90_inquire_dimension(ncid,latid,len=nlat_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'createFAAData get CLIMO lat dimension')

   status = nf90_inq_dimid(ncid,'lon',lonid)
   if (status /= nf90_noerr) call handleErr(status,'createFAAData get CLIMO lon id')
   status = nf90_inquire_dimension(ncid,lonid,len=nlon_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'createFAAData get CLIMO lon dimension')

   status = nf90_inq_dimid(ncid,'lev',levid)
   if (status /= nf90_noerr) call handleErr(status,'createFAAData get CLIMO lev id')
   status = nf90_inquire_dimension(ncid,levid,len=nlev_CLIMO)
   if (status /= nf90_noerr) call handleErr(status,'createFAAData get CLIMO lev dimension')

   allocate(lev(nlev_CLIMO))
   status = nf90_inq_varid(ncid,'lev',varid)
   if (status /= nf90_noerr) call handleErr(status,'createFAAData get CLIMO lev variable id')
   status = nf90_get_var(ncid,varid,lev)
   if (status /= nf90_noerr) call handleErr(status,'createFAAData - Problem reading CLIMO lev ')

end subroutine getClimInfo


subroutine writeHeaderNCDF(vids,nlev_CLIMO,latgrid,longrid,lev,hyam_CLIMO,hybm_CLIMO,hyai_CLIMO,hybi_CLIMO,p0_climo)

!-------------------------------------------------------------------
! This routine writes out the one time (header) information
!-------------------------------------------------------------------

  type(vids_type)                 :: vids
  integer                         :: nlev_CLIMO
  real(r8)                        :: latGrid(:),lonGrid(:)
  real(r8)                        :: P0_CLIMO
  real(r8), dimension(:)          :: hyam_CLIMO,hybm_CLIMO
  real(r8), dimension(:)          :: hyai_CLIMO,hybi_CLIMO
  real(r8), dimension(:), pointer :: lev

  integer :: ifield,status
  integer,dimension(MAXDIMS)              :: start,count,stride

  count=0
  start=1
  stride=1

!++ Chen
  do ifield=1,NFIELDS_FAA
!  do ifield=1,2
!-- Chen
    count(1)=NLAT_FAA
    status=nf90_put_var(vids%ncid(ifield),vids%vidlat(ifield),latgrid,start,count,stride)
    if (status /= nf90_noerr) call handleErr(status,'nf90_put var for lat')

    count(1)=NLON_FAA
    status=nf90_put_var(vids%ncid(ifield),vids%vidlon(ifield),longrid(:),start,count,stride)
    if (status /= nf90_noerr) call handleErr(status,'nf90_put var for lon')

    count(1)=nlev_CLIMO
    status=nf90_put_var(vids%ncid(ifield),vids%vidlev(ifield),lev(:),start,count,stride)
    if (status /= nf90_noerr) call handleErr(status,'nf90_put var for lev')

    count(1)=nlev_CLIMO
    status=nf90_put_var(vids%ncid(ifield),vids%vidhyam(ifield),hyam_CLIMO(:),start,count,stride)
    if (status /= nf90_noerr) call handleErr(status,'nf90_put var for hyam')

    count(1)=nlev_CLIMO
    status=nf90_put_var(vids%ncid(ifield),vids%vidhybm(ifield),hybm_CLIMO(:),start,count,stride)
    if (status /= nf90_noerr) call handleErr(status,'nf90_put var for hybm')

    count(1)=nlev_CLIMO+1
    status=nf90_put_var(vids%ncid(ifield),vids%vidhyai(ifield),hyai_CLIMO(:),start,count,stride)
    if (status /= nf90_noerr) call handleErr(status,'nf90_put var for hyai')

    count(1)=nlev_CLIMO+1
    status=nf90_put_var(vids%ncid(ifield),vids%vidhybi(ifield),hybi_CLIMO(:),start,count,stride)
    if (status /= nf90_noerr) call handleErr(status,'nf90_put var for hybi')

    status=nf90_put_var(vids%ncid(ifield),vids%vidp0(ifield),p0_climo)
    if (status /= nf90_noerr) call handleErr(status,'nf90_put var for p0')

  end do

end subroutine writeHeaderNCDF

subroutine writeDataNCDF(itime,nlev_CLIMO,vids,fields,pin_CLIMO,month,day,hour,mixing_ratio)

!-------------------------------------------------------------------
! This routine writes out the data to each file (field)
!-------------------------------------------------------------------
  integer,intent(in)                   :: itime
  integer,intent(in)                   :: nlev_CLIMO
  type(vids_type)                      :: vids
  character(len=*),dimension(:)        :: fields
  real(r8),dimension(:,:,:)            :: pin_CLIMO
  integer,dimension(500000),intent(in) :: month,day,hour
  real,dimension(:,:,:,:),intent(in)   :: mixing_ratio  ! (NLON_FAA,NLAT_FAA,NLEV_CLIMO,NFIELDS_FAA2)

  integer,dimension(MAXDIMS)           :: start,count,stride
  integer                              :: ifield
  integer                              :: status
  integer,dimension(1)                 :: date,datesec

 !++ Chen
 ! do ifield=1,2
  do ifield=1,NFIELDS_FAA
 !--Chen

!        Put 3D field in 4D output variable
     start(1)=1
     start(2)=1
     start(3)=itime
     count(1)=NLON_FAA
     count(2)=NLAT_FAA
     count(3)=1
     stride=1
     status=nf90_put_var(vids%ncid(ifield),vids%vidps(ifield),pin_CLIMO(:,:,nlev_CLIMO+1),start,count,stride)
     if (status /= nf90_noerr) call handleErr(status,'nf90_put var for ps')

     date=month(1)*100+day(1)
     start(1)=itime
     count(1)=1
     stride=1
     status=nf90_put_var(vids%ncid(ifield),vids%viddate(ifield),date,start,count,stride)
     if (status /= nf90_noerr) call handleErr(status,'nf90_put var for date')

     datesec=hour(1)*3600
     write(0,*) ' date=',date,' datesec=',datesec,' month=',month(1),'day=',day(1),&
                'hour=',hour(1)
     start(1)=itime
     count(1)=1
     stride=1
     status=nf90_put_var(vids%ncid(ifield),vids%viddatesec(ifield),datesec,start,count,stride)
     if (status /= nf90_noerr) call handleErr(status,'nf90_put var for datesec')

     start(1)=1
     start(2)=1
     start(3)=1
     start(4)=itime
     count(1)=NLON_FAA
     count(2)=NLAT_FAA
     count(3)=nlev_CLIMO
     count(4)=1
     stride=1

     write(0,*) 'time step=',itime,' ',trim(fields(ifield)),'  being written'
     status=nf90_put_var(vids%ncid(ifield),vids%vidmix(ifield),mixing_ratio(:,:,:,ifield),start,count,stride)
     if (status /= nf90_noerr) call handleErr(status,'nf90_put var for'//trim(fields(ifield)))


  end do ! ifield

end subroutine writeDataNCDF

subroutine readData(inputfile,startTime,nTimesRead,month,day,hour,ilat,ilon,ialt,AllData)

!----------------------------------------------------------
! Read in the data for the entire filelist
!----------------------------------------------------------
  character(len=500),dimension(:),pointer  :: inputfile
  integer,intent(in)                       :: startTime
  integer,intent(in)                       :: nTimesRead

  integer,dimension(500000),intent(out)    :: month,day,ilat,ilon,ialt,hour
  real(r8),dimension(:,:,:,:),intent(out)  :: AllData

  integer                                  :: i,j,k,l,ierr,itime
  character(len=500)                       :: tempChar
  real(r8),dimension(NLON_FAA,NLAT_FAA,NALT_FAA,NFIELDS_FAA) :: Sum
  

  if (nTimesRead > 1) then

    do l=1,NFIELDS_FAA
!     do k=1,NALT_FAA
             Sum(:,:,:,l)=0.
!     end do
    end do

  end if

!----------------------------------------------------------
! Read in the data, looping over the number of times requested
! All data required for one write to the files is read
!----------------------------------------------------------

!  Read data in reverse order so that the first month/day/hour is kept for writing to the file
!  Since summing, order for reading is not important for the data values
  do itime=startTime+nTimesRead-1,startTime,-1

     do l=1,NFIELDS_FAA
!      do k=1,NALT_FAA
              AllData(:,:,:,l)=0.
!      end do
     end do

    open(file=inputfile(itime),unit=20,iostat=ierr)
    if (ierr /= 0) then
     write(0,*) ' problem opening ',trim(inputfile(itime)),' ... Stopping'
     stop
    endif

    read (20,*) tempChar  ! Read the comment line and discard
    i=0

    do
      i=i+1
      read (20,*,end=20) month(i),day(i),hour(i),ilat(i),ilon(i),ialt(i),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),1),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),2),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),3),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),4),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),5),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),6),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),7),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),8),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),9),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),10),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),11),&
         AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),12)
!      write(0,*) ' inside read, i=',i,month(i),day(i),hour(i),ilat(i),ilon(i),ialt(i),&
!                 AllData(ilon(i)+1,ilat(i)+1,NALT_FAA-ialt(i),1:NFIELDS_FAA)

! 999 format(i,i,i,3i4,13G25.21)

      if (ialt(i) > NALT_FAA-1) then
        write(0,*) ' Found altitude > NALT_FAA-1  ialt(i)=',ialt(i),' NALT_FAA-1=',NALT_FAA-1
        stop
      end if

    enddo

20  continue

!  Close the FAA file
    close(20)

    write(0,*) ' after read, first record=' , month(1),day(1),hour(1),ilat(1),ilon(1),ialt(1),&
                AllData(ilon(1)+1,ilat(1)+1,NALT_FAA-ialt(1),1:NFIELDS_FAA)

!----------------------------------------------------------
!  Sum all of the data if reading more than one time
!----------------------------------------------------------
    if (ntimesRead > 1 ) sum=sum+alldata
    write(0,*) ' alldata(288,99,120,3)=', alldata(288,99,120,3)
    write(0,*) ' sum(288,99,120,3)=    ', sum(288,99,120,3)

  end do

!----------------------------------------------------------
! Assign the summed data to the output array
!----------------------------------------------------------

  if (ntimesRead > 1)  alldata = sum

!++ Chen,unit conversion
    alldata(:,:,:,1) = alldata(:,:,:,1)*1852.0  ! nautical mile to meter
    alldata(:,:,:,2) = alldata(:,:,:,2)*1852.0  ! nattical mile to meter
    alldata(:,:,:,3) = alldata(:,:,:,3)*1000.0  ! fule burn from kg to g
!-- Chen

end subroutine readData

end module FAARoutines
