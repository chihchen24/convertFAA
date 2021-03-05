#!/usr/bin/perl


# This script creates the daily files required by the CreateFAAData.F90
# It reads in the file provided in the $InputMask and substitutes in
#    the day and month information

# Use one of these lines ------------------

  $month=JAN; $monthDOY=1;   $numMonth=31; $monthInt=1;
# $month=FEB; $monthDOY=32;  $numMonth=28; $monthInt=2;
# $month=MAR; $monthDOY=60;  $numMonth=31; $monthInt=3;
# $month=APR; $monthDOY=91;  $numMonth=30; $monthInt=4;
# $month=MAY; $monthDOY=121; $numMonth=31; $monthInt=5;
# $month=JUN; $monthDOY=152; $numMonth=30; $monthInt=6;
# $month=JUL; $monthDOY=182; $numMonth=31; $monthInt=7;
# $month=AUG; $monthDOY=213; $numMonth=31; $monthInt=8;
# $month=SEP; $monthDOY=244; $numMonth=30; $monthInt=9;
# $month=OCT; $monthDOY=274; $numMonth=31; $monthInt=10;
# $month=NOV; $monthDOY=305; $numMonth=30; $monthInt=11;
# $month=DEC; $monthDOY=335; $numMonth=31; $monthInt=12;

#------------------------------------------

 $InputMask='InputFAAFileList-MASK.txt';

 $lastday=$monthDOY+$numMonth;
 $iday=1;

  print "iday=",$iday,"\n" ;

while ($iday <= $numMonth) {

  $DOY=$monthDOY+$iday-1;
  $filename='InputFAAFileList-'.$DOY.'.txt';
  print $filename,"\n";

  system("sed -e s/MONTHSTR/$month/ -e s/MONTHINT/$monthInt/ -e s/MONTHDAY/$iday/ $InputMask > $filename");
  $iday++;
  print $iday,"\n" ;
}
