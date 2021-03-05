#! /bin/csh

set dir = '/fis/cgd/home/cacraig/ACCRI_2006_MixRat-DayAvg/'


set file1 = 'filelist_SLANT_DIST.dat'
set file2 = 'filelist_TRACK_DIST.dat'

set var1 = 'SLANT_DIST';
set var2 = 'TRACK_DIST';

set month = 1
set ndays = 1

while($month <= 12)

if($month == 1) then
 set max_days = 31
 set month_name = 'JAN'
else if($month == 2) then
 set max_days = 28
 set month_name = 'FEB'
else if($month == 3) then
 set max_days = 31
 set month_name = 'MAR'
else if($month == 4) then
 set max_days = 30
 set month_name = 'APR'
else if($month == 5) then
 set max_days = 31
 set month_name = 'MAY'
else if($month == 6) then
 set max_days = 30
 set month_name = 'JUN'
else if($month == 7) then
 set max_days = 31
 set month_name = 'JUL'
else if($month == 8) then
 set max_days = 31
 set month_name = 'AUG'
else if($month == 9) then
 set max_days = 30
 set month_name = 'SEP'
else if($month == 10) then
 set max_days = 31
 set month_name = 'OCT'
else if($month == 11) then
 set max_days = 30
 set month_name = 'NOV'
else if($month == 12) then
 set max_days = 31
 set month_name = 'DEC'
endif

set day = 1

while ($day <= $max_days)


if( $ndays == 1 ) then
echo {$dir}{$month_name}'/'{$var1}'_'{$month}'_'{$day}'_2006_c20120114_grid.nc' > $file1
echo {$dir}{$month_name}'/'{$var2}'_'{$month}'_'{$day}'_2006_c20120114_grid.nc' > $file2
else
echo {$dir}{$month_name}'/'{$var1}'_'{$month}'_'{$day}'_2006_c20120114_grid.nc' >> $file1
echo {$dir}{$month_name}'/'{$var2}'_'{$month}'_'{$day}'_2006_c20120114_grid.nc' >> $file2
endif

set hour = 0


@ day = $day + 1
@ ndays = $ndays + 1

echo $month_name $day $ndays
end


@ month = $month + 1

end
