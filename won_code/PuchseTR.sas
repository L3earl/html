DATA product;
INFILE 'C:\ColFilter\Product.txt' firstobs=2;
INPUT id $11-17;
RUN;

DATA temp;
SET PRODUCT;
retain u00001-u19383 0;
RUN;

proc transpose data=temp out=UserBased;
id id;
run;

DATA purchase_all;
INFILE 'C:\ColFilter\Purchase.txt' firstobs=2;
INPUT u_id 28-32 p_id $20-26;
RUN;
/*구매이력 중복 제거*/
proc sql;
create table purchase as select distinct * from purchase_all;
quit;

proc iml;
use work.Purchase;
read all var _NUM_ into _Purchase1;
read all var _CHAR_ into _Purchase2;

use work.Userbased;
read all into _usr[colName=varNames];

do rowNum = 1 to nrow(_purchase1);
	_usr[_Purchase1[rowNum],_Purchase2[rowNum]]=1;
end;

create _PuchseTR from _usr[colname=varNames];
append from _usr;
close _PuchseTR;
run;

/* 컬럼명 변경  ->  COL1 - COL4386 */
proc iml;
	use work._PuchseTR;
	read all var _NUM_ into _PuchseTR;

	create _PuchseTR_COL from _PuchseTR;
	append from _PuchseTR;
	close _PuchseTR_COL;
run;

/* 로우 추가  ->  userID 1 - 19383 */
data PuchseTR;
	do userID = 1 to 19383;
		set _PuchseTR_COL;
		output;
	end;
run;

/* 구매행렬 내보내기 */
proc export data = PuchseTR
			outfile = "C:\ColFilter\PuchseTR.sav"
			dbms = spss label replace;
run;
