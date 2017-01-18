
/* 참고, sav 파일 불러오는 코드 예제 */
/*
PROC IMPORT OUT = Userbasedsimilarity
			DATAFILE = "F:\googledrive\temp\Userbasedsimilarity.sav"
			DBMS = sav REPLACE;
RUN;
*/

/* --------------------------------------------------- */
/*-------------- 구매 매트릭스 만드는 과정 --------------- */
/* --------------------------------------------------- */

/* 아이템 id를 읽어서 productID로 저장 */
DATA productID;
	INFILE 'F:\googledrive\L.point 빅데이터\제3회 Big Data Competition-개인화상품추천\제3회 Big Data Competition-분석용데이터-03.상품분류.txt' firstobs=2;
	INPUT id $11-17;
RUN;

/* col 이름이 id, u00001-19383 인 4386*(19383+1) 크기의 tempPID를 만듬. id column에는 productID의 데이터가 들어가며, 나머지는 0이 들어감*/
DATA tempPID;
	SET productID;
	retain u00001-u19383 0;
RUN;

/* tempPID를 전치하여 trandsposedTempPID를 만듬 */
proc transpose 
	data=tempPID 
	out=trandsposedTempPID;
	id id;
run;

/* 구매 데이터에서 사용자 ID와 구매한 아이템의 소분류코드를 읽어서 purchasedList로 저장 */
DATA tempPurchasedList;
	INFILE 'F:\googledrive\L.point 빅데이터\제3회 Big Data Competition-개인화상품추천\제3회 Big Data Competition-분석용데이터-02.구매상품TR.txt' firstobs=2;
	INPUT u_id 28-32 p_id $20-26;
RUN;

/* purchasedList를 매트릭스로 만듬 */
proc sql;
	create table purchasedList as select distinct * from tempPurchasedList;
quit;

/* purchasedList에서 중복된 구매 이력을 제거하고, trandsposedTempPID에 값을 넣어 구매 매트릭스를 만듬 */
/* 좀 더 이해가 필요함 */
proc iml;
	use work.purchasedList;
	read all var _NUM_ into _Purchase1;
	read all var _CHAR_ into _Purchase2;

	use work.trandsposedTempPID;
	read all into _usr[colName=varNames];

	do rowNum = 1 to nrow(_purchase1);
		_usr[_Purchase1[rowNum],_Purchase2[rowNum]]=1;
	end;

	create purchaseMatrix from _usr[colname=varNames];
	append from _usr;
	close purchaseMatrix;
run;



/* --------------------------------------------------- */
/*-------- 사용자 기반 선호도 매트릭스  만드는 과정 -------- */
/* --------------------------------------------------- */

/* Jaccard 기반 사용자간 유사도 계산 */
PROC DISTANCE DATA   = PurchaseMatrix
			  OUT    = _UserBasedSimilarity
			  METHOD = JACCARD;
	 VAR ANOMINAL(item1 - item4386);
RUN;


/* 대칭행렬 복사로 사용자 유사도 매트릭스의 Null값 제거 */
PROC IML;
	USE  WORK._UserBasedSimilarity;
	READ ALL INTO _UsrSimity;

	DO rowNum = 1 TO nrow(_UsrSimity);
		DO colNum = 1 TO ncol(_UsrSimity);
			IF   _UsrSimity[rowNum,colNum] = . 
			THEN _UsrSimity[rowNum,colNum] = _UsrSimity[colNum,rowNum];		
		END;
	END;

	CREATE UserBasedSimilarity FROM _UsrSimity;
	APPEND FROM _UsrSimity;
	CLOSE  UserBasedSimilarity;
RUN;

/* 선호도 매트릭스 = 사용자 유사도 매트릭스*구매 매트릭스 */
PROC IML;
	USE  WORK.Userbasedsimilarity;
	READ ALL into _Userbasedsimilarity ;

	USE  WORK.PurchaseMatrix;
	READ ALL into _UserBased;

	_Result = _Userbasedsimilarity *_UserBased;

	CREATE UserBasedDist FROM _Result;
	APPEND FROM _Result;
	CLOSE  UserBasedDist;
RUN;

/* 사용자 기반 선호도 매트릭스를 sav 파일로 내보냄 */
PROC EXPORT DATA= WORK.Userbaseddist 
            OUTFILE= "F:\googledrive\temp\Userbaseddist.sav" 
            DBMS=SPSS LABEL REPLACE;
RUN;


/* --------------------------------------------------- */
/*-------- 아이템 기반 선호도 매트릭스  만드는 과정 -------- */
/* --------------------------------------------------- */

/* 구매 매트릭스를 전치함 */
PROC TRANSPOSE DATA = PurchaseMatrix 
			   OUT  = TransposedPurchaseMatrix(drop = _NAME_);
RUN;

/* jaccard기반 상품간 유사도 계산 */
PROC DISTANCE DATA   = TransposedPurchaseMatrix
			  OUT    = _ItemBasedSimilarity
			  METHOD = JACCARD;
	 VAR ANOMINAL(col1 - col19383);
RUN;

/* 대칭행렬 복사로 상품간 유사도 매트릭스의 Null값 제거 */
PROC IML;
	USE  WORK._ItemBasedSimilarity;
	READ ALL INTO _ItemSimity;

	DO rowNum = 1 TO nrow(_ItemSimity);
		DO colNum = 1 TO ncol(_ItemSimity);
			IF   _ItemSimity[rowNum,colNum] = . 
			THEN _ItemSimity[rowNum,colNum] = _ItemSimity[colNum,rowNum];		
		END;
	END;

	CREATE ItemBasedSimilarity FROM _ItemSimity;
	APPEND FROM _ItemSimity;
	CLOSE  ItemBasedSimilarity;
RUN;


/* 선호도 매트릭스 = 상품간 유사도 매트릭스*구매 매트릭스 */
PROC IML;
	USE  WORK.ItemBasedSimilarity;
	READ ALL into _Itembasedsimilarity ;

	USE  WORK.TransposedPurchaseMatrix;
	READ ALL into _ItemBased;

	_Result = _Itembasedsimilarity *_ItemBased;

	CREATE ItemBasedDist FROM _Result;
	APPEND FROM _Result;
	CLOSE  ItemBasedDist;
RUN;

/* 아이템 기반 선호도 매트릭스를 sav 파일로 내보냄 */
PROC EXPORT DATA= WORK.ItemBasedDist 
            OUTFILE= "F:\googledrive\temp\ItemBasedDist.sav" 
            DBMS=SPSS LABEL REPLACE;
RUN;
