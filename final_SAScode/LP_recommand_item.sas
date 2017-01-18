
/* ����, sav ���� �ҷ����� �ڵ� ���� */
/*
PROC IMPORT OUT = Userbasedsimilarity
			DATAFILE = "F:\googledrive\temp\Userbasedsimilarity.sav"
			DBMS = sav REPLACE;
RUN;
*/

/* --------------------------------------------------- */
/*-------------- ���� ��Ʈ���� ����� ���� --------------- */
/* --------------------------------------------------- */

/* ������ id�� �о productID�� ���� */
DATA productID;
	INFILE 'F:\googledrive\L.point ������\��3ȸ Big Data Competition-����ȭ��ǰ��õ\��3ȸ Big Data Competition-�м��뵥����-03.��ǰ�з�.txt' firstobs=2;
	INPUT id $11-17;
RUN;

/* col �̸��� id, u00001-19383 �� 4386*(19383+1) ũ���� tempPID�� ����. id column���� productID�� �����Ͱ� ����, �������� 0�� ��*/
DATA tempPID;
	SET productID;
	retain u00001-u19383 0;
RUN;

/* tempPID�� ��ġ�Ͽ� trandsposedTempPID�� ���� */
proc transpose 
	data=tempPID 
	out=trandsposedTempPID;
	id id;
run;

/* ���� �����Ϳ��� ����� ID�� ������ �������� �Һз��ڵ带 �о purchasedList�� ���� */
DATA tempPurchasedList;
	INFILE 'F:\googledrive\L.point ������\��3ȸ Big Data Competition-����ȭ��ǰ��õ\��3ȸ Big Data Competition-�м��뵥����-02.���Ż�ǰTR.txt' firstobs=2;
	INPUT u_id 28-32 p_id $20-26;
RUN;

/* purchasedList�� ��Ʈ������ ���� */
proc sql;
	create table purchasedList as select distinct * from tempPurchasedList;
quit;

/* purchasedList���� �ߺ��� ���� �̷��� �����ϰ�, trandsposedTempPID�� ���� �־� ���� ��Ʈ������ ���� */
/* �� �� ���ذ� �ʿ��� */
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
/*-------- ����� ��� ��ȣ�� ��Ʈ����  ����� ���� -------- */
/* --------------------------------------------------- */

/* Jaccard ��� ����ڰ� ���絵 ��� */
PROC DISTANCE DATA   = PurchaseMatrix
			  OUT    = _UserBasedSimilarity
			  METHOD = JACCARD;
	 VAR ANOMINAL(item1 - item4386);
RUN;


/* ��Ī��� ����� ����� ���絵 ��Ʈ������ Null�� ���� */
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

/* ��ȣ�� ��Ʈ���� = ����� ���絵 ��Ʈ����*���� ��Ʈ���� */
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

/* ����� ��� ��ȣ�� ��Ʈ������ sav ���Ϸ� ������ */
PROC EXPORT DATA= WORK.Userbaseddist 
            OUTFILE= "F:\googledrive\temp\Userbaseddist.sav" 
            DBMS=SPSS LABEL REPLACE;
RUN;


/* --------------------------------------------------- */
/*-------- ������ ��� ��ȣ�� ��Ʈ����  ����� ���� -------- */
/* --------------------------------------------------- */

/* ���� ��Ʈ������ ��ġ�� */
PROC TRANSPOSE DATA = PurchaseMatrix 
			   OUT  = TransposedPurchaseMatrix(drop = _NAME_);
RUN;

/* jaccard��� ��ǰ�� ���絵 ��� */
PROC DISTANCE DATA   = TransposedPurchaseMatrix
			  OUT    = _ItemBasedSimilarity
			  METHOD = JACCARD;
	 VAR ANOMINAL(col1 - col19383);
RUN;

/* ��Ī��� ����� ��ǰ�� ���絵 ��Ʈ������ Null�� ���� */
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


/* ��ȣ�� ��Ʈ���� = ��ǰ�� ���絵 ��Ʈ����*���� ��Ʈ���� */
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

/* ������ ��� ��ȣ�� ��Ʈ������ sav ���Ϸ� ������ */
PROC EXPORT DATA= WORK.ItemBasedDist 
            OUTFILE= "F:\googledrive\temp\ItemBasedDist.sav" 
            DBMS=SPSS LABEL REPLACE;
RUN;
