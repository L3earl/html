
%MACRO PuchseClstrPrferMACRO(FOLDERNAME, CLUST, TRAINING, tempNum); 

	/* cluster ������� �ҷ����� */
	PROC IMPORT OUT = &CLUST
				DATAFILE = "&FOLDERNAME\&CLUST"
				DBMS = SAV REPLACE;
	RUN;

	/* ���絵��� ���� */
	PROC DISTANCE DATA   = &CLUST
				  OUT    = _JACCARD
				  METHOD = JACCARD;
		 VAR ANOMINAL(COL1 - COL4386);
	RUN;

	/* ��Ī��� ����� ���絵 ��Ʈ������ Null�� ���� */
	PROC IML;
		USE  WORK._JACCARD;
		READ ALL INTO _UsrSimity;

		DO rowNum = 1 TO nrow(_UsrSimity);
			DO colNum = 1 TO ncol(_UsrSimity);
				IF   _UsrSimity[rowNum,colNum] = . 
				THEN _UsrSimity[rowNum,colNum] = _UsrSimity[colNum,rowNum];		
			END;
		END;

		CREATE JACCARD FROM _UsrSimity;
		APPEND FROM _UsrSimity;
		CLOSE  JACCARD;
	RUN;

	/* jaccard��� �������� */
	PROC EXPORT DATA = JACCARD
				OUTFILE = "&FOLDERNAME\JACCARD&tempNum"
				DBMS = SPSS LABEL REPLACE;
	RUN;

	/* training ������� �ҷ����� */
	PROC IMPORT OUT = &TRAINING
				DATAFILE = "&FOLDERNAME\&TRAINING"
				DBMS = SAV REPLACE;
	RUN;

	/* ��ȣ�� ��Ʈ���� = ���絵 ��Ʈ����*���� ��Ʈ���� */
	PROC IML;
		USE  WORK.JACCARD;
		READ ALL into _Userbasedsimilarity ;

		USE  WORK.&TRAINING;
		READ ALL into _UserBased;

		_Result = _Userbasedsimilarity *_UserBased;

		CREATE _UserDist FROM _Result;
		APPEND FROM _Result;
		CLOSE  _UserDist;
	RUN;

	/* ��� ��ġ */
	proc transpose 
		data=_UserDist
		out=userDist (DROP =_NAME_  _LABEL_ );
	run;

	/* ��ȣ����� �������� */
	PROC EXPORT DATA = UserDist
				OUTFILE = "&FOLDERNAME\UserDist&tempNum"
				DBMS = SPSS LABEL REPLACE;
	RUN;
%MEND;

/* �ݺ����� ���� ���� */
%MACRO PrferMacroRepeat(scenario, start, end); 
	%DO i = &start %TO &end;
			%PuchseClstrPrferMACRO(&scenario, purchaseSparse1_&i, purchaseSparse2_&i, &i);
	%END;
%MEND;

/* ���� ���� ����, (�ó����� ���� �ּ�, ���� ���� ��ȣ, ���� �� ��ȣ)�� �ְ� �����ϸ� �� */
%PrferMacroRepeat(F:\googledrive\temp\Lpoint\scenario\s_005, 1, 3);

