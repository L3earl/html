
%MACRO PuchseClstrPrferMACRO(FOLDERNAME, CLUST, TRAINING, tempNum); 

	/* cluster ������� �ҷ����� */
	PROC IMPORT OUT = _&CLUST
				DATAFILE = "&FOLDERNAME\&CLUST"
				DBMS = SAV REPLACE;
	RUN;

	/* ��� ��ġ , test is needed */
	proc transpose 
		data= _&CLUST
		out= &CLUST (DROP =_NAME_  _LABEL_ );
	run;

	/* dist t��� ���� */
	PROC DISTANCE REPLACE 
				  SHAPE = SQUARE 
				  DATA   = &CLUST
				  OUT    = _JACCARD
				  METHOD = JACCARD;
		 VAR ANOMINAL(_ALL_);
	RUN;

	/* dist ��Ʈ������ Missing Value �� ���� */
	PROC IML;
		USE  WORK._JACCARD;
		READ ALL INTO _UsrSimity;

		DO rowNum = 1 TO nrow(_UsrSimity);
			DO colNum = 1 TO ncol(_UsrSimity);
				IF   _UsrSimity[rowNum,colNum] = . 
				THEN _UsrSimity[rowNum,colNum] = 0;		
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
	PROC IMPORT OUT = _&TRAINING
				DATAFILE = "&FOLDERNAME\&TRAINING"
				DBMS = SAV REPLACE;
	RUN;

	/* ��� ��ġ , test is needed */
	proc transpose 
		data= _&TRAINING
		out= &TRAINING (DROP =_NAME_  _LABEL_ );
	run;

	/* ��ȣ�� ��Ʈ���� = ���絵 ��Ʈ����*���� ��Ʈ���� */
	PROC IML;
		USE  WORK.JACCARD;
		READ ALL into _Userbasedsimilarity ;

		USE  WORK.&TRAINING;
		READ ALL into _UserBased;

		_Result = _Userbasedsimilarity *_UserBased;

		CREATE UserDist FROM _Result;
		APPEND FROM _Result;
		CLOSE  UserDist;
	RUN;

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
%PrferMacroRepeat(F:\temp\Lpoint\scenario\s_013, 1, 3);

