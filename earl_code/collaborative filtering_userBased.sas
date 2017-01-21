
%MACRO PuchseClstrPrferMACRO(FOLDERNAME, CLUST, TRAINING, tempNum); 

	/* cluster ������� �ҷ����� */
	PROC IMPORT OUT = &CLUST
				DATAFILE = "F:\googledrive\L.point ������\scenario\&FOLDERNAME\data\&CLUST"
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
				OUTFILE = "F:\googledrive\L.point ������\scenario\&FOLDERNAME\data\JACCARD&tempNum"
				DBMS = SPSS LABEL REPLACE;
	RUN;

	/* training ������� �ҷ����� */
	PROC IMPORT OUT = &TRAINING
				DATAFILE = "F:\googledrive\L.point ������\scenario\&FOLDERNAME\data\&TRAINING"
				DBMS = SAV REPLACE;
	RUN;

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
				OUTFILE = "F:\googledrive\L.point ������\scenario\&FOLDERNAME\data\UserDist&tempNum"
				DBMS = SPSS LABEL REPLACE;
	RUN;
%MEND;

/* �ݺ����� ���� ���� */
%MACRO PrferMacroRepeat(scenario, clusterNum); 
	%DO i = 1 %TO &clusterNum;
			%PuchseClstrPrferMACRO(&scenario, purchaseSparse1_&i, purchaseSparse2_&i, &i);
	%END;
%MEND;

/* ���� ���� ����, (�ó����� ������, ���� ����)�� �ְ� �����ϸ� �� */
%PrferMacroRepeat(s_003, 4);

