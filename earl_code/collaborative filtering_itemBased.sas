
%MACRO PuchseClstrPrferMACRO(FOLDERNAME, CLUST, TRAINING, tempNum); 

	/* cluster 구매행렬 불러오기 */
	PROC IMPORT OUT = _&CLUST
				DATAFILE = "&FOLDERNAME\&CLUST"
				DBMS = SAV REPLACE;
	RUN;

	/* 행렬 전치 , test is needed */
	proc transpose 
		data= _&CLUST
		out= &CLUST (DROP =_NAME_  _LABEL_ );
	run;

	/* dist t행렬 생성 */
	PROC DISTANCE REPLACE 
				  SHAPE = SQUARE 
				  DATA   = &CLUST
				  OUT    = _JACCARD
				  METHOD = JACCARD;
		 VAR ANOMINAL(_ALL_);
	RUN;

	/* dist 매트릭스의 Missing Value 값 제거 */
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

	/* jaccard행렬 내보내기 */
	PROC EXPORT DATA = JACCARD
				OUTFILE = "&FOLDERNAME\JACCARD&tempNum"
				DBMS = SPSS LABEL REPLACE;
	RUN;

	/* training 구매행렬 불러오기 */
	PROC IMPORT OUT = _&TRAINING
				DATAFILE = "&FOLDERNAME\&TRAINING"
				DBMS = SAV REPLACE;
	RUN;

	/* 행렬 전치 , test is needed */
	proc transpose 
		data= _&TRAINING
		out= &TRAINING (DROP =_NAME_  _LABEL_ );
	run;

	/* 선호도 매트릭스 = 유사도 매트릭스*구매 매트릭스 */
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

	/* 선호도행렬 내보내기 */
	PROC EXPORT DATA = UserDist
				OUTFILE = "&FOLDERNAME\UserDist&tempNum"
				DBMS = SPSS LABEL REPLACE;
	RUN;
%MEND;

/* 반복적인 파일 생성 */
%MACRO PrferMacroRepeat(scenario, start, end); 
	%DO i = &start %TO &end;
			%PuchseClstrPrferMACRO(&scenario, purchaseSparse1_&i, purchaseSparse2_&i, &i);
	%END;
%MEND;

/* 최종 실행 문장, (시나리오 폴더 주소, 군집 시작 번호, 군집 끝 번호)를 넣고 실행하면 됨 */
%PrferMacroRepeat(F:\temp\Lpoint\scenario\s_013, 1, 3);

