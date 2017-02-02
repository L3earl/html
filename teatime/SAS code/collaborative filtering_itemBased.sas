
%MACRO PuchseClstrPrferMACRO(FOLDERNAME, CLUST, TRAINING, tempNum); 

	/*  군집의 구매 희소 행렬 불러오기 */
	PROC IMPORT OUT = _&CLUST
				DATAFILE = "&FOLDERNAME\&CLUST"
				DBMS = SAV REPLACE;
	RUN;

	/* 행렬 전치 */
	proc transpose 
		data= _&CLUST
		out= &CLUST (DROP =_NAME_  _LABEL_ );
	run;

	/* 자카드 계산 */
	PROC DISTANCE REPLACE 
				  SHAPE = SQUARE 
				  DATA   = &CLUST
				  OUT    = _JACCARD
				  METHOD = JACCARD;
		 VAR ANOMINAL(_ALL_);
	RUN;

	/* 자카드 매트릭스의 Missing Value 값을 0으로 변경  */
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

	/* jaccard 행렬 내보내기 */
	PROC EXPORT DATA = JACCARD
				OUTFILE = "&FOLDERNAME\JACCARD&tempNum"
				DBMS = SPSS LABEL REPLACE;
	RUN;

	/* training 데이터 (구매 희소 행렬) 불러오기 */
	PROC IMPORT OUT = _&TRAINING
				DATAFILE = "&FOLDERNAME\&TRAINING"
				DBMS = SAV REPLACE;
	RUN;

	/* 행렬 전치 */
	proc transpose 
		data= _&TRAINING
		out= &TRAINING (DROP =_NAME_  _LABEL_ );
	run;

	/* 선호도 매트릭스 = 자카드 유사도 매트릭스*구매 희소 행렬 */
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

/* 전체 과정 매크로 */
%MACRO PrferMacroRepeat(scenario, start, end); 
	%DO i = &start %TO &end;
			%PuchseClstrPrferMACRO(&scenario, purchaseSparse1_&i, purchaseSparse2_&i, &i);
	%END;
%MEND;

/* 최종 실행 문장, (시나리오 폴더 주소, 군집 시작 번호, 군집 끝 번호)를 넣고 실행하면 됨 */
%PrferMacroRepeat(F:\temp\Lpoint\scenario\s_067, 1, 3);

