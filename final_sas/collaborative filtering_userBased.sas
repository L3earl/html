
%MACRO PuchseClstrPrferMACRO(FOLDERNAME, CLUST, TRAINING, tempNum); 

	/* cluster 구매행렬 불러오기 */
	PROC IMPORT OUT = &CLUST
				DATAFILE = "&FOLDERNAME\&CLUST"
				DBMS = SAV REPLACE;
	RUN;

	/* 유사도행렬 생성 */
	PROC DISTANCE DATA   = &CLUST
				  OUT    = _JACCARD
				  METHOD = JACCARD;
		 VAR ANOMINAL(COL1 - COL4386);
	RUN;

	/* 대칭행렬 복사로 유사도 매트릭스의 Null값 제거 */
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

	/* jaccard행렬 내보내기 */
	PROC EXPORT DATA = JACCARD
				OUTFILE = "&FOLDERNAME\JACCARD&tempNum"
				DBMS = SPSS LABEL REPLACE;
	RUN;

	/* training 구매행렬 불러오기 */
	PROC IMPORT OUT = &TRAINING
				DATAFILE = "&FOLDERNAME\&TRAINING"
				DBMS = SAV REPLACE;
	RUN;

	/* 선호도 매트릭스 = 유사도 매트릭스*구매 매트릭스 */
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

	/* 행렬 전치 */
	proc transpose 
		data=_UserDist
		out=userDist (DROP =_NAME_  _LABEL_ );
	run;

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
%PrferMacroRepeat(F:\googledrive\temp\Lpoint\scenario\s_005, 1, 3);

