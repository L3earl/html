/* 구매행렬 불러오기 */
PROC IMPORT OUT = PuchseTR
			DATAFILE = "C:\ColFilter\PuchseTR.sav"
			DBMS = SAV REPLACE;
RUN;


%MACRO PuchseClstrPrferMACRO(FOLDERNAME, FILENAME);
	/* 군집 파일 불러오기 */
	PROC IMPORT OUT = &FILENAME
				DATAFILE = "C:\ColFilter\&FOLDERNAME\&FILENAME"
				DBMS = SAV REPLACE;
	DATA &FILENAME;
		SET &FILENAME;
		swtch = 1;
	RUN;

	/* 구매상품TR(군집) 생성 */
	DATA Puchse&FILENAME(DROP = swtch userID);
		MERGE PuchseTR &FILENAME;
		BY userID;
		IF swtch = . THEN DELETE;
	RUN;

	/* 유사도행렬 생성 (군집 - 구매상품TR)*/
	PROC DISTANCE DATA   = Puchse&FILENAME
				  OUT    = PuchseSimty&FILENAME
				  METHOD = JACCARD;
		 VAR ANOMINAL(COL1 - COL4386);
	RUN;

	/* 선호도행렬 생성 (군집 - 구매상품TR)*/
	PROC IML;
		USE  WORK.PuchseSimty&FILENAME;
		READ ALL INTO PuchseSimty&FILENAME;

		USE  WORK.Puchse&FILENAME;
		READ ALL INTO Puchse&FILENAME;

		/* 계산을 위한 대칭행렬(유사도행렬) 수정 */
		DO rowNum = 1 TO nrow(PuchseSimty&FILENAME);
			DO colNum = 1 TO ncol(PuchseSimty&FILENAME);
				IF PuchseSimty&FILENAME[rowNum,colNum] = . 
				THEN PuchseSimty&FILENAME[rowNum,colNum] = PuchseSimty&FILENAME[colNum,rowNum];		
			END;
		END;

		/* 선호도행렬 = 유사도행렬 x 구매상품TR */
		_Result = PuchseSimty&FILENAME * Puchse&FILENAME;
		CREATE Result FROM _Result;
		APPEND FROM _Result;
		CLOSE Result;
	DATA Prfer&FILENAME(DROP = swtch);
		MERGE &FILENAME Result;
	RUN;

	/* 선호도행렬 내보내기 */
	PROC EXPORT DATA = Prfer&FILENAME
				OUTFILE = "C:\ColFilter\&FOLDERNAME\Prfer&FILENAME"
				DBMS = SPSS LABEL REPLACE;
	RUN;
%MEND;

/* 반복적인 파일 생성 */
%MACRO PrferMacroRepeat(frontCnt, backCnt); 
	%DO i = 1 %TO &frontCnt;
		%DO j = 1 %TO &backCnt;
			%PuchseClstrPrferMACRO(Scenario1, Clstr&i._&j);
		%END;
	%END;
%MEND;

/* 최종 실행 문장 */
%PrferMacroRepeat(3, 3);
