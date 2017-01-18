/* ������� �ҷ����� */
PROC IMPORT OUT = PuchseTR
			DATAFILE = "C:\ColFilter\PuchseTR.sav"
			DBMS = SAV REPLACE;
RUN;


%MACRO PuchseClstrPrferMACRO(FOLDERNAME, FILENAME);
	/* ���� ���� �ҷ����� */
	PROC IMPORT OUT = &FILENAME
				DATAFILE = "C:\ColFilter\&FOLDERNAME\&FILENAME"
				DBMS = SAV REPLACE;
	DATA &FILENAME;
		SET &FILENAME;
		swtch = 1;
	RUN;

	/* ���Ż�ǰTR(����) ���� */
	DATA Puchse&FILENAME(DROP = swtch userID);
		MERGE PuchseTR &FILENAME;
		BY userID;
		IF swtch = . THEN DELETE;
	RUN;

	/* ���絵��� ���� (���� - ���Ż�ǰTR)*/
	PROC DISTANCE DATA   = Puchse&FILENAME
				  OUT    = PuchseSimty&FILENAME
				  METHOD = JACCARD;
		 VAR ANOMINAL(COL1 - COL4386);
	RUN;

	/* ��ȣ����� ���� (���� - ���Ż�ǰTR)*/
	PROC IML;
		USE  WORK.PuchseSimty&FILENAME;
		READ ALL INTO PuchseSimty&FILENAME;

		USE  WORK.Puchse&FILENAME;
		READ ALL INTO Puchse&FILENAME;

		/* ����� ���� ��Ī���(���絵���) ���� */
		DO rowNum = 1 TO nrow(PuchseSimty&FILENAME);
			DO colNum = 1 TO ncol(PuchseSimty&FILENAME);
				IF PuchseSimty&FILENAME[rowNum,colNum] = . 
				THEN PuchseSimty&FILENAME[rowNum,colNum] = PuchseSimty&FILENAME[colNum,rowNum];		
			END;
		END;

		/* ��ȣ����� = ���絵��� x ���Ż�ǰTR */
		_Result = PuchseSimty&FILENAME * Puchse&FILENAME;
		CREATE Result FROM _Result;
		APPEND FROM _Result;
		CLOSE Result;
	DATA Prfer&FILENAME(DROP = swtch);
		MERGE &FILENAME Result;
	RUN;

	/* ��ȣ����� �������� */
	PROC EXPORT DATA = Prfer&FILENAME
				OUTFILE = "C:\ColFilter\&FOLDERNAME\Prfer&FILENAME"
				DBMS = SPSS LABEL REPLACE;
	RUN;
%MEND;

/* �ݺ����� ���� ���� */
%MACRO PrferMacroRepeat(frontCnt, backCnt); 
	%DO i = 1 %TO &frontCnt;
		%DO j = 1 %TO &backCnt;
			%PuchseClstrPrferMACRO(Scenario1, Clstr&i._&j);
		%END;
	%END;
%MEND;

/* ���� ���� ���� */
%PrferMacroRepeat(3, 3);
