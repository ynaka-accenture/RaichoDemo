//RJD030   JOB (RACS),'RYOKIN KEISAN',CLASS=A,MSGCLASS=X
//*--------------------------------------------------------------
//* 日次D030: 電気料金計算 (RBRYO01C)
//*  ・COND=(4,LT): 前段が RC>4 なら本ステップをスキップ (D-03)
//*    ※新様式 IF文と混在しているので読み違い注意
//*  ・SAIKEI は日次は DD DUMMY (0件=再計算なし を表現: D-02)
//*    月次JCL(RJM010)では実データセットを割当てる
//*  ・TANKA が欠落しても RBRYO01C は内蔵単価で続行する運用 (D-01)
//*--------------------------------------------------------------
//RYOKEI   EXEC PGM=RBRYO01C,COND=(4,LT)
//STEPLIB  DD DSN=RACS.LOADLIB,DISP=SHR
//KENIN    DD DSN=RACS.KENSHIN.SORTED,DISP=SHR
//KYKMAST  DD DSN=RACS.KYKMAST,DISP=SHR
//TANKA    DD DSN=RACS.TANKA,DISP=SHR
//SAIKEI   DD DUMMY
//RYOOUT   DD DSN=RACS.RYOFILE,DISP=SHR
//SYSOUT   DD SYSOUT=*
//*--- 後続判定: RC=4 は「対象0件」で正常 (D-09 意味逆転に注意) ---
// IF (RYOKEI.RC > 4) THEN
//ABENDMSG EXEC PGM=IEBGENER
//SYSUT1   DD *
RJD030 RYOKEI RC>4 : UNYO TANTO NI RENRAKU
/*
//SYSUT2   DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSPRINT DD SYSOUT=*
// ENDIF
