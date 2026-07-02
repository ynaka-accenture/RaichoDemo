//RJD020   JOB (RACS),'KENSHIN SORT',CLASS=A,MSGCLASS=X
//*--------------------------------------------------------------
//* 日次D020: 検針データ選別ソート
//*   E15出口(RXSRT15C)=テストレコード除去 : ビジネスロジックが
//*   SORT出口に埋没 (D-05)
//*   E35出口(RXSRT35C)=部門コード付与
//*   ERRACM は DISP=MOD 追記: 再実行時は手順書により事前削除 (D-06)
//*--------------------------------------------------------------
//SORT01   EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=RACS.KENSHIN.HENSYU,DISP=SHR
//SORTOUT  DD DSN=RACS.KENSHIN.SORTED,DISP=(NEW,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(50,10))
//ERRACM   DD DSN=RACS.KENSHIN.ERRACM,DISP=MOD
//SYSIN    DD *
  SORT FIELDS=(2,22,CH,A,24,6,CH,A)
  MODS E15=(RXSRT15C,7000,,C),E35=(RXSRT35C,7000,,C)
/*
