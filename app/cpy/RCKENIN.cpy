      *----------------------------------------------------------------
      * RCKENIN 検針データ受信 (RECFM=VB 最大283) H/D/K/T 4種
      *   FD内の複数01による同一レコード域の多重定義 (B-05)
      *   + ODO (B-06). 本コピー句はFILE SECTIONでCOPYすること
      *----------------------------------------------------------------
       01  KIN-REC.
           05  KIN-KBN             PIC X(1).
           05  KIN-BODY            PIC X(282).
       01  KIN-H-REC.
           05  FILLER              PIC X(1).
           05  KIN-H-SOSIN-CD      PIC X(5).
           05  KIN-H-SAKUSEI-BI    PIC 9(8).
           05  KIN-H-KENSU         PIC 9(7).
           05  FILLER              PIC X(262).
       01  KIN-D-REC.
           05  FILLER              PIC X(1).
           05  KIN-D-SPT-NO        PIC X(22).
           05  KIN-D-KENSHIN-BI    PIC 9(6).
           05  KIN-D-SIJISU        PIC 9(6).
           05  KIN-D-KENSHININ     PIC 9(5).
           05  KIN-D-KBN           PIC X(1).
           05  KIN-D-SU            PIC 9(2).
      *      30分値: 可変繰返し. 範囲外領域に前レコード残骸 (B-06)
           05  KIN-D-VAL           PIC 9(5)
               OCCURS 1 TO 48 TIMES DEPENDING ON KIN-D-SU.
       01  KIN-K-REC.
           05  FILLER              PIC X(1).
           05  KIN-K-SPT-NO        PIC X(22).
           05  KIN-K-KYU-MTR       PIC X(10).
           05  KIN-K-SHIN-MTR      PIC X(10).
           05  KIN-K-KOKAN-BI      PIC 9(8).
           05  KIN-K-KYU-SIJI      PIC 9(6).
           05  KIN-K-SHIN-SIJI     PIC 9(6).
           05  FILLER              PIC X(220).
       01  KIN-T-REC.
           05  FILLER              PIC X(1).
           05  KIN-T-KENSU         PIC 9(7).
           05  KIN-T-SIJI-GOKEI    PIC 9(10).
           05  FILLER              PIC X(265).
