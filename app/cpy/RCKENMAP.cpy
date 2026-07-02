      *----------------------------------------------------------------
      * RCKENMAP  検針照会画面 シンボリックマップ (RKEN01M)
      *----------------------------------------------------------------
       01  KEN01MI.
           05  KNI-SPT         PIC X(22).
           05  FILLER          PIC X(58).
       01  KEN01MO REDEFINES KEN01MI.
           05  KNO-SPT         PIC X(22).
           05  KNO-YM          PIC X(6).
           05  KNO-BI          PIC X(6).
           05  KNO-ZEN         PIC X(6).
           05  KNO-KON         PIC X(6).
           05  KNO-SIYO        PIC X(7).
           05  KNO-KBN         PIC X(1).
           05  KNO-MSG         PIC X(26).
