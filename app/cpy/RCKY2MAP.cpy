      *----------------------------------------------------------------
      * RCKY2MAP  契約更新画面 シンボリックマップ (RKY201M)
      *----------------------------------------------------------------
       01  KY201MI.
           05  KY2-SPT         PIC X(22).
           05  KY2-SYURYO      PIC X(8).
           05  KY2-TEISI       PIC X(1).
           05  KY2-KAKUNIN     PIC X(1).
           05  FILLER          PIC X(48).
       01  KY201MO REDEFINES KY201MI.
           05  KYO2-SPT        PIC X(22).
           05  KYO2-SYURYO     PIC X(8).
           05  KYO2-TEISI      PIC X(1).
           05  KYO2-KYU-SYUR   PIC X(8).
           05  KYO2-KYU-TEI    PIC X(1).
           05  KYO2-MSG        PIC X(40).
