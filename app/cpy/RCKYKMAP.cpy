      *----------------------------------------------------------------
      * RCKYKMAP  契約照会画面 シンボリックマップ (RKYK01M)
      *----------------------------------------------------------------
       01  KYK01MI.
           05  KYI-SPT         PIC X(22).
           05  FILLER          PIC X(58).
       01  KYK01MO REDEFINES KYK01MI.
           05  KYO-SPT         PIC X(22).
           05  KYO-JYU         PIC X(10).
           05  KYO-SYU         PIC X(2).
           05  KYO-KAISI       PIC X(8).
           05  KYO-KIHON       PIC X(9).
           05  KYO-FLG         PIC X(1).
           05  KYO-MSG         PIC X(28).
