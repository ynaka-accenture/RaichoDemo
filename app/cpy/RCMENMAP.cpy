      *----------------------------------------------------------------
      * RCMENMAP  メニュー画面 シンボリックマップ (RMEN01M)
      *----------------------------------------------------------------
       01  MEN01MI.
           05  MEN-SENTAKU     PIC X(1).
           05  FILLER          PIC X(79).
       01  MEN01MO REDEFINES MEN01MI.
           05  MENO-TITLE      PIC X(20).
           05  MENO-USER       PIC X(8).
           05  MENO-MSG        PIC X(40).
           05  FILLER          PIC X(12).
