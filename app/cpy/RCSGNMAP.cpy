      *----------------------------------------------------------------
      * RCSGNMAP  サインオン画面 シンボリックマップ (RSGN00M)
      *----------------------------------------------------------------
       01  SGN00MI.
           05  SGN-USERID      PIC X(8).
           05  SGN-PASSWD      PIC X(8).
           05  FILLER          PIC X(64).
       01  SGN00MO REDEFINES SGN00MI.
           05  SGNO-USERID     PIC X(8).
           05  SGNO-PASSWD     PIC X(8).
           05  SGNO-MSG        PIC X(40).
           05  SGNO-DATE       PIC X(10).
           05  FILLER          PIC X(14).
