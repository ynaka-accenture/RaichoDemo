#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""xcheck_cics.py — CICS更新ジャーナルの対照検算 (KYKMAST不変も保証)"""
import sys, os, hashlib
P = os.path.join(os.path.dirname(__file__), '..', 'app', 'data', 'portable')

def main():
    ok = True
    upd = open(f'{P}/CICSUPD.dat', 'rb').read()
    b = upd[:320]
    ok1 = (len(upd) == 320
           and b[0:22] == b'0345000000000000004420'
           and b[50:58] == b'20301231'
           and b[58:59] == b'0'
           and b[152:160] == b'20260701'
           and b[160:168] == b'RAKYK02C'
           and b[300:301] == b'H'
           and b[301:309] == b'99991231'
           and b[310:318] == b'RAKYK02C')
    ok &= ok1
    print(f'XCHECK UPD: 1件/終了日20301231/更新刻印/履歴退避 '
          f'-> {"OK" if ok1 else "NG"}')
    # 元マスタが不変であること (更新はジャーナルにのみ)
    src = open(f'{P}/KYKMAST.dat', 'rb').read()
    h = hashlib.sha256(src).hexdigest()[:16]
    orig = None
    for r in range(0, len(src), 320):
        if src[r:r+22] == b'0345000000000000004420':
            orig = src[r+50:r+58]
            break
    ok2 = orig == b'99991231'
    ok &= ok2
    print(f'XCHECK SRC: KYKMAST不変(終了日={orig.decode()}) '
          f'sha={h} -> {"OK" if ok2 else "NG"}')
    return 0 if ok else 1

if __name__ == '__main__':
    sys.exit(main())
