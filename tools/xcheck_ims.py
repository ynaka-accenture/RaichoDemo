#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""xcheck_ims.py — IMS計器設備DBの対照検算
   1. MTRDB (unload) から Python 独立実装で MTRMAST を再導出し
      COBOL 抽出物 (=正本 MTRMAST.dat) とバイト比較
   2. 階層規則: 各ルートに現行取付 (取外日=0) が丁度 1、
      取付日は昇順、期間は連鎖 (取外日=次の取付日)"""
import sys, os, hashlib
P = os.path.join(os.path.dirname(__file__), '..', 'app', 'data', 'portable')

def main():
    d = open(f'{P}/MTRDB.dat', 'rb').read()
    recs = [d[i:i+80] for i in range(0, len(d), 80)]
    out, ng, roots, twins = [], 0, 0, 0
    root, cur, prev_t = None, None, None

    def flush():
        nonlocal ng
        if root is None:
            return
        if cur is None:
            ng += 1
            return
        no = root[1:11]
        rec = (no + cur[19:41] + root[11:12] + root[12:14]
               + root[14:16] + root[16:18] + root[18:19]
               + root[19:27] + cur[11:19] + root[27:33])
        out.append(rec + b' ' * (128 - len(rec)))

    for r in recs:
        if r[0:1] == b'R':
            flush()
            root, cur, prev_t = r, None, None
            roots += 1
        elif r[0:1] == b'T':
            twins += 1
            if r[1:11] != root[1:11]:
                ng += 1
            if prev_t is not None:
                if r[11:19] < prev_t[11:19]:
                    ng += 1          # 取付日昇順
                if prev_t[41:49] != r[11:19]:
                    ng += 1          # 期間連鎖
            if r[41:49] == b'00000000':
                if cur is not None:
                    ng += 1          # 現行重複
                cur = r
            prev_t = r
    flush()
    py = b''.join(out)
    cob = open(f'{P}/MTRMAST.dat', 'rb').read()
    eq = (py == cob)
    h = hashlib.sha256(cob).hexdigest()[:16]
    ok = eq and ng == 0
    print(f'XCHECK IMS: roots={roots} twins={twins} '
          f'byte-equal={eq} rule-NG={ng} sha={h} '
          f'-> {"OK" if ok else "NG"}')
    return 0 if ok else 1

if __name__ == '__main__':
    sys.exit(main())
