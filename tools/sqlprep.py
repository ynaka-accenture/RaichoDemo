#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
sqlprep.py — Db2 プリコンパイラの模擬
  1. EXEC SQL INCLUDE SQLCA -> SQLCA 展開
  2. EXEC SQL INSERT ... END-EXEC -> RXDB2TB スタブ呼出し
     (ホスト変数 :VAR を順に SQ-HV1.. へ MOVE してから CALL)
対応: INCLUDE SQLCA / INSERT (VALUES にホスト変数最大6個)
"""
import re, sys, os

A = ' ' * 11
SQLCA = """       01  SQLCA.
           05  SQLCAID         PIC X(8) VALUE 'SQLCA   '.
           05  SQLCABC         PIC S9(9) COMP VALUE 136.
           05  SQLCODE         PIC S9(9) COMP VALUE ZERO.
           05  SQLERRM         PIC X(74).
           05  SQLERRP         PIC X(8).
           05  SQLERRD         PIC S9(9) COMP OCCURS 6.
           05  SQLWARN         PIC X(11).
           05  SQLSTATE        PIC X(5).
       01  SQ-PARM.
           05  SQ-STMT         PIC X(18).
           05  SQ-HV1          PIC X(22).
           05  SQ-HV2          PIC X(8).
           05  SQ-HV3          PIC X(8).
           05  SQ-HV4          PIC X(8).
           05  SQ-HV5          PIC X(8).
           05  SQ-HV6          PIC X(8)."""

def translate(src):
    lines, out, i = src.split('\n'), [], 0
    in_sql, buf = False, []
    while i < len(lines):
        ln = lines[i]; code = ln[6:72] if len(ln) > 6 else ''
        if not in_sql:
            if re.search(r'\bEXEC\s+SQL\b', code) and \
               (len(ln) < 7 or ln[6] not in '*/'):
                buf = [re.sub(r'.*EXEC\s+SQL', '', code)]
                if 'END-EXEC' in buf[0]:
                    out.extend(emit(' '.join(buf)))
                else:
                    in_sql = True
            else:
                out.append(ln)
        else:
            buf.append(code)
            if 'END-EXEC' in code:
                out.extend(emit(' '.join(buf)))
                in_sql = False
        i += 1
    return '\n'.join(out)

def emit(stmt):
    period = stmt.rstrip().endswith('.')
    stmt = re.sub(r'END-EXEC\.?', '', stmt).strip()
    if stmt.upper().startswith('INCLUDE SQLCA'):
        return SQLCA.split('\n')
    m = re.match(r'INSERT\s+INTO\s+(\w+)', stmt, re.I)
    if m:
        tbl = m.group(1)
        hvs = re.findall(r':([\w-]+)', stmt)
        o = [A + "MOVE 'INSERT " + tbl[:11].ljust(11) + "' TO SQ-STMT"]
        for n, hv in enumerate(hvs[:6], 1):
            o.append(A + "MOVE " + hv + " TO SQ-HV" + str(n))
        o.append(A + "CALL 'RXDB2TB' USING SQLCA SQ-PARM")
        if period:
            o[-1] += '.'
        return o
    return [A + 'CONTINUE' + ('.' if period else '')]

if __name__ == '__main__':
    src, dst = sys.argv[1], sys.argv[2]
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    open(dst, 'w').write(translate(open(src).read()))
    print(f'sqlprep: {src} -> {dst}')
