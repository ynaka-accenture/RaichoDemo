#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
cicsprep.py — CICS コマンドレベル変換器の模擬
実機の CICS 翻訳前処理と同様に:
  1. LINKAGE SECTION へ DFHEIBLK を注入
  2. PROCEDURE DIVISION を USING DFHEIBLK DFHCOMMAREA に書換え
  3. EXEC CICS ... END-EXEC をスタブ呼出し (RXCICSTB) へ展開
対応コマンド: SEND MAP / RECEIVE MAP / RETURN [TRANSID] /
  XCTL / READ DATASET / ASKTIME / ABEND
入力: 元ソース  出力: 変換後ソース (tests/cics-gen/)
"""
import re, sys, os

A = ' ' * 11  # B領域開始

def emit_call(opts, data_area):
    out = []
    out.append(A + "MOVE '" + opts['CMD'].ljust(11) + "' TO CP-CMD")
    for k, f in (('MAP', 'CP-MAP'), ('MAPSET', 'CP-MAPSET'),
                 ('DATASET', 'CP-DATASET'), ('TRANSID', 'CP-TRANSID'),
                 ('PROGRAM', 'CP-PROGRAM'), ('ABCODE', 'CP-ABCODE')):
        if k in opts:
            out.append(A + "MOVE '" + opts[k] + "' TO " + f)
    if 'RIDFLD' in opts:
        out.append(A + "MOVE " + opts['RIDFLD'] + " TO CP-RIDFLD")
    if 'LENGTH' in opts:
        out.append(A + "MOVE " + opts['LENGTH'] + " TO CP-DATA-LEN")
    out.append(A + "CALL 'RXCICSTB' USING DFHEIBLK CICS-PARM "
               + (data_area if data_area else 'CP-DUMMY'))
    if 'RESP' in opts:
        out.append(A + "MOVE CP-RESP TO " + opts['RESP'])
    return out

def parse_exec(stmt):
    """EXEC CICS 本文を解析し (opts, data_area) を返す"""
    body = stmt.strip()
    opts, data = {}, None
    m = re.match(r'(SEND\s+MAP|RECEIVE\s+MAP|RETURN|XCTL|REWRITE|READ|'
                 r'ASKTIME|ABEND)', body)
    verb = re.sub(r'\s+', ' ', m.group(1))
    for key, val in re.findall(r"(\w+)\s*\(\s*([^)]*?)\s*\)", body):
        key = key.upper()
        if key in ('MAP', 'MAPSET', 'DATASET', 'TRANSID',
                   'PROGRAM', 'ABCODE'):
            opts[key] = val.strip("'")
        elif key in ('INTO', 'FROM', 'COMMAREA'):
            data = val
        elif key in ('RIDFLD', 'LENGTH', 'RESP'):
            opts[key] = val
    if verb == 'SEND MAP':
        opts['CMD'] = 'SEND-MAP'
    elif verb == 'RECEIVE MAP':
        opts['CMD'] = 'RECEIVE-MAP'
    elif verb == 'RETURN':
        opts['CMD'] = 'RETURN-TRN' if 'TRANSID' in opts \
            else 'RETURN-END'
    elif verb == 'XCTL':
        opts['CMD'] = 'XCTL'
    elif verb == 'READ':
        opts['CMD'] = 'READ-DS'
    elif verb == 'REWRITE':
        opts['CMD'] = 'REWRITE-DS'
    elif verb == 'ASKTIME':
        opts['CMD'] = 'ASKTIME'
    elif verb == 'ABEND':
        opts['CMD'] = 'ABEND'
    return opts, data

def translate(src):
    lines = src.split('\n')
    out, i, in_exec, buf = [], 0, False, []
    tail_period = False
    for ln in lines:
        code = ln[6:72] if len(ln) > 6 else ''
        if not in_exec:
            if re.search(r'\bEXEC\s+CICS\b', code) and \
               (len(ln) < 7 or ln[6] not in '*/'):
                in_exec = True
                buf = [re.sub(r'.*EXEC\s+CICS', '', code)]
                if 'END-EXEC' in buf[0]:
                    stmt = buf[0]
                    tail_period = stmt.rstrip().endswith('.')
                    stmt = re.sub(r'END-EXEC\.?', '', stmt)
                    opts, data = parse_exec(stmt)
                    calls = emit_call(opts, data)
                    if opts['CMD'] in ('RETURN-TRN', 'RETURN-END',
                                       'XCTL', 'ABEND'):
                        calls.append(A + 'GOBACK')
                    if tail_period:
                        calls[-1] += '.'
                    out.extend(calls)
                    in_exec = False
                continue
            if 'LINKAGE SECTION.' in code:
                out.append(ln)
                out.append('       01  DFHEIBLK.')
                out.append('       COPY RCEIBLK.')
                continue
            m = re.match(r'(\s*PROCEDURE\s+DIVISION)\s*\.', code)
            if m:
                out.append(ln[:7] + m.group(1).strip()
                           + ' USING DFHEIBLK DFHCOMMAREA.')
                continue
            if 'WORKING-STORAGE SECTION.' in code:
                out.append(ln)
                out.append('       01  CICS-PARM.')
                out.append('       COPY RCCICSPM.')
                continue
            out.append(ln)
        else:
            buf.append(code)
            if 'END-EXEC' in code:
                stmt = ' '.join(buf)
                tail_period = stmt.rstrip().endswith('.')
                stmt = re.sub(r'END-EXEC\.?', '', stmt)
                opts, data = parse_exec(stmt)
                calls = emit_call(opts, data)
                if opts['CMD'] in ('RETURN-TRN', 'RETURN-END',
                                   'XCTL', 'ABEND'):
                    calls.append(A + 'GOBACK')
                if tail_period:
                    calls[-1] += '.'
                out.extend(calls)
                in_exec = False
    return '\n'.join(out)

if __name__ == '__main__':
    src_path, dst_path = sys.argv[1], sys.argv[2]
    os.makedirs(os.path.dirname(dst_path), exist_ok=True)
    open(dst_path, 'w').write(translate(open(src_path).read()))
    print(f'cicsprep: {src_path} -> {dst_path}')
