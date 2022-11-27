import argparse
import os
from typing import List

LLVM_PATH = os.environ.get('LLVM_PATH', '')
if LLVM_PATH == '':
    raise ValueError('Unkown LLVM_PATH')
LLVM_LIB_PATH = os.path.join(LLVM_PATH, 'lib')
LLVM_BIN_PATH = os.path.join(LLVM_PATH, 'bin')
CLANG_PATH = os.path.join(LLVM_BIN_PATH, 'clang')
OPT_PATH = os.path.join(LLVM_BIN_PATH, 'opt')

parser = argparse.ArgumentParser()
parser.add_argument("-i", "--input", help="Input path or directory", required=True)
parser.add_argument("-o", "--output", help="Output directory", default="")
parser.add_argument("-d", "--debug", help="Enable debug", action='store_true')
parser.add_argument("-j", "--json", help="Only generates json", default=False, action="store_true")
parser.add_argument("-O", "--optlevel", help="Optimization level", default=0, type=int)
parser.add_argument("-C", "--cflags", help="Extra cflags", default="")
parser.add_argument("-I", "--includes", help="Extra include dirs", default="", nargs="*")

DEBUG_ENABLE = False
def printer(ps: str):
    if DEBUG_ENABLE:
        print(ps)


def gen_llvm_ir(cfile_path: str, output_path: str, opt_level: int, cflags: str, includes: str):
    params = [
        f'-S {cfile_path}',
        f'-o {output_path}',
        f'-O{opt_level}',
        '-emit-llvm',
        '-Xclang',
        '-disable-O0-optnone',
        # https://clang.llvm.org/docs/UsersManual.html#cmdoption-fno-discard-value-names
        '-fno-discard-value-names',
        includes,
        cflags,
        f'1> /dev/null',
        f'2>&1',
    ]
    param_str = ' '.join(params)
    return f'{CLANG_PATH} {param_str}'


def remove_annotations(ir_temp_path: str):
    def _non_annotations(s: str):
        return not s.startswith((';', 'source_filename = '))

    with open(ir_temp_path, 'r') as f:
        origin_lines = f.readlines()
    filtered_lines = filter(_non_annotations, origin_lines)

    with open(ir_temp_path, 'w') as f:
        f.writelines(filtered_lines)


def gen_mem2reg_ir(input_path: str, output_path: str):
    params = [
        '-S',
        '-mem2reg',
        '-instnamer',
        f'-o {output_path}',
        f'{input_path}',
        f'1> /dev/null',
        f'2>&1',
    ]
    param_str = ' '.join(params)
    return f'{OPT_PATH} {param_str}'


def gen_json(output_file_path: str):
    json_path = output_file_path.replace(".ll", ".json")
    params = [
        f'-load {LLVM_LIB_PATH}/IRExtractor.so',
        '--IRExtractor',
        '-enable-new-pm=0',
        f'{output_file_path}',
        f'1> /dev/null',
        f'2> {json_path}',
    ]
    param_str = ' '.join(params)
    return f'{OPT_PATH} {param_str}', json_path

def cfile2ir(input_path: str, output_path: str, opt_level: str, cflags: str, includes: str):
    ir_temp_path = output_path.replace(".ll", "_temp.ll")
    exec_cmd = gen_llvm_ir(input_path, ir_temp_path, opt_level, cflags, includes)

    printer(f"  1. Generate LLVM IR.")
    printer(f"Exec: {exec_cmd}")
    printer(f"Output: {ir_temp_path}")

    os.system(exec_cmd)
    remove_annotations(ir_temp_path)

    exec_cmd = gen_mem2reg_ir(ir_temp_path, output_path)

    printer(f"  2. Generate LLVM IR passing mem2reg.")
    printer(f"Exec: {exec_cmd}")
    printer(f"Output: {output_path}")

    os.system(exec_cmd)
    remove_annotations(output_path)

    os.remove(ir_temp_path)

def ir2json(output_file_path: str):
    exec_cmd, json_path = gen_json(output_file_path)

    printer(f"  3. Generate JSON line for rosette")
    printer(f"Exec: {exec_cmd}")
    printer(f"Output: {json_path}")
    printer("\n\n")
    os.system(exec_cmd)


def _main():
    args = parser.parse_args()
    global DEBUG_ENABLE
    DEBUG_ENABLE = args.debug
    input_path = os.path.join(os.getcwd(), args.input)
    if args.output:
        output_path = os.path.join(os.getcwd(), args.output)
    else:
        output_path = input_path

    if not os.path.isdir(output_path):
        raise ValueError(f"Output should be directory, current is: {output_path}")
    input_file_paths = []
    if os.path.isdir(input_path):
        for f in os.listdir(input_path):
            if f.endswith((".cpp", "c")):
                input_file_path = os.path.join(input_path, f)
                input_file_paths.append(input_file_path)
    else:
        input_file_paths = [input_path]

    only_json = args.json
    opt_level = args.optlevel
    cflags = args.cflags
    includes = " ".join(args.includes)

    for i in input_file_paths:
        filename = os.path.basename(i)
        output_filename = filename.replace(".cpp", ".ll")
        output_filename = output_filename.replace(".c", ".ll")
        output_file_path = os.path.join(output_path, output_filename)
        if not only_json:
            cfile2ir(i, output_file_path, opt_level, cflags, includes)
        ir2json(output_file_path)


if __name__ == '__main__':
    _main()
