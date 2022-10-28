import argparse
import os

LLVM_PATH = os.environ.get('LLVM_PATH', '')
if LLVM_PATH == '':
    raise ValueError('Unkown LLVM_PATH')
LLVM_LIB_PATH = os.path.join(LLVM_PATH, 'lib')
LLVM_BIN_PATH = os.path.join(LLVM_PATH, 'bin')
CLANG_PATH = os.path.join(LLVM_BIN_PATH, 'clang')
LLC_PATH = os.path.join(LLVM_BIN_PATH, 'llc')
OPT_PATH = os.path.join(LLVM_BIN_PATH, 'opt')
TARGET = 'x86_64-pc-linux-gnu'


def gen_llvm_ir(target: str, cfile_path: str, output_path: str, opt_level: int, cflags: str):
    params = [
        f'-S {cfile_path}',
        f'-o {output_path}',
        f'-O{opt_level}',
        # '-g',
        # '-mem2reg',
        '-emit-llvm',
        '-Xclang',
        '-disable-O0-optnone',
        # # https://clang.llvm.org/docs/UsersManual.html#cmdoption-fno-discard-value-names
        # '-fno-discard-value-names',
        f'--target={target}',
        cflags,
        f'> /dev/null',
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
        f'> /dev/null',
        f'2>&1',
    ]
    param_str = ' '.join(params)
    return f'{OPT_PATH} {param_str}'


def gen_json(output_name: str):
    ir_path = f'{output_name}.ll'
    json_path = f'{output_name}.json'
    params = [
        f'-load {LLVM_LIB_PATH}/IRExtractor.so',
        '--IRExtractor',
        '-enable-new-pm=0',
        f'{ir_path}',
        f'1> /dev/null',
        f'2> {json_path}',
    ]
    param_str = ' '.join(params)
    return f'{OPT_PATH} {param_str}', json_path


def gen_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input", help="input file", required=True)
    parser.add_argument(
        "-o", "--output", help="output file", default="outputs/output.ll")
    parser.add_argument(
        "-d", "--debug", help="enable debug", action='store_true')
    parser.add_argument("-J", "--json", help="only generates json",
                        default=False, action="store_true")
    parser.add_argument(
        "-O", "--opt", help="optimization level", default=0, type=int)
    parser.add_argument("-C", "--cflags", help="extra cflags", default="")
    parser.add_argument("-I", "--includes",
                        help="extra include dirs", default="", nargs="*")
    return parser.parse_args()


def main():
    args = gen_args()
    DEBUG_ENABLE = args.debug

    def printer(ps: str):
        if DEBUG_ENABLE:
            print(ps)
    input_path = os.path.join(os.getcwd(), args.input)
    output_path = os.path.join(os.getcwd(), args.output)

    only_json = args.json

    if not only_json:
        if not output_path.endswith(".ll"):
            raise ValueError(
                f"Output Path should end with .ll, current: {output_path}")
        ir_temp_path = output_path.replace(".ll", "_temp.ll")
        exec_cmd = gen_llvm_ir(
            TARGET, input_path, ir_temp_path, args.opt, args.cflags)

        printer(f"  1. Generate LLVM IR targeting {TARGET}")
        printer(f"Exec: {exec_cmd}")
        printer(f"Output: {ir_temp_path}")

        os.system(exec_cmd)
        remove_annotations(ir_temp_path)

        exec_cmd = gen_mem2reg_ir(ir_temp_path, output_path)

        printer(f"  2. Generate LLVM IR passing mem2reg")
        printer(f"Exec: {exec_cmd}")
        printer(f"Output: {output_path}")

        os.system(exec_cmd)
        remove_annotations(output_path)

    output_name = output_path.replace(".ll", "")
    exec_cmd, json_path = gen_json(output_name)

    printer(f"  3. Generate JSON line for rosette")
    printer(f"Exec: {exec_cmd}")
    printer(f"Output: {json_path}")
    os.system(exec_cmd)


if __name__ == '__main__':
    main()
