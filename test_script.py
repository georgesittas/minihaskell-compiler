##############################################################################################################################
#
# Usage: python3 test_script.py
# Requirements: 1) Adjust path in "test_dir" to reflect the path of your test directory, relative to the script's directory.
#               2) Adjust path in "custom_exec_name" to reflect the path of your custom interpreter executable, relative to
#                  the directory where your tests exist.
#               3) [Optionally] Adjust the value of "max_timeout" to reflect the max time that the custom interpeter or ghc
#                               are allowed to run before being interrupted.
##############################################################################################################################

import glob
import subprocess


test_dir         = './tests'
custom_exec_name = './TestAll'

max_timeout = 20


def main():
    tests_failed = 0

    print('> Begin testing...')
    for testfile in glob.glob(f'{test_dir}/*'):
        success = test_custom_vs_ghc(testfile)

        if not success:
            print('> Failed: ' + testfile)
            tests_failed += 1

    print('> Finished testing...')
    print('> Tests failed: ', tests_failed)
    print('> Bye!')


def check_with_custom(testfile):
    exec_name = custom_exec_name
    try:
        res = subprocess.check_output(
            f'{exec_name} < {testfile}; exit 0',
            stderr=subprocess.STDOUT,
            timeout=max_timeout,
            shell=True
        )
    except:
        res = b'error'

    res = res.decode().strip()
    try:
        # Check if result can be converted to integer (aka if int or error was returned)
        res = str(int(res))
    except:
        res = 'error'

    return res


def check_with_ghc(testfile):
    tmpname = testfile + '_tmp'
    rm_cmd = f'rm -f ./{tmpname} ./{tmpname}.hi ./{tmpname}.o ./{tmpname}.hs'

    try:
        res = subprocess.check_output(
            f'cp {testfile} {tmpname}.hs && printf "\n\nmain = print result\n\n" >> {tmpname}.hs \
                && ghc {tmpname}.hs && ./{tmpname} && {rm_cmd}; exit 0',
            stderr=subprocess.STDOUT,
            timeout=max_timeout,
            shell=True
        )
    except:
        subprocess.check_output(rm_cmd, shell=True)  # Cleanup leftovers
        res = b'error'

    res = res.decode().strip()

    if 'error' in res.lower():
        res = 'error'
    else:
        res = res.split('\n')[-1]
        try:
            res = str(int(res))
        except:
            res = 'error'

    return res


def test_custom_vs_ghc(testfile):
    ghc_res = check_with_ghc(testfile)
    cus_res = check_with_custom(testfile)

    if ghc_res == 'error' or cus_res == 'error':
        return False

    return int(ghc_res) == int(cus_res)


if __name__ == '__main__':
    main()
