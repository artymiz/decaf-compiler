"""
Run:

    python zipsrc.py

This will create a file `source.zip` which contains all the source code

To customize the files used by default, run:

    python zipsrc.py -h
"""
import sys, os, optparse, shutil

if __name__ == '__main__':
    optparser = optparse.OptionParser()
    optparser.add_option("-s", "--srcdir", dest="src_dir", default='src', help="specify the source code directory")
    optparser.add_option("-z", "--zipfile", dest="zipfile", default='source', help="create a zip file for the source code")
    (opts, _) = optparser.parse_args()

    outputs_zipfile = shutil.make_archive(opts.zipfile, 'zip', opts.src_dir)
    print("{0} created".format(outputs_zipfile), file=sys.stderr)

