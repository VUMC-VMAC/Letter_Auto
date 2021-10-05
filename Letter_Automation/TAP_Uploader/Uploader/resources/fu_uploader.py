#!/usr/bin/env python
import requests
import sys, getopt

def main(argv):

    vmac_id = ''
    filename = ''

    try:
        opts, args = getopt.getopt(argv, "hv:f:",["vmac=","file="])
    except getopt.GetoptError:
        print("help")
        sys.exit()
    for opt, arg in opts:
        if opt in ("-v", "--vmac"):
            vmac_id = arg
        elif opt in ("-f","--file"):
            filename = arg
    print("vmac ID is "+vmac_id)
    print("filename is "+filename)

    file_path = filename
    data = {
        'token': 'B07286F2BCFC9B49C157FD44A62F3320',
        'content': 'file',
        'action': 'import',
        'record': vmac_id,
        'field': 'vmac_letter',
        'event': 'initial_vmac_call_arm_1',
        'returnFormat': 'json'
    }
    file_obj = open(file_path, 'rb')
    r = requests.post('https://redcap.vanderbilt.edu/api/',data=data,files={'file':file_obj})
    file_obj.close()
    print('HTTP Status: ' + str(r.status_code))

if __name__ == "__main__":
	main(sys.argv[1:])