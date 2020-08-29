#!/usr/bin/python
import random
import requests
import sys

urls = [
  "https://ifconfig.me/ip",
  "https://ipinfo.io/ip",
  "http://ipecho.net/plain",
  # IPv6
  # "https://ifconfig.co",
  # "https://icanhazip.com"
]
max_retry = 10
timeout = 1.0

if __name__ == '__main__':
    l = len(urls)
    i = random.randint(0, l - 1)
    cnt = 0
    ip = ""
    while True:
        try:
            # TODO let's cache a result to save server resouces!
            ip = requests.get(urls[i], timeout=timeout).text.strip()
            break
        except KeyboardInterrupt:
            sys.exit()
        except:
            if cnt >= max_retry:
                ip = "Can't detect global IP"
                break
            cnt += 1
            i += 1
            if i >= l:
                i = 0
    print(ip)
