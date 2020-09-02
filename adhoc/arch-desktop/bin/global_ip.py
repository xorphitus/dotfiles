#!/usr/bin/python
import os
import random
import requests
import sys
import time

urls = [
  'https://ifconfig.me/ip',
  'https://ipinfo.io/ip',
  'http://ipecho.net/plain',
  # IPv6
  # 'https://ifconfig.co',
  # 'https://icanhazip.com'
]

max_retry = 10
timeout = 1.0

cache_path = '/tmp/my_global_ip_cache'
cache_expire_sec = 60 * 60

if __name__ == '__main__':
    ip = None

    now = time.time()
    if os.path.exists(cache_path) and os.stat(cache_path).st_mtime + cache_expire_sec > now:
        with open(cache_path, 'r') as f:
            ip = f.read().strip()

    l = len(urls)
    i = 0
    offset = random.randint(0, l - 1)
    while ip is None:
        try:
            j = (i + offset) % l
            ip = requests.get(urls[j], timeout=timeout).text.strip()
            with open(cache_path, 'w') as f:
                f.write(ip)
        except KeyboardInterrupt:
            sys.exit()
        except:
            if i >= max_retry:
                ip = "Can't detect global IP"
            i += 1

    print(ip)
