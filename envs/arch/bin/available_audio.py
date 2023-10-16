#!/usr/bin/python
import subprocess
from subprocess import PIPE

DESCRIPTION_PREFIX = 'Description: '

def exec(cmd):
    proc = subprocess.run(cmd, shell=True, stdout=PIPE, stderr=PIPE, text=True)
    return proc.stdout

def get_available_items(cmd):
    items = exec(cmd)
    available_items = []
    current_item = None
    for l in items.split('\n'):
        l2 = l.strip()
        if l2.startswith(DESCRIPTION_PREFIX):
            current_item = l2[len(DESCRIPTION_PREFIX):]
        if l2 == 'Mute: yes':
            current_item = None
        if l2.startswith('Active Port: ') and current_item is not None:
            available_items.append(current_item)
    return available_items

def convert_source(s):
    if 'Family 17h' in s:
        return 'Line IN'
    if 'Webcam' in s:
        return 'Webcam'
    if 'TREKZ Titanium' in s:
        return 'Titanium'
    if 'Sound Blaster Play' in s:
        return 'SBP! IN'
    return s

def convert_sink(s):
    if 'Family 17h' in s:
        return 'Line OUT'
    if 'HDMI' in s:
        return 'HDMI'
    if 'TREKZ Titanium' in s:
        return 'Titanium'
    if 'MB16AMT' in s:
        return 'MB16AMT'
    if 'Sound Blaster Play' in s:
        return 'SBP! OUT'
    if 'JBL Pebbles ' in s:
        return 'JBL Pebbles'
    return s

def get_sources():
    return [convert_source(i) for i in get_available_items('pactl list sources')]

def get_sinks():
    return [convert_sink(i) for i in get_available_items('pactl list sinks')]

if __name__ == '__main__':
    sources = ', '.join(get_sources())
    sinks = ', '.join(get_sinks())
    print('↳ {} / {} ↴'.format(sources, sinks))
