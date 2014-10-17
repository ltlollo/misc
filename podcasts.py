#!/usr/bin/env python3

import urllib3
import os
import json
from xml.dom import minidom

setting_file = 'podcasts.json'

def fileName(url):
    return url.rpartition('/')[-1]

def createFolder(folder):
    if not os.path.exists(folder):
        os.makedirs(folder)
    elif not os.path.isdir(folder):
        raise IOError(folder + " is not a directory")

def fetchFile(hpool, url, filepath):
    res, action = 'err', url
    req = hpool.request('GET', url)
    if req.status is 200:
        f = open(filepath, 'wb')
        f.write(req.data)
        f.close()
        res, action = 'ok', filepath
    return (res, action)

def fetchRecents(hpool, folder, urls, fetches=0):
    res = []
    trim_urls = zip(urls, range(fetches)) if fetches else zip(urls, enumerate(urls))
    for url, _ in trim_urls:
        filename = fileName(url)
        filepath = os.path.join(folder, filename)
        if os.path.exists(filepath): break
        else: res.append(fetchFile(hpool, url, filepath))
    return res

def downloadRecent(hpool, podinfo, basedir='.', fetches=0):
    subfolder, podcast = podinfo
    folder = os.path.join(basedir, subfolder)
    createFolder(folder)
    status, downloaded = 'err', []
    req = hpool.request('GET', podcast)
    if req.status is 200:
        content_xml = minidom.parseString(req.data)
        nodes_url = content_xml.getElementsByTagName('enclosure')
        podcast_urls = [ x.getAttribute('url') for x in nodes_url if x.getAttribute('url') ]
        downloaded = fetchRecents(hpool, folder, podcast_urls, fetches)
        status = 'ok'
    return (status, downloaded)


class PodGet:
    def __init__(self):
        self.settings = json.load(open(setting_file, 'r'))
        self.hpool = urllib3.PoolManager()
    
    def download(self):
        basedir = self.settings['folder']
        createFolder(basedir)
        for podinfo in self.settings['podcasts'].items():
            status, downloaded = downloadRecent(self.hpool, podinfo, basedir)

if __name__ == "__main__":
    PodGet().download()

