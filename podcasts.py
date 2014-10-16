#!/usr/bin/env python3

import urllib3
import os
from xml.dom import minidom

base = "/home/lorenzo/"
podcasts = [
    ("ringcast",        "http://www.gehennainc.com/west/podcasts/ringcast/feed/generateXML.aspx"),
    ("coderradio",      "http://feeds.feedburner.com/se-radio"),
    ("YANSS",           "http://feeds.soundcloud.com/users/soundcloud:users:16745745/sounds.rss"),
    ( "roguelikeradio", "http://feeds.feedsburner.com/RoguelikeRadio")
]

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

def downloadRecent(hpool, podinfo, fetches=0):
    subfolder, podcast = podinfo
    folder = os.path.join(base, subfolder)
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
        self.hpool = urllib3.PoolManager()
    
    def download(self, podinfos):
        for podinfo in podinfos:
            status, downloaded = downloadRecent(self.hpool, podinfo)

if __name__ == "__main__":
    PodGet().download(podcasts)

